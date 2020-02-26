
/*
 * module.c - Output modules for Speech Dispatcher
 *
 * Copyright (C) 2001, 2002, 2003, 2006, 2007 Brailcom, o.p.s.
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * $Id: module.c,v 1.40 2008-07-07 14:30:51 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>
#include <dirent.h>
#include <glib.h>
#include <dotconf.h>

#include "speechd.h"
#include <spd_utils.h>
#include "output.h"
#include "module.h"

static char *spd_get_path(const char *filename, const char *startdir)
{
	char *ret;
	if (filename == NULL)
		return NULL;
	if (filename[0] != '/') {
		if (startdir == NULL)
			ret = g_strdup(filename);
		else
			ret = g_strdup_printf("%s/%s", startdir, filename);
	} else {
		ret = g_strdup(filename);
	}
	return ret;
}

void destroy_module(OutputModule * module)
{
	g_free(module->name);
	g_free(module->filename);
	g_free(module->configfilename);
	g_free(module);
}

/*
 * Check that we can execute the configured command
 */
DOTCONF_CB(GenericCmdDependency_cb)
{
	unsigned *missing_paths = ctx;
	char s[5 + strlen(cmd->data.str) + 17 + 1];

	if (!cmd->data.str[0])
		return NULL;

	snprintf(s, sizeof(s), "type %s > /dev/null 2>&1", cmd->data.str);
	if (system(s) != 0)
	{
		MSG(5, "Did not find command %s", cmd->data.str);
		(*missing_paths)++;
	}
	return NULL;
}
FUNC_ERRORHANDLER(ignore_errors)
{
	return 0;
}

/*
 * detect_output_modules: automatically discover all available modules.
 * Parameters:
 * dirname: name of the directory containing module binaries.
 * Returns: a list of detected modules.
 * For each file in the directory containing module binaries,
 * add an entry to a list of discovered modules.
 */
GList *detect_output_modules(const char *dirname, const char *config_dirname)
{
	static const int FNAME_PREFIX_LENGTH = 3;
	DIR *module_dir = opendir(dirname);
	struct dirent *entry;
	char **module_parameters;
	GList *modules = NULL;
	char *full_path;
	struct stat fileinfo;
	int sys_ret;

	if (module_dir == NULL) {
		MSG(3, "couldn't open directory %s because of error %s\n",
		    dirname, strerror(errno));
		return NULL;
	}

	while (NULL != (entry = readdir(module_dir))) {

		if (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, ".."))
			continue;
		full_path = spd_get_path(entry->d_name, dirname);
		sys_ret = stat(full_path, &fileinfo);
		g_free(full_path);
		if (sys_ret != 0) {
			MSG(4, "stat failed on file %s in %s", entry->d_name,
			    dirname);
			continue;
		}
		/* Note: stat(2) dereferences symlinks. */
		if (!S_ISREG(fileinfo.st_mode)) {
			MSG(4, "Ignoring %s in %s; not a regular file.",
			    entry->d_name, dirname);
			continue;
		}

		if (strncmp(entry->d_name, "sd_", FNAME_PREFIX_LENGTH)
		    || (entry->d_name[FNAME_PREFIX_LENGTH] == '\0')) {
			MSG(1,
			    "Module discovery ignoring %s: malformed filename.",
			    entry->d_name);
			continue;
		}

		module_parameters = g_malloc(4 * sizeof(char *));
		module_parameters[0] =
		    g_strdup(entry->d_name + FNAME_PREFIX_LENGTH);
		module_parameters[1] = g_strdup(entry->d_name);
		module_parameters[2] =
		    g_strdup_printf("%s.conf", module_parameters[0]);
		module_parameters[3] =
		    g_strdup_printf("%s/%s.log", SpeechdOptions.log_dir,
				    module_parameters[0]);
		modules = g_list_append(modules, module_parameters);

		MSG(5,
		    "Module name=%s being inserted into detected_modules list",
		    module_parameters[0]);

                /* Special-case the generic module: autoload for the various
                 * configurations */
		if (strcmp(module_parameters[0], "generic") == 0) {
			full_path = spd_get_path("modules", config_dirname);
			MSG(5, "Looking for generic variants configurations in %s",
			    full_path);
			DIR *config_dir = opendir(full_path);
			if (config_dir == NULL)
			{
				MSG(4, "couldn't open directory %s because of error %s\n",
				    full_path, strerror(errno));
				g_free(full_path);
				continue;
			}

			while (NULL != (entry = readdir(config_dir))) {
				size_t len;
				char *file_path;

				static const configoption_t options[] = {
					{
						.name = "GenericCmdDependency",
						.type = ARG_STR,
						.callback = GenericCmdDependency_cb,
						.info = NULL,
						.context = 0,
					},
					{
						.name = "",
						.type = 0,
						.callback = NULL,
						.info = NULL,
						.context = 0,
					}
				};
				configfile_t *configfile;
				unsigned missing_paths = 0;

				if (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, ".."))
					continue;
				file_path = spd_get_path(entry->d_name, full_path);
				sys_ret = stat(file_path, &fileinfo);
				if (sys_ret != 0) {
					MSG(4, "stat failed on file %s in %s", entry->d_name,
					    config_dirname);
					continue;
				}

				/* Note: stat(2) dereferences symlinks. */
				if (!S_ISREG(fileinfo.st_mode)) {
					MSG(4, "Ignoring %s in %s; not a regular file.",
					    entry->d_name, config_dirname);
					g_free(file_path);
					continue;
				}

				len = strlen(entry->d_name);
				if (strcmp(entry->d_name + len - strlen("-generic.conf"),
					   "-generic.conf")) {
					MSG(5, "Ignoring %s: not named something-generic.conf",
					    entry->d_name);
					g_free(file_path);
					continue;
				}

				/* Check for actual binaries given by GenericCmdDependency */

				configfile = dotconf_create(file_path, options,
							    &missing_paths, CASE_INSENSITIVE);
				if (!configfile) {
					MSG(5, "Ignoring %s: Can not parse config file", file_path);
					g_free(file_path);
					continue;
				}
				configfile->errorhandler = (dotconf_errorhandler_t) ignore_errors;

				if (dotconf_command_loop(configfile) == 0) {
					MSG(5, "Ignoring %s: Can not parse config file", file_path);
					g_free(file_path);
					dotconf_cleanup(configfile);
					continue;
				}
				dotconf_cleanup(configfile);

				if (missing_paths != 0) {
					MSG(5, "Ignoring %s: did not find %d commands",
					       file_path, missing_paths);
					g_free(file_path);
					continue;
				}
				g_free(file_path);

				module_parameters = g_malloc(4 * sizeof(char *));
				module_parameters[1] = g_strdup("sd_generic");
				module_parameters[2] = g_strdup(entry->d_name);
				entry->d_name[len - strlen(".conf")] = '\0';
				module_parameters[0] = g_strdup(entry->d_name);
				module_parameters[3] =
				    g_strdup_printf("%s/%s.log", SpeechdOptions.log_dir,
						    entry->d_name);
				modules = g_list_append(modules, module_parameters);

				MSG(5,
				    "Module name=%s being inserted into detected_modules list",
				    entry->d_name);
			}
			g_free(full_path);
			closedir(config_dir);
		}
	}

	closedir(module_dir);
	return modules;
}

OutputModule *load_output_module(char *mod_name, char *mod_prog,
				 char *mod_cfgfile, char *mod_dbgfile)
{
	OutputModule *module;
	int fr;
	char *argv[3] = { 0, 0, 0 };
	int ret;
	char *module_conf_dir;
	char *rep_line = NULL;
	FILE *f;
	size_t n = 0;
	char s;
	GString *reply;

	if (mod_name == NULL)
		return NULL;

	module = (OutputModule *) g_malloc(sizeof(OutputModule));

	module->name = (char *)g_strdup(mod_name);
	module->filename = (char *)spd_get_path(mod_prog, SpeechdOptions.module_dir);

	module_conf_dir = g_strdup_printf("%s/modules",
					  SpeechdOptions.conf_dir);

	module->configfilename =
	    (char *)spd_get_path(mod_cfgfile, module_conf_dir);
	g_free(module_conf_dir);

	if (mod_dbgfile != NULL)
		module->debugfilename = g_strdup(mod_dbgfile);
	else
		module->debugfilename = NULL;

	if (!strcmp(mod_name, "testing")) {
		module->pipe_in[1] = 1;	/* redirect to stdin */
		module->pipe_out[0] = 0;	/* redirect to stdout */
		return module;
	}

	if ((pipe(module->pipe_in) != 0)
	    || (pipe(module->pipe_out) != 0)) {
		MSG(3, "Can't open pipe! Module not loaded.");
		return NULL;
	}

	argv[0] = module->filename;
	if (mod_cfgfile) {
		argv[1] = module->configfilename;
	}

	/* Open the file for child stderr (logging) redirection */
	if (module->debugfilename != NULL) {
		module->stderr_redirect = open(module->debugfilename,
					       O_WRONLY | O_CREAT | O_TRUNC,
					       S_IRUSR | S_IWUSR);
		if (module->stderr_redirect == -1)
			MSG(1,
			    "ERROR: Opening debug file for %s failed: (error=%d) %s",
			    module->name, module->stderr_redirect,
			    strerror(errno));
	} else {
		module->stderr_redirect = -1;
	}

	MSG(2,
	    "Initializing output module %s with binary %s and configuration %s",
	    module->name, module->filename, module->configfilename);
	if (module->stderr_redirect >= 0)
		MSG(3, "Output module is logging to file %s",
		    module->debugfilename);
	else
		MSG(3,
		    "Output module is logging to standard error output (stderr)");

	fr = fork();
	if (fr == -1) {
		printf("Can't fork, error! Module not loaded.");
		return NULL;
	}

	if (fr == 0) {
		ret = dup2(module->pipe_in[0], 0);
		close(module->pipe_in[0]);
		close(module->pipe_in[1]);

		ret = dup2(module->pipe_out[1], 1);
		close(module->pipe_out[1]);
		close(module->pipe_out[0]);

		/* Redirrect stderr to the appropriate logfile */
		if (module->stderr_redirect >= 0) {
			ret = dup2(module->stderr_redirect, 2);
		}

		execvp(argv[0], argv);
		MSG(1,
		    "Exec of module \"%s\" with config \"%s\" failed with error %d: %s",
		    argv[0], argv[1] ? argv[1] : "<none>", errno,
		    strerror(errno));
		exit(1);
	}

	module->pid = fr;
	close(module->pipe_in[0]);
	close(module->pipe_out[1]);

	usleep(100);		/* So that the other child has at least time to fail
				   with the execlp */
	ret = waitpid(module->pid, NULL, WNOHANG);
	if (ret != 0) {
		MSG(2,
		    "ERROR: Can't load output module %s with binary %s. Bad filename in configuration?",
		    module->name, module->filename);
		destroy_module(module);
		return NULL;
	}

	module->working = 1;
	MSG(2, "Module %s loaded.", module->name);

	/* Create a stream from the socket */
	module->stream_out = fdopen(module->pipe_out[0], "r");
	if (!module->stream_out)
		FATAL("Can't create a stream for socket, fdopen() failed.");

	/* Switch to line buffering mode */
	ret = setvbuf(module->stream_out, NULL, _IONBF, 4096);
	if (ret)
		FATAL("Can't set line buffering, setvbuf failed.");

	MSG(4, "Trying to initialize %s.", module->name);
	if (output_send_data("INIT\n", module, 0) != 0) {
		MSG(1, "ERROR: Something wrong with %s, can't initialize",
		    module->name);
		output_close(module);
		return NULL;
	}

	reply = g_string_new("\n---------------\n");
	f = fdopen(dup(module->pipe_out[0]), "r");
	while (1) {
		ret = spd_getline(&rep_line, &n, f);
		if (ret <= 0) {
			MSG(1, "ERROR: Bad syntax from output module %s 1",
			    module->name);
			if (rep_line != NULL)
				g_free(rep_line);
			fclose(f);
			return NULL;
		}
		assert(rep_line != NULL);
		MSG(5, "Reply from output module: %ld %s", (long) n, rep_line);
		if (ret <= 4) {
			MSG(1, "ERROR: Bad syntax from output module %s 2",
			    module->name);
			g_free(rep_line);
			fclose(f);
			return NULL;
		}

		if (rep_line[3] != '-') {
			s = rep_line[0];
			g_free(rep_line);
			break;
		}

		g_string_append(reply, rep_line + 4);
	}

	fclose(f);
	g_string_append_printf(reply, "---------------\n");

	if (s == '3') {
		MSG(1, "ERROR: Module %s failed to initialize. Reason: %s",
		    module->name, reply->str);
		module->working = 0;
		kill(module->pid, 9);
		waitpid(module->pid, NULL, WNOHANG);
		destroy_module(module);
		g_string_free(reply, TRUE);
		return NULL;
	}

	if (s == '2')
		MSG(2, "Module %s started successfully with message: %s",
		    module->name, reply->str);

	g_string_free(reply, 1);

	if (SpeechdOptions.debug) {
		MSG(4, "Switching debugging on for output module %s",
		    module->name);
		output_module_debug(module);
	}

	/* Initialize audio settings */
	ret = output_send_audio_settings(module);
	if (ret != 0) {
		MSG(1,
		    "ERROR: Can't initialize audio in output module, see reason above.");
		module->working = 0;
		kill(module->pid, 9);
		waitpid(module->pid, NULL, WNOHANG);
		destroy_module(module);
		return NULL;
	}

	/* Send log level configuration setting */
	ret = output_send_loglevel_setting(module);
	if (ret != 0) {
		MSG(1,
		    "ERROR: Can't set the log level inin the output module.");
		module->working = 0;
		kill(module->pid, 9);
		waitpid(module->pid, NULL, WNOHANG);
		destroy_module(module);
		return NULL;
	}

	return module;
}

int unload_output_module(OutputModule * module)
{
	assert(module != NULL);

	MSG(3, "Unloading module name=%s", module->name);

	output_close(module);

	close(module->pipe_in[1]);
	close(module->pipe_out[0]);

	destroy_module(module);

	return 0;
}

int reload_output_module(OutputModule * old_module)
{
	OutputModule *new_module;
	int pos;

	assert(old_module != NULL);
	assert(old_module->name != NULL);

	if (old_module->working)
		return 0;

	MSG(3, "Reloading output module %s", old_module->name);

	output_close(old_module);
	close(old_module->pipe_in[1]);
	close(old_module->pipe_out[0]);

	new_module = load_output_module(old_module->name, old_module->filename,
					old_module->configfilename,
					old_module->debugfilename);
	if (new_module == NULL) {
		MSG(3, "Can't load module %s while reloading modules.",
		    old_module->name);
		return -1;
	}

	pos = g_list_index(output_modules, old_module);
	output_modules = g_list_remove(output_modules, old_module);
	output_modules = g_list_insert(output_modules, new_module, pos);
	destroy_module(old_module);

	return 0;
}

int output_module_debug(OutputModule * module)
{
	char *new_log_path;

	assert(module != NULL);
	assert(module->name != NULL);
	if (!module->working)
		return -1;

	MSG(4, "Output module debug logging for %s into %s", module->name,
	    SpeechdOptions.debug_destination);

	new_log_path = g_strdup_printf("%s/%s.log",
				       SpeechdOptions.debug_destination,
				       module->name);

	output_send_debug(module, 1, new_log_path);

	return 0;
}

int output_module_nodebug(OutputModule * module)
{
	assert(module != NULL);
	assert(module->name != NULL);
	if (!module->working)
		return -1;

	MSG(4, "Output module debug logging off for %s", module->name);

	output_send_debug(module, 0, NULL);

	return 0;
}

static GList *requested_modules = NULL;

/*
 * module_already_requested: determine whether we have already received
 * a load request for a given module.
 * Parameters:
 * module_name: the name of the module.
 * module_cmd: name of the binary associated with this module.
 * module_cfgfile: the name of the module's configuration file.
 *
 * Returns:
 * TRUE if the module was previously requested, FALSE otherwise.
 *
 * A module is previously requested if:
 * 1. module_name is the same as some other module in the list of load
 * requests.  We select a given output module using its name, so names
 * must be unique.
 * 2. The name of the executable and the name of the configuration file
 * are exactly the same as those of another module.
 * It is acceptable to load the same module binary multiple times, where
 * each instance uses a different configuration file.  In fact, this is
 * how sd_generic works.  The instances are differentiated by module_name.
 * However, it is not reasonable to load multiple instances, all of which
 * use the same (binary, config file) pair.
 * These two rules should cover all cases adequately.
 *
 */
gboolean
module_already_requested(char *module_name, char *module_cmd,
			 char *module_cfgfile)
{
	GList *lp;

	for (lp = requested_modules; lp != NULL; lp = lp->next) {
		char **params = lp->data;
		if (strcmp(module_name, params[0]) == 0 ||
		    (strcmp(module_cmd, params[1]) == 0 &&
		     strcmp(module_cfgfile, params[2]) == 0))
			return TRUE;
	}

	return FALSE;
}

/*
 * module_add_load_request - request that a module be loaded.
 * In other words, add it to the list of modules which will be loaded
 * by module_load_requested_modules.
 * Returns: nothing.
 * Parameters:
 * module_name: the name of the module.
 * module_cmd: name of the binary associated with this module.
 * module_cfgfile: the name of the module's configuration file.
 * module_dbgfile: name of the file to which the module writes logging
 * and debugging information.
 * Note that all parameters are dynamically-allocated strings (char *),
 * and the caller relinquishes ownership of them when calling this function.
 */
void
module_add_load_request(char *module_name, char *module_cmd,
			char *module_cfgfile, char *module_dbgfile)
{
	char **module_params = NULL;

	if (module_already_requested(module_name, module_cmd, module_cfgfile)) {
		MSG(1,
		    "Load request for module %s with command %s and configuration file %s was previously received; ignoring the second one.",
		    module_name, module_cmd, module_cfgfile);
		g_free(module_name);
		g_free(module_cmd);
		g_free(module_cfgfile);
		g_free(module_dbgfile);
		return;
	}

	module_params = g_malloc(4 * sizeof(char *));
	module_params[0] = module_name;
	module_params[1] = module_cmd;
	module_params[2] = module_cfgfile;
	module_params[3] = module_dbgfile;
	requested_modules = g_list_append(requested_modules, module_params);
	MSG(5, "Module name=%s being inserted into requested_modules list",
	    module_params[0]);
}

/*
 * module_load_requested_modules: load all modules requested by calls
 * to module_add_load_request.
 * Returns: nothing.
 * Parameters: none.
 */
void module_load_requested_modules(void)
{
	while (NULL != requested_modules) {
		OutputModule *new_module;
		char **module_params = requested_modules->data;

		new_module =
		    load_output_module(module_params[0], module_params[1],
				       module_params[2], module_params[3]);

		if (new_module != NULL)
			output_modules =
			    g_list_append(output_modules, new_module);

		g_free(module_params[0]);
		g_free(module_params[1]);
		g_free(module_params[2]);
		g_free(module_params[3]);
		g_free(module_params);
		requested_modules =
		    g_list_delete_link(requested_modules, requested_modules);
	}
}

/*
 * module_number_of_requested_modules: return the current number of requested modules.
 * Returns: number of modules.
 * Parameters: none.
 */
guint module_number_of_requested_modules(void)
{
	if (requested_modules == NULL) {
		return 0;
	}
	return g_list_length(requested_modules);
}
