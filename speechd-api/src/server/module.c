
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
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $Id: module.c,v 1.40 2008-07-07 14:30:51 hanke Exp $
 */

#define _GNU_SOURCE

#include <sys/wait.h>
#include <sys/stat.h>
#include <stdio.h>
#include "speechd.h"
#include "output.h"

#if !(defined(__GLIBC__) && defined(_GNU_SOURCE))
/* Added by Willie Walker - TEMP_FAILURE_RETRY, strndup, and getline
 * are gcc-isms
 */
ssize_t getline (char **lineptr, size_t *n, FILE *f);
#endif

void
destroy_module(OutputModule *module)
{
    spd_free(module->name);
    spd_free(module->filename);
    spd_free(module->configfilename);
    spd_free(module);
}

OutputModule*
load_output_module(char* mod_name, char* mod_prog, char* mod_cfgfile, char* mod_dbgfile)
{
    OutputModule *module;
    int fr;
    char *arg1 = NULL;
    int cfg = 0;
    int ret;
    char *module_conf_dir;

    if (mod_name == NULL) return NULL;
    
    module = (OutputModule*) spd_malloc(sizeof(OutputModule));

    module->name = (char*) spd_strdup(mod_name);
    module->filename = (char*) spd_get_path(mod_prog, MODULEBINDIR);    
    
    module_conf_dir = g_strdup_printf("%s/modules/",
				      SpeechdOptions.conf_dir);

    module->configfilename = (char*) spd_get_path(mod_cfgfile, module_conf_dir);
    g_free(module_conf_dir);

    if (mod_dbgfile != NULL) module->debugfilename = strdup(mod_dbgfile);
    else module->debugfilename = NULL;

    if (!strcmp(mod_name, "testing")){
        module->pipe_in[1] = 1; /* redirect to stdin */
        module->pipe_out[0] = 0; /* redirect to stdout */
        return module;
    }

    if (  (pipe(module->pipe_in) != 0) 
          || ( pipe(module->pipe_out) != 0)  ){
        MSG(3, "Can't open pipe! Module not loaded.");
        return NULL;
    }     

    if (mod_cfgfile){
        arg1 = g_strdup_printf("%s", module->configfilename);
        cfg=1;
    }

    /* Open the file for child stderr (logging) redirection */
    if (module->debugfilename != NULL){
	module->stderr_redirect = open(module->debugfilename,
				       O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR);
	if (module->stderr_redirect == -1)
	    MSG(1,"ERROR: Openning debug file for %s failed: (error=%d) %s", module->name,
		module->stderr_redirect, strerror(errno));    
    }else{
	module->stderr_redirect = -1;
    }

    MSG(2,"Initializing output module %s with binary %s and configuration %s",
        module->name, module->filename, module->configfilename);    
    if (module->stderr_redirect >= 0)
	MSG(3,"Output module is logging to file %s", module->debugfilename);    
    else
	MSG(3,"Output module is logging to standard error output (stderr)");    


    fr = fork();
    switch(fr){
    case -1: printf("Can't fork, error! Module not loaded."); return NULL;
    case 0:
        ret = dup2(module->pipe_in[0], 0);
        close(module->pipe_in[0]);
        close(module->pipe_in[1]);
 
        ret = dup2(module->pipe_out[1], 1);
        close(module->pipe_out[1]);
        close(module->pipe_out[0]);        

	/* Redirrect stderr to the appropriate logfile */
	if (module->stderr_redirect >= 0 ){
	    ret = dup2(module->stderr_redirect, 2);
	}

        if (cfg == 0){
            if (execlp(module->filename, "", (char *) 0) == -1){
                exit(1);
            }
        }else{
	    //if (execlp("valgrind", "" ,"--trace-children=yes", module->filename, arg1, arg2, (char *) 0) == -1){
	    if (execlp(module->filename, "", arg1, (char *) 0) == -1){
                exit(1);
            }
        }
        assert(0);
    default:

	if (cfg) spd_free(arg1);

        module->pid = fr;
        close(module->pipe_in[0]);
        close(module->pipe_out[1]);

        usleep(100);            /* So that the other child has at least time to fail
                                   with the execlp */
        ret = waitpid(module->pid, NULL, WNOHANG);
        if (ret != 0){
            MSG(2, "ERROR: Can't load output module %s with binary %s. Bad filename in configuration?", 
		module->name, module->filename);
            destroy_module(module);
            return NULL;
        }

        module->working = 1;
        MSG(2, "Module %s loaded.", module->name);        

	/* Create a stream from the socket */
	module->stream_out = fdopen(module->pipe_out[0], "r");
	if (!module->stream_out) FATAL("Can't create a stream for socket, fdopen() failed.");
	/* Switch to line buffering mode */
	ret = setvbuf(module->stream_out, NULL, _IONBF, 4096);
	if (ret) FATAL("Can't set line buffering, setvbuf failed.");

	MSG(4, "Trying to initialize %s.", module->name);
	if (output_send_data("INIT\n", module, 0) != 0){
	    MSG(1, "ERROR: Something wrong with %s, can't initialize", module->name);
	    output_close(module);
	    return NULL;
	}else{
	    char *rep_line = malloc(1024);
	    FILE *f;
	    size_t n = 1024;
	    char s;
	    GString *reply;

	    reply = g_string_new("\n---------------\n");
	    f = fdopen(dup(module->pipe_out[0]), "r");
	    while(1){
		ret = getline(&rep_line, &n, f);
		if (ret <= 0){
		    MSG(1, "ERROR: Bad syntax from output module %s 1", module->name);
		    return NULL;
		}
		assert(rep_line != NULL);
		MSG(5, "Reply from output module: %d %s", n, rep_line);
		if (strlen(rep_line) <= 4){
		    MSG(1, "ERROR: Bad syntax from output module %s 2", module->name);
		    return NULL;
		}

		if (rep_line[3] == '-') g_string_append(reply, rep_line + 4);
		else{
		    s = rep_line[0];
		    spd_free(rep_line);
		    break;
		}

	    }

	    if (SpeechdOptions.debug){
	      MSG(4, "Switching debugging on for output module %s", module->name);
	      output_module_debug(module);
	    }
	    /* Initialize audio settings */
	    ret = output_send_audio_settings(module);
	    if (ret !=0){
	      MSG(1, "ERROR: Can't initialize audio in output module, see reason above.");
	      module->working = 0;
	      kill(module->pid, 9);
	      waitpid(module->pid, NULL, WNOHANG);
	      destroy_module(module);
	      return NULL;
	    }

	    /* Send log level configuration setting */
	    ret = output_send_loglevel_setting(module);
	    if (ret !=0){
	      MSG(1, "ERROR: Can't set the log level inin the output module.");
	      module->working = 0;
	      kill(module->pid, 9);
	      waitpid(module->pid, NULL, WNOHANG);
	      destroy_module(module);
	      return NULL;
	    }

	    /* Get a list of supported voices */
	    _output_get_voices(module);
	    fclose(f);
	    g_string_append_printf(reply, "---------------\n");

	    if (s == '2') MSG(2, "Module %s started sucessfully with message: %s", module->name, reply->str);
	    else if (s == '3'){
		MSG(1, "ERROR: Module %s failed to initialize. Reason: %s", module->name, reply->str);
		module->working = 0;
		kill(module->pid, 9);
		waitpid(module->pid, NULL, WNOHANG);
		destroy_module(module);
		return NULL;
	    }
	    g_string_free(reply, 1);
	}

        return module;
    }

    assert(0);
}

int
unload_output_module(OutputModule *module)
{
    assert(module != NULL);

    MSG(3,"Unloading module name=%s", module->name);

    output_close(module);

    close(module->pipe_in[1]);
    close(module->pipe_out[0]);

    destroy_module(module);    

    return 0;
}

int
reload_output_module(OutputModule *old_module)
{
    OutputModule *new_module;

    assert(old_module != NULL); assert(old_module->name != NULL);

    if (old_module->working) return 0;

    MSG(3, "Reloading output module %s", old_module->name);    

    output_close(old_module);
    close(old_module->pipe_in[1]);
    close(old_module->pipe_out[0]);

    new_module = load_output_module(old_module->name, old_module->filename,
                                    old_module->configfilename, 
				    old_module->debugfilename);
    if (new_module == NULL){
        MSG(3, "Can't load module %s while reloading modules.", 
	    old_module->name);
        return -1;
    }

    g_hash_table_replace(output_modules, new_module->name, new_module);
    destroy_module(old_module);

    return 0;
}


int
output_module_debug(OutputModule *module)
{
    char *new_log_path;

    assert(module != NULL); assert(module->name != NULL);
    if (!module->working) return -1;

    MSG(4, "Output module debug logging for %s into %s", module->name,
	SpeechdOptions.debug_destination);    
    
    new_log_path = g_strdup_printf("%s/%s.log", 
				   SpeechdOptions.debug_destination,
				   module->name);
    
    output_send_debug(module, 1, new_log_path);
    
    return 0;
}


int
output_module_nodebug(OutputModule *module)
{
    assert(module != NULL); assert(module->name != NULL);
    if (!module->working) return -1;

    MSG(4, "Output module debug logging off for", module->name);
    
    output_send_debug(module, 0, NULL);
    
    return 0;
}
