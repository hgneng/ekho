/*
 * speechd.c - Speech Dispatcher server program
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
 * $Id: speechd.c,v 1.81 2008-07-10 15:36:49 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <glib.h>
#include <gmodule.h>
#include <glib/gstdio.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/un.h>

#ifdef HAVE_SYS_FILIO_H
#include <sys/filio.h>	/* Needed for FIONREAD on Solaris */
#endif

#include "speechd.h"

/* Declare dotconf functions and data structures*/
#include "configuration.h"

/* Declare functions to allocate and create important data
 * structures */
#include "alloc.h"
#include "sem_functions.h"
#include "speaking.h"
#include "set.h"
#include "options.h"
#include "server.h"

#include <i18n.h>

/* list of output modules */
GList *output_modules;

/* Manipulating pid files */
int create_pid_file();
void destroy_pid_file();

/* Server socket file descriptor */
int server_socket;

GMainLoop *main_loop = NULL;
gint server_timeout_source = 0;

int client_count = 0;

struct SpeechdOptions SpeechdOptions;
struct SpeechdStatus SpeechdStatus;

pthread_t speak_thread;
pthread_mutex_t logging_mutex;
pthread_mutex_t element_free_mutex;
pthread_mutex_t output_layer_mutex;
pthread_mutex_t socket_com_mutex;

GHashTable *fd_settings;
GHashTable *language_default_modules;
GHashTable *fd_uid;

TSpeechDQueue *MessageQueue;
GList *MessagePausedList;

GList *client_specific_settings;

GList *last_p5_block;

TFDSetElement GlobalFDSet;

int speaking_pipe[2];

GHashTable *speechd_sockets_status;

FILE *logfile;
FILE *custom_logfile;
char *custom_log_kind;
FILE *debug_logfile;

TSpeechDMode spd_mode;

static gboolean speechd_client_terminate(gpointer key, gpointer value, gpointer user);
static gboolean speechd_reload_dead_modules(gpointer user_data);
static gboolean speechd_load_configuration(gpointer user_data);
static gboolean speechd_quit(gpointer user_data);

static gboolean server_process_incoming (gint          fd,
				  GIOCondition  condition,
				  gpointer      data);

static gboolean client_process_incoming (gint          fd,
				  GIOCondition  condition,
				  gpointer      data);

void check_client_count(void);

#ifndef HAVE_DAEMON
/* Added by Willie Walker - daemon is a common, but not universal, extension.
 */
static int daemon(int nochdir, int noclose)
{
	int fd, i;

	switch (fork()) {
	case 0:
		break;
	case -1:
		return -1;
	default:
		_exit(0);
	}

	if (!nochdir) {
		chdir("/");
	}

	if (setsid() < 0) {
		return -1;
	}

	if (!noclose) {
		if (fd = open("/dev/null", O_RDWR) >= 0) {
			for (i = 0; i < 3; i++) {
				dup2(fd, i);
			}
			if (fd > 2) {
				close(fd);
			}
		}
	}
	return 0;
}
#endif /* HAVE_DAEMON */

/* --- DEBUGGING --- */

/* Just to be able to set breakpoints */
void fatal_error(void)
{
	int i = 0;
	i++;
}

/* Logging messages, level of verbosity is defined between 1 and 5,
 * see documentation */
void MSG2(int level, char *kind, char *format, ...)
{
	int std_log = level <= SpeechdOptions.log_level;
	int custom_log = (kind != NULL && custom_log_kind != NULL &&
			  !strcmp(kind, custom_log_kind) &&
			  custom_logfile != NULL);

	if (std_log || custom_log) {
		va_list args;
		int i;

		pthread_mutex_lock(&logging_mutex);

		{
			{
				/* Print timestamp */
				time_t t;
				char *tstr;
				struct timeval tv;
				t = time(NULL);
				tstr = g_strdup(ctime(&t));
				gettimeofday(&tv, NULL);
				assert(tstr);
				/* Remove the trailing \n */
				assert(strlen(tstr) > 1);
				tstr[strlen(tstr) - 1] = 0;
				if (std_log) {
					fprintf(logfile, "[%s : %d] speechd: ",
						tstr, (int)tv.tv_usec);
					//            fprintf(logfile, "[test : %d] speechd: ",
					//                     (int) tv.tv_usec);
				}
				if (custom_log) {
					fprintf(custom_logfile,
						"[%s : %d] speechd: ", tstr,
						(int)tv.tv_usec);
				}
				if (SpeechdOptions.debug) {
					fprintf(debug_logfile,
						"[%s : %d] speechd: ", tstr,
						(int)tv.tv_usec);
				}
				g_free(tstr);
			}
			for (i = 1; i < level; i++) {
				if (std_log) {
					fprintf(logfile, " ");
				}
				if (custom_log) {
					fprintf(custom_logfile, " ");
				}
			}
			if (std_log) {
				va_start(args, format);
				vfprintf(logfile, format, args);
				va_end(args);
				fprintf(logfile, "\n");
				fflush(logfile);
			}
			if (custom_log) {
				va_start(args, format);
				vfprintf(custom_logfile, format, args);
				va_end(args);
				fprintf(custom_logfile, "\n");
				fflush(custom_logfile);
			}
			if (SpeechdOptions.debug) {
				va_start(args, format);
				vfprintf(debug_logfile, format, args);
				va_end(args);
				fprintf(debug_logfile, "\n");
				fflush(debug_logfile);
			}
		}
		pthread_mutex_unlock(&logging_mutex);
	}
}

/* The main logging function for Speech Dispatcher,
   level is between -1 and 5. 1 means the most important,
   5 less important. Loglevels after 4 can contain private
   data. -1 logs also to stderr. See Speech Dispatcher
   documentation */
/* TODO: Define this in terms of MSG somehow. I don't
   know how to pass '...' arguments to another C function */
void MSG(int level, char *format, ...)
{

	if ((level <= SpeechdOptions.log_level)
	    || (SpeechdOptions.debug)) {
		va_list args;
		int i;
		pthread_mutex_lock(&logging_mutex);
		{
			/* Print timestamp */
			{
				time_t t;
				char *tstr;
				struct timeval tv;
				t = time(NULL);
				tstr = g_strdup(ctime(&t));
				gettimeofday(&tv, NULL);
				/* Remove the trailing \n */
				assert(tstr);
				assert(strlen(tstr) > 1);
				assert((level >= -1) && (level <= 5));
				tstr[strlen(tstr) - 1] = 0;
				/* Write timestamps */
				if (level <= SpeechdOptions.log_level)
					fprintf(logfile, "[%s : %d] speechd: ",
						tstr, (int)tv.tv_usec);
				if (SpeechdOptions.debug)
					fprintf(debug_logfile,
						"[%s : %d] speechd: ", tstr,
						(int)tv.tv_usec);
				/*                fprintf(logfile, "[%s : %d] speechd: ",
				   tstr, (int) tv.tv_usec); */
				g_free(tstr);
			}

			for (i = 1; i < level; i++) {
				fprintf(logfile, " ");
			}
			/* Log to ordinary logfile */
			if (level <= SpeechdOptions.log_level) {
				va_start(args, format);
				vfprintf(logfile, format, args);
				va_end(args);
				fprintf(logfile, "\n");
				fflush(logfile);
			}
			/* Log into debug logfile */
			if (SpeechdOptions.debug) {
				va_start(args, format);
				vfprintf(debug_logfile, format, args);
				va_end(args);
				fprintf(debug_logfile, "\n");
				fflush(debug_logfile);
			}
			/* Log also into stderr for loglevel -1 */
			if (level == -1) {
				va_start(args, format);
				vfprintf(stderr, format, args);
				va_end(args);
				fprintf(stderr, "\n");
				fflush(stderr);
			}
		}
		pthread_mutex_unlock(&logging_mutex);
	}
}

/* --- CLIENTS / CONNECTIONS MANAGING --- */

/* Initialize sockets status table */
int speechd_sockets_status_init(void)
{
	speechd_sockets_status = g_hash_table_new_full(g_int_hash, g_int_equal,
						       (GDestroyNotify) g_free,
						       (GDestroyNotify)
						       speechd_socket_free);
	if (speechd_sockets_status)
		return 0;
	else
		return 1;
}

/* Register a new socket for SSIP connection */
int speechd_socket_register(int fd)
{
	int *fd_key;
	TSpeechDSock *speechd_socket;
	speechd_socket = g_malloc(sizeof(TSpeechDSock));
	speechd_socket->o_buf = NULL;
	speechd_socket->o_bytes = 0;
	speechd_socket->awaiting_data = 0;
	speechd_socket->inside_block = 0;
	fd_key = g_malloc(sizeof(int));
	*fd_key = fd;
	g_hash_table_insert(speechd_sockets_status, fd_key, speechd_socket);
	return 0;
}

/* Free a TSpeechDSock structure including it's data */
void speechd_socket_free(TSpeechDSock * speechd_socket)
{
	if (speechd_socket->o_buf)
		g_string_free(speechd_socket->o_buf, 1);
	g_free(speechd_socket);
}

/* Unregister a socket for SSIP communication */
int speechd_socket_unregister(int fd)
{
	return (!g_hash_table_remove(speechd_sockets_status, &fd));
}

/* Get a pointer to the TSpeechDSock structure for a given file descriptor */
TSpeechDSock *speechd_socket_get_by_fd(int fd)
{
	return g_hash_table_lookup(speechd_sockets_status, &fd);
}

/* activity is on server_socket (request for a new connection) */
int speechd_connection_new(int server_socket)
{
	TFDSetElement *new_fd_set;
	struct sockaddr_in client_address;
	unsigned int client_len = sizeof(client_address);
	int client_socket;
	int *p_client_socket, *p_client_uid, *p_client_uid2;

	client_socket =
	    accept(server_socket, (struct sockaddr *)&client_address,
		   &client_len);

	if (client_socket == -1) {
		MSG(2,
		    "Error: Can't handle connection request of a new client");
		return -1;
	}

	/* We add the associated client_socket to the descriptor set. */
	if (client_socket > SpeechdStatus.max_fd)
		SpeechdStatus.max_fd = client_socket;
	MSG(4, "Adding client on fd %d", client_socket);

	speechd_socket_register(client_socket);

	/* Create a record in fd_settings */
	new_fd_set = (TFDSetElement *) default_fd_set();
	if (new_fd_set == NULL) {
		MSG(2,
		    "Error: Failed to create a record in fd_settings for the new client");
		if (SpeechdStatus.max_fd == client_socket)
			SpeechdStatus.max_fd--;
		return -1;
	}
	new_fd_set->fd = client_socket;
	new_fd_set->uid = ++SpeechdStatus.max_uid;
	p_client_socket = (int *)g_malloc(sizeof(int));
	p_client_uid = (int *)g_malloc(sizeof(int));
	p_client_uid2 = (int *)g_malloc(sizeof(int));
	*p_client_socket = client_socket;
	*p_client_uid = SpeechdStatus.max_uid;
	*p_client_uid2 = SpeechdStatus.max_uid;

	g_hash_table_insert(fd_settings, p_client_uid, new_fd_set);
	g_hash_table_insert(fd_uid, p_client_socket, p_client_uid2);

	new_fd_set->fd_source = g_unix_fd_add(client_socket, G_IO_IN, client_process_incoming, NULL);

	MSG(4, "Data structures for client on fd %d created", client_socket);

	client_count++;
	check_client_count();

	return 0;

}

int speechd_connection_destroy(int fd)
{
	TFDSetElement *fdset_element;

	/* Client has gone away and we remove it from the descriptor set. */
	MSG(4, "Removing client on fd %d", fd);

	MSG(4, "Tagging client as inactive in settings");
	fdset_element = get_client_settings_by_fd(fd);
	if (fdset_element != NULL) {
		fdset_element->fd = -1;
		fdset_element->active = 0;
		g_source_remove(fdset_element->fd_source);
		/* The fdset_element will be freed and removed from the
		   hash table as soon as the client no longer has any
		   message in the queues, check out the speak() function */
	} else if (SPEECHD_DEBUG) {
		DIE("Can't find settings for this client\n");
	}

	MSG(4, "Removing client from the fd->uid table.");

	g_hash_table_remove(fd_uid, &fd);

	speechd_socket_unregister(fd);

	MSG(4, "Closing clients file descriptor %d", fd);

	if (close(fd) != 0)
		if (SPEECHD_DEBUG)
			DIE("Can't close file descriptor associated to this client");

	if (fd == SpeechdStatus.max_fd)
		SpeechdStatus.max_fd--;

	MSG(4, "Connection closed");

	client_count--;
	check_client_count();

	return 0;
}

static gboolean speechd_client_terminate(gpointer key, gpointer value, gpointer user)
{
	TFDSetElement *set;

	set = (TFDSetElement *) value;
	if (set == NULL) {
		MSG(2, "Error: Empty connection, internal error");
		if (SPEECHD_DEBUG)
			FATAL("Internal error");
		return TRUE;
	}

	if (set->fd > 0) {
		MSG(4, "Closing connection on fd %d\n", set->fd);
		speechd_connection_destroy(set->fd);
	}
	mem_free_fdset(set);
	g_free(set);
	return TRUE;
}

/* --- OUTPUT MODULES MANAGING --- */

void speechd_modules_terminate(gpointer data, gpointer user_data)
{
	OutputModule *module;

	module = (OutputModule *) data;
	if (module == NULL) {
		MSG(2, "Error: Empty module, internal error");
		return;
	}
	unload_output_module(module);

	return;
}

void speechd_modules_reload(gpointer data, gpointer user_data)
{
	OutputModule *module;

	module = (OutputModule *) data;
	if (module == NULL) {
		MSG(2, "Empty module, internal error");
		return;
	}

	reload_output_module(module);

	return;
}

void speechd_module_debug(gpointer data, gpointer user_data)
{
	OutputModule *module;

	module = (OutputModule *) data;
	if (module == NULL) {
		MSG(2, "Empty module, internal error");
		return;
	}

	output_module_debug(module);

	return;
}

void speechd_module_nodebug(gpointer data, gpointer user_data)
{
	OutputModule *module;

	module = (OutputModule *) data;
	if (module == NULL) {
		MSG(2, "Empty module, internal error");
		return;
	}

	output_module_nodebug(module);

	return;
}

static gboolean speechd_reload_dead_modules(gpointer user_data)
{
	/* Reload dead modules */
	g_list_foreach(output_modules, speechd_modules_reload, NULL);

	/* Make sure there aren't any more child processes left */
	while (waitpid(-1, NULL, WNOHANG) > 0) ;
	return TRUE;
}

void speechd_modules_debug(void)
{
	/* Redirect output to debug for all modules */
	g_list_foreach(output_modules, speechd_module_debug, NULL);

}

void speechd_modules_nodebug(void)
{
	/* Redirect output to normal for all modules */
	g_list_foreach(output_modules, speechd_module_nodebug, NULL);
}

/* --- SPEECHD START/EXIT FUNCTIONS --- */

void speechd_options_init(void)
{
	SpeechdOptions.spawn = FALSE;
	SpeechdOptions.log_level_set = 0;
	SpeechdOptions.communication_method = NULL;
	SpeechdOptions.socket_path = NULL;
	SpeechdOptions.port_set = 0;
	SpeechdOptions.localhost_access_only_set = 0;
	SpeechdOptions.pid_file = NULL;
	SpeechdOptions.conf_file = NULL;
	SpeechdOptions.module_dir = MODULEBINDIR;
	SpeechdOptions.runtime_speechd_dir = NULL;
	SpeechdOptions.log_dir = NULL;
	SpeechdOptions.log_dir_set = 0;
	SpeechdOptions.debug = 0;
	SpeechdOptions.debug_destination = NULL;
	debug_logfile = NULL;
	spd_mode = SPD_MODE_DAEMON;
}

void speechd_init()
{
	int ret;

	SpeechdStatus.max_uid = 0;
	SpeechdStatus.max_gid = 0;

	/* Initialize inter-thread comm pipe */
	if (pipe(speaking_pipe)) {
		MSG(1, "Speaking pipe creation failed (%s)", strerror(errno));
		FATAL("Can't create pipe");
	}

	/* Initialize Speech Dispatcher priority queue */
	MessageQueue = g_malloc0(sizeof(TSpeechDQueue));
	if (MessageQueue == NULL)
		FATAL("Couldn't allocate memory for MessageQueue.");

	/* Initialize lists */
	MessagePausedList = NULL;

	/* Initialize hash tables */
	fd_settings = g_hash_table_new_full(g_int_hash, g_int_equal,
					    (GDestroyNotify) g_free, NULL);
	assert(fd_settings != NULL);

	fd_uid = g_hash_table_new_full(g_int_hash, g_int_equal,
				       (GDestroyNotify) g_free,
				       (GDestroyNotify) g_free);
	assert(fd_uid != NULL);

	language_default_modules = g_hash_table_new(g_str_hash, g_str_equal);
	assert(language_default_modules != NULL);

	speechd_sockets_status_init();

	pause_requested = 0;
	resume_requested = 0;

	/* Perform some functionality tests */
	if (g_module_supported() == FALSE)
		DIE("Loadable modules not supported by current platform.\n");

	if (_POSIX_VERSION < 199506L)
		DIE("This system doesn't support POSIX.1c threads\n");

	/* Fill GlobalFDSet with default values */
	GlobalFDSet.min_delay_progress = 2000;

	/* Initialize list of different client specific settings entries */
	client_specific_settings = NULL;

	/* Initialize mutexes, semaphores and synchronization */
	ret = pthread_mutex_init(&element_free_mutex, NULL);
	if (ret != 0)
		DIE("Mutex initialization failed");

	ret = pthread_mutex_init(&output_layer_mutex, NULL);
	if (ret != 0)
		DIE("Mutex initialization failed");

	ret = pthread_mutex_init(&socket_com_mutex, NULL);
	if (ret != 0)
		DIE("Mutex initialization failed");

	if (SpeechdOptions.log_dir == NULL) {
		SpeechdOptions.log_dir =
		    g_strdup_printf("%s/log/",
				    SpeechdOptions.runtime_speechd_dir);
		mkdir(SpeechdOptions.log_dir, S_IRWXU);
		if (!SpeechdOptions.debug_destination) {
			SpeechdOptions.debug_destination =
			    g_strdup_printf("%s/log/debug",
					    SpeechdOptions.runtime_speechd_dir);
			mkdir(SpeechdOptions.debug_destination, S_IRWXU);
		}
	}

	/* Load configuration from the config file */
	MSG(4, "Reading Speech Dispatcher configuration from %s",
	    SpeechdOptions.conf_file);
	speechd_load_configuration(NULL);

	logging_init();

	/* Check for output modules */
	if (g_list_length(output_modules) == 0) {
		DIE("No speech output modules were loaded - aborting...");
	} else {
		MSG(3, "Speech Dispatcher started with %d output module%s",
		    g_list_length(output_modules),
		    g_list_length(output_modules) > 1 ? "s" : "");
	}

	last_p5_block = NULL;
}

static gboolean speechd_load_configuration(gpointer user_data)
{
	configfile_t *configfile = NULL;
	GList *detected_modules = NULL;

	/* Clean previous configuration */
	if (output_modules != NULL) {
		g_list_foreach(output_modules, speechd_modules_terminate, NULL);
		g_list_free(output_modules);
		output_modules = NULL;
	}

	/* Make sure there aren't any more child processes left */
	while (waitpid(-1, NULL, WNOHANG) > 0) ;

	/* Load new configuration */
	load_default_global_set_options();

	spd_num_options = 0;
	spd_options = load_config_options(&spd_num_options);

	/* Add the LAST option */
	spd_options = add_config_option(spd_options, &spd_num_options, "", 0,
					NULL, NULL, 0);

	configfile =
	    dotconf_create(SpeechdOptions.conf_file, spd_options, 0,
			   CASE_INSENSITIVE);
	if (configfile) {
		configfile->includepath = g_strdup(SpeechdOptions.conf_dir);
		MSG(5, "Config file include path is: %s",
		    configfile->includepath);
		if (dotconf_command_loop(configfile) == 0)
			DIE("Error reading config file\n");
		dotconf_cleanup(configfile);
		MSG(2, "Configuration has been read from \"%s\"",
		    SpeechdOptions.conf_file);

		/* We need to load modules here, since this is called both by speechd_init
		 * and to handle SIGHUP. */
		if (module_number_of_requested_modules() < 1) {
			detected_modules = detect_output_modules(SpeechdOptions.module_dir,
								 SpeechdOptions.conf_dir);
			while (detected_modules != NULL) {
				char **parameters = detected_modules->data;
				module_add_load_request(parameters[0],
							parameters[1],
							parameters[2],
							parameters[3]);
				g_free(detected_modules->data);
				detected_modules->data = NULL;
				detected_modules =
				    g_list_delete_link(detected_modules,
						       detected_modules);
			}
		}

		module_load_requested_modules();
	} else {
		MSG(1, "Can't open %s", SpeechdOptions.conf_file);
	}

	free_config_options(spd_options, &spd_num_options);

	return TRUE;
}

static gboolean speechd_quit(gpointer user_data)
{
	g_main_loop_quit(main_loop);
	return FALSE;
}

/* --- PID FILES --- */

int create_pid_file()
{
	FILE *pid_file;
	int pid_fd;
	struct flock lock;
	int ret;

	/* If the file exists, examine it's lock */
	pid_file = fopen(SpeechdOptions.pid_file, "r");
	if (pid_file != NULL) {
		pid_fd = fileno(pid_file);

		lock.l_type = F_WRLCK;
		lock.l_whence = SEEK_SET;
		lock.l_start = 1;
		lock.l_len = 3;

		/* If there is a lock, exit, otherwise remove the old file */
		ret = fcntl(pid_fd, F_GETLK, &lock);
		if (ret == -1) {
			MSG(-1,
			    "Can't check lock status of an existing pid file.\n");
			return -1;
		}

		fclose(pid_file);
		if (lock.l_type != F_UNLCK) {
			MSG(-1, "Speech Dispatcher already running.\n");
			return -1;
		}

		unlink(SpeechdOptions.pid_file);
	}

	/* Create a new pid file and lock it */
	pid_file = fopen(SpeechdOptions.pid_file, "w");
	if (pid_file == NULL) {
		MSG(-1, "Can't create pid file in %s, wrong permissions?\n",
		    SpeechdOptions.pid_file);
		return -1;
	}
	fprintf(pid_file, "%d\n", getpid());
	fflush(pid_file);

	pid_fd = fileno(pid_file);
	lock.l_type = F_WRLCK;
	lock.l_whence = SEEK_SET;
	lock.l_start = 1;
	lock.l_len = 3;

	ret = fcntl(pid_fd, F_SETLK, &lock);
	if (ret == -1) {
		MSG(-1, "Can't set lock on pid file.\n");
		return -1;
	}

	return 0;
}

void destroy_pid_file(void)
{
	unlink(SpeechdOptions.pid_file);
}

void logging_init(void)
{
	char *file_name =
	    g_strdup_printf("%s/speech-dispatcher.log", SpeechdOptions.log_dir);
	assert(file_name != NULL);
	if (!strncmp(file_name, "stdout", 6)) {
		logfile = stdout;
	} else if (!strncmp(file_name, "stderr", 6)) {
		logfile = stderr;
	} else {
		logfile = fopen(file_name, "a");
		if (logfile == NULL) {
			fprintf(stderr,
				"Error: can't open logging file %s! Using stdout.\n",
				file_name);
			logfile = stdout;
		} else {
			MSG(3, "Speech Dispatcher Logging to file %s",
			    file_name);
		}
	}

	if (!debug_logfile)
		debug_logfile = stdout;

	g_free(file_name);
	return;
}

/* --- Sockets --- */

int make_local_socket(const char *filename)
{
	struct sockaddr_un name;
	int sock;
	size_t size;

	/* Create the socket. */
	sock = socket(AF_UNIX, SOCK_STREAM, 0);
	if (sock < 0) {
		FATAL("Can't create local socket");
	}

	/* Bind a name to the socket. */
	name.sun_family = AF_UNIX;
	strncpy(name.sun_path, filename, sizeof(name.sun_path));
	name.sun_path[sizeof(name.sun_path) - 1] = '\0';
	size = SUN_LEN(&name);

	if (bind(sock, (struct sockaddr *)&name, size) < 0) {
		FATAL("Can't bind local socket");
	}

	if (listen(sock, 50) == -1) {
		MSG(2, "listen failed: ERRNO:%s", strerror(errno));
		FATAL("listen() failed for local socket");
	}

	return sock;
}

int make_inet_socket(const int port)
{
	struct sockaddr_in server_address;
	int server_socket;

	/* Create an inet socket */
	server_socket = socket(AF_INET, SOCK_STREAM, 0);
	if (server_socket < 0) {
		FATAL("Can't create inet socket");
	}

	/* Set REUSEADDR flag */
	const int flag = 1;
	if (setsockopt(server_socket, SOL_SOCKET, SO_REUSEADDR, &flag,
		       sizeof(int)))
		MSG(2, "Error: Setting socket option failed!");

	server_address.sin_family = AF_INET;

	/* Enable access only to localhost or for any address
	   based on LocalhostAccessOnly configuration option. */
	if (SpeechdOptions.localhost_access_only)
		server_address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
	else
		server_address.sin_addr.s_addr = htonl(INADDR_ANY);

	server_address.sin_port = htons(port);

	MSG(4, "Opening inet socket connection");
	if (bind(server_socket, (struct sockaddr *)&server_address,
		 sizeof(server_address)) == -1) {
		MSG(-1, "bind() failed: %s", strerror(errno));
		FATAL("Couldn't open inet socket, try a few minutes later.");
	}

	if (listen(server_socket, 50) == -1) {
		MSG(2, "ERRNO:%s", strerror(errno));
		FATAL
		    ("listen() failed for inet socket, another Speech Dispatcher running?");
	}

	return server_socket;
}

gboolean server_process_incoming (gint          fd,
				  GIOCondition  condition,
				  gpointer      data)
{
	int ret;

	ret = speechd_connection_new(fd);
	if (ret != 0) {
		MSG(2, "Error: Failed to add new client!");
		if (SPEECHD_DEBUG) {
			FATAL("Failed to add new client");
		}
	}

	return TRUE;
}

gboolean client_process_incoming (gint          fd,
				  GIOCondition  condition,
				  gpointer      data)
{
	int ret;
	int nread;

	ioctl(fd, FIONREAD, &nread);

	if (nread == 0) {
		/* client has gone */
		ret = speechd_connection_destroy(fd);
		if (ret != 0) {
			MSG(2, "Error: Failed to close the client!");
		}
		return FALSE;
	}

	/* client sends some commands or data */
	if (serve(fd) == -1) {
		MSG(2, "Error: Failed to serve client on fd %d!", fd);
	}

	return TRUE;
}

void check_client_count(void)
{
	if (client_count <= 0
	    && SpeechdOptions.server_timeout > 0) {
		MSG(4, "Currently no clients connected, enabling shutdown timer.");
		server_timeout_source = 
		                        g_timeout_add_seconds(
		                        SpeechdOptions.server_timeout,
		                        speechd_quit, NULL);
	} else {
	MSG(4, "Clients connected, disabling shutdown timer.");
		g_source_remove(server_timeout_source);
	}
}

/* --- MAIN --- */

int main(int argc, char *argv[])
{
	int ret;
	/* Autospawn helper variables */
	char *spawn_communication_method = NULL;
	int spawn_port = 0;
	char *spawn_socket_path = NULL;

	/* Strip all permisions for 'others' from the files created */
	umask(007);

	/* Initialize logging */
	logfile = stdout;
	SpeechdOptions.log_level = 1;
	custom_logfile = NULL;
	custom_log_kind = NULL;

	/* initialize i18n support */
	i18n_init();

	speechd_options_init();

	options_parse(argc, argv);

	if (SpeechdOptions.spawn) {
		/* In case of --spawn, copy the host port and socket_path
		   parameters into temporary spawn_ variables for later comparison
		   with the config file and unset them */
		if (SpeechdOptions.communication_method_set) {
			spawn_communication_method =
			    g_strdup(SpeechdOptions.communication_method);
			g_free(SpeechdOptions.communication_method);
			SpeechdOptions.communication_method_set = 0;
		}
		if (SpeechdOptions.port_set) {
			spawn_port = SpeechdOptions.port;
			SpeechdOptions.port_set = 0;
		}
		if (SpeechdOptions.socket_path_set) {
			spawn_socket_path =
			    g_strdup(SpeechdOptions.socket_path);
			g_free(SpeechdOptions.socket_path);
			SpeechdOptions.socket_path_set = 0;
		}
	}

	MSG(1, "Speech Dispatcher " VERSION " starting");

	/* By default, search for configuration options in $XDG_CONFIG_HOME/speech-dispatcher
	   and sockets and pid files in $XDG_RUNTIME_DIR/speech-dispatcher */
	{
		const char *user_runtime_dir;
		const char *user_config_dir;
		char *test_speechd_conf_file = NULL;

		user_runtime_dir = g_get_user_runtime_dir();
		user_config_dir = g_get_user_config_dir();

		/* Setup a speechd-dispatcher directory or create a new one */
		SpeechdOptions.runtime_speechd_dir =
		    g_strdup_printf("%s/speech-dispatcher", user_runtime_dir);
		MSG(4, "Trying to find %s", SpeechdOptions.runtime_speechd_dir);
		g_mkdir_with_parents(SpeechdOptions.runtime_speechd_dir,
				     S_IRWXU);
		MSG(4, "Using directory: %s for pidfile and logging",
		    SpeechdOptions.runtime_speechd_dir);
		/* Pidfile */
		if (SpeechdOptions.pid_file == NULL) {
			/* If no pidfile path specified on command line, use default local dir */
			SpeechdOptions.pid_file =
			    g_strdup_printf("%s/pid/speech-dispatcher.pid",
					    SpeechdOptions.runtime_speechd_dir);
			g_mkdir(g_path_get_dirname(SpeechdOptions.pid_file),
				S_IRWXU);
		}
		/* Config file */
		if (SpeechdOptions.conf_dir == NULL) {
			/* If no conf_dir was specified on command line, try default local config dir */
			SpeechdOptions.conf_dir =
			    g_build_filename(user_config_dir,
					     "speech-dispatcher", NULL);
			test_speechd_conf_file =
			    g_build_filename(SpeechdOptions.conf_dir,
					     "speechd.conf", NULL);
			if (!g_file_test
			    (test_speechd_conf_file, G_FILE_TEST_IS_REGULAR)) {
				/* If the local configuration file doesn't exist, read the global configuration */
				if (strcmp(SYS_CONF, ""))
					SpeechdOptions.conf_dir =
					    g_strdup(SYS_CONF);
				else
					SpeechdOptions.conf_dir =
					    g_strdup("/etc/speech-dispatcher/");
			}
			g_free(test_speechd_conf_file);
		}
		SpeechdOptions.conf_file =
		    g_strdup_printf("%s/speechd.conf", SpeechdOptions.conf_dir);
	}

	/* Check for PID file or create a new one or exit if Speech Dispatcher
	   is already running */
	if (create_pid_file() != 0)
		exit(1);

	/* Handle --spawn request */
	if (SpeechdOptions.spawn) {
		/* Check whether spawning is not disabled */
		gchar *config_contents;
		int err;
		GRegex *regexp;
		int result;

		err =
		    g_file_get_contents(SpeechdOptions.conf_file,
					&config_contents, NULL, NULL);
		if (err == FALSE) {
			MSG(-1, "Error opening %s", SpeechdOptions.conf_file);
			FATAL("Can't open conf file");
		}
		regexp =
		    g_regex_new("^[ ]*DisableAutoSpawn", G_REGEX_MULTILINE, 0,
				NULL);
		result = g_regex_match(regexp, config_contents, 0, NULL);
		if (result) {
			MSG(-1,
			    "Autospawn requested but disabled in configuration");
			exit(1);
		}
		g_free(config_contents);
		g_regex_unref(regexp);
		MSG(2, "Starting Speech Dispatcher due to auto-spawn");
	}

	/* Initialize logging mutex to workaround ctime threading bug */
	/* Must be done no later than here */
	ret = pthread_mutex_init(&logging_mutex, NULL);
	if (ret != 0) {
		fprintf(stderr, "Mutex initialization failed");
		exit(1);
	}

	speechd_init();

	/* Handle socket_path 'default' */
	// TODO: This is a hack, we should do that at appropriate places...
	if (!strcmp(SpeechdOptions.socket_path, "default")) {
		/* This code cannot be moved above next to conf_dir and pidpath resolution because
		   we need to also consider the DotConf configuration, which is read in speechd_init() */
		GString *socket_filename;
		socket_filename = g_string_new("");
		if (SpeechdOptions.runtime_speechd_dir) {
			g_string_printf(socket_filename, "%s/speechd.sock",
					SpeechdOptions.runtime_speechd_dir);
		} else {
			FATAL
			    ("Socket name file not set and user has no runtime directory");
		}
		g_free(SpeechdOptions.socket_path);
		SpeechdOptions.socket_path = g_strdup(socket_filename->str);
		g_string_free(socket_filename, 1);
	}

	/* Check if the communication method corresponds to the spawn request */
	/* TODO: This should preferably be done much sooner, but the current
	   configuration mechanism doesn't allow it */
	if (SpeechdOptions.spawn) {
		if (spawn_communication_method) {
			if (strcmp
			    (spawn_communication_method,
			     SpeechdOptions.communication_method)) {
				MSG(-1,
				    "Autospawn failed: Mismatch in communication methods. Client "
				    "requests %s, most probably due to its configuration or the value of "
				    "the SPEECHD_ADDRESS environment variable, but the server is configured "
				    "to provide the %s method.",
				    spawn_communication_method,
				    SpeechdOptions.communication_method);
				exit(1);
			} else {
				if (!strcmp
				    (SpeechdOptions.communication_method,
				     "inet_socket")) {
					/* Check port */
					if (spawn_port != 0)
						if (spawn_port !=
						    SpeechdOptions.port) {
							MSG(-1,
							    "Autospawn failed: Mismatch in port numbers. Server "
							    "is configured to use the inet_socket method on the port %d "
							    "while the client requests port %d, most probably due to its "
							    "configuration or the value of the SPEECHD_ADDRESS environment "
							    "variable.",
							    SpeechdOptions.port,
							    spawn_port);
							exit(1);
						}
				} else if (!strcmp
					   (SpeechdOptions.communication_method,
					    "unix_socket")) {
					/* Check socket name */
					if (spawn_socket_path)
						if (strcmp
						    (spawn_socket_path,
						     SpeechdOptions.socket_path))
						{
							MSG(-1,
							    "Autospawn failed: Mismatch in socket names. The server "
							    "is configured to provide a socket interface in %s, but the "
							    "client requests a different path: %s. This is most probably "
							    "due to the client application configuration or the value of "
							    "the SPEECHD_ADDRESS environment variable.",
							    SpeechdOptions.socket_path,
							    spawn_socket_path);
							exit(1);
						}
				} else
					assert(0);
			}
		}
		g_free(spawn_communication_method);
		g_free(spawn_socket_path);
	}

	if (!strcmp(SpeechdOptions.communication_method, "inet_socket")) {
		MSG(4, "Speech Dispatcher will use inet port %d",
		    SpeechdOptions.port);
		/* Connect and start listening on inet socket */
		server_socket = make_inet_socket(SpeechdOptions.port);
	} else if (!strcmp(SpeechdOptions.communication_method, "unix_socket")) {
		/* Determine appropariate socket file name */
		MSG(4, "Speech Dispatcher will use local unix socket: %s",
		    SpeechdOptions.socket_path);
		/* Delete an old socket file if it exists */
		if (g_file_test(SpeechdOptions.socket_path, G_FILE_TEST_EXISTS))
			if (g_unlink(SpeechdOptions.socket_path) == -1)
				FATAL
				    ("Local socket file exists but impossible to delete. Wrong permissions?");
		/* Connect and start listening on local unix socket */
		server_socket = make_local_socket(SpeechdOptions.socket_path);
	} else {
		FATAL("Unknown communication method");
	}

	/* Fork, set uid, chdir, etc. */
	if (spd_mode == SPD_MODE_DAEMON) {
		if (daemon(0, 0)) {
			FATAL("Can't fork child process");
		}
		/* Re-create the pid file under this process */
		unlink(SpeechdOptions.pid_file);
		if (create_pid_file() == -1)
			return -1;
	}

	/* Set up the main loop and register signals */
        main_loop = g_main_loop_new(g_main_context_default(), FALSE);
	g_unix_signal_add(SIGINT, speechd_quit, NULL);
	g_unix_signal_add(SIGTERM, speechd_quit, NULL);
	g_unix_signal_add(SIGHUP, speechd_load_configuration, NULL);
	g_unix_signal_add(SIGUSR1, speechd_reload_dead_modules, NULL);
	(void)signal(SIGPIPE, SIG_IGN);

	MSG(4, "Creating new thread for speak()");
	ret = pthread_create(&speak_thread, NULL, speak, NULL);
	if (ret != 0)
		FATAL("Speak thread failed!\n");

	SpeechdStatus.max_fd = server_socket;

	g_unix_fd_add(server_socket, G_IO_IN,
		      server_process_incoming, NULL);

	/* Now wait for clients and requests. */
	MSG(1, "Speech Dispatcher started and waiting for clients ...");

	check_client_count();

	g_main_loop_run(main_loop);

	MSG(1, "Terminating...");

	MSG(2, "Closing open connections...");
	/* We will browse through all the connections and close them. */
	g_hash_table_foreach_remove(fd_settings, speechd_client_terminate,
				    NULL);
	g_hash_table_destroy(fd_settings);

	MSG(4, "Closing speak() thread...");
	ret = pthread_cancel(speak_thread);
	if (ret != 0)
		FATAL("Speak thread failed to cancel!\n");

	ret = pthread_join(speak_thread, NULL);
	if (ret != 0)
		FATAL("Speak thread failed to join!\n");

	MSG(2, "Closing open output modules...");
	/*  Call the close() function of each registered output module. */
	g_list_foreach(output_modules, speechd_modules_terminate, NULL);
	g_list_free(output_modules);

	MSG(2, "Closing server connection...");
	if (close(server_socket) == -1)
		MSG(2, "close() failed: %s", strerror(errno));

	MSG(4, "Removing pid file");
	destroy_pid_file();

	fflush(NULL);

	g_main_loop_unref(main_loop);
	main_loop = NULL;

	MSG(2, "Speech Dispatcher terminated correctly");

	exit(0);
}

void check_locked(pthread_mutex_t * lock)
{
	if (pthread_mutex_trylock(lock) == 0) {
		MSG(1,
		    "CRITICAL ERROR: Not locked but accessing structure data!");
		fprintf(stderr, "WARNING! WARNING! MUTEX CHECK FAILED!\n");
		fflush(stderr);
		exit(0);
	}
}
