
/*
 * speechd.h - Speech Dispatcher header
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
 * $Id: speechd.h,v 1.64 2008-06-27 12:29:07 hanke Exp $
 */

#ifndef SPEECHDH
#define SPEECHDH

#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/wait.h>

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <limits.h>
#include <signal.h>
#include <assert.h>

#include <pthread.h>

#include <glib.h>
#include <glib-unix.h>

#include <semaphore.h>
#include <sys/ipc.h>
#include <sys/sem.h>

#define SPEECHD_DEBUG 0

/* Definition of semun needed for semaphore manipulation */
/* TODO: This fixes compilation for Mac OS X but might not be a correct
   solution for other platforms. A better check is needed, possibly including
   _POSIX_C_SOURCE and friends*/
#if (defined(__GNU_LIBRARY__) && !defined(_SEM_SEMUN_UNDEFINED)) || defined(__APPLE__)
/* union semun is defined by including <sys/sem.h> */
#else
/* according to X/OPEN we have to define it ourselves */
union semun {
	int val;		/* value for SETVAL */
	struct semid_ds *buf;	/* buffer for IPC_STAT, IPC_SET */
	unsigned short int *array;	/* array for GETALL, SETALL */
	struct seminfo *__buf;	/* buffer for IPC_INFO */
};
#endif

#include <speechd_types.h>
#include "module.h"
#include "compare.h"

typedef struct {
	unsigned int uid;	/* Unique ID of the client */
	int fd;			/* File descriptor the client is on. */
	guint fd_source;	/* Used to store the GSource ID for watching fd activity in the main loop */
	int active;		/* Is this client still active on socket or gone? */
	int paused;		/* Internal flag, 1 for paused client or 0 for normal. */
	int paused_while_speaking;
	SPDMessageType type;	/* Type of the message (1=text, 2=icon, 3=char, 4=key) */
	SPDDataMode ssml_mode;	/* SSML mode on (1)/off (0) */
	gboolean symbols_preprocessing; /* whether to transform symbols/punctuation in the input */
	SPDPriority priority;	/* Priority between 1 and 5 (1 - highest, 5 - lowest) */
	SPDMsgSettings msg_settings;
	char *client_name;	/* Name of the client. */
	char *output_module;	/* Output module name. (e.g. "festival", "flite", "apollo", ...) */

	SPDNotification notification;	/* Notification about start and stop of messages, about reached
					   index marks and state (canceled, paused, resumed). */

	int reparted;
	unsigned int min_delay_progress;
	int pause_context;	/* Number of words that should be repeated after a pause */
	char *index_mark;	/* Current index mark for the message (only if paused) */

	char *audio_output_method;
	char *audio_oss_device;
	char *audio_alsa_device;
	char *audio_nas_server;
	char *audio_pulse_server;
	int audio_pulse_min_length;
	int log_level;

	/* TODO: Should be moved out */
	unsigned int hist_cur_uid;
	int hist_cur_pos;
	ESort hist_sorted;

} TFDSetElement;

typedef struct {
	char *pattern;
	TFDSetElement val;
} TFDSetClientSpecific;

/* Size of the buffer for socket communication */
#define BUF_SIZE 128

/* Mode of speechd execution */
typedef enum {
	SPD_MODE_DAEMON,	/* Run as daemon (background, ...) */
	SPD_MODE_SINGLE		/*  */
} TSpeechDMode;

extern TSpeechDMode spd_mode;

/*  TSpeechDQueue is a queue for messages. */
typedef struct {
	GList *p1;		/* important */
	GList *p2;		/* text */
	GList *p3;		/* message */
	GList *p4;		/* notification */
	GList *p5;		/* progress */
} TSpeechDQueue;

/*  TSpeechDMessage is an element of TSpeechDQueue,
    that is, some text with or without index marks
    inside  and it's configuration. */
typedef struct {
	guint id;		/* unique id */
	time_t time;		/* when was this message received */
	char *buf;		/* the actual text */
	int bytes;		/* number of bytes in buf */
	TFDSetElement settings;	/* settings of the client when queueing this message */
} TSpeechDMessage;

#include "alloc.h"
#include "speaking.h"

extern struct SpeechdOptions {
	char *communication_method;
	int communication_method_set;
	char *socket_path;
	int socket_path_set;
	int port, port_set;
	int localhost_access_only, localhost_access_only_set;
	int log_level, log_level_set;
	char *pid_file;
	char *conf_file;
	char *conf_dir;
	char *runtime_speechd_dir;
	char *log_dir;
	char *module_dir;
	int log_dir_set;
	int spawn;
	int debug;
	char *debug_destination;
	char *debug_logfile;
	int max_history_messages;	/* Maximum of messages in history before they expire */
	int server_timeout;
	int server_timeout_set;
} SpeechdOptions;

extern struct SpeechdStatus {
	int max_uid;		/* The largest assigned uid + 1 */
	int max_gid;		/* The largest assigned gid + 1 */
	int max_fd;
} SpeechdStatus;

/* speak() thread defined in speaking.c */
extern pthread_t speak_thread;
extern pthread_mutex_t logging_mutex;
extern pthread_mutex_t element_free_mutex;
extern pthread_mutex_t output_layer_mutex;
extern pthread_mutex_t socket_com_mutex;

/* Table of all configured (and succesfully loaded) output modules */
extern GList *output_modules;

/* Table of settings for each active client (=each active socket)*/
extern GHashTable *fd_settings;
/* Table of default output modules for different languages */
extern GHashTable *language_default_modules;
/* Table of relations between client file descriptors and their uids */
extern GHashTable *fd_uid;

/* Speech Dispatcher main priority queue for messages */
extern TSpeechDQueue *MessageQueue;
/* List of messages from paused clients waiting for resume */
extern GList *MessagePausedList;

/* List of different entries of client-specific configuration */
extern GList *client_specific_settings;

/* Saves the last received priority progress message */
extern GList *last_p5_block;

/* Global default settings */
extern TFDSetElement GlobalFDSet;

/* Inter thread comm pipe */
extern int speaking_pipe[2];

/* Managing sockets communication */
extern GHashTable *speechd_sockets_status;
typedef struct {
	int awaiting_data;
	int inside_block;
	size_t o_bytes;
	GString *o_buf;
} TSpeechDSock;
int speechd_sockets_status_init(void);
int speechd_socket_register(int fd);
void speechd_socket_free(TSpeechDSock * speechd_socket);
int speechd_socket_unregister(int fd);
TSpeechDSock *speechd_socket_get_by_fd(int fd);

#include "parse.h"

/* Debugging */
void MSG(int level, char *format, ...);
void MSG2(int level, char *kind, char *format, ...);
#define FATAL(msg) do { fatal_error(); MSG(-1,"Fatal error [%s:%d]:"msg, __FILE__, __LINE__); exit(EXIT_FAILURE); } while (0)
#define DIE(msg) do { MSG(0,"Error [%s:%d]:"msg, __FILE__, __LINE__); exit(EXIT_FAILURE); } while (0)

extern FILE *logfile;
extern FILE *custom_logfile;
extern char *custom_log_kind;
extern FILE *debug_logfile;

/* For debugging purposes, does nothing */
void fatal_error(void);

/* isanum() tests if the given string is a number,
 * returns 1 if yes, 0 otherwise. */
int isanum(const char *str);

/* Functions used in speechd.c only */
int speechd_connection_new(int server_socket);
int speechd_connection_destroy(int fd);
void speechd_modules_terminate(gpointer data, gpointer user_data);
void speechd_modules_reload(gpointer data, gpointer user_data);
void speechd_modules_debug(void);
void speechd_modules_nodebug(void);

void speechd_options_init(void);
void speechd_init(void);
int create_pid_file(void);
void destroy_pid_file(void);

void logging_init(void);

void check_locked(pthread_mutex_t * lock);

#endif
