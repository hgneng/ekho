
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
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $Id: speechd.h,v 1.64 2008-06-27 12:29:07 hanke Exp $
 */

#ifndef SPEECHDH
#define SPEECHDH

#define _GNU_SOURCE

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

#include <semaphore.h>
#include <sys/ipc.h>
#include <sys/sem.h>

/* Definition of semun needed for semaphore manipulation */
/* TODO: This fixes compilation for Mac OS X but might not be a correct
   solution for other platforms. A better check is needed, possibly including
   _POSIX_C_SOURCE and friends*/
#if (defined(__GNU_LIBRARY__) && !defined(_SEM_SEMUN_UNDEFINED)) || defined(__APPLE__)
 /* union semun is defined by including <sys/sem.h> */
#else
 /* according to X/OPEN we have to define it ourselves */
union semun {
    int val;                    /* value for SETVAL */
    struct semid_ds *buf;       /* buffer for IPC_STAT, IPC_SET */
    unsigned short int *array;  /* array for GETALL, SETALL */
    struct seminfo *__buf;      /* buffer for IPC_INFO */
};
#endif

#include "def.h"
#include "fdset.h"
#include "module.h"
#include "compare.h"

/* Size of the buffer for socket communication */
#define BUF_SIZE 128

/* Mode of speechd execution */
typedef enum{
    SPD_MODE_DAEMON,            /* Run as daemon (background, ...) */
    SPD_MODE_SINGLE             /*  */
}TSpeechDMode;

TSpeechDMode spd_mode;

/*  TSpeechDQueue is a queue for messages. */
typedef struct{
    GList *p1;			/* important */
    GList *p2;			/* text */
    GList *p3;			/* message */
    GList *p4;                  /* notification */
    GList *p5;                  /* progress */
}TSpeechDQueue;

/*  TSpeechDMessage is an element of TSpeechDQueue,
    that is, some text with or without index marks
    inside  and it's configuration. */
typedef struct{
    guint id;			/* unique id */
    time_t time;                /* when was this message received */
    char *buf;			/* the actual text */
    int bytes;			/* number of bytes in buf */
    TFDSetElement settings;	/* settings of the client when queueing this message */
}TSpeechDMessage;

#include "alloc.h"
#include "speaking.h"

struct{
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
    char *home_speechd_dir;
    char *log_dir;
    int spawn;
    int debug;
    char *debug_destination;
    char *debug_logfile;
    int max_history_messages;	/* Maximum of messages in history before they expire */
}SpeechdOptions;

struct{
    int max_uid;		/* The largest assigned uid + 1 */
    int max_gid;		/* The largest assigned gid + 1 */
    int max_fd;
}SpeechdStatus;

/* speak() thread defined in speaking.c */
pthread_t speak_thread;
pthread_mutex_t logging_mutex;
pthread_mutex_t element_free_mutex;
pthread_mutex_t output_layer_mutex;
pthread_mutex_t socket_com_mutex;

/* Activity requests for the speaking thread are
 handled with SYSV/IPC semaphore */
key_t speaking_sem_key;
int speaking_sem_id;

/* Table of all configured (and succesfully loaded) output modules */
GHashTable *output_modules;	
GList *output_modules_list;
/* Table of settings for each active client (=each active socket)*/
GHashTable *fd_settings;	
/* Table of default output modules for different languages */
GHashTable *language_default_modules;
/* Table of relations between client file descriptors and their uids */
GHashTable *fd_uid;

/* Speech Dispatcher main priority queue for messages */
TSpeechDQueue *MessageQueue;
/* List of messages from paused clients waiting for resume */
GList *MessagePausedList;
/* List of settings related to history */
GList *history_settings;
/* List of messages in history */
GList *message_history;

/* List of different entries of client-specific configuration */
GList *client_specific_settings;

/* Saves the last received priority progress message */
GList *last_p5_block;

/* Global default settings */
TFDSetElement GlobalFDSet;

/* Variables for socket communication */
fd_set readfds;

/* Inter thread comm pipe */
int speaking_pipe[2];


/* Managing sockets communication */
GHashTable *speechd_sockets_status;
typedef struct{
    int awaiting_data;
    int inside_block;
    size_t o_bytes;
    GString *o_buf;
}TSpeechDSock;
int speechd_sockets_status_init(void);
int speechd_socket_register(int fd);
void speechd_socket_free(TSpeechDSock* speechd_socket);
int speechd_socket_unregister(int fd);
TSpeechDSock* speechd_socket_get_by_fd(int fd);

#include "parse.h"

/* Debugging */
void MSG(int level, char *format, ...);
void MSG2(int level, char* kind, char *format, ...);
#define FATAL(msg) { fatal_error(); MSG(-1,"Fatal error [%s:%d]:"msg, __FILE__, __LINE__); exit(EXIT_FAILURE); }
#define DIE(msg) { MSG(0,"Error [%s:%d]:"msg, __FILE__, __LINE__); exit(EXIT_FAILURE); }

FILE *logfile;
FILE *custom_logfile;
char *custom_log_kind;
FILE *debug_logfile;

/* For debugging purposes, does nothing */
void fatal_error(void);

/* isanum() tests if the given string is a number,
 * returns 1 if yes, 0 otherwise. */
int isanum(const char *str);

/* Construct a path given a filename and the directory
 where to refer relative paths. filename can be either
 absolute (starting with slash) or relative. */
char* spd_get_path(char *filename, char* startdir);


/* Functions used in speechd.c only */
int speechd_connection_new(int server_socket);
int speechd_connection_destroy(int fd);
gboolean speechd_client_terminate(gpointer key, gpointer value, gpointer user);
gboolean speechd_modules_terminate(gpointer key, gpointer value, gpointer user);
void speechd_modules_reload(gpointer key, gpointer value, gpointer user);
void speechd_modules_debug(void);
void speechd_modules_nodebug(void);

void speechd_reload_dead_modules(int sig);
void speechd_options_init(void);
void speechd_init(void);
void speechd_load_configuration(int sig);
void speechd_quit(int sig);
int create_pid_file(void);
void destroy_pid_file(void);

void logging_init(void);

void check_locked(pthread_mutex_t *lock);


#endif
