/*
  libspeechd.c - Shared library for easy acces to Speech Dispatcher functions
 *
 * Copyright (C) 2001, 2002, 2003, 2006, 2007, 2008 Brailcom, o.p.s.
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
 * $Id: libspeechd.c,v 1.37 2008-12-23 09:15:32 pdm Exp $
 */


#include <sys/types.h>
#include <wchar.h>
#include <sys/socket.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <arpa/inet.h>
#include <unistd.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <glib.h>
#include <errno.h>
#include <assert.h>
#include <netdb.h>

#include "def.h"
#include "libspeechd.h"

/* Comment/uncomment to switch debugging on/off */
// #define LIBSPEECHD_DEBUG 1

/* --------------  Private functions headers ------------------------*/

#ifdef LIBSPEECHD_DEBUG
FILE *spd_debug = NULL;
#endif


static int spd_set_priority(SPDConnection* connection, SPDPriority priority);
static char* escape_dot(const char *text);
static int isanum(char* str);		
static char* get_reply(SPDConnection *connection);
static int get_err_code(char *reply);
static char* get_param_str(char* reply, int num, int *err);
static int get_param_int(char* reply, int num, int *err);
static void *xmalloc(size_t bytes);
static void xfree(void *ptr);   
static int ret_ok(char *reply);
static void SPD_DBG(char *format, ...);
static void* spd_events_handler(void*);

pthread_mutex_t spd_logging_mutex;

#if !(defined(__GLIBC__) && defined(_GNU_SOURCE))
/* Added by Willie Walker - strndup and getline are gcc-isms */
char *strndup ( const char *s, size_t n)
{
        size_t nAvail;
        char *p;

        if ( !s )
                return 0;

        if ( strlen(s) > n )
                nAvail = n + 1;
        else
                nAvail = strlen(s) + 1;
        p = malloc ( nAvail );
        memcpy ( p, s, nAvail );
        p[nAvail - 1] = '\0';

        return p;
}

#define BUFFER_LEN 256
ssize_t getline (char **lineptr, size_t *n, FILE *f)
{
        char ch;
        size_t m = 0;
        ssize_t buf_len = 0;
        char * buf = NULL;
        char * p = NULL;

	if (errno != 0) {
                SPD_DBG("getline: errno came in as %d!!!\n", errno);
	        errno = 0;
	}
        while ( (ch = getc(f)) !=EOF )
        {
                if (errno != 0)
                        return -1;
                if ( m++ >= buf_len )
                {
                        buf_len += BUFFER_LEN;
                        buf = (char *) realloc(buf, buf_len + 1);
                        if ( buf == NULL )
                        {
                                SPD_DBG("buf==NULL");
                                return -1;
                        }
                        p = buf + buf_len - BUFFER_LEN;
                }
                *p = ch;
                p++;
                if ( ch == '\n' )
                        break;
        }
        if ( m == 0 )
        {
                SPD_DBG("getline: m=%d!",m);
                return -1;
        } else {
                *p = '\0';
                *lineptr = buf;
                *n = m;
                return m;
        }
}
#endif /* !(defined(__GLIBC__) && defined(_GNU_SOURCE)) */

/* --------------------- Public functions ------------------------- */

#define SPD_REPLY_BUF_SIZE 65536

/* Determine address for the unix socket */
static char*
_get_default_unix_socket_name(void)
{
  GString* socket_filename;
  char *h;
  const char *homedir = g_getenv("HOME");
  if (!homedir)
    homedir = g_get_home_dir();
  socket_filename = g_string_new("");
  g_string_printf(socket_filename, "%s/.speech-dispatcher/speechd.sock", homedir);
  // Do not regurn glib string, but glibc string...
  h = strdup(socket_filename->str);
  g_string_free(socket_filename, 1);
  return h;
}

SPDConnectionAddress*
spd_get_default_address(char **error)
{
  const gchar *env_address = g_getenv("SPEECHD_ADDRESS");
  gchar **pa; /* parsed address */
  SPDConnectionAddress *address = malloc(sizeof(SPDConnectionAddress));

  if (env_address == NULL){ // Default method = unix sockets
    address->method = SPD_METHOD_UNIX_SOCKET;
    address->unix_socket_name = _get_default_unix_socket_name();
  }else{
    pa = g_strsplit(env_address, ":", 0);
    assert (pa);
    if (!g_strcmp0(pa[0], "unix_socket") || pa[0] == NULL){ // Unix sockets
      address->method = SPD_METHOD_UNIX_SOCKET;
      if (pa[1] == NULL){
	address->unix_socket_name = _get_default_unix_socket_name();
      }else{
	address->unix_socket_name = strdup(pa[1]);
      }
    }else if (!g_strcmp0(pa[0], "inet_socket")){ // Inet sockets
      address->method = SPD_METHOD_INET_SOCKET;
      if (pa[1] == NULL){
	address->inet_socket_host = strdup("127.0.0.1");
	address->inet_socket_port = 6560;
      }else{
	address->inet_socket_host = strdup(pa[1]);
	if (pa[2] == NULL){
	  address->inet_socket_port = SPEECHD_DEFAULT_PORT;
	}else{
	  address->inet_socket_port = atoi(pa[2]);
        }
      }
    }else{ // Unknown or unsupported method requested
      *error = strdup("Unknown or unsupported communication method");
      address = NULL;
    }
    g_strfreev(pa);
  }
  return address;
}


static void _init_debug(void)
{
#ifdef LIBSPEECHD_DEBUG
  if (!spd_debug){
    spd_debug = fopen("/tmp/libspeechd.log", "w");
    if (spd_debug == NULL) SPD_FATAL("COULDN'T ACCES FILE INTENDED FOR DEBUG");
     
    if(pthread_mutex_init(&spd_logging_mutex, NULL)){
      fprintf(stderr, "Mutex initialization failed");
      fflush(stderr);
      exit(1);
    }
    SPD_DBG("Debugging started");
  }
#endif /* LIBSPEECHD_DEBUG */
}

/* Opens a new Speech Dispatcher connection.
 * Returns socket file descriptor of the created connection
 * or -1 if no connection was opened. */

SPDConnection*
spd_open(const char* client_name, const char* connection_name, const char* user_name,
	 SPDConnectionMode mode)
{
  char *error;
  int autospawn = 1;
  SPDConnection *conn;
  conn = spd_open2(client_name, connection_name, user_name,
		   mode, NULL, autospawn, &error);
  if (!conn){
    _init_debug();
    assert(error);
    SPD_DBG("Could not connect to Speech Dispatcher: %s", error);
    xfree(error);
  }
  return conn;
}

#define MAX_IP_SIZE 16+1
/* TODO: This only works in IPV4 */
static char*
resolve_host(char* host_name_or_ip, int *is_localhost, gchar **error)
{
    struct addrinfo *addr_result;
    int err;
    char *resolve_buffer = malloc(MAX_IP_SIZE*sizeof(char));
    const char *resolved_ip = NULL;
    char *ip;
    *error = NULL;
    struct sockaddr_in *addr_in;
    
    if (resolve_buffer == NULL) {
	*error = g_strdup("Failed to allocate memory.");
	return NULL;
    }

    err = getaddrinfo(host_name_or_ip, 0, NULL, &addr_result);
    if (err){
	*error = g_strdup_printf("Can't resolve address %d due to error %s:",
				 err, gai_strerror(err));
	xfree(resolve_buffer);
	return NULL;
    }
    /* Take the first address returned as we are only interested in host ip */
    addr_in = (struct sockaddr_in *) addr_result->ai_addr;
    resolved_ip = inet_ntop(AF_INET, &(addr_in->sin_addr.s_addr), resolve_buffer, MAX_IP_SIZE);
    if (resolved_ip == NULL) {
	*error = g_strdup_printf("Could not convert address, due to the following error: %s",
				 strerror(errno));
	freeaddrinfo(addr_result);
	xfree(resolve_buffer);
	return NULL;
    }

    if (!strncmp(resolved_ip, "127.",4)){
	*is_localhost = 1;
	/* In case of local addresses, use 127.0.0.1 which is guaranteed
	   to be local and the server listens on it */
	xfree(resolve_buffer);
	ip = strdup("127.0.0.1");
    }else{
	*is_localhost = 0;
	ip = resolve_buffer;
    }
    freeaddrinfo(addr_result);
    return ip;
}

static int
spawn_server(SPDConnectionAddress *address, int is_localhost, gchar **spawn_error)
{
    gchar **speechd_cmd = malloc(16*sizeof(char*));
    gchar *stderr_output;
    gboolean spawn_ok;
    GError *gerror = NULL;
    int exit_status;
    int i;
    char *resolved_ip;

    if ((address->method==SPD_METHOD_INET_SOCKET) && (!is_localhost)){
	*spawn_error = g_strdup("Spawn failed, the given network address doesn't seem to be on localhost");
	return 1;
    }

    speechd_cmd[0] = g_strdup(SPD_SPAWN_CMD);
    speechd_cmd[1] = g_strdup("--spawn");
    speechd_cmd[2] = g_strdup("--communication-method");
    if (address->method==SPD_METHOD_INET_SOCKET){
	speechd_cmd[3] = g_strdup("inet_socket");
	speechd_cmd[4] = g_strdup("--port");
	speechd_cmd[5] = g_strdup_printf("%d",address->inet_socket_port);
	speechd_cmd[6] = NULL;
    }else if (address->method==SPD_METHOD_UNIX_SOCKET){
	speechd_cmd[3] = g_strdup("unix_socket");
	speechd_cmd[4] = g_strdup("--socket-path");
	speechd_cmd[5] = g_strdup_printf("%s", address->unix_socket_name);
	speechd_cmd[6] = NULL;
    }else assert (0);
    
    spawn_ok = g_spawn_sync(NULL, (gchar**)speechd_cmd, NULL, G_SPAWN_SEARCH_PATH | G_SPAWN_STDOUT_TO_DEV_NULL,
			    NULL, NULL, NULL, &stderr_output, &exit_status, &gerror);
    for (i=0;speechd_cmd[i]!=NULL;i++)
	g_free(speechd_cmd[i]);
    if (!spawn_ok){
	*spawn_error = g_strdup_printf("Autospawn failed. Spawn error %d: %s", gerror->code, gerror->message);
	return 1;
    }else{
	if (exit_status){
	    *spawn_error = g_strdup_printf("Autospawn failed. Speech Dispatcher refused to start with error code, " \
					   "stating this as a reason: %s", stderr_output);
	    return 1;
	}else{
	    *spawn_error = NULL;
	    return 0;
	}
    }
    assert(0);
}

SPDConnection*
spd_open2(const char* client_name, const char* connection_name, const char* user_name,
	  SPDConnectionMode mode, SPDConnectionAddress *address, int autospawn,
	  char **error_result)
{
    SPDConnection *connection;
    char *set_client_name;
    char* conn_name;
    char* usr_name;
    int ret;
    char tcp_no_delay = 1;

    /* Autospawn related */
    int spawn_err;
    gchar *spawn_report;
    char *host_ip;
    int is_localhost = 1;
    
    struct sockaddr_in address_inet;    
    struct sockaddr_un address_unix;    
    struct sockaddr *sock_address;
    size_t sock_address_len;
    gchar *resolve_error;

    _init_debug();

    if (client_name == NULL){
      *error_result = strdup("ERROR: Client name not specified");
      SPD_DBG(*error_result);
      return NULL;
    }
    
    if (user_name == NULL)
    {
        usr_name = strdup((char*) g_get_user_name());
    }
    else
        usr_name = strdup(user_name);
    
    if(connection_name == NULL)
        conn_name = strdup("main");
    else
        conn_name = strdup(connection_name);

    if (address == NULL){
      char *err = NULL;
      address = spd_get_default_address(&err);
      if (!address){
	assert(err);
	*error_result = err;
	SPD_DBG(*error_result);
	return NULL;
      }
    }
    
    /* Connect to server using the selected method */
    connection = xmalloc(sizeof(SPDConnection));
    if (address->method==SPD_METHOD_INET_SOCKET){    
	host_ip = resolve_host(address->inet_socket_host, &is_localhost, &resolve_error);
	if (host_ip == NULL){
	    *error_result = strdup(resolve_error);
	    g_free(resolve_error);
	    return NULL;
	}	
	address_inet.sin_addr.s_addr = inet_addr(host_ip);
	address_inet.sin_port = htons(address->inet_socket_port);
	address_inet.sin_family = AF_INET;
	connection->socket = socket(AF_INET, SOCK_STREAM, 0);
	sock_address = (struct sockaddr*) &address_inet;
	sock_address_len = sizeof(address_inet);
    }else if (address->method==SPD_METHOD_UNIX_SOCKET){
      /* Create the unix socket */
      address_unix.sun_family = AF_UNIX;
      strncpy (address_unix.sun_path, address->unix_socket_name, sizeof (address_unix.sun_path));
      address_unix.sun_path[sizeof (address_unix.sun_path) - 1] = '\0';
      connection->socket = socket(AF_UNIX, SOCK_STREAM, 0);
      sock_address = (struct sockaddr*) &address_unix;
      sock_address_len = SUN_LEN(&address_unix);
    }else SPD_FATAL("Unsupported connection method for spd_open2()");

    ret = connect(connection->socket, sock_address, sock_address_len);    
    if (ret == -1){
	/* Suppose server might not be running, try to autospawn (autostart) it */
	if (autospawn){	
	    spawn_err = spawn_server(address, is_localhost, &spawn_report);
	    if (!spawn_err)
		spawn_report = g_strdup("Server successfully autospawned");
	    ret = connect(connection->socket, sock_address, sock_address_len);    
	}else{
	    spawn_report = g_strdup("Autospawn disabled");
	}
	if (ret == -1){
	    if (address->method == SPD_METHOD_INET_SOCKET)
		*error_result = g_strdup_printf("Error: Can't connect to %s on port %d using inet sockets: %s. " \
						"Autospawn: %s", address->inet_socket_host,
						address->inet_socket_port, strerror(errno), spawn_report);
	    else if (address->method == SPD_METHOD_UNIX_SOCKET)
		*error_result = g_strdup_printf("Error: Can't connect to unix socket %s: %s. Autospawn: %s",
						address->unix_socket_name, strerror(errno), spawn_report);
	    else assert (0);
	    SPD_DBG(*error_result);
	    close(connection->socket);	
	    return NULL;
	}
    }

    if (address->method == SPD_METHOD_INET_SOCKET)
	setsockopt(connection->socket, IPPROTO_TCP, TCP_NODELAY, &tcp_no_delay, sizeof(int));
    
    connection->callback_begin = NULL;
    connection->callback_end = NULL;
    connection->callback_im = NULL;
    connection->callback_pause = NULL;
    connection->callback_resume = NULL;
    connection->callback_cancel = NULL;

    connection->mode = mode;

    /* Create a stream from the socket */
    connection->stream = fdopen(connection->socket, "r");
    if (!connection->stream) SPD_FATAL("Can't create a stream for socket, fdopen() failed.");
    /* Switch to line buffering mode */
    ret = setvbuf(connection->stream, NULL, _IONBF, SPD_REPLY_BUF_SIZE);
    if (ret) SPD_FATAL("Can't set buffering, setvbuf failed.");

    connection->ssip_mutex = xmalloc(sizeof(pthread_mutex_t));
    pthread_mutex_init(connection->ssip_mutex, NULL);

    if (mode == SPD_MODE_THREADED){
	SPD_DBG("Initializing threads, condition variables and mutexes...");
	connection->events_thread = xmalloc(sizeof(pthread_t));
	connection->cond_reply_ready = xmalloc(sizeof(pthread_cond_t));
	connection->mutex_reply_ready = xmalloc(sizeof(pthread_mutex_t));
	connection->cond_reply_ack = xmalloc(sizeof(pthread_cond_t));
	connection->mutex_reply_ack = xmalloc(sizeof(pthread_mutex_t));
	pthread_cond_init(connection->cond_reply_ready, NULL);
	pthread_mutex_init(connection->mutex_reply_ready, NULL);
	pthread_cond_init(connection->cond_reply_ack, NULL);
	pthread_mutex_init(connection->mutex_reply_ack, NULL);
	ret = pthread_create(connection->events_thread, NULL, spd_events_handler, connection);
	if(ret != 0){
	    *error_result = strdup("Thread initialization failed");
	    SPD_DBG(*error_result);
	    return NULL;
	}
    }

    /* By now, the connection is created and operational */
    set_client_name = g_strdup_printf("SET SELF CLIENT_NAME \"%s:%s:%s\"", usr_name,
				      client_name, conn_name);
    ret = spd_execute_command_wo_mutex(connection, set_client_name);   
    xfree(usr_name);  xfree(conn_name);  xfree(set_client_name);
    return connection;
}


#define RET(r) \
    { \
    pthread_mutex_unlock(connection->ssip_mutex); \
    return r; \
    }


/* Close a Speech Dispatcher connection */
void
spd_close(SPDConnection* connection)
{

    pthread_mutex_lock(connection->ssip_mutex);

    if (connection->mode == SPD_MODE_THREADED){
	pthread_cancel(*connection->events_thread);
	pthread_mutex_destroy(connection->mutex_reply_ready);
	pthread_mutex_destroy(connection->mutex_reply_ack);
	pthread_cond_destroy(connection->cond_reply_ready);
	pthread_cond_destroy(connection->cond_reply_ack);
	pthread_join(*connection->events_thread, NULL);
	connection->mode = SPD_MODE_SINGLE;
    }

    /* close the socket */
    close(connection->socket);

    pthread_mutex_unlock(connection->ssip_mutex);

    pthread_mutex_destroy(connection->ssip_mutex);
    xfree(connection);
}

/* Helper functions for spd_say. */
static inline int
spd_say_prepare(SPDConnection *connection, SPDPriority priority,
	const char* text, char **escaped_text)
{
    int ret = 0;

    SPD_DBG("Text to say is: %s", text);

    /* Insure that there is no escape sequence in the text */
    *escaped_text = escape_dot(text);
    /* Caller is now responsible for escaped_text. */
    if (*escaped_text == NULL) {	/* Out of memory. */
	SPD_DBG("spd_say could not allocate memory.");
	ret = -1;
    } else {
	/* Set priority */
	SPD_DBG("Setting priority");
	ret = spd_set_priority(connection, priority);
	if (!ret) {
	    /* Start the data flow */
	    SPD_DBG("Sending SPEAK");
	    ret = spd_execute_command_wo_mutex(connection, "speak");
	    if (ret) {
		SPD_DBG("Error: Can't start data flow!");
	    }
	}
    }

    return ret;
}

static inline int
spd_say_sending(SPDConnection *connection, const char* text)
{
    int msg_id = -1;
    int err = 0;
    char *reply = NULL;
    char *pret = NULL;

    /* Send data */
    SPD_DBG("Sending data");
    pret = spd_send_data_wo_mutex(connection, text, SPD_NO_REPLY);
    if (pret==NULL) {
	SPD_DBG("Can't send data wo mutex");
    } else {
	/* Terminate data flow */
	SPD_DBG("Terminating data flow");
	err = spd_execute_command_with_reply(connection, "\r\n.", &reply);
	if (err) {
	    SPD_DBG("Can't terminate data flow");
	} else {
	    msg_id = get_param_int(reply, 1, &err);
	    if (err < 0) {
		SPD_DBG("Can't determine SSIP message unique ID parameter.");
		msg_id = -1;
	    }
	}
    }

    xfree(reply);
    xfree(pret);
    return msg_id;
}

/* Say TEXT with priority PRIORITY.
 * Returns msg_uid on success, -1 otherwise. */
int
spd_say(SPDConnection *connection, SPDPriority priority, const char* text)
{
    char *escaped_text = NULL;
    int msg_id = -1;
    int prepare_failed = 0;

    if (text != NULL) {
    pthread_mutex_lock(connection->ssip_mutex);

	prepare_failed = spd_say_prepare(connection, priority, text, &escaped_text);
	if (!prepare_failed)
	    msg_id = spd_say_sending(connection, escaped_text);

	xfree(escaped_text);
	pthread_mutex_unlock(connection->ssip_mutex);
    } else {
	SPD_DBG("spd_say called with a NULL argument for <text>");
    }

    SPD_DBG("Returning from spd_say");
    return msg_id;
}

/* The same as spd_say, accepts also formated strings */
int
spd_sayf(SPDConnection *connection, SPDPriority priority, const char *format, ...)
{
    static int ret;    
    va_list args;
    char *buf;

    if (format == NULL) return -1;
    
    /* Print the text to buffer */
    va_start(args, format);
    buf = g_strdup_vprintf(format, args);
    va_end(args);
    
    /* Send the buffer to Speech Dispatcher */
    ret = spd_say(connection, priority, buf);	
    xfree(buf);

    return ret;
}

int
spd_stop(SPDConnection *connection)
{
  return spd_execute_command(connection, "STOP SELF");
}

int
spd_stop_all(SPDConnection *connection)
{
  return spd_execute_command(connection, "STOP ALL");
}

int
spd_stop_uid(SPDConnection *connection, int target_uid)
{
  static char command[16];

  sprintf(command, "STOP %d", target_uid);
  return spd_execute_command(connection, command);
}

int
spd_cancel(SPDConnection *connection)
{
  return spd_execute_command(connection, "CANCEL SELF");
}

int
spd_cancel_all(SPDConnection *connection)
{
  return spd_execute_command(connection, "CANCEL ALL");
}

int
spd_cancel_uid(SPDConnection *connection, int target_uid)
{
  static char command[16];

  sprintf(command, "CANCEL %d", target_uid);
  return spd_execute_command(connection, command);
}

int
spd_pause(SPDConnection *connection)
{
  return spd_execute_command(connection, "PAUSE SELF");
}

int
spd_pause_all(SPDConnection *connection)
{
  return spd_execute_command(connection, "PAUSE ALL");
}

int
spd_pause_uid(SPDConnection *connection, int target_uid)
{
  char command[16];

  sprintf(command, "PAUSE %d", target_uid);
  return spd_execute_command(connection, command);
}

int
spd_resume(SPDConnection *connection)
{
  return spd_execute_command(connection, "RESUME SELF");
}

int
spd_resume_all(SPDConnection *connection)
{
  return spd_execute_command(connection, "RESUME ALL");
}

int
spd_resume_uid(SPDConnection *connection, int target_uid)
{
  static char command[16];

  sprintf(command, "RESUME %d", target_uid);
  return spd_execute_command(connection, command);
}

int
spd_key(SPDConnection *connection, SPDPriority priority, const char *key_name)
{
    char *command_key;
    int ret;

    if (key_name == NULL) return -1;

    pthread_mutex_lock(connection->ssip_mutex);

    ret = spd_set_priority(connection, priority);
    if (ret) RET(-1);

    command_key = g_strdup_printf("KEY %s", key_name);
    ret = spd_execute_command_wo_mutex(connection, command_key);
    xfree(command_key);
    if (ret) RET(-1);

    pthread_mutex_unlock(connection->ssip_mutex);

    return 0;
}

int
spd_char(SPDConnection *connection, SPDPriority priority, const char *character)
{
    static char command[16];
    int ret;

    if (character == NULL) return -1;
    if (strlen(character)>6) return -1;

    pthread_mutex_lock(connection->ssip_mutex);

    ret = spd_set_priority(connection, priority);
    if (ret) RET(-1);

    sprintf(command, "CHAR %s", character);
    ret = spd_execute_command_wo_mutex(connection, command);
    if (ret) RET(-1);

    pthread_mutex_unlock(connection->ssip_mutex);

    return 0;
}

int
spd_wchar(SPDConnection *connection, SPDPriority priority, wchar_t wcharacter)
{
    static char command[16];
    char character[8];
    int ret;

    pthread_mutex_lock(connection->ssip_mutex);

    ret = wcrtomb(character, wcharacter, NULL);
    if (ret <= 0) RET(-1);

    ret = spd_set_priority(connection, priority);
    if (ret) RET(-1);

    assert(character != NULL);
    sprintf(command, "CHAR %s", character);
    ret = spd_execute_command_wo_mutex(connection, command);
    if (ret) RET(-1);

    pthread_mutex_unlock(connection->ssip_mutex);
 
    return 0;
}

int
spd_sound_icon(SPDConnection *connection, SPDPriority priority, const char *icon_name)
{
    char *command;
    int ret;

    if (icon_name == NULL) return -1;

    pthread_mutex_lock(connection->ssip_mutex);

    ret = spd_set_priority(connection, priority);
    if (ret) RET(-1);

    command = g_strdup_printf("SOUND_ICON %s", icon_name);
    ret = spd_execute_command_wo_mutex(connection, command);
    xfree (command);
    if (ret) RET(-1);

    pthread_mutex_unlock(connection->ssip_mutex);
    
    return 0;
}

int
spd_w_set_punctuation(SPDConnection *connection, SPDPunctuation type, const char* who)
{
    char command[32];
    int ret;

    if (type == SPD_PUNCT_ALL)
        sprintf(command, "SET %s PUNCTUATION all", who);
    if (type == SPD_PUNCT_NONE)
        sprintf(command, "SET %s PUNCTUATION none", who);
    if (type == SPD_PUNCT_SOME)
        sprintf(command, "SET %s PUNCTUATION some", who);

    ret = spd_execute_command(connection, command);    
    
    return ret;
}

int
spd_w_set_capital_letters(SPDConnection *connection, SPDCapitalLetters type, const char* who)
{
    char command[64];
    int ret;

    if (type == SPD_CAP_NONE)
        sprintf(command, "SET %s CAP_LET_RECOGN none", who);
    if (type == SPD_CAP_SPELL)
        sprintf(command, "SET %s CAP_LET_RECOGN spell", who);
    if (type == SPD_CAP_ICON)
        sprintf(command, "SET %s CAP_LET_RECOGN icon", who);

    ret = spd_execute_command(connection, command);    
    
    return ret;
}

int
spd_w_set_spelling(SPDConnection *connection, SPDSpelling type, const char* who)
{
    char command[32];
    int ret;

    if (type == SPD_SPELL_ON)
        sprintf(command, "SET %s SPELLING on", who);
    if (type == SPD_SPELL_OFF)
        sprintf(command, "SET %s SPELLING off", who);

    ret = spd_execute_command(connection, command);

    return ret;
}

int
spd_set_data_mode(SPDConnection *connection, SPDDataMode mode)
{
    char command[32];
    int ret;

    if (mode == SPD_DATA_TEXT)
        sprintf(command, "SET SELF SSML_MODE off");
    if (mode == SPD_DATA_SSML)
        sprintf(command, "SET SELF SSML_MODE on");

    ret = spd_execute_command(connection, command);

    return ret;
}

int
spd_w_set_voice_type(SPDConnection *connection, SPDVoiceType type, const char *who)
{
    static char command[64];

    switch(type){
    case SPD_MALE1: sprintf(command, "SET %s VOICE MALE1", who); break;
    case SPD_MALE2: sprintf(command, "SET %s VOICE MALE2", who); break;
    case SPD_MALE3: sprintf(command, "SET %s VOICE MALE3", who); break;
    case SPD_FEMALE1: sprintf(command, "SET %s VOICE FEMALE1", who); break;
    case SPD_FEMALE2: sprintf(command, "SET %s VOICE FEMALE2", who); break;
    case SPD_FEMALE3: sprintf(command, "SET %s VOICE FEMALE3", who); break;
    case SPD_CHILD_MALE: sprintf(command, "SET %s VOICE CHILD_MALE", who); break;
    case SPD_CHILD_FEMALE: sprintf(command, "SET %s VOICE CHILD_FEMALE", who); break;
    default: return -1;
    }
    
    return spd_execute_command(connection, command);      
}

#define SPD_SET_COMMAND_INT(param, ssip_name, condition) \
    int \
    spd_w_set_ ## param (SPDConnection *connection, signed int val, const char* who) \
    { \
        static char command[64]; \
        if ((!condition)) return -1; \
        sprintf(command, "SET %s " #ssip_name " %d", who, val); \
        return spd_execute_command(connection, command); \
    } \
    int \
    spd_set_ ## param (SPDConnection *connection, signed int val) \
    { \
        return spd_w_set_ ## param (connection, val, "SELF"); \
    } \
    int \
    spd_set_ ## param ## _all(SPDConnection *connection, signed int val) \
    { \
        return spd_w_set_ ## param (connection, val, "ALL"); \
    } \
    int \
    spd_set_ ## param ## _uid(SPDConnection *connection, signed int val, unsigned int uid) \
    { \
        char who[8]; \
        sprintf(who, "%d", uid); \
        return spd_w_set_ ## param (connection, val, who); \
    }

#define SPD_SET_COMMAND_STR(param, ssip_name) \
    int \
    spd_w_set_ ## param (SPDConnection *connection, const char *str, const char* who) \
    { \
        char *command; \
        int ret; \
        if (str == NULL) return -1; \
        command = g_strdup_printf("SET %s " #param " %s", \
                              who, str); \
        ret = spd_execute_command(connection, command); \
        xfree(command); \
        return ret; \
    } \
    int \
    spd_set_ ## param (SPDConnection *connection, const char *str) \
    { \
        return spd_w_set_ ## param (connection, str, "SELF"); \
    } \
    int \
    spd_set_ ## param ## _all(SPDConnection *connection, const char *str) \
    { \
        return spd_w_set_ ## param (connection, str, "ALL"); \
    } \
    int \
    spd_set_ ## param ## _uid(SPDConnection *connection, const char *str, unsigned int uid) \
    { \
        char who[8]; \
        sprintf(who, "%d", uid); \
        return spd_w_set_ ## param (connection, str, who); \
    }

#define SPD_SET_COMMAND_SPECIAL(param, type) \
    int \
    spd_set_ ## param (SPDConnection *connection, type val) \
    { \
        return spd_w_set_ ## param (connection, val, "SELF"); \
    } \
    int \
    spd_set_ ## param ## _all(SPDConnection *connection, type val) \
    { \
        return spd_w_set_ ## param (connection, val, "ALL"); \
    } \
    int \
    spd_set_ ## param ## _uid(SPDConnection *connection, type val, unsigned int uid) \
    { \
        char who[8]; \
        sprintf(who, "%d", uid); \
        return spd_w_set_ ## param (connection, val, who); \
    }

SPD_SET_COMMAND_INT(voice_rate, RATE, ((val >= -100) && (val <= +100)) )
SPD_SET_COMMAND_INT(voice_pitch, PITCH, ((val >= -100) && (val <= +100)) )
SPD_SET_COMMAND_INT(volume, VOLUME, ((val >= -100) && (val <= +100)) )

SPD_SET_COMMAND_STR(language, LANGUAGE)
SPD_SET_COMMAND_STR(output_module, OUTPUT_MODULE)
SPD_SET_COMMAND_STR(synthesis_voice, SYNTHESIS_VOICE)

SPD_SET_COMMAND_SPECIAL(punctuation, SPDPunctuation)
SPD_SET_COMMAND_SPECIAL(capital_letters, SPDCapitalLetters)
SPD_SET_COMMAND_SPECIAL(spelling, SPDSpelling)
SPD_SET_COMMAND_SPECIAL(voice_type, SPDVoiceType)


#undef SPD_SET_COMMAND_INT
#undef SPD_SET_COMMAND_STR
#undef SPD_SET_COMMAND_SPECIAL

int
spd_set_notification_on(SPDConnection *connection, SPDNotification notification)
{
    if (connection->mode == SPD_MODE_THREADED)
	return spd_set_notification(connection, notification, "on");
    else
	return -1;
}

int
spd_set_notification_off(SPDConnection *connection, SPDNotification notification)
{
    if (connection->mode == SPD_MODE_THREADED)
	return spd_set_notification(connection, notification, "off");
    else
	return -1;
}


#define NOTIFICATION_SET(val, ssip_val) \
    if (notification & val){ \
	sprintf(command, "SET SELF NOTIFICATION "ssip_val" %s", state);\
	ret = spd_execute_command_wo_mutex(connection, command);\
	if (ret < 0) RET(-1);\
    }

int
spd_set_notification(SPDConnection *connection, SPDNotification notification, const char* state)
{
    static char command[64];
    int ret;

    if (connection->mode != SPD_MODE_THREADED) return -1;

    if (state == NULL){
      SPD_DBG("Requested state is NULL");
      return -1;
    }
    if (strcmp(state, "on") && strcmp(state, "off")){
      SPD_DBG("Invalid argument for spd_set_notification: %s", state);
      return -1;
    }

    pthread_mutex_lock(connection->ssip_mutex);

    NOTIFICATION_SET(SPD_INDEX_MARKS, "index_marks");
    NOTIFICATION_SET(SPD_BEGIN, "begin");
    NOTIFICATION_SET(SPD_END, "end");
    NOTIFICATION_SET(SPD_CANCEL, "cancel");
    NOTIFICATION_SET(SPD_PAUSE, "pause");
    NOTIFICATION_SET(SPD_RESUME, "resume");
    NOTIFICATION_SET(SPD_RESUME, "pause");

    pthread_mutex_unlock(connection->ssip_mutex);

    return 0;
}
#undef NOTIFICATION_SET


/* spd_list_modules retrieves information about the available output modules.
   The return value is a null-terminated array of strings containing output module
   names.
*/

char**
spd_list_modules(SPDConnection *connection)
{
  char **available_modules;
  available_modules = spd_execute_command_with_list_reply(connection, "LIST OUTPUT_MODULES");
  return available_modules;
}

char**
spd_list_voices(SPDConnection *connection)
{
  char **voices;
  voices = spd_execute_command_with_list_reply(connection, "LIST VOICES");
  return voices;
}

SPDVoice**
spd_list_synthesis_voices(SPDConnection *connection)
{
  char **svoices_str;
  SPDVoice **svoices;
  int i, num_items;
  svoices_str = spd_execute_command_with_list_reply(connection, "LIST SYNTHESIS_VOICES");

  if (svoices_str == NULL) return NULL;

  for (i=0;;i++)
    if (svoices_str[i] == NULL) break;
  num_items = i;
  svoices = (SPDVoice**) malloc((num_items+1) * sizeof(SPDVoice*));

  for (i=0;i<=num_items;i++){
    const char delimiters[] = " ";
    char *running;

    if (svoices_str[i] == NULL) break;
    running = strdup (svoices_str[i]);

    svoices[i] = (SPDVoice*) malloc(sizeof(SPDVoice));
    svoices[i]->name = strsep (&running, delimiters);
    svoices[i]->language = strsep (&running, delimiters);
    svoices[i]->variant = strsep (&running, delimiters);
    assert (svoices[i]->name != NULL);
  }

  svoices[num_items] = NULL;

  return svoices;
}

char**
spd_execute_command_with_list_reply(SPDConnection *connection, char *command)
{
  char *reply, *line;
  int err;
  int max_items = 50;
  char **result;
  int i, ret;

  result = malloc((max_items+1)*sizeof(char*));

  ret = spd_execute_command_with_reply(connection, command, &reply);
  if(!ret_ok(reply)) return NULL;
  
  for(i=0;  ;i++){
    line = get_param_str(reply, i+1, &err);
    if ((err) || (line == NULL)) break;
    result[i] = strdup(line);
    if (i>=max_items-2){
      max_items *= 2;
      result = realloc(result, max_items*sizeof(char*));
    }
  }

  result[i] = NULL;
  
  return result;
}

//int
//spd_get_client_list(SPDConnection *connection, char **client_names, int *client_ids, int* active){
//        SPD_DBG("spd_get_client_list: History is not yet implemented.");
//        return -1;
//
//}

int
spd_get_message_list_fd(SPDConnection *connection, int target, int *msg_ids, char **client_names)
{
        SPD_DBG("spd_get_client_list: History is not yet implemented.");
        return -1;
#if 0
	sprintf(command, "HISTORY GET MESSAGE_LIST %d 0 20\r\n", target);
	reply = spd_send_data(fd, command, 1);

/*	header_ok = parse_response_header(reply);
	if(header_ok != 1){
		free(reply);
		return -1;
	}*/

	for(count=0;  ;count++){
		record = (char*) parse_response_data(reply, count+1);
		if (record == NULL) break;
		record_int = get_rec_int(record, 0);
		msg_ids[count] = record_int;
		record_str = (char*) get_rec_str(record, 1);
		assert(record_str!=NULL);
		client_names[count] = record_str;
	}
	return count;
#endif
}

int
spd_execute_command(SPDConnection *connection, char* command)
{
    char *reply;
    int ret;

    pthread_mutex_lock(connection->ssip_mutex);

    ret = spd_execute_command_with_reply(connection, command, &reply);
    if (ret){
      SPD_DBG("Can't execute command in spd_execute_command");
    }
    xfree(reply);

    pthread_mutex_unlock(connection->ssip_mutex);

    return ret;
}

int
spd_execute_command_wo_mutex(SPDConnection *connection, char* command)
{
    char *reply;
    int ret;

    SPD_DBG("Executing command wo_mutex");
    ret = spd_execute_command_with_reply(connection, command, &reply);
    if (ret)
      SPD_DBG("Can't execute command in spd_execute_command_wo_mutex");

    xfree(reply);

    return ret;
}

int
spd_execute_command_with_reply(SPDConnection *connection, char* command, char **reply)
{
    char *buf;    
    int r;
    SPD_DBG("Inside execute_command_with_reply");

    buf = g_strdup_printf("%s\r\n", command);
    *reply = spd_send_data_wo_mutex(connection, buf, SPD_WAIT_REPLY);
    xfree(buf);
    buf = NULL;
    if(*reply==NULL){
        SPD_DBG("Can't send data wo mutex in spd_execute_command_with_reply");
	return -1;
    }

    r = ret_ok(*reply);

    if (!r) return -1;
    else return 0;
}


char*
spd_send_data(SPDConnection *connection, const char *message, int wfr)
{
    char *reply;
    pthread_mutex_lock(connection->ssip_mutex);


    if (connection->stream ==NULL) RET( NULL);

    reply = spd_send_data_wo_mutex(connection, message, wfr);
    if(reply==NULL){
        SPD_DBG("Can't send data wo mutex in spd_send_data");
        RET(NULL); 
    }

    pthread_mutex_unlock(connection->ssip_mutex);
    return reply;
}

char*
spd_send_data_wo_mutex(SPDConnection *connection, const char *message, int wfr)
{

    char *reply;
    int bytes;

    SPD_DBG("Inside spd_send_data_wo_mutex");

    if (connection->stream == NULL) return NULL;

    if (connection->mode == SPD_MODE_THREADED){
	/* Make sure we don't get the cond_reply_ready signal before we are in
	   cond_wait() */
	pthread_mutex_lock(connection->mutex_reply_ready);
    }
    /* write message to the socket */
    SPD_DBG("Writing to socket");
    if(!write(connection->socket, message, strlen(message))){
	SPD_DBG("Can't write to socket: %s", strerror(errno));
	pthread_mutex_unlock(connection->mutex_reply_ready);
	return NULL;
    }
    SPD_DBG("Written to socket");
    SPD_DBG(">> : |%s|", message);

    /* read reply to the buffer */
    if (wfr){
	if (connection->mode == SPD_MODE_THREADED){
	  /* Wait until the reply is ready */
	  SPD_DBG("Waiting for cond_reply_ready in spd_send_data_wo_mutex");
	  pthread_cond_wait(connection->cond_reply_ready, connection->mutex_reply_ready);
	  SPD_DBG("Condition for cond_reply_ready satisfied");
	  pthread_mutex_unlock(connection->mutex_reply_ready);
	  SPD_DBG("Reading the reply in spd_send_data_wo_mutex threaded mode");
	  /* Read the reply */
	  if (connection->reply != NULL){
		reply = strdup(connection->reply);
	    }else{
	        SPD_DBG("Error: Can't read reply, broken socket in spd_send_data.");
		return NULL;
	    }
	    xfree(connection->reply);
	    bytes = strlen(reply);
	    if (bytes == 0){
	        SPD_DBG("Error: Empty reply, broken socket.");
		return NULL;
	    }
	    /* Signal the reply has been read */
	    pthread_mutex_lock(connection->mutex_reply_ack);
	    pthread_cond_signal(connection->cond_reply_ack);
	    pthread_mutex_unlock(connection->mutex_reply_ack);
	}else{
	    reply = get_reply(connection);
	}
	if (reply != NULL)
	  SPD_DBG("<< : |%s|\n", reply);	
    }else{
	if (connection->mode == SPD_MODE_THREADED)
	    pthread_mutex_unlock(connection->mutex_reply_ready);
	SPD_DBG("<< : no reply expected");
        return strdup("NO REPLY");
    } 

    if (reply==NULL)
      SPD_DBG("Reply from get_reply is NULL in spd_send_data_wo_mutex");

    SPD_DBG("Returning from spd_send_data_wo_mutex");
    return reply;
}


/* --------------------- Internal functions ------------------------- */

static int
spd_set_priority(SPDConnection *connection, SPDPriority priority)
{
    static char p_name[16];
    static char command[64];

    switch(priority){
    case SPD_IMPORTANT: strcpy(p_name, "IMPORTANT"); break;
    case SPD_MESSAGE: strcpy(p_name, "MESSAGE"); break;
    case SPD_TEXT: strcpy(p_name, "TEXT"); break;
    case SPD_NOTIFICATION: strcpy(p_name, "NOTIFICATION"); break;
    case SPD_PROGRESS: strcpy(p_name, "PROGRESS"); break;
    default: 
      SPD_DBG("Error: Can't set priority! Incorrect value.");
      return -1;
    }
		 
    sprintf(command, "SET SELF PRIORITY %s", p_name);
    return spd_execute_command_wo_mutex(connection, command);
}


static char*
get_reply(SPDConnection *connection)
{
    GString *str;
    char *line = NULL;
    size_t N = 0;
    int bytes;
    char *reply;
    gboolean errors = FALSE;

    str = g_string_new("");

    /* Wait for activity on the socket, when there is some,
       read all the message line by line */
    do{
	bytes = getline(&line, &N, connection->stream);	
	if (bytes == -1){
	    SPD_DBG("Error: Can't read reply, broken socket in get_reply!");
	    if (connection->stream != NULL)
	      fclose(connection->stream);
	    connection->stream = NULL;
	    errors = TRUE;
	} else {
	    g_string_append(str, line);
	}
	/* terminate if we reached the last line (without '-' after numcode) */
    }while(!errors &&  !((strlen(line) < 4) || (line[3] == ' ')));
    
    xfree(line);	/* getline allocates with malloc. */

    if (errors) {
	/* Free the GString and its character data, and return NULL. */
	g_string_free(str, TRUE);
	reply = NULL;
    } else {
	    /* The resulting message received from the socket is stored in reply */
	    reply = str->str;
	/* Free the GString, but not its character data. */
	    g_string_free(str, FALSE);
    }

    return reply;
}

static void*
spd_events_handler(void* conn)
{
    char *reply;
    int reply_code;
    SPDConnection *connection = conn;

    while(1){

	/* Read the reply/event (block if none is available) */
        SPD_DBG("Getting reply in spd_events_handler");
	reply = get_reply(connection);
	if (reply == NULL){
	    SPD_DBG("ERROR: BROKEN SOCKET");
	    reply_code = -1;
	}else{
	    SPD_DBG("<< : |%s|\n", reply);
	    reply_code = get_err_code(reply);
	}

	if ((reply_code >= 700) && (reply_code < 800)){
	    int msg_id;
	    int client_id;
	    int err;

	    SPD_DBG("Callback detected: %s", reply);

	    /* This is an index mark */
	    /* Extract message id */
	    msg_id = get_param_int(reply, 1, &err);
	    if (err < 0){
	      SPD_DBG("Bad reply from Speech Dispatcher: %s (code %d)", reply, err);
	      break;
	    }
	    client_id = get_param_int(reply, 2, &err);
	    if (err < 0){
	      SPD_DBG("Bad reply from Speech Dispatcher: %s (code %d)", reply, err);
	      break;
	    }
	    /*  Decide if we want to call a callback */
	    if ((reply_code == 701) && (connection->callback_begin))
		connection->callback_begin(msg_id, client_id, SPD_EVENT_BEGIN);
	    if ((reply_code == 702) && (connection->callback_end))
		connection->callback_end(msg_id, client_id, SPD_EVENT_END);
	    if ((reply_code == 703) && (connection->callback_cancel))
		connection->callback_cancel(msg_id, client_id, SPD_EVENT_CANCEL);
	    if ((reply_code == 704) && (connection->callback_pause))
		connection->callback_pause(msg_id, client_id, SPD_EVENT_PAUSE);
	    if ((reply_code == 705) && (connection->callback_resume))
		connection->callback_resume(msg_id, client_id, SPD_EVENT_RESUME);
	    if ((reply_code == 700) && (connection->callback_im)){
		char* im;
		int err;
		im = get_param_str(reply, 3, &err);
		if ((err < 0) || (im == NULL)){
		  SPD_DBG("Broken reply from Speech Dispatcher: %s", reply);
		  break;
		}
		/* Call the callback */
		connection->callback_im(msg_id, client_id, SPD_EVENT_INDEX_MARK, im);
		xfree(im);
	    }

	}else{
	    /* This is a protocol reply */
	    pthread_mutex_lock(connection->mutex_reply_ready);
	    /* Prepare the reply to the reply buffer in connection */
	    if (reply != NULL){
		connection->reply = strdup(reply);
	    }else{
	      SPD_DBG("Connection reply is NULL");
	      connection->reply = NULL;
	      break;
	    }
	    /* Signal the reply is available on the condition variable */
	    /* this order is correct and necessary */
	    pthread_cond_signal(connection->cond_reply_ready);
	    pthread_mutex_lock(connection->mutex_reply_ack); 
	    pthread_mutex_unlock(connection->mutex_reply_ready);
	    /* Wait until it has bean read */
	    pthread_cond_wait(connection->cond_reply_ack, connection->mutex_reply_ack);
	    pthread_mutex_unlock(connection->mutex_reply_ack);
	    xfree(reply);
	    /* Continue */	
	}
    }
    /* In case of broken socket, we must still signal reply ready */
    if (connection->reply == NULL){
      SPD_DBG("Signalling reply ready after communication failure");
      pthread_mutex_unlock(connection->mutex_reply_ready);
      pthread_mutex_unlock(connection->mutex_reply_ack);
      if (connection->stream != NULL)
	fclose(connection->stream);
      connection->stream = NULL;
      pthread_cond_signal(connection->cond_reply_ready);
      pthread_exit(0);
    }
    return 0; 			/* to please gcc */
}

static int
ret_ok(char *reply)
{
	int err;

	if (reply == NULL) return -1;

	err = get_err_code(reply);
		
	if ((err>=100) && (err<300)) return 1;
	if (err>=300) return 0;
    
	SPD_FATAL("Internal error during communication.");
}

static char*
get_param_str(char* reply, int num, int *err)
{
    int i;
    char *tptr;
    char *pos;
    char *pos_begin;
    char *pos_end;
    char *rep;

    assert(err != NULL);

    if (num < 1){
	*err = -1;
	return NULL;
    }

    pos = reply;
    for (i=0; i<=num-2; i++){
	pos = strstr(pos, "\r\n");	
	if (pos == NULL){
	    *err = -2;
	    return NULL;
	}
	pos += 2;
    }

    if (strlen(pos) < 4) return NULL;
    
    *err = strtol(pos, &tptr, 10);
    if (*err >= 300 && *err <= 399)
	return NULL;

    if ((*tptr != '-') || (tptr != pos+3)){
	*err = -3;
	return NULL;
    }

    pos_begin = pos + 4;
    pos_end = strstr(pos_begin, "\r\n");
    if (pos_end == NULL){
	*err = -4;
	return NULL;
    }

    rep = (char*) strndup(pos_begin, pos_end - pos_begin);
    *err = 0;
    
    return rep;
}

static int
get_param_int(char* reply, int num, int *err)
{
    char *rep_str;
    char *tptr;
    int ret;

    rep_str = get_param_str(reply, num, err);
    if (rep_str == NULL){
	/* err is already set to the error return code, just return */
	return 0;
    }
    
    ret = strtol(rep_str, &tptr, 10);
    if (*tptr != '\0'){
	/* this is not a number */
	*err = -3;
	return 0;
    }    
    xfree(rep_str);

    return ret;
}

static int
get_err_code(char *reply)
{
    char err_code[4];
    int err;
	
    if (reply == NULL) return -1;
    SPD_DBG("spd_send_data:	reply: %s\n", reply);

    err_code[0] = reply[0];	err_code[1] = reply[1];
    err_code[2] = reply[2];	err_code[3] = '\0';

    SPD_DBG("ret_ok: err_code:	|%s|\n", err_code);
   
    if(isanum(err_code)){
        err = atoi(err_code);
    }else{
      SPD_DBG("ret_ok: not a number\n");
      return -1;	
    }

    return err;
}

/* isanum() tests if the given string is a number,
 *  returns 1 if yes, 0 otherwise. */
static int
isanum(char *str)
{
    int i;
    if (str == NULL) return 0;
    for(i=0;i<=strlen(str)-1;i++){
        if (!isdigit(str[i]))   return 0;
    }
    return 1;
}

static void*
xmalloc(size_t bytes)
{
    void *mem;

    mem = malloc(bytes);
    if (mem == NULL){
      SPD_FATAL("Not enough memmory!");
      exit(1);
    }
    
    return mem;
}

static void
xfree(void *ptr)
{
    if (ptr != NULL)
        free(ptr);
}

/*
 * escape_dot: Replace . with .. at the start of lines.
 * @text: text to escape
 * @Returns: An allocated string, containing the escaped text.
 */
static char*
escape_dot(const char *text)
{
    size_t orig_len = 0;
    const char *orig_end;
    char *result = NULL;
    char *result_ptr;
    static const char *ESCAPED_DOTLINE = "\r\n..";
    static const size_t ESCAPED_DOTLINELEN = 4;
    static const size_t DOTLINELEN = 3;

    if (text == NULL)
        return NULL;

orig_len = strlen(text);
    orig_end = text + orig_len;
    result = malloc((orig_len * 2 + 1) * sizeof(char));

    if (result == NULL)
        return NULL;

    result_ptr = result;

    /* We're over-allocating.  Even if we replaced every character
     * in text with "..", the length of the escaped string can be no more
     * than orig_len * 2.  We could tighten that upper bound with
     * a little more work.
     */

    if ((orig_len >= 1) && (text[0] == '.')) {
        *(result_ptr++) = '.';
        *(result_ptr++) = '.';
        text += 1;
    }

    while (text < orig_end) {
        if ((text[0] == '\r') && (text[1] == '\n') && (text[2] == '.')) {
            memcpy(result_ptr, ESCAPED_DOTLINE, ESCAPED_DOTLINELEN);
            result_ptr += ESCAPED_DOTLINELEN;
            text += DOTLINELEN;
        } else {
            *(result_ptr++) = *(text++);
        }
    }

    * result_ptr = '\0';
    return result;
}

#ifdef LIBSPEECHD_DEBUG
static void
SPD_DBG(char *format, ...)
{
        va_list args;

	pthread_mutex_lock(&spd_logging_mutex);    	
	va_start(args, format);
	vfprintf(spd_debug, format, args);
	va_end(args);
	fprintf(spd_debug, "\n");
	fflush(spd_debug);
	pthread_mutex_unlock(&spd_logging_mutex);    	
}
#else  /* LIBSPEECHD_DEBUG */
static void
SPD_DBG(char *format, ...)
{
}
#endif /* LIBSPEECHD_DEBUG */
