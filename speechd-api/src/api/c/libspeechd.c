/*
  libspeechd.c - Shared library for easy acces to Speech Dispatcher functions
 *
 * Copyright (C) 2001, 2002, 2003, 2006, 2007, 2008 Brailcom, o.p.s.
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * $Id: libspeechd.c,v 1.37 2008-12-23 09:15:32 pdm Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

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

/*
 * This is needed because speechd_types.h is in a different location in
 * the source tree's include directory than it will be when it is
 * installed on the user's system.
 */
#include <speechd_types.h>
#include <speechd_defines.h>
#include "libspeechd.h"

/* Comment/uncomment to switch debugging on/off */
// #define LIBSPEECHD_DEBUG 1

/* Unless there is an fatal error, it doesn't print anything */
#define SPD_FATAL(msg) { printf("Fatal error (libspeechd) [%s:%d]:"msg, __FILE__, __LINE__); fflush(stdout); exit(EXIT_FAILURE); }

/* --------------  Private functions headers ------------------------*/

#ifdef LIBSPEECHD_DEBUG
static FILE *spd_debug = NULL;
#endif

static int spd_set_priority(SPDConnection * connection, SPDPriority priority);
static char *escape_dot(const char *text);
static int isanum(char *str);
static char *get_reply(SPDConnection * connection);
static int get_err_code(char *reply);
static char *get_param_str(char *reply, int num, int *err);
static int get_param_int(char *reply, int num, int *err);
static int ret_ok(char *reply);
static void SPD_DBG(char *format, ...);
static void *spd_events_handler(void *);

const int range_low = -100;
const int range_high = 100;

pthread_mutex_t spd_logging_mutex;

struct SPDConnection_threaddata {
	pthread_t events_thread;
	pthread_cond_t cond_reply_ready;
	pthread_mutex_t mutex_reply_ready;
	pthread_cond_t cond_reply_ack;
	pthread_mutex_t mutex_reply_ack;
};

/*
 * Added by Willie Walker - strndup and getline were GNU libc extensions
 * that were adopted in the POSIX.1-2008 standard, but are not yet found
 * on all systems.
 */
#ifndef HAVE_STRNDUP
char *strndup(const char *s, size_t n)
{
	size_t nAvail;
	char *p;

	if (!s)
		return 0;

	if (strlen(s) > n)
		nAvail = n + 1;
	else
		nAvail = strlen(s) + 1;
	p = malloc(nAvail);
	memcpy(p, s, nAvail);
	p[nAvail - 1] = '\0';

	return p;
}
#endif /* HAVE_STRNDUP */

#ifndef HAVE_GETLINE
#define BUFFER_LEN 256
ssize_t getline(char **lineptr, size_t * n, FILE * f)
{
	int ch;
	size_t m = 0;
	ssize_t buf_len = 0;
	char *buf = NULL;
	char *p = NULL;

	if (errno != 0) {
		SPD_DBG("getline: errno came in as %d!!!\n", errno);
		errno = 0;
	}
	while ((ch = getc(f)) != EOF) {
		if (errno != 0)
			return -1;
		if (m++ >= buf_len) {
			buf_len += BUFFER_LEN;
			buf = (char *)realloc(buf, buf_len + 1);
			if (buf == NULL) {
				SPD_DBG("buf==NULL");
				return -1;
			}
			p = buf + buf_len - BUFFER_LEN;
		}
		*p = ch;
		p++;
		if (ch == '\n')
			break;
	}
	if (m == 0) {
		SPD_DBG("getline: m=%d!", m);
		return -1;
	} else {
		*p = '\0';
		*lineptr = buf;
		*n = m;
		return m;
	}
}
#endif /* HAVE_GETLINE */

/* --------------------- Public functions ------------------------- */

#define SPD_REPLY_BUF_SIZE 65536

/* Determine address for the unix socket */
static char *_get_default_unix_socket_name(void)
{
	GString *socket_filename;
	char *h;
	const char *rundir = g_get_user_runtime_dir();
	socket_filename = g_string_new("");
	g_string_printf(socket_filename, "%s/speech-dispatcher/speechd.sock",
			rundir);
	// Do not return glib string, but glibc string...
	h = strdup(socket_filename->str);
	g_string_free(socket_filename, 1);
	return h;
}

void SPDConnectionAddress__free(SPDConnectionAddress * address)
{
	if (!address)
		return;
	free(address->unix_socket_name);
	free(address->inet_socket_host);
	free(address->dbus_bus);
	free(address);
}

SPDConnectionAddress *spd_get_default_address(char **error)
{
	const gchar *env_address = g_getenv("SPEECHD_ADDRESS");
	gchar **pa;		/* parsed address */
	SPDConnectionAddress *address = malloc(sizeof(SPDConnectionAddress));
	address->unix_socket_name = NULL;
	address->inet_socket_host = NULL;
	address->dbus_bus = NULL;

	if (env_address == NULL) {	// Default method = unix sockets
		address->method = SPD_METHOD_UNIX_SOCKET;
		address->unix_socket_name = _get_default_unix_socket_name();
	} else {
		pa = g_strsplit(env_address, ":", 0);
		assert(pa);
		if (!g_strcmp0(pa[0], "unix_socket") || pa[0] == NULL) {	// Unix sockets
			address->method = SPD_METHOD_UNIX_SOCKET;
			if (pa[1] == NULL) {
				address->unix_socket_name =
				    _get_default_unix_socket_name();
			} else {
				address->unix_socket_name = strdup(pa[1]);
			}
		} else if (!g_strcmp0(pa[0], "inet_socket")) {	// Inet sockets
			address->method = SPD_METHOD_INET_SOCKET;
			if (pa[1] == NULL) {
				address->inet_socket_host = strdup("127.0.0.1");
				address->inet_socket_port = 6560;
			} else {
				address->inet_socket_host = strdup(pa[1]);
				if (pa[2] == NULL) {
					address->inet_socket_port =
					    SPEECHD_DEFAULT_PORT;
				} else {
					address->inet_socket_port = atoi(pa[2]);
				}
			}
		} else {	// Unknown or unsupported method requested
			*error =
			    strdup
			    ("Unknown or unsupported communication method");
			SPDConnectionAddress__free(address);
			address = NULL;
		}
		g_strfreev(pa);
	}
	return address;
}

static void _init_debug(void)
{
#ifdef LIBSPEECHD_DEBUG
	if (!spd_debug) {
		spd_debug = fopen("/tmp/libspeechd.log", "w");
		if (spd_debug == NULL)
			SPD_FATAL("COULDN'T ACCES FILE INTENDED FOR DEBUG");

		if (pthread_mutex_init(&spd_logging_mutex, NULL)) {
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

SPDConnection *spd_open(const char *client_name, const char *connection_name,
			const char *user_name, SPDConnectionMode mode)
{
	char *error;
	int autospawn = 1;
	SPDConnection *conn;
	conn = spd_open2(client_name, connection_name, user_name,
			 mode, NULL, autospawn, &error);
	if (!conn) {
		_init_debug();
		assert(error);
		SPD_DBG("Could not connect to Speech Dispatcher: %s", error);
		free(error);
	}
	return conn;
}

#define MAX_IP_SIZE 16+1
/* TODO: This only works in IPV4 */
static char *resolve_host(char *host_name_or_ip, int *is_localhost,
			  gchar ** error)
{
	struct addrinfo *addr_result;
	int err;
	char *resolve_buffer = malloc(MAX_IP_SIZE * sizeof(char));
	const char *resolved_ip = NULL;
	char *ip;
	*error = NULL;
	struct sockaddr_in *addr_in;

	if (resolve_buffer == NULL) {
		*error = g_strdup("Failed to allocate memory.");
		return NULL;
	}

	err = getaddrinfo(host_name_or_ip, 0, NULL, &addr_result);
	if (err) {
		*error =
		    g_strdup_printf("Can't resolve address %d due to error %s:",
				    err, gai_strerror(err));
		free(resolve_buffer);
		return NULL;
	}
	/* Take the first address returned as we are only interested in host ip */
	addr_in = (struct sockaddr_in *)addr_result->ai_addr;
	resolved_ip =
	    inet_ntop(AF_INET, &(addr_in->sin_addr.s_addr), resolve_buffer,
		      MAX_IP_SIZE);
	if (resolved_ip == NULL) {
		*error =
		    g_strdup_printf
		    ("Could not convert address, due to the following error: %s",
		     strerror(errno));
		freeaddrinfo(addr_result);
		free(resolve_buffer);
		return NULL;
	}

	if (!strncmp(resolved_ip, "127.", 4)) {
		*is_localhost = 1;
		/* In case of local addresses, use 127.0.0.1 which is guaranteed
		   to be local and the server listens on it */
		free(resolve_buffer);
		ip = strdup("127.0.0.1");
	} else {
		*is_localhost = 0;
		ip = resolve_buffer;
	}
	freeaddrinfo(addr_result);
	return ip;
}

static int
spawn_server(SPDConnectionAddress * address, int is_localhost,
	     gchar ** spawn_error)
{
	gchar *speechd_cmd[16];
	gchar *stderr_output;
	gboolean spawn_ok;
	GError *gerror = NULL;
	int exit_status;
	int i;

	if ((address->method == SPD_METHOD_INET_SOCKET) && (!is_localhost)) {
		*spawn_error =
		    g_strdup
		    ("Spawn failed, the given network address doesn't seem to be on localhost");
		return 1;
	}

	speechd_cmd[0] = g_strdup(SPD_SPAWN_CMD);
	speechd_cmd[1] = g_strdup("--spawn");
	speechd_cmd[2] = g_strdup("--communication-method");
	if (address->method == SPD_METHOD_INET_SOCKET) {
		speechd_cmd[3] = g_strdup("inet_socket");
		speechd_cmd[4] = g_strdup("--port");
		speechd_cmd[5] =
		    g_strdup_printf("%d", address->inet_socket_port);
		speechd_cmd[6] = NULL;
	} else if (address->method == SPD_METHOD_UNIX_SOCKET) {
		speechd_cmd[3] = g_strdup("unix_socket");
		speechd_cmd[4] = g_strdup("--socket-path");
		speechd_cmd[5] =
		    g_strdup_printf("%s", address->unix_socket_name);
		speechd_cmd[6] = NULL;
	} else
		assert(0);

	spawn_ok =
	    g_spawn_sync(NULL, (gchar **) speechd_cmd, NULL,
			 G_SPAWN_SEARCH_PATH | G_SPAWN_STDOUT_TO_DEV_NULL, NULL,
			 NULL, NULL, &stderr_output, &exit_status, &gerror);
	for (i = 0; speechd_cmd[i] != NULL; i++)
		g_free(speechd_cmd[i]);
	if (!spawn_ok) {
		*spawn_error =
		    g_strdup_printf("Autospawn failed. Spawn error %d: %s",
				    gerror->code, gerror->message);
		return 1;
	} else {
		if (exit_status) {
			*spawn_error =
			    g_strdup_printf
			    ("Autospawn failed. Speech Dispatcher refused to start with error code, "
			     "stating this as a reason: %s", stderr_output);
			return 1;
		} else {
			*spawn_error = NULL;
			return 0;
		}
	}
	assert(0);
}

SPDConnection *spd_open2(const char *client_name, const char *connection_name,
			 const char *user_name, SPDConnectionMode mode,
			 SPDConnectionAddress * address, int autospawn,
			 char **error_result)
{
	SPDConnection *connection = NULL;
	SPDConnectionAddress *defaultAddress = NULL;
	char *set_client_name = NULL;
	char *conn_name = NULL;
	char *usr_name = NULL;
	int ret;
	int tcp_no_delay = 1;

	/* Autospawn related */
	int spawn_err;
	gchar *spawn_report;
	char *host_ip;
	int is_localhost = 1;

	struct sockaddr_in address_inet;
	struct sockaddr_un address_unix;
	struct sockaddr *sock_address;
	size_t sock_address_len;

	_init_debug();

	if (client_name == NULL) {
		*error_result = strdup("ERROR: Client name not specified");
		SPD_DBG(*error_result);
		return NULL;
	}

	if (user_name == NULL) {
		usr_name = strdup((char *)g_get_user_name());
	} else
		usr_name = strdup(user_name);

	if (connection_name == NULL)
		conn_name = strdup("main");
	else
		conn_name = strdup(connection_name);

	if (address == NULL) {
		char *err = NULL;
		defaultAddress = spd_get_default_address(&err);
		address = defaultAddress;
		if (!address) {
			assert(err);
			*error_result = err;
			SPD_DBG(*error_result);
			goto out;
		}
	}

	/* Connect to server using the selected method */
	connection = malloc(sizeof(SPDConnection));
	if (address->method == SPD_METHOD_INET_SOCKET) {
		gchar *resolve_error;
		host_ip =
		    resolve_host(address->inet_socket_host, &is_localhost,
				 &resolve_error);
		if (host_ip == NULL) {
			*error_result = strdup(resolve_error);
			g_free(resolve_error);
			free(connection);
			connection = NULL;
			goto out;
		}
		address_inet.sin_addr.s_addr = inet_addr(host_ip);
		free(host_ip);
		address_inet.sin_port = htons(address->inet_socket_port);
		address_inet.sin_family = AF_INET;
		connection->socket = socket(AF_INET, SOCK_STREAM, 0);
		sock_address = (struct sockaddr *)&address_inet;
		sock_address_len = sizeof(address_inet);
	} else if (address->method == SPD_METHOD_UNIX_SOCKET) {
		/* Create the unix socket */
		address_unix.sun_family = AF_UNIX;
		strncpy(address_unix.sun_path, address->unix_socket_name,
			sizeof(address_unix.sun_path));
		address_unix.sun_path[sizeof(address_unix.sun_path) - 1] = '\0';
		connection->socket = socket(AF_UNIX, SOCK_STREAM, 0);
		sock_address = (struct sockaddr *)&address_unix;
		sock_address_len = SUN_LEN(&address_unix);
	} else
		SPD_FATAL("Unsupported connection method for spd_open2()");

	if (connection->socket < 0) {
		free(connection);
		connection = NULL;
		goto out;
	}
	ret = connect(connection->socket, sock_address, sock_address_len);
	if (ret == -1) {
		/* Suppose server might not be running, try to autospawn (autostart) it */
		if (autospawn) {
			spawn_err =
			    spawn_server(address, is_localhost, &spawn_report);
			if (!spawn_err)
				spawn_report =
				    g_strdup("Server successfully autospawned");
			ret =
			    connect(connection->socket, sock_address,
				    sock_address_len);
		} else {
			spawn_report = g_strdup("Autospawn disabled");
		}
		if (ret == -1) {
			if (address->method == SPD_METHOD_INET_SOCKET)
				*error_result =
				    g_strdup_printf
				    ("Error: Can't connect to %s on port %d using inet sockets: %s. "
				     "Autospawn: %s", address->inet_socket_host,
				     address->inet_socket_port, strerror(errno),
				     spawn_report);
			else if (address->method == SPD_METHOD_UNIX_SOCKET)
				*error_result =
				    g_strdup_printf
				    ("Error: Can't connect to unix socket %s: %s. Autospawn: %s",
				     address->unix_socket_name, strerror(errno),
				     spawn_report);
			else
				assert(0);
			SPD_DBG(*error_result);
			close(connection->socket);
			free(connection);
			connection = NULL;
			goto out;
		}
		g_free(spawn_report);
	}

	if (address->method == SPD_METHOD_INET_SOCKET)
		setsockopt(connection->socket, IPPROTO_TCP, TCP_NODELAY,
			   &tcp_no_delay, sizeof(int));

	connection->callback_begin = NULL;
	connection->callback_end = NULL;
	connection->callback_im = NULL;
	connection->callback_pause = NULL;
	connection->callback_resume = NULL;
	connection->callback_cancel = NULL;

	connection->mode = mode;

	/* Create a stream from the socket */
	connection->stream = fdopen(connection->socket, "r");
	if (!connection->stream)
		SPD_FATAL("Can't create a stream for socket, fdopen() failed.");
	/* Switch to line buffering mode */
	ret = setvbuf(connection->stream, NULL, _IONBF, SPD_REPLY_BUF_SIZE);
	if (ret)
		SPD_FATAL("Can't set buffering, setvbuf failed.");

	pthread_mutex_init(&connection->ssip_mutex, NULL);

	if (mode == SPD_MODE_THREADED) {
		SPD_DBG
		    ("Initializing threads, condition variables and mutexes...");
		connection->td = malloc(sizeof(*connection->td));
		pthread_cond_init(&connection->td->cond_reply_ready, NULL);
		pthread_mutex_init(&connection->td->mutex_reply_ready, NULL);
		pthread_cond_init(&connection->td->cond_reply_ack, NULL);
		pthread_mutex_init(&connection->td->mutex_reply_ack, NULL);
		ret =
		    pthread_create(&connection->td->events_thread, NULL,
				   spd_events_handler, connection);
		if (ret != 0) {
			*error_result = strdup("Thread initialization failed");
			SPD_DBG(*error_result);
			fclose(connection->stream);
			close(connection->socket);
			free(connection);
			connection = NULL;
			goto out;
		}
	}

	/* By now, the connection is created and operational */
	set_client_name =
	    g_strdup_printf("SET SELF CLIENT_NAME \"%s:%s:%s\"", usr_name,
			    client_name, conn_name);
	ret = spd_execute_command_wo_mutex(connection, set_client_name);

out:
	free(usr_name);
	free(conn_name);
	free(set_client_name);
	SPDConnectionAddress__free(defaultAddress);
	return connection;
}

#define RET(r) \
	{ \
		pthread_mutex_unlock(&connection->ssip_mutex); \
		return r; \
	}

/* Close a Speech Dispatcher connection */
void spd_close(SPDConnection * connection)
{

	pthread_mutex_lock(&connection->ssip_mutex);

	if (connection->mode == SPD_MODE_THREADED) {
		pthread_cancel(connection->td->events_thread);
		pthread_mutex_destroy(&connection->td->mutex_reply_ready);
		pthread_mutex_destroy(&connection->td->mutex_reply_ack);
		pthread_cond_destroy(&connection->td->cond_reply_ready);
		pthread_cond_destroy(&connection->td->cond_reply_ack);
		pthread_join(connection->td->events_thread, NULL);
		connection->mode = SPD_MODE_SINGLE;
		free(connection->td);
	}

	/* close the socket */
	close(connection->socket);

	pthread_mutex_unlock(&connection->ssip_mutex);

	pthread_mutex_destroy(&connection->ssip_mutex);
	free(connection);
}

/* Helper functions for spd_say. */
static inline int
spd_say_prepare(SPDConnection * connection, SPDPriority priority,
		const char *text, char **escaped_text)
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

static inline int spd_say_sending(SPDConnection * connection, const char *text)
{
	int msg_id = -1;
	int err = 0;
	char *reply = NULL;
	char *pret = NULL;

	/* Send data */
	SPD_DBG("Sending data");
	pret = spd_send_data_wo_mutex(connection, text, SPD_NO_REPLY);
	if (pret == NULL) {
		SPD_DBG("Can't send data wo mutex");
	} else {
		/* Terminate data flow */
		SPD_DBG("Terminating data flow");
		err =
		    spd_execute_command_with_reply(connection, "\r\n.", &reply);
		if (err) {
			SPD_DBG("Can't terminate data flow");
		} else {
			msg_id = get_param_int(reply, 1, &err);
			if (err < 0) {
				SPD_DBG
				    ("Can't determine SSIP message unique ID parameter.");
				msg_id = -1;
			}
		}
	}

	free(reply);
	free(pret);
	return msg_id;
}

/* Say TEXT with priority PRIORITY.
 * Returns msg_uid on success, -1 otherwise. */
int spd_say(SPDConnection * connection, SPDPriority priority, const char *text)
{
	char *escaped_text = NULL;
	int msg_id = -1;
	int prepare_failed = 0;

	if (text != NULL) {
		pthread_mutex_lock(&connection->ssip_mutex);

		prepare_failed =
		    spd_say_prepare(connection, priority, text, &escaped_text);
		if (!prepare_failed)
			msg_id = spd_say_sending(connection, escaped_text);

		free(escaped_text);
		pthread_mutex_unlock(&connection->ssip_mutex);
	} else {
		SPD_DBG("spd_say called with a NULL argument for <text>");
	}

	SPD_DBG("Returning from spd_say");
	return msg_id;
}

/* The same as spd_say, accepts also formated strings */
int
spd_sayf(SPDConnection * connection, SPDPriority priority, const char *format,
	 ...)
{
	static int ret;
	va_list args;
	char *buf;

	if (format == NULL)
		return -1;

	/* Print the text to buffer */
	va_start(args, format);
	buf = g_strdup_vprintf(format, args);
	va_end(args);

	/* Send the buffer to Speech Dispatcher */
	ret = spd_say(connection, priority, buf);
	free(buf);

	return ret;
}

int spd_stop(SPDConnection * connection)
{
	return spd_execute_command(connection, "STOP SELF");
}

int spd_stop_all(SPDConnection * connection)
{
	return spd_execute_command(connection, "STOP ALL");
}

int spd_stop_uid(SPDConnection * connection, int target_uid)
{
	static char command[16];

	sprintf(command, "STOP %d", target_uid);
	return spd_execute_command(connection, command);
}

int spd_cancel(SPDConnection * connection)
{
	return spd_execute_command(connection, "CANCEL SELF");
}

int spd_cancel_all(SPDConnection * connection)
{
	return spd_execute_command(connection, "CANCEL ALL");
}

int spd_cancel_uid(SPDConnection * connection, int target_uid)
{
	static char command[16];

	sprintf(command, "CANCEL %d", target_uid);
	return spd_execute_command(connection, command);
}

int spd_pause(SPDConnection * connection)
{
	return spd_execute_command(connection, "PAUSE SELF");
}

int spd_pause_all(SPDConnection * connection)
{
	return spd_execute_command(connection, "PAUSE ALL");
}

int spd_pause_uid(SPDConnection * connection, int target_uid)
{
	char command[16];

	sprintf(command, "PAUSE %d", target_uid);
	return spd_execute_command(connection, command);
}

int spd_resume(SPDConnection * connection)
{
	return spd_execute_command(connection, "RESUME SELF");
}

int spd_resume_all(SPDConnection * connection)
{
	return spd_execute_command(connection, "RESUME ALL");
}

int spd_resume_uid(SPDConnection * connection, int target_uid)
{
	static char command[16];

	sprintf(command, "RESUME %d", target_uid);
	return spd_execute_command(connection, command);
}

int
spd_key(SPDConnection * connection, SPDPriority priority, const char *key_name)
{
	char *command_key;
	int ret;

	if (key_name == NULL)
		return -1;

	pthread_mutex_lock(&connection->ssip_mutex);

	ret = spd_set_priority(connection, priority);
	if (ret)
		RET(-1);

	command_key = g_strdup_printf("KEY %s", key_name);
	ret = spd_execute_command_wo_mutex(connection, command_key);
	free(command_key);
	if (ret)
		RET(-1);

	pthread_mutex_unlock(&connection->ssip_mutex);

	return 0;
}

int
spd_char(SPDConnection * connection, SPDPriority priority,
	 const char *character)
{
	static char command[16];
	int ret;

	if (character == NULL)
		return -1;
	if (strlen(character) > 6)
		return -1;

	pthread_mutex_lock(&connection->ssip_mutex);

	ret = spd_set_priority(connection, priority);
	if (ret)
		RET(-1);

	sprintf(command, "CHAR %s", character);
	ret = spd_execute_command_wo_mutex(connection, command);
	if (ret)
		RET(-1);

	pthread_mutex_unlock(&connection->ssip_mutex);

	return 0;
}

int
spd_wchar(SPDConnection * connection, SPDPriority priority, wchar_t wcharacter)
{
	static char command[16];
	char character[8];
	int ret;

	pthread_mutex_lock(&connection->ssip_mutex);

	ret = wcrtomb(character, wcharacter, NULL);
	if (ret <= 0)
		RET(-1);

	ret = spd_set_priority(connection, priority);
	if (ret)
		RET(-1);

	assert(character != NULL);
	sprintf(command, "CHAR %s", character);
	ret = spd_execute_command_wo_mutex(connection, command);
	if (ret)
		RET(-1);

	pthread_mutex_unlock(&connection->ssip_mutex);

	return 0;
}

int
spd_sound_icon(SPDConnection * connection, SPDPriority priority,
	       const char *icon_name)
{
	char *command;
	int ret;

	if (icon_name == NULL)
		return -1;

	pthread_mutex_lock(&connection->ssip_mutex);

	ret = spd_set_priority(connection, priority);
	if (ret)
		RET(-1);

	command = g_strdup_printf("SOUND_ICON %s", icon_name);
	ret = spd_execute_command_wo_mutex(connection, command);
	free(command);
	if (ret)
		RET(-1);

	pthread_mutex_unlock(&connection->ssip_mutex);

	return 0;
}

// Set functions for Punctuation
int spd_w_set_punctuation(SPDConnection * connection, SPDPunctuation type,
			  const char *who)
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

int spd_set_punctuation(SPDConnection * connection, SPDPunctuation type)
{
	return spd_w_set_punctuation(connection, type, SPD_SELF);
}

int spd_set_punctuation_all(SPDConnection * connection, SPDPunctuation type)
{
	return spd_w_set_punctuation(connection, type, SPD_ALLCLIENTS);
}

int spd_set_punctuation_uid(SPDConnection * connection, SPDPunctuation type,
			    unsigned int uid)
{
	char who[8];
	sprintf(who, "%d", uid);
	return spd_w_set_punctuation(connection, type, who);
}

// Set functions for Capital Letters
int spd_w_set_capital_letters(SPDConnection * connection,
			      SPDCapitalLetters type, const char *who)
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

int spd_set_capital_letters(SPDConnection * connection, SPDCapitalLetters type)
{
	return spd_w_set_capital_letters(connection, type, SPD_SELF);
}

int spd_set_capital_letters_all(SPDConnection * connection,
				SPDCapitalLetters type)
{
	return spd_w_set_capital_letters(connection, type, SPD_ALLCLIENTS);
}

int spd_set_capital_letters_uid(SPDConnection * connection,
				SPDCapitalLetters type, unsigned int uid)
{
	char who[8];
	sprintf(who, "%d", uid);
	return spd_w_set_capital_letters(connection, type, who);
}

// Set functions for Spelling
int spd_w_set_spelling(SPDConnection * connection, SPDSpelling type,
		       const char *who)
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

int spd_set_spelling(SPDConnection * connection, SPDSpelling type)
{
	return spd_w_set_spelling(connection, type, SPD_SELF);
}

int spd_set_spelling_all(SPDConnection * connection, SPDSpelling type)
{
	return spd_w_set_spelling(connection, type, SPD_ALLCLIENTS);
}

int spd_set_spelling_uid(SPDConnection * connection, SPDSpelling type,
			 unsigned int uid)
{
	char who[8];
	sprintf(who, "%d", uid);
	return spd_w_set_spelling(connection, type, who);
}

int spd_set_data_mode(SPDConnection * connection, SPDDataMode mode)
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

// Set functions for Voice type
int spd_w_set_voice_type(SPDConnection * connection, SPDVoiceType type,
			 const char *who)
{
	static char command[64];

	switch (type) {
	case SPD_MALE1:
		sprintf(command, "SET %s VOICE_TYPE MALE1", who);
		break;
	case SPD_MALE2:
		sprintf(command, "SET %s VOICE_TYPE MALE2", who);
		break;
	case SPD_MALE3:
		sprintf(command, "SET %s VOICE_TYPE MALE3", who);
		break;
	case SPD_FEMALE1:
		sprintf(command, "SET %s VOICE_TYPE FEMALE1", who);
		break;
	case SPD_FEMALE2:
		sprintf(command, "SET %s VOICE_TYPE FEMALE2", who);
		break;
	case SPD_FEMALE3:
		sprintf(command, "SET %s VOICE_TYPE FEMALE3", who);
		break;
	case SPD_CHILD_MALE:
		sprintf(command, "SET %s VOICE_TYPE CHILD_MALE", who);
		break;
	case SPD_CHILD_FEMALE:
		sprintf(command, "SET %s VOICE_TYPE CHILD_FEMALE", who);
		break;
	default:
		return -1;
	}

	return spd_execute_command(connection, command);
}

int spd_set_voice_type(SPDConnection * connection, SPDVoiceType type)
{
	return spd_w_set_voice_type(connection, type, SPD_SELF);
}

int spd_set_voice_type_all(SPDConnection * connection, SPDVoiceType type)
{
	return spd_w_set_voice_type(connection, type, SPD_ALLCLIENTS);
}

int spd_set_voice_type_uid(SPDConnection * connection, SPDVoiceType type,
			   unsigned int uid)
{
	char who[8];
	sprintf(who, "%d", uid);
	return spd_w_set_voice_type(connection, type, who);
}

// Set function for Voice type
SPDVoiceType spd_get_voice_type(SPDConnection * connection)
{
	char *command;
	SPDVoiceType ret;
	int err;
	char *reply = NULL;
	command = g_strdup_printf("GET voice_type");
	spd_execute_command_with_reply(connection, command, &reply);
	free(command);
	ret = get_param_int(reply, 1, &err);
	free(reply);
	return ret;
}

static int spd_w_set_command_int(SPDConnection * connection,
				 const char *ssip_name, signed int val,
				 const char *who)
{
	static char command[64];
	// NOTE: if any new int ssip_name are added that don't use -100 - 100 as their
	// range, these values will need to become parameters (or the new ssip_name)
	// methods will need to call a different helper method.
	if (val < range_low || val > range_high)
		return -1;
	sprintf(command, "SET %s %s %d", who, ssip_name, val);
	return spd_execute_command(connection, command);
}

static int spd_get_command_int(SPDConnection * connection,
			       const char *ssip_name)
{
	char *command;
	int ret = 0;
	int err;
	char *reply = NULL;
	command = g_strdup_printf("GET %s", ssip_name);
	spd_execute_command_with_reply(connection, command, &reply);
	free(command);
	ret = get_param_int(reply, 1, &err);
	free(reply);
	return ret;
}

static int spd_w_set_command_str(SPDConnection * connection,
				 const char *ssip_name, const char *str,
				 const char *who)
{
	char *command;
	int ret;
	if (str == NULL)
		return -1;
	command = g_strdup_printf("SET %s %s %s", who, ssip_name, str);
	ret = spd_execute_command(connection, command);
	free(command);
	return ret;
}

static char *spd_get_command_str(SPDConnection * connection,
				 const char *ssip_name)
{
	char *command;
	char *ret = NULL;
	int err;
	char *reply = NULL;
	command = g_strdup_printf("GET %s", ssip_name);
	spd_execute_command_with_reply(connection, command, &reply);
	free(command);
	ret = get_param_str(reply, 1, &err);
	free(reply);
	return ret;
}

// Set functions for rate
int spd_set_voice_rate(SPDConnection * connection, signed int rate)
{
	return spd_w_set_command_int(connection, SPD_RATE, rate, SPD_SELF);
}

int spd_set_voice_rate_all(SPDConnection * connection, signed int rate)
{
	return spd_w_set_command_int(connection, SPD_RATE, rate,
				     SPD_ALLCLIENTS);
}

int spd_set_voice_rate_uid(SPDConnection * connection, signed int rate,
			   unsigned int uid)
{
	char who[8];
	sprintf(who, "%d", uid);
	return spd_w_set_command_int(connection, SPD_RATE, rate, who);
}

// Get function for rate
int spd_get_voice_rate(SPDConnection * connection)
{
	return spd_get_command_int(connection, SPD_RATE);
}

// Set functions for pitch
int spd_set_voice_pitch(SPDConnection * connection, signed int pitch)
{
	return spd_w_set_command_int(connection, SPD_PITCH, pitch, SPD_SELF);
}

int spd_set_voice_pitch_all(SPDConnection * connection, signed int pitch)
{
	return spd_w_set_command_int(connection, SPD_PITCH, pitch,
				     SPD_ALLCLIENTS);
}

int spd_set_voice_pitch_uid(SPDConnection * connection, signed int pitch,
			    unsigned int uid)
{
	char who[8];
	sprintf(who, "%d", uid);
	return spd_w_set_command_int(connection, SPD_PITCH, pitch, who);
}

// Get function for pitch
int spd_get_voice_pitch(SPDConnection * connection)
{
	return spd_get_command_int(connection, SPD_PITCH);
}

// Set functions for pitch_range
int spd_set_voice_pitch_range(SPDConnection * connection,
			      signed int pitch_range)
{
	return spd_w_set_command_int(connection, SPD_PITCH_RANGE, pitch_range,
				     SPD_SELF);
}

int spd_set_voice_pitch_range_all(SPDConnection * connection,
				  signed int pitch_range)
{
	return spd_w_set_command_int(connection, SPD_PITCH, pitch_range,
				     SPD_ALLCLIENTS);
}

int spd_set_voice_pitch_range_uid(SPDConnection * connection,
				  signed int pitch_range, unsigned int uid)
{
	char who[8];
	sprintf(who, "%d", uid);
	return spd_w_set_command_int(connection, SPD_PITCH, pitch_range, who);
}

// Set functions for volume
int spd_set_volume(SPDConnection * connection, signed int volume)
{
	return spd_w_set_command_int(connection, SPD_VOLUME, volume, SPD_SELF);
}

int spd_set_volume_all(SPDConnection * connection, signed int volume)
{
	return spd_w_set_command_int(connection, SPD_VOLUME, volume,
				     SPD_ALLCLIENTS);
}

int spd_set_volume_uid(SPDConnection * connection, signed int volume,
		       unsigned int uid)
{
	char who[8];
	sprintf(who, "%d", uid);
	return spd_w_set_command_int(connection, SPD_VOLUME, volume, who);
}

// Get function for volume
int spd_get_volume(SPDConnection * connection)
{
	return spd_get_command_int(connection, SPD_VOLUME);
}

// Set functions for language
int spd_set_language(SPDConnection * connection, const char *language)
{
	return spd_w_set_command_str(connection, SPD_LANGUAGE, language,
				     SPD_SELF);
}

int spd_set_language_all(SPDConnection * connection, const char *language)
{
	return spd_w_set_command_str(connection, SPD_LANGUAGE, language,
				     SPD_ALLCLIENTS);
}

int spd_set_language_uid(SPDConnection * connection, const char *language,
			 unsigned int uid)
{
	char who[8];
	sprintf(who, "%d", uid);
	return spd_w_set_command_str(connection, SPD_LANGUAGE, language, who);
}

// Get function for language
char *spd_get_language(SPDConnection * connection)
{
	return spd_get_command_str(connection, SPD_LANGUAGE);
}

// Set functions for output_module
int spd_set_output_module(SPDConnection * connection, const char *output_module)
{
	return spd_w_set_command_str(connection, SPD_OUTPUT_MODULE,
				     output_module, SPD_SELF);
}

int spd_set_output_module_all(SPDConnection * connection,
			      const char *output_module)
{
	return spd_w_set_command_str(connection, SPD_OUTPUT_MODULE,
				     output_module, SPD_ALLCLIENTS);
}

int spd_set_output_module_uid(SPDConnection * connection,
			      const char *output_module, unsigned int uid)
{
	char who[8];
	sprintf(who, "%d", uid);
	return spd_w_set_command_str(connection, SPD_OUTPUT_MODULE,
				     output_module, who);
}

// Get function for output_module
char *spd_get_output_module(SPDConnection * connection)
{
	return spd_get_command_str(connection, SPD_OUTPUT_MODULE);
}

// Set functions for synthesis_voice
int spd_set_synthesis_voice(SPDConnection * connection, const char *voice_name)
{
	return spd_w_set_command_str(connection, SPD_SYNTHESIS_VOICE,
				     voice_name, SPD_SELF);
}

int spd_set_synthesis_voice_all(SPDConnection * connection,
				const char *voice_name)
{
	return spd_w_set_command_str(connection, SPD_SYNTHESIS_VOICE,
				     voice_name, SPD_ALLCLIENTS);
}

int spd_set_synthesis_voice_uid(SPDConnection * connection,
				const char *voice_name, unsigned int uid)
{
	char who[8];
	sprintf(who, "%d", uid);
	return spd_w_set_command_str(connection, SPD_SYNTHESIS_VOICE,
				     voice_name, who);
}

int
spd_set_notification_on(SPDConnection * connection,
			SPDNotification notification)
{
	if (connection->mode == SPD_MODE_THREADED)
		return spd_set_notification(connection, notification, "on");
	else
		return -1;
}

int
spd_set_notification_off(SPDConnection * connection,
			 SPDNotification notification)
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
spd_set_notification(SPDConnection * connection, SPDNotification notification,
		     const char *state)
{
	static char command[64];
	int ret;

	if (connection->mode != SPD_MODE_THREADED)
		return -1;

	if (state == NULL) {
		SPD_DBG("Requested state is NULL");
		return -1;
	}
	if (strcmp(state, "on") && strcmp(state, "off")) {
		SPD_DBG("Invalid argument for spd_set_notification: %s", state);
		return -1;
	}

	pthread_mutex_lock(&connection->ssip_mutex);

	NOTIFICATION_SET(SPD_INDEX_MARKS, "index_marks");
	NOTIFICATION_SET(SPD_BEGIN, "begin");
	NOTIFICATION_SET(SPD_END, "end");
	NOTIFICATION_SET(SPD_CANCEL, "cancel");
	NOTIFICATION_SET(SPD_PAUSE, "pause");
	NOTIFICATION_SET(SPD_RESUME, "resume");
	NOTIFICATION_SET(SPD_ALL, "all");

	pthread_mutex_unlock(&connection->ssip_mutex);

	return 0;
}

#undef NOTIFICATION_SET

/* spd_list_modules retrieves information about the available output modules.
   The return value is a null-terminated array of strings containing output module
   names.
*/

char **spd_list_modules(SPDConnection * connection)
{
	char **available_modules;
	available_modules =
	    spd_execute_command_with_list_reply(connection,
						"LIST OUTPUT_MODULES");
	return available_modules;
}

void free_spd_modules(char **modules)
{
	int i = 0;
	while (modules != NULL && modules[i] != NULL) {
		free(modules[i]);
		++i;
	}
	free(modules);
}

char **spd_list_voices(SPDConnection * connection)
{
	char **voices;
	voices = spd_execute_command_with_list_reply(connection, "LIST VOICES");
	return voices;
}

SPDVoice **spd_list_synthesis_voices(SPDConnection * connection)
{
	char **svoices_str;
	SPDVoice **svoices;
	int i, num_items;
	svoices_str =
	    spd_execute_command_with_list_reply(connection,
						"LIST SYNTHESIS_VOICES");

	if (svoices_str == NULL)
		return NULL;

	for (i = 0;; i++)
		if (svoices_str[i] == NULL)
			break;
	num_items = i;
	svoices = (SPDVoice **) malloc((num_items + 1) * sizeof(SPDVoice *));

	for (i = 0; i <= num_items; i++) {
		const char delimiters[] = "\t";
		char *running;

		if (svoices_str[i] == NULL)
			break;
		running = svoices_str[i];

		svoices[i] = (SPDVoice *) malloc(sizeof(SPDVoice));
		svoices[i]->name = strsep(&running, delimiters);
		svoices[i]->language = strsep(&running, delimiters);
		svoices[i]->variant = strsep(&running, delimiters);
		assert(svoices[i]->name != NULL);
	}
	free(svoices_str);

	svoices[num_items] = NULL;

	return svoices;
}

void free_spd_voices(SPDVoice ** voices)
{
	int i = 0;
	while (voices != NULL && voices[i] != NULL) {
		free(voices[i]->name);
		free(voices[i]);
		++i;
	}
	free(voices);
}

char **spd_execute_command_with_list_reply(SPDConnection * connection,
					   char *command)
{
	char *reply = NULL;
	char *line;
	int err;
	int max_items = 50;
	char **result;
	int i;

	spd_execute_command_with_reply(connection, command, &reply);
	if (!ret_ok(reply)) {
		if (reply != NULL)
			free(reply);
		return NULL;
	}

	result = malloc((max_items + 1) * sizeof(char *));

	for (i = 0;; i++) {
		line = get_param_str(reply, i + 1, &err);
		if ((err) || (line == NULL))
			break;
		result[i] = line;
		if (i >= max_items - 2) {
			max_items *= 2;
			result = realloc(result, max_items * sizeof(char *));
		}
	}

	result[i] = NULL;

	free(reply);
	return result;
}

//int
//spd_get_client_list(SPDConnection *connection, char **client_names, int *client_ids, int* active){
//        SPD_DBG("spd_get_client_list: History is not yet implemented.");
//        return -1;
//
//}

int
spd_get_message_list_fd(SPDConnection * connection, int target, int *msg_ids,
			char **client_names)
{
	SPD_DBG("spd_get_client_list: History is not yet implemented.");
	return -1;
#if 0
	sprintf(command, "HISTORY GET MESSAGE_LIST %d 0 20\r\n", target);
	reply = spd_send_data(fd, command, 1);

	/*      header_ok = parse_response_header(reply);
	   if(header_ok != 1){
	   free(reply);
	   return -1;
	   } */

	for (count = 0;; count++) {
		record = (char *)parse_response_data(reply, count + 1);
		if (record == NULL)
			break;
		record_int = get_rec_int(record, 0);
		msg_ids[count] = record_int;
		record_str = (char *)get_rec_str(record, 1);
		assert(record_str != NULL);
		client_names[count] = record_str;
	}
	return count;
#endif
}

int spd_execute_command(SPDConnection * connection, char *command)
{
	char *reply;
	int ret;

	pthread_mutex_lock(&connection->ssip_mutex);

	ret = spd_execute_command_with_reply(connection, command, &reply);
	if (ret) {
		SPD_DBG("Can't execute command in spd_execute_command");
	}
	free(reply);

	pthread_mutex_unlock(&connection->ssip_mutex);

	return ret;
}

int spd_execute_command_wo_mutex(SPDConnection * connection, char *command)
{
	char *reply;
	int ret;

	SPD_DBG("Executing command wo_mutex");
	ret = spd_execute_command_with_reply(connection, command, &reply);
	if (ret)
		SPD_DBG
		    ("Can't execute command in spd_execute_command_wo_mutex");

	free(reply);

	return ret;
}

int
spd_execute_command_with_reply(SPDConnection * connection, char *command,
			       char **reply)
{
	char *buf;
	int r;
	SPD_DBG("Inside execute_command_with_reply");

	buf = g_strdup_printf("%s\r\n", command);
	*reply = spd_send_data_wo_mutex(connection, buf, SPD_WAIT_REPLY);
	free(buf);
	buf = NULL;
	if (*reply == NULL) {
		SPD_DBG
		    ("Can't send data wo mutex in spd_execute_command_with_reply");
		return -1;
	}

	r = ret_ok(*reply);

	if (!r)
		return -1;
	else
		return 0;
}

char *spd_send_data(SPDConnection * connection, const char *message, int wfr)
{
	char *reply;
	pthread_mutex_lock(&connection->ssip_mutex);

	if (connection->stream == NULL)
		RET(NULL);

	reply = spd_send_data_wo_mutex(connection, message, wfr);
	if (reply == NULL) {
		SPD_DBG("Can't send data wo mutex in spd_send_data");
		RET(NULL);
	}

	pthread_mutex_unlock(&connection->ssip_mutex);
	return reply;
}

char *spd_send_data_wo_mutex(SPDConnection * connection, const char *message,
			     int wfr)
{

	char *reply;
	int bytes;

	SPD_DBG("Inside spd_send_data_wo_mutex");

	if (connection->stream == NULL)
		return NULL;

	if (connection->mode == SPD_MODE_THREADED) {
		/* Make sure we don't get the cond_reply_ready signal before we are in
		   cond_wait() */
		pthread_mutex_lock(&connection->td->mutex_reply_ready);
	}
	/* write message to the socket */
	SPD_DBG("Writing to socket");
	if (!write(connection->socket, message, strlen(message))) {
		SPD_DBG("Can't write to socket: %s", strerror(errno));
		if (connection->mode == SPD_MODE_THREADED)
			pthread_mutex_unlock(&connection->td->mutex_reply_ready);
		return NULL;
	}
	SPD_DBG("Written to socket");
	SPD_DBG(">> : |%s|", message);

	/* read reply to the buffer */
	if (wfr) {
		if (connection->mode == SPD_MODE_THREADED) {
			/* Wait until the reply is ready */
			SPD_DBG
			    ("Waiting for cond_reply_ready in spd_send_data_wo_mutex");
			pthread_cond_wait(&connection->td->cond_reply_ready,
					  &connection->td->mutex_reply_ready);
			SPD_DBG("Condition for cond_reply_ready satisfied");
			pthread_mutex_unlock(&connection->td->mutex_reply_ready);
			SPD_DBG
			    ("Reading the reply in spd_send_data_wo_mutex threaded mode");
			/* Read the reply */
			if (connection->reply != NULL) {
				reply = connection->reply;
				connection->reply = NULL;
			} else {
				SPD_DBG
				    ("Error: Can't read reply, broken socket in spd_send_data.");
				return NULL;
			}
			bytes = strlen(reply);
			if (bytes == 0) {
				free(reply);
				SPD_DBG("Error: Empty reply, broken socket.");
				return NULL;
			}
			/* Signal the reply has been read */
			pthread_mutex_lock(&connection->td->mutex_reply_ack);
			pthread_cond_signal(&connection->td->cond_reply_ack);
			pthread_mutex_unlock(&connection->td->mutex_reply_ack);
		} else {
			reply = get_reply(connection);
		}
		if (reply != NULL)
			SPD_DBG("<< : |%s|\n", reply);
	} else {
		if (connection->mode == SPD_MODE_THREADED)
			pthread_mutex_unlock(&connection->td->mutex_reply_ready);
		SPD_DBG("<< : no reply expected");
		return strdup("NO REPLY");
	}

	if (reply == NULL)
		SPD_DBG
		    ("Reply from get_reply is NULL in spd_send_data_wo_mutex");

	SPD_DBG("Returning from spd_send_data_wo_mutex");
	return reply;
}

/* --------------------- Internal functions ------------------------- */

static int spd_set_priority(SPDConnection * connection, SPDPriority priority)
{
	static char p_name[16];
	static char command[64];

	switch (priority) {
	case SPD_IMPORTANT:
		strcpy(p_name, "IMPORTANT");
		break;
	case SPD_MESSAGE:
		strcpy(p_name, "MESSAGE");
		break;
	case SPD_TEXT:
		strcpy(p_name, "TEXT");
		break;
	case SPD_NOTIFICATION:
		strcpy(p_name, "NOTIFICATION");
		break;
	case SPD_PROGRESS:
		strcpy(p_name, "PROGRESS");
		break;
	default:
		SPD_DBG("Error: Can't set priority! Incorrect value.");
		return -1;
	}

	sprintf(command, "SET SELF PRIORITY %s", p_name);
	return spd_execute_command_wo_mutex(connection, command);
}

static char *get_reply(SPDConnection * connection)
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
	do {
		bytes = getline(&line, &N, connection->stream);
		if (bytes == -1) {
			SPD_DBG
			    ("Error: Can't read reply, broken socket in get_reply!");
			if (connection->stream != NULL)
				fclose(connection->stream);
			connection->stream = NULL;
			errors = TRUE;
		} else {
			g_string_append(str, line);
		}
		/* terminate if we reached the last line (without '-' after numcode) */
	} while (!errors && !((strlen(line) < 4) || (line[3] == ' ')));

	free(line);		/* getline allocates with malloc. */

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

static void *spd_events_handler(void *conn)
{
	char *reply;
	int reply_code;
	SPDConnection *connection = conn;

	while (1) {

		/* Read the reply/event (block if none is available) */
		SPD_DBG("Getting reply in spd_events_handler");
		reply = get_reply(connection);
		if (reply == NULL) {
			SPD_DBG("ERROR: BROKEN SOCKET");
			reply_code = -1;
		} else {
			SPD_DBG("<< : |%s|\n", reply);
			reply_code = get_err_code(reply);
		}

		if ((reply_code >= 700) && (reply_code < 800)) {
			int msg_id;
			int client_id;
			int err;

			SPD_DBG("Callback detected: %s", reply);

			/* This is an index mark */
			/* Extract message id */
			msg_id = get_param_int(reply, 1, &err);
			if (err < 0) {
				SPD_DBG
				    ("Bad reply from Speech Dispatcher: %s (code %d)",
				     reply, err);
				free(reply);
				break;
			}
			client_id = get_param_int(reply, 2, &err);
			if (err < 0) {
				SPD_DBG
				    ("Bad reply from Speech Dispatcher: %s (code %d)",
				     reply, err);
				free(reply);
				break;
			}
			/*  Decide if we want to call a callback */
			if ((reply_code == 701) && (connection->callback_begin))
				connection->callback_begin(msg_id, client_id,
							   SPD_EVENT_BEGIN);
			if ((reply_code == 702) && (connection->callback_end))
				connection->callback_end(msg_id, client_id,
							 SPD_EVENT_END);
			if ((reply_code == 703)
			    && (connection->callback_cancel))
				connection->callback_cancel(msg_id, client_id,
							    SPD_EVENT_CANCEL);
			if ((reply_code == 704) && (connection->callback_pause))
				connection->callback_pause(msg_id, client_id,
							   SPD_EVENT_PAUSE);
			if ((reply_code == 705)
			    && (connection->callback_resume))
				connection->callback_resume(msg_id, client_id,
							    SPD_EVENT_RESUME);
			if ((reply_code == 700) && (connection->callback_im)) {
				char *im;
				int err;
				im = get_param_str(reply, 3, &err);
				if ((err < 0) || (im == NULL)) {
					SPD_DBG
					    ("Broken reply from Speech Dispatcher: %s",
					     reply);
					free(reply);
					break;
				}
				/* Call the callback */
				connection->callback_im(msg_id, client_id,
							SPD_EVENT_INDEX_MARK,
							im);
				free(im);
			}
			free(reply);

		} else {
			/* This is a protocol reply */
			pthread_mutex_lock(&connection->td->mutex_reply_ready);
			/* Prepare the reply to the reply buffer in connection */
			if (reply != NULL) {
				connection->reply = reply;
			} else {
				SPD_DBG("Connection reply is NULL");
				connection->reply = NULL;
				pthread_mutex_unlock(&connection->td->mutex_reply_ready);
				break;
			}
			/* Signal the reply is available on the condition variable */
			/* this order is correct and necessary */
			pthread_cond_signal(&connection->td->cond_reply_ready);
			pthread_mutex_lock(&connection->td->mutex_reply_ack);
			pthread_mutex_unlock(&connection->td->mutex_reply_ready);
			/* Wait until it has bean read */
			pthread_cond_wait(&connection->td->cond_reply_ack,
					  &connection->td->mutex_reply_ack);
			pthread_mutex_unlock(&connection->td->mutex_reply_ack);
			/* Continue */
		}
	}
	/* In case of broken socket, we must still signal reply ready */
	if (connection->reply == NULL) {
		SPD_DBG("Signalling reply ready after communication failure");
		if (connection->stream != NULL)
			fclose(connection->stream);
		connection->stream = NULL;
		pthread_cond_signal(&connection->td->cond_reply_ready);
		pthread_exit(0);
	}
	return 0;		/* to please gcc */
}

static int ret_ok(char *reply)
{
	int err;

	if (reply == NULL)
		return -1;

	err = get_err_code(reply);

	if ((err >= 100) && (err < 300))
		return 1;
	if (err >= 300)
		return 0;

	SPD_FATAL("Internal error during communication.");
}

static char *get_param_str(char *reply, int num, int *err)
{
	int i;
	char *tptr;
	char *pos;
	char *pos_begin;
	char *pos_end;
	char *rep;

	assert(err != NULL);

	if (num < 1) {
		*err = -1;
		return NULL;
	}

	pos = reply;
	for (i = 0; i <= num - 2; i++) {
		pos = strstr(pos, "\r\n");
		if (pos == NULL) {
			*err = -2;
			return NULL;
		}
		pos += 2;
	}

	if (strlen(pos) < 4)
		return NULL;

	*err = strtol(pos, &tptr, 10);
	if (*err >= 300 && *err <= 399)
		return NULL;

	if ((*tptr != '-') || (tptr != pos + 3)) {
		*err = -3;
		return NULL;
	}

	pos_begin = pos + 4;
	pos_end = strstr(pos_begin, "\r\n");
	if (pos_end == NULL) {
		*err = -4;
		return NULL;
	}

	rep = (char *)strndup(pos_begin, pos_end - pos_begin);
	*err = 0;

	return rep;
}

static int get_param_int(char *reply, int num, int *err)
{
	char *rep_str;
	char *tptr;
	int ret;

	rep_str = get_param_str(reply, num, err);
	if (rep_str == NULL) {
		/* err is already set to the error return code, just return */
		return 0;
	}

	ret = strtol(rep_str, &tptr, 10);
	if (*tptr != '\0') {
		/* this is not a number */
		*err = -3;
		free(rep_str);
		return 0;
	}
	free(rep_str);

	return ret;
}

static int get_err_code(char *reply)
{
	char err_code[4];
	int err;

	if (reply == NULL)
		return -1;
	SPD_DBG("spd_send_data:	reply: %s\n", reply);

	err_code[0] = reply[0];
	err_code[1] = reply[1];
	err_code[2] = reply[2];
	err_code[3] = '\0';

	SPD_DBG("ret_ok: err_code:	|%s|\n", err_code);

	if (isanum(err_code)) {
		err = atoi(err_code);
	} else {
		SPD_DBG("ret_ok: not a number\n");
		return -1;
	}

	return err;
}

/* isanum() tests if the given string is a number,
 *  returns 1 if yes, 0 otherwise. */
static int isanum(char *str)
{
	int i;
	if (str == NULL)
		return 0;
	for (i = 0; i <= strlen(str) - 1; i++) {
		if (!isdigit(str[i]))
			return 0;
	}
	return 1;
}

/*
 * escape_dot: Replace . with .. at the start of lines.
 * @text: text to escape
 * @Returns: An allocated string, containing the escaped text.
 */
static char *escape_dot(const char *text)
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

	*result_ptr = '\0';
	return result;
}

#ifdef LIBSPEECHD_DEBUG
static void SPD_DBG(char *format, ...)
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
#else /* LIBSPEECHD_DEBUG */
static void SPD_DBG(char *format, ...)
{
}
#endif /* LIBSPEECHD_DEBUG */
