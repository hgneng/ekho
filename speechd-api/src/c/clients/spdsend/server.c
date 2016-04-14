/* server.c -- Server part of spdsend
   Author: Milan Zamazal <pdm@brailcom.org>
*/

/* Copyright (C) 2004 Brailcom, o.p.s.

   COPYRIGHT NOTICE

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 2, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA.
*/


#include "spdsend.h"

#ifndef USE_THREADS
#define USE_THREADS 1
#endif

#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#if USE_THREADS
#include <pthread.h>
#endif
#include <pwd.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <unistd.h>

#if !(defined(__GLIBC__) && defined(_GNU_SOURCE))
/* Added by Willie Walker - TEMP_FAILURE_RETRY, strndup, and getline
 * are gcc-isms
 */
ssize_t getline (char **lineptr, size_t *n, FILE *f);
#endif

/* Utilities */


static void system_error (const char *message)
{
  perror (message);
  exit (1);
}


/* Connection management */


Stream *connections;
#if USE_THREADS
pthread_mutex_t connections_mutex = PTHREAD_MUTEX_INITIALIZER;
#endif


static Stream get_connection (Connection_Id id)
{
  return connections[id];
}

static void set_connection (Connection_Id id, Stream s)
{
#if USE_THREADS
  pthread_mutex_lock (&connections_mutex);
#endif
  connections[id] = s;
#if USE_THREADS
  pthread_mutex_unlock (&connections_mutex);
#endif
}

static Connection_Id new_connection (Stream s)
{
#if USE_THREADS
  pthread_mutex_lock (&connections_mutex);
#endif
  int id;
  for (id = CONNECTION_ID_MIN;
       id < CONNECTION_ID_MAX && connections[id] != NONE;
       id++)
    ;
  if (id >= CONNECTION_ID_MAX)
    return NONE;
  connections[id] = s;
#if USE_THREADS
  pthread_mutex_unlock (&connections_mutex);
#endif
  return id;
}

static Connection_Id do_open_connection (const char *host, int port)
{
  int sock = socket (AF_INET, SOCK_STREAM, 0);
  
  {
    struct sockaddr_in name;
    name.sin_family = AF_INET;
    name.sin_port = htons (port);
    {
      struct hostent *hostinfo;
      hostinfo = gethostbyname (host);
      if (hostinfo == NULL)
        return NONE;
      name.sin_addr = *(struct in_addr *) hostinfo->h_addr;
    }
    if (connect (sock, (struct sockaddr *)&name, sizeof (name)) < 0)
      return NONE;
    {
      int arg = 1;
      setsockopt (sock, IPPROTO_TCP, TCP_NODELAY, &arg, sizeof(int));
    }
  }
  
  {
    Connection_Id id = new_connection (sock);
    if (id == NONE)
      close (sock);
    return id;
  }
}

static Success do_close_connection (Connection_Id id)
{
  Stream c = get_connection (id);
  if (c == NONE)
    return ERROR;
  close (c);
  set_connection (id, NONE);
  return OK;
}

static Success do_send_data (Connection_Id id, Stream from, Stream to,
                             Success (*forwarder) (Stream, Stream, bool))
{
  int sock = get_connection (id);
  if (sock == NONE)
    return ERROR;
  if (from == NONE)
    from = sock;
  else if (to == NONE)
    to = sock;
  {
    Success result = ((*forwarder) (from, to, FALSE));
    if (result != OK)
      do_close_connection (id);
    return result;
  }
}


/* Processing requests */

/* Protocol:

   Client request:
     First comes the action code, of the type Action.
     If Action is A_OPEN, the following data follows:
       int port, int strlen(hostname), hostname
     Else:
       Connection_Id
     Then, if Action is A_DATA, the SSIP lines follow.
     
   Server answer:
     The result code, of the type Result.
     If Result is OK, Connection_Id follows.
     Additionally, if Action is A_DATA, SSIP reply follows.
*/

static Success report (Stream s, Result code)
{
  return write_data (s, &code, sizeof (Result));
}

static Success report_ok (Stream s, Connection_Id id)
{
  if (report (s, OK_CODE) == OK &&
      write_data (s, &id, sizeof (Connection_Id)) == OK)
    return OK;
  else
    return ERROR;
}

static Success report_error (Stream s)
{
  return report (s, ER_CODE);
}


static Connection_Id read_id (Stream s)
{
  Connection_Id id;
  if (read_data (s, &id, sizeof (Connection_Id)) == ERROR)
    return NONE;
  return id;
}


static Success forward_ssip_answer (Stream from, Stream to, bool _closep)
{
  int result = OK;
  FILE *f = fdopen (from, "r");
  size_t line_size = 256;
  char *line = malloc (line_size);
  if (line == NULL)
    system_error ("memory allocation");

  while (1)
    {
      int n = getline (&line, &line_size, f);
      if (n < 0 || write_data (to, line, n) == ERROR)
        {
          result = ERROR;
          break;
        }
      if (n > 3 && line[3] == ' ')
        break;
    }

  free (line);
  return result;
}


static void process_open (Stream s)
{
  Connection_Id id;
  int port;
  int hostlen;
  
  if (read_data (s, &port, sizeof (int)) != sizeof (int))
    {
      report_error (s);
      return;
    }
  if (read_data (s, &hostlen, sizeof (int)) != sizeof (int))
    {
      report_error (s);
      return;
    }
  {
    char *host = malloc (hostlen+1);
    if (host == NULL)
      system_error ("memory allocation");
    if (read_data (s, host, hostlen) != hostlen)
      {
        free (host);
        report_error (s);
        return;
      }
    host[hostlen] = '\0';
    id = do_open_connection (host, port);
    free (host);
  }

  if (id == NONE)
    report_error (s);
  else
    report_ok (s, id);
}

static void process_close (Stream s)
{
  Connection_Id id = read_id (s);
  if (id != NONE && do_close_connection (id) == OK)
    report_ok (s, id);
  else
    report_error (s);
}

static void process_data (Stream s)
{
  Connection_Id id = read_id (s);
  if (id != NONE)
    report_ok (s, id);
  else
    report_error (s);

  do_send_data (id, s, NONE, forward_data) == OK &&
  do_send_data (id, NONE, s, forward_ssip_answer) == OK;
}


static void process_request (Stream s)
{
  Action action;

  if (read_data (s, &action, sizeof (Action)) == NONE)
    return;
  
  if (action == A_OPEN)
    process_open (s);
  else if (action == A_CLOSE)
    process_close (s);
  else if (action == A_DATA)
    process_data (s);
  else
    report_error (s);
  
  close (s);
}

#if USE_THREADS
static void *process_request_thread (void *s)
{
  Stream s_deref = *((Stream *) s);
  free (s);
  pthread_detach (pthread_self ());
  process_request (s_deref);
  return NULL;
}
#endif


/* Starting the server */


static const char *login_name ()
{
  return getpwuid (getuid ()) -> pw_name;
}

static const char *server_socket_name ()
{
  char *name;
  if (asprintf (&name, "/tmp/spdsend-server.%s", login_name ()) < 0)
    system_error ("memory allocation");
  return name;
}

static void serve ()
{
  struct sockaddr_un name;
  int sock;
  size_t size;
  const char *filename = server_socket_name ();
     
  sock = socket (PF_LOCAL, SOCK_STREAM, 0);
  if (sock < 0)
    system_error ("socket creation");
  
  name.sun_family = AF_LOCAL;
  strncpy (name.sun_path, filename, sizeof (name.sun_path));
  name.sun_path[sizeof (name.sun_path) - 1] = '\0';
  size = (offsetof (struct sockaddr_un, sun_path)
          + strlen (name.sun_path) + 1);
  if (bind (sock, (struct sockaddr *) &name, size) < 0)
    system_error ("bind");
  if (listen (sock, LISTEN_QUEUE_LENGTH) < 0)
    system_error ("listen");
  
  while (1)
    {
      struct sockaddr_un client_address;
      socklen_t client_address_len = sizeof (client_address);
      Stream *s = malloc (sizeof (Stream));
      if (s == NULL)
        system_error ("memory allocation");
      *s = accept (sock, (struct sockaddr *)&client_address,
                   &client_address_len);
      if (*s < 0)
        break;
      {
#if USE_THREADS
        pthread_t tid;
#endif
#if USE_THREADS
        pthread_create (&tid, NULL, &process_request_thread, s);
#else
        process_request (*s);
#endif
      }
    }
}

static void daemonize ()
{
  if (fork () != 0)
    exit (0);
  setsid ();
  signal (SIGHUP, SIG_IGN);
  if (fork () != 0)
    exit (0);
  chdir ("/");
  umask (0);
  {
    int i;
    for (i = 0; i < 4; i++)
      close (i);
  }
}

static void init_connections ()
{
  connections = malloc (CONNECTION_ID_MAX * sizeof (Connection_Id));
  if (connections == NULL)
    system_error ("memory allocation");
  {
    int i;
    for (i = CONNECTION_ID_MIN; i < CONNECTION_ID_MAX; i++)
      connections[i] = NONE;
  }
#if USE_THREADS
  pthread_mutex_init (&connections_mutex, NULL);
#endif
}

static void start_server ()
{
  const char *socket_name = server_socket_name ();
  unlink (socket_name);
  
  {
    int pid = fork ();
    if (pid == -1)
      system_error ("fork");

    if (pid == 0)
      {
        daemonize ();
        init_connections ();
        serve ();
        unlink (socket_name);
        exit (0);
      }
    else
      sleep (1);
  }
}

static int connect_server ()
{    
  struct sockaddr_un name;
  int sock = socket (AF_LOCAL, SOCK_STREAM, 0);
  int name_size;
  name.sun_family = AF_LOCAL;
  strncpy (name.sun_path, server_socket_name (), sizeof (name.sun_path));
  name.sun_path[sizeof (name.sun_path) - 1] = '\0';
  name_size = (offsetof (struct sockaddr_un, sun_path)
               + strlen (name.sun_path) + 1);
     
  if (connect (sock, (struct sockaddr *)&name, name_size) < 0)
    return NONE;
  else
    return sock;
}


/* External functions */


Stream open_server ()
{
  Stream s;

  s = connect_server ();
  if (s == NONE)
    {
      start_server ();
      s = connect_server ();
    }
  if (s == NONE)
    return NONE;

  return s;
}

