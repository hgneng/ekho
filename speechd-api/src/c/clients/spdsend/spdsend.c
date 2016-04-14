/* spdsend.c -- Send SSIP commands to Speech Dispatcher
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


#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "spdsend.h"


const char *const VERSION = "0.0.0";

#if !(defined(__GLIBC__) && defined(_GNU_SOURCE))
/* Added by Willie Walker - getline is a gcc-ism */
#define BUFFER_LEN 256
ssize_t getline (char **lineptr, size_t *n, FILE *f)
{
        char ch;
        size_t m = 0;
        ssize_t buf_len = 0;
        char * buf = NULL;
        char * p = NULL;

	if (errno != 0) {
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
                return -1;
        } else {
                *p = '\0';
                *lineptr = buf;
                *n = m;
                return m;
        }
}
#endif /* !(defined(__GLIBC__) && defined(_GNU_SOURCE)) */

static void usage (const char *const message)
{
  if (message != NULL)
    fprintf (stderr, "spdsend: %s\n", message);
  fprintf (stderr,
           "usage: spdsend { --open SERVER PORT | --close ID | --send ID }\n");
  exit (EXIT_ERROR);
}


static long string_to_number (const char *string,
                              long low_limit, long high_limit)
{
  char *tailptr;
  errno = 0;
  long int number = strtol (string, &tailptr, 0);
  if (errno || *tailptr || number < low_limit || number > high_limit)
    usage ("Invalid parameter");
  return number;
}


int main (int argc, char **argv)
{
  if (argc < 2)
    usage ("Invalid number of arguments");

  {    
    const char *const action = argv[1];    
    Success (*function) (Stream, Connection_Id);
    Connection_Id conn_id;
    char *host;
    int port;

    if (! strcmp (action, "--version"))
      {
        printf ("spdsend %s\n", VERSION);
        exit (EXIT_OK);
      }
    
    if (! strcmp (action, "--open"))
      {
        if (argc != 4)
          usage ("Invalid number of arguments");
        host = argv[2];
        port = string_to_number (argv[3], 0, 65535);
      }
    else
      {
        if (argc != 3)
          usage ("Invalid number of arguments");
        conn_id = string_to_number (argv[2], CONNECTION_ID_MIN,
                                    CONNECTION_ID_MAX);
        if (! strcmp (action, "--close"))
          function = close_connection;
        else if (! strcmp (action, "--send"))
          function = send_command;
        else
          usage ("Invalid option");
      }
    
    {
      Stream server = open_server ();
      if (server == NONE)
        return EXIT_ERROR;
      
      {
        int result = (! strcmp (action, "--open")
                      ? open_connection (server, host, port)
                      : function (server, conn_id));
        return (result == OK ? EXIT_OK : EXIT_ERROR);
      }
    }
  }
}
