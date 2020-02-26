/* client.c -- Client part of spdsend
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
   along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "spdsend.h"

#include <stddef.h>
#include <stdio.h>
#include <string.h>

static Success send_header(Stream s, Action action, Connection_Id id)
{
	if (write_data(s, &action, sizeof(Action)) == OK &&
	    write_data(s, &id, sizeof(Connection_Id)) == OK)
		return OK;
	else
		return ERROR;
}

#define SEND_HEADER(action)                      \
	if (send_header (s, action, id) == ERROR) \
		return ERROR

static Success send_open_header(Stream s, const char *host, int port)
{
	int hostlen = strlen(host);
	Action action = A_OPEN;

	if (write_data(s, &action, sizeof(Action)) == OK &&
	    write_data(s, &port, sizeof(int)) == OK &&
	    write_data(s, &hostlen, sizeof(int)) == OK &&
	    write_data(s, host, hostlen) == OK)
		return OK;
	else
		return ERROR;
}

static Connection_Id read_reply(Stream s)
{
	Result result;
	Connection_Id id;

	if (read_data(s, &result, sizeof(Result)) != sizeof(Result))
		return NONE;
	if (result != OK_CODE)
		return NONE;

	if (read_data(s, &id, sizeof(Connection_Id)) != sizeof(Connection_Id))
		return NONE;

	return id;
}

/* External functions */

extern Success open_connection(Stream s, const char *host, int port)
{
	if (send_open_header(s, host, port) == ERROR)
		return ERROR;
	{
		Connection_Id id = read_reply(s);
		if (id == NONE)
			return ERROR;
		printf("%d\n", id);
	}
	return OK;
}

extern Success close_connection(Stream s, Connection_Id id)
{
	SEND_HEADER(A_CLOSE);
	return (read_reply(s) == OK ? OK : ERROR);
}

extern Success send_command(Stream s, Connection_Id id)
{
	SEND_HEADER(A_DATA);
	if (read_reply(s) == NONE)
		return ERROR;
	if (forward_data(0, s, TRUE) == OK && forward_data(s, 1, FALSE) == OK)
		return OK;
	else
		return ERROR;
}
