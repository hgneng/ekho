/* common.c -- Common parts of the client and the server
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

#include <limits.h>
#include <sys/socket.h>
#include <unistd.h>

#include "spdsend.h"

const long CONNECTION_ID_MIN = 1;
const long CONNECTION_ID_MAX = 1000;

const int EXIT_OK = 0;
const int EXIT_ERROR = 1;

extern Success write_data(Stream s, const void *buffer, size_t size)
{
	int written;

	for (; size > 0; size -= written, buffer += written) {
		written = write(s, buffer, size);
		if (written < 0)
			return ERROR;
	}

	return OK;
}

extern int read_data(Stream s, void *buffer, size_t max_size)
{
	size_t nread = 0;
	ssize_t n;

	while (nread < max_size) {
		n = read(s, buffer, max_size);
		if (n < 0)
			return NONE;
		if (n == 0)
			break;
		nread += n;
		buffer += n;
		max_size -= n;
	}

	return nread;
}

extern Success forward_data(Stream from, Stream to, bool closep)
{
	const size_t buffer_size = 4096;
	char buffer[buffer_size];
	ssize_t n;

	while ((n = read(from, buffer, buffer_size)) > 0) {
		if (write_data(to, buffer, n) == ERROR)
			return ERROR;
	}
	if (closep)
		shutdown(to, SHUT_WR);

	return (n == NONE ? ERROR : OK);
}
