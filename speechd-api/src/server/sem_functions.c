
/*
 * sem_functions.c - Functions for manipulating System V / IPC semaphores
 *
 * Copyright (C) 2001, 2002, 2003 Brailcom, o.p.s.
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
 * $Id: sem_functions.c,v 1.9 2006-07-11 16:12:27 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <safe_io.h>

#include "speechd.h"
#include "sem_functions.h"

void speaking_semaphore_post(void)
{
	char buf[1];
	buf[0] = 42;
	const ssize_t wr_bytes = safe_write(speaking_pipe[1], buf, 1);
	if (wr_bytes != 1)
		FATAL("write to polled fd: could not write 1 byte");
}
