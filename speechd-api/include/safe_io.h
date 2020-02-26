/*
 * safe_io.h - Wrapper around read and write
 *
 * Copyright (C) 2001, 2002, 2003, 2007 Brailcom, o.p.s.
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
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <unistd.h>
#include <errno.h>

#ifdef TEMP_FAILURE_RETRY	/* GNU libc */
#define safe_read(fd, buf, count) TEMP_FAILURE_RETRY(read(fd, buf, count))
#define safe_write(fd, buf, count) TEMP_FAILURE_RETRY(write(fd, buf, count))
#else /* TEMP_FAILURE_RETRY */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
static inline ssize_t safe_read(int fd, void *buf, size_t count)
{
	do {
		ssize_t w = read(fd, buf, count);

		if (w == -1 && errno == EINTR)
			continue;
		return w;
	} while (1);
}

static inline ssize_t safe_write(int fd, const void *buf, size_t count)
{
	do {
		ssize_t w = write(fd, buf, count);

		if (w == -1 && errno == EINTR)
			continue;
		return w;
	} while (1);
}
#endif /* TEMP_FAILURE_RETRY */
