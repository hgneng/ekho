/*
 * module_readline.c - Input buffering
 *
 * Copyright (C) 2020 Samuel Thibault <samuel.thibault@ens-lyon.org>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY Samuel Thibault AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/select.h>

#include "module_main.h"

/*
 * This provides simple input buffering for modules.
 */

#define INIT_DATA_ALLOCATED 2

/* Already-received data */
static char *data = NULL;

/* Size of data */
static size_t data_allocated = 0;
/* Index of first pending character in data */
static size_t data_ptr = 0;
/* Number of pending characters in data */
static size_t data_used = 0;
/* Index until where we know that there is no \n in the pending characters */
static size_t data_no_lf = 0;

char *module_readline(int fd, int block)
{
	char *str;
	fd_set set;
	int ret;
	struct timeval zero_tv = { .tv_sec = 0, .tv_usec = 0 };

	while (1) {
		if (data_used) {
			/* Look for \n */
			for (;
			     data_no_lf < data_ptr + data_used;
			     data_no_lf++) {
				if (data[data_no_lf] == '\n') {
					/* We have a line! */
					size_t len;

					data_no_lf++;

					len = data_no_lf - data_ptr;
					str = strndup(data + data_ptr, len);
					data_used -= len;
					if (!data_used)
						/* Emptied the buffer, just start over */
						data_ptr = 0;
					else
						data_ptr += len;
					return str;
				}
			}
		}

		/* No \n, we should try to read more */
		FD_ZERO(&set);
		FD_SET(fd, &set);
		ret = select(fd + 1, &set, NULL, NULL, block ? NULL : &zero_tv);

		if (ret == -1) {
			if (errno == EINTR
			    || errno == EAGAIN
			    || errno == EINPROGRESS) {
				/* Temporary hickup, come back later */
				if (!block)
					return NULL;
				continue;
			}

			perror("select on stdin");
			return NULL;
		}
		if (!FD_ISSET(fd, &set)) {
			/* Nothing incoming */
			if (!block)
				return NULL;
			continue;
		}

		/* We have data to read, make sure we have room */
		if (data_ptr + data_used == data_allocated) {
			/* No room at the end */

			if (data_ptr) {
				/* But room at the beginning, copy data over */
				memmove(data, data + data_ptr, data_used);
				data_no_lf -= data_ptr;
				data_ptr = 0;
			} else {
				/* No room at all, reallocate */
				size_t new_allocated;
				char *new_data;

				if (!data_allocated) {
					new_allocated = INIT_DATA_ALLOCATED;
				} else {
					new_allocated = data_allocated*2;
					if (new_allocated < data_allocated) {
						fprintf(stderr, "input line overflow\n");
						return NULL;
					}
				}

				new_data = realloc(data, new_allocated);
				if (!new_data) {
					/* No room, cannot do much but wait */
					if (!block)
						return NULL;
					continue;
				}

				data = new_data;
				data_allocated = new_allocated;
			}
		}

		/* Actually read */
		ret = read(fd, data + data_ptr + data_used,
				data_allocated - data_ptr - data_used);

		if (ret == -1) {
			if (errno == EINTR
			    || errno == EAGAIN
			    || errno == EINPROGRESS) {
				/* Temporary hickup, come back later */
				if (!block)
					return NULL;
				continue;
			}

			perror("read on stdin");
			return NULL;
		}

		if (ret == 0) {
			fprintf(stderr, "stdin over\n");
			return NULL;
		}

		/* Some more data */
		data_used += ret;
		data_no_lf = 0;
	}
}
