/*
 * common.c - Common utilities
 * Copyright (C) 2003,2006, 2007 Brailcom, o.p.s.
 *
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1, or (at your option) any later
 * version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include <signal.h>
#include <pthread.h>

#include "common.h"

/* This is the same as pthread_create, but blocks all signals in the created
 * thread. */
int spd_pthread_create(pthread_t *thread, const pthread_attr_t *attr,
                          void *(*start_routine) (void *), void *arg)
{
	int retsig, ret;
	sigset_t all_signals;
	sigset_t old_signals;

	retsig = sigfillset(&all_signals);
	if (retsig != 0)
		MSG(1, "Can't fill signal set (%d), expect problems when terminating!\n", retsig);
	else {
		retsig = pthread_sigmask(SIG_BLOCK, &all_signals, &old_signals);
		if (retsig != 0)
			MSG(1, "Can't set signal set (%d), expect problems when terminating!\n", retsig);
	}

	ret = pthread_create(thread, attr, start_routine, arg);

	if (retsig == 0)
		pthread_sigmask(SIG_SETMASK, &old_signals, NULL);

	return ret;
}

void set_speaking_thread_parameters(void)
{
	pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
	pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
}
