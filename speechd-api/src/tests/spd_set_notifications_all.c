/*
* spd_set_notifications_all.c - test SPD_ALL behaviour
*
 * Copyright (C) 2010 Brailcom, o.p.s.
*
* This program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 2, or (at your option)
* any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
* See the GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program.  If not, see <https://www.gnu.org/licenses/>.
*
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "speechd_types.h"
#include "libspeechd.h"

#define TEST_NAME __FILE__
#define TEST_WAIT_COUNT (100)
static int notification_mask;
static SPDConnection *spd;

static const char *events[] = {
	"SPD_EVENT_BEGIN",
	"SPD_EVENT_END",
	"SPD_EVENT_INDEX_MARK",
	"SPD_EVENT_CANCEL",
	"SPD_EVENT_PAUSE",
	"SPD_EVENT_RESUME"
};

/* Callback for Speech Dispatcher notifications */
void notification_cb(size_t msg_id, size_t client_id, SPDNotificationType type)
{
	notification_mask |= (1 << type);
	printf("notification %s received\n", events[type]);
}

void index_mark_cb(size_t msg_id, size_t client_id, SPDNotificationType type,
		   char *index_mark)
{
	notification_mask |= (1 << type);
	printf("notification %s received\n", events[type]);
}

int main(int argc, char *argv[])
{
	int result, count;

	/* Open Speech Dispatcher connection in THREADED mode. */
	spd = spd_open(TEST_NAME, __FUNCTION__, NULL, SPD_MODE_THREADED);
	if (!spd) {
		printf("Speech-dispatcher: Failed to open connection. \n");
		exit(1);
	}

	printf("Speech-dispatcher: connection opened. \n");

	spd->callback_begin = notification_cb;
	spd->callback_end = notification_cb;
	spd->callback_cancel = notification_cb;
	spd->callback_pause = notification_cb;
	spd->callback_resume = notification_cb;
	spd->callback_im = index_mark_cb;

	/* Ask Speech Dispatcher to notify us about these events. */
	result = spd_set_notification_on(spd, SPD_ALL);
	if (result == -1) {
		printf("Notification SPD_ALL not set correctly \n");
		exit(1);
	}

	/* The message is spoken as SSML */
	result = spd_set_data_mode(spd, SPD_DATA_SSML);
	if (result == -1) {
		printf("Could not set spd_set_data_mode() to SPD_DATA_SSML \n");
		exit(1);
	}

	result = spd_say(spd, SPD_MESSAGE, "<speak> \
                                        S S M L or Speech <mark name=\"mark1\"/> Synthesis Markup Language is an XML \
                                        language. \
                                        </speak>");
	if (result == -1) {
		printf("spd_say() failed. \n");
		exit(1);
	}

	count = 0;
	while (~notification_mask & SPD_BEGIN) {
		sleep(1);
		if (count++ == TEST_WAIT_COUNT) {
			printf("SPD_BEGIN wait count exceeded \n");
			exit(1);
		}
	}

	result = spd_pause(spd);
	if (result == -1) {
		printf("spd_pause() failed. \n");
		exit(1);
	}

	count = 0;
	while (~notification_mask & SPD_PAUSE) {
		sleep(1);
		if (count++ == TEST_WAIT_COUNT) {
			printf("SPD_PAUSE wait count exceeded \n");
			exit(1);
		}
	}

	result = spd_say(spd, SPD_MESSAGE, "<speak> \
                                        S S M L or Speech <mark name=\"mark1\"/> Synthesis Markup Language is an XML \
                                        language. \
                                        </speak>");
	if (result == -1) {
		printf("spd_say() failed. \n");
		exit(1);
	}

	result = spd_resume(spd);
	if (result == -1) {
		printf("spd_resume() failed. \n");
		exit(1);
	}

	count = 0;
	while (~notification_mask & SPD_RESUME) {
		sleep(1);
		if (count++ == TEST_WAIT_COUNT) {
			printf("SPD_PAUSE wait count exceeded \n");
			exit(1);
		}
	}

	result = spd_say(spd, SPD_MESSAGE, "<speak> \
                                        S S M L or Speech <mark name=\"mark1\"/> Synthesis Markup Language is an XML \
                                        language. \
                                        </speak>");
	if (result == -1) {
		printf("spd_say() failed. \n");
		exit(1);
	}

	count = 0;
	while (~notification_mask & SPD_INDEX_MARKS) {
		sleep(1);
		if (count++ == TEST_WAIT_COUNT) {
			printf("SPD_INDEX_MARKS wait count exceeded \n");
			exit(1);
		}
	}

	count = 0;
	while (~notification_mask & SPD_END) {
		sleep(1);
		if (count++ == TEST_WAIT_COUNT) {
			printf("SPD_END wait count exceeded \n");
			exit(1);
		}
	}

	result = spd_say(spd, SPD_MESSAGE, "<speak> \
                                        S S M L or Speech <mark name=\"mark1\"/> Synthesis Markup Language is an XML \
                                        language. \
                                        </speak>");
	if (result == -1) {
		printf("spd_say() failed. \n");
		exit(1);
	}

	result = spd_cancel(spd);
	if (result == -1) {
		printf("spd_cancel() failed. \n");
		exit(1);
	}

	count = 0;
	while (~notification_mask & SPD_CANCEL) {
		sleep(1);
		if (count++ == TEST_WAIT_COUNT) {
			printf("SPD_CANCEL wait count exceeded \n");
			exit(1);
		}
	}

	printf("All notifications received. \n");

	spd_close(spd);
	printf("Speech-dispatcher: connection closed. \n");

	exit(0);
}
