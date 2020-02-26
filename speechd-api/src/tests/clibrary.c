
/*
 * clibrary.c - Testing the C library for Speech Dispatcher
 *
 * Copyright (C) 2003 Brailcom, o.p.s.
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
 * $Id: clibrary.c,v 1.6 2006-07-11 16:12:28 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "speechd_types.h"
#include "libspeechd.h"

int main()
{
	SPDConnection *conn;
	int i, ret;

	printf("Start of the test of the test.\n");

	printf("Trying to initialize Speech Deamon...");
	conn = spd_open("say", NULL, NULL, SPD_MODE_SINGLE);
	if (conn == 0) {
		printf("Speech Deamon failed");
		exit(1);
	}
	printf("OK\n");

	printf("Say inviting message\n");
	ret =
	    spd_say(conn, SPD_MESSAGE, "Hello, how are you? I'm Speech Deamon");
	if (ret == -1) {
		printf("spd_say failed");
		exit(1);
	}

	sleep(4);

	printf("\n");
	printf("Try to lower pitch and higher rate...\n");
	ret = spd_set_voice_pitch(conn, -20);
	if (ret == -1) {
		printf("spd_set_voice_pitch failed");
		exit(1);
	}

	ret = spd_set_voice_rate(conn, +20);
	if (ret == -1) {
		printf("spd_set_voice_rate failed");
		exit(1);
	}

	printf("...and say something\n");
	ret = spd_say(conn, SPD_MESSAGE, "Do you like this voice more?");
	if (ret == -1) {
		printf("spd_say failed");
		exit(1);
	}

	sleep(4);

	printf("\n");
	printf("Try to lower pitch and raise pitch range and rate...\n");
	ret = spd_set_voice_pitch(conn, -20);
	if (ret == -1) {
		printf("spd_set_voice_pitch failed");
		exit(1);
	}

	ret = spd_set_voice_pitch_range(conn, +20);
	if (ret == -1) {
		printf("spd_set_voice_pitch_range failed");
		exit(1);
	}

	ret = spd_set_voice_rate(conn, +20);
	if (ret == -1) {
		printf("spd_set_voice_rate failed");
		exit(1);
	}

	printf("...and say something\n");
	ret = spd_say(conn, SPD_MESSAGE, "Do you like this voice more?");
	if (ret == -1) {
		printf("spd_say failed");
		exit(1);
	}

	sleep(4);

	printf("Switch punctuation mode to `all'.");
	printf("\n");

	printf("\n");
	printf("Keep this pitch, set higher rate and let's test priorities.\n"
	       "Priority progress should choose some messages\n"
	       "to report the progrees and it should allways say\n"
	       "the last message.\n");

	sleep(10);

	ret = spd_set_voice_rate(conn, +90);
	if (ret == -1) {
		printf("spd_set_voice_rate failed");
		exit(1);
	}

	for (i = 0; i <= 100; i++) {
		printf("%d%% completed\n", i);
		ret = spd_sayf(conn, SPD_PROGRESS, "%d%% completed", i);
		if (ret == -1) {
			printf("spd_sayf failed");
			exit(1);
		}
	}

	printf("Trying to close Speech Dispatcher connection...");
	spd_close(conn);
	printf("OK\n");

	printf("End of the test.\n");
	exit(0);
}
