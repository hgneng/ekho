
/*
 * connection-recovery.c - Test of connection recovery
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
 * $Id: connection-recovery.c,v 1.1 2008-02-08 10:04:18 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <wchar.h>
#include <wctype.h>
#include "speechd_types.h"
#include <libspeechd.h>
#include <signal.h>
#include <unistd.h>

SPDConnection *try_to_reconnect(void)
{
	SPDConnection *conn;
	while (1) {
		printf("Trying to reconnect\n");
		usleep(1000);
		conn = spd_open("test", NULL, NULL, SPD_MODE_THREADED);
		if (conn != NULL) {
			spd_say(conn, SPD_MESSAGE, "Reconnect succesful");
			printf("Reconnect successful\n");
			return conn;
		}
	}
}

int main(void)
{
	SPDConnection *conn;
	int i = 0;
	int failures = 0;
	int ret;

	conn = spd_open("test", NULL, NULL, SPD_MODE_THREADED);
	if (conn == 0) {
		printf("Speech Deamon failed");
		exit(1);
	}

	printf("Connection recovery test \n\n");
	printf("This test will keep saying a message 'Testing connection' \n");
	printf("until Speech Dispatcher is stopped. Then it output at least\n");
	printf
	    ("5 messages about connection failure and will try to reconnect.\n");
	printf
	    ("On successful reconnect (after speechd) is started, it will keep\n");
	printf("saying 'Testing connection' again until terminated.\n");
	fflush(stdout);

	while (1) {
		sleep(5);
		printf("Speaking message %d ", i++);
		ret = spd_say(conn, SPD_MESSAGE, "Testing connection");
		printf("with result %d\n", ret);
		if (ret == -1)
			failures++;

		if (failures >= 5) {
			spd_close(conn);
			conn = try_to_reconnect();
			failures = 0;
		}
	}
}
