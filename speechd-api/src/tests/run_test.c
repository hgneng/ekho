
/*
 * run_test.c - Read a set of Speech Dispatcher commands and try them
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
 * $Id: run_test.c,v 1.14 2008-02-08 10:01:08 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <glib.h>

#define FATAL(msg) { printf(msg"\n"); exit(1); }

int sockk;

#ifndef HAVE_STRCASESTR
/* Added by Willie Walker - strcasestr is a common but non-standard extension
 */
char *strcasestr(const char *a, const char *b)
{
	size_t l;
	char f[3];

	snprintf(f, sizeof(f), "%c%c", tolower(*b), toupper(*b));
	for (l = strcspn(a, f); l != strlen(a); l += strcspn(a + l + 1, f) + 1)
		if (strncasecmp(a + l, b, strlen(b)) == 0)
			return (a + l);
	return NULL;
}
#endif /* HAVE_STRCASESTR */

char *send_data(int fd, char *message, int wfr)
{
	char *reply;
	int bytes;

	/* TODO: 1000?! */
	reply = (char *)malloc(sizeof(char) * 1000);

	/* write message to the socket */
	if (!write(fd, message, strlen(message))) {
		fprintf(stderr, "send_data filed: %s", strerror(errno));
	}

	/* read reply to the buffer */
	if (wfr == 1) {
		bytes = read(fd, reply, 1000);
		/* print server reply to as a string */
		reply[bytes] = 0;
	} else {
		return "";
	}

	return reply;
}

void wait_for(int fd, char *event)
{
	char *reply;
	int bytes;

	printf("       Waiting for: |%s|\n", event);
	reply = (char *)malloc(sizeof(char) * 1000);
	reply[0] = 0;
	while (0 == strcasestr(reply, event)) {
		bytes = read(fd, reply, 1000);
		if (bytes > 0) {
			reply[bytes] = 0;
			printf("       < %s\n", reply);
			fflush(NULL);
		}
	}
	free(reply);
	printf("       Continuing.\n");
	fflush(NULL);
}

/*
 * set_socket_path: establish the pathname that our Unix socket should
 * have.  If the SPEECHD_SOCKET environment variable is set, then that
 * will be our pathname.  Otherwise, the pathname
 * is $XDG_RUNTIME_DIR/speech-dispatcher/speechd.sock.
 */

void set_socket_path(struct sockaddr_un *address)
{
	size_t path_max = sizeof(address->sun_path);
	const char *path;
	char *pathcopy = NULL;

	path = g_getenv("SPEECHD_SOCKET");
	if (path == NULL || path[0] == '\0') {
		pathcopy = g_build_filename(g_get_user_runtime_dir(),
					    "speech-dispatcher",
					    "speechd.sock", NULL);
		path = pathcopy;
	}

	strncpy(address->sun_path, path, path_max - 1);
	address->sun_path[path_max - 1] = '\0';

	g_free(pathcopy);
}

/*
 * init: create and connect our Unix-domain socket.
 * Returns the file descriptor of the socket on success, or -1 on
 * failure.
 */

int init(void)
{
	int sockfd;
	int connect_success;
	struct sockaddr_un address;

	set_socket_path(&address);
	address.sun_family = AF_UNIX;
	sockfd = socket(AF_UNIX, SOCK_STREAM, 0);
	if (sockfd != -1) {
		connect_success =
		    connect(sockfd, (struct sockaddr *)&address,
			    SUN_LEN(&address));
		if (connect_success == -1) {
			close(sockfd);
			sockfd = -1;
		}
	}

	return sockfd;
}

int main(int argc, char *argv[])
{
	char *line;
	char *command;
	char *reply;
	int i;
	char *ret;
	FILE *test_file = NULL;
	int delays = 1;
	int indent = 0;

	if (argc < 2) {
		printf("No test script specified!\n");
		exit(1);
	}

	if (!strcmp(argv[1], "stdin")) {
		test_file = stdin;
	} else {
		test_file = fopen(argv[1], "r");
		if (test_file == NULL)
			FATAL("Test file doesn't exist");
	}

	if (argc == 3) {
		if (!strcmp(argv[2], "fast"))
			delays = 0;
		else {
			printf("Unrecognized parameter\n");
			exit(1);
		}
	}

	printf("Start of the test.\n");
	printf("==================\n\n");

	line = malloc(1024 * sizeof(char));
	reply = malloc(4096 * sizeof(char));

	sockk = init();
	if (sockk == -1)
		FATAL("Can't connect to Speech Dispatcher");

	assert(line != 0);

	while (1) {
		ret = fgets(line, 1024, test_file);
		if (ret == NULL)
			break;
		if (strlen(line) <= 1) {
			printf("\n");
			continue;
		}

		if (line[0] == '@') {
			command = (char *)strtok(line, "@\r\n");
			if (command == NULL)
				printf("\n");
			else
				printf("  %s\n", command);
			continue;
		}

		if (line[0] == '!') {
			command = (char *)strtok(line, "!\r\n");
			strcat(command, "\r\n");
			if (command == NULL)
				continue;

			printf("     >> %s", command);
			fflush(NULL);
			reply = send_data(sockk, command, 1);
			printf("     < %s", reply);
			fflush(NULL);
			continue;
		}

		if (line[0] == '.') {
			reply = send_data(sockk, "\r\n.\r\n", 1);
			printf("       < %s", reply);
			continue;
		}

		if (line[0] == '+') {
			command = (char *)strtok(&(line[1]), "+\r\n");
			wait_for(sockk, command);
			continue;
		}

		if (line[0] == '$') {
			if (delays) {
				command = (char *)strtok(&(line[1]), "$\r\n");
				sleep(atoi(command));
			}
			continue;
		}

		if (line[0] == '^') {
			if (delays) {
				command = (char *)strtok(&(line[1]), "$\r\n");
				usleep(atol(command));
			}
			continue;
		}

		if (line[0] == '~') {
			command = (char *)strtok(line, "~\r\n");
			indent = atoi(command);
			continue;
		}

		if (line[0] == '?') {
			getc(stdin);
			continue;
		}

		if (line[0] == '*') {
			int ret = system("clear");
			if (ret == -1)
				FATAL("Could not execute subprocess");
			for (i = 0; i <= indent - 1; i++) {
				printf("\n");
			}
			continue;
		}

		if (line[0] == '#') {
			/* Comment */
			continue;
		}

		send_data(sockk, line, 0);
		printf("     >> %s", line);
	}

	close(sockk);

	printf("\n==================\n");
	printf("End of the test.\n");
	exit(0);
}
