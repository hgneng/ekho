/*
 * module_main.c - Main file of output modules.
 *
 * Copyright (C) 2020-2021 Samuel Thibault <samuel.thibault@ens-lyon.org>
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

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "module_main.h"

/*
 * This provides the main startup structure for modules.
 */

int main(int argc, char *argv[])
{
	char *configfile = NULL;
	char *line, *msg;
	int ret;

	if (argc >= 2)
		configfile = argv[1];

	/* Read configuration */
	ret = module_config(configfile);
	if (ret) {
		module_close();
		exit(1);
	}

	/* Wait for server init */
	line = module_readline(STDIN_FILENO, 1);
	if (strcmp(line, "INIT\n") != 0) {
		fprintf(stderr, "ERROR: Server did not start with INIT\n");
		module_close();
		exit(3);
	}

	/* Initialize module */
	ret = module_init(&msg);
	if (ret) {
		if (msg == NULL)
			msg = strdup("Unspecified initialization error\n");
		printf("399-%s\n", msg);
		printf("399 ERR CANT INIT MODULE\n");
		free(msg);
		module_close();
		exit(1);
	}

	if (msg == NULL)
		msg = strdup("Unspecified initialization success\n");
	printf("299-%s\n", msg);
	printf("299 OK LOADED SUCCESSFULLY\n");
	fflush(stdout);
	free(msg);

	/* Run module */
	ret = module_loop();
	if (ret) {
		printf("399 ERR MODULE CLOSED\n");
		fflush(stdout);
		module_close();
	}

	/* Module termination */
	exit(ret);
}
