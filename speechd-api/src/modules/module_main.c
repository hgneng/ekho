/*
 * module_main.c - One way of doing main() in output modules.
 *
 * Copyright (C) 2001, 2002, 2003, 2006 Brailcom, o.p.s.
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
 *
 * $Id: module_main.c,v 1.17 2008-10-15 17:05:37 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <glib.h>
#include <dotconf.h>
#include <ltdl.h>

#include <spd_utils.h>
#include "module_utils.h"

#define PROCESS_CMD(command, function) \
	if (!strcmp(cmd_buf, #command"\n")){ \
		char *msg; \
		pthread_mutex_lock(&module_stdout_mutex); \
		if (printf("%s\n", msg = (char*) function()) < 0){ \
			DBG("Broken pipe, exiting...\n"); \
			ret = 2; \
			break; \
		} \
		fflush(stdout); \
		pthread_mutex_unlock(&module_stdout_mutex);\
		g_free(msg); \
	}

#define PROCESS_CMD_W_ARGS(command, function) \
	if (!strncmp(cmd_buf, #command, strlen(#command))){	\
		char *msg; \
		pthread_mutex_lock(&module_stdout_mutex); \
		if (printf("%s\n", msg = (char*) function(cmd_buf)) < 0){ \
			DBG("Broken pipe, exiting...\n"); \
			ret = 2; \
			break; \
		} \
		fflush(stdout); \
		pthread_mutex_unlock(&module_stdout_mutex);\
		g_free(msg); \
	}

#define PROCESS_CMD_NRP(command, function) \
	if (!strcmp(cmd_buf, #command"\n")){ \
		function(); \
	}

int main(int argc, char *argv[])
{
	char *cmd_buf;
	int ret;
	size_t n;
	char *configfilename = NULL;
	char *status_info = NULL;

	/* Initialize ltdl's list of preloaded audio backends. */
	LTDL_SET_PRELOADED_SYMBOLS();
	module_num_dc_options = 0;
	module_audio_id = 0;

	if (argc >= 2) {
		configfilename = g_strdup(argv[1]);
	}

	ret = module_load();
	if (ret == -1) {
		module_close();
		exit(1);
	}

	if (configfilename != NULL) {
		/* Add the LAST option */
		module_dc_options = module_add_config_option(module_dc_options,
							     &module_num_dc_options,
							     "", 0, NULL, NULL,
							     0);

		configfile =
		    dotconf_create(configfilename, module_dc_options, 0,
				   CASE_INSENSITIVE);
		if (configfile) {
			if (dotconf_command_loop(configfile) == 0) {
				DBG("Error reading config file\n");
				module_close();
				exit(1);
			}
			dotconf_cleanup(configfile);
			DBG("Configuration (pre) has been read from \"%s\"\n",
			    configfilename);

			g_free(configfilename);
		} else {
			DBG("Can't read specified config file!\n");
		}
	} else {
		DBG("No config file specified, using defaults...\n");
	}

	cmd_buf = NULL;
	n = 0;
	ret = spd_getline(&cmd_buf, &n, stdin);
	if (ret == -1) {
		DBG("Broken pipe when reading INIT, exiting... \n");
		module_close();
		exit(2);
	}

	if (strcmp(cmd_buf, "INIT\n")) {
		DBG("ERROR: Wrong communication from module client: didn't call INIT\n");
		module_close();
		exit(3);
	}

	ret = module_init(&status_info);

	if (status_info == NULL) {
		status_info = g_strdup("unknown, was not set by module");
	}

	if (ret != 0) {
		printf("399-%s\n", status_info);
		printf("%s\n", "399 ERR CANT INIT MODULE");
		g_free(status_info);
		module_close();
		exit(1);
	}

	printf("299-%s\n", status_info);
	ret = printf("%s\n", "299 OK LOADED SUCCESSFULLY");

	if (ret < 0) {
		DBG("Broken pipe, exiting...\n");
		module_close();
		exit(2);
	}
	fflush(stdout);

	g_free(status_info);
	g_free(cmd_buf);

	while (1) {
		cmd_buf = NULL;
		n = 0;
		ret = spd_getline(&cmd_buf, &n, stdin);
		if (ret == -1) {
			DBG("Broken pipe, exiting... \n");
			ret = 2;
			break;
		}

		DBG("CMD: <%s>", cmd_buf);

		PROCESS_CMD(SPEAK, do_speak)
		    else
		PROCESS_CMD(SOUND_ICON, do_sound_icon)
		    else
		PROCESS_CMD(CHAR, do_char)
		    else
		PROCESS_CMD(KEY, do_key)
		    else
		PROCESS_CMD_NRP(STOP, do_stop)
		    else
		PROCESS_CMD_NRP(PAUSE, do_pause)
		    else
		PROCESS_CMD(LIST VOICES, do_list_voices)
		    else
		PROCESS_CMD(SET, do_set)
		    else
		PROCESS_CMD(AUDIO, do_audio)
		    else
		PROCESS_CMD(LOGLEVEL, do_loglevel)
		    else
		PROCESS_CMD_W_ARGS(DEBUG, do_debug)
		    else
	if (!strcmp(cmd_buf, "QUIT\n")) {
		do_quit();
		exit(0);
	} else {
		printf("300 ERR UNKNOWN COMMAND\n");
		fflush(stdout);
	}

	g_free(cmd_buf);
	}

	module_close();
	exit(ret);
}

#undef PROCESS_CMD
