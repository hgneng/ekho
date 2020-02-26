
/*
 * say.c - Super-simple Speech Dispatcher client
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
 *
 * $Id: say.c,v 1.16 2007-05-03 09:43:12 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <semaphore.h>
#include <errno.h>
#include <getopt.h>

/*
 * Since this client is built as part of the Speech Dispatcher source
 * tree, we must include speechd_types.h directly.
 * Clients built outside the speech dispatcher source tree should not do
 * this.
 */
#include <speechd_types.h>
#include <libspeechd.h>

#include "options.h"
#include <i18n.h>
#include <locale.h>

#define MAX_LINELEN 16384

sem_t semaphore;

/* Callback for Speech Dispatcher notifications */
void end_of_speech(size_t msg_id, size_t client_id, SPDNotificationType type)
{
	sem_post(&semaphore);
}

void index_mark(size_t msg_id, size_t client_id, SPDNotificationType type, char *index_mark)
{
	if (type == SPD_EVENT_INDEX_MARK)
		fprintf(stderr, "reached mark '%s'\n", index_mark);
}

int main(int argc, char **argv)
{
	SPDConnection *conn;
	SPDPriority spd_priority;
	int err;
	char *error;
	int msg_arg_required = 0;
	int ret;
	int option_ret;
	char *line;

	/* initialize i18n support */
	i18n_init();

	rate = -101;
	pitch = -101;
	pitch_range = -101;
	volume = -101;
	language = NULL;
	voice_type = NULL;
	punctuation_mode = NULL;
	spelling = -2;
	ssml_mode = SPD_DATA_TEXT;
	wait_till_end = 0;
	stop_previous = 0;
	cancel_previous = 0;
	list_synthesis_voices = 0;
	list_output_modules = 0;
	synthesis_voice = NULL;
	pipe_mode = 0;
	priority = NULL;
	application_name = NULL;
	connection_name = NULL;

	option_ret = options_parse(argc, argv);

	/* Check if the text to say or options are specified in the argument */
	msg_arg_required = (pipe_mode != 1) && (stop_previous != 1)
	    && (cancel_previous != 1) && (list_synthesis_voices != 1)
	    && (list_output_modules != 1) && (sound_icon == NULL);
	if ((optind >= argc) && msg_arg_required) {
		options_print_help(argv);
		return 1;
	}

	/* Open a connection to Speech Dispatcher */
	conn = spd_open2(application_name ? application_name : "spd-say",
			 connection_name ? connection_name : "main",
			 NULL, SPD_MODE_THREADED, NULL, 1, &error);
	if (conn == NULL) {
		fprintf(stderr, "Failed to connect to Speech Dispatcher:\n%s\n",
			error);
		exit(1);
	}

	if (stop_previous)
		spd_stop_all(conn);
	if (cancel_previous)
		spd_cancel_all(conn);

	/* Set the desired parameters */

	if (language != NULL) {
		if (spd_set_language(conn, language))
			printf("Invalid language!\n");
	} else {
		char *locale = strdup(setlocale(LC_MESSAGES, NULL));
		char *dot = index(locale, '.');
		if (dot)
			*dot = 0;
		char *at = index(locale, '@');
		if (at)
			*at = 0;
		char *underscore = index(locale, '_');
		if (underscore)
			*underscore = '-';
		if (spd_set_language(conn, locale))
			printf("Invalid language %s!\n", locale);
		free(locale);
	}

	if (output_module != NULL)
		if (spd_set_output_module(conn, output_module))
			printf("Invalid output module!\n");

	if (list_output_modules) {
		char **list;
		int i;

		list = spd_list_modules(conn);

		if (list != NULL) {
			printf("OUTPUT MODULES\n");
			for (i = 0; list[i]; i++) {
				printf("%s\n", list[i]);
			}
		} else {
			printf("Output modules not found.\n");
		}
	}

	if (voice_type != NULL) {
		if (!strcmp(voice_type, "male1")) {
			if (spd_set_voice_type(conn, SPD_MALE1))
				printf("Can't set this voice!\n");
		} else if (!strcmp(voice_type, "male2")) {
			if (spd_set_voice_type(conn, SPD_MALE2))
				printf("Can't set this voice!\n");
		} else if (!strcmp(voice_type, "male3")) {
			if (spd_set_voice_type(conn, SPD_MALE3))
				printf("Can't set this voice!\n");
		} else if (!strcmp(voice_type, "female1")) {
			if (spd_set_voice_type(conn, SPD_FEMALE1))
				printf("Can't set this voice!\n");
		} else if (!strcmp(voice_type, "female2")) {
			if (spd_set_voice_type(conn, SPD_FEMALE2))
				printf("Can't set this voice!\n");
		} else if (!strcmp(voice_type, "female3")) {
			if (spd_set_voice_type(conn, SPD_FEMALE3))
				printf("Can't set this voice!\n");
		} else if (!strcmp(voice_type, "child_male")) {
			if (spd_set_voice_type(conn, SPD_CHILD_MALE))
				printf("Can't set this voice!\n");
		} else if (!strcmp(voice_type, "child_female")) {
			if (spd_set_voice_type(conn, SPD_CHILD_FEMALE))
				printf("Can't set this voice!\n");
		} else {
			printf("Invalid voice\n");
		}
	}

	if (list_synthesis_voices) {
		SPDVoice **list;
		int i;

		list = spd_list_synthesis_voices(conn);
		if (list != NULL) {
			printf("%25s%25s%25s\n", "NAME", "LANGUAGE", "VARIANT");
			for (i = 0; list[i]; i++) {
				printf("%25s%25s%25s\n", list[i]->name,
				       list[i]->language, list[i]->variant);
			}
		} else {
			printf("Failed to get voice list.\n");
		}
	}

	if (synthesis_voice != NULL)
		if (spd_set_synthesis_voice(conn, synthesis_voice))
			printf("Failed to set synthesis voice!\n");

	if (ssml_mode == SPD_DATA_SSML)
		if (spd_set_data_mode(conn, ssml_mode))
			printf("Failed to set SSML mode.\n");

	if (rate != -101)
		if (spd_set_voice_rate(conn, rate))
			printf("Invalid rate!\n");

	if (pitch != -101)
		if (spd_set_voice_pitch(conn, pitch))
			printf("Invalid pitch!\n");

	if (pitch_range != -101)
		if (spd_set_voice_pitch_range(conn, pitch_range))
			printf("Invalid pitch range!\n");

	if (volume != -101)
		if (spd_set_volume(conn, volume))
			printf("Invalid volume!\n");

	if (spelling == 1)
		if (spd_set_spelling(conn, SPD_SPELL_ON))
			printf("Can't set spelling to on!\n");

	if (punctuation_mode != NULL) {
		if (!strcmp(punctuation_mode, "none")) {
			if (spd_set_punctuation(conn, SPD_PUNCT_NONE))
				printf("Can't set this punctuation mode!\n");
		} else if (!strcmp(punctuation_mode, "some")) {
			if (spd_set_punctuation(conn, SPD_PUNCT_SOME))
				printf("Can't set this punctuation mode!\n");
		} else if (!strcmp(punctuation_mode, "all")) {
			if (spd_set_punctuation(conn, SPD_PUNCT_ALL))
				printf("Can't set this punctuation mode!\n");
		} else {
			printf("Invalid punctuation mode.\n");
		}
	}

	/* Set default priority... */
	if (1 == pipe_mode)
		spd_priority = SPD_MESSAGE;
	else
		spd_priority = SPD_TEXT;
	/* ...and look if it wasn't overriden */
	if (priority != NULL) {
		if (!strcmp(priority, "important"))
			spd_priority = SPD_IMPORTANT;
		else if (!strcmp(priority, "message"))
			spd_priority = SPD_MESSAGE;
		else if (!strcmp(priority, "text"))
			spd_priority = SPD_TEXT;
		else if (!strcmp(priority, "notification"))
			spd_priority = SPD_NOTIFICATION;
		else if (!strcmp(priority, "progress"))
			spd_priority = SPD_PROGRESS;
		else {
			printf("Invalid priority.\n");
		}
	}

	if (sound_icon != NULL)
		if (spd_sound_icon(conn, spd_priority, sound_icon))
			printf("Invalid sound_icon!\n");

	if (wait_till_end) {
		ret = sem_init(&semaphore, 0, 0);
		if (ret < 0) {
			fprintf(stderr, "Can't initialize semaphore: %s",
				strerror(errno));
			return 0;
		}

		/* Notify when the message is canceled or the speech terminates */
		conn->callback_end = end_of_speech;
		conn->callback_cancel = end_of_speech;
		conn->callback_im = index_mark;
		spd_set_notification_on(conn, SPD_END);
		spd_set_notification_on(conn, SPD_CANCEL);
		spd_set_notification_on(conn, SPD_INDEX_MARKS);
	}

	/* In pipe mode, read from stdin, write to stdout, and also to Speech Dispatcher. */
	if (pipe_mode == 1) {
		line = (char *)malloc(MAX_LINELEN);
		while (NULL != fgets(line, MAX_LINELEN, stdin)) {
			fputs(line, stdout);
			if (0 == strncmp(line, "!-!", 3)) {
				/* Remove EOL */
				line[strlen(line) - 1] = 0;
				spd_execute_command(conn, line + 3);
			} else {
				spd_say(conn, spd_priority, line);
				if (wait_till_end)
					sem_wait(&semaphore);
			}
		}
		free(line);

	} else {
		/* Say the message with priority "text" */
		/* Or do nothing in case of -C or -S with no message. */
		if (optind < argc) {
			err =
			    spd_sayf(conn, spd_priority, (char *)argv[optind]);
			if (err == -1) {
				fprintf(stderr,
					"Speech Dispatcher failed to say message");
				exit(1);
			}

			/* Wait till the callback is called */
			if (wait_till_end)
				sem_wait(&semaphore);
		}
	}

	/* Close the connection */
	spd_close(conn);

	return 0;
}
