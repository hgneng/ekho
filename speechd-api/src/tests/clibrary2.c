
/*
 * clibrary2.c - Testing LIST and associated set functions in Speech Dispatcher
 *
 * Copyright (C) 2008 Brailcom, o.p.s.
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
 * $Id: clibrary2.c,v 1.1 2008-04-09 11:41:52 hanke Exp $
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
	int i, j, ret;
	char **modules;
	char **voices;
	char *module;
	char *language;
	int value;
	SPDVoiceType voice_type = SPD_CHILD_MALE;
	SPDVoice **synth_voices;

	printf("Start of the test.\n");

	printf("Trying to initialize Speech Deamon...");
	conn = spd_open("say", NULL, NULL, SPD_MODE_SINGLE);
	if (conn == 0) {
		printf("Speech Deamon failed");
		exit(1);
	}
	printf("OK\n");

	printf("Trying to get the current output module...");
	module = spd_get_output_module(conn);
	printf("Got module %s\n", module);
	if (module == NULL) {
		printf("Can't get current output module\n");
		exit(1);
	}

	printf("Trying to get the language...");
	language = spd_get_language(conn);
	printf("Got language %s\n", language);
	if (language == NULL) {
		printf("Can't get the language\n");
		exit(1);
	}

	printf("Trying to get the voice rate...");
	value = spd_get_voice_rate(conn);
	printf("Got rate %d\n", value);

	printf("Trying to get the voice pitch...");
	value = spd_get_voice_pitch(conn);
	printf("Got pitch %d\n", value);

	printf("Trying to get the current volume...");
	value = spd_get_volume(conn);
	printf("Got volume %d\n", value);

	printf("Trying to get the current voice type...");
	spd_set_voice_type(conn, voice_type);
	voice_type = spd_get_voice_type(conn);
	printf("Got voice type %d\n", voice_type);

	modules = spd_list_modules(conn);
	if (modules == NULL) {
		printf("Can't list modules\n");
		exit(1);
	}

	printf("Available output modules:\n");
	for (i = 0;; i++) {
		if (modules[i] == NULL)
			break;
		printf("     %s\n", modules[i]);
	}

	voices = spd_list_voices(conn);
	if (voices == NULL) {
		printf("Can't list voices\n");
		exit(1);
	}

	printf("Available symbolic voices:\n");
	for (i = 0;; i++) {
		if (voices[i] == NULL)
			break;
		printf("     %s\n", voices[i]);
	}

	for (j = 0;; j++) {
		if (modules[j] == NULL)
			break;
		ret = spd_set_output_module(conn, modules[j]);
		if (ret == -1) {
			printf("spd_set_output_module failed");
			exit(1);
		}
		printf("\nListing voices for %s\n", modules[j]);
		synth_voices = spd_list_synthesis_voices(conn);
		if (synth_voices == NULL) {
			printf("Can't list voices\n");
			exit(1);
		}
		printf("Available synthesis voices:\n");
		for (i = 0;; i++) {
			if (synth_voices[i] == NULL)
				break;
			printf("     name: %s language: %s variant: %s\n",
			       synth_voices[i]->name, synth_voices[i]->language,
			       synth_voices[i]->variant);
			ret =
			    spd_set_synthesis_voice(conn,
						    synth_voices[i]->name);
			if (ret == -1) {
				printf("spd_set_synthesis_voice failed");
				exit(1);
			}

			ret = spd_say(conn, SPD_TEXT, "test");
			if (ret == -1) {
				printf("spd_say failed");
				exit(1);
			}
			sleep(1);
		}
	}

	printf("Trying to close Speech Dispatcher connection...");
	spd_close(conn);
	printf("OK\n");

	printf("End of the test.\n");
	exit(0);
}
