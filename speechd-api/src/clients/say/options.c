/*
 * options.c - Parse and process possible command line options
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
 * $Id: options.c,v 1.9 2006-07-11 16:12:26 hanke Exp $
 */

/* NOTE: Be careful not to include options.h, we would
   get repetitive initializations warnings */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "options.h"
#include <i18n.h>

signed int rate;
signed int pitch;
signed int pitch_range;
signed int volume;

int list_output_modules;
char *output_module;
char *sound_icon;
char *language;
char *voice_type;
char *punctuation_mode;
char *priority;
int pipe_mode;
int character;
int key;
SPDDataMode ssml_mode;
int spelling;
int wait_till_end;
int stop_previous;
int cancel_previous;
int list_synthesis_voices;
char *synthesis_voice;

char *application_name;
char *connection_name;

void options_print_help(char *argv[])
{
	assert(argv);
	assert(argv[0]);

	printf(_("send text-to-speech output request to speech-dispatcher\n\n"));
	printf(_("Usage: %s [options] \"some text\"\n\n"), argv[0]);

	printf(_("Options:\n"));
	printf("  -r, --rate                      ");
	printf(_("Set the rate of the speech\n"));
	printf("                                  ");
	printf(_("(between %+d and %+d, default: %d)\n"), -100, 100, 0);

	printf("  -p, --pitch                     ");
	printf(_("Set the pitch of the speech\n"));
	printf("                                  ");
	printf(_("(between %+d and %+d, default: %d)\n"), -100, 100, 0);

	printf("  -R, --pitch-range               ");
	printf(_("Set the pitch range of the speech\n"));
	printf("                                  ");
	printf(_("(between %+d and %+d, default: %d)\n"), -100, 100, 0);

	printf("  -i, --volume                    ");
	printf(_("Set the volume (intensity) of the speech\n"));
	printf("                                  ");
	printf(_("(between %+d and %+d, default: %d)\n"), -100, 100, 0);

	printf("  -o, --output-module             ");
	printf(_("Set the output module\n"));

	printf("  -O, --list-output-modules       ");
	printf(_("Get the list of output modules\n"));

	printf("  -I, --sound-icon                ");
	printf(_("Play the sound icon\n"));

	printf("  -l, --language                  ");
	printf(_("Set the language (ISO code)\n"));

	printf("  -t, --voice-type                ");
	printf(_("Set the preferred voice type\n"));
	printf("                                  (male1, male2, male3, female1, female2\n"
	       "                                  female3, child_male, child_female)\n");

	printf("  -L, --list-synthesis-voices     ");
	printf(_("Get the list of synthesis voices\n"));

	printf("  -y, --synthesis-voice           ");
	printf(_("Set the synthesis voice\n"));

	printf("  -c, --character                 ");
	printf(_("Speak the character\n"));

	printf("  -k, --key                       ");
	printf(_("Speak the key\n"));

	printf("  -m, --punctuation-mode          ");
	printf(_("Set the punctuation mode %s\n"), "(none, some, most, all)");

	printf("  -s, --spelling                  ");
	printf(_("Spell the message\n"));

	printf("  -x, --ssml                      ");
	printf(_("Set SSML mode on (default: off)\n"));
	printf("\n");

	printf("  -e, --pipe-mode                 ");
	printf(_("Pipe from stdin to stdout plus Speech Dispatcher\n"));

	printf("  -P, --priority                  ");
	printf(_("Set priority of the message "));
	printf("(important, message,\n"
	       "%stext, notification, progress;",
	       "                                  ");
	printf(_("default: %s)\n"), "text");

	printf("  -N, --application-name          ");
	printf(_("Set the application name used to establish\n"
		 "%sthe connection to specified string value\n"),
	       "                                  ");
	printf("                                  ");
	printf(_("(default: %s)\n"), "spd-say");

	printf("  -n, --connection-name           ");
	printf(_("Set the connection name used to establish\n"
		 "%sthe connection to specified string value\n"),
	       "                                  ");
	printf("                                  ");
	printf(_("(default: %s)\n"), "main");
	printf("\n");

	printf("  -w, --wait                      ");
	printf(_("Wait till the message is spoken or discarded\n"));

	printf("  -S, --stop                      ");
	printf(_("Stop speaking the message being spoken\n"));

	printf("  -C, --cancel                    ");
	printf(_("Cancel all messages\n"));
	printf("\n");

	printf("  -v, --version                   ");
	printf(_("Print version and copyright info\n"));

	printf("  -h, --help                      ");
	printf(_("Print this info\n"));
	printf("\n");

	printf(_("Please report bugs to %s\n\n"), PACKAGE_BUGREPORT);

}

void options_print_version()
{
	printf("spd-say " VERSION "\n");
	printf(_("Copyright (C) %d-%d Brailcom, o.p.s.\n"
		 "This is free software; you can redistribute it and/or modify it\n"
		 "under the terms of the GNU General Public License as published by\n"
		 "the Free Software Foundation; either version 2, or (at your option)\n"
		 "any later version. Please see COPYING for more details.\n\n"), 2002, 2012);
}

#define OPT_SET_INT(param) \
	val = strtol(optarg, &tail_ptr, 10); \
	if(tail_ptr != optarg){ \
		param = val; \
	}else{ \
		printf(_("Syntax error or bad parameter!\n"));	\
		options_print_help(argv); \
		exit(1); \
	}

#define OPT_SET_STR(param) \
	if(optarg != NULL){ \
		if (param) \
			free(param); \
		param = (char*) strdup(optarg); \
	}else{ \
		printf(_("Missing argument!\n"));	\
		options_print_help(argv); \
		exit(1); \
	}

int options_parse(int argc, char *argv[])
{
	char *tail_ptr;
	int c_opt;
	int option_index;
	int val;

	assert(argc > 0);
	assert(argv);

	while (1) {
		option_index = 0;

		c_opt = getopt_long(argc, argv, short_options, long_options,
				    &option_index);
		if (c_opt == -1)
			break;
		switch (c_opt) {
		case 'r':
			OPT_SET_INT(rate);
			break;
		case 'p':
			OPT_SET_INT(pitch);
			break;
		case 'R':
			OPT_SET_INT(pitch_range);
			break;
		case 'i':
			OPT_SET_INT(volume);
			break;
		case 'l':
			OPT_SET_STR(language);
			break;
		case 'o':
			OPT_SET_STR(output_module);
			break;
		case 'O':
			list_output_modules = 1;
			break;
		case 'I':
			OPT_SET_STR(sound_icon);
			break;
		case 't':
			OPT_SET_STR(voice_type);
			break;
		case 'L':
			list_synthesis_voices = 1;
			break;
		case 'y':
			OPT_SET_STR(synthesis_voice);
			break;
		case 'c':
			character = 1;
			break;
		case 'k':
			key = 1;
			break;
		case 'm':
			OPT_SET_STR(punctuation_mode);
			break;
		case 's':
			spelling = 1;
			break;
		case 'e':
			pipe_mode = 1;
			break;
		case 'P':
			OPT_SET_STR(priority);
			break;
		case 'x':
			ssml_mode = SPD_DATA_SSML;
			break;
		case 'N':
			OPT_SET_STR(application_name);
			break;
		case 'n':
			OPT_SET_STR(connection_name);
			break;
		case 'w':
			wait_till_end = 1;
			break;
		case 'S':
			stop_previous = 1;
			break;
		case 'C':
			cancel_previous = 1;
			break;
		case 'v':
			options_print_version();
			exit(0);
			break;
		case 'h':
			options_print_help(argv);
			exit(0);
			break;
		default:
			printf(_("Unrecognized option\n"));
			options_print_help(argv);
			exit(1);
		}
	}
	return 0;
}

#undef SPD_OPTION_SET_INT
