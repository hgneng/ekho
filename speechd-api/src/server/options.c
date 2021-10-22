/*
 * options.c - Parse and process possible command line options
 *
 * Copyright (C) 2003, 2006 Brailcom, o.p.s.
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
 * $Id: options.c,v 1.13 2008-07-10 15:35:56 hanke Exp $
 */

/* NOTE: Be careful not to include options.h, we would
   get repetitive initializations warnings */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/stat.h>

#include "speechd.h"

#include "options.h"

#include <i18n.h>

static const struct option spd_long_options[] = {
	{"run-daemon", no_argument, 0, 'd'},
	{"run-single", no_argument, 0, 's'},
	{"spawn", no_argument, 0, 'a'},
	{"log-level", required_argument, 0, 'l'},
	{"log-dir", required_argument, 0, 'L'},
	{"communication-method", required_argument, 0, 'c'},
	{"socket-path", required_argument, 0, 'S'},
	{"port", required_argument, 0, 'p'},
	{"pid-file", required_argument, 0, 'P'},
	{"config-dir", required_argument, 0, 'C'},
	{"version", no_argument, 0, 'v'},
	{"debug", no_argument, 0, 'D'},
	{"help", no_argument, 0, 'h'},
	{"timeout", required_argument, 0, 't'},
	{"module-dir", required_argument, 0, 'm'},
	{0, 0, 0, 0}
};

static const char *const spd_short_options = "dsal:L:c:S:p:P:C:t:vDhm:";

void options_print_help(char *argv[])
{
	assert(argv);
	assert(argv[0]);

	printf(_("%s -- Common interface for Speech Synthesis %s\n\n"),
	       "Speech Dispatcher", "(GNU GPL)");
	printf(_("Usage: "));
	printf
	    ("%s [-{d|s}] [-l {1|2|3|4|5}] [-c com_method] [-S socket_path] [-p port] [-t timeout] | [-v] | [-h]\n\n",
	     argv[0]);
	printf(_("Options: \n"));
	printf("  -d, --run-daemon      ");
	printf(_("Run as a daemon\n"));
	printf("  -s, --run-single      ");
	printf(_("Run as single application\n"));
	printf("  -a, --spawn           ");
	printf(_("Start only if autospawn is not disabled\n"));
	printf("  -l, --log-level       ");
	printf(_("Set log level (between %d and %d)\n"), 1, 5);
	printf("  -L, --log-dir         ");
	printf(_("Set path to logging\n"));
	printf("  -c, --communication-method\n");
	printf("                        ");
	printf(_("Communication method to use ('%s'\n"), "unix_socket");
	printf("                        ");
	printf(_("or '%s')\n"), "inet_socket");
	printf("  -S, --socket-path     ");
	printf(_
	       ("Socket path to use for '%s' method\n"), "unix_socket");
	printf("                        ");
	printf(_("(filesystem path or '%s')\n"), "default");
	printf("  -p, --port            ");
	printf(_("Specify a port number for '%s' method\n"), "inet_socket");
	printf("  -t, --timeout         ");
	printf(_("Set time in seconds for the server to wait before it\n"));
	printf("                        ");
	printf(_("shuts down, if it has no clients connected. Setting to\n"));
	printf("                        ");
	printf(_("0 disables such shutdown\n"));
	printf("  -P, --pid-file        ");
	printf(_("Set path to pid file\n"));
	printf("  -C, --config-dir      ");
	printf(_("Set path to configuration\n"));
	printf("  -m, --module-dir      ");
	printf(_("Set path to modules\n"));
	printf("  -v, --version         ");
	printf(_("Report version of this program\n"));
	printf("  -D, --debug           ");
	printf(_("Output debugging information into $TMPDIR/%s\n"),
	       "speechd-debug");
	printf("                        ");
	printf(_("if TMPDIR is exported, otherwise to /tmp/%s\n"),
	       "speechd-debug");
	printf("  -h, --help            ");
	printf(_("Print this info\n"));
	printf("\n");
	printf(_("Please report bugs to %s\n\n"), PACKAGE_BUGREPORT);
}

void options_print_version(void)
{
	printf("%s %s\n", PACKAGE, VERSION);
	printf(_("Copyright (C) %d-%d Brailcom, o.p.s.\n"
		 "This is free software; you can redistribute it and/or modify it\n"
		 "under the terms of the GNU General Public License as published by\n"
		 "the Free Software Foundation; either version 2, or (at your option)\n"
		 "any later version. Please see COPYING for more details.\n\n"), 2002, 2012);
}

#define SPD_OPTION_SET_INT(param) \
	val = strtol(optarg, &tail_ptr, 10); \
	if(tail_ptr != optarg){ \
		SpeechdOptions.param ## _set = 1; \
		SpeechdOptions.param = val; \
	}

#define SPD_OPTION_SET_STR(param) \
	SpeechdOptions.param = g_strdup(optarg)

void options_parse(int argc, char *argv[])
{
	char *tail_ptr;
	int c_opt;
	int option_index;
	int val;
	int ret;

	char *tmpdir;
	char *debug_logfile_path;

	assert(argc > 0);
	assert(argv);

	while (1) {
		option_index = 0;

		c_opt =
		    getopt_long(argc, argv, spd_short_options, spd_long_options,
				&option_index);
		if (c_opt == -1)
			break;
		switch (c_opt) {
		case 'd':
			spd_mode = SPD_MODE_DAEMON;
			break;
		case 's':
			spd_mode = SPD_MODE_SINGLE;
			break;
		case 'l':
			SPD_OPTION_SET_INT(log_level);
			break;
		case 'L':
			SPD_OPTION_SET_STR(log_dir);
			SpeechdOptions.log_dir_set = 1;
			break;
		case 'c':
			SPD_OPTION_SET_STR(communication_method);
			SpeechdOptions.communication_method_set = 1;
			break;
		case 'S':
			SPD_OPTION_SET_STR(socket_path);
			SpeechdOptions.socket_path_set = 1;
			break;
		case 'p':
			SPD_OPTION_SET_INT(port);
			break;
		case 'a':
			SpeechdOptions.spawn = TRUE;
			break;
		case 'P':
			SPD_OPTION_SET_STR(pid_file);
			break;
		case 'C':
			SPD_OPTION_SET_STR(conf_dir);
			break;
		case 'm':
			SPD_OPTION_SET_STR(module_dir);
			break;
		case 'v':
			options_print_version();
			exit(0);
			break;
		case 'D':
			tmpdir = g_strdup(getenv("TMPDIR"));
			if (!tmpdir)
				tmpdir = g_strdup("/tmp");
			SpeechdOptions.debug_destination =
			    g_strdup_printf("%s/speechd-debug", tmpdir);
			g_free(tmpdir);

			ret = mkdir(SpeechdOptions.debug_destination, S_IRWXU);
			if (ret) {
				MSG(1,
				    "Can't create additional debug destination in %s, reason %d-%s",
				    SpeechdOptions.debug_destination, errno,
				    strerror(errno));
				if (errno == EEXIST) {
					MSG(1,
					    "Debugging directory %s already exists, please delete it first",
					    SpeechdOptions.debug_destination);
				}
				exit(1);
			}

			debug_logfile_path =
			    g_strdup_printf("%s/speech-dispatcher.log",
					    SpeechdOptions.debug_destination);
			/* Open logfile for writing */
			debug_logfile = fopen(debug_logfile_path, "wx");
			g_free(debug_logfile_path);
			if (debug_logfile == NULL) {
				MSG(1,
				    "Error: can't open additional debug logging file %s [%d-%s]!\n",
				    debug_logfile_path, errno, strerror(errno));
				exit(1);
			}
			SpeechdOptions.debug = 1;
			break;
		case 'h':
			options_print_help(argv);
			exit(0);
			break;
		case 't':
			SPD_OPTION_SET_INT(server_timeout);
			break;
		default:
			MSG(2, "Unrecognized option\n");
			options_print_help(argv);
			exit(1);
		}
	}
}

#undef SPD_OPTION_SET_INT
