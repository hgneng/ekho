/*
 * options.h - Defines possible command line options
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
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $Id: options.h,v 1.10 2006-07-11 16:12:27 hanke Exp $
 */

#include <getopt.h>

signed int rate;
signed int pitch;
signed int volume;

char *output_module;
char *language;
char *voice_type;
char *punctuation_mode;
char *priority;
int pipe_mode;
int ssml_mode;
int spelling;
int wait_till_end;
int stop_previous;
int cancel_previous;

char *application_name;
char *connection_name;

static struct option long_options[] = {
    {"rate", 1, 0, 'r'},
    {"pitch", 1, 0, 'p'},
    {"volume", 1, 0, 'i'},
    {"output-module", 1, 0, 'o'},
    {"language", 1, 0, 'l'},
    {"voice-type", 1, 0, 't'},
    {"punctuation-mode", 1, 0, 'm'},
    {"spelling", 0, 0, 's'},
    {"ssml", 0, 0, 'x'},
    {"pipe-mode", 0, 0, 'e'},
    {"priority", 1, 0, 'P'},
    {"application-name", 1, 0, 'N'},
    {"connection-name", 1, 0, 'n'},
    {"wait", 0, 0, 'w'},
    {"stop", 1, 0, 'S'},
    {"cancel", 1, 0, 'C'},
    {"version", 0, 0, 'v'},
    {"help", 0, 0, 'h'},
    {0, 0, 0, 0}
};

static char* short_options = "r:p:i:l:o:t:m:sxeP:N:n:wSCvh";

int options_parse(int argc, char *argv[]);
void options_print_version();
void options_print_help(char *argv[]);
