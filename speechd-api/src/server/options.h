/*
 * options.h - Defines possible command line options
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
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $Id: options.h,v 1.7 2008-07-07 14:31:08 hanke Exp $
 */

#include <getopt.h>

static struct option spd_long_options[] = {
    {"run-daemon", 0, 0, 'd'},
    {"run-single", 0, 0, 's'},
    {"spawn", 0, 0, 'a'},
    {"log-level", 1, 0, 'l'},
    {"communication-method", 1, 0, 'c'},
    {"socket-path", 1, 0, 'S'},
    {"port", 1, 0, 'p'},
    {"pid-file", 1, 0, 'P'},
    {"config-file", 1, 0, 'C'},
    {"version", 0, 0, 'v'},
    {"debug", 0, 0, 'D'},
    {"help", 0, 0, 'h'},
    {0, 0, 0, 0}
};

static char* spd_short_options = "dsal:c:S:p:P:C:vDh";

void options_print_help(char *argv[]);
void options_print_version(void);
void options_parse(int argc, char *argv[]);
