
/*
 * module.h - Definition of a module for Speech Dispatcher
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
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $Id: module.h,v 1.6 2008-06-27 12:28:43 hanke Exp $
 */

#ifndef MODULE_H
#define MODULE_H

#include <stdlib.h>
#include <glib.h>

typedef struct{
    char* name;
    char* filename;
    char* configfilename;
    char* debugfilename;
    int pipe_in[2];
    int pipe_out[2];    
    FILE *stream_out;
    int stderr_redirect;
    pid_t pid;
    int working;
    VoiceDescription **voices;
}OutputModule;

OutputModule* load_output_module(char* mod_name, char* mod_prog,
                                 char* mod_cfgfile, char * mod_dbgfile);
int unload_output_module(OutputModule *module);
int reload_output_module(OutputModule *old_module);
int output_module_debug(OutputModule *module);		       
int output_module_nodebug(OutputModule *module);		       
void destroy_module(OutputModule *module);

#endif

