/*
 * output.h - Output layer for Speech Dispatcher header
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
 * $Id: output.h,v 1.9 2008-06-27 12:28:50 hanke Exp $
 */

#include "speechd.h"
#include "speaking.h"

OutputModule *get_output_module(const TSpeechDMessage * message);

int output_speak(TSpeechDMessage * msg, OutputModule *output);
int output_stop(void);
size_t output_pause(void);
int output_is_speaking(char **index_mark);
int output_send_debug(OutputModule * output, int flag, const char *logfile_path);

int output_check_module(OutputModule * output);

char *escape_dot(char *otext);

void output_set_speaking_monitor(TSpeechDMessage * msg, OutputModule * output);
GString *output_read_reply(OutputModule * output);
int output_send_data(const char *cmd, OutputModule * output, int wfr);
int output_send_settings(TSpeechDMessage * msg, OutputModule * output);
int output_send_audio_settings(OutputModule * output);
int output_send_loglevel_setting(OutputModule * output);
int waitpid_with_timeout(pid_t pid, int *status_ptr, int options,
			 size_t timeout);
int output_close(OutputModule * module);
SPDVoice **output_list_voices(const char *module_name);
