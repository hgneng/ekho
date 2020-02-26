
/*
 * set.h - Settings related functions for Speech Dispatcher header
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
 * $Id: set.h,v 1.17 2008-06-27 12:29:02 hanke Exp $
 */

#ifndef SET_H
#define SET_H

#include "speechd.h"
#include "history.h"

TFDSetElement *get_client_settings_by_uid(int uid);
TFDSetElement *get_client_settings_by_fd(int fd);
void remove_client_settings_by_uid(int uid);
int get_client_uid_by_fd(int fd);

int set_priority_uid(int uid, SPDPriority priority);
int set_language_uid(int uid, char *language);
int set_rate_uid(int uid, int rate);
int set_pitch_uid(int uid, int pitch);
int set_pitch_range_uid(int uid, int pitch_range);
int set_volume_uid(int uid, int volume);
int set_punct_mode_uid(int uid, int punct);
int set_cap_let_recog_uid(int uid, int recog);
int set_spelling_uid(int uid, SPDSpelling spelling);
int set_output_module_self(int uid, char *output_module);
int set_voice_uid(int uid, char *voice);
int set_synthesis_voice_uid(int uid, char *synthesis_voice);
int set_punctuation_mode_uid(int uid, SPDPunctuation punctuation);
int set_capital_letter_recognition_uid(int uid, SPDCapitalLetters recogn);
int set_output_module_uid(int uid, char *output_module);
int set_ssml_mode_uid(int uid, SPDDataMode ssml_mode);
int set_symbols_preprocessing_uid(int uid, gboolean symbols_preprocessing);
int set_pause_context_uid(int uid, int pause_context);
int set_debug_uid(int uid, int debug);
int set_debug_destination_uid(int uid, char *debug_destination);

int set_priority_self(int fd, SPDPriority priority);
int set_language_self(int fd, char *language);
int set_rate_self(int fd, int rate);
int set_pitch_self(int fd, int pitch);
int set_pitch_range_self(int fd, int pitch_range);
int set_volume_self(int fd, int volume);
int set_punct_mode_self(int fd, int punct);
int set_cap_let_recog_self(int fd, int recog);
int set_spelling_self(int fd, SPDSpelling spelling);
int set_output_module_self(int fd, char *output_module);
int set_client_name_self(int fd, char *client_name);
int set_voice_self(int fd, char *voice);
int set_synthesis_voice_self(int fd, char *synthesis_voice);
int set_punctuation_mode_self(int fd, SPDPunctuation punctuation);
int set_capital_letter_recognition_self(int fd, SPDCapitalLetters recogn);
int set_ssml_mode_self(int fd, SPDDataMode ssml_mode);
int set_symbols_preprocessing_self(int fd, gboolean symbols_preprocessing);
int set_notification_self(int fd, char *type, int val);
int set_pause_context_self(int fd, int pause_context);
int set_debug_self(int fd, int debug);
int set_debug_destination_self(int fd, char *debug_destination);

int set_priority_all(SPDPriority priority);
int set_language_all(char *language);
int set_rate_all(int rate);
int set_pitch_all(int pitch);
int set_pitch_range_all(int pitch_range);
int set_volume_all(int volume);
int set_punct_mode_all(int punct);
int set_cap_let_recog_all(int recog);
int set_spelling_all(SPDSpelling spelling);
int set_output_module_all(char *output_module);
int set_voice_all(char *voice);
int set_synthesis_voice_all(char *synthesis_voice);
int set_punctuation_mode_all(SPDPunctuation punctuation);
int set_capital_letter_recognition_all(SPDCapitalLetters recogn);
int set_ssml_mode_all(SPDDataMode ssml_mode);
int set_symbols_preprocessing_all(gboolean symbols_preprocessing);
int set_pause_context_all(int pause_context);
int set_debug_all(int debug);
int set_debug_destination_all(char *debug_destination);

TFDSetElement *default_fd_set(void);

char *set_param_str(char *parameter, char *value);

void update_cl_settings(gpointer data, gpointer user_data);

gint spd_str_compare(gconstpointer a, gconstpointer b);

#endif
