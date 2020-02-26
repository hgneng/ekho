
/*
 * set.c - Settings related functions for Speech Dispatcher
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
 * $Id: set.c,v 1.46 2008-07-01 09:00:32 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <fnmatch.h>

#include "set.h"
#include "alloc.h"
#include "msg.h"

gint spd_str_compare(gconstpointer a, gconstpointer b)
{
	return strcmp((char *)a, (char *)b);
}

int set_priority_self(int fd, SPDPriority priority)
{
	int uid;
	int ret;

	uid = get_client_uid_by_fd(fd);
	if (uid == 0)
		return 1;
	ret = set_priority_uid(uid, priority);

	return ret;
}

int set_priority_uid(int uid, SPDPriority priority)
{
	TFDSetElement *settings;
	if ((priority < SPD_IMPORTANT) || (priority > SPD_PROGRESS))
		return 1;
	settings = get_client_settings_by_uid(uid);
	if (settings == NULL)
		return 1;

	settings->priority = priority;
	return 0;
}

#define SET_SELF_ALL(type, param) \
	int \
	set_ ## param ## _self(int fd, type param) \
	{ \
		int uid; \
		uid = get_client_uid_by_fd(fd); \
		if (uid == 0) return 1; \
		return set_ ## param ## _uid(uid, param); \
	} \
	int \
	set_ ## param ## _all(type param) \
	{ \
		int i; \
		int uid; \
		int err = 0; \
		for(i=1;i<=SpeechdStatus.max_fd;i++){ \
			uid = get_client_uid_by_fd(i); \
			if (uid == 0) continue; \
			err += set_ ## param ## _uid(uid, param); \
		} \
		if (err > 0) return 1; \
		return 0; \
	}

SET_SELF_ALL(int, rate)

int set_rate_uid(int uid, int rate)
{
	TFDSetElement *settings;

	if ((rate > 100) || (rate < -100))
		return 1;

	settings = get_client_settings_by_uid(uid);
	if (settings == NULL)
		return 1;

	settings->msg_settings.rate = rate;
	return 0;
}

SET_SELF_ALL(int, pitch)

int set_pitch_uid(int uid, int pitch)
{
	TFDSetElement *settings;

	if ((pitch > 100) || (pitch < -100))
		return 1;

	settings = get_client_settings_by_uid(uid);
	if (settings == NULL)
		return 1;

	settings->msg_settings.pitch = pitch;
	return 0;
}

SET_SELF_ALL(int, pitch_range)

int set_pitch_range_uid(int uid, int pitch_range)
{
	TFDSetElement *settings;

	if ((pitch_range > 100) || (pitch_range < -100))
		return 1;

	settings = get_client_settings_by_uid(uid);
	if (settings == NULL)
		return 1;

	settings->msg_settings.pitch_range = pitch_range;
	return 0;
}

SET_SELF_ALL(int, volume)

int set_volume_uid(int uid, int volume)
{
	TFDSetElement *settings;

	if ((volume > 100) || (volume < -100))
		return 1;

	settings = get_client_settings_by_uid(uid);
	if (settings == NULL)
		return 1;

	settings->msg_settings.volume = volume;
	return 0;
}

SET_SELF_ALL(char *, voice)

int set_voice_uid(int uid, char *voice)
{
	TFDSetElement *settings;

	settings = get_client_settings_by_uid(uid);
	if (settings == NULL)
		return 1;

	if (!strcmp(voice, "male1"))
		settings->msg_settings.voice_type = SPD_MALE1;
	else if (!strcmp(voice, "male2"))
		settings->msg_settings.voice_type = SPD_MALE2;
	else if (!strcmp(voice, "male3"))
		settings->msg_settings.voice_type = SPD_MALE3;
	else if (!strcmp(voice, "female1"))
		settings->msg_settings.voice_type = SPD_FEMALE1;
	else if (!strcmp(voice, "female2"))
		settings->msg_settings.voice_type = SPD_FEMALE2;
	else if (!strcmp(voice, "female3"))
		settings->msg_settings.voice_type = SPD_FEMALE3;
	else if (!strcmp(voice, "child_male"))
		settings->msg_settings.voice_type = SPD_CHILD_MALE;
	else if (!strcmp(voice, "child_female"))
		settings->msg_settings.voice_type = SPD_CHILD_FEMALE;
	else
		return 1;

	if (settings->msg_settings.voice.name != NULL) {
		g_free(settings->msg_settings.voice.name);
		settings->msg_settings.voice.name = NULL;
	}
	return 0;
}

SET_SELF_ALL(SPDPunctuation, punctuation_mode)

int set_punctuation_mode_uid(int uid, SPDPunctuation punctuation)
{
	TFDSetElement *settings;

	settings = get_client_settings_by_uid(uid);
	if (settings == NULL)
		return 1;

	settings->msg_settings.punctuation_mode = punctuation;
	return 0;
}

#define SET_PARAM_STR(name) \
	settings->name = set_param_str(settings->name, name);

SET_SELF_ALL(SPDCapitalLetters, capital_letter_recognition)

int set_capital_letter_recognition_uid(int uid, SPDCapitalLetters recogn)
{
	TFDSetElement *settings;

	settings = get_client_settings_by_uid(uid);
	if (settings == NULL)
		return 1;

	settings->msg_settings.cap_let_recogn = recogn;
	return 0;
}

SET_SELF_ALL(SPDSpelling, spelling)

int set_spelling_uid(int uid, SPDSpelling spelling)
{
	TFDSetElement *settings;

	assert((spelling == 0) || (spelling == 1));

	settings = get_client_settings_by_uid(uid);
	if (settings == NULL)
		return 1;

	settings->msg_settings.spelling_mode = spelling;
	return 0;
}

SET_SELF_ALL(char *, language)

int set_language_uid(int uid, char *language)
{
	TFDSetElement *settings;
	char *output_module;

	settings = get_client_settings_by_uid(uid);
	if (settings == NULL)
		return 1;

	settings->msg_settings.voice.language =
	    set_param_str(settings->msg_settings.voice.language, language);

	/* Check if it is not desired to change output module */
	output_module = g_hash_table_lookup(language_default_modules, language);
	if (output_module != NULL) {
		set_output_module_uid(uid, output_module);
	} else {
		char *dash = strchr(language, '-');

		if (dash) {
			*dash = 0;

			output_module = g_hash_table_lookup(language_default_modules, language);
			if (output_module != NULL) {
				set_output_module_uid(uid, output_module);
			}
		}
	}

	return 0;
}

SET_SELF_ALL(char *, synthesis_voice)

int set_synthesis_voice_uid(int uid, char *synthesis_voice)
{
	TFDSetElement *settings;

	settings = get_client_settings_by_uid(uid);
	if (settings == NULL)
		return 1;

	settings->msg_settings.voice.name =
	    set_param_str(settings->msg_settings.voice.name, synthesis_voice);

	/* Delete ordinary voice settings so that we don't mix */
	settings->msg_settings.voice_type = -1;

	return 0;
}

#define CHECK_SET_PAR(name, ival) \
	if (cl_set->val.name != ival){ set->name = cl_set->val.name; \
		MSG(4,"parameter " #name " set to %d", cl_set->val.name); }
#define CHECK_SET_PAR_STR(name) \
	if (cl_set->val.name != NULL){ \
		g_free(set->name); \
		set->name = g_strdup(cl_set->val.name); \
		MSG(4,"parameter " #name " set to %s", cl_set->val.name); \
	}

void update_cl_settings(gpointer data, gpointer user_data)
{
	TFDSetClientSpecific *cl_set = data;
	TFDSetElement *set = user_data;

	MSG(4, "Updating client specific settings %s against %s",
	    set->client_name, cl_set->pattern);

	if (fnmatch(cl_set->pattern, set->client_name, 0))
		return;

	/*  Warning: If you modify this, you must also modify cb_BeginClient in config.c ! */
	CHECK_SET_PAR(msg_settings.rate, -101)
	    CHECK_SET_PAR(msg_settings.pitch, -101)
	    CHECK_SET_PAR(msg_settings.pitch_range, -101)
	    CHECK_SET_PAR(msg_settings.volume, -101)
	    CHECK_SET_PAR(msg_settings.punctuation_mode, -1)
	    CHECK_SET_PAR(msg_settings.spelling_mode, -1)
	    CHECK_SET_PAR(msg_settings.voice_type, -1)
	    CHECK_SET_PAR(msg_settings.cap_let_recogn, -1)
	    CHECK_SET_PAR(pause_context, -1)
	    CHECK_SET_PAR(ssml_mode, -1)
	    CHECK_SET_PAR(symbols_preprocessing, -1)
	    CHECK_SET_PAR_STR(msg_settings.voice.language)
	    CHECK_SET_PAR_STR(output_module)

	    return;
}

#undef CHECK_SET_PAR
#undef CHECK_SET_PAR_STR

int set_client_name_self(int fd, char *client_name)
{
	TFDSetElement *settings;
	int dividers = 0;
	int i;

	assert(client_name != NULL);

	settings = get_client_settings_by_fd(fd);
	if (settings == NULL)
		return 1;

	/* Is the parameter a valid client name? */
	for (i = 0; i <= strlen(client_name) - 1; i++)
		if (client_name[i] == ':')
			dividers++;
	if (dividers != 2)
		return 1;

	SET_PARAM_STR(client_name);

	/* Update fd_set for this cilent with client-specific options */
	g_list_foreach(client_specific_settings, update_cl_settings, settings);

	return 0;
}

SET_SELF_ALL(char *, output_module)

int set_output_module_uid(int uid, char *output_module)
{
	TFDSetElement *settings;

	settings = get_client_settings_by_uid(uid);
	if (settings == NULL)
		return 1;
	if (output_module == NULL)
		return 1;

	MSG(5, "Setting output module to %s", output_module);

	MSG(5, "In set_output_module the desired output module is x%s",
	    output_module);

	SET_PARAM_STR(output_module);

	/* Delete synth_voice since it is module specific */
	if (settings->msg_settings.voice.name != NULL) {
		g_free(settings->msg_settings.voice.name);
		settings->msg_settings.voice.name = NULL;
	}

	return 0;
}

SET_SELF_ALL(int, pause_context)

int set_pause_context_uid(int uid, int pause_context)
{
	TFDSetElement *settings;

	settings = get_client_settings_by_uid(uid);
	if (settings == NULL)
		return 1;

	settings->pause_context = pause_context;
	return 0;
}

SET_SELF_ALL(SPDDataMode, ssml_mode)

int set_ssml_mode_uid(int uid, SPDDataMode ssml_mode)
{
	TFDSetElement *settings;

	settings = get_client_settings_by_uid(uid);
	if (settings == NULL)
		return 1;

	settings->ssml_mode = ssml_mode;
	return 0;
}

SET_SELF_ALL(gboolean, symbols_preprocessing)

int set_symbols_preprocessing_uid(int uid, gboolean symbols_preprocessing)
{
	TFDSetElement *settings;

	settings = get_client_settings_by_uid(uid);
	if (settings == NULL)
		return 1;

	settings->symbols_preprocessing = symbols_preprocessing;
	return 0;
}

SET_SELF_ALL(int, debug)

int set_debug_uid(int uid, int debug)
{
	char *debug_logfile_path;

	/* Do not switch debugging on when already on
	   and vice-versa */
	if (SpeechdOptions.debug && debug)
		return 1;
	if (!SpeechdOptions.debug && !debug)
		return 1;

	if (debug) {
		debug_logfile_path =
		    g_strdup_printf("%s/speech-dispatcher.log",
				    SpeechdOptions.debug_destination);

		debug_logfile = fopen(debug_logfile_path, "w");
		if (debug_logfile == NULL) {
			MSG(3,
			    "Error: can't open additional debug logging file %s!\n",
			    debug_logfile_path);
			return 1;
		}
		SpeechdOptions.debug = debug;

		g_free(debug_logfile_path);

		/* Redirecting debugging for all output modules */
		speechd_modules_debug();
	} else {
		SpeechdOptions.debug = 0;
		speechd_modules_nodebug();
		fclose(debug_logfile);
	}
	return 0;
}

#define SET_NOTIFICATION_STATE(state) \
	if (val) \
		settings->notification = settings->notification | SPD_ ## state; \
	else \
		settings->notification = settings->notification & (~ SPD_ ## state);

int set_notification_self(int fd, char *type, int val)
{
	TFDSetElement *settings;
	int uid;

	uid = get_client_uid_by_fd(fd);
	if (uid == 0)
		return 1;

	settings = get_client_settings_by_uid(uid);
	if (settings == NULL)
		return 1;

	if (!strcmp(type, "begin")) {
		SET_NOTIFICATION_STATE(BEGIN);
	} else if (!strcmp(type, "end")) {
		SET_NOTIFICATION_STATE(END);
	} else if (!strcmp(type, "index_marks")) {
		SET_NOTIFICATION_STATE(INDEX_MARKS);
	} else if (!strcmp(type, "pause")) {
		SET_NOTIFICATION_STATE(PAUSE);
	} else if (!strcmp(type, "resume")) {
		SET_NOTIFICATION_STATE(RESUME);
	} else if (!strcmp(type, "cancel")) {
		SET_NOTIFICATION_STATE(CANCEL);
	} else if (!strcmp(type, "all")) {
		SET_NOTIFICATION_STATE(END);
		SET_NOTIFICATION_STATE(BEGIN);
		SET_NOTIFICATION_STATE(INDEX_MARKS);
		SET_NOTIFICATION_STATE(CANCEL);
		SET_NOTIFICATION_STATE(PAUSE);
		SET_NOTIFICATION_STATE(RESUME);
	} else
		return 1;

	return 0;

}

TFDSetElement *default_fd_set(void)
{
	TFDSetElement *new;

	new = (TFDSetElement *) g_malloc(sizeof(TFDSetElement));

	new->paused = 0;

	/* Fill with the global settings values */
	/* We can't use global_fdset copy as this
	   returns static structure and we need dynamic */
	new->priority = GlobalFDSet.priority;
	new->msg_settings.punctuation_mode =
	    GlobalFDSet.msg_settings.punctuation_mode;
	new->msg_settings.rate = GlobalFDSet.msg_settings.rate;
	new->msg_settings.pitch = GlobalFDSet.msg_settings.pitch;
	new->msg_settings.pitch_range = GlobalFDSet.msg_settings.pitch_range;
	new->msg_settings.volume = GlobalFDSet.msg_settings.volume;
	new->msg_settings.voice.language =
	    g_strdup(GlobalFDSet.msg_settings.voice.language);
	new->output_module = g_strdup(GlobalFDSet.output_module);
	new->client_name = g_strdup(GlobalFDSet.client_name);
	new->index_mark = g_strdup(GlobalFDSet.index_mark);
	new->audio_output_method = g_strdup(GlobalFDSet.audio_output_method);
	new->audio_oss_device = g_strdup(GlobalFDSet.audio_oss_device);
	new->audio_alsa_device = g_strdup(GlobalFDSet.audio_alsa_device);
	new->audio_nas_server = g_strdup(GlobalFDSet.audio_nas_server);
	new->audio_pulse_server = g_strdup(GlobalFDSet.audio_pulse_server);

	new->msg_settings.voice_type = GlobalFDSet.msg_settings.voice_type;
	new->msg_settings.voice.name = NULL;
	new->msg_settings.spelling_mode =
	    GlobalFDSet.msg_settings.spelling_mode;
	new->msg_settings.cap_let_recogn =
	    GlobalFDSet.msg_settings.cap_let_recogn;

	new->pause_context = GlobalFDSet.pause_context;
	new->ssml_mode = GlobalFDSet.ssml_mode;
	new->symbols_preprocessing = GlobalFDSet.symbols_preprocessing;
	new->notification = GlobalFDSet.notification;

	new->active = 1;
	new->hist_cur_uid = -1;
	new->hist_cur_pos = -1;
	new->hist_sorted = 0;
	new->index_mark = NULL;
	new->paused_while_speaking = 0;

	return (new);
}

int get_client_uid_by_fd(int fd)
{
	int *uid;
	if (fd <= 0)
		return 0;
	uid = g_hash_table_lookup(fd_uid, &fd);
	if (uid == NULL)
		return 0;
	return *uid;
}

TFDSetElement *get_client_settings_by_fd(int fd)
{
	TFDSetElement *settings;
	int uid;

	uid = get_client_uid_by_fd(fd);
	if (uid == 0)
		return NULL;

	settings = g_hash_table_lookup(fd_settings, &uid);
	return settings;
}

TFDSetElement *get_client_settings_by_uid(int uid)
{
	TFDSetElement *element;

	if (uid < 0)
		return NULL;

	element = g_hash_table_lookup(fd_settings, &uid);
	return element;
}

void remove_client_settings_by_uid(int uid)
{
	TFDSetElement *element;
	assert(uid > 0);
	element = (TFDSetElement *) g_hash_table_lookup(fd_settings, &uid);
	if (element) {
		mem_free_fdset(element);
		g_hash_table_remove(fd_settings, &uid);
		g_free(element);
	} else {
		MSG(5, "Warning: FDSet element to be removed not found");
	}
}

char *set_param_str(char *parameter, char *value)
{
	char *new;

	if (value == NULL) {
		new = NULL;
		return new;
	}

	new = g_realloc(parameter, (strlen(value) + 1) * sizeof(char));
	strcpy(new, value);

	return new;
}
