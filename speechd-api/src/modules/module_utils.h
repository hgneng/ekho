/*
 * module_utils.h - Module utilities
 *           Functions to help writing output modules for Speech Dispatcher
 * Copyright (C) 2003, 2004, 2007 Brailcom, o.p.s.
 *
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1, or (at your option) any later
 * version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * $Id: module_utils.h,v 1.22 2008-06-27 12:29:21 hanke Exp $
 */

#ifndef __MODULE_UTILS_H
#define __MODULE_UTILS_H

#include <dotconf.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <glib.h>
#include <pthread.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <errno.h>
#include <dotconf.h>

#include <sys/ipc.h>

#include <speechd_types.h>
#include "spd_audio.h"

typedef struct SPDMarks {
	unsigned num;
	unsigned allocated;
	unsigned *samples;
	char **names;
	gboolean stop;
} SPDMarks;

extern int log_level;

extern AudioID *module_audio_id;

extern SPDMsgSettings msg_settings;
extern SPDMsgSettings msg_settings_old;

extern int current_index_mark;

extern int Debug;
extern FILE *CustomDebugFile;

extern configfile_t *configfile;
extern configoption_t *module_dc_options;
extern int module_num_dc_options;

extern const char *module_name;

#define CLEAN_OLD_SETTINGS_TABLE() do { \
	msg_settings_old.rate = -101;\
	msg_settings_old.pitch = -101;\
	msg_settings_old.pitch_range = -101;\
	msg_settings_old.volume = -101;\
	msg_settings_old.punctuation_mode = (SPDPunctuation) -1;\
	msg_settings_old.spelling_mode = (SPDSpelling) -1;\
	msg_settings_old.cap_let_recogn = (SPDCapitalLetters) -1;\
	msg_settings_old.voice_type = (SPDVoiceType) -1;\
	msg_settings_old.voice.name = NULL;\
	msg_settings_old.voice.language = NULL; \
} while (0)

#define INIT_SETTINGS_TABLES() do { \
	module_name = MODULE_NAME; \
	module_dc_options = NULL;\
	msg_settings.rate = 0;\
	msg_settings.pitch = 0;\
	msg_settings.pitch_range = 0;\
	msg_settings.volume = 0;\
	msg_settings.punctuation_mode = SPD_PUNCT_NONE;\
	msg_settings.spelling_mode = SPD_SPELL_OFF;\
	msg_settings.cap_let_recogn = SPD_CAP_NONE;\
	msg_settings.voice_type = SPD_MALE1;\
	msg_settings.voice.name = NULL;\
	msg_settings.voice.language = NULL;\
	CLEAN_OLD_SETTINGS_TABLE(); \
} while (0)

#define DBG(arg...) do { \
	if (Debug){ \
		time_t t; \
		struct timeval tv; \
		char *tstr; \
		t = time(NULL); \
		tstr = g_strdup(ctime(&t)); \
		tstr[strlen(tstr)-1] = 0; \
		gettimeofday(&tv,NULL); \
		fprintf(stderr," %s [%d]",tstr, (int) tv.tv_usec); \
		fprintf(stderr, ": "); \
		fprintf(stderr, arg); \
		fprintf(stderr, "\n"); \
		fflush(stderr); \
		if ((Debug==2) || (Debug==3)){ \
			fprintf(CustomDebugFile," %s [%d]",tstr, (int) tv.tv_usec);	\
			fprintf(CustomDebugFile, ": ");					\
			fprintf(CustomDebugFile, arg);					\
			fprintf(CustomDebugFile, "\n");                                   \
			fflush(CustomDebugFile);			\
		} \
		g_free(tstr); \
	} \
} while(0)

#define FATAL(msg) do { \
		fprintf(stderr, "FATAL ERROR in output module [%s:%d]:\n   "msg, \
		        __FILE__, __LINE__); \
		if (Debug > 1) \
			fprintf(CustomDebugFile, "FATAL ERROR in output module [%s:%d]:\n   "msg,	\
			        __FILE__, __LINE__); \
		exit(EXIT_FAILURE); \
} while (0)

int module_load(void);
int module_init(char **status_info);
SPDVoice **module_list_voices(void);
int module_speak(char *data, size_t bytes, SPDMessageType msgtype);
int module_stop(void);
SPDVoice **module_get_voices(void);
int module_tts_output(AudioTrack track, AudioFormat format);
int module_play_file(const char *filename);
int module_marks_init(SPDMarks *marks);
int module_marks_add(SPDMarks *marks, unsigned sample, const char *name);
int module_tts_output_marks(AudioTrack track, AudioFormat format, SPDMarks *marks);
int module_marks_stop(SPDMarks *marks);
int module_marks_clear(SPDMarks *marks);
size_t module_pause(void);
char *module_is_speaking(void);
int module_close(void);
SPDVoice **module_list_registered_voices(void);

#define UPDATE_PARAMETER(value, setter) do { \
	if (msg_settings_old.value != msg_settings.value) \
	{ \
		msg_settings_old.value = msg_settings.value; \
		setter (msg_settings.value); \
	} \
} while (0)

#define UPDATE_STRING_PARAMETER(value, setter) do { \
	if (msg_settings_old.value == NULL || msg_settings.value == NULL \
	        || strcmp (msg_settings_old.value, msg_settings.value)) \
	{ \
		if (msg_settings_old.value != NULL) \
		{ \
			g_free (msg_settings_old.value); \
			msg_settings_old.value = NULL; \
		} \
		if (msg_settings.value != NULL) \
		{ \
			msg_settings_old.value = g_strdup (msg_settings.value); \
			setter (msg_settings.value); \
		} \
	} \
} while (0)

#define CHILD_SAMPLE_BUF_SIZE 16384

typedef struct {
	int pc[2];
	int cp[2];
} TModuleDoublePipe;

int module_get_message_part(const char *message, char *part,
			    unsigned int *pos, size_t maxlen,
			    const char *dividers);

void set_speaking_thread_parameters();

void module_parent_dp_init(TModuleDoublePipe dpipe);
void module_child_dp_init(TModuleDoublePipe dpipe);
void module_parent_dp_close(TModuleDoublePipe dpipe);
void module_child_dp_close(TModuleDoublePipe dpipe);

void module_child_dp_write(TModuleDoublePipe dpipe, const char *msg,
			   size_t bytes);
int module_parent_dp_write(TModuleDoublePipe dpipe, const char *msg,
			   size_t bytes);
int module_parent_dp_read(TModuleDoublePipe dpipe, char *msg, size_t maxlen);
int module_child_dp_read(TModuleDoublePipe dpipe, char *msg, size_t maxlen);

void module_signal_end(void);

void module_strip_punctuation_default(char *buf);
void module_strip_punctuation_some(char *buf, char *punct_some);
char *module_strip_ssml(char *buf);

void module_sigblockall(void);
void module_sigblockusr(sigset_t * signal_set);
void module_sigunblockusr(sigset_t * signal_set);

char *do_message(SPDMessageType msgtype);
char *do_speak(void);
char *do_sound_icon(void);
char *do_char(void);
char *do_key(void);
void do_stop(void);
void do_pause(void);
char *do_list_voices(void);
char *do_set(void);
char *do_audio(void);
char *do_loglevel(void);
char *do_debug(char *cmd_buf);
void do_quit(void);

size_t module_parent_wfork(TModuleDoublePipe dpipe, const char *message,
			   SPDMessageType msgtype, const size_t maxlen,
			   const char *dividers, int *pause_requested);

int module_parent_wait_continue(TModuleDoublePipe dpipe);

void set_speaking_thread_parameters();
int module_terminate_thread(pthread_t thread);
char *module_recode_to_iso(char *data, int bytes, char *language,
			   char *fallback);
void module_signal_end(void);
configoption_t *module_add_config_option(configoption_t * options,
					 int *num_options, const char *name, int type,
					 dotconf_callback_t callback,
					 info_t * info, unsigned long context);

configoption_t *add_config_option(configoption_t * options,
				  int *num_config_options, const char *name, int type,
				  dotconf_callback_t callback, info_t * info,
				  unsigned long context);

/* --- MODULE DOTCONF OPTIONS DEFINITION AND REGISTRATION --- */

#define MOD_OPTION_1_STR(name) \
	static char *name = NULL; \
	DOTCONF_CB(name ## _cb) \
	{ \
		if (cmd->data.str != NULL) { \
			g_free(name); \
			name = g_strdup(cmd->data.str); \
		} \
		return NULL; \
	}

#define MOD_OPTION_1_INT(name) \
	static int name; \
	DOTCONF_CB(name ## _cb) \
	{ \
		name = cmd->data.value; \
		return NULL; \
	}

/* TODO: Switch this to real float, not /100 integer,
   as soon as DotConf supports floats */
#define MOD_OPTION_1_FLOAT(name) \
	static float name; \
	DOTCONF_CB(name ## _cb) \
	{ \
		name = ((float) cmd->data.value) / ((float) 100); \
		return NULL; \
	}

#define MOD_OPTION_2(name, arg1, arg2) \
	typedef struct{ \
		char* arg1; \
		char* arg2; \
	}T ## name; \
	T ## name name; \
	\
	DOTCONF_CB(name ## _cb) \
	{ \
		if (cmd->data.list[0] != NULL) \
			name.arg1 = g_strdup(cmd->data.list[0]); \
		if (cmd->data.list[1] != NULL) \
			name.arg2 = g_strdup(cmd->data.list[1]); \
		return NULL; \
	}

#define MOD_OPTION_2_HT(name, arg1, arg2) \
	typedef struct{ \
		char* arg1; \
		char* arg2; \
	}T ## name; \
	GHashTable *name; \
	\
	DOTCONF_CB(name ## _cb) \
	{ \
		T ## name *new_item; \
		char* new_key; \
		new_item = (T ## name *) g_malloc(sizeof(T ## name)); \
		if (cmd->data.list[0] == NULL) return NULL; \
		new_item->arg1 = g_strdup(cmd->data.list[0]); \
		new_key = g_strdup(cmd->data.list[0]); \
		if (cmd->data.list[1] != NULL) \
			new_item->arg2 = g_strdup(cmd->data.list[1]); \
		else \
			new_item->arg2 = NULL; \
		g_hash_table_insert(name, new_key, new_item); \
		return NULL; \
	}

#define MOD_OPTION_3_HT(name, arg1, arg2, arg3) \
	typedef struct{ \
		char* arg1; \
		char* arg2; \
		char *arg3; \
	}T ## name; \
	GHashTable *name; \
	\
	DOTCONF_CB(name ## _cb) \
	{ \
		T ## name *new_item; \
		char* new_key; \
		new_item = (T ## name *) g_malloc(sizeof(T ## name)); \
		if (cmd->data.list[0] == NULL) return NULL; \
		new_item->arg1 = g_strdup(cmd->data.list[0]); \
		new_key = g_strdup(cmd->data.list[0]); \
		if (cmd->data.list[1] != NULL) \
			new_item->arg2 = g_strdup(cmd->data.list[1]); \
		else \
			new_item->arg2 = NULL; \
		if (cmd->data.list[2] != NULL) \
			new_item->arg3 = g_strdup(cmd->data.list[2]); \
		else \
			new_item->arg3 = NULL; \
		g_hash_table_insert(name, new_key, new_item); \
		return NULL; \
	}

#define MOD_OPTION_1_STR_REG(name, default) do { \
	if (default != NULL) name = g_strdup(default); \
	else name = NULL; \
	module_dc_options = module_add_config_option(module_dc_options, \
	                    &module_num_dc_options, #name, \
	                    ARG_STR, name ## _cb, NULL, 0); \
} while (0)

#define MOD_OPTION_1_INT_REG(name, default) do { \
	name = default; \
	module_dc_options = module_add_config_option(module_dc_options, \
	                    &module_num_dc_options, #name, \
	                    ARG_INT, name ## _cb, NULL, 0); \
} while (0)

/* TODO: Switch this to real float, not /100 integer,
   as soon as DotConf supports floats */
#define MOD_OPTION_1_FLOAT_REG(name, default) do { \
	name = default; \
	module_dc_options = module_add_config_option(module_dc_options, \
	                    &module_num_dc_options, #name, \
	                    ARG_INT, name ## _cb, NULL, 0); \
} while (0)

#define MOD_OPTION_MORE_REG(name) do { \
	module_dc_options = module_add_config_option(module_dc_options, \
	                    &module_num_dc_options, #name, \
	                    ARG_LIST, name ## _cb, NULL, 0); \
} while (0)

#define MOD_OPTION_HT_REG(name) do { \
	name = g_hash_table_new(g_str_hash, g_str_equal); \
	module_dc_options = module_add_config_option(module_dc_options, \
	                    &module_num_dc_options, #name, \
	                    ARG_LIST, name ## _cb, NULL, 0); \
} while (0)

/* --- DEBUGGING SUPPORT  ---*/

#define DECLARE_DEBUG() \
	DOTCONF_CB(Debug ## _cb) \
	{ \
		Debug = cmd->data.value; \
		return NULL; \
	}

#define REGISTER_DEBUG() \
	MOD_OPTION_1_INT_REG(Debug, 0); \

	/* --- INDEX MARKING --- */

#define INDEX_MARK_BODY_LEN 6
#define INDEX_MARK_BODY "__spd_"

extern char *module_index_mark;

	/* This macro must be placed at the initialization of the module so that the
	   later functions are possible to use */

#define INIT_INDEX_MARKING() module_index_mark = NULL;

void module_report_index_mark(char *mark);
void module_report_event_begin(void);
void module_report_event_end(void);
void module_report_event_stop(void);
void module_report_event_pause(void);

extern pthread_mutex_t module_stdout_mutex;

int module_utils_init(void);
int module_audio_init(char **status_info);

	/* Prototypes from module_utils_addvoice.c */
void module_register_available_voices(void);
void module_register_settings_voices(void);
char *module_getvoice(char *language, SPDVoiceType voice);
gboolean module_existsvoice(char *voicename);

#endif /* #ifndef __MODULE_UTILS_H */
