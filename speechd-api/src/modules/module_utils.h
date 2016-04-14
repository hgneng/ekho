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
 * along with this package; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA.
 *
 * $Id: module_utils.h,v 1.22 2008-06-27 12:29:21 hanke Exp $
 */

#ifndef __MODULE_UTILS_H
#define __MODULE_UTILS_H

#include <semaphore.h>
#include <dotconf.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <glib.h>
#include <pthread.h>
#include <signal.h>
#include <semaphore.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <errno.h>
#include <dotconf.h>

#include <semaphore.h>
#include <sys/ipc.h>
#include <sys/sem.h>

#include "spd_audio.h"

typedef struct{
  char* audio_output_method;
  char* audio_oss_device;
  char* audio_alsa_device;
  char* audio_nas_server;
  char* audio_pulse_server;
  int audio_pulse_min_length;
}SPDAudioSettings;

//int log_level;

AudioID *module_audio_id;
AudioOutputType module_audio_output_method;
char* module_audio_pars[10];


SPDMsgSettings msg_settings;
SPDAudioSettings audio_settings;
SPDMsgSettings msg_settings_old;

int current_index_mark;

int Debug;
FILE *CustomDebugFile;

configfile_t *configfile;
configoption_t *module_dc_options;
int module_num_dc_options;


#define CLEAN_OLD_SETTINGS_TABLE()\
 msg_settings_old.rate = -101;\
 msg_settings_old.pitch = -101;\
 msg_settings_old.volume = -101;\
 msg_settings_old.punctuation_mode = -1;\
 msg_settings_old.spelling_mode = -1;\
 msg_settings_old.cap_let_recogn = -1;\
 msg_settings_old.language = NULL;	\
 msg_settings_old.voice = NO_VOICE;\
 msg_settings_old.synthesis_voice = NULL;

#define INIT_SETTINGS_TABLES()\
 module_dc_options = NULL;\
 msg_settings.rate = 0;\
 msg_settings.pitch = 0;\
 msg_settings.volume = 0;\
 msg_settings.punctuation_mode = PUNCT_NONE;\
 msg_settings.spelling_mode = SPELLING_OFF;\
 msg_settings.cap_let_recogn = RECOGN_NONE;\
 msg_settings.language = NULL;\
 msg_settings.voice = MALE1;\
 msg_settings.synthesis_voice = NULL;\
 CLEAN_OLD_SETTINGS_TABLE()

#define DBG(arg...) \
  if (Debug){ \
    time_t t; \
    struct timeval tv; \
    char *tstr; \
    t = time(NULL); \
    tstr = strdup(ctime(&t)); \
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
   xfree(tstr); \
  }

#define FATAL(msg) { \
     fprintf(stderr, "FATAL ERROR in output module [%s:%d]:\n   "msg, \
             __FILE__, __LINE__); \
     if (Debug > 1) \
       fprintf(CustomDebugFile, "FATAL ERROR in output module [%s:%d]:\n   "msg,	\
             __FILE__, __LINE__); \
     exit(EXIT_FAILURE); \
   }

int     module_load         (void);
int     module_init         (char **status_info);
int     module_audio_init_spd     (char **status_info);
VoiceDescription** module_list_voices(void);
int     module_speak        (char *data, size_t bytes, EMessageType msgtype);
int     module_stop         (void);
VoiceDescription**     module_get_voices   (void);
size_t  module_pause        (void);
char*     module_is_speaking  (void);
void    module_close        (int status);

#define UPDATE_PARAMETER(value, setter) \
  if (msg_settings_old.value != msg_settings.value) \
    { \
      msg_settings_old.value = msg_settings.value; \
      setter (msg_settings.value); \
    }

#define UPDATE_STRING_PARAMETER(value, setter) \
  if (msg_settings_old.value == NULL || msg_settings.value == NULL \
         || strcmp (msg_settings_old.value, msg_settings.value)) \
    { \
      if (msg_settings_old.value != NULL) \
      { \
        xfree (msg_settings_old.value); \
	msg_settings_old.value = NULL; \
      } \
      if (msg_settings.value != NULL) \
      { \
        msg_settings_old.value = g_strdup (msg_settings.value); \
        setter (msg_settings.value); \
      } \
    }

#define CHILD_SAMPLE_BUF_SIZE 16384

typedef struct{
    int pc[2];
    int cp[2];
}TModuleDoublePipe;

void* xmalloc(size_t size);
void* xrealloc(void* data, size_t size);
void xfree(void* data);

int module_get_message_part(const char* message, char* part,
			    unsigned int *pos, size_t maxlen,
			    const char* dividers);

void set_speaking_thread_parameters();

void module_parent_dp_init(TModuleDoublePipe dpipe);
void module_child_dp_init(TModuleDoublePipe dpipe);
void module_parent_dp_close(TModuleDoublePipe dpipe);
void module_child_dp_close(TModuleDoublePipe dpipe);

void module_child_dp_write(TModuleDoublePipe dpipe, const char *msg, size_t bytes);
int module_parent_dp_write(TModuleDoublePipe dpipe, const char *msg, size_t bytes);
int module_parent_dp_read(TModuleDoublePipe dpipe, char *msg, size_t maxlen);
int module_child_dp_read(TModuleDoublePipe dpipe, char *msg, size_t maxlen);

void module_signal_end(void);

void module_strip_punctuation_default(char *buf);
void module_strip_punctuation_some(char *buf, char* punct_some);
char* module_strip_ssml(char *buf);

void module_sigblockall(void);
void module_sigblockusr(sigset_t *signal_set);
void module_sigunblockusr(sigset_t *signal_set);

char* do_message(EMessageType msgtype);
char* do_speak(void);
char* do_sound_icon(void);
char* do_char(void);
char* do_key(void);
void  do_stop(void);
void  do_pause(void);
char* do_list_voices(void);
char* do_set(void);
char* do_audio(void);
char* do_loglevel(void);
char* do_debug(char *cmd_buf);
void do_quit(void);


typedef void (*TChildFunction)(TModuleDoublePipe dpipe, const size_t maxlen);
typedef size_t (*TParentFunction)(TModuleDoublePipe dpipe, const char* message,
                                  const EMessageType msgtype, const size_t maxlen,
                                  const char* dividers, int *pause_requested);

void module_speak_thread_wfork(sem_t *semaphore, pid_t *process_pid, 
                          TChildFunction child_function,
                          TParentFunction parent_function,
                          int *speaking_flag, char **message, const EMessageType *msgtype,
                          const size_t maxlen, const char *dividers, size_t *module_position,
                          int *pause_requested);

size_t module_parent_wfork(TModuleDoublePipe dpipe, const char* message, EMessageType msgtype,
                    const size_t maxlen, const char* dividers, int *pause_requested);

int module_parent_wait_continue(TModuleDoublePipe dpipe);

void set_speaking_thread_parameters();
int module_write_data_ok(char *data);
int module_terminate_thread(pthread_t thread);
sem_t* module_semaphore_init();
char * module_recode_to_iso(char *data, int bytes, char *language, char *fallback);
int semaphore_post(int sem_id);
void module_signal_end(void);
configoption_t * module_add_config_option(configoption_t *options,
						 int *num_options, char *name, int type,
						 dotconf_callback_t callback, info_t *info,
						 unsigned long context);

void* module_get_ht_option(GHashTable *hash_table, const char *key);

configoption_t * add_config_option(configoption_t *options, int *num_config_options,
				   char *name, int type,
				   dotconf_callback_t callback, info_t *info,
				   unsigned long context);


     
/* --- MODULE DOTCONF OPTIONS DEFINITION AND REGISTRATION --- */

#define MOD_OPTION_1_STR(name) \
    static char *name = NULL; \
    DOTCONF_CB(name ## _cb) \
    { \
        xfree(name); \
        if (cmd->data.str != NULL) \
            name = strdup(cmd->data.str); \
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
            name.arg1 = strdup(cmd->data.list[0]); \
        if (cmd->data.list[1] != NULL) \
            name.arg2 = strdup(cmd->data.list[1]); \
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
        new_item = (T ## name *) malloc(sizeof(T ## name)); \
        if (cmd->data.list[0] == NULL) return NULL; \
        new_item->arg1 = strdup(cmd->data.list[0]); \
        new_key = strdup(cmd->data.list[0]); \
        if (cmd->data.list[1] != NULL) \
           new_item->arg2 = strdup(cmd->data.list[1]); \
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
        new_item = (T ## name *) malloc(sizeof(T ## name)); \
        if (cmd->data.list[0] == NULL) return NULL; \
        new_item->arg1 = strdup(cmd->data.list[0]); \
        new_key = strdup(cmd->data.list[0]); \
        if (cmd->data.list[1] != NULL) \
           new_item->arg2 = strdup(cmd->data.list[1]); \
        else \
            new_item->arg2 = NULL; \
        if (cmd->data.list[2] != NULL) \
           new_item->arg3 = strdup(cmd->data.list[2]); \
        else \
            new_item->arg3 = NULL; \
        g_hash_table_insert(name, new_key, new_item); \
        return NULL; \
    }

#define MOD_OPTION_1_STR_REG(name, default) \
    if (default != NULL) name = strdup(default); \
    else name = NULL; \
    module_dc_options = module_add_config_option(module_dc_options, \
                                     &module_num_dc_options, #name, \
                                     ARG_STR, name ## _cb, NULL, 0); 

#define MOD_OPTION_1_INT_REG(name, default) \
    name = default; \
    module_dc_options = module_add_config_option(module_dc_options, \
                                     &module_num_dc_options, #name, \
                                     ARG_INT, name ## _cb, NULL, 0); 

/* TODO: Switch this to real float, not /100 integer,
   as soon as DotConf supports floats */
#define MOD_OPTION_1_FLOAT_REG(name, default) \
    name = default; \
    module_dc_options = module_add_config_option(module_dc_options, \
                                     &module_num_dc_options, #name, \
                                     ARG_INT, name ## _cb, NULL, 0);

#define MOD_OPTION_MORE_REG(name) \
    module_dc_options = module_add_config_option(module_dc_options, \
                                     &module_num_dc_options, #name, \
                                     ARG_LIST, name ## _cb, NULL, 0);

#define MOD_OPTION_HT_REG(name) \
    name = g_hash_table_new(g_str_hash, g_str_equal); \
    module_dc_options = module_add_config_option(module_dc_options, \
                                     &module_num_dc_options, #name, \
                                     ARG_LIST, name ## _cb, NULL, 0);


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

char *module_index_mark;

/* This macro must be placed at the initialization of the module so that the
   later functions are possible to use */

#define INIT_INDEX_MARKING() module_index_mark = NULL;

void module_report_index_mark(char *mark);
void module_report_event_begin(void);
void module_report_event_end(void);
void module_report_event_stop(void);
void module_report_event_pause(void);



pthread_mutex_t module_stdout_mutex;

int module_utils_init(void);
int module_audio_init_spd(char **status_info);
int module_audio_init(char **status_info);

/* Prototypes from module_utils_addvoice.c */
void module_register_settings_voices(void);
char* module_getvoice(char* language, EVoiceType voice);

#endif /* #ifndef __MODULE_UTILS_H */
