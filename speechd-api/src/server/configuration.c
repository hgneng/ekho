
/*
 * dc_decl.h - Dotconf functions and types for Speech Dispatcher
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
 * $Id: config.c,v 1.18 2009-05-14 08:11:33 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <glib.h>

#include <dotconf.h>

#include "speechd.h"
#include "configuration.h"
#include "symbols.h"
#include <fdsetconv.h>

configoption_t *spd_options;
int spd_num_options;

static TFDSetClientSpecific *cl_spec_section;

/* So that gcc doesn't comply about casts to char* */

/* == CONFIGURATION MANAGEMENT FUNCTIONS */

/* Add dotconf configuration option */
configoption_t *add_config_option(configoption_t * options,
				  int *num_config_options, const char *name, int type,
				  dotconf_callback_t callback, info_t * info,
				  unsigned long context)
{
	configoption_t *opts;

	(*num_config_options)++;
	opts =
	    (configoption_t *) g_realloc(options,
					 (*num_config_options) *
					 sizeof(configoption_t));
	opts[*num_config_options - 1].name = g_strdup(name);
	opts[*num_config_options - 1].type = type;
	opts[*num_config_options - 1].callback = callback;
	opts[*num_config_options - 1].info = info;
	opts[*num_config_options - 1].context = context;
	return opts;
}

/* Free all configuration options. */
void free_config_options(configoption_t * opts, int *num)
{
	int i = 0;

	if (opts == NULL)
		return;

	for (i = 0; i <= (*num) - 1; i++) {
		g_free((char *)opts[i].name);
	}
	g_free(opts);
	*num = 0;
	opts = NULL;
}

/* == CALLBACK DEFINITION MACROS == */

#define GLOBAL_FDSET_OPTION_CB_STR(name, arg) \
	DOTCONF_CB(cb_ ## name) \
	{ \
		assert(cmd->data.str != NULL); \
		if (!cl_spec_section) \
			GlobalFDSet.arg = g_strdup(cmd->data.str); \
		else \
			cl_spec_section->val.arg = g_strdup(cmd->data.str); \
		return NULL; \
	}

#define GLOBAL_FDSET_OPTION_CB_INT(name, arg, cond, str) \
	DOTCONF_CB(cb_ ## name) \
	{ \
		int val = cmd->data.value; \
		if (!(cond)) FATAL(str); \
		if (!cl_spec_section) \
			GlobalFDSet.arg = val; \
		else \
			cl_spec_section->val.arg = val; \
		return NULL; \
	}

#define GLOBAL_FDSET_OPTION_CB_SPECIAL(name, arg, type, fconv) \
	DOTCONF_CB(cb_ ## name) \
	{ \
		char *val_str; \
		type val; \
		val_str = g_ascii_strdown(cmd->data.str, strlen(cmd->data.str)); \
		if (val_str == NULL) FATAL("Invalid parameter in configuration"); \
		val = fconv(val_str); \
		g_free(val_str); \
		if (val == -1) FATAL("Invalid parameter in configuration."); \
		if (!cl_spec_section) \
			GlobalFDSet.arg = val; \
		else \
			cl_spec_section->val.arg = val; \
		return NULL; \
	}

#define SPEECHD_OPTION_CB_STR_M(name, arg) \
	DOTCONF_CB(cb_ ## name) \
	{ \
		if (cl_spec_section) \
			FATAL("This command isn't allowed in a client specific section!"); \
		if (!SpeechdOptions.arg ## _set) SpeechdOptions.arg = g_strdup(cmd->data.str); \
		return NULL; \
	}

#define SPEECHD_OPTION_CB_INT_M(name, arg, cond, str) \
	DOTCONF_CB(cb_ ## name) \
	{ \
		int val = cmd->data.value; \
		if (cl_spec_section) \
			FATAL("This command isn't allowed in a client specific section!"); \
		if (!(cond)) FATAL(str); \
		if (!SpeechdOptions.arg ## _set) SpeechdOptions.arg = val; \
		return NULL; \
	}

#define SPEECHD_OPTION_CB_STR(name, arg) \
	DOTCONF_CB(cb_ ## name) \
	{ \
		if (cl_spec_section) \
			FATAL("This command isn't allowed in a client specific section!"); \
		SpeechdOptions.arg = g_strdup(cmd->data.str); \
		return NULL; \
	}

#define SPEECHD_OPTION_CB_INT(name, arg, cond, str) \
	DOTCONF_CB(cb_ ## name) \
	{ \
		int val = cmd->data.value; \
		if (cl_spec_section) \
			FATAL("This command isn't allowed in a client specific section!"); \
		if (!(cond)) FATAL(str); \
		SpeechdOptions.arg = val; \
		return NULL; \
	}

#define GLOBAL_SET_LOGLEVEL(name, arg, cond, str) \
	DOTCONF_CB(cb_ ## name) \
	{ \
		int val = cmd->data.value; \
		if (cl_spec_section) \
			FATAL("This command isn't allowed in a client specific section!"); \
		if (!(cond)) FATAL(str); \
		if (!SpeechdOptions.arg ## _set){ \
			SpeechdOptions.arg = val; \
			GlobalFDSet.arg = val; \
		} \
		return NULL; \
	}

/* == CALLBACK DEFINITIONS == */
GLOBAL_FDSET_OPTION_CB_STR(DefaultModule, output_module)
    GLOBAL_FDSET_OPTION_CB_STR(DefaultLanguage, msg_settings.voice.language)
    GLOBAL_FDSET_OPTION_CB_STR(DefaultClientName, client_name)

    GLOBAL_FDSET_OPTION_CB_STR(AudioOutputMethod, audio_output_method)
    GLOBAL_FDSET_OPTION_CB_STR(AudioOSSDevice, audio_oss_device)
    GLOBAL_FDSET_OPTION_CB_STR(AudioALSADevice, audio_alsa_device)
    GLOBAL_FDSET_OPTION_CB_STR(AudioNASServer, audio_nas_server)
    GLOBAL_FDSET_OPTION_CB_STR(AudioPulseServer, audio_pulse_server)
    GLOBAL_FDSET_OPTION_CB_STR(AudioPulseDevice, audio_pulse_device)
    GLOBAL_FDSET_OPTION_CB_INT(AudioPulseMinLength, audio_pulse_min_length, 1, "")

    GLOBAL_FDSET_OPTION_CB_INT(DefaultRate, msg_settings.rate, (val >= -100)
			       && (val <= +100), "Rate out of range.")
    GLOBAL_FDSET_OPTION_CB_INT(DefaultPitch, msg_settings.pitch, (val >= -100)
			       && (val <= +100), "Pitch out of range.")
    GLOBAL_FDSET_OPTION_CB_INT(DefaultPitchRange, msg_settings.pitch_range,
			   (val >= -100)
			       && (val <= +100), "Pitch range out of range.")
    GLOBAL_FDSET_OPTION_CB_INT(DefaultVolume, msg_settings.volume, (val >= -100)
			       && (val <= +100), "Volume out of range.")
    GLOBAL_FDSET_OPTION_CB_INT(DefaultSpelling, msg_settings.spelling_mode, 1,
			   "Invalid spelling mode")
    GLOBAL_FDSET_OPTION_CB_INT(DefaultPauseContext, pause_context, 1, "")

    GLOBAL_FDSET_OPTION_CB_SPECIAL(DefaultPriority, priority, SPDPriority,
			       str2intpriority)
    GLOBAL_FDSET_OPTION_CB_SPECIAL(DefaultVoiceType, msg_settings.voice_type,
			       SPDVoiceType, str2EVoice)
    GLOBAL_FDSET_OPTION_CB_SPECIAL(DefaultPunctuationMode,
			       msg_settings.punctuation_mode, SPDPunctuation,
			       str2EPunctMode)
    GLOBAL_FDSET_OPTION_CB_SPECIAL(SymbolsPreproc, symbols_preprocessing,
			       SymLvl,str2SymLvl)
    GLOBAL_FDSET_OPTION_CB_SPECIAL(DefaultCapLetRecognition,
			       msg_settings.cap_let_recogn, SPDCapitalLetters,
			       str2ECapLetRecogn)

    SPEECHD_OPTION_CB_STR_M(CommunicationMethod, communication_method)
    SPEECHD_OPTION_CB_STR_M(SocketPath, socket_path)
    SPEECHD_OPTION_CB_INT_M(Port, port, val >= 0, "Invalid port number!")
    SPEECHD_OPTION_CB_INT_M(LocalhostAccessOnly, localhost_access_only, val >= 0,
			"Invalid access control mode!")
    GLOBAL_SET_LOGLEVEL(LogLevel, log_level, (val >= 0)
			&& (val <= 5), "Invalid log (verbosity) level!")
    SPEECHD_OPTION_CB_INT(MaxHistoryMessages, max_history_messages, val >= 0,
		      "Invalid parameter!")
    SPEECHD_OPTION_CB_INT(MaxQueueSize, max_queue_size, val >= 0,
		      "Invalid parameter!")
    SPEECHD_OPTION_CB_INT_M(Timeout, server_timeout, val >= 0, "Invalid timeout value!")

    DOTCONF_CB(cb_LanguageDefaultModule)
{
	char *key;
	char *value;

	if (cmd->data.list[0] == NULL)
		FATAL("No language specified for LanguageDefaultModule");
	if (cmd->data.list[0] == NULL)
		FATAL("No module specified for LanguageDefaultModule");

	key = g_strdup(cmd->data.list[0]);
	value = g_strdup(cmd->data.list[1]);

	g_hash_table_insert(language_default_modules, key, value);

	return NULL;
}

DOTCONF_CB(cb_LogFile)
{
	/* This option is DEPRECATED. If it is specified, get the directory. */
	assert(cmd->data.str != NULL);
	SpeechdOptions.log_dir = g_path_get_dirname(cmd->data.str);
	logging_init();

	MSG(1,
	    "WARNING: The LogFile option is deprecated. Directory accepted but filename ignored");
	return NULL;
}

DOTCONF_CB(cb_LogDir)
{
	assert(cmd->data.str != NULL);

	if (strcmp(cmd->data.str, "default")
	    && !SpeechdOptions.log_dir_set) {
		// cmd->data.str different from "default"
		SpeechdOptions.log_dir = g_strdup(cmd->data.str);
	}
	logging_init();
	return NULL;
}

DOTCONF_CB(cb_CustomLogFile)
{
	char *kind;
	char *file;

	if (cmd->data.list[0] == NULL)
		FATAL("No log kind specified in CustomLogFile");
	if (cmd->data.list[1] == NULL)
		FATAL("No log file specified in CustomLogFile");
	kind = g_strdup(cmd->data.list[0]);
	assert(kind != NULL);
	file = g_strdup(cmd->data.list[1]);
	assert(file != NULL);

	custom_log_kind = kind;
	if (!strncmp(file, "stdout", 6)) {
		custom_logfile = stdout;
		return NULL;
	}
	if (!strncmp(file, "stderr", 6)) {
		custom_logfile = stderr;
		return NULL;
	}
	custom_logfile = fopen(file, "a");
	if (custom_logfile == NULL) {
		fprintf(stderr,
			"Error: can't open custom log file, using stdout\n");
		custom_logfile = stdout;
	}

	MSG(2, "Speech Dispatcher custom logging to file %s", file);
	return NULL;
}

DOTCONF_CB(cb_SymbolsPreprocFile)
{
	if (cmd->data.list[0] == NULL) {
		MSG(3,
		    "No symbol preprocessing name specified in configuration under SymbolsPreprocFile");
		return NULL;
	}

	symbols_preprocessing_add_file(cmd->data.list[0]);

	return NULL;
}

DOTCONF_CB(cb_AddModule)
{
	if (cmd->data.list[0] == NULL) {
		MSG(3,
		    "No output module name specified in configuration under AddModule");
		return NULL;
	}

	module_add_load_request(g_strdup(cmd->data.list[0]),
				g_strdup(cmd->data.list[1]),
				g_strdup(cmd->data.list[2]),
				g_strdup_printf("%s/%s.log",
						SpeechdOptions.log_dir,
						cmd->data.list[0]),
				NULL,
				NULL);

	return NULL;
}

/* == CLIENT SPECIFIC CONFIGURATION == */

#define SET_PAR(name, value) cl_spec->val.name = value;
#define SET_PAR_STR(name) cl_spec->val.name = NULL;
DOTCONF_CB(cb_BeginClient)
{
	TFDSetClientSpecific *cl_spec;

	if (cl_spec_section != NULL)
		FATAL
		    ("Configuration: Already in client specific section, can't open a new one!");

	if (cmd->data.str == NULL)
		FATAL
		    ("Configuration: You must specify some client's name for BeginClient");

	cl_spec =
	    (TFDSetClientSpecific *) g_malloc(sizeof(TFDSetClientSpecific));
	cl_spec->pattern = g_strdup(cmd->data.str);
	cl_spec_section = cl_spec;

	MSG(4, "Reading configuration for pattern %s", cl_spec->pattern);

	/*  Warning: If you modify this, you must also modify update_cl_settings() in set.c ! */
	SET_PAR(msg_settings.rate, -101)
	    SET_PAR(msg_settings.pitch, -101)
	    SET_PAR(msg_settings.pitch_range, -101)
	    SET_PAR(msg_settings.volume, -101)
	    SET_PAR(msg_settings.punctuation_mode, -1)
	    SET_PAR(msg_settings.spelling_mode, -1)
	    SET_PAR(msg_settings.voice_type, -1)
	    SET_PAR(msg_settings.cap_let_recogn, -1)
	    SET_PAR(pause_context, -1);
	SET_PAR(ssml_mode, -1);
	SET_PAR(symbols_preprocessing, -1);
	SET_PAR_STR(msg_settings.voice.language)
	    SET_PAR_STR(output_module)

	    return NULL;
}

#undef SET_PAR
#undef SET_PAR_STR

DOTCONF_CB(cb_EndClient)
{
	if (cl_spec_section == NULL)
		FATAL
		    ("Configuration: Already outside the client specific section!");

	client_specific_settings =
	    g_list_append(client_specific_settings, cl_spec_section);

	cl_spec_section = NULL;

	return NULL;
}

/* == CALLBACK FOR UNKNOWN OPTIONS == */

DOTCONF_CB(cb_unknown)
{
	MSG(2, "Unknown option in configuration!");
	return NULL;
}

/* == Auto-spawn no-action callback == */
DOTCONF_CB(cb_DisableAutoSpawn)
{
	/* DisableAutoSpawn option is handled earlier during startup, not via the DotConf
	   mechanism. This callback here is a hack to ensure DotConf doesn't complain about
	   an unknown option */
	return NULL;
}

/* == LOAD CALLBACKS == */

#define ADD_CONFIG_OPTION(name, arg_type) \
	options = add_config_option(options, num_options, #name, arg_type, cb_ ## name, 0, 0);

#define ADD_LAST_OPTION() \
	options = add_config_option(options, num_options, "", 0, NULL, NULL, 0);

configoption_t *load_config_options(int *num_options)
{
	configoption_t *options = NULL;

	cl_spec_section = NULL;

	ADD_CONFIG_OPTION(CommunicationMethod, ARG_STR);
	ADD_CONFIG_OPTION(SocketPath, ARG_STR);
	ADD_CONFIG_OPTION(Port, ARG_INT);
	ADD_CONFIG_OPTION(DisableAutoSpawn, ARG_NONE);
	ADD_CONFIG_OPTION(LocalhostAccessOnly, ARG_INT);
	ADD_CONFIG_OPTION(LogFile, ARG_STR);
	ADD_CONFIG_OPTION(LogDir, ARG_STR);
	ADD_CONFIG_OPTION(CustomLogFile, ARG_LIST);
	ADD_CONFIG_OPTION(LogLevel, ARG_INT);
	ADD_CONFIG_OPTION(DefaultModule, ARG_STR);
	ADD_CONFIG_OPTION(LanguageDefaultModule, ARG_LIST);
	ADD_CONFIG_OPTION(DefaultRate, ARG_INT);
	ADD_CONFIG_OPTION(DefaultPitch, ARG_INT);
	ADD_CONFIG_OPTION(DefaultPitchRange, ARG_INT);
	ADD_CONFIG_OPTION(DefaultVolume, ARG_INT);
	ADD_CONFIG_OPTION(DefaultLanguage, ARG_STR);
	ADD_CONFIG_OPTION(DefaultPriority, ARG_STR);
	ADD_CONFIG_OPTION(MaxHistoryMessages, ARG_INT);
	ADD_CONFIG_OPTION(MaxQueueSize, ARG_INT);
	ADD_CONFIG_OPTION(DefaultPunctuationMode, ARG_STR);
	ADD_CONFIG_OPTION(SymbolsPreproc, ARG_STR);
	ADD_CONFIG_OPTION(SymbolsPreprocFile, ARG_STR);
	ADD_CONFIG_OPTION(DefaultClientName, ARG_STR);
	ADD_CONFIG_OPTION(DefaultVoiceType, ARG_STR);
	ADD_CONFIG_OPTION(DefaultSpelling, ARG_TOGGLE);
	ADD_CONFIG_OPTION(DefaultCapLetRecognition, ARG_STR);
	ADD_CONFIG_OPTION(DefaultPauseContext, ARG_INT);
	ADD_CONFIG_OPTION(Timeout, ARG_INT);
	ADD_CONFIG_OPTION(AddModule, ARG_LIST);

	ADD_CONFIG_OPTION(AudioOutputMethod, ARG_STR);
	ADD_CONFIG_OPTION(AudioOSSDevice, ARG_STR);
	ADD_CONFIG_OPTION(AudioALSADevice, ARG_STR);
	ADD_CONFIG_OPTION(AudioNASServer, ARG_STR);
	ADD_CONFIG_OPTION(AudioPulseServer, ARG_STR);
	ADD_CONFIG_OPTION(AudioPulseDevice, ARG_STR);
	ADD_CONFIG_OPTION(AudioPulseMinLength, ARG_INT);

	ADD_CONFIG_OPTION(BeginClient, ARG_STR);
	ADD_CONFIG_OPTION(EndClient, ARG_NONE);
	return options;

}

/* == DEFAULT OPTIONS == */

void load_default_global_set_options()
{
	GlobalFDSet.priority = SPD_MESSAGE;
	GlobalFDSet.msg_settings.punctuation_mode = SPD_PUNCT_NONE;
	GlobalFDSet.symbols_preprocessing = SYMLVL_NONE;
	GlobalFDSet.msg_settings.spelling_mode = 0;
	GlobalFDSet.msg_settings.rate = 0;
	GlobalFDSet.msg_settings.pitch = 0;
	GlobalFDSet.msg_settings.pitch_range = 0;
	GlobalFDSet.msg_settings.volume = 0;
	GlobalFDSet.client_name = g_strdup("unknown:unknown:unknown");
	GlobalFDSet.msg_settings.voice.language = g_strdup("en-US");
	GlobalFDSet.output_module = NULL;
	GlobalFDSet.msg_settings.voice_type = SPD_MALE1;
	GlobalFDSet.msg_settings.cap_let_recogn = SPD_CAP_NONE;
	GlobalFDSet.min_delay_progress = 2000;
	GlobalFDSet.pause_context = 0;
	GlobalFDSet.ssml_mode = SPD_DATA_TEXT;
	GlobalFDSet.notification = 0;

	GlobalFDSet.audio_output_method = g_strdup(DEFAULT_AUDIO_METHOD);
	GlobalFDSet.audio_oss_device = g_strdup("/dev/dsp");
	GlobalFDSet.audio_alsa_device = g_strdup("default");
	GlobalFDSet.audio_nas_server = g_strdup("tcp/localhost:5450");
	GlobalFDSet.audio_pulse_server = g_strdup("default");
	GlobalFDSet.audio_pulse_device = g_strdup("default");
	GlobalFDSet.audio_pulse_min_length = 10;

	SpeechdOptions.max_history_messages = 10000;
	SpeechdOptions.max_queue_size = 10000;

	/* Options which are accessible from command line must be handled
	   specially to make sure we don't overwrite them */
	if (!SpeechdOptions.log_level_set)
		SpeechdOptions.log_level = 3;
	if (!SpeechdOptions.communication_method_set)
		SpeechdOptions.communication_method = g_strdup("unix_socket");
	if (!SpeechdOptions.socket_path_set)
		SpeechdOptions.socket_path = g_strdup("default");
	if (!SpeechdOptions.port_set)
		SpeechdOptions.port = SPEECHD_DEFAULT_PORT;
	if (!SpeechdOptions.localhost_access_only_set)
		SpeechdOptions.localhost_access_only = 1;
	if (!SpeechdOptions.server_timeout_set)
		SpeechdOptions.server_timeout = 5;
	

	logfile = stderr;
	custom_logfile = NULL;
}
