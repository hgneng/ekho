
/*
 * generic.c - Speech Dispatcher generic output module
 *
 * Copyright (C) 2001, 2002, 2003, 2007 Brailcom, o.p.s.
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
 * $Id: generic.c,v 1.30 2008-07-30 09:15:51 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <glib.h>
#include <semaphore.h>

#include <speechd_types.h>

#include "module_utils.h"

#define MODULE_NAME     "generic"
#define MODULE_VERSION  "0.2"

DECLARE_DEBUG()

/* Thread and process control */
static int generic_speaking = 0;

static pthread_t generic_speak_thread;
static pid_t generic_pid;
static sem_t generic_semaphore;

static char *generic_message;
static SPDMessageType generic_message_type;

static int generic_position = 0;
static int generic_pause_requested = 0;

static char *execute_synth_str1;
static char *execute_synth_str2;

/* Internal functions prototypes */
static void *get_ht_option(GHashTable * hash_table, const char *key);
static void *_generic_speak(void *);
static void _generic_child(TModuleDoublePipe dpipe, const size_t maxlen);
static void generic_child_close(TModuleDoublePipe dpipe);

void generic_set_rate(signed int rate);
void generic_set_pitch(signed int pitch);
void generic_set_pitch_range(signed int pitch_range);
void generic_set_voice(SPDVoiceType voice);
void generic_set_synthesis_voice(char *name);
void generic_set_language(char *language);
void generic_set_volume(signed int volume);
void generic_set_punct(SPDPunctuation punct);

/* Fill the module_info structure with pointers to this modules functions */

MOD_OPTION_1_STR(GenericExecuteSynth)
    MOD_OPTION_1_STR(GenericCmdDependency)
    MOD_OPTION_1_INT(GenericPortDependency)
    MOD_OPTION_1_STR(GenericSoundIconFolder)

    MOD_OPTION_1_INT(GenericMaxChunkLength)
    MOD_OPTION_1_STR(GenericDelimiters)
    MOD_OPTION_1_STR(GenericPunctNone)
    MOD_OPTION_1_STR(GenericPunctSome)
    MOD_OPTION_1_STR(GenericPunctMost)
    MOD_OPTION_1_STR(GenericPunctAll)
    MOD_OPTION_1_STR(GenericStripPunctChars)
    MOD_OPTION_1_STR(GenericRecodeFallback)

    MOD_OPTION_1_INT(GenericRateAdd)
    MOD_OPTION_1_FLOAT(GenericRateMultiply)
    MOD_OPTION_1_INT(GenericRateForceInteger)
    MOD_OPTION_1_INT(GenericPitchAdd)
    MOD_OPTION_1_FLOAT(GenericPitchMultiply)
    MOD_OPTION_1_INT(GenericPitchForceInteger)
    MOD_OPTION_1_INT(GenericPitchRangeAdd)
    MOD_OPTION_1_FLOAT(GenericPitchRangeMultiply)
    MOD_OPTION_1_INT(GenericPitchRangeForceInteger)
    MOD_OPTION_1_INT(GenericVolumeAdd)
    MOD_OPTION_1_FLOAT(GenericVolumeMultiply)
    MOD_OPTION_1_INT(GenericVolumeForceInteger)
    MOD_OPTION_3_HT(GenericLanguage, code, name, charset)

static char generic_msg_pitch_str[16];
static char generic_msg_pitch_range_str[16];
static char generic_msg_rate_str[16];
static char generic_msg_volume_str[16];
static char *generic_msg_voice_str = NULL;
static TGenericLanguage *generic_msg_language = NULL;
static char *generic_msg_punct_str;

/* Public functions */
int module_load(void)
{

	INIT_SETTINGS_TABLES();

	MOD_OPTION_1_STR_REG(GenericExecuteSynth, "");
	MOD_OPTION_1_STR_REG(GenericCmdDependency, "");
	MOD_OPTION_1_INT_REG(GenericPortDependency, 0);
	MOD_OPTION_1_STR_REG(GenericSoundIconFolder, "/usr/share/sounds/sound-icons/");

	REGISTER_DEBUG();

	MOD_OPTION_1_INT_REG(GenericMaxChunkLength, 300);
	MOD_OPTION_1_STR_REG(GenericDelimiters, ".");
	MOD_OPTION_1_STR_REG(GenericStripPunctChars, "");
	MOD_OPTION_1_STR_REG(GenericRecodeFallback, "?");

	MOD_OPTION_1_INT_REG(GenericRateAdd, 0);
	MOD_OPTION_1_FLOAT_REG(GenericRateMultiply, 1);
	MOD_OPTION_1_INT_REG(GenericRateForceInteger, 0);

	MOD_OPTION_1_INT_REG(GenericPitchAdd, 0);
	MOD_OPTION_1_FLOAT_REG(GenericPitchMultiply, 1);
	MOD_OPTION_1_INT_REG(GenericPitchForceInteger, 0);

	MOD_OPTION_1_INT_REG(GenericPitchRangeAdd, 0);
	MOD_OPTION_1_FLOAT_REG(GenericPitchRangeMultiply, 1);
	MOD_OPTION_1_INT_REG(GenericPitchRangeForceInteger, 0);

	MOD_OPTION_1_INT_REG(GenericVolumeAdd, 0);
	MOD_OPTION_1_FLOAT_REG(GenericVolumeMultiply, 1);
	MOD_OPTION_1_INT_REG(GenericVolumeForceInteger, 0);

	MOD_OPTION_HT_REG(GenericLanguage);

	MOD_OPTION_1_STR_REG(GenericPunctNone, "");
	MOD_OPTION_1_STR_REG(GenericPunctSome, "");
	MOD_OPTION_1_STR_REG(GenericPunctMost, "");
	MOD_OPTION_1_STR_REG(GenericPunctAll, "");

	module_register_available_voices();
	module_register_settings_voices();

	return 0;
}

int module_init(char **status_info)
{
	int ret;

	*status_info = NULL;

	DBG("GenericMaxChunkLength = %d\n", GenericMaxChunkLength);
	DBG("GenericDelimiters = %s\n", GenericDelimiters);
	DBG("GenericExecuteSynth = %s\n", GenericExecuteSynth);
	DBG("GenericCmdDependency = %s\n", GenericCmdDependency);
	DBG("GenericPortDependency = %u\n", GenericPortDependency);

	generic_msg_language =
	    (TGenericLanguage *) g_malloc(sizeof(TGenericLanguage));
	generic_msg_language->code = g_strdup("en-US");
	generic_msg_language->charset = g_strdup("iso-8859-1");
	generic_msg_language->name = g_strdup("english");

	generic_message = NULL;

	sem_init(&generic_semaphore, 0, 0);

	DBG("Generic: creating new thread for generic_speak\n");
	generic_speaking = 0;
	ret = spd_pthread_create(&generic_speak_thread, NULL, _generic_speak, NULL);
	if (ret != 0) {
		DBG("Generic: thread failed\n");
		*status_info = g_strdup("The module couldn't initialize threads"
					"This can be either an internal problem or an"
					"architecture problem. If you are sure your architecture"
					"supports threads, please report a bug.");
		return -1;
	}

	*status_info = g_strdup("Everything ok so far.");
	return 0;
}

SPDVoice **module_list_voices(void)
{
	return module_list_registered_voices();
}

int module_speak(const gchar * data, size_t bytes, SPDMessageType msgtype)
{
	char *tmp;

	DBG("speak()\n");

	if (generic_speaking) {
		DBG("Speaking when requested to write");
		return 0;
	}
	UPDATE_STRING_PARAMETER(voice.language, generic_set_language);
	UPDATE_PARAMETER(voice_type, generic_set_voice);
	UPDATE_STRING_PARAMETER(voice.name, generic_set_synthesis_voice);
	UPDATE_PARAMETER(punctuation_mode, generic_set_punct);
	UPDATE_PARAMETER(pitch, generic_set_pitch);
	UPDATE_PARAMETER(pitch_range, generic_set_pitch_range);
	UPDATE_PARAMETER(rate, generic_set_rate);
	UPDATE_PARAMETER(volume, generic_set_volume);

	/* Set the appropriate charset */
	assert(generic_msg_language != NULL);
	if (generic_msg_language->charset != NULL) {
		DBG("Recoding from UTF-8 to %s...",
		    generic_msg_language->charset);
		tmp =
		    (char *)g_convert_with_fallback(data, bytes,
						    generic_msg_language->charset,
						    "UTF-8",
						    GenericRecodeFallback, NULL,
						    NULL, NULL);
	} else {
		DBG("Warning: Preferred charset not specified, recoding to iso-8859-1");
		tmp =
		    (char *)g_convert_with_fallback(data, bytes, "iso-8859-1",
						    "UTF-8",
						    GenericRecodeFallback, NULL,
						    NULL, NULL);
	}

	if (tmp == NULL)
		return -1;

	/* TODO: use a generic engine for SPELL, CHAR, KEY */
	if (msgtype == SPD_MSGTYPE_TEXT)
		generic_message = module_strip_ssml(tmp);
	else
		generic_message = g_strdup(tmp);
	g_free(tmp);

	module_strip_punctuation_some(generic_message, GenericStripPunctChars);

	generic_message_type = msgtype;

	DBG("Requested data (%d): |%s|\n", msgtype, data);

	/* Send semaphore signal to the speaking thread */
	generic_speaking = 1;
	sem_post(&generic_semaphore);

	DBG("Generic: leaving write() normally\n\r");
	return bytes;
}

int module_stop(void)
{
	DBG("generic: stop()\n");

	if (generic_speaking && generic_pid != 0) {
		DBG("generic: stopping process group pid %d\n", generic_pid);
		kill(-generic_pid, SIGKILL);
	}
	return 0;
}

size_t module_pause(void)
{
	DBG("pause requested\n");
	if (generic_speaking) {
		DBG("Sending request to pause to child\n");
		generic_pause_requested = 1;

		DBG("paused at byte: %d", generic_position);
		return 0;
	} else {
		return -1;
	}
}

char *module_is_speaking(void)
{
	return NULL;
}

int module_close(void)
{
	DBG("generic: close()\n");

	if (generic_speaking) {
		module_stop();
	}

	if (module_terminate_thread(generic_speak_thread) != 0)
		return -1;

	sem_destroy(&generic_semaphore);

	return 0;
}

/* Internal functions */

static void *get_ht_option(GHashTable * hash_table, const char *key)
{
	void *option;
	assert(key != NULL);

	option = g_hash_table_lookup(hash_table, key);
	if (option == NULL)
		DBG("Requested option by key %s not found.\n", key);

	return option;
}

/* Replace all occurances of 'token' in 'sting'
   with 'data' */
char *string_replace(char *string, const char *token, const char *data)
{
	char *p;
	char *str1;
	char *str2;
	char *new;
	char *mstring;

	mstring = g_strdup(string);
	while (1) {
		/* Split the string in two parts, ommit the token */
		p = strstr(mstring, token);
		if (p == NULL) {
			return mstring;
		}
		*p = 0;

		str1 = mstring;
		str2 = p + (strlen(token));

		/* Put it together, replacing token with data */
		new = g_strdup_printf("%s%s%s", str1, data, str2);
		g_free(mstring);
		mstring = new;
	}

}

void *_generic_speak(void *nothing)
{
	TModuleDoublePipe module_pipe;
	int ret;
	int status;

	DBG("generic: speaking thread starting.......\n");

	/* Make interruptible */
	set_speaking_thread_parameters();

	while (1) {
		sem_wait(&generic_semaphore);
		DBG("Semaphore on\n");

		const char *play_command = NULL;
		play_command = spd_audio_get_playcmd(module_audio_id);

		if (play_command == NULL) {
			DBG("This audio backend has no default play command; using \"play\"\n");
			play_command = "play";
		}

		if (generic_message_type == SPD_MSGTYPE_SOUND_ICON) {
			if (strchr(generic_message, '\\') ||
			    strchr(generic_message, '\'') ||
			    strchr(generic_message, '/')) {
				DBG("Warning: bad icon name %s\n", generic_message);
			}
			char *cmd = g_strdup_printf("%s '%s/%s'", play_command, GenericSoundIconFolder, generic_message);
			module_report_event_begin();
			DBG("icon command = |%s|\n", cmd);
			ret = system(cmd);
			if (ret)
				DBG("failed to run icon command: (error=%d) %s\n", errno, strerror(errno));
			module_report_event_end();
			free(cmd);
			generic_speaking = 0;
			continue;
		}

		ret = pipe(module_pipe.pc);
		if (ret != 0) {
			DBG("Can't create pipe pc\n");
			generic_speaking = 0;
			continue;
		}

		ret = pipe(module_pipe.cp);
		if (ret != 0) {
			DBG("Can't create pipe cp\n");
			close(module_pipe.pc[0]);
			close(module_pipe.pc[1]);
			generic_speaking = 0;
			continue;
		}

		module_report_event_begin();

		/* Create a new process so that we could send it signals */
		generic_pid = fork();

		switch (generic_pid) {
		case -1:
			DBG("Can't say the message. fork() failed!\n");
			close(module_pipe.pc[0]);
			close(module_pipe.pc[1]);
			close(module_pipe.cp[0]);
			close(module_pipe.cp[1]);
			generic_speaking = 0;
			continue;

		case 0:{
				char *e_string;
				char *p;
				char *tmpdir, *homedir;
				const char *helper;

				helper = getenv("TMPDIR");
				if (helper)
					tmpdir = g_strdup(helper);
				else
					tmpdir = g_strdup("/tmp");

				helper = g_get_home_dir();
				if (helper)
					homedir = g_strdup(helper);
				else
					homedir =
					    g_strdup("UNKNOWN_HOME_DIRECTORY");

				/* Set this process as a process group leader (so that SIGKILL
				   is also delivered to the child processes created by system()) */
				if (setpgid(0, 0) == -1)
					DBG("Can't set myself as project group leader!");

				e_string = g_strdup(GenericExecuteSynth);

				e_string =
				    string_replace(e_string, "$PLAY_COMMAND",
						   play_command);
				e_string =
				    string_replace(e_string, "$TMPDIR", tmpdir);
				g_free(tmpdir);
				e_string =
				    string_replace(e_string, "$HOMEDIR",
						   homedir);
				g_free(homedir);
				e_string =
				    string_replace(e_string, "$PITCH",
						   generic_msg_pitch_str);
				e_string =
				    string_replace(e_string, "$PITCH_RANGE",
						   generic_msg_pitch_range_str);
				e_string =
				    string_replace(e_string, "$RATE",
						   generic_msg_rate_str);
				e_string =
				    string_replace(e_string, "$VOLUME",
						   generic_msg_volume_str);
				e_string =
				    string_replace(e_string, "$LANGUAGE",
						   generic_msg_language->name);
				e_string =
				    string_replace(e_string, "$PUNCT",
						   generic_msg_punct_str);
				if (generic_msg_voice_str != NULL)
					e_string =
					    string_replace(e_string, "$VOICE",
							   generic_msg_voice_str);
				else {
					char *default_voice = module_getdefaultvoice();
					if (!default_voice)
						default_voice = "no_voice";
					e_string =
					    string_replace(e_string, "$VOICE",
							    default_voice);
				}

				/* Cut it into two strings */
				p = strstr(e_string, "$DATA");
				if (p == NULL)
					exit(1);
				*p = 0;
				execute_synth_str1 = g_strdup(e_string);
				execute_synth_str2 =
				    g_strdup(p + (strlen("$DATA")));

				g_free(e_string);

				/* execute_synth_str1 se sem musi nejak dostat */
				DBG("Starting child...\n");
				_generic_child(module_pipe,
					       GenericMaxChunkLength);
			}
			break;

		default:
			/* This is the parent. Send data to the child. */

			generic_position =
			    module_parent_wfork(module_pipe, generic_message,
						generic_message_type,
						GenericMaxChunkLength,
						GenericDelimiters,
						&generic_pause_requested);

			DBG("Waiting for child...");
			waitpid(generic_pid, &status, 0);
			generic_speaking = 0;

			// Report CANCEL if the process was signal-terminated
			// and END if it terminated normally
			if (WIFSIGNALED(status))
				module_report_event_stop();
			else
				module_report_event_end();

			DBG("child terminated -: status:%d signal?:%d signal number:%d.\n", WIFEXITED(status), WIFSIGNALED(status), WTERMSIG(status));
		}
	}

	generic_speaking = 0;

	DBG("generic: speaking thread ended.......\n");

	pthread_exit(NULL);
}

void _generic_child(TModuleDoublePipe dpipe, const size_t maxlen)
{
	char *text;
	sigset_t some_signals;
	int bytes;
	char *command;
	GString *message;
	int i;
	int ret;

	sigfillset(&some_signals);
	module_sigunblockusr(&some_signals);

	module_child_dp_init(dpipe);

	DBG("Entering child loop\n");
	while (1) {
		/* Read the waiting data */
		text = g_malloc((maxlen + 1) * sizeof(char));
		bytes = module_child_dp_read(dpipe, text, maxlen);
		DBG("read %d bytes in child", bytes);
		if (bytes == 0) {
			g_free(text);
			generic_child_close(dpipe);
		}

		text[bytes] = 0;
		DBG("text read is: |%s|\n", text);

		/* Escape any quotes */
		message = g_string_new("");
		for (i = 0; i <= bytes - 1; i++) {
			if (text[i] == '\'')
				message = g_string_append(message, "'\\''");
			else {
				g_string_append_printf(message, "%c", text[i]);
			}
		}

		DBG("child: escaped text is |%s|", message->str);

		command =
		    g_malloc((strlen(message->str) +
			      strlen(execute_synth_str1) +
			      strlen(execute_synth_str2) + 8) * sizeof(char));

		if (strlen(message->str) != 0) {
			sprintf(command, "%s%s%s", execute_synth_str1,
				message->str, execute_synth_str2);

			DBG("child: synth command = |%s|", command);

			DBG("Speaking in child...");
			module_sigblockusr(&some_signals);
			{
				ret = system(command);
				DBG("Executed shell command returned with %d",
				    ret);
			}
		}
		module_sigunblockusr(&some_signals);

		g_free(command);
		g_free(text);
		g_string_free(message, 1);

		DBG("child->parent: ok, send more data");
		module_child_dp_write(dpipe, "C", 1);
	}
}

static void generic_child_close(TModuleDoublePipe dpipe)
{
	DBG("child: Pipe closed, exiting, closing pipes..\n");
	module_child_dp_close(dpipe);
	DBG("Child ended...\n");
	exit(0);
}

void generic_set_pitch(int pitch)
{
	float hpitch;

	hpitch = ((float)pitch) * GenericPitchMultiply + GenericPitchAdd;
	if (!GenericPitchForceInteger) {
		snprintf(generic_msg_pitch_str, 15, "%.2f", hpitch);
	} else {
		snprintf(generic_msg_pitch_str, 15, "%d", (int)hpitch);
	}
}

void generic_set_pitch_range(int pitch_range)
{
	float hpitch_range;

	hpitch_range =
	    ((float)pitch_range) * GenericPitchRangeMultiply +
	    GenericPitchRangeAdd;
	if (!GenericPitchRangeForceInteger) {
		snprintf(generic_msg_pitch_range_str, 15, "%.2f", hpitch_range);
	} else {
		snprintf(generic_msg_pitch_range_str, 15, "%d",
			 (int)hpitch_range);
	}
}

void generic_set_rate(int rate)
{
	float hrate;

	hrate = ((float)rate) * GenericRateMultiply + GenericRateAdd;
	if (!GenericRateForceInteger) {
		snprintf(generic_msg_rate_str, 15, "%.2f", hrate);
	} else {
		snprintf(generic_msg_rate_str, 15, "%d", (int)hrate);
	}
}

void generic_set_volume(int volume)
{
	float hvolume;

	DBG("Volume: %d", volume);

	hvolume = ((float)volume) * GenericVolumeMultiply + GenericVolumeAdd;
	DBG("HVolume: %f", hvolume);
	if (!GenericVolumeForceInteger) {
		snprintf(generic_msg_volume_str, 15, "%.2f", hvolume);
	} else {
		snprintf(generic_msg_volume_str, 15, "%d", (int)hvolume);
	}
}

void generic_set_language(char *lang)
{
	DBG("Setting language %s", lang);
	char *dash = strchr(lang, '-');
	generic_msg_language =
	    (TGenericLanguage *) get_ht_option(GenericLanguage, lang);
	if (generic_msg_language == NULL && dash) {
		char *lang_only = g_strdup(lang);
		lang_only[dash-lang] = 0;
		generic_msg_language =
		    (TGenericLanguage *) get_ht_option(GenericLanguage, lang_only);
		g_free(lang_only);
	}
	if (generic_msg_language == NULL) {
		DBG("Language %s not found in the configuration file, using defaults.", lang);
		generic_msg_language =
		    (TGenericLanguage *) g_malloc(sizeof(TGenericLanguage));
		generic_msg_language->code = g_strdup(lang);
		generic_msg_language->charset = NULL;
		generic_msg_language->name = g_strdup(lang);
	}

	if (generic_msg_language->name == NULL) {
		DBG("Language name for %s not found in the configuration file.",
		    lang);
		generic_msg_language =
		    (TGenericLanguage *) g_malloc(sizeof(TGenericLanguage));
		generic_msg_language->code = g_strdup("en-US");
		generic_msg_language->charset = g_strdup("iso-8859-1");
		generic_msg_language->name = g_strdup("english");
	}

	generic_set_voice(msg_settings.voice_type);
}

void generic_set_voice(SPDVoiceType voice)
{
	DBG("Setting voice type %d", voice);
	assert(generic_msg_language);
	generic_msg_voice_str =
	    module_getvoice(generic_msg_language->code, voice);
	if (generic_msg_voice_str == NULL) {
		DBG("Invalid voice type specified or no voice available!");
	}
}

void generic_set_synthesis_voice(char *name)
{
	DBG("Setting voice name %s (%s)", name, msg_settings.voice.name);
	assert(msg_settings.voice.name);
	if (module_existsvoice(msg_settings.voice.name))
		generic_msg_voice_str = msg_settings.voice.name;
}

void generic_set_punct(SPDPunctuation punct)
{
	if (punct == SPD_PUNCT_NONE) {
		generic_msg_punct_str = g_strdup((char *)GenericPunctNone);
		return;
	} else if (punct == SPD_PUNCT_SOME) {
		generic_msg_punct_str = g_strdup((char *)GenericPunctSome);
		return;
	} else if (punct == SPD_PUNCT_MOST) {
		if (GenericPunctMost[0] == '\0' && GenericPunctSome[0] != '\0')
			/* Compatibility with old configuration files */
			generic_msg_punct_str = g_strdup((char *)GenericPunctSome);
		else
			generic_msg_punct_str = g_strdup((char *)GenericPunctMost);
		return;
	} else if (punct == SPD_PUNCT_ALL) {
		generic_msg_punct_str = g_strdup((char *)GenericPunctAll);
		return;
	} else {
		DBG("ERROR: Unknown punctuation setting, ignored");
	}
}
