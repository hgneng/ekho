
/*
 * espeak.c - Speech Dispatcher backend for espeak
 *
 * Copyright (C) 2007 Brailcom, o.p.s.
 * Copyright (C) 2019-2021 Samuel Thibault <samuel.thibault@ens-lyon.org>
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
 * @author Lukas Loehrer
 * Based on ibmtts.c.
 *
 * $Id: espeak.c,v 1.11 2008-10-15 17:04:36 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* < Includes*/

/* System includes. */
#include <string.h>
#include <ctype.h>
#include <glib.h>
#include <fcntl.h>

/* espeak header file */
#ifdef ESPEAK_NG_INCLUDE
#include <espeak-ng/espeak_ng.h>
#else
#include <espeak/speak_lib.h>
#endif

#ifndef ESPEAK_API_REVISION
#define ESPEAK_API_REVISION 1
#endif

/* Speech Dispatcher includes. */
#include "spd_audio.h"
#include <speechd_types.h>
#include "module_utils.h"

/* > */
/* < Basic definitions*/

#ifdef ESPEAK_NG_INCLUDE
#define MODULE_NAME     "espeak-ng"
#define DBG_MODNAME     "Espeak-ng:"
#else
#define MODULE_NAME     "espeak"
#define DBG_MODNAME     "Espeak:"
#endif

#define MODULE_VERSION  "0.1"

#define DEBUG_MODULE 1
DECLARE_DEBUG()
#define DBG_WARN(e, msg) do {						\
	if (Debug && !(e)) {						\
		DBG(DBG_MODNAME " Warning:  " msg);			\
	} \
} while (0)
typedef enum {
	FATAL_ERROR = -1,
	OK = 0,
	ERROR = 1
} TEspeakSuccess;

static int espeak_sample_rate = 0;
static SPDVoice **espeak_voice_list = NULL;
#ifdef ESPEAK_NG_INCLUDE
static const espeak_VOICE **espeak_variants = NULL;
#endif

/* When a voice is set, this is the baseline pitch of the voice.
   SSIP PITCH commands then adjust relative to this. */
static int espeak_voice_pitch_baseline = 50;

/* When a voice is set, this is the baseline pitch range of the voice.
   SSIP PITCH range commands then adjust relative to this. */
static int espeak_voice_pitch_range_baseline = 50;

static int stop_requested = 0;
static int pause_requested = 0;
static int pause_index_sent = 0;
static int began = 0;

/* <Function prototypes*/

static TEspeakSuccess espeak_set_punctuation_list_from_utf8(const char *punct);
static SPDVoice **espeak_list_synthesis_voices();
static void espeak_free_voice_list();

/* Callbacks */
static int synth_callback(short *wav, int numsamples, espeak_EVENT * events);
static int uri_callback(int type, const char *uri, const char *base);

/* Internal function prototypes for main thread. */

/* Basic parameters */
static void espeak_set_rate(signed int rate);
static void espeak_set_pitch(signed int pitch);
static void espeak_set_pitch_range(signed int pitch_range);
static void espeak_set_volume(signed int volume);
static void espeak_set_punctuation_mode(SPDPunctuation punct_mode);
static void espeak_set_cap_let_recogn(SPDCapitalLetters cap_mode);

/* Voices and languages */
static void espeak_set_language(char *lang);
static void espeak_set_voice(SPDVoiceType voice);
static void espeak_set_language_and_voice(char *lang, SPDVoiceType voice);
static void espeak_set_synthesis_voice(char *);

/* > */
/* < Module configuration options*/

MOD_OPTION_1_STR(EspeakPunctuationList)
    MOD_OPTION_1_INT(EspeakCapitalPitchRise)
    MOD_OPTION_1_INT(EspeakMinRate)
    MOD_OPTION_1_INT(EspeakNormalRate)
    MOD_OPTION_1_INT(EspeakMaxRate)
    MOD_OPTION_1_INT(EspeakIndexing)

    MOD_OPTION_1_INT(EspeakAudioChunkSize)
    MOD_OPTION_1_INT(EspeakAudioQueueMaxSize)
    MOD_OPTION_1_STR(EspeakSoundIconFolder)
    MOD_OPTION_1_INT(EspeakSoundIconVolume)

/* > */
/* < Public functions */
int module_load(void)
{
	INIT_SETTINGS_TABLES();

	REGISTER_DEBUG();

	/* Options */
	MOD_OPTION_1_INT_REG(EspeakAudioChunkSize, 2000);
	MOD_OPTION_1_INT_REG(EspeakAudioQueueMaxSize, 20 * 22050);
	MOD_OPTION_1_STR_REG(EspeakSoundIconFolder,
			     "/usr/share/sounds/sound-icons/");
	MOD_OPTION_1_INT_REG(EspeakSoundIconVolume, 0);

	MOD_OPTION_1_INT_REG(EspeakMinRate, 80);
	MOD_OPTION_1_INT_REG(EspeakNormalRate, 170);
	MOD_OPTION_1_INT_REG(EspeakMaxRate, 449);
	MOD_OPTION_1_STR_REG(EspeakPunctuationList, "@/+-_");
	MOD_OPTION_1_INT_REG(EspeakCapitalPitchRise, 800);
	MOD_OPTION_1_INT_REG(EspeakIndexing, 1);
	if (EspeakCapitalPitchRise == 1 || EspeakCapitalPitchRise == 2) {
		EspeakCapitalPitchRise = 0;
	}

	return OK;
}

int module_init(char **status_info)
{
	int ret;
	const char *espeak_version;

	DBG(DBG_MODNAME " Module init().");

	module_audio_set_server();

	*status_info = NULL;

	/* Report versions. */
	espeak_version = espeak_Info(NULL);
	DBG(DBG_MODNAME " espeak Output Module version %s, espeak Engine version %s",
	    MODULE_VERSION, espeak_version);

	/* <Espeak setup */

	DBG(DBG_MODNAME " Initializing engine with buffer size %d ms.",
	    EspeakAudioChunkSize);
#if ESPEAK_API_REVISION == 1
	espeak_sample_rate =
	    espeak_Initialize(AUDIO_OUTPUT_SYNCHRONOUS, EspeakAudioChunkSize,
			      NULL);
#else
	espeak_sample_rate =
	    espeak_Initialize(AUDIO_OUTPUT_SYNCHRONOUS, EspeakAudioChunkSize,
			      NULL, 0);
#endif
	if (espeak_sample_rate == EE_INTERNAL_ERROR) {
		DBG(DBG_MODNAME " Could not initialize engine.");
		*status_info = g_strdup("Could not initialize engine. ");
		return FATAL_ERROR;
	}

	DBG(DBG_MODNAME " Registering callbacks.");
	espeak_SetSynthCallback(synth_callback);
	espeak_SetUriCallback(uri_callback);

	DBG("Setting up espeak specific configuration settings.");
	ret = espeak_set_punctuation_list_from_utf8(EspeakPunctuationList);
	if (ret != OK)
		DBG(DBG_MODNAME " Failed to set punctuation list.");

	espeak_voice_list = espeak_list_synthesis_voices();

	*status_info = g_strdup(DBG_MODNAME " Initialized successfully.");

	return OK;
}

SPDVoice **module_list_voices(void)
{
	return espeak_voice_list;
}

void module_speak_sync(const gchar * data, size_t bytes, SPDMessageType msgtype)
{
	espeak_ERROR result = EE_INTERNAL_ERROR;
	int flags = espeakSSML | espeakCHARS_UTF8;
	gchar *msg = NULL;

	DBG(DBG_MODNAME " module_speak().");

	DBG(DBG_MODNAME " Requested data: |%s| %d %lu", data, msgtype,
	    (unsigned long)bytes);

	/* Setting speech parameters. */
	UPDATE_STRING_PARAMETER(voice.language, espeak_set_language);
	UPDATE_PARAMETER(voice_type, espeak_set_voice);
	UPDATE_STRING_PARAMETER(voice.name, espeak_set_synthesis_voice);

	if (msg_settings_old.voice.name && strstr(msg_settings_old.voice.name , "mbrola"))
	{
		if (msgtype == SPD_MSGTYPE_TEXT)
		{
			/* espeak has troubles with setting mbrola voices in SSML mode
			 * https://github.com/espeak-ng/espeak-ng/issues/1011 */
			DBG(DBG_MODNAME " mbrola voice, stripping SSML.");
			msg = module_strip_ssml(data);
			data = msg;
		}

		flags &= ~espeakSSML;
	}

	UPDATE_PARAMETER(rate, espeak_set_rate);
	UPDATE_PARAMETER(volume, espeak_set_volume);
	UPDATE_PARAMETER(pitch, espeak_set_pitch);
	UPDATE_PARAMETER(pitch_range, espeak_set_pitch_range);
	UPDATE_PARAMETER(punctuation_mode, espeak_set_punctuation_mode);
	UPDATE_PARAMETER(cap_let_recogn, espeak_set_cap_let_recogn);

	began = 0;
	stop_requested = 0;
	pause_requested = 0;
	pause_index_sent = 0;

	module_speak_ok();

	/*
	   UPDATE_PARAMETER(spelling_mode, espeak_set_spelling_mode);
	 */
	/* Send data to espeak */
	switch (msgtype) {
	case SPD_MSGTYPE_TEXT:
		result = espeak_Synth(data, bytes + 1, 0, POS_CHARACTER, 0,
				      flags, NULL, NULL);
		break;
	case SPD_MSGTYPE_SOUND_ICON:{
			char *msg =
			    g_strdup_printf("<audio src=\"%s%s\">%s</audio>",
					    EspeakSoundIconFolder, data, data);
			result =
			    espeak_Synth(msg, strlen(msg) + 1, 0, POS_CHARACTER,
					 0, flags, NULL, NULL);
			g_free(msg);
			break;
		}
	case SPD_MSGTYPE_CHAR:{
			wchar_t wc = 0;
			if (bytes == 1) {	// ASCII
				wc = (wchar_t) data[0];
			} else if (bytes == 5
				   && (0 == strncmp(data, "space", bytes))) {
				wc = (wchar_t) 0x20;
			} else {
				gsize bytes_out;
				gchar *tmp =
				    g_convert(data, -1, "wchar_t", "utf-8",
					      NULL, &bytes_out, NULL);
				if (tmp != NULL && bytes_out == sizeof(wchar_t)) {
					wchar_t *wc_ptr = (wchar_t *) tmp;
					wc = wc_ptr[0];
				} else {
					DBG(DBG_MODNAME " Failed to convert utf-8 to wchar_t, or not exactly one utf-8 character given.");
				}
				g_free(tmp);
			}
			char *msg =
			    g_strdup_printf
			    ("<say-as interpret-as=\"tts:char\">&#%ld;</say-as>",
			     (long)wc);
			result =
			    espeak_Synth(msg, strlen(msg) + 1, 0, POS_CHARACTER,
					 0, flags, NULL, NULL);
			g_free(msg);
			break;
		}
	case SPD_MSGTYPE_KEY:{
			/* TODO: Convert unspeakable keys to speakable form */
			char *msg =
			    g_strdup_printf
			    ("<say-as interpret-as=\"tts:key\">%s</say-as>",
			     data);
			result =
			    espeak_Synth(msg, strlen(msg) + 1, 0, POS_CHARACTER,
					 0, flags, NULL, NULL);
			g_free(msg);
			break;
		}
	case SPD_MSGTYPE_SPELL:
		/* TODO: use a generic engine */
		break;
	}

	if (result != EE_OK) {
		DBG(DBG_MODNAME " Synth error %d", result);
	}

	if (pause_requested) {
		DBG(DBG_MODNAME " Synth paused");
		module_report_event_pause();
	} else if (stop_requested) {
		DBG(DBG_MODNAME " Synth stopped");
		module_report_event_stop();
	} else {
		DBG(DBG_MODNAME " Leaving module_speak() normally.");
		module_report_event_end();
	}

	g_free(msg);
}

int module_stop(void)
{
	DBG(DBG_MODNAME " module_stop().");

	stop_requested = 1;

	return OK;
}

size_t module_pause(void)
{
	DBG(DBG_MODNAME " module_pause().");

	pause_requested = 1;

	return OK;
}

int module_close(void)
{
	DBG(DBG_MODNAME " close().");

	DBG(DBG_MODNAME " Terminating threads");

	DBG(DBG_MODNAME " terminating synthesis.");
	espeak_Terminate();

	espeak_free_voice_list();

	return 0;
}

/* > */
/* < Internal functions */
static void espeak_set_rate(signed int rate)
{
	assert(rate >= -100 && rate <= +100);
	int speed;
	int normal_rate = EspeakNormalRate, max_rate = EspeakMaxRate, min_rate =
	    EspeakMinRate;

	if (rate < 0)
		speed = normal_rate + (normal_rate - min_rate) * rate / 100;
	else
		speed = normal_rate + (max_rate - normal_rate) * rate / 100;

	espeak_ERROR ret = espeak_SetParameter(espeakRATE, speed, 0);
	if (ret != EE_OK) {
		DBG(DBG_MODNAME " Error setting rate %i.", speed);
	} else {
		DBG(DBG_MODNAME " Rate set to %i.", speed);
	}
}

static void espeak_set_volume(signed int volume)
{
	assert(volume >= -100 && volume <= +100);
	int vol;
	vol = volume + 100;
	espeak_ERROR ret = espeak_SetParameter(espeakVOLUME, vol, 0);
	if (ret != EE_OK) {
		DBG(DBG_MODNAME " Error setting volume %i.", vol);
	} else {
		DBG(DBG_MODNAME " Volume set to %i.", vol);
	}
}

static void espeak_set_pitch(signed int pitch)
{
	assert(pitch >= -100 && pitch <= +100);
	int pitchBaseline;
	/* Possible range 0 to 100. */
	if (pitch < 0) {
		pitchBaseline =
		    ((float)(pitch + 100) * espeak_voice_pitch_baseline) /
		    (float)100;
	} else {
		pitchBaseline =
		    (((float)pitch * (100 - espeak_voice_pitch_baseline))
		     / (float)100) + espeak_voice_pitch_baseline;
	}
	assert(pitchBaseline >= 0 && pitchBaseline <= 100);
	espeak_ERROR ret = espeak_SetParameter(espeakPITCH, pitchBaseline, 0);
	if (ret != EE_OK) {
		DBG(DBG_MODNAME " Error setting pitch %i.", pitchBaseline);
	} else {
		DBG(DBG_MODNAME " Pitch set to %i.", pitchBaseline);
	}
}

static void espeak_set_pitch_range(signed int pitch_range)
{
	assert(pitch_range >= -100 && pitch_range <= +100);
	int pitchRangeBaseline;
	/* Possible range 0 to 100. */
	if (pitch_range < 0) {
		pitchRangeBaseline =
		    ((float)(pitch_range + 100) *
		     espeak_voice_pitch_range_baseline) / (float)100;
	} else {
		pitchRangeBaseline =
		    (((float)pitch_range *
		      (100 - espeak_voice_pitch_range_baseline))
		     / (float)100) + espeak_voice_pitch_range_baseline;
	}
	assert(pitchRangeBaseline >= 0 && pitchRangeBaseline <= 100);
	espeak_ERROR ret =
	    espeak_SetParameter(espeakRANGE, pitchRangeBaseline, 0);
	if (ret != EE_OK) {
		DBG(DBG_MODNAME " Error setting pitch range %i.",
		    pitchRangeBaseline);
	} else {
		DBG(DBG_MODNAME " Pitch range set to %i.", pitchRangeBaseline);
	}
}

static void espeak_set_punctuation_mode(SPDPunctuation punct_mode)
{
	espeak_PUNCT_TYPE espeak_punct_mode = espeakPUNCT_SOME;
	switch (punct_mode) {
	case SPD_PUNCT_ALL:
		espeak_punct_mode = espeakPUNCT_ALL;
		break;
	case SPD_PUNCT_MOST:
		/* XXX: Approximation */
		espeak_punct_mode = espeakPUNCT_SOME;
		break;
	case SPD_PUNCT_SOME:
		espeak_punct_mode = espeakPUNCT_SOME;
		break;
	case SPD_PUNCT_NONE:
		espeak_punct_mode = espeakPUNCT_NONE;
		break;
	}

	espeak_ERROR ret =
	    espeak_SetParameter(espeakPUNCTUATION, espeak_punct_mode, 0);
	if (ret != EE_OK) {
		DBG(DBG_MODNAME " Failed to set punctuation mode.");
	} else {
		DBG("Set punctuation mode.");
	}
}

static void espeak_set_cap_let_recogn(SPDCapitalLetters cap_mode)
{
	int espeak_cap_mode = 0;
	switch (cap_mode) {
	case SPD_CAP_NONE:
		espeak_cap_mode = EspeakCapitalPitchRise;
		break;
	case SPD_CAP_SPELL:
		espeak_cap_mode = 2;
		break;
	case SPD_CAP_ICON:
		espeak_cap_mode = 1;
		break;
	}

	espeak_ERROR ret =
	    espeak_SetParameter(espeakCAPITALS, espeak_cap_mode, 1);
	if (ret != EE_OK) {
		DBG(DBG_MODNAME " Failed to set capitals mode.");
	} else {
		DBG("Set capitals mode.");
	}
}

/* Given a language code and SD voice code, sets the espeak voice. */
static void espeak_set_language_and_voice(char *lang, SPDVoiceType voice_code)
{
	DBG(DBG_MODNAME " set_language_and_voice %s %d", lang, voice_code);
	espeak_ERROR ret;

	unsigned char overlay = 0;
	switch (voice_code) {
	case SPD_MALE1:
		overlay = 0;
		break;
	case SPD_MALE2:
		overlay = 1;
		break;
	case SPD_MALE3:
		overlay = 2;
		break;
	case SPD_FEMALE1:
		overlay = 11;
		break;
	case SPD_FEMALE2:
		overlay = 12;
		break;
	case SPD_FEMALE3:
		overlay = 13;
		break;
	case SPD_CHILD_MALE:
		overlay = 4;
		break;
	case SPD_CHILD_FEMALE:
		overlay = 14;
		break;
	default:
		overlay = 0;
		break;
	}

	char *name = g_strdup_printf("%s+%d", lang, overlay);
	DBG(DBG_MODNAME " set_language_and_voice name=%s", name);
	ret = espeak_SetVoiceByName(name);

	if (ret != EE_OK) {
		espeak_VOICE voice_select;
		memset(&voice_select, 0, sizeof(voice_select));
		voice_select.languages = name;
		ret = espeak_SetVoiceByProperties(&voice_select);
	}

	if (ret != EE_OK) {
		DBG(DBG_MODNAME " Error selecting language %s", name);
	} else {
		DBG(DBG_MODNAME " Successfully set voice to \"%s\"", name);
	}
	g_free(name);
}

static void espeak_set_voice(SPDVoiceType voice)
{
	assert(msg_settings.voice.language);
	espeak_set_language_and_voice(msg_settings.voice.language, voice);
}

static void espeak_set_language(char *lang)
{
	espeak_set_language_and_voice(lang, msg_settings.voice_type);
}

static void espeak_set_synthesis_voice(char *synthesis_voice)
{
	if (synthesis_voice != NULL) {
#ifdef ESPEAK_NG_INCLUDE
		gchar *voice_name = NULL;
		gchar *variant_name = NULL;
		gchar **voice_split = NULL;
		gchar **identifier = NULL;
		gchar *variant_file = NULL;
		gchar *voice = NULL;
		int i = 0;

		/* Espeak-ng can accept the full voice name as the voice, but will
		 * only accept the file name of the variant to use, which can be
		 * found in the identifier member of the variant list, which
		 * itself is of type espeak_VOICE
		 */
		if (g_strstr_len(synthesis_voice, -1, "+") != NULL) {
			voice_split = g_strsplit(synthesis_voice, "+", 2);
			voice_name = voice_split[0];
			variant_name = voice_split[1];
			g_free(voice_split);

			for (i = 0; espeak_variants[i] != NULL; i++) {
				identifier = g_strsplit(espeak_variants[i]->identifier, "/", 2);

				if (g_strcmp0(espeak_variants[i]->name, variant_name) == 0) {
					if (identifier[1] != NULL)
						variant_file = g_strdup(identifier[1]);
				} else if (g_strcmp0(identifier[1], variant_name) == 0) {
					variant_file = g_strdup(variant_name);
				}

				g_strfreev(identifier);
				identifier = NULL;
			}

			if (variant_file != NULL) {
				voice = synthesis_voice = g_strdup_printf("%s+%s", voice_name, variant_file);
				g_free(variant_file);
			} else {
				DBG(DBG_MODNAME " Cannot find the variant file name for the given variant.");
			}

			g_free(voice_name);
			g_free(variant_name);
		}
#endif

		espeak_ERROR ret = espeak_SetVoiceByName(synthesis_voice);
		if (ret != EE_OK) {
			DBG(DBG_MODNAME " Failed to set synthesis voice to %s.",
			    synthesis_voice);
		} else {
			DBG(DBG_MODNAME " Successfully set synthesis voice to %s.",
			    synthesis_voice);
		}

#ifdef ESPEAK_NG_INCLUDE
		g_free(voice);
#endif
	}
}

/* Callbacks */

static gboolean espeak_send_audio_upto(short *wav, int *sent, int upto)
{
	assert(*sent >= 0);
	assert(upto >= 0);
	int numsamples = upto - (*sent);
	if (wav == NULL || numsamples == 0) {
		return TRUE;
	}
#ifdef ESPEAK_NG_INCLUDE
	int rate = espeak_ng_GetSampleRate();
#else
	int rate = espeak_sample_rate;
#endif
	AudioTrack track = {
		.bits = 16,
		.num_channels = 1,
		.sample_rate = rate,
		.num_samples = numsamples,
		.samples = wav + (*sent),
	};
	DBG(DBG_MODNAME " pushing %d samples", numsamples);
	module_tts_output_server(&track, SPD_AUDIO_LE);
	*sent = upto;
	return 0;
}

static int synth_callback(short *wav, int numsamples, espeak_EVENT * events)
{
	/* Number of samples sent in current message. */
	static int numsamples_sent_msg = 0;
	/* Number of samples already sent during this call to the callback. */
	int numsamples_sent = 0;

	/* Process server events in case we were told to stop in between */
	module_process(STDIN_FILENO, 0);

	if (stop_requested)
		return 1;

	if (pause_requested && pause_index_sent)
		return 1;

	if (!began) {
		began = 1;
		module_report_event_begin();
		numsamples_sent_msg = 0;
	}

	/* Process events and audio data */
	while (events->type != espeakEVENT_LIST_TERMINATED) {
		/* Enqueue audio upto event */
		switch (events->type) {
		case espeakEVENT_MARK:
			if (!EspeakIndexing)
				break;
		case espeakEVENT_PLAY:{
				/* Convert ms position to samples */
				gint64 pos_msg = events->audio_position;
				pos_msg = pos_msg * espeak_sample_rate / 1000;
				/* Convert position in message to position in current chunk */
				int upto =
				    (int)CLAMP(pos_msg - numsamples_sent_msg,
					       0, numsamples);	/* This is just for safety */
				espeak_send_audio_upto(wav, &numsamples_sent,
						       upto);
				break;
			}
		default:
			break;
		}
		if (stop_requested)
			return 1;
		/* Process actual event */
		switch (events->type) {
		case espeakEVENT_MARK:
			if (EspeakIndexing) {
				DBG(DBG_MODNAME " Reporting mark %s", events->id.name);
				module_report_index_mark(events->id.name);
				if (pause_requested &&
					!strncmp(events->id.name,
						INDEX_MARK_BODY,
						INDEX_MARK_BODY_LEN)) {
					pause_index_sent = 1;
					return 1;
				}
			}
			break;
		case espeakEVENT_PLAY:
			module_report_icon(events->id.name);
			break;
		case espeakEVENT_MSG_TERMINATED:
			// This event never has any audio in the same callback
			DBG(DBG_MODNAME " Synth terminated");
			break;
		default:
			break;
		}
		if (stop_requested)
			return 1;
		events++;
	}
	espeak_send_audio_upto(wav, &numsamples_sent, numsamples);
	numsamples_sent_msg += numsamples;
	if (stop_requested)
		return 1;
	return 0;
}

static int uri_callback(int type, const char *uri, const char *base)
{
	int result = 1;
	if (type == 1) {
		/* Audio icon */
		if (g_file_test(uri, G_FILE_TEST_EXISTS)) {
			result = 0;
		}
	}
	return result;
}

static SPDVoice **espeak_list_synthesis_voices()
{
	SPDVoice **result = NULL;
	SPDVoice *voice = NULL;
	SPDVoice *vo = NULL;
	const espeak_VOICE **espeak_voices = NULL;
#ifndef ESPEAK_NG_INCLUDE
	const espeak_VOICE **espeak_variants = NULL;
#endif
	const espeak_VOICE **espeak_mbrola = NULL;
	espeak_VOICE voice_spec;
	const espeak_VOICE *v = NULL;
	GQueue *voice_list = NULL;
	GQueue *variant_list = NULL;
	GList *voice_list_iter = NULL;
	GList *variant_list_iter = NULL;
	const gchar *first_lang = NULL;
	gchar *dash;
	gchar *vname = NULL;
	int numvoices = 0;
	int numvariants = 0;
	int nummbrola = 0, totnummbrola = 0;
	int totalvoices;
	int i = 0, j;

	espeak_voices = espeak_ListVoices(NULL);
	voice_list = g_queue_new();

	for (i = 0; espeak_voices[i] != NULL; i++) {
		v = espeak_voices[i];
		if (!g_str_has_prefix(v->identifier, "mb/")) {
			/* Not an mbrola voice */
			voice = g_new0(SPDVoice, 1);

			voice->name = g_strdup(v->name);

			first_lang = v->languages + 1;
			voice->language = g_strdup(first_lang);
			for (dash = strchr(voice->language, '-');
			     dash && *dash;
			     dash++) {
				*dash = toupper(*dash);
			}
			voice->variant = NULL;

			g_queue_push_tail(voice_list, voice);
		}

	}

	numvoices = g_queue_get_length(voice_list);
	DBG(DBG_MODNAME " %d voices total.", numvoices);

	memset(&voice_spec, 0, sizeof(voice_spec));
	voice_spec.languages = "variant";
	espeak_variants = espeak_ListVoices(&voice_spec);

	variant_list = g_queue_new();

	for (i = 0; espeak_variants[i] != NULL; i++) {
		v = espeak_variants[i];

		vname = g_strdup(v->name);
		g_queue_push_tail(variant_list, vname);
	}

	numvariants = g_queue_get_length(variant_list);
	DBG(DBG_MODNAME " %d variants total.", numvariants);

#ifdef ESPEAK_NG_INCLUDE
	voice_spec.languages = "mbrola";
	espeak_mbrola = espeak_ListVoices(&voice_spec);
	const char *espeak_data;

	espeak_Info(&espeak_data);

	for (j = 0; espeak_mbrola[j] != NULL; j++)
	{
		const char *identifier = espeak_mbrola[j]->identifier;
		char *voicename, *dash, *path;
		struct stat st;

		totnummbrola++;

		/* We try to load the voice to check whether it works, but
		 * espeak-ng is not currently reporting load failures, see
		 * https://github.com/espeak-ng/espeak-ng/pull/1022 */

		if (!strncmp(identifier, "mb/mb-", 6)) {
			voicename = g_strdup(identifier + 6);
			dash = index(voicename, '-');
			if (dash)
				/* Ignore "-en" language specification */
				*dash = 0;

			path = g_strdup_printf("%s/mbrola/%s", espeak_data, voicename);
			if (access(path, O_RDONLY) != 0) {
				g_free(path);
				path = g_strdup_printf("/usr/share/mbrola/%s", voicename);
				if (access(path, O_RDONLY) != 0) {
					g_free(path);
					path = g_strdup_printf("/usr/share/mbrola/%s/%s", voicename, voicename);
					if (access(path, O_RDONLY) != 0) {
						g_free(path);
						path = g_strdup_printf("/usr/share/mbrola/voices/%s", voicename);
						if (access(path, O_RDONLY) != 0) {
							g_free(path);
							espeak_mbrola[j] = NULL;
							continue;
						}
					}
				}
			}
			g_free(path);
		}

		espeak_ERROR ret = espeak_SetVoiceByName(espeak_mbrola[j]->name);
		if (ret != EE_OK) {
			espeak_mbrola[j] = NULL;
			continue;
		}

		nummbrola++;
	}

	DBG(DBG_MODNAME " %d mbrola total.", nummbrola);
#endif

	totalvoices = (numvoices * (numvariants + 1)) + nummbrola;
	result = g_new0(SPDVoice *, totalvoices + 1);
	voice_list_iter = g_queue_peek_head_link(voice_list);

	for (i = 0; i < numvoices * (numvariants + 1); i++) {
		result[i] = voice_list_iter->data;

		if (!g_queue_is_empty(variant_list)) {
			vo = voice_list_iter->data;
			variant_list_iter = g_queue_peek_head_link(variant_list);

			while (variant_list_iter != NULL && variant_list_iter->data != NULL) {
				voice = g_new0(SPDVoice, 1);

				voice->name = g_strdup_printf("%s+%s", vo->name,
							      (char *)variant_list_iter->data);
				voice->language = g_strdup(vo->language);
				voice->variant = g_strdup((char *)variant_list_iter->data);

				result[++i] = voice;
				variant_list_iter = variant_list_iter->next;
			}
		}

		voice_list_iter = voice_list_iter->next;
	}

	for (j = 0; j < totnummbrola; j++) {
		if (!espeak_mbrola[j])
			continue;

		voice = g_new0(SPDVoice, 1);
		voice->name = g_strdup_printf("%s", espeak_mbrola[j]->name);
		voice->language = g_strdup_printf("%s", espeak_mbrola[j]->languages + 1);
		for (dash = strchr(voice->language, '-');
		     dash && *dash;
		     dash++) {
			*dash = toupper(*dash);
		}
		voice->variant = NULL;
		result[i++] = voice;
	}

	assert(i == totalvoices);

	if (voice_list != NULL)
		g_queue_free(voice_list);
	if (variant_list != NULL)
		g_queue_free_full(variant_list, (GDestroyNotify)g_free);

	result[i] = NULL;
	DBG(DBG_MODNAME " %d usable voices.", totalvoices);

	return result;
}

static void espeak_free_voice_list()
{
	if (espeak_voice_list != NULL) {
		int i;
		for (i = 0; espeak_voice_list[i] != NULL; i++) {
			g_free(espeak_voice_list[i]->name);
			g_free(espeak_voice_list[i]->language);
			g_free(espeak_voice_list[i]->variant);
			g_free(espeak_voice_list[i]);
		}
		g_free(espeak_voice_list);
		espeak_voice_list = NULL;
	}
}

static TEspeakSuccess espeak_set_punctuation_list_from_utf8(const gchar * punct)
{
	TEspeakSuccess result = ERROR;
	wchar_t *wc_punct =
	    (wchar_t *) g_convert(punct, -1, "wchar_t", "utf-8", NULL, NULL,
				  NULL);
	if (wc_punct != NULL) {
		espeak_ERROR ret = espeak_SetPunctuationList(wc_punct);
		if (ret == EE_OK)
			result = OK;
		g_free(wc_punct);
	}
	return result;
}

/* > */

/* local variables: */
/* folded-file: t */
/* c-basic-offset: 4 */
/* end: */
