

/*
 * kali.cpp - Speech Dispatcher backend for Kali
 *
 * Copyright (C)2016 Hypra
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
 * $Id: kali.c,v 1.59 2008-06-09 10:38:02 hanke Exp $
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <kali/Kali/kali.h>
extern "C" {
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <semaphore.h>
#include "spd_audio.h"

#include <speechd_types.h>

#include "module_utils.h"
}
#define MODULE_NAME     "kali"
#define MODULE_VERSION  "0.0"
#define DEBUG_MODULE 1
DECLARE_DEBUG();

/* Thread and process control */
static int kali_speaking = 0;

static pthread_t kali_speak_thread;
static sem_t kali_semaphore;

static char **kali_message;
static SPDMessageType kali_message_type;

static int kali_position = 0;
static int kali_pause_requested = 0;

signed int kali_volume = 0;
SPDVoice **kali_voice_list = NULL;

/* Internal functions prototypes */
static void kali_set_rate(signed int rate);
static void kali_set_pitch(signed int pitch);
static void kali_set_volume(signed int volume);
static void kali_set_punctuation_mode(SPDPunctuation punct);
static void kali_set_voice(char *voice);

static SPDVoice **kali_get_voices();
static void *_kali_speak(void *);

int kali_stop = 0;

MOD_OPTION_1_INT(KaliMaxChunkLength);
MOD_OPTION_1_STR(KaliDelimiters);
MOD_OPTION_1_INT(KaliNormalRate);
MOD_OPTION_1_INT(KaliNormalVolume);
MOD_OPTION_1_INT(KaliNormalPitch);
MOD_OPTION_1_STR(KaliVoiceParameters);
MOD_OPTION_1_INT(KaliExpandAbbreviations);

/* Public functions */

int module_load(void)
{
	INIT_SETTINGS_TABLES();

	REGISTER_DEBUG();

	MOD_OPTION_1_INT_REG(KaliMaxChunkLength, 4999);
	MOD_OPTION_1_STR_REG(KaliDelimiters, ".");
	MOD_OPTION_1_INT_REG(KaliNormalRate, 70);
	MOD_OPTION_1_INT_REG(KaliNormalVolume, 10);
	MOD_OPTION_1_INT_REG(KaliNormalPitch, 6);
	MOD_OPTION_1_STR_REG(KaliVoiceParameters, "Patrick");
	MOD_OPTION_1_INT_REG(KaliExpandAbbreviations, 1);

	return 0;
}

#define ABORT(msg) g_string_append(info, msg); \
	DBG("FATAL ERROR:", info->str); \
	*status_info = info->str; \
	g_string_free(info, 0); \
	return -1;

int module_init(char **status_info)
{
	int ret;
	GString *info;

	DBG("Module init");
	INIT_INDEX_MARKING();

	*status_info = NULL;
	info = g_string_new("");

	/* Init kali and register a new voice */
	initGlobal();
	initParle();
	initTrans();
	initAnalyse();
	initKali();
	SetSortieSonMultiKaliStd(0, false);	//sound output
	SetSortieBufMultiKaliStd(0, true);	//Buffer output
	SetDebitKali(KaliNormalRate);
	SetVolumeKali(KaliNormalVolume);
	SetHauteurKali(KaliNormalPitch);
	kali_voice_list = kali_get_voices();
	kali_set_voice(KaliVoiceParameters);

	DBG("KaliMaxChunkLength = %d\n", KaliMaxChunkLength);
	DBG("KaliDelimiters = %s\n", KaliDelimiters);
	DBG("KaliExpandAbbreviations = %d\n", KaliExpandAbbreviations);

	kali_message = (char **)g_malloc(sizeof(char *));
	*kali_message = NULL;

	sem_init(&kali_semaphore, 0, 0);

	DBG("Kali: creating new thread for kali_speak\n");
	kali_speaking = 0;
	ret = pthread_create(&kali_speak_thread, NULL, _kali_speak, NULL);
	if (ret != 0) {
		DBG("Kali: thread failed\n");
		*status_info =
		    g_strdup("The module couldn't initialize threads "
			     "This could be either an internal problem or an "
			     "architecture problem. If you are sure your architecture "
			     "supports threads, please report a bug.");
		return -1;
	}

	*status_info = g_strdup("Kali initialized successfully.");

	return 0;
}

#undef ABORT

SPDVoice **module_list_voices(void)
{
	return kali_voice_list;
}

int module_speak(gchar * data, size_t bytes, SPDMessageType msgtype)
{
	DBG("write()\n");

	if (kali_speaking) {
		DBG("Speaking when requested to write");
		return 0;
	}

	DBG("Requested data: |%s|\n", data);

	if (*kali_message != NULL) {
		g_free(*kali_message);
		*kali_message = NULL;
	}
	*kali_message = module_strip_ssml(data);
	kali_message_type = SPD_MSGTYPE_TEXT;

	/* Setting voice */
	UPDATE_PARAMETER(rate, kali_set_rate);
	UPDATE_PARAMETER(volume, kali_set_volume);
	UPDATE_PARAMETER(pitch, kali_set_pitch);
	UPDATE_PARAMETER(punctuation_mode, kali_set_punctuation_mode);
	UPDATE_STRING_PARAMETER(voice.name, kali_set_voice);
	kali_set_voice(msg_settings.voice.name);

	/* Send semaphore signal to the speaking thread */
	kali_speaking = 1;
	sem_post(&kali_semaphore);

	DBG("Kali: leaving write() normally\n\r");
	return bytes;
}

int module_stop(void)
{
	int ret;
	DBG("kali: stop()\n");

	kali_stop = 1;
	if (module_audio_id) {
		DBG("Stopping audio");
		ret = spd_audio_stop(module_audio_id);
		if (ret != 0)
			DBG("WARNING: Non 0 value from spd_audio_stop: %d",
			    ret);
	}

	return 0;
}

size_t module_pause(void)
{
	DBG("pause requested\n");
	if (kali_speaking) {
		DBG("Kali doesn't support pause, stopping\n");

		module_stop();

		return -1;
	} else {
		return 0;
	}
}

int module_close(void)
{

	DBG("kali: close()\n");

	DBG("Stopping speech");
	if (kali_speaking) {
		module_stop();
	}

	quitteAnalyse();
	quitteTrans();
	quitteParle();
	quitteGlobal();

	DBG("Terminating threads");
	if (module_terminate_thread(kali_speak_thread) != 0)
		return -1;

	sem_destroy(&kali_semaphore);

	return 0;
}

/* Internal functions */

void *_kali_speak(void *nothing)
{
	AudioTrack track;
#if defined(BYTE_ORDER) && (BYTE_ORDER == BIG_ENDIAN)
	AudioFormat format = SPD_AUDIO_BE;
#else
	AudioFormat format = SPD_AUDIO_LE;
#endif
	const AudioTrackKali *wav;
	unsigned int pos;
	char *buf;
	int bytes;
	int ret;

	DBG("kali: speaking thread starting.......\n");

	set_speaking_thread_parameters();

	while (1) {
		sem_wait(&kali_semaphore);
		DBG("Semaphore on\n");

		kali_stop = 0;
		kali_speaking = 1;

		/* TODO: free(buf) */
		buf = (char *)g_malloc((KaliMaxChunkLength + 1) * sizeof(char));
		pos = 0;
		module_report_event_begin();
		while (1) {
			if (kali_stop) {
				DBG("Stop in child, terminating");
				kali_speaking = 0;
				module_report_event_stop();
				break;
			}
			bytes =
			    module_get_message_part(*kali_message, buf, &pos,
						    KaliMaxChunkLength,
						    KaliDelimiters);

			if (bytes < 0) {
				DBG("End of message");
				kali_speaking = 0;
				module_report_event_end();
				break;
			}

			buf[bytes] = 0;
			DBG("Returned %d bytes from get_part\n", bytes);
			DBG("Text to synthesize is '%s'\n", buf);

			if (kali_pause_requested && (current_index_mark != -1)) {
				DBG("Pause requested in parent, position %d\n",
				    current_index_mark);
				kali_pause_requested = 0;
				kali_position = current_index_mark;
				break;
			}

			if (bytes > 0) {
				DBG("Speaking in child...");

				DBG("Trying to synthesize text");
				MessageKali((unsigned char *)buf);
				while (QueryIndexKali() > 0)
					;
				wav =
				    (const AudioTrackKali *)
				    GetBufMultiKaliStd(0);

				if (wav == NULL) {
					DBG("Stop in child, terminating");
					kali_speaking = 0;
					module_report_event_stop();
					break;
				}

				track.num_samples = wav->num_samples;
				track.num_channels = wav->num_channels;
				track.sample_rate = wav->sample_rate;
				track.bits = wav->bits;
				track.samples = (signed short *)wav->samples;

				DBG("Got %d samples", track.num_samples);
				if (track.samples != NULL) {
					if (kali_stop) {
						DBG("Stop in child, terminating");
						kali_speaking = 0;
						module_report_event_stop();
						break;
					}
					DBG("Playing part of the message");
					ret = module_tts_output(track, format);
					if (ret < 0)
						DBG("ERROR: failed to play the track");
					if (kali_stop) {
						DBG("Stop in child, terminating (s)");
						kali_speaking = 0;
						module_report_event_stop();
						break;
					}
				}
			} else if (bytes == -1) {
				DBG("End of data in speaking thread");
				kali_speaking = 0;
				module_report_event_end();
				break;
			} else {
				kali_speaking = 0;
				module_report_event_end();
				break;
			}

			if (kali_stop) {
				DBG("Stop in child, terminating");
				kali_speaking = 0;
				module_report_event_stop();
				break;
			}
		}
		kali_stop = 0;
		g_free(buf);
	}

	kali_speaking = 0;

	DBG("kali: speaking thread ended.......\n");

	pthread_exit(NULL);
}

static void kali_set_rate(signed int rate)
{
	short speed;

	assert(rate >= -100 && rate <= +100);
	if (rate < 0)
	  speed = GetDebitDefautKaliStd() - rate * (GetDebitMinKaliStd() - GetDebitDefautKaliStd()) / 100;
	else
	  speed = GetDebitDefautKaliStd() + rate * (GetDebitMaxKaliStd() - GetDebitDefautKaliStd()) / 100;
	SetDebitKali(speed);
}

static void kali_set_volume(signed int volume)
{
	short vol;

	assert(volume >= -100 && volume <= +100);
	if (volume < 0)
	  vol = GetVolumeDefautKaliStd() - volume * (GetVolumeMinKaliStd() - GetVolumeDefautKaliStd()) / 100;
	else
	  vol = GetVolumeDefautKaliStd() + volume * (GetVolumeMaxKaliStd() - GetVolumeDefautKaliStd()) / 100;
	SetVolumeKali(vol);
}

static void kali_set_pitch(signed int pitch)
{
	short ptch;

	assert(pitch >= -100 && pitch <= +100);
	if (pitch < 0)
	  ptch = GetHauteurDefautKaliStd() - pitch * (GetHauteurMinKaliStd() - GetHauteurDefautKaliStd()) / 100;
	else
	  ptch = GetHauteurDefautKaliStd() + pitch * (GetHauteurMaxKaliStd() - GetHauteurDefautKaliStd()) / 100;
	SetHauteurKali(ptch);
}

void kali_set_punctuation_mode(SPDPunctuation punct)
{
	switch (punct) {
	case SPD_PUNCT_NONE:
		if (KaliExpandAbbreviations)
			SetModeLectureKali(0);
		else
			SetModeLectureKali(1);
		break;
	case SPD_PUNCT_SOME:
		SetModeLectureKali(2);
		break;
	case SPD_PUNCT_ALL:
		SetModeLectureKali(3);
		break;
	default:
		break;
	}
}

static void kali_set_voice(char *voice)
{
	short i, nlang;
	char *v = voice;

	if (v == NULL)
		v = KaliVoiceParameters;

	for (i = 0; kali_voice_list[i] != NULL; i++) {
		if (strcasecmp(kali_voice_list[i]->name, v) == 0) {
			nlang = GetNLangueVoixKaliStd(i + 1);
			SetLangueKali(nlang);
			SetVoixKali(i + 1);
			break;
		}
	}
}

static SPDVoice **kali_get_voices()
{
	short i;
	SPDVoice **result = NULL;
	short num_voices;
	char *voice;
	short nlang;
	char *language;

	num_voices = GetNbVoixKali();
	DBG("Kali: %d voices total.", num_voices);
	voice = (char *)g_malloc(12);
	language = (char *)g_malloc(9);
	result = g_new0(SPDVoice *, num_voices);

	for (i = 0; i < num_voices; i++) {
		result[i] = g_new0(SPDVoice, 1);
		GetNomVoixKali(i + 1, voice);
		result[i]->name = (char *)g_strdup(voice);
		nlang = GetNLangueVoixKaliStd(i + 1);
		GetNomLangueKali(nlang, language);
		result[i]->language = (char *)g_strdup(language);
		result[i]->variant = NULL;
	}
	result[i] = NULL;

	g_free(voice);
	g_free(language);

	return result;
}
