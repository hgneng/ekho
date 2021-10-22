

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <kali/Kali/kali.h>

#include <speechd_types.h>

#include "module_utils.h"

#define MODULE_NAME     "kali"
#define MODULE_VERSION  "0.0"
#define DEBUG_MODULE 1
DECLARE_DEBUG();

/* Thread and process control */
static int kali_speaking = 0;

static char *buf;

signed int kali_volume = 0;
SPDVoice **kali_voice_list = NULL;

/* Internal functions prototypes */
static void kali_set_rate(signed int rate);
static void kali_set_pitch(signed int pitch);
static void kali_set_volume(signed int volume);
static void kali_set_punctuation_mode(SPDPunctuation punct);
static void kali_set_voice(char *voice);

static SPDVoice **kali_get_voices();

static int kali_stop = 0;

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

int module_init(char **status_info)
{
	DBG("Module init");

	module_audio_set_server();

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

	kali_speaking = 0;

	buf = (char *)g_malloc((KaliMaxChunkLength + 1) * sizeof(char));

	*status_info = g_strdup("Kali initialized successfully.");

	return 0;
}

SPDVoice **module_list_voices(void)
{
	return kali_voice_list;
}

void module_speak_sync(const char * data, size_t len, SPDMessageType msgtype)
{
	DBG("Requested data: |%s|\n", data);

	if (kali_speaking) {
		module_speak_error();
		DBG("Speaking when requested to write");
		return;
	}

	kali_speaking = 1;
	kali_stop = 0;

	AudioTrack track;
#if defined(BYTE_ORDER) && (BYTE_ORDER == BIG_ENDIAN)
	AudioFormat format = SPD_AUDIO_BE;
#else
	AudioFormat format = SPD_AUDIO_LE;
#endif
	const AudioTrackKali *wav;
	char *kali_message;

	kali_message = module_strip_ssml(data);

	module_speak_ok();

	/* Setting voice */
	UPDATE_PARAMETER(rate, kali_set_rate);
	UPDATE_PARAMETER(volume, kali_set_volume);
	UPDATE_PARAMETER(pitch, kali_set_pitch);
	UPDATE_PARAMETER(punctuation_mode, kali_set_punctuation_mode);
	UPDATE_STRING_PARAMETER(voice.name, kali_set_voice);
	kali_set_voice(msg_settings.voice.name);

	unsigned int pos;
	int bytes;

	pos = 0;
	module_report_event_begin();
	while (1) {
		/* Process server events in case we were told to stop in between */
		module_process(STDIN_FILENO, 0);

		if (kali_stop) {
			DBG("Stop in child, terminating");
			module_report_event_stop();
			break;
		}

		bytes =
		    module_get_message_part(kali_message, buf, &pos,
					    KaliMaxChunkLength,
					    KaliDelimiters);

		if (bytes < 0) {
			DBG("End of message");
			module_report_event_end();
			break;
		}

		if (bytes == 0) {
			DBG("No data");
			module_report_event_end();
			break;
		}

		buf[bytes] = 0;
		DBG("Returned %d bytes from get_part\n", bytes);
		DBG("Text to synthesize is '%s'\n", buf);

		DBG("Trying to synthesize text");
		MessageKali((unsigned char *)buf);
		DBG("Waiting for synthesis");
		while (QueryIndexKali() > 0)
			usleep(1000);
		wav =
		    (const AudioTrackKali *)
		    GetBufMultiKaliStd(0);
		DBG("Got buffer");

		if (wav == NULL) {
			DBG("Stop in child, terminating");
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
			DBG("Playing part of the message");
			module_tts_output_server(&track, format);
		}
	}
	g_free(kali_message);
	kali_speaking = 0;
}

int module_stop(void)
{
	DBG("kali: stop()\n");

	kali_stop = 1;

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

	return 0;
}

/* Internal functions */

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
	case SPD_PUNCT_MOST:
		/* XXX approximation */
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
	static char voice[128];
	short nlang;
	static char language[128];

	num_voices = GetNbVoixKali();
	DBG("Kali: %d voices total.", num_voices);
	result = g_new0(SPDVoice *, num_voices+1);

	for (i = 0; i < num_voices; i++) {
		result[i] = g_new0(SPDVoice, 1);
		GetNomVoixKali(i + 1, voice);
		if (!memchr(voice, '\0', sizeof(voice))) {
			voice[sizeof(voice)-1] = '\0';
			MSG(1, "Kali: voice name overflow! truncated to %s but possibly overflowed memory...", voice);
		}
		result[i]->name = (char *)g_strdup(voice);

		nlang = GetNLangueVoixKaliStd(i + 1);
		GetNomLangueKali(nlang, language);
		if (!memchr(language, '\0', sizeof(language))) {
			language[sizeof(language)-1] = '\0';
			MSG(1, "Kali: language name overflow! truncated to %s but possibly overflowed memory...", language);
		}
		result[i]->language = (char *)g_strdup(language);

		result[i]->variant = NULL;
	}
	result[i] = NULL;

	return result;
}
