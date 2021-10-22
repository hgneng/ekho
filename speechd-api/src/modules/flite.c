
/*
 * flite.c - Speech Dispatcher backend for Flite (Festival Lite)
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
 * $Id: flite.c,v 1.59 2008-06-09 10:38:02 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <flite/flite.h>

#include <speechd_types.h>

#include "module_utils.h"

#define MODULE_NAME     "flite"
#define MODULE_VERSION  "0.6"

#define DEBUG_MODULE 1
DECLARE_DEBUG();

/* Thread and process control */
static int flite_speaking = 0;

static char *buf;

static signed int flite_volume = 0;

/* Internal functions prototypes */
static void flite_set_rate(signed int rate);
static void flite_set_pitch(signed int pitch);
static void flite_set_volume(signed int pitch);

/* Voice */
static cst_voice *flite_voice;

static int flite_stop = 0;

MOD_OPTION_1_INT(FliteMaxChunkLength);
MOD_OPTION_1_STR(FliteDelimiters);

/* Public functions */

int module_load(void)
{
	INIT_SETTINGS_TABLES();

	REGISTER_DEBUG();

	MOD_OPTION_1_INT_REG(FliteMaxChunkLength, 300);
	MOD_OPTION_1_STR_REG(FliteDelimiters, ".");

	return 0;
}

int module_init(char **status_info)
{
	DBG("Module init");

	module_audio_set_server();

	*status_info = NULL;

	/* Init flite and register a new voice */
	flite_init();

#ifdef HAVE_REGISTER_CMU_US_KAL16
	cst_voice *register_cmu_us_kal16();	/* This isn't declared in any headers. */
	flite_voice = register_cmu_us_kal16();
#else
	cst_voice *register_cmu_us_kal();
	flite_voice = register_cmu_us_kal();
#endif /* HAVE_REGISTER_CMU_US_KAL16 */

	if (flite_voice == NULL) {
		DBG("Couldn't register the basic kal voice.\n");
		*status_info = g_strdup("Can't register the basic kal voice. "
					"Currently only kal is supported. Seems your FLite "
					"installation is incomplete.");
		return -1;
	}

	DBG("FliteMaxChunkLength = %d\n", FliteMaxChunkLength);
	DBG("FliteDelimiters = %s\n", FliteDelimiters);

	flite_speaking = 0;

	buf = (char *)g_malloc((FliteMaxChunkLength + 1) * sizeof(char));

	*status_info = g_strdup("Flite initialized successfully.");

	return 0;
}

SPDVoice **module_list_voices(void)
{
	return NULL;
}

void module_speak_sync(const gchar * data, size_t len, SPDMessageType msgtype)
{
	DBG("Requested data: |%s|\n", data);

	if (flite_speaking) {
		module_speak_error();
		DBG("Speaking when requested to write");
		return;
	}

	flite_speaking = 1;

	AudioTrack track;
#if defined(BYTE_ORDER) && (BYTE_ORDER == BIG_ENDIAN)
	AudioFormat format = SPD_AUDIO_BE;
#else
	AudioFormat format = SPD_AUDIO_LE;
#endif
	cst_wave *wav;
	unsigned int pos, first;
	int bytes;
	char *flite_message;

	flite_message = module_strip_ssml(data);
	/* TODO: use a generic engine for SPELL, CHAR, KEY */

	module_speak_ok();

	/* Setting voice */
	UPDATE_PARAMETER(rate, flite_set_rate);
	UPDATE_PARAMETER(volume, flite_set_volume);
	UPDATE_PARAMETER(pitch, flite_set_pitch);

	pos = 0;
	first = 1;

	module_report_event_begin();
	while (1) {
		/* Process server events in case we were told to stop in between */
		module_process(STDIN_FILENO, 0);

		if (flite_stop) {
			DBG("Stop in child, terminating");
			module_report_event_stop();
			break;
		}

		bytes =
		    module_get_message_part(flite_message, buf, &pos,
					    FliteMaxChunkLength,
					    FliteDelimiters);

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
		wav = flite_text_to_wave(buf, flite_voice);

		if (wav == NULL) {
			DBG("Stop in child, terminating");
			module_report_event_stop();
			break;
		}

		track.num_samples = wav->num_samples;
		track.num_channels = wav->num_channels;
		track.sample_rate = wav->sample_rate;
		track.bits = 16;
		track.samples = wav->samples;

		if (first) {
			module_strip_head_silence(&track);
			first = 0;
		}
		if (flite_message[pos] == 0)
			module_strip_tail_silence(&track);

		DBG("Got %d samples", track.num_samples);
		if (track.samples != NULL) {
			DBG("Sending part of the message");
			module_tts_output_server(&track, format);
		}
		delete_wave(wav);
	}
	g_free(flite_message);
	flite_speaking = 0;
	flite_stop = 0;
}

int module_stop(void)
{
	DBG("flite: stop()\n");

	if (flite_speaking)
		flite_stop = 1;

	return 0;
}

size_t module_pause(void)
{
	DBG("pause requested\n");
	if (flite_speaking) {
		DBG("Flite doesn't support pause, stopping\n");

		module_stop();

		return -1;
	} else {
		return 0;
	}
}

int module_close(void)
{

	DBG("flite: close()\n");

	DBG("Stopping speech");
	if (flite_speaking) {
		module_stop();
	}

	g_free(flite_voice);
	g_free(buf);

	return 0;
}

/* Internal functions */

static void flite_set_rate(signed int rate)
{
	float stretch = 1;

	assert(rate >= -100 && rate <= +100);
	if (rate < 0)
		stretch -= ((float)rate) / 50;
	if (rate > 0)
		stretch -= ((float)rate) / 175;
	feat_set_float(flite_voice->features, "duration_stretch", stretch);
}

static void flite_set_volume(signed int volume)
{
	assert(volume >= -100 && volume <= +100);
	flite_volume = volume;
}

static void flite_set_pitch(signed int pitch)
{
	float f0;

	assert(pitch >= -100 && pitch <= +100);
	f0 = (((float)pitch) * 0.8) + 100;
	feat_set_float(flite_voice->features, "int_f0_target_mean", f0);
}
