
/*
 * pulse.c -- The simple pulseaudio backend for the spd_audio library.
 *
 * Copyright 2007-2009 Gilles Casse <gcasse@oralux.org>
 * Copyright 2008-2010 Brailcom, o.p.s
 * Copyright 2008-2015 Luke Yelavich <luke.yelavich@canonical.com>
 * Copyright 2009 Rui Batista <ruiandrebatista@gmail.com>
 * Copyright 2009 Marco Skambraks <marco@openblinux.de>
 * Copyright 2010 Andrei Kholodnyi <Andrei.Kholodnyi@gmail.com>
 * Copyright 2010 Christopher Brannon <cmbrannon79@gmail.com>
 * Copyright 2010-2011 William Hubbs <w.d.hubbs@gmail.com>
 * Copyright 2015 Jeremy Whiting <jpwhiting@kde.org>
 * Copyright 2018-2020 Samuel Thibault <samuel.thibault@ens-lyon.org>
 *
 * Copied from Luke Yelavich's libao.c driver, and merged with code from
 * Marco's ao_pulse.c driver, by Bill Cox, Dec 21, 2009.
 *
 * Minor changes be Rui Batista <rui.batista@ist.utl.pt> to configure settings through speech-dispatcher configuration files
 * Date: Dec 22, 2009
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
 */

#define _GNU_SOURCE

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>
#include <glib.h>

#include <pulse/simple.h>
#include <pulse/error.h>

#define SPD_AUDIO_PLUGIN_ENTRY spd_pulse_LTX_spd_audio_plugin_get
#include <spd_audio_plugin.h>

typedef struct {
	AudioID id;
	pa_simple *pa_simple;
	char *pa_server;
	char *pa_device;
	char *pa_name;
	int pa_min_audio_length;	// in ms
	volatile int pa_stop_playback;
	int pa_current_rate;	// Sample rate for currently PA connection
	int pa_current_bps;	// Bits per sample rate for currently PA connection
	int pa_current_channels;	// Number of channels for currently PA connection
} spd_pulse_id_t;

/* send a packet of XXX bytes to the sound device */
#define PULSE_SEND_BYTES 256

/* Initial values, most often what synths will requests */
#define DEF_RATE 44100
#define DEF_CHANNELS 1
#define DEF_BYTES_PER_SAMPLE 2

/* This is the smallest audio sound we are expected to play immediately without buffering. */
/* Changed to define on config file. Default is the same. */
/* Default to 10 ms of latency */
#define DEFAULT_PA_MIN_AUDIO_LENGTH 10

static int pulse_log_level;
static char const *pulse_play_cmd = "paplay -n speech-dispatcher-generic";

#define MSG(level, arg...) \
	if(level <= pulse_log_level){ \
		time_t t; \
		struct timeval tv; \
		char *tstr; \
		t = time(NULL); \
		tstr = g_strdup(ctime(&t)); \
		tstr[strlen(tstr)-1] = 0; \
		gettimeofday(&tv,NULL); \
		fprintf(stderr," %s [%d]",tstr, (int) tv.tv_usec); \
		fprintf(stderr," Pulse: "); \
		fprintf(stderr,arg); \
		fprintf(stderr,"\n"); \
		fflush(stderr); \
		g_free(tstr); \
	}

#define ERR(arg...) \
	{ \
		time_t t; \
		struct timeval tv; \
		char *tstr; \
		t = time(NULL); \
		tstr = g_strdup(ctime(&t)); \
		tstr[strlen(tstr)-1] = 0; \
		gettimeofday(&tv,NULL); \
		fprintf(stderr," %s [%d]",tstr, (int) tv.tv_usec); \
		fprintf(stderr," Pulse ERROR: "); \
		fprintf(stderr,arg); \
		fprintf(stderr,"\n"); \
		fflush(stderr); \
		g_free(tstr); \
	}

static int _pulse_open(spd_pulse_id_t * id, int sample_rate,
		       int num_channels, int bytes_per_sample)
{
	pa_buffer_attr buffAttr;
	pa_sample_spec ss;
	int error;
	char *client_name;

	ss.rate = sample_rate;
	ss.channels = num_channels;
	if (bytes_per_sample == 2) {
		switch (id->id.format) {
		case SPD_AUDIO_LE:
			ss.format = PA_SAMPLE_S16LE;
			break;
		case SPD_AUDIO_BE:
			ss.format = PA_SAMPLE_S16BE;
			break;
		}
	} else {
		ss.format = PA_SAMPLE_U8;
	}

	/* Set prebuf to one sample so that keys are spoken as soon as typed rather than delayed until the next key pressed */
	buffAttr.maxlength = (uint32_t) - 1;
	//buffAttr.tlength = (uint32_t)-1; - this is the default, which causes key echo to not work properly.
	buffAttr.tlength = id->pa_min_audio_length * sample_rate * num_channels * bytes_per_sample / 1000;
	buffAttr.prebuf = (uint32_t) - 1;
	buffAttr.minreq = (uint32_t) - 1;
	buffAttr.fragsize = (uint32_t) - 1;

	if (!id->pa_name ||
	    asprintf(&client_name, "speech-dispatcher-%s", id->pa_name) < 0)
		client_name = strdup("speech-dispatcher");

	/* Open new connection */
	if (!
	    (id->pa_simple =
	     pa_simple_new(id->pa_server, client_name,
			   PA_STREAM_PLAYBACK, id->pa_device, "playback",
			   &ss, NULL, &buffAttr, &error))) {
		fprintf(stderr, __FILE__ ": pa_simple_new() failed: %s\n",
			pa_strerror(error));
		free(client_name);
		return 1;
	}
	free(client_name);
	return 0;
}

/* Close the connection to the server.  Does not free the AudioID struct. */
/* Usable in pulse_play, which closes connections on failure or */
/* changes in audio parameters. */
static void pulse_connection_close(spd_pulse_id_t * pulse_id)
{
	if (pulse_id->pa_simple != NULL) {
		pa_simple_free(pulse_id->pa_simple);
		pulse_id->pa_simple = NULL;
	}
}

static AudioID *pulse_open(void **pars)
{
	spd_pulse_id_t *pulse_id;
	int ret;

	if (pars[3] == NULL) {
		ERR("Can't open Pulse sound output, missing parameters in argument.");
		return NULL;
	}

	pulse_id = (spd_pulse_id_t *) g_malloc(sizeof(spd_pulse_id_t));

	/* Select an Endianness for the initial connection. */
#if defined(BYTE_ORDER) && (BYTE_ORDER == BIG_ENDIAN)
	pulse_id->id.format = SPD_AUDIO_BE;
#else
	pulse_id->id.format = SPD_AUDIO_LE;
#endif
	pulse_id->pa_simple = NULL;
	pulse_id->pa_server = NULL;
	pulse_id->pa_device = (char *)pars[3];
	pulse_id->pa_name = (char *)pars[5];
	pulse_id->pa_min_audio_length = DEFAULT_PA_MIN_AUDIO_LENGTH;

	pulse_id->pa_current_rate = -1;
	pulse_id->pa_current_bps = -1;
	pulse_id->pa_current_channels = -1;

	if (!strcmp(pulse_id->pa_device, "default")) {
		pulse_id->pa_device = NULL;
	}

	if (pars[4] != NULL && atoi(pars[4]) != 0)
		pulse_id->pa_min_audio_length = atoi(pars[4]);

	pulse_id->pa_stop_playback = 0;

	ret = _pulse_open(pulse_id, DEF_RATE, DEF_CHANNELS, DEF_BYTES_PER_SAMPLE);
	if (ret) {
		g_free(pulse_id);
		pulse_id = NULL;
	}

	return (AudioID *) pulse_id;
}

static int pulse_play(AudioID * id, AudioTrack track)
{
	int bytes_per_sample;
	int num_bytes;
	int outcnt = 0;
	signed short *output_samples;
	int i;
	int error;
	spd_pulse_id_t *pulse_id = (spd_pulse_id_t *) id;

	if (id == NULL) {
		return -1;
	}
	if (track.samples == NULL || track.num_samples <= 0) {
		return 0;
	}
	MSG(4, "Starting playback\n");
	/* Choose the correct format */
	if (track.bits == 16) {
		bytes_per_sample = 2;
	} else if (track.bits == 8) {
		bytes_per_sample = 1;
	} else {
		ERR("ERROR: Unsupported sound data format, track.bits = %d\n",
		    track.bits);
		return -1;
	}
	output_samples = track.samples;
	num_bytes = track.num_samples * bytes_per_sample;

	/* Check if the current connection has suitable parameters for this track */
	if (pulse_id->pa_current_rate != track.sample_rate
	    || pulse_id->pa_current_bps != track.bits
	    || pulse_id->pa_current_channels != track.num_channels) {
		MSG(4, "Reopening connection due to change in track parameters sample_rate:%d bps:%d channels:%d\n", track.sample_rate, track.bits, track.num_channels);
		/* Close old connection if any */
		pulse_connection_close(pulse_id);
		/* Open a new connection */
		error = _pulse_open(pulse_id, track.sample_rate, track.num_channels,
			    bytes_per_sample);
		if (error) {
			pulse_id->pa_current_rate = -1;
			pulse_id->pa_current_bps = -1;
			pulse_id->pa_current_channels = -1;
			return -1;
		}
		/* Keep track of current connection parameters */
		pulse_id->pa_current_rate = track.sample_rate;
		pulse_id->pa_current_bps = track.bits;
		pulse_id->pa_current_channels = track.num_channels;
	}
	MSG(4, "bytes to play: %d, (%f secs)\n", num_bytes,
	    (((float)(num_bytes) / 2) / (float)track.sample_rate));
	pulse_id->pa_stop_playback = 0;
	outcnt = 0;
	i = 0;
	while ((outcnt < num_bytes) && !pulse_id->pa_stop_playback) {
		if ((num_bytes - outcnt) > PULSE_SEND_BYTES) {
			i = PULSE_SEND_BYTES;
		} else {
			i = (num_bytes - outcnt);
		}
		if (pa_simple_write
		    (pulse_id->pa_simple, ((char *)output_samples) + outcnt, i,
		     &error) < 0) {
			pa_simple_drain(pulse_id->pa_simple, NULL);
			pulse_connection_close(pulse_id);
			MSG(4, "ERROR: Audio: pulse_play(): %s - closing device - re-open it in next run\n", pa_strerror(error));
			break;
		} else {
			MSG(5, "Pulse: wrote %u bytes\n", i);
		}
		outcnt += i;
	}
	return 0;
}

/* stop the pulse_play() loop */
static int pulse_stop(AudioID * id)
{
	spd_pulse_id_t *pulse_id = (spd_pulse_id_t *) id;

	pulse_id->pa_stop_playback = 1;
	return 0;
}

static int pulse_close(AudioID * id)
{
	spd_pulse_id_t *pulse_id = (spd_pulse_id_t *) id;
	pulse_connection_close(pulse_id);
	g_free(pulse_id);
	id = NULL;

	return 0;
}

static int pulse_set_volume(AudioID * id, int volume)
{
	return 0;
}

static void pulse_set_loglevel(int level)
{
	if (level) {
		pulse_log_level = level;
	}
}

static char const *pulse_get_playcmd(void)
{
	return pulse_play_cmd;
}

/* Provide the pulse backend. */
static spd_audio_plugin_t pulse_functions = {
	"pulse",
	pulse_open,
	pulse_play,
	pulse_stop,
	pulse_close,
	pulse_set_volume,
	pulse_set_loglevel,
	pulse_get_playcmd
};

spd_audio_plugin_t *pulse_plugin_get(void)
{
	return &pulse_functions;
}

spd_audio_plugin_t *SPD_AUDIO_PLUGIN_ENTRY(void)
    __attribute__ ((weak, alias("pulse_plugin_get")));

#undef MSG
#undef ERR
