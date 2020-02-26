/*
 * libao.c -- The libao backend for the spd_audio library.
 *
 * Copyright 2009 Luke Yelavich <luke.yelavich@canonical.com>
 * Copyright 2010 Andrei Kholodnyi <Andrei.Kholodnyi@gmail.com>
 * Copyright 2010 Christopher Brannon <cmbrannon79@gmail.com>
 * Copyright 2010-2011 William Hubbs <w.d.hubbs@gmail.com>
 * Copyright 2015 Jeremy Whiting <jpwhiting@kde.org>
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/time.h>
#include <time.h>
#include <string.h>
#include <glib.h>
#include <ao/ao.h>

#define SPD_AUDIO_PLUGIN_ENTRY spd_libao_LTX_spd_audio_plugin_get
#include <spd_audio_plugin.h>

/* send a packet of XXX bytes to the sound device */
#define AO_SEND_BYTES 256
/* Put a message into the logfile (stderr) */
#define MSG(level, arg...) \
	if(level <= libao_log_level){ \
		time_t t; \
		struct timeval tv; \
		char *tstr; \
		t = time(NULL); \
		tstr = g_strdup(ctime(&t)); \
		tstr[strlen(tstr)-1] = 0; \
		gettimeofday(&tv,NULL); \
		fprintf(stderr," %s [%d]",tstr, (int) tv.tv_usec); \
		fprintf(stderr," libao:: "); \
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
		fprintf(stderr," libao ERROR: "); \
		fprintf(stderr,arg); \
		fprintf(stderr,"\n"); \
		fflush(stderr); \
		g_free(tstr); \
	}

/* AO_FORMAT_INITIALIZER is an ao_sample_format structure with zero values
   in all of its fields.  We can guarantee that the fields of a
   stack-allocated ao_sample_format are zeroed by assigning
   AO_FORMAT_INITIALIZER to it.
   This is the most portable way to initialize a stack-allocated struct to
   zero. */
static ao_sample_format AO_FORMAT_INITIALIZER;
static ao_sample_format current_ao_parameters;

static volatile int ao_stop_playback = 0;

static int default_driver;
static int libao_log_level;

ao_device *device = NULL;

static inline void libao_open_handle(int rate, int channels, int bits)
{
	ao_sample_format format = AO_FORMAT_INITIALIZER;

	format.channels = channels;
	format.rate = rate;
	format.bits = bits;
	format.byte_format = AO_FMT_NATIVE;
	device = ao_open_live(default_driver, &format, NULL);

	if (device != NULL)
		current_ao_parameters = format;
}

static inline void libao_close_handle(void)
{
	if (device != NULL) {
		ao_close(device);
		device = NULL;
	}
}

static AudioID *libao_open(void **pars)
{
	AudioID *id;

	id = (AudioID *) g_malloc(sizeof(AudioID));

	ao_initialize();
	default_driver = ao_default_driver_id();
	return id;
}

static int libao_play(AudioID * id, AudioTrack track)
{
	int bytes_per_sample;

	int num_bytes;

	int outcnt = 0;

	signed short *output_samples;

	int i;

	if (id == NULL)
		return -1;
	if (track.samples == NULL || track.num_samples <= 0)
		return 0;

	/* Choose the correct format */
	if (track.bits == 16)
		bytes_per_sample = 2;
	else if (track.bits == 8)
		bytes_per_sample = 1;
	else {
		ERR("Audio: Unrecognized sound data format.\n");
		return -10;
	}
	MSG(3, "Starting playback");
	output_samples = track.samples;
	num_bytes = track.num_samples * bytes_per_sample;

	if ((device == NULL)
	    || (track.num_channels != current_ao_parameters.channels)
	    || (track.sample_rate != current_ao_parameters.rate)
	    || (track.bits != current_ao_parameters.bits)) {
		libao_close_handle();
		libao_open_handle(track.sample_rate, track.num_channels,
				  track.bits);
	}

	if (device == NULL) {
		ERR("error opening libao dev");
		return -2;
	}
	MSG(3, "bytes to play: %d, (%f secs)", num_bytes,
	    (((float)(num_bytes) / 2) / (float)track.sample_rate));

	ao_stop_playback = 0;
	outcnt = 0;
	i = 0;

	while ((outcnt < num_bytes) && !ao_stop_playback) {
		if ((num_bytes - outcnt) > AO_SEND_BYTES)
			i = AO_SEND_BYTES;
		else
			i = (num_bytes - outcnt);

		if (!ao_play(device, (char *)output_samples + outcnt, i)) {
			libao_close_handle();
			ERR("Audio: ao_play() - closing device - re-open it in next run\n");
			return -1;
		}
		outcnt += i;
	}

	return 0;

}

/* stop the libao_play() loop */
static int libao_stop(AudioID * id)
{

	ao_stop_playback = 1;
	return 0;
}

static int libao_close(AudioID * id)
{
	libao_close_handle();
	ao_shutdown();

	g_free(id);
	id = NULL;
	return 0;
}

static int libao_set_volume(AudioID * id, int volume)
{
	return 0;
}

static void libao_set_loglevel(int level)
{
	if (level) {
		libao_log_level = level;
	}
}

static char const *libao_get_playcmd(void)
{
	int driver_id = ao_default_driver_id();
	ao_info *driver_info = ao_driver_info(driver_id);

	if (!strcmp(driver_info->short_name, "oss"))
		return "play";
	else if (!strcmp(driver_info->short_name, "alsa"))
		return "aplay";
	else if (!strcmp(driver_info->short_name, "pulse"))
		return "paplay";

	/* For others we can not know how we are supposed to play.  */
	return NULL;
}

/* Provide the libao backend. */
static spd_audio_plugin_t libao_functions = {
	"libao",
	libao_open,
	libao_play,
	libao_stop,
	libao_close,
	libao_set_volume,
	libao_set_loglevel,
	libao_get_playcmd
};

spd_audio_plugin_t *libao_plugin_get(void)
{
	return &libao_functions;
}

spd_audio_plugin_t *SPD_AUDIO_PLUGIN_ENTRY(void)
    __attribute__ ((weak, alias("libao_plugin_get")));
#undef MSG
#undef ERR
