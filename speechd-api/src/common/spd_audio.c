
/*
 * spd_audio.c -- Spd Audio Output Library
 *
 * Copyright (C) 2004, 2006 Brailcom, o.p.s.
 * Copyright (C) 2019, 2021 Samuel Thibault <samuel.thibault@ens-lyon.org>
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
 * $Id: spd_audio.c,v 1.21 2008-06-09 10:29:12 hanke Exp $
 */

/*
 * spd_audio is a simple realtime audio output library with the capability of
 * playing 8 or 16 bit data, immediate stop and synchronization. This library
 * currently provides OSS, NAS, ALSA and PulseAudio backend. The available backends are
 * specified at compile-time using the directives WITH_OSS, WITH_NAS, WITH_ALSA,
 * WITH_PULSE, WITH_LIBAO but the user program is allowed to switch between them at run-time.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "spd_audio.h"

#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>
#include <errno.h>

#include <pthread.h>

#include <glib.h>
#include <ltdl.h>

static int spd_audio_log_level;
static lt_dlhandle lt_h;

/* Dynamically load a library with RTLD_GLOBAL set.

   This is needed when a dynamically-loaded library has its own plugins
   that call into the parent library.
   Most of the credit for this function goes to Gary Vaughan.
*/
static lt_dlhandle my_dlopenextglobal(const char *filename)
{
	lt_dlhandle handle = NULL;
	lt_dladvise advise;

	if (lt_dladvise_init(&advise))
		return handle;

	if (!lt_dladvise_ext(&advise) && !lt_dladvise_global(&advise))
		handle = lt_dlopenadvise(filename, advise);

	lt_dladvise_destroy(&advise);
	return handle;
}

/* Open the audio device.

   Arguments:
   type -- The requested device. Currently AudioOSS or AudioNAS.
   pars -- and array of pointers to parameters to pass to
           the device backend, terminated by a NULL pointer.
           See the source/documentation of each specific backend.
   error -- a pointer to the string where error description is
           stored in case of failure (returned AudioID == NULL).
           Otherwise will contain NULL.

   Return value:
   Newly allocated AudioID structure that can be passed to
   all other spd_audio functions, or NULL in case of failure.

*/
AudioID *spd_audio_open(const char *name, void **pars, char **error)
{
	AudioID *id;
	spd_audio_plugin_t const *p;
	spd_audio_plugin_t *(*fn) (void);
	gchar *libname;
	int ret;

	/* now check whether dynamic plugin is available */
	ret = lt_dlinit();
	if (ret != 0) {
		*error = (char *)g_strdup_printf("lt_dlinit() failed");
		return (AudioID *) NULL;
	}

	ret = lt_dlsetsearchpath(PLUGIN_DIR);
	if (ret != 0) {
		*error = (char *)g_strdup_printf("lt_dlsetsearchpath() failed");
		return (AudioID *) NULL;
	}

	libname = g_strdup_printf(SPD_AUDIO_LIB_PREFIX "%s", name);
	lt_h = my_dlopenextglobal(libname);
	g_free(libname);
	if (NULL == lt_h) {
		*error =
		    (char *)g_strdup_printf("Cannot open plugin %s. error: %s",
					    name, lt_dlerror());
		return (AudioID *) NULL;
	}

	fn = lt_dlsym(lt_h, SPD_AUDIO_PLUGIN_ENTRY_STR);
	if (NULL == fn) {
		*error = (char *)g_strdup_printf("Cannot find symbol %s",
						 SPD_AUDIO_PLUGIN_ENTRY_STR);
		return (AudioID *) NULL;
	}

	p = fn();
	if (p == NULL || p->name == NULL) {
		*error = (char *)g_strdup_printf("plugin %s not found", name);
		return (AudioID *) NULL;
	}

	id = p->open(pars);
	if (id == NULL) {
		*error =
		    (char *)g_strdup_printf("Couldn't open %s plugin", name);
		return (AudioID *) NULL;
	}

	id->function = p;
#if defined(BYTE_ORDER) && (BYTE_ORDER == BIG_ENDIAN)
	id->format = SPD_AUDIO_BE;
#else
	id->format = SPD_AUDIO_LE;
#endif

	*error = NULL;

	return id;
}

/* Initialize for playing a track on the audio device.

   Arguments:
   id -- the AudioID* of the device returned by spd_audio_open
   track -- a track to play (see spd_audio.h)

   Return value:
   0 if everything is ok, a non-zero value in case of failure.
   See the particular backend documentation or source for the
   meaning of these non-zero values.
*/
int spd_audio_begin(AudioID * id, AudioTrack track, AudioFormat format)
{
	if (!id) {
		fprintf(stderr, "No audio open\n");
		return -1;
	}

	if (!id->function->begin) {
		/* Too bad */
		return 0;
	}

	return id->function->begin(id, track);
}

/* Perform byte-swapping if needed */
static void spd_audio_convert(AudioID * id, AudioTrack track, AudioFormat format)
{
	/* Only perform byte swapping if the driver in use has given us audio in
	   an endian format other than what the running CPU supports. */
	if (format != id->format && track.bits == 16) {
		unsigned char *out_ptr, *out_end, c;
		out_ptr = (unsigned char *)track.samples;
		out_end =
		    out_ptr +
		    track.num_samples * 2 * track.num_channels;
		while (out_ptr < out_end) {
			c = out_ptr[0];
			out_ptr[0] = out_ptr[1];
			out_ptr[1] = c;
			out_ptr += 2;
		}
	}
}

/* Feed a track to the audio device (blocking).

   This can be called several times to feed more audio samples with the same
   configuration, but since this version is blocking, the audio card will
   underrun. Using spd_audio_feed_overlap is thus preferred in that case.

   Arguments:
   id -- the AudioID* of the device returned by spd_audio_open
   track -- a track to play (see spd_audio.h)

   Return value:
   0 if everything is ok, a non-zero value in case of failure.
   See the particular backend documentation or source for the
   meaning of these non-zero values.

   Comment:
   spd_audio_feed() is a blocking function. It returns exactly
   when the given piece of track stopped playing. However, it's possible
   to safely interrupt it using spd_audio_stop() described below.
   (spd_audio_stop() needs to be called from another thread, obviously.)

*/
int spd_audio_feed_sync(AudioID * id, AudioTrack track, AudioFormat format)
{
	if (!id) {
		fprintf(stderr, "No audio open\n");
		return -1;
	}

	spd_audio_convert(id, track, format);

	if (id->function->feed_sync) {
		return id->function->feed_sync(id, track);
	}

	if (id->function->play) {
		return id->function->play(id, track);
	}

	fprintf(stderr,"Play not supported on this device\n");
	return -1;
}

/* Feed a track to the audio device (blocking, with overlapping).

   This can be called several times to feed more audio samples with the same
   configuration, this version is blocking, but with some overlap, so that it
   can be called within the overlapping period without audio card overrun.

   Arguments:
   id -- the AudioID* of the device returned by spd_audio_open
   track -- a track to play (see spd_audio.h)

   Return value:
   0 if everything is ok, a non-zero value in case of failure.
   See the particular backend documentation or source for the
   meaning of these non-zero values.

   Comment:
   spd_audio_feed() is a blocking function. It returns exactly
   when the given piece of track stopped playing. However, it's possible
   to safely interrupt it using spd_audio_stop() described below.
   (spd_audio_stop() needs to be called from another thread, obviously.)

*/
int spd_audio_feed_sync_overlap(AudioID * id, AudioTrack track, AudioFormat format)
{
	if (!id) {
		fprintf(stderr, "No audio open\n");
		return -1;
	}

	spd_audio_convert(id, track, format);

	if (id->function->feed_sync_overlap) {
		return id->function->feed_sync_overlap(id, track);
	}

	if (id->function->feed_sync) {
		return id->function->feed_sync(id, track);
	}

	if (id->function->play) {
		return id->function->play(id, track);
	}

	fprintf(stderr,"Play not supported on this device\n");
	return -1;
}

/* Finish playing a track on the audio device.

   Arguments:
   id -- the AudioID* of the device returned by spd_audio_open

   Return value:
   0 if everything is ok, a non-zero value in case of failure.
   See the particular backend documentation or source for the
   meaning of these non-zero values.
*/
int spd_audio_end(AudioID * id)
{
	if (!id) {
		fprintf(stderr, "No audio open\n");
		return -1;
	}

	if (!id->function->end) {
		/* Too bad */
		return 0;
	}

	return id->function->end(id);
}

/* Play a track on the audio device (blocking).

   Arguments:
   id -- the AudioID* of the device returned by spd_audio_open
   track -- a track to play (see spd_audio.h)

   Return value:
   0 if everything is ok, a non-zero value in case of failure.
   See the particular backend documentation or source for the
   meaning of these non-zero values.

   Comment:
   spd_audio_play() is a blocking function. It returns exactly
   when the given track stopped playing. However, it's possible
   to safely interrupt it using spd_audio_stop() described below.
   (spd_audio_stop() needs to be called from another thread, obviously.)

*/
int spd_audio_play(AudioID * id, AudioTrack track, AudioFormat format)
{
	int ret;

	ret = spd_audio_begin(id, track, format);
	if (ret)
		return ret;

	ret = spd_audio_feed_sync(id, track, format);
	if (ret)
		return ret;

	return spd_audio_end(id);
}

/* Stop playing the current track on device id

Arguments:
   id -- the AudioID* of the device returned by spd_audio_open

Return value:
   0 if everything is ok, a non-zero value in case of failure.
   See the particular backend documentation or source for the
   meaning of these non-zero values.

Comment:
   spd_audio_stop() safely interrupts spd_audio_play() when called
   from another thread. It shouldn't cause any clicks or unwanted
   effects in the sound output.

   It's safe to call spd_audio_stop() even if the device isn't playing
   any track. In that case, it does nothing. However, there is a danger
   when using spd_audio_stop(). Since you must obviously do it from
   another thread than where spd_audio_play is running, you must make
   yourself sure that the device is still open and the id you pass it
   is valid and will be valid until spd_audio_stop returns. In other words,
   you should use some mutex or other synchronization device to be sure
   spd_audio_close isn't called before or during spd_audio_stop execution.
*/

int spd_audio_stop(AudioID * id)
{
	int ret;
	if (id && id->function->stop) {
		ret = id->function->stop(id);
	} else {
		fprintf(stderr, "Stop not supported on this device\n");
		return -1;
	}
	return ret;
}

/* Close the audio device id

Arguments:
   id -- the AudioID* of the device returned by spd_audio_open

Return value:
   0 if everything is ok, a non-zero value in case of failure.

Comments:

   Please make sure no other spd_audio function with this device id
   is running in another threads. See spd_audio_stop() for detailed
   description of possible problems.
*/
int spd_audio_close(AudioID * id)
{
	int ret = 0;
	if (id && id->function->close) {
		ret = (id->function->close(id));
	}

	if (NULL != lt_h) {
		lt_dlclose(lt_h);
		lt_h = NULL;
		lt_dlexit();
	}

	return ret;
}

/* Set volume for playing tracks on the device id

Arguments:
   id -- the AudioID* of the device returned by spd_audio_open
   volume -- a value in the range <-100:100> where -100 means the
             least volume (probably silence), 0 the default volume
	     and +100 the highest volume possible to make on that
	     device for a single flow (i.e. not using mixer).

Return value:
   0 if everything is ok, a non-zero value in case of failure.
   See the particular backend documentation or source for the
   meaning of these non-zero values.

Comments:

   In case of /dev/dsp, it's not possible to set volume for
   the particular flow. For that reason, the value 0 means
   the volume the track was recorded on and each smaller value
   means less volume (since this works by deviding the samples
   in the track by a constant).
*/
int spd_audio_set_volume(AudioID * id, int volume)
{
	if ((volume > 100) || (volume < -100)) {
		fprintf(stderr, "Requested volume out of range");
		return -1;
	}
	if (id == NULL) {
		fprintf(stderr, "audio id is NULL in spd_audio_set_volume\n");
		return -1;
	}
	id->volume = volume;
	return 0;
}

void spd_audio_set_loglevel(AudioID * id, int level)
{
	if (level) {
		spd_audio_log_level = level;
		if (id != 0 && id->function != 0)
			id->function->set_loglevel(level);
	}
}

char const *spd_audio_get_playcmd(AudioID * id)
{
	if (id != 0 && id->function != 0) {
		return id->function->get_playcmd();
	}
	return NULL;
}
