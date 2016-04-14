
/*
 * pulse.c -- The simple pulseaudio backend for the spd_audio library.
 *
 * Based on libao.c from Marco Skambraks <marco@openblinux.de>
 * Date:  2009-12-15
 *
 * Copied from Luke Yelavich's libao.c driver, and merged with code from
 * Marco's ao_pulse.c driver, by Bill Cox, Dec 21, 2009.
 *
 * Minor changes be Rui Batista <rui.batista@ist.utl.pt> to configure settings through speech-dispatcher configuration files
 * Date: Dec 22, 2009
 *
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU Leser General Public License as published by the Free
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
 */

#include <sys/time.h>
#include <time.h>
#include <string.h>
#include <pulse/simple.h>
#include <pulse/error.h>
#include <stdarg.h>

/* Switch this on to debug, see output log location in MSG() */
//#define DEBUG_PULSE

/* send a packet of XXX bytes to the sound device */
#define PULSE_SEND_BYTES 256

/* This is the smallest audio sound we are expected to play immediately without buffering. */
/* Changed to define on config file. Default is the same. */
#define DEFAULT_PA_MIN_AUDIO_LENgTH 100

static FILE *pulseDebugFile = NULL;

/* Write to /tmp/speech-dispatcher-pulse.log */
#ifdef DEBUG_PULSE
static void MSG(char *message, ...)
{
    va_list ap;

    if(pulseDebugFile == NULL) {
        pulseDebugFile = fopen ("/tmp/speech-dispatcher-pulse.log", "w");
    }
    va_start(ap, message);
    vfprintf(pulseDebugFile, message, ap);
    va_end(ap);
    fflush(pulseDebugFile);
}
#else
static void MSG(char *message, ...)
{
}
#endif

int pulse_stop (AudioID * id);

int pulse_open (AudioID * id, void **pars);

int pulse_close (AudioID * id);

int pulse_play (AudioID * id, AudioTrack track);

int pulse_set_volume (AudioID * id, int volume);


static int _pulse_open(AudioID * id, int sample_rate, int num_channels,
		       int bytes_per_sample)
{
  pa_buffer_attr buffAttr;
  pa_sample_spec ss;  
  int error;

  ss.rate = sample_rate;
  ss.channels = num_channels;
  if(bytes_per_sample == 2) {
      switch (spd_audio_endian) {
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
  buffAttr.maxlength = (uint32_t)-1;
  //buffAttr.tlength = (uint32_t)-1; - this is the default, which causes key echo to not work properly.
  buffAttr.tlength = id->pa_min_audio_length;
  buffAttr.prebuf = (uint32_t)-1;
  buffAttr.minreq = (uint32_t)-1;
  buffAttr.fragsize = (uint32_t)-1;
  /* Open new connection */
  if(!(id->pa_simple = pa_simple_new(id->pa_server, "speech-dispatcher", PA_STREAM_PLAYBACK,
				     NULL, "playback", &ss, NULL, &buffAttr, &error))) {
    fprintf(stderr, __FILE__": pa_simple_new() failed: %s\n", pa_strerror(error));
    return 1;
  }
  return 0;
}

int pulse_open (AudioID * id, void **pars)
{
    id->pa_simple = NULL;
    id->pa_server = (char *)pars[0];

    id->pa_current_rate = -1;
    id->pa_current_bps = -1;
    id->pa_current_channels = -1;
    
    if(! strcmp(id->pa_server, "default")) {
    id->pa_server = NULL;
    }

    id->pa_min_audio_length = pars[1]?(int)pars[1] : DEFAULT_PA_MIN_AUDIO_LENgTH;
    id->pa_stop_playback = 0;

    return _pulse_open(id, 44100, 1, 2);
}

int pulse_play (AudioID * id, AudioTrack track)
{
    int bytes_per_sample;
    int num_bytes;
    int outcnt = 0;
    signed short *output_samples;
    int i;
    int error;

    if(id == NULL) {
        return -1;
    }
    if(track.samples == NULL || track.num_samples <= 0) {
        return 0;
    }
    MSG("Starting playback\n");
    /* Choose the correct format */
    if(track.bits == 16){	
        bytes_per_sample = 2;
    } else if(track.bits == 8){
        bytes_per_sample = 1;
    } else {
        MSG("ERROR: Unsupported sound data format, track.bits = %d\n", track.bits);
        return -1;
    }
    output_samples = track.samples;
    num_bytes = track.num_samples * bytes_per_sample;

    /* Check if the current connection has suitable parameters for this track */
    if(id->pa_current_rate != track.sample_rate || id->pa_current_bps != track.bits
       || id->pa_current_channels != track.num_channels) {
        MSG("Reopenning connection due to change in track parameters sample_rate:%d bps:%d channels:%d\n",
	    track.sample_rate, track.bits, track.num_channels);
	/* Close old connection if any */
        pulse_close(id);
	/* Open a new connection */
	_pulse_open(id, track.sample_rate, track.num_channels, bytes_per_sample);
	/* Keep track of current connection parameters */
	id->pa_current_rate = track.sample_rate;
	id->pa_current_bps = track.bits;
	id->pa_current_channels = track.num_channels;
    }
    MSG("bytes to play: %d, (%f secs)\n", num_bytes, (((float) (num_bytes) / 2) / (float) track.sample_rate));
    id->pa_stop_playback = 0;
    outcnt = 0;
    i = 0;
    while((outcnt < num_bytes) && !id->pa_stop_playback) {
       if((num_bytes - outcnt) > PULSE_SEND_BYTES) {
           i = PULSE_SEND_BYTES;
       } else {
           i = (num_bytes - outcnt);
       }
       if(pa_simple_write(id->pa_simple, ((char *)output_samples) + outcnt, i, &error) < 0) {
           pa_simple_drain(id->pa_simple, NULL);
           pulse_close(id);
           MSG("ERROR: Audio: pulse_play(): %s - closing device - re-open it in next run\n", pa_strerror(error));
           break;
       } else {
           MSG("Pulse: wrote %u bytes\n", i);
       }
       outcnt += i;
    }
    return 0;
}

/* stop the pulse_play() loop */
int pulse_stop (AudioID * id)
{
    id->pa_stop_playback = 1;
    return 0;
}

int pulse_close (AudioID * id)
{
    if(id->pa_simple != NULL) {
        pa_simple_free(id->pa_simple);
        id->pa_simple = NULL;
    }
    return 0;
}

int pulse_set_volume (AudioID * id, int volume)
{
    return 0;
}

/* Provide the pulse backend. */
spd_audio_plugin_t pulse_functions = {pulse_open, pulse_play, pulse_stop, pulse_close, pulse_set_volume};
