/*
 * libao.c -- The libao backend for the spd_audio library.
 *
 * Author: Marco Skambraks <marco@openblinux.de>
 * Date:  2009-12-15
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
#include <ao/ao.h>

/* send a packet of XXX bytes to the sound device */
#define AO_SEND_BYTES 256
/* Put a message into the logfile (stderr) */
#define MSG(level, arg...) \
 if(level <= log_level){ \
     time_t t; \
     struct timeval tv; \
     char *tstr; \
     t = time(NULL); \
     tstr = strdup(ctime(&t)); \
     tstr[strlen(tstr)-1] = 0; \
     gettimeofday(&tv,NULL); \
     fprintf(stderr," %s [%d]",tstr, (int) tv.tv_usec); \
     fprintf(stderr," libao:: "); \
     fprintf(stderr,arg); \
     fprintf(stderr,"\n"); \
     fflush(stderr); \
     xfree(tstr); \
  }

#define ERR(arg...) \
 { \
     time_t t; \
     struct timeval tv; \
     char *tstr; \
     t = time(NULL); \
     tstr = strdup(ctime(&t)); \
     tstr[strlen(tstr)-1] = 0; \
     gettimeofday(&tv,NULL); \
     fprintf(stderr," %s [%d]",tstr, (int) tv.tv_usec); \
     fprintf(stderr," libao ERROR: "); \
     fprintf(stderr,arg); \
     fprintf(stderr,"\n"); \
     fflush(stderr); \
     xfree(tstr); \
  }

int libao_stop (AudioID * id);

int libao_open (AudioID * id, void **pars);

int libao_close (AudioID * id);

int libao_play (AudioID * id, AudioTrack track);

int libao_set_volume (AudioID * id, int volume);

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

ao_device *device = NULL;

static inline void libao_open_handle(int rate, int channels, int bits)
{
  ao_sample_format format = AO_FORMAT_INITIALIZER;

  format.channels = channels;
  format.rate = rate;
  format.bits = bits;
  format.byte_format = AO_FMT_NATIVE;
  device = ao_open_live (default_driver, &format, NULL);

  if (device != NULL)
    current_ao_parameters = format;
}

static inline void libao_close_handle(void)
{
  if (device != NULL)
    {
      ao_close(device);
      device = NULL;
    }
}

int libao_open (AudioID * id, void **pars)
{

  ao_initialize ();
  default_driver = ao_default_driver_id ();
  return 0;
}

int libao_play (AudioID * id, AudioTrack track)
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
  else
   {
     ERR ("Audio: Unrecognized sound data format.\n");
     return -10;
   }
  MSG (3, "Starting playback");
  output_samples = track.samples;
  num_bytes = track.num_samples * bytes_per_sample;

  if ((device == NULL)
      || (track.num_channels != current_ao_parameters.channels)
      || (track.sample_rate != current_ao_parameters.rate)
      || (track.bits != current_ao_parameters.bits))
    {
      libao_close_handle();
      libao_open_handle(track.sample_rate, track.num_channels, track.bits);
    }

  if (device == NULL)
   {
     ERR ("error opening libao dev");
     return -2;
   }
  MSG (3, "bytes to play: %d, (%f secs)", num_bytes,
       (((float) (num_bytes) / 2) / (float) track.sample_rate));

  ao_stop_playback = 0;
  outcnt = 0;
  i = 0;

  while ((outcnt < num_bytes) && !ao_stop_playback)
   {
     if ((num_bytes - outcnt) > AO_SEND_BYTES)
       i = AO_SEND_BYTES;
     else
       i = (num_bytes - outcnt);

     if (!ao_play (device, (char *) output_samples + outcnt, i))
      {
        libao_close_handle();
        ERR ("Audio: ao_play() - closing device - re-open it in next run\n");
        return -1;
      }
     outcnt += i;
   }

  return 0;

}

/* stop the libao_play() loop */
int libao_stop (AudioID * id)
{

  ao_stop_playback = 1;
  return 0;
}

int libao_close (AudioID * id)
{
  libao_close_handle();
  ao_shutdown ();
  id = NULL;
  return 0;
}

int libao_set_volume (AudioID * id, int volume)
{
  return 0;
}

/* Provide the libao backend. */
spd_audio_plugin_t libao_functions =
  { libao_open, libao_play, libao_stop, libao_close, libao_set_volume };

#undef MSG
#undef ERR
