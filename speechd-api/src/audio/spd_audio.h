
/*
 * spd_audio.h -- The SPD Audio Library Header
 *
 * Copyright (C) 2004 Brailcom, o.p.s.
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
 * along with this package; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA.
 *
 * $Id: spd_audio.h,v 1.21 2008-10-15 17:28:17 hanke Exp $
 */

#ifndef __SPD_AUDIO_H
#define __SPD_AUDIO_H

#include <pthread.h>
#include <sys/types.h>

#ifdef WITH_NAS
#include <audio/audiolib.h>
#include <audio/soundlib.h>
#endif

#ifdef WITH_ALSA
#include <alsa/asoundlib.h>
#endif

#ifdef WITH_PULSE
#include <pulse/simple.h>
#endif

#define AUDIO_BUF_SIZE 4096

typedef enum{AUDIO_OSS = 0, AUDIO_NAS = 1, AUDIO_ALSA=2, AUDIO_PULSE=3, AUDIO_LIBAO=4} AudioOutputType;
typedef enum{SPD_AUDIO_LE, SPD_AUDIO_BE} AudioFormat;

AudioFormat spd_audio_endian;

typedef struct{
    int bits;
    int num_channels;
    int sample_rate;

    int num_samples;
    signed short *samples;
}AudioTrack;

struct spd_audio_plugin;

typedef struct{
    AudioOutputType type;

    int volume;

#ifdef WITH_OSS
    /* OSS specific */
    int fd;
    char* device_name;
    pthread_mutex_t fd_mutex;
    pthread_cond_t pt_cond;
    pthread_mutex_t pt_mutex;
#endif

#ifdef WITH_ALSA
    snd_pcm_t *alsa_pcm;		/* identifier of the ALSA device */
    snd_pcm_hw_params_t *alsa_hw_params;	/* parameters of sound */
    snd_pcm_sw_params_t *alsa_sw_params;	/* parameters of playback */
    snd_pcm_uframes_t alsa_buffer_size;
    pthread_mutex_t alsa_pcm_mutex;	/* mutex to guard the state of the device */
    pthread_mutex_t alsa_pipe_mutex;	/* mutex to guard the stop pipes */
    int alsa_stop_pipe[2];		/* Pipe for communication about stop requests*/
    int alsa_fd_count;		/* Counter of descriptors to poll */
    struct pollfd *alsa_poll_fds; /* Descriptors to poll */
    int alsa_opened; 		/* 1 between snd_pcm_open and _close, 0 otherwise */
    char *alsa_device_name; 	/* the name of the device to open */
#endif

#ifdef WITH_NAS
    AuServer *aud;
    AuFlowID flow;
    pthread_mutex_t flow_mutex;
    pthread_t nas_event_handler;
#endif
#ifdef WITH_PULSE
    pa_simple *pa_simple;
    char *pa_server;
    int pa_min_audio_length;
    volatile int pa_stop_playback;
    int pa_current_rate;  // Sample rate for currently PA connection
    int pa_current_bps; // Bits per sample rate for currently PA connection
    int pa_current_channels; // Number of channels for currently PA connection
#endif

    struct spd_audio_plugin *function;

    int working;
}AudioID;

typedef struct spd_audio_plugin {
    int   (* open)  (AudioID *id, void** pars);
    int   (* play)  (AudioID *id, AudioTrack track);
    int   (* stop)  (AudioID *id);
    int   (* close) (AudioID *id);
    int   (* set_volume) (AudioID *id, int);
} spd_audio_plugin_t;

AudioID* spd_audio_open(AudioOutputType type, void **pars, char **error);

int log_level;

int spd_audio_play(AudioID *id, AudioTrack track, AudioFormat format);

int spd_audio_stop(AudioID *id);

int spd_audio_close(AudioID *id);

int spd_audio_set_volume(AudioID *id, int volume);

void spd_audio_set_loglevel(int level);

#endif /* ifndef #__SPD_AUDIO_H */
