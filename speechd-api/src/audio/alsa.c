
/*
 * alsa.c -- The Advanced Linux Sound System backend for Speech Dispatcher
 *
 * Copyright (C) 2005,2006 Brailcom, o.p.s.
 * Copyright (C) 2019 Samuel Thibault <samuel.thibault@ens-lyon.org>
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
 * $Id: alsa.c,v 1.30 2008-10-15 17:27:32 hanke Exp $
 */

/* NOTE: This module uses the non-blocking write() / poll() approach to
    alsa-lib functions.*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <sys/time.h>
#include <time.h>
#include <pthread.h>
#include <glib.h>

#include <alsa/asoundlib.h>
#include <alsa/pcm.h>

#define SPD_AUDIO_PLUGIN_ENTRY spd_alsa_LTX_spd_audio_plugin_get
#include <spd_audio_plugin.h>

typedef struct {
	AudioID id;
	snd_pcm_t *alsa_pcm;	/* identifier of the ALSA device */
	snd_pcm_hw_params_t *alsa_hw_params;	/* parameters of sound */
	snd_pcm_sw_params_t *alsa_sw_params;	/* parameters of playback */
	snd_pcm_uframes_t alsa_buffer_size;
	pthread_mutex_t alsa_pcm_mutex;	/* mutex to guard the state of the device */
	pthread_mutex_t alsa_pipe_mutex;	/* mutex to guard the stop pipes */
	pthread_cond_t alsa_pipe_cond;	/* mutex to guard the stop pipes */
	int alsa_stop_pipe[2];	/* Pipe for communication about stop requests */
	int stop_requested;	/* Whether we want to stop */
	int alsa_fd_count;	/* Counter of descriptors to poll */
	struct pollfd *alsa_poll_fds;	/* Descriptors to poll */
	int alsa_opened;	/* 1 between snd_pcm_open and _close, 0 otherwise */
	char *alsa_device_name;	/* the name of the device to open */
} spd_alsa_id_t;

static int _alsa_close(spd_alsa_id_t * id);
static int _alsa_open(spd_alsa_id_t * id);

static int xrun(spd_alsa_id_t * id);
static int suspend(spd_alsa_id_t * id);

static int wait_for_poll(spd_alsa_id_t * id, struct pollfd *alsa_poll_fds,
			 unsigned int count, int draining);

#ifndef timersub
#define	timersub(a, b, result) \
	do { \
		(result)->tv_sec = (a)->tv_sec - (b)->tv_sec; \
		(result)->tv_usec = (a)->tv_usec - (b)->tv_usec; \
		if ((result)->tv_usec < 0) { \
			--(result)->tv_sec; \
			(result)->tv_usec += 1000000; \
		} \
	} while (0)
#endif

/* Put a message into the logfile (stderr) */
#define MSG(level, arg...) \
	if(level <= alsa_log_level){ \
		time_t t; \
		struct timeval tv; \
		char *tstr; \
		t = time(NULL); \
		tstr = g_strdup(ctime(&t)); \
		tstr[strlen(tstr)-1] = 0; \
		gettimeofday(&tv,NULL); \
		fprintf(stderr," %s [%d.%06d]",tstr, (int)tv.tv_sec % 10, (int) tv.tv_usec); \
		fprintf(stderr," ALSA: "); \
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
		fprintf(stderr," ALSA ERROR: "); \
		fprintf(stderr,arg); \
		fprintf(stderr,"\n"); \
		fflush(stderr); \
		g_free(tstr); \
	}

static int alsa_log_level;
static char const *alsa_play_cmd = "aplay";

/* I/O error handler */
static int xrun(spd_alsa_id_t * id)
{
	snd_pcm_status_t *status;
	int res;

	if (id == NULL)
		return -1;

	MSG(1, "WARNING: Entering XRUN handler");

	snd_pcm_status_alloca(&status);
	if ((res = snd_pcm_status(id->alsa_pcm, status)) < 0) {
		ERR("status error: %s", snd_strerror(res));

		return -1;
	}
	if (snd_pcm_status_get_state(status) == SND_PCM_STATE_XRUN) {
		struct timeval now, diff, tstamp;
		gettimeofday(&now, 0);
		snd_pcm_status_get_trigger_tstamp(status, &tstamp);
		timersub(&now, &tstamp, &diff);
		MSG(1, "underrun!!! (at least %.3f ms long)",
		    diff.tv_sec * 1000 + diff.tv_usec / 1000.0);
		if ((res = snd_pcm_prepare(id->alsa_pcm)) < 0) {
			ERR("xrun: prepare error: %s", snd_strerror(res));

			return -1;
		}

		return 0;	/* ok, data should be accepted again */
	}
	ERR("read/write error, state = %s",
	    snd_pcm_state_name(snd_pcm_status_get_state(status)));

	return -1;
}

/* I/O suspend handler */
static int suspend(spd_alsa_id_t * id)
{
	int res;

	MSG(1, "WARNING: Entering SUSPEND handler.");

	if (id == NULL)
		return -1;

	while ((res = snd_pcm_resume(id->alsa_pcm)) == -EAGAIN)
		sleep(1);	/* wait until suspend flag is released */

	if (res < 0) {
		if ((res = snd_pcm_prepare(id->alsa_pcm)) < 0) {
			ERR("suspend: prepare error: %s", snd_strerror(res));

			return -1;
		}
	}

	return 0;
}

/* Open the device so that it's ready for playing on the default
   device. Internal function used by the public alsa_open. */
static int _alsa_open(spd_alsa_id_t * id)
{
	int err;

	MSG(1, "Opening ALSA device");
	fflush(stderr);

	/* Open the device */
	if ((err = snd_pcm_open(&id->alsa_pcm, id->alsa_device_name,
				SND_PCM_STREAM_PLAYBACK,
				SND_PCM_NONBLOCK)) < 0) {
		ERR("Cannot open audio device %s (%s)", id->alsa_device_name,
		    snd_strerror(err));
		return -1;
	}

	/* Allocate space for hw_params (description of the sound parameters) */
	/* Allocate space for sw_params (description of the sound parameters) */
	MSG(2, "Allocating new sw_params structure");
	if ((err = snd_pcm_sw_params_malloc(&id->alsa_sw_params)) < 0) {
		ERR("Cannot allocate hardware parameter structure (%s)",
		    snd_strerror(err));
		return -1;
	}

	MSG(1, "Opening ALSA device ... success");

	return 0;
}

/*
   Close the device. Internal function used by public alsa_close.
*/

static int _alsa_close(spd_alsa_id_t * id)
{
	int err;

	MSG(1, "Closing ALSA device");

	pthread_mutex_lock(&id->alsa_pipe_mutex);

	if (id->alsa_opened == 0) {
		pthread_mutex_unlock(&id->alsa_pipe_mutex);
		return 0;
	}

	id->alsa_opened = 0;

	if ((err = snd_pcm_close(id->alsa_pcm)) < 0) {
		MSG(2, "Cannot close ALSA device (%s)", snd_strerror(err));
		pthread_mutex_unlock(&id->alsa_pipe_mutex);
		return -1;
	}

	snd_pcm_sw_params_free(id->alsa_sw_params);

	g_free(id->alsa_poll_fds);
	pthread_mutex_unlock(&id->alsa_pipe_mutex);

	MSG(1, "Closing ALSA device ... success");

	return 0;
}

/* Open ALSA for playback.

  These parameters are passed in pars:
  (char*) pars[0] ... null-terminated string containing the name
                      of the device to be used for sound output
                      on ALSA
  (void*) pars[1] ... =NULL
*/
static AudioID *alsa_open(void **pars)
{
	spd_alsa_id_t *alsa_id;
	int ret;

	if (pars[1] == NULL) {
		ERR("Can't open ALSA sound output, missing parameters in argument.");
		return NULL;
	}

	alsa_id = (spd_alsa_id_t *) g_malloc(sizeof(spd_alsa_id_t));

	pthread_mutex_init(&alsa_id->alsa_pipe_mutex, NULL);
	pthread_cond_init(&alsa_id->alsa_pipe_cond, NULL);

	alsa_id->alsa_opened = 0;

	MSG(1, "Opening ALSA sound output");

	alsa_id->alsa_device_name = g_strdup(pars[1]);

	ret = _alsa_open(alsa_id);
	if (ret) {
		ERR("Cannot initialize Alsa device '%s': Can't open.",
		    alsa_id->alsa_device_name);
		g_free(alsa_id);
		return NULL;
	}

	MSG(1, "Device '%s' initialized successfully.",
	    alsa_id->alsa_device_name);

	return (AudioID *) alsa_id;
}

/* Close ALSA */
static int alsa_close(AudioID * id)
{
	int err;
	spd_alsa_id_t *alsa_id = (spd_alsa_id_t *) id;

	/* Close device */
	if ((err = _alsa_close(alsa_id)) < 0) {
		ERR("Cannot close audio device");
		return -1;
	}
	MSG(1, "ALSA closed.");

	g_free(alsa_id->alsa_device_name);
	g_free(alsa_id);
	id = NULL;

	return 0;
}

/* Wait until ALSA is readdy for more samples or alsa_stop() was called.

Returns 0 if ALSA is ready for more input, +1 if a request to stop
the sound output was received and a negative value on error.  */

int wait_for_poll(spd_alsa_id_t * id, struct pollfd *alsa_poll_fds,
		  unsigned int count, int draining)
{
	unsigned short revents;
	snd_pcm_state_t state;
	int ret;

	//      MSG("Waiting for poll");

	/* Wait for certain events */
	while (1) {
		ret = poll(id->alsa_poll_fds, count, -1);
		//      MSG("wait_for_poll: activity on %d descriptors", ret);

		/* Check for stop request from alsa_stop on the last file
		   descriptors */
		revents = id->alsa_poll_fds[count - 1].revents;
		if (0 != revents) {
			if (revents & POLLIN) {
				MSG(4, "wait_for_poll: stop requested");
				return 1;
			}
		}

		/* Check the first count-1 descriptors for ALSA events */
		snd_pcm_poll_descriptors_revents(id->alsa_pcm,
						 id->alsa_poll_fds, count - 1,
						 &revents);

		/* Ensure we are in the right state */
		state = snd_pcm_state(id->alsa_pcm);
		//      MSG("State after poll returned is %s", snd_pcm_state_name(state));

		if (SND_PCM_STATE_XRUN == state) {
			if (!draining) {
				MSG(1, "WARNING: Buffer underrun detected!");
				if (xrun(id) != 0)
					return -1;
				return 0;
			} else {
				MSG(4, "Poll: Playback terminated");
				return 0;
			}
		}

		if (SND_PCM_STATE_SUSPENDED == state) {
			MSG(1, "WARNING: Suspend detected!");
			if (suspend(id) != 0)
				return -1;
			return 0;
		}

		/* Check for errors */
		if (revents & POLLERR) {
			MSG(4, "wait_for_poll: poll revents says POLLERR");
			return -EIO;
		}

		/* Is ALSA ready for more input? */
		if ((revents & POLLOUT)) {
			MSG(5, "Poll: Ready for more input");
			return 0;
		}
	}
}

#define ERROR_EXIT() do {\
	g_free(track_volume.samples); \
	ERR("alsa_play() abnormal exit"); \
	_alsa_close(alsa_id); \
	return -1; \
} while (0)

/* Configure ALSA playback for the given configuration of track
   But do not play anything yet */
static int alsa_begin(AudioID * id, AudioTrack track)
{
	snd_pcm_format_t format;
	spd_alsa_id_t *alsa_id = (spd_alsa_id_t *) id;

	int err;

	snd_pcm_uframes_t period_size;
	unsigned int sr;

	snd_pcm_state_t state;

	struct pollfd alsa_stop_pipe_pfd;

	if (alsa_id == NULL) {
		ERR("Invalid device passed to alsa_play()");
		return -1;
	}

	pthread_mutex_lock(&alsa_id->alsa_pipe_mutex);

	MSG(2, "Start of playback on ALSA");

	/* Is it not an empty track? */
	/* Passing an empty track is not an error */
	if (track.samples == NULL) {
		pthread_mutex_unlock(&alsa_id->alsa_pipe_mutex);
		return 0;
	}
	/* Allocate space for hw_params (description of the sound parameters) */
	MSG(2, "Allocating new hw_params structure");
	if ((err = snd_pcm_hw_params_malloc(&alsa_id->alsa_hw_params)) < 0) {
		ERR("Cannot allocate hardware parameter structure (%s)",
		    snd_strerror(err));
		pthread_mutex_unlock(&alsa_id->alsa_pipe_mutex);
		return -1;
	}

	/* Initialize hw_params on our pcm */
	if ((err =
	     snd_pcm_hw_params_any(alsa_id->alsa_pcm,
				   alsa_id->alsa_hw_params)) < 0) {
		ERR("Cannot initialize hardware parameter structure (%s)",
		    snd_strerror(err));
		pthread_mutex_unlock(&alsa_id->alsa_pipe_mutex);
		return -1;
	}

	/* Create the pipe for communication about stop requests */
	if (pipe(alsa_id->alsa_stop_pipe)) {
		ERR("Stop pipe creation failed (%s)", strerror(errno));
		pthread_mutex_unlock(&alsa_id->alsa_pipe_mutex);
		return -1;
	}
	alsa_id->stop_requested = 0;

	/* Find how many descriptors we will get for poll() */
	alsa_id->alsa_fd_count =
	    snd_pcm_poll_descriptors_count(alsa_id->alsa_pcm);
	if (alsa_id->alsa_fd_count <= 0) {
		ERR("Invalid poll descriptors count returned from ALSA.");
		pthread_mutex_unlock(&alsa_id->alsa_pipe_mutex);
		return -1;
	}

	/* Create and fill in struct pollfd *alsa_poll_fds with ALSA descriptors */
	alsa_id->alsa_poll_fds =
	    g_malloc((alsa_id->alsa_fd_count + 1) * sizeof(struct pollfd));
	assert(alsa_id->alsa_poll_fds);
	if ((err =
	     snd_pcm_poll_descriptors(alsa_id->alsa_pcm, alsa_id->alsa_poll_fds,
				      alsa_id->alsa_fd_count)) < 0) {
		ERR("Unable to obtain poll descriptors for playback: %s\n",
		    snd_strerror(err));
		pthread_mutex_unlock(&alsa_id->alsa_pipe_mutex);
		return -1;
	}

	/* Create a new pollfd structure for requests by alsa_stop() */
	alsa_stop_pipe_pfd.fd = alsa_id->alsa_stop_pipe[0];
	alsa_stop_pipe_pfd.events = POLLIN;
	alsa_stop_pipe_pfd.revents = 0;

	/* Join this our own pollfd to the ALSAs ones */
	alsa_id->alsa_poll_fds[alsa_id->alsa_fd_count] = alsa_stop_pipe_pfd;
	alsa_id->alsa_fd_count++;

	alsa_id->alsa_opened = 1;
	pthread_mutex_unlock(&alsa_id->alsa_pipe_mutex);

	/* Report current state */
	state = snd_pcm_state(alsa_id->alsa_pcm);
	MSG(4, "PCM state before setting audio parameters: %s",
	    snd_pcm_state_name(state));

	/* Choose the correct format */
	if (track.bits == 16) {
		switch (alsa_id->id.format) {
		case SPD_AUDIO_LE:
			format = SND_PCM_FORMAT_S16_LE;
			break;
		case SPD_AUDIO_BE:
			format = SND_PCM_FORMAT_S16_BE;
			break;
		default:
			ERR("unknown audio format (%d)", alsa_id->id.format);
			return -1;
		}
	} else if (track.bits == 8) {
		format = SND_PCM_FORMAT_S8;
	} else {
		ERR("Unsupported sound data format, track.bits = %d",
		    track.bits);
		return -1;
	}

	/* Set access mode, bitrate, sample rate and channels */
	MSG(4, "Setting access type to INTERLEAVED");
	if ((err = snd_pcm_hw_params_set_access(alsa_id->alsa_pcm,
						alsa_id->alsa_hw_params,
						SND_PCM_ACCESS_RW_INTERLEAVED)
	    ) < 0) {
		ERR("Cannot set access type (%s)", snd_strerror(err));
		return -1;
	}

	MSG(4, "Setting sample format to %s", snd_pcm_format_name(format));
	if ((err =
	     snd_pcm_hw_params_set_format(alsa_id->alsa_pcm,
					  alsa_id->alsa_hw_params,
					  format)) < 0) {
		ERR("Cannot set sample format (%s)", snd_strerror(err));
		return -1;
	}

	MSG(4, "Setting sample rate to %i", track.sample_rate);
	sr = track.sample_rate;
	if ((err =
	     snd_pcm_hw_params_set_rate_near(alsa_id->alsa_pcm,
					     alsa_id->alsa_hw_params, &sr,
					     0)) < 0) {
		ERR("Cannot set sample rate (%s)", snd_strerror(err));

		return -1;
	}

	MSG(4, "Setting channel count to %i", track.num_channels);
	if ((err =
	     snd_pcm_hw_params_set_channels(alsa_id->alsa_pcm,
					    alsa_id->alsa_hw_params,
					    track.num_channels)) < 0) {
		MSG(4, "cannot set channel count (%s)", snd_strerror(err));
		return -1;
	}

	MSG(4, "Setting hardware parameters on the ALSA device");
	if ((err =
	     snd_pcm_hw_params(alsa_id->alsa_pcm,
			       alsa_id->alsa_hw_params)) < 0) {
		MSG(4, "cannot set parameters (%s) state=%s", snd_strerror(err),
		    snd_pcm_state_name(snd_pcm_state(alsa_id->alsa_pcm)));
		return -1;
	}

	/* Get the current swparams */
	if ((err =
	     snd_pcm_sw_params_current(alsa_id->alsa_pcm,
				       alsa_id->alsa_sw_params)) < 0) {
		ERR("Unable to determine current swparams for playback: %s\n",
		    snd_strerror(err));
		return -1;
	}
	//    MSG("Checking buffer size");
	if ((err =
	     snd_pcm_hw_params_get_buffer_size(alsa_id->alsa_hw_params,
					       &(alsa_id->alsa_buffer_size))) <
	    0) {
		ERR("Unable to get buffer size for playback: %s\n",
		    snd_strerror(err));
		return -1;
	}
	MSG(4, "Buffer size on ALSA device is %d frames",
	    (int)alsa_id->alsa_buffer_size);

	/* This is probably better left for the device driver to decide */
	/* allow the transfer when at least period_size samples can be processed */
	/*    err = snd_pcm_sw_params_set_avail_min(id->alsa_pcm, id->alsa_sw_params, id->alsa_buffer_size/4);
	   if (err < 0) {
	   ERR("Unable to set avail min for playback: %s\n", snd_strerror(err));
	   return err;
	   } */

	/* Get period size. */
	snd_pcm_hw_params_get_period_size(alsa_id->alsa_hw_params, &period_size,
	                                  0);
	MSG(4, "Period size on ALSA device is %lu frames", (unsigned long) period_size);

	MSG(4, "Preparing device for playback");
	if ((err = snd_pcm_prepare(alsa_id->alsa_pcm)) < 0) {
		ERR("Cannot prepare audio interface for playback (%s)",
		    snd_strerror(err));

		return -1;
	}

	return 0;
}

/* Push audio track to ALSA playback */
static int alsa_feed(AudioID * id, AudioTrack track)
{
	int bytes_per_sample;
	int num_bytes;
	spd_alsa_id_t *alsa_id = (spd_alsa_id_t *) id;

	AudioTrack track_volume;
	float real_volume;
	int i;

	signed short *output_samples;

	int err;
	int ret;

	snd_pcm_state_t state;

	snd_pcm_uframes_t framecount;
	size_t volume_size;

	bytes_per_sample = track.bits / 8;

	/* Calculate space needed to round up to nearest period size. */
	volume_size = bytes_per_sample * track.num_samples;
	MSG(4, "volume size = %i", (int)volume_size);

	/* Create a copy of track with adjusted volume. */
	MSG(4, "Making copy of track and adjusting volume");
	track_volume = track;
	track_volume.samples = (short *)g_malloc(volume_size);
	real_volume = ((float)alsa_id->id.volume + 100) / (float)200;
	for (i = 0; i <= track.num_samples - 1; i++)
		track_volume.samples[i] = track.samples[i] * real_volume;

	/* Loop until all samples are played on the device. */
	output_samples = track_volume.samples;
	num_bytes = volume_size;
	MSG(4, "%d bytes to be played", num_bytes);
	while (num_bytes > 0) {

		/* Write as much samples as possible */
		framecount = num_bytes / bytes_per_sample / track.num_channels;

		/* Report current state state */
		state = snd_pcm_state(alsa_id->alsa_pcm);
		//      MSG("PCM state before writei: %s",
		//          snd_pcm_state_name(state));

		/* MSG("snd_pcm_writei() called") */
		ret =
		    snd_pcm_writei(alsa_id->alsa_pcm, output_samples,
				   framecount);
		if (ret >= 0)
			MSG(5, "Sent %d of %d remaining bytes", ret*bytes_per_sample*track.num_channels, num_bytes);

		if (ret == -EAGAIN) {
			MSG(4, "Warning: Forced wait!");
			snd_pcm_wait(alsa_id->alsa_pcm, 100);
		} else if (ret == -EPIPE) {
			MSG(4, "Warning: returned EPIPE!");
			if (xrun(alsa_id) != 0)
				ERROR_EXIT();
#ifdef ESTRPIPE
		} else if (ret == -ESTRPIPE) {
			MSG(4, "Warning: returned ESTRPIPE!");
			if (suspend(alsa_id) != 0)
				ERROR_EXIT();
#endif
		} else if (ret == -EBUSY) {
			MSG(4, "WARNING: sleeping while PCM BUSY");
			usleep(100);
			continue;
		} else if (ret < 0) {
			ERR("Write to audio interface failed (%s)",
			    snd_strerror(ret));
			ERROR_EXIT();
		}

		if (ret > 0) {
			/* Update counter of bytes left and move the data pointer */
			num_bytes -=
			    ret * bytes_per_sample * track.num_channels;
			output_samples +=
			    ret * bytes_per_sample * track.num_channels / 2;
		}

		/* Report current state */
		state = snd_pcm_state(alsa_id->alsa_pcm);
		//      MSG("PCM state before polling: %s",
		//          snd_pcm_state_name(state));

		err =
		    wait_for_poll(alsa_id, alsa_id->alsa_poll_fds,
				  alsa_id->alsa_fd_count, 0);
		if (err < 0) {
			ERR("Wait for poll() failed\n");
			ERROR_EXIT();
		} else if (err == 1) {
			MSG(4, "Playback stopped");

			/* Drop the playback on the sound device (probably
			   still in progress up till now) */
			err = snd_pcm_drop(alsa_id->alsa_pcm);
			if (err < 0) {
				ERR("snd_pcm_drop() failed: %s",
				    snd_strerror(err));
				return -1;
			}

			/* Terminating (successfully or after a stop) */
			goto terminate;
		}

		if (num_bytes <= 0)
			break;
//      MSG("ALSA ready for more samples");

		/* Stop requests can be issued again */
	}

terminate:
	if (track_volume.samples != NULL)
		g_free(track_volume.samples);

	return 0;
}

/* Drain ALSA playback until only `left' samples are left in the buffer */
static int alsa_drain_left(AudioID * id, snd_pcm_uframes_t left)
{
	spd_alsa_id_t *alsa_id = (spd_alsa_id_t *) id;

	int err;

	MSG(4, "Draining until %lu frames left...", (unsigned long) left);

	/* We want to get next "device ready" notification only after the buffer
	   is already empty */
	err =
	    snd_pcm_sw_params_set_avail_min(alsa_id->alsa_pcm,
					    alsa_id->alsa_sw_params,
					    alsa_id->alsa_buffer_size - left);
	if (err < 0) {
		ERR("Unable to set avail min for playback: %s\n",
		    snd_strerror(err));
		return err;
	}
	/* write the parameters to the playback device */
	err = snd_pcm_sw_params(alsa_id->alsa_pcm, alsa_id->alsa_sw_params);
	if (err < 0) {
		ERR("Unable to set sw params for playback: %s\n",
		    snd_strerror(err));
		return -1;
	}

	err =
	    wait_for_poll(alsa_id, alsa_id->alsa_poll_fds,
			  alsa_id->alsa_fd_count, 1);
	if (err < 0) {
		ERR("Wait for poll() failed\n");
		return -1;
	} else if (err == 1) {
		MSG(4, "Playback stopped while draining");

		/* Drop the playback on the sound device (probably
		   still in progress up till now) */
		err = snd_pcm_drop(alsa_id->alsa_pcm);
		if (err < 0) {
			ERR("snd_pcm_drop() failed: %s", snd_strerror(err));
			return -1;
		}
	}

	/* When ALSA is going through Pulseaudio, wait_for_poll returns too
	   early because the file descriptor is always availble for writing
	   :/ */
	while (!alsa_id->stop_requested)
	{
		snd_pcm_sframes_t frames;
		snd_pcm_state_t state;
		struct timeval tv;
		struct timespec ts;

		/* Poll server */
		frames = snd_pcm_avail(alsa_id->alsa_pcm);
		if (frames < 0) {
			MSG(4, "Drain: Buffer clear");
			break;
		}

		MSG(5, "Drain: %lu frames left in buffer",
			(unsigned long) alsa_id->alsa_buffer_size - frames);
		if (alsa_id->alsa_buffer_size - frames <= left) {
			MSG(4, "Drain: Buffer clear enough");
			break;
		}

		state = snd_pcm_state(alsa_id->alsa_pcm);
		if (err != 0) {
			MSG(4, "Drain: Status error %d", err);
			break;
		}
		if (state == SND_PCM_STATE_XRUN) {
			MSG(4, "Drain: Playback terminated");
			break;
		}

		/* Poll every 10ms */
		gettimeofday(&tv, NULL);
		ts.tv_sec = tv.tv_sec;
		ts.tv_nsec = tv.tv_usec * 1000 + 10000000;
		if (ts.tv_nsec >= 1000000000) {
			ts.tv_sec += 1;
			ts.tv_nsec -= 1000000000;
		}

		pthread_mutex_lock(&alsa_id->alsa_pipe_mutex);
		if (!alsa_id->stop_requested)
			pthread_cond_timedwait(&alsa_id->alsa_pipe_cond,
					       &alsa_id->alsa_pipe_mutex, &ts);
		pthread_mutex_unlock(&alsa_id->alsa_pipe_mutex);
	}

	MSG(4, "Draining terminated");

	return 0;
}

/* Drain until the amount of samples left in the buffer is big enough to make
   sure we will have time to push the rest of the audio */
static int alsa_drain_overlap(AudioID * id, AudioTrack track)
{
	/* We typically want 20ms overlap: usually about a period size, and
	   small enough to be unnoticeable */
	unsigned min_ms = 20;

	spd_alsa_id_t *alsa_id = (spd_alsa_id_t *) id;
	snd_pcm_uframes_t min;
	snd_pcm_uframes_t period_size;
	snd_pcm_uframes_t min2;
	snd_pcm_uframes_t left;

	min = (min_ms * track.sample_rate) / 1000;

	/* Get period size. */
	snd_pcm_hw_params_get_period_size(alsa_id->alsa_hw_params, &period_size,
	                                  0);

	/* Round minimum to two period size */
	if (2*period_size >= min)
		min2 = 2*period_size;
	else
		min2 = (min + period_size - 1) / period_size;

	/* Usually the buffer size will be far enough */
	if (alsa_id->alsa_buffer_size >= min2)
		left = min2;
	else
		/* That's very odd. Just wait for some room to be available */
		left = alsa_id->alsa_buffer_size - 1;

	MSG(4, "Draining with at least %ums left, i.e. %lu frames, with period %lu frames, thus %lu frames, i.e. %lu left of %lu",
		min_ms, (unsigned long) min, (unsigned long) period_size,
		(unsigned long) min2, (unsigned long) left, (unsigned long)
		alsa_id->alsa_buffer_size);

	return alsa_drain_left(id, left);
}

static int alsa_drain(AudioID * id)
{
	return alsa_drain_left(id, 0);
}

static int alsa_feed_sync(AudioID * id, AudioTrack track)
{
	int ret;

	ret = alsa_feed(id, track);
	if (ret)
		return ret;

	return alsa_drain(id);
}

static int alsa_feed_sync_overlap(AudioID * id, AudioTrack track)
{
	int ret;

	ret = alsa_feed(id, track);
	if (ret)
		return ret;

	return alsa_drain_overlap(id, track);
}

static int alsa_end(AudioID * id)
{
	spd_alsa_id_t *alsa_id = (spd_alsa_id_t *) id;
	int err;

	if (!alsa_id->stop_requested)
		alsa_drain(id);

	err = snd_pcm_drop(alsa_id->alsa_pcm);
	if (err < 0) {
		ERR("snd_pcm_drop() failed: %s", snd_strerror(err));
		return -1;
	}

	MSG(2, "Freeing HW parameters");
	snd_pcm_hw_params_free(alsa_id->alsa_hw_params);

	pthread_mutex_lock(&alsa_id->alsa_pipe_mutex);
	alsa_id->alsa_opened = 0;
	close(alsa_id->alsa_stop_pipe[0]);
	close(alsa_id->alsa_stop_pipe[1]);

	g_free(alsa_id->alsa_poll_fds);
	pthread_mutex_unlock(&alsa_id->alsa_pipe_mutex);

	MSG(1, "End of playback on ALSA");

	return 0;
}

/* Play the track _track_ (see spd_audio.h) using the id->alsa_pcm device and
 id-hw_params parameters. This is a blocking function, however, it's possible
 to interrupt playing from a different thread with alsa_stop(). alsa_play
 returns after and immediatelly after the whole sound was played on the
 speakers.

 The idea is that we get the ALSA file descriptors and we will poll() to see
 when alsa is ready for more input while sleeping in the meantime. We will
 additionally poll() for one more descriptor used by alsa_stop() to notify the
 thread with alsa_play() that the stop of the playback is requested. The
 variable can_be_stopped is used for very simple synchronization between the
 two threads. */
static int alsa_play(AudioID * id, AudioTrack track)
{
	int ret;

	ret = alsa_begin(id, track);
	if (ret)
		return ret;

	ret = alsa_feed_sync(id, track);
	if (ret)
		return ret;

	return alsa_end(id);
}

#undef ERROR_EXIT

/*
 Stop the playback on the device and interrupt alsa_play()
*/
static int alsa_stop(AudioID * id)
{
	char buf;
	int ret;
	spd_alsa_id_t *alsa_id = (spd_alsa_id_t *) id;

	MSG(1, "STOP!");

	if (alsa_id == NULL)
		return 0;

	pthread_mutex_lock(&alsa_id->alsa_pipe_mutex);
	if (alsa_id->alsa_opened) {
		alsa_id->stop_requested = 1;

		/* This constant is arbitrary */
		buf = 42;

		ret = write(alsa_id->alsa_stop_pipe[1], &buf, 1);
		if (ret <= 0) {
			ERR("Can't write stop request to pipe, err %d: %s",
			    errno, strerror(errno));
		}
		pthread_cond_broadcast(&alsa_id->alsa_pipe_cond);
	}
	pthread_mutex_unlock(&alsa_id->alsa_pipe_mutex);

	return 0;
}

/*
  Set volume

  Comments: It's not possible to set individual track volume with Alsa, so we
   handle volume in alsa_play() by multiplication of each sample.
*/
static int alsa_set_volume(AudioID * id, int volume)
{
	return 0;
}

static void alsa_set_loglevel(int level)
{
	if (level) {
		alsa_log_level = level;
	}
}

static char const *alsa_get_playcmd(void)
{
	return alsa_play_cmd;
}

/* Provide the Alsa backend. */
static spd_audio_plugin_t alsa_functions = {
	"alsa",
	alsa_open,
	alsa_play,
	alsa_stop,
	alsa_close,
	alsa_set_volume,
	alsa_set_loglevel,
	alsa_get_playcmd,
	alsa_begin,
	alsa_feed_sync,
	alsa_feed_sync_overlap,
	alsa_end,
};

spd_audio_plugin_t *alsa_plugin_get(void)
{
	return &alsa_functions;
}

spd_audio_plugin_t *SPD_AUDIO_PLUGIN_ENTRY(void)
    __attribute__ ((weak, alias("alsa_plugin_get")));
#undef MSG
#undef ERR
