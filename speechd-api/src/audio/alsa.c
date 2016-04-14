
/*
 * alsa.c -- The Advanced Linux Sound System backend for Speech Dispatcher
 *
 * Copyright (C) 2005,2006 Brailcom, o.p.s.
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
 * $Id: alsa.c,v 1.30 2008-10-15 17:27:32 hanke Exp $
 */

/* NOTE: This module uses the non-blocking write() / poll() approach to
    alsa-lib functions.*/

#include <sys/time.h>
#include <time.h>

int _alsa_close(AudioID *id);
int _alsa_open(AudioID *id);

int xrun(AudioID *id);
int suspend(AudioID *id);

int alsa_open(AudioID *id, void **pars);
int alsa_close(AudioID *id);
int alsa_play(AudioID *id, AudioTrack track);
int alsa_stop(AudioID *id);
int alsa_set_volume(AudioID*id, int volume);

int wait_for_poll(AudioID *id, struct pollfd *alsa_poll_fds, 
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
 if(level <= log_level){ \
     time_t t; \
     struct timeval tv; \
     char *tstr; \
     t = time(NULL); \
     tstr = strdup(ctime(&t)); \
     tstr[strlen(tstr)-1] = 0; \
     gettimeofday(&tv,NULL); \
     fprintf(stderr," %s [%d]",tstr, (int) tv.tv_usec); \
     fprintf(stderr," ALSA: "); \
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
     fprintf(stderr," ALSA ERROR: "); \
     fprintf(stderr,arg); \
     fprintf(stderr,"\n"); \
     fflush(stderr); \
     xfree(tstr); \
  }

/* I/O error handler */
int
xrun(AudioID *id)
{
    snd_pcm_status_t *status;
    int res;
    
    if (id == NULL) return -1;

    MSG(1, "WARNING: Entering XRUN handler");
    
    snd_pcm_status_alloca(&status);
    if ((res = snd_pcm_status(id->alsa_pcm, status))<0) {
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
	
	return 0;		/* ok, data should be accepted again */
    }
    ERR("read/write error, state = %s",
	snd_pcm_state_name(snd_pcm_status_get_state(status)));
        
    return -1;
}

/* I/O suspend handler */
int
suspend(AudioID *id)
{
    int res;

    MSG(1, "WARNING: Entering SUSPEND handler.");
    
    if (id == NULL) return -1;
    
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
int
_alsa_open(AudioID *id)
{
    int err;

    MSG(1, "Opening ALSA device");
    fflush(stderr);

    /* Open the device */
    if ((err = snd_pcm_open (&id->alsa_pcm, id->alsa_device_name,
			     SND_PCM_STREAM_PLAYBACK, SND_PCM_NONBLOCK)) < 0) { 
	ERR("Cannot open audio device %s (%s)", id->alsa_device_name, snd_strerror (err));
	return -1;
    }

    /* Allocate space for hw_params (description of the sound parameters) */
    /* Allocate space for sw_params (description of the sound parameters) */
    MSG(2, "Allocating new sw_params structure");
    if ((err = snd_pcm_sw_params_malloc (&id->alsa_sw_params)) < 0) {
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

int
_alsa_close(AudioID *id)
{
    int err;

    MSG(1, "Closing ALSA device");

    if (id->alsa_opened == 0) return 0;

    pthread_mutex_lock(&id->alsa_pipe_mutex);
    id->alsa_opened = 0;
    
    if ((err = snd_pcm_close (id->alsa_pcm)) < 0) {
	MSG(2, "Cannot close ALSA device (%s)", snd_strerror (err));
	return -1;
    }

    snd_pcm_sw_params_free (id->alsa_sw_params);

    free(id->alsa_poll_fds);
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
int
alsa_open(AudioID *id, void **pars)
{
    int ret;

    pthread_mutex_init(&id->alsa_pipe_mutex, NULL);

    id->alsa_opened = 0;

    if (id == NULL){
	ERR("Can't open ALSA sound output, invalid AudioID structure.");
	return 0;
    }

    if (pars[0] == NULL){
	ERR("Can't open ALSA sound output, missing parameters in argument.");
	return -1;
    }
    
    MSG(1, "Opening ALSA sound output");

    id->alsa_device_name = strdup(pars[0]);
    
    ret = _alsa_open(id);
    if (ret){
	ERR("Cannot initialize Alsa device '%s': Can't open.", (char*) pars[0]);
	return -1;
    }

    MSG(1, "Device '%s' initialized succesfully.", (char*) pars[0]);
    
    return 0; 
}

/* Close ALSA */
int
alsa_close(AudioID *id)
{
    int err;

    /* Close device */
    if ((err = _alsa_close(id)) < 0) { 
	ERR("Cannot close audio device");
	return -1;
    }
    MSG(1, "ALSA closed.");

    id = NULL;

    return 0;
}

/* Wait until ALSA is readdy for more samples or alsa_stop() was called.

Returns 0 if ALSA is ready for more input, +1 if a request to stop
the sound output was received and a negative value on error.  */

int wait_for_poll(AudioID *id, struct pollfd *alsa_poll_fds, 
			 unsigned int count, int draining)
{
        unsigned short revents;
	snd_pcm_state_t state;
	int ret;

	//	MSG("Waiting for poll");

	/* Wait for certain events */
        while (1) {
	    ret = poll(id->alsa_poll_fds, count, -1);
	    //	    MSG("wait_for_poll: activity on %d descriptors", ret);

	    /* Check for stop request from alsa_stop on the last file
	       descriptors*/
	    if (revents = id->alsa_poll_fds[count-1].revents){
		if (revents & POLLIN){
		    MSG(4, "wait_for_poll: stop requested");
		    return 1;
		}
	    }
	    
	    /* Check the first count-1 descriptors for ALSA events */
	    snd_pcm_poll_descriptors_revents(id->alsa_pcm, id->alsa_poll_fds, count-1, &revents);
	    
	    /* Ensure we are in the right state */
	    state = snd_pcm_state(id->alsa_pcm);
	    //	    MSG("State after poll returned is %s", snd_pcm_state_name(state));
	    
	    if (SND_PCM_STATE_XRUN == state){
		if (!draining){
		    MSG(1, "WARNING: Buffer underrun detected!");
		    if (xrun(id) != 0) return -1;
		    return 0;
		}else{
		    MSG(4, "Poll: Playback terminated");
		    return 0;
		}
	    }
	    
	    if (SND_PCM_STATE_SUSPENDED == state){
		MSG(1, "WARNING: Suspend detected!");
		if (suspend(id) != 0) return -1;
		return 0;
	    }
	    
	    /* Check for errors */
	    if (revents & POLLERR) {
                MSG(4, "wait_for_poll: poll revents says POLLERR");
		return -EIO;
            }
	    
	    /* Is ALSA ready for more input? */
	    if ((revents & POLLOUT)){
		// MSG("Poll: Ready for more input");
		return 0;	       
	    }
        }
}


#define ERROR_EXIT()\
    free(track_volume.samples); \
    ERR("alsa_play() abnormal exit"); \
    _alsa_close(id); \
    return -1;

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
int
alsa_play(AudioID *id, AudioTrack track)
{
    snd_pcm_format_t format;
    int bytes_per_sample;
    int num_bytes;

    signed short* output_samples;

    AudioTrack track_volume;
    float real_volume;
    int i;

    int err;
    int ret;

    snd_pcm_uframes_t framecount;
    snd_pcm_uframes_t period_size;
    size_t samples_per_period;
    size_t silent_samples;
    size_t volume_size;
    unsigned int sr;

    snd_pcm_state_t state;

    struct pollfd alsa_stop_pipe_pfd;

    if (id == NULL){
	ERR("Invalid device passed to alsa_play()");
	return -1;
    }

    pthread_mutex_lock(&id->alsa_pipe_mutex);

    MSG(2, "Start of playback on ALSA");

    /* Is it not an empty track? */
    /* Passing an empty track is not an error */
    if (track.samples == NULL){
      pthread_mutex_unlock(&id->alsa_pipe_mutex);
      return 0;
    }
    /* Allocate space for hw_params (description of the sound parameters) */
    MSG(2, "Allocating new hw_params structure");
    if ((err = snd_pcm_hw_params_malloc (&id->alsa_hw_params)) < 0) {
	ERR("Cannot allocate hardware parameter structure (%s)", 
	    snd_strerror(err));
	pthread_mutex_unlock(&id->alsa_pipe_mutex);
	return -1;
    }

    /* Initialize hw_params on our pcm */
    if ((err = snd_pcm_hw_params_any (id->alsa_pcm, id->alsa_hw_params)) < 0) {
	ERR("Cannot initialize hardware parameter structure (%s)", 
	    snd_strerror (err));
	pthread_mutex_unlock(&id->alsa_pipe_mutex);
	return -1;
    }

    /* Create the pipe for communication about stop requests */
    if (pipe (id->alsa_stop_pipe))
	{
	    ERR("Stop pipe creation failed (%s)", strerror(errno));
	    pthread_mutex_unlock(&id->alsa_pipe_mutex);
	    return -1;
	}   

    /* Find how many descriptors we will get for poll() */
    id->alsa_fd_count = snd_pcm_poll_descriptors_count(id->alsa_pcm);
    if (id->alsa_fd_count <= 0){
	ERR("Invalid poll descriptors count returned from ALSA.");
	pthread_mutex_unlock(&id->alsa_pipe_mutex);
	return -1;
    }

    /* Create and fill in struct pollfd *alsa_poll_fds with ALSA descriptors */
    id->alsa_poll_fds = malloc ((id->alsa_fd_count + 1) * sizeof(struct pollfd));
    assert(id->alsa_poll_fds);    
    if ((err = snd_pcm_poll_descriptors(id->alsa_pcm, id->alsa_poll_fds, id->alsa_fd_count)) < 0) {
	ERR("Unable to obtain poll descriptors for playback: %s\n", snd_strerror(err));
	pthread_mutex_unlock(&id->alsa_pipe_mutex);
	return -1;
    }
    
    /* Create a new pollfd structure for requests by alsa_stop()*/
    alsa_stop_pipe_pfd.fd = id->alsa_stop_pipe[0];
    alsa_stop_pipe_pfd.events = POLLIN;
    alsa_stop_pipe_pfd.revents = 0;
        
    /* Join this our own pollfd to the ALSAs ones */
    id->alsa_poll_fds[id->alsa_fd_count] = alsa_stop_pipe_pfd;
    id->alsa_fd_count++;

    id->alsa_opened = 1;
    pthread_mutex_unlock(&id->alsa_pipe_mutex);

    /* Report current state */
    state = snd_pcm_state(id->alsa_pcm);
    MSG(4, "PCM state before setting audio parameters: %s",
	snd_pcm_state_name(state));

    /* Choose the correct format */
    if (track.bits == 16){	
        switch (spd_audio_endian){
            case SPD_AUDIO_LE:
                format = SND_PCM_FORMAT_S16_LE;
                break;
            case SPD_AUDIO_BE:
                format = SND_PCM_FORMAT_S16_BE;
                break;
        }
	bytes_per_sample = 2;
    }else if (track.bits == 8){
	bytes_per_sample = 1;
	format = SND_PCM_FORMAT_S8;
    }else{
	ERR("Unsupported sound data format, track.bits = %d", track.bits);		
	return -1;
    }

    /* Set access mode, bitrate, sample rate and channels */
    MSG(4, "Setting access type to INTERLEAVED");
    if ((err = snd_pcm_hw_params_set_access (id->alsa_pcm, 
					     id->alsa_hw_params, 
					     SND_PCM_ACCESS_RW_INTERLEAVED)
	 )< 0) {
	ERR("Cannot set access type (%s)",
		 snd_strerror (err));	
	return -1;
    }
    
    MSG(4, "Setting sample format to %s", snd_pcm_format_name(format));
    if ((err = snd_pcm_hw_params_set_format (id->alsa_pcm, id->alsa_hw_params, format)) < 0) {
	ERR("Cannot set sample format (%s)",
		 snd_strerror (err));
	return -1;
    }

    MSG(4, "Setting sample rate to %i", track.sample_rate);
    sr = track.sample_rate;
    if ((err = snd_pcm_hw_params_set_rate_near (id->alsa_pcm, id->alsa_hw_params,
						&sr, 0)) < 0) {
	ERR("Cannot set sample rate (%s)",
	    snd_strerror (err));
	
	return -1;
    }

    MSG(4, "Setting channel count to %i", track.num_channels);
    if ((err = snd_pcm_hw_params_set_channels (id->alsa_pcm, id->alsa_hw_params,
					       track.num_channels)) < 0) {
	MSG(4, "cannot set channel count (%s)",
		 snd_strerror (err));	
	return -1;
    }

    MSG(4, "Setting hardware parameters on the ALSA device");	
    if ((err = snd_pcm_hw_params (id->alsa_pcm, id->alsa_hw_params)) < 0) {
	MSG(4, "cannot set parameters (%s) state=%s",
	    snd_strerror (err), snd_pcm_state_name(snd_pcm_state(id->alsa_pcm)));	
	return -1;
    }

    /* Get the current swparams */
    if ((err = snd_pcm_sw_params_current(id->alsa_pcm, id->alsa_sw_params)) < 0){
	ERR("Unable to determine current swparams for playback: %s\n",
	       snd_strerror(err));
	return -1;
    }    

    //    MSG("Checking buffer size");
    if ((err = snd_pcm_hw_params_get_buffer_size(id->alsa_hw_params, &(id->alsa_buffer_size))) < 0){	
	ERR("Unable to get buffer size for playback: %s\n", snd_strerror(err));
	return -1;
    }
    MSG(4, "Buffer size on ALSA device is %d bytes", (int) id->alsa_buffer_size);

    /* This is probably better left for the device driver to decide */
    /* allow the transfer when at least period_size samples can be processed */
    /*    err = snd_pcm_sw_params_set_avail_min(id->alsa_pcm, id->alsa_sw_params, id->alsa_buffer_size/4);
    if (err < 0) {
	ERR("Unable to set avail min for playback: %s\n", snd_strerror(err));
	return err;
	}*/

    /* Get period size. */
    snd_pcm_hw_params_get_period_size(id->alsa_hw_params, &period_size, 0);

    /* Calculate size of silence at end of buffer. */
    samples_per_period = period_size * track.num_channels;
    //    MSG("samples per period = %i", samples_per_period);
    //    MSG("num_samples = %i", track.num_samples);
    silent_samples = samples_per_period - (track.num_samples % samples_per_period);
    //    MSG("silent samples = %i", silent_samples);


    MSG(4, "Preparing device for playback");
    if ((err = snd_pcm_prepare (id->alsa_pcm)) < 0) {
	ERR("Cannot prepare audio interface for playback (%s)",
		 snd_strerror (err));
	
	return -1;
    }

    /* Calculate space needed to round up to nearest period size. */
    volume_size = bytes_per_sample*(track.num_samples + silent_samples);
    MSG(4, "volume size = %i", (int) volume_size);

    /* Create a copy of track with adjusted volume. */
    MSG(4, "Making copy of track and adjusting volume");
    track_volume = track;
    track_volume.samples = (short*) malloc(volume_size);
    real_volume = ((float) id->volume + 100)/(float)200;
    for (i=0; i<=track.num_samples-1; i++)
        track_volume.samples[i] = track.samples[i] * real_volume;

    if (silent_samples > 0) {
        u_int16_t silent16;
        u_int8_t silent8;

        /* Fill remaining space with silence */
        MSG(4, "Filling with silence up to the period size, silent_samples=%d", (int) silent_samples);
        /* TODO: This hangs.  Why?
        snd_pcm_format_set_silence(format,
            track_volume.samples + (track.num_samples * bytes_per_sample), silent_samples);
        */
        switch (bytes_per_sample) {
	case 2:
	    silent16 = snd_pcm_format_silence_16(format);
	    for (i = 0; i < silent_samples; i++)
		track_volume.samples[track.num_samples + i] = silent16;
	    break;
	case 1:	    
	    silent8 = snd_pcm_format_silence(format);
	    for (i = 0; i < silent_samples; i++)
		track_volume.samples[track.num_samples + i] = silent8;
	    break;
        }
    }

    /* Loop until all samples are played on the device. */
    output_samples = track_volume.samples;
    num_bytes = (track.num_samples + silent_samples)*bytes_per_sample;
    //    MSG("Still %d bytes left to be played", num_bytes);
    while(num_bytes > 0) {
	
	/* Write as much samples as possible */
        framecount = num_bytes/bytes_per_sample/track.num_channels;
        if (framecount < period_size) framecount = period_size;

	/* Report current state state */
	state = snd_pcm_state(id->alsa_pcm);
	//	MSG("PCM state before writei: %s",
	//	    snd_pcm_state_name(state));

	/* MSG("snd_pcm_writei() called") */
	ret = snd_pcm_writei (id->alsa_pcm, output_samples, framecount);
	//        MSG("Sent %d of %d remaining bytes", ret*bytes_per_sample, num_bytes);

        if (ret == -EAGAIN) {
	    MSG(4, "Warning: Forced wait!");
	    snd_pcm_wait(id->alsa_pcm, 100);
        } else if (ret == -EPIPE) {
            if (xrun(id) != 0) ERROR_EXIT();
	} else if (ret == -ESTRPIPE) {
	    if (suspend(id) != 0) ERROR_EXIT();
	} else if (ret == -EBUSY){
            MSG(4, "WARNING: sleeping while PCM BUSY");
            usleep(100);
            continue;
        } else if (ret < 0) {	    
	    ERR("Write to audio interface failed (%s)",
		snd_strerror (ret));
	    ERROR_EXIT();
	}

	if (ret > 0) {
            /* Update counter of bytes left and move the data pointer */
            num_bytes -= ret*bytes_per_sample*track.num_channels;
            output_samples += ret*bytes_per_sample*track.num_channels/2;
        }
	
	/* Report current state */
	state = snd_pcm_state(id->alsa_pcm);
	//	MSG("PCM state before polling: %s",
	//	    snd_pcm_state_name(state));

	err = wait_for_poll(id, id->alsa_poll_fds, id->alsa_fd_count, 0);
	if (err < 0) {
	    ERR("Wait for poll() failed\n");
	    ERROR_EXIT();
	}	
	else if (err == 1){
	    MSG(4, "Playback stopped");

	    /* Drop the playback on the sound device (probably
	       still in progress up till now) */
	    err = snd_pcm_drop(id->alsa_pcm);
	    if (err < 0) {
		ERR("snd_pcm_drop() failed: %s", snd_strerror (err));	
		return -1;
	    }
	    
	    goto terminate;
	}
	
	if (num_bytes <= 0) break;
//	MSG("ALSA ready for more samples");

	/* Stop requests can be issued again */
    }

    MSG(4, "Draining...");

    /* We want to next "device ready" notification only after the buffer is
       already empty */
    err = snd_pcm_sw_params_set_avail_min(id->alsa_pcm, id->alsa_sw_params, id->alsa_buffer_size);
    if (err < 0) {
	ERR("Unable to set avail min for playback: %s\n", snd_strerror(err));
	return err;
    }
    /* write the parameters to the playback device */
    err = snd_pcm_sw_params(id->alsa_pcm, id->alsa_sw_params);
    if (err < 0) {
	ERR("Unable to set sw params for playback: %s\n", snd_strerror(err));
	return -1;
    }
   
    err = wait_for_poll(id, id->alsa_poll_fds, id->alsa_fd_count, 1);
    if (err < 0) {
	ERR("Wait for poll() failed\n");
	return -1;
    } else if (err == 1){
	MSG(4, "Playback stopped while draining");
	
	/* Drop the playback on the sound device (probably
	   still in progress up till now) */
	err = snd_pcm_drop(id->alsa_pcm);
	if (err < 0) {
	    ERR("snd_pcm_drop() failed: %s", snd_strerror (err));	
	    return -1;
	}
    }
    MSG(4, "Draining terminated");

 terminate:
    /* Terminating (successfully or after a stop) */
    if (track_volume.samples != NULL)
	free(track_volume.samples);

    err = snd_pcm_drop(id->alsa_pcm);
    if (err < 0) {
	ERR("snd_pcm_drop() failed: %s", snd_strerror (err));	
	return -1;
    }
    

    MSG(2, "Freeing HW parameters");
    snd_pcm_hw_params_free(id->alsa_hw_params);

    pthread_mutex_lock(&id->alsa_pipe_mutex);
    id->alsa_opened = 0;
    close(id->alsa_stop_pipe[0]);
    close(id->alsa_stop_pipe[1]);

    xfree(id->alsa_poll_fds);
    pthread_mutex_unlock(&id->alsa_pipe_mutex);
    
    MSG(1, "End of playback on ALSA");

    return 0;   
}

#undef ERROR_EXIT

/*
 Stop the playback on the device and interrupt alsa_play()
*/
int
alsa_stop(AudioID *id)
{
    char buf;
    int ret;

    MSG(1, "STOP!");

    pthread_mutex_lock(&id->alsa_pipe_mutex);
    if (id->alsa_opened){
	/* This constant is arbitrary */
	buf = 42;
	
	if (id == NULL) return 0;
	
	ret =  write(id->alsa_stop_pipe[1], &buf, 1);
	if (ret <= 0){
	  ERR("Can't write stop request to pipe, err %d: %s", errno, strerror(errno));
	}
    }	
    pthread_mutex_unlock(&id->alsa_pipe_mutex);

    return 0;
}

/* 
  Set volume

  Comments: It's not possible to set individual track volume with Alsa, so we
   handle volume in alsa_play() by multiplication of each sample.
*/
int
alsa_set_volume(AudioID*id, int volume)
{
    return 0;
}

/* Provide the Alsa backend. */
spd_audio_plugin_t alsa_functions = {alsa_open, alsa_play, alsa_stop, alsa_close, alsa_set_volume};

#undef MSG
#undef ERR
