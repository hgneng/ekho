/*
 * nas.c -- The Network Audio System backend for the spd_audio library.
 *
 * Copyright (C) 2004,2006 Brailcom, o.p.s.
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
 * $Id: nas.c,v 1.8 2006-07-11 16:12:26 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <glib.h>
#include <audio/audiolib.h>
#include <audio/soundlib.h>

#include <pthread.h>

#define SPD_AUDIO_PLUGIN_ENTRY spd_nas_LTX_spd_audio_plugin_get
#include <spd_audio_plugin.h>

typedef struct {
	AudioID id;
	AuServer *aud;
	AuFlowID flow;
	pthread_mutex_t flow_mutex;
	pthread_t nas_event_handler;
	pthread_cond_t pt_cond;
	pthread_mutex_t pt_mutex;
} spd_nas_id_t;

static int nas_log_level;

/* Internal event handler */
static void *_nas_handle_events(void *par)
{
	spd_nas_id_t *nas_id = (spd_nas_id_t *) par;
	pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);

	while (1)
		AuHandleEvents(nas_id->aud);

}

/* NAS Server error handler */
/* Unfortunatelly we can't return these errors to the caller
   since this handler gets called in the event handler thread. */
static AuBool _nas_handle_server_error(AuServer * server, AuErrorEvent * event)
{
	fprintf(stderr, "ERROR: Non-fatal server error in NAS\n");

	if (event->type != 0) {
		fprintf(stderr,
			"Event of a different type received in NAS error handler.");
		return -1;
	}

	/* It's a pain but we can't ask for string return code
	   since it's not allowed to talk to the server inside error handlers
	   because of possible deadlocks. */
	fprintf(stderr, "NAS: Serial number of failed request: %d\n",
		event->serial);
	fprintf(stderr, "NAS: Error code: %d\n", event->error_code);
	fprintf(stderr, "NAS: Resource id: %d\n", event->resourceid);
	fprintf(stderr, "NAS: Request code: %d\n", event->request_code);
	fprintf(stderr, "NAS: Minor code: %d\n\n", event->minor_code);

	return 0;
}

static AudioID *nas_open(void **pars)
{
	spd_nas_id_t *nas_id;
	int ret;

	nas_id = (spd_nas_id_t *) g_malloc(sizeof(spd_nas_id_t));

	nas_id->aud = AuOpenServer(pars[2], 0, NULL, 0, NULL, NULL);
	if (!nas_id->aud) {
		fprintf(stderr, "Can't connect to NAS audio server\n");
		return NULL;
	}

	AuSetErrorHandler(nas_id->aud, _nas_handle_server_error);
	/* return value incompatible with documentation here */
	/*    if (!r){
	   fprintf(stderr, "Can't set default NAS event handler\n");
	   return -1;
	   } */

	nas_id->flow = 0;

	pthread_cond_init(&nas_id->pt_cond, NULL);
	pthread_mutex_init(&nas_id->pt_mutex, NULL);
	pthread_mutex_init(&nas_id->flow_mutex, NULL);

	ret =
	    pthread_create(&nas_id->nas_event_handler, NULL, _nas_handle_events,
			   (void *)nas_id);
	if (ret != 0) {
		fprintf(stderr,
			"ERROR: NAS Audio module: thread creation failed\n");
		return NULL;
	}

	return (AudioID *) nas_id;
}

static int nas_play(AudioID * id, AudioTrack track)
{
	char *buf;
	Sound s;
	AuEventHandlerRec *event_handler;
	float lenght;
	struct timeval now;
	struct timespec timeout;
	spd_nas_id_t *nas_id = (spd_nas_id_t *) id;

	if (nas_id == NULL)
		return -2;

	s = SoundCreate(SoundFileFormatNone,
			AuFormatLinearSigned16LSB,
			track.num_channels,
			track.sample_rate, track.num_samples, NULL);

	buf = (char *)track.samples;

	pthread_mutex_lock(&nas_id->flow_mutex);

	event_handler = AuSoundPlayFromData(nas_id->aud,
					    s,
					    buf,
					    AuNone,
					    ((nas_id->id.volume +
					      100) / 2) * 1500, NULL, NULL,
					    &nas_id->flow, NULL, NULL, NULL);

	if (event_handler == NULL) {
		pthread_mutex_unlock(&nas_id->flow_mutex);
		fprintf(stderr,
			"AuSoundPlayFromData failed for unknown resons.\n");
		return -1;
	}

	if (nas_id->flow == 0) {
		fprintf(stderr, "Couldn't start data flow");
	}
	pthread_mutex_unlock(&nas_id->flow_mutex);

	/* Another timing magic */
	pthread_mutex_lock(&nas_id->pt_mutex);
	lenght = (((float)track.num_samples) / (float)track.sample_rate);
	gettimeofday(&now, NULL);
	timeout.tv_sec = now.tv_sec + (int)lenght;
	timeout.tv_nsec =
	    now.tv_usec * 1000 + (lenght - (int)lenght) * 1000000000;
	pthread_cond_timedwait(&nas_id->pt_cond, &nas_id->pt_mutex, &timeout);
	pthread_mutex_unlock(&nas_id->pt_mutex);

	pthread_mutex_lock(&nas_id->flow_mutex);
	nas_id->flow = 0;
	pthread_mutex_unlock(&nas_id->flow_mutex);

	return 0;
}

static int nas_stop(AudioID * id)
{
	spd_nas_id_t *nas_id = (spd_nas_id_t *) id;

	if (nas_id == NULL)
		return -2;

	pthread_mutex_lock(&nas_id->flow_mutex);
	if (nas_id->flow != 0)
		AuStopFlow(nas_id->aud, nas_id->flow, NULL);
	nas_id->flow = 0;
	pthread_mutex_unlock(&nas_id->flow_mutex);

	pthread_mutex_lock(&nas_id->pt_mutex);
	pthread_cond_signal(&nas_id->pt_cond);
	pthread_mutex_unlock(&nas_id->pt_mutex);

	return 0;
}

static int nas_close(AudioID * id)
{
	spd_nas_id_t *nas_id = (spd_nas_id_t *) id;

	if (nas_id == NULL)
		return -2;

	pthread_cancel(nas_id->nas_event_handler);
	pthread_join(nas_id->nas_event_handler, NULL);

	pthread_mutex_destroy(&nas_id->pt_mutex);
	pthread_mutex_destroy(&nas_id->flow_mutex);

	AuCloseServer(nas_id->aud);

	g_free(nas_id);
	id = NULL;

	return 0;
}

static int nas_set_volume(AudioID * id, int volume)
{
	return 0;
}

static void nas_set_loglevel(int level)
{
	if (level) {
		nas_log_level = level;
	}
}

static char const *nas_get_playcmd(void)
{
	return NULL;
}

/* Provide the NAS backend */
static spd_audio_plugin_t nas_functions = {
	"nas",
	nas_open,
	nas_play,
	nas_stop,
	nas_close,
	nas_set_volume,
	nas_set_loglevel,
	nas_get_playcmd
};

spd_audio_plugin_t *nas_plugin_get(void)
{
	return &nas_functions;
}

spd_audio_plugin_t *SPD_AUDIO_PLUGIN_ENTRY(void)
    __attribute__ ((weak, alias("nas_plugin_get")));
