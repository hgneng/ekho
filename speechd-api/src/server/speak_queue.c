/*
 * speak_queue.c - Speak queue helper
 *
 * Copyright (C) 2003,2006,2007 Brailcom, o.p.s.
 * Copyright (C) 2019-2021 Samuel Thibault <samuel.thibault@ens-lyon.org>
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
 * Based on espeak.c
 *
 * @author Lukas Loehrer
 * Based on ibmtts.c.
 */

#include <sndfile.h>

#include "speak_queue.h"
#include "common.h"
#include "spd_audio.h"

#define DBG_MODNAME "speak_queue"

typedef enum {
	IDLE,
	BEFORE_SYNTH,
	BEFORE_PLAY,
	SPEAKING
} speak_queue_state_t;

typedef enum {
	SPEAK_QUEUE_PAUSE_OFF,
	SPEAK_QUEUE_PAUSE_REQUESTED,
	SPEAK_QUEUE_PAUSE_MARK_REPORTED
} speak_queue_pause_state_t;

/* Thread and process control. */
/* Global mutex for the whole speak queue mechanism */
static pthread_mutex_t speak_queue_mutex = PTHREAD_MUTEX_INITIALIZER;

static speak_queue_state_t speak_queue_state = IDLE;
static gboolean speak_queue_configured = FALSE; /* Whether we have configured audio */

static pthread_t speak_queue_play_thread;
static pthread_t speak_queue_stop_or_pause_thread;

/* Used to wake the stop_or_pause thread from main */
static pthread_cond_t speak_queue_stop_or_pause_cond = PTHREAD_COND_INITIALIZER;

static int speak_queue_stop_or_pause_sleeping;

/* Used to wake the play thread from main */
static pthread_cond_t speak_queue_play_cond = PTHREAD_COND_INITIALIZER;
/* Used to wait for the play thread to go sleeping */
static pthread_cond_t speak_queue_play_sleeping_cond = PTHREAD_COND_INITIALIZER;
static int speak_queue_play_sleeping;

static gboolean speak_queue_close_requested = FALSE;
static speak_queue_pause_state_t speak_queue_pause_state = SPEAK_QUEUE_PAUSE_OFF;
static gboolean speak_queue_stop_requested = FALSE;
static gboolean speak_queue_flush_requested = FALSE;

static void module_speak_queue_reset(void);

/* The playback queue. */

static int speak_queue_maxsize;

static GSList *playback_queue = NULL;
static int playback_queue_size = 0;	/* Number of audio frames currently in queue */

/* Use to wait for queue room availability. Theoretically several threads might
 * be wanting to push, so use broadcast. */
static pthread_cond_t playback_queue_room_condition = PTHREAD_COND_INITIALIZER;
/* Use to wait for queue data availability */
static pthread_cond_t playback_queue_data_condition = PTHREAD_COND_INITIALIZER;

/* Internal function prototypes for playback thread. */
static gboolean speak_queue_add_flag_to_playback_queue(speak_queue_entry_type type);
static void speak_queue_delete_playback_queue_entry(speak_queue_entry *
					       playback_queue_entry);
static gboolean speak_queue_send_to_audio(speak_queue_entry *
				     playback_queue_entry);

/* Miscellaneous internal function prototypes. */
static void speak_queue_clear_playback_queue();

/* The playback thread start routine. */
static void *speak_queue_play(void *);
/* The stop_or_pause start routine. */
static void *speak_queue_stop_or_pause(void *);

int module_speak_queue_init(int maxsize, char **status_info)
{
	int ret;

	speak_queue_maxsize = maxsize;

	/* Reset global state */
	module_speak_queue_reset();

	DBG(DBG_MODNAME " Creating new thread for stop or pause.");
	speak_queue_stop_or_pause_sleeping = 0;

	ret =
	    spd_pthread_create(&speak_queue_stop_or_pause_thread, NULL,
			       speak_queue_stop_or_pause, NULL);
	if (0 != ret) {
		DBG("Failed to create stop-or-pause thread.");
		*status_info =
		    g_strdup("Failed to create stop-or-pause thread.");
		return -1;
	}

	speak_queue_play_sleeping = 0;

	DBG(DBG_MODNAME " Creating new thread for playback.");
	ret = spd_pthread_create(&speak_queue_play_thread, NULL, speak_queue_play, NULL);
	if (ret != 0) {
		DBG("Failed to create playback thread.");
		*status_info = g_strdup("Failed to create playback thread.");
		return -1;
	}

	return 0;
}

void module_speak_queue_reset(void)
{
	speak_queue_state = IDLE;
	speak_queue_pause_state = SPEAK_QUEUE_PAUSE_OFF;
	speak_queue_stop_requested = FALSE;
	speak_queue_flush_requested = FALSE;
}

int module_speak_queue_before_synth(void)
{
	pthread_mutex_lock(&speak_queue_mutex);
	if (speak_queue_state != IDLE) {
		DBG(DBG_MODNAME " Warning, module_speak called when not ready.");
		pthread_mutex_unlock(&speak_queue_mutex);
		return FALSE;
	}

	module_speak_queue_reset();
	speak_queue_state = BEFORE_SYNTH;
	pthread_mutex_unlock(&speak_queue_mutex);
	return TRUE;
}

int module_speak_queue_before_play(void)
{
	int ret = 0;
	pthread_mutex_lock(&speak_queue_mutex);
	if (speak_queue_state == BEFORE_SYNTH) {
		ret = 1;
		speak_queue_state = BEFORE_PLAY;
		speak_queue_add_flag_to_playback_queue(SPEAK_QUEUE_QET_BEGIN);
		/* Wake up playback thread */
		pthread_cond_signal(&speak_queue_play_cond);
	}
	pthread_mutex_unlock(&speak_queue_mutex);
	return ret;
}

gboolean module_speak_queue_add_end(void)
{
	pthread_mutex_lock(&speak_queue_mutex);
	gboolean ret = speak_queue_add_flag_to_playback_queue(SPEAK_QUEUE_QET_END);
	pthread_mutex_unlock(&speak_queue_mutex);
	return ret;
}

static speak_queue_entry *playback_queue_pop()
{
	speak_queue_entry *result = NULL;
	pthread_mutex_lock(&speak_queue_mutex);
	while (!speak_queue_stop_requested && playback_queue == NULL) {
		pthread_cond_wait(&playback_queue_data_condition,
				  &speak_queue_mutex);
	}
	if (!speak_queue_stop_requested) {
		result = (speak_queue_entry *) playback_queue->data;
		playback_queue =
		    g_slist_remove(playback_queue, playback_queue->data);
		if (result->type == SPEAK_QUEUE_QET_AUDIO) {
			playback_queue_size -= result->data.audio.track.num_samples;
			pthread_cond_broadcast(&playback_queue_room_condition);
		}
	}
	pthread_mutex_unlock(&speak_queue_mutex);
	return result;
}

static gboolean playback_queue_push(speak_queue_entry * entry)
{
	playback_queue = g_slist_append(playback_queue, entry);
	if (entry->type == SPEAK_QUEUE_QET_AUDIO) {
		playback_queue_size += entry->data.audio.track.num_samples;
	}
	pthread_cond_signal(&playback_queue_data_condition);
	return TRUE;
}

/* Adds a chunk of pcm audio to the audio playback queue.
   Waits until there is enough space in the queue. */
gboolean
module_speak_queue_add_audio(const AudioTrack *track, AudioFormat format)
{
	pthread_mutex_lock(&speak_queue_mutex);
	while (playback_queue_size > speak_queue_maxsize) {
		if (speak_queue_state == IDLE
			|| speak_queue_stop_requested
			|| speak_queue_flush_requested) {
			pthread_mutex_unlock(&speak_queue_mutex);
			return FALSE;
		}
		pthread_cond_wait(&playback_queue_room_condition,
				  &speak_queue_mutex);
	}
	if (speak_queue_state == IDLE || speak_queue_stop_requested || speak_queue_flush_requested) {
		pthread_mutex_unlock(&speak_queue_mutex);
		return FALSE;
	}

	speak_queue_entry *playback_queue_entry =
	    g_new(speak_queue_entry, 1);

	playback_queue_entry->type = SPEAK_QUEUE_QET_AUDIO;
	playback_queue_entry->data.audio.track = *track;
	gint nbytes = track->bits / 8 * track->num_samples;
	playback_queue_entry->data.audio.track.samples = g_memdup(track->samples, nbytes);
	playback_queue_entry->data.audio.format = format;

	playback_queue_push(playback_queue_entry);
	pthread_mutex_unlock(&speak_queue_mutex);
	return TRUE;
}

/* Adds an Index Mark to the audio playback queue. */
gboolean module_speak_queue_add_mark(const char *markId)
{
	speak_queue_entry *playback_queue_entry =
	    (speak_queue_entry *) g_malloc(sizeof(speak_queue_entry));

	playback_queue_entry->type = SPEAK_QUEUE_QET_INDEX_MARK;
	playback_queue_entry->data.markId = g_strdup(markId);
	pthread_mutex_lock(&speak_queue_mutex);
	gboolean ret = playback_queue_push(playback_queue_entry);
	pthread_mutex_unlock(&speak_queue_mutex);
	return ret;
}

/* Adds a begin or end flag to the playback queue. */
static gboolean speak_queue_add_flag_to_playback_queue(speak_queue_entry_type type)
{
	speak_queue_entry *playback_queue_entry =
	    (speak_queue_entry *) g_malloc(sizeof(speak_queue_entry));

	playback_queue_entry->type = type;
	return playback_queue_push(playback_queue_entry);
}

/* Add a sound icon to the playback queue. */
gboolean module_speak_queue_add_sound_icon(const char *filename)
{
	speak_queue_entry *playback_queue_entry =
	    (speak_queue_entry *) g_malloc(sizeof(speak_queue_entry));

	playback_queue_entry->type = SPEAK_QUEUE_QET_SOUND_ICON;
	playback_queue_entry->data.sound_icon_filename = g_strdup(filename);
	pthread_mutex_lock(&speak_queue_mutex);
	gboolean ret = playback_queue_push(playback_queue_entry);
	pthread_mutex_unlock(&speak_queue_mutex);
	return ret;
}

/* Deletes an entry from the playback audio queue, freeing memory. */
static void
speak_queue_delete_playback_queue_entry(speak_queue_entry * playback_queue_entry)
{
	switch (playback_queue_entry->type) {
	case SPEAK_QUEUE_QET_AUDIO:
		g_free(playback_queue_entry->data.audio.track.samples);
		break;
	case SPEAK_QUEUE_QET_INDEX_MARK:
		g_free(playback_queue_entry->data.markId);
		break;
	case SPEAK_QUEUE_QET_SOUND_ICON:
		g_free(playback_queue_entry->data.sound_icon_filename);
		break;
	default:
		break;
	}
	g_free(playback_queue_entry);
}

/* Erases the entire playback queue, freeing memory. */
static void speak_queue_clear_playback_queue()
{
	pthread_mutex_lock(&speak_queue_mutex);

	while (NULL != playback_queue) {
		speak_queue_entry *playback_queue_entry =
		    playback_queue->data;
		speak_queue_delete_playback_queue_entry(playback_queue_entry);
		playback_queue =
		    g_slist_remove(playback_queue, playback_queue->data);
	}
	playback_queue = NULL;
	playback_queue_size = 0;
	pthread_cond_broadcast(&playback_queue_room_condition);
	pthread_mutex_unlock(&speak_queue_mutex);
}

/* Sends a chunk of audio to the audio player and waits for completion or error. */
static gboolean speak_queue_send_track_to_audio(AudioTrack *track, AudioFormat format)
{
	int ret = 0;
	DBG(DBG_MODNAME " Sending %i samples to audio.",
	    track->num_samples);
	if (!speak_queue_configured)
	{
		spd_audio_begin(module_audio_id, *track, format);
		speak_queue_configured = TRUE;
	}
	ret = spd_audio_feed_sync_overlap(module_audio_id, *track, format);
	if (ret < 0) {
		DBG("ERROR: Can't play track for unknown reason.");
		return FALSE;
	}
	DBG(DBG_MODNAME " Sent to audio.");
	return TRUE;
}

/* Plays the specified audio file. */
static gboolean speak_queue_send_file_to_audio(const char *filename)
{
	gboolean result = 0;
	int subformat;
	sf_count_t items;
	sf_count_t readcount;
	SNDFILE *sf;
	SF_INFO sfinfo;

	DBG("Playing |%s|", filename);
	memset(&sfinfo, 0, sizeof(sfinfo));
	sf = sf_open(filename, SFM_READ, &sfinfo);
	if (NULL == sf) {
		DBG("%s", sf_strerror(NULL));
		return -1;
	}
	if (sfinfo.channels < 1 || sfinfo.channels > 2) {
		DBG("ERROR: channels = %d.\n", sfinfo.channels);
		result = FALSE;
		goto cleanup1;
	}
	if (sfinfo.frames > 0x7FFFFFFF || sfinfo.frames == 0) {
		DBG("ERROR: Unknown number of frames.");
		result = FALSE;
		goto cleanup1;
	}

	subformat = sfinfo.format & SF_FORMAT_SUBMASK;
	items = sfinfo.channels * sfinfo.frames;
	DBG("Frames = %jd, channels = %ld", sfinfo.frames,
	    (long)sfinfo.channels);
	DBG("Samplerate = %i, items = %lld", sfinfo.samplerate,
	    (long long)items);
	DBG("Major format = 0x%08X, subformat = 0x%08X, endian = 0x%08X",
	    sfinfo.format & SF_FORMAT_TYPEMASK, subformat,
	    sfinfo.format & SF_FORMAT_ENDMASK);

	if (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE) {
		/* Set scaling for float to integer conversion. */
		sf_command(sf, SFC_SET_SCALE_FLOAT_INT_READ, NULL, SF_TRUE);
	}
	AudioTrack track;
	track.num_samples = sfinfo.frames;
	track.num_channels = sfinfo.channels;
	track.sample_rate = sfinfo.samplerate;
	track.bits = 16;
	track.samples = g_malloc(items * sizeof(short));
	readcount = sf_read_short(sf, (short *)track.samples, items);
	DBG("Read %lld items from audio file.", (long long)readcount);

	if (readcount > 0) {
		track.num_samples = readcount / sfinfo.channels;
		DBG("Sending %i samples to audio.", track.num_samples);
		int ret = speak_queue_send_track_to_audio(&track, SPD_AUDIO_LE);
		if (ret < 0) {
			DBG("ERROR: Can't play track for unknown reason.");
			result = -1;
			goto cleanup2;
		}
		DBG("Sent to audio.");
	}
cleanup2:
	g_free(track.samples);
cleanup1:
	sf_close(sf);
	return result;
}

static gboolean speak_queue_send_to_audio(speak_queue_entry * playback_queue_entry)
{
	return speak_queue_send_track_to_audio(&playback_queue_entry->data.audio.track,
					        playback_queue_entry->data.audio.format);
}

/* Playback thread. */
static void *speak_queue_play(void *nothing)
{
	char *markId;
	speak_queue_entry *playback_queue_entry = NULL;

	DBG(DBG_MODNAME " Playback thread starting.......");

	pthread_mutex_lock(&speak_queue_mutex);
	while (!speak_queue_close_requested) {
		speak_queue_play_sleeping = 1;
		pthread_cond_signal(&speak_queue_play_sleeping_cond);
		while (speak_queue_state < BEFORE_PLAY && !speak_queue_close_requested) {
			pthread_cond_wait(&speak_queue_play_cond, &speak_queue_mutex);
		}
		speak_queue_play_sleeping = 0;
		pthread_cond_signal(&speak_queue_play_sleeping_cond);
		DBG(DBG_MODNAME " Playback.");
		if (speak_queue_close_requested)
			break;
		pthread_mutex_unlock(&speak_queue_mutex);

		while (1) {
			gboolean finished = FALSE;
			playback_queue_entry = playback_queue_pop();
			if (playback_queue_entry == NULL) {
				DBG(DBG_MODNAME " playback thread detected stop.");
				break;
			}

			switch (playback_queue_entry->type) {
			case SPEAK_QUEUE_QET_AUDIO:
				speak_queue_send_to_audio(playback_queue_entry);
				break;
			case SPEAK_QUEUE_QET_INDEX_MARK:
				markId = playback_queue_entry->data.markId;
				DBG(DBG_MODNAME " reporting index mark |%s|.",
				    markId);
				module_report_index_mark(markId);
				DBG(DBG_MODNAME " index mark reported.");
				pthread_mutex_lock(&speak_queue_mutex);
				if (speak_queue_state == SPEAKING
				    && speak_queue_pause_state ==
				    SPEAK_QUEUE_PAUSE_REQUESTED
				    && speak_queue_stop_or_pause_sleeping
				    && g_str_has_prefix(markId, "__spd_")) {
					DBG(DBG_MODNAME " Pause requested in playback thread.  Stopping.");
					speak_queue_stop_requested = TRUE;
					speak_queue_pause_state =
					    SPEAK_QUEUE_PAUSE_MARK_REPORTED;
					pthread_cond_signal
					    (&speak_queue_stop_or_pause_cond);
					finished = TRUE;
				}
				pthread_mutex_unlock(&speak_queue_mutex);
				break;
			case SPEAK_QUEUE_QET_SOUND_ICON:
				if (speak_queue_configured) {
					spd_audio_end(module_audio_id);
					speak_queue_configured = FALSE;
				}
				speak_queue_send_file_to_audio(playback_queue_entry->
						 data.sound_icon_filename);
				break;
			case SPEAK_QUEUE_QET_BEGIN:{
					gboolean report_begin = FALSE;
					pthread_mutex_lock(&speak_queue_mutex);
					if (speak_queue_state == BEFORE_PLAY) {
						speak_queue_state = SPEAKING;
						report_begin = TRUE;
					}
					pthread_mutex_unlock
					    (&speak_queue_mutex);
					if (report_begin)
						module_report_event_begin();
					break;
				}
			case SPEAK_QUEUE_QET_END:
				if (speak_queue_configured) {
					spd_audio_end(module_audio_id);
					speak_queue_configured = FALSE;
				}
				pthread_mutex_lock(&speak_queue_mutex);
				DBG(DBG_MODNAME " playback thread got END from queue.");
				if (speak_queue_state == SPEAKING) {
					if (!speak_queue_stop_requested) {
						DBG(DBG_MODNAME " playback thread reporting end.");
						speak_queue_state = IDLE;
						speak_queue_pause_state =
						    SPEAK_QUEUE_PAUSE_OFF;
					}
					finished = TRUE;
				}
				pthread_mutex_unlock(&speak_queue_mutex);
				if (finished)
					module_report_event_end();
				break;
			case SPEAK_QUEUE_QET_PAUSE:
				DBG(DBG_MODNAME " ??? got pause in speak queue ???");
				break;
			case SPEAK_QUEUE_QET_STOP:
				DBG(DBG_MODNAME " ??? got stop in speak queue ???");
				break;
			case SPEAK_QUEUE_QET_BROKEN:
				DBG(DBG_MODNAME " ??? got broken in speak queue ???");
				break;
			}

			speak_queue_delete_playback_queue_entry
			    (playback_queue_entry);
			if (finished)
				break;
		}
		if (speak_queue_configured) {
			spd_audio_end(module_audio_id);
			speak_queue_configured = FALSE;
		}
		pthread_mutex_lock(&speak_queue_mutex);
	}
	speak_queue_play_sleeping = 1;
	pthread_mutex_unlock(&speak_queue_mutex);
	DBG(DBG_MODNAME " Playback thread ended.......");
	return 0;
}

int module_speak_queue_stop_requested(void)
{
	return speak_queue_stop_requested;
}

void module_speak_queue_flush(void)
{
	pthread_mutex_lock(&speak_queue_mutex);
	speak_queue_flush_requested = TRUE;
	pthread_cond_signal(&playback_queue_room_condition);
	pthread_mutex_unlock(&speak_queue_mutex);
}

void module_speak_queue_stop(void)
{
	pthread_mutex_lock(&speak_queue_mutex);
	if (speak_queue_state != IDLE &&
	    !speak_queue_stop_requested &&
	    speak_queue_stop_or_pause_sleeping) {
		DBG(DBG_MODNAME " stopping...");
		speak_queue_stop_requested = TRUE;
		/* Wake the stop_or_pause thread. */
		pthread_cond_signal(&speak_queue_stop_or_pause_cond);
		/* Unlock anybody trying to push audio. */
		pthread_cond_wait(&playback_queue_room_condition,
				  &speak_queue_mutex);
	} else {
		DBG(DBG_MODNAME " Cannot stop now.");
	}
	pthread_mutex_unlock(&speak_queue_mutex);
}

void module_speak_queue_pause(void)
{
	pthread_mutex_lock(&speak_queue_mutex);
	if (speak_queue_pause_state == SPEAK_QUEUE_PAUSE_OFF && !speak_queue_stop_requested) {
		speak_queue_pause_state = SPEAK_QUEUE_PAUSE_REQUESTED;
	}
	pthread_mutex_unlock(&speak_queue_mutex);
}

void module_speak_queue_terminate(void)
{
	pthread_mutex_lock(&speak_queue_mutex);
	speak_queue_stop_requested = TRUE;
	speak_queue_close_requested = TRUE;

	pthread_cond_broadcast(&playback_queue_room_condition);
	pthread_cond_signal(&playback_queue_data_condition);

	pthread_cond_signal(&speak_queue_play_cond);
	pthread_cond_signal(&speak_queue_stop_or_pause_cond);
	pthread_mutex_unlock(&speak_queue_mutex);

	DBG(DBG_MODNAME " Joining play thread.");
	pthread_join(speak_queue_play_thread, NULL);
	DBG(DBG_MODNAME " Joining stop thread.");
	pthread_join(speak_queue_stop_or_pause_thread, NULL);
}

void module_speak_queue_free(void)
{
	DBG(DBG_MODNAME " Freeing resources.");
	speak_queue_clear_playback_queue();
}

/* Stop or Pause thread. */
static void *speak_queue_stop_or_pause(void *nothing)
{
	int ret;

	DBG(DBG_MODNAME " Stop or pause thread starting.......");

	pthread_mutex_lock(&speak_queue_mutex);
	while (!speak_queue_close_requested) {
		speak_queue_stop_or_pause_sleeping = 1;
		while (!speak_queue_stop_requested)
			pthread_cond_wait(&speak_queue_stop_or_pause_cond, &speak_queue_mutex);
		speak_queue_stop_or_pause_sleeping = 0;

		DBG(DBG_MODNAME " Stop or pause.");
		if (speak_queue_close_requested)
			break;

		pthread_cond_signal(&playback_queue_data_condition);
		pthread_cond_broadcast(&playback_queue_room_condition);
		pthread_mutex_unlock(&speak_queue_mutex);

		if (module_audio_id) {
			pthread_mutex_lock(&speak_queue_mutex);
			speak_queue_state = IDLE;
			pthread_mutex_unlock(&speak_queue_mutex);
			DBG(DBG_MODNAME " Stopping audio.");
			ret = spd_audio_stop(module_audio_id);
			if (ret != 0)
				DBG("spd_audio_stop returned non-zero value.");
			pthread_mutex_lock(&speak_queue_mutex);
			while (!speak_queue_play_sleeping) {
				ret = spd_audio_stop(module_audio_id);
				if (ret != 0)
					DBG("spd_audio_stop returned non-zero value.");
				pthread_mutex_unlock(&speak_queue_mutex);
				g_usleep(5000);
				pthread_mutex_lock(&speak_queue_mutex);
			}
			pthread_mutex_unlock(&speak_queue_mutex);
		} else {
			pthread_mutex_lock(&speak_queue_mutex);
			while (!speak_queue_play_sleeping)
				pthread_cond_wait(&speak_queue_play_sleeping_cond, &speak_queue_mutex);
			pthread_mutex_unlock(&speak_queue_mutex);
		}

		DBG(DBG_MODNAME " Waiting for synthesis to stop.");

		module_speak_queue_cancel();

		DBG(DBG_MODNAME " Clearing playback queue.");
		speak_queue_clear_playback_queue();

		int save_pause_state = speak_queue_pause_state;
		pthread_mutex_lock(&speak_queue_mutex);
		module_speak_queue_reset();
		pthread_mutex_unlock(&speak_queue_mutex);

		if (save_pause_state == SPEAK_QUEUE_PAUSE_MARK_REPORTED) {
			module_report_event_pause();
		} else {
			module_report_event_stop();
		}

		DBG(DBG_MODNAME " Stop or pause thread ended.......\n");
		pthread_mutex_lock(&speak_queue_mutex);
	}
	pthread_mutex_unlock(&speak_queue_mutex);
	pthread_exit(NULL);
}
