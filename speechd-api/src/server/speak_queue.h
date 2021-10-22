/*
 * speak_queue.h - Speak queue helper for Speech Dispatcher modules
 *
 * Copyright (C) 2007 Brailcom, o.p.s.
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

/*
 * This provides convenience support for pipeline speech synthesis in modules.
 *
 *
 * In summary, a module will look like this:
 *
 * int module_init(char **status_info) {
 * 	if (mysynth_init()) {
 * 		*status_info = "synth initialization failed";
 * 		return -1;
 * 	}
 * 	mysynth_set_play_callback(mycallback_play);
 * 	mysynth_set_mark_callback(mycallback_mark);
 * 	mysynth_set_end_callback(mycallback_end);
 *
 * 	if (module_speak_queue_init(queue_size, status_info))
 * 		return -1;
 *
 * 	*status_info = "initialization succeeded";
 * }
 *
 * int module_speak(gchar *data, size_t bytes, SPDMessageType msgtype) {
 * 	if (!module_speak_queue_before_synth())
 * 		return 0;
 *
 * 	switch (msgtype) {
 * 	case SPD_MSGTYPE_TEXT:
 * 		mysynth_synth(data, bytes);
 * 		break;
 * 	...
 * }
 *
 * int mycallback_play(short *wav, int samples)
 * {
 * 	if (module_speak_queue_stop_requested()) {
 * 		return STOP;
 * 	}
 * 	AudioTrack track = {
 * 		.samples = wav,
 * 		.num_samples = samples,
 * 		.bits = ...,
 * 		...
 * 	};
 *
 * 	module_speak_queue_before_play();
 * 	module_speak_queue_add_audio(&track, SPD_AUDIO_LE);
 * }
 *
 * int mycallback_mark(const char *mark)
 * {
 * 	if (module_speak_queue_stop_requested()) {
 * 		return STOP;
 * 	}
 * 	module_speak_queue_before_play();
 * 	module_speak_queue_add_mark(mark);
 * }
 *
 * int mycallback_end(const char *mark)
 * {
 * 	if (module_speak_queue_stop_requested()) {
 * 		return STOP;
 * 	}
 * 	module_speak_queue_before_play();
 * 	module_speak_queue_add_end();
 * }
 *
 * void module_speak_queue_cancel(void)
 * {
 * 	mysynth_cancel();
 * }
 *
 * int module_stop(void) {
 * 	module_speak_queue_stop();
 * }
 *
 * int module_pause(void) {
 * 	module_speak_queue_pause();
 * }
 *
 * int module_close(void) {
 * 	module_speak_queue_terminate();
 * 	mysynth_terminate();
 * 	module_speak_queue_free();
 * }
 *
 *
 * the module_speak_queue_before_synth() call replaces module_report_event_begin(),
 * the module_speak_queue_add_end() call replaces module_report_event_end(),
 * the module_speak_queue_stop() call replaces module_report_event_stop() and spd_audio_stop(),
 * the module_speak_queue_pause() call replaces module_report_event_pause(),
 *
 * The principle is that module_speak_queue_init() starts playback threads which
 * will handle the audio part. The mysynth_synth() call from module_speak() will
 * periodically callback into mycallback_*(), which just queues audio to the
 * playback threads, which can thus start playing immediately, without having to
 * wait for the whole synth to be completed.
 */

#ifndef __MODULE_UTILS_SPEAK_QUEUE_H
#define __MODULE_UTILS_SPEAK_QUEUE_H

#include <pthread.h>
#include <glib.h>

#include "spd_audio_plugin.h"

extern AudioID *module_audio_id;

typedef enum {
	SPEAK_QUEUE_QET_AUDIO,	/* Chunk of audio. */
	SPEAK_QUEUE_QET_INDEX_MARK,	/* Index mark event. */
	SPEAK_QUEUE_QET_SOUND_ICON,	/* A Sound Icon */
	SPEAK_QUEUE_QET_BEGIN,	/* Beginning of speech. */
	SPEAK_QUEUE_QET_END,		/* Speech completed. */
	SPEAK_QUEUE_QET_PAUSE,		/* Speech pause. */
	SPEAK_QUEUE_QET_STOP,		/* Speech stop. */
	SPEAK_QUEUE_QET_BROKEN,		/* Speech module is broken. */
} speak_queue_entry_type;

typedef struct {
	AudioTrack track;
	AudioFormat format;
} speak_queue_audio_chunk;

typedef struct {
	speak_queue_entry_type type;
	union {
		char *markId;
		speak_queue_audio_chunk audio;
		char *sound_icon_filename;
	} data;
} speak_queue_entry;

/* To be called in module_init after synth initialization, to start playback
 * threads.  */
int module_speak_queue_init(int maxsize, char **status_info);


/* To be called from module_speak before synthesizing the voice.  */
int module_speak_queue_before_synth(void);


/* To be called from the synth callback before looking through its events.  */
int module_speak_queue_before_play(void);

/* To be called from the synth callback to push different types of events.  */
gboolean module_speak_queue_add_audio(const AudioTrack *track, AudioFormat format);
gboolean module_speak_queue_add_mark(const char *markId);
gboolean module_speak_queue_add_sound_icon(const char *filename);
/* To be called on the last synth callback call.  */
gboolean module_speak_queue_add_end(void);

/* To be called in the synth callback to look for early stopping.  */
int module_speak_queue_stop_requested(void);


/* To be called from module_stop.  */
void module_speak_queue_stop(void);

/* To be called from module_pause.  */
void module_speak_queue_pause(void);

/* To be called first from module_close to terminate audio early.  */
void module_speak_queue_terminate(void);

/* To be called last from module_close to release resources.  */
void module_speak_queue_free(void);

/* Can be called early to quickly discard audio */
void module_speak_queue_flush(void);

/* To be provided by the module, shall stop the synthesizer, i.e. make
 * it stop calling the module callback, and thus make the module stop calling
 * module_speak_queue_add_*.  */
void module_speak_queue_cancel(void);

#endif /* #ifndef __MODULE_UTILS_SPEAK_QUEUE_H */
