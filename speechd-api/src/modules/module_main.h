/*
 * module_main.h - Interface for main loop of output modules.
 *
 * Copyright (C) 2020-2021 Samuel Thibault <samuel.thibault@ens-lyon.org>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY Samuel Thibault AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef _SPEECHD_MODULE_MAIN_H
#define _SPEECHD_MODULE_MAIN_H

#include <speechd_types.h>
#include <spd_audio.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Functions shall return 0 on success, -1 on error
 */

/*
 * These must be provided by the module.
 */

/* Called at startup.  */
int module_config(const char *configfile);

/* Called after server sends INIT.  */
int module_init(char **msg);

/* List voices */
SPDVoice **module_list_voices(void);

/* Asynchronous Speak */
int module_speak(const char *data, size_t bytes, SPDMessageType msgtype);

/* Synchronous Speak */
void module_speak_sync(const char *data, size_t bytes, SPDMessageType msgtype);

/* Report speak start */
void module_speak_begin(void);

/* Report speak end */
void module_speak_end(void);

/* Pause */
size_t module_pause(void);

/* Report speak pause */
void module_speak_pause(void);

/* Stop */
int module_stop(void);

/* Report speak stop */
void module_speak_stop(void);

/* Called before exit.  */
int module_close(void);


/*
 * These can be provided by the module, or obtained from module_utils.c
 */

/* Called after init confirmed.  */
int module_loop(void);

/* Set parameter */
int module_set(const char *var, const char *val);

/* Set audio parameter */
int module_audio_set(const char *var, const char *val);

/* Initialize audio */
int module_audio_init(char **status_info);

/* Set loglevel value */
int module_loglevel_set(const char *var, const char *val);

/* Enable or disable debugging in the given file */
int module_debug(int enable, const char *file);


/*
 * These are provided by the module basis.
 */

/*
 * Specify that the module will send its audio as wave events to the server
 * thanks to module_tts_output_server().
 * To be called in module_init.
 */
void module_audio_set_server(void);

/*
 * Send piece of audio to server. Can be called as many times as desired, e.g.
 * for each chunk produced by the synthesizer.
 */
void module_tts_output_server(const AudioTrack *track, AudioFormat format);

/* Return one line of input from the given file, to be freed with free().
 *
 * Since this function implements its own buffering, it must always be called
 * with the same input, usually simply stdin.
 *
 * When block is set to 1, this waits until a line of input is received, thus
 * never returning NULL.
 *
 * On I/O error or end of file, exit(2) is called.
 */
char *module_readline(int fd, int block);

/* This protects multi-line answers against asynchronous event reporting */
extern pthread_mutex_t module_stdout_mutex;

/* This should be called by module_speak_sync to confirm the data is ok before
 * actually synthesizing. From this point the server may send stop requests, so
 * reseting the stop state must be done before calling this. */
void module_speak_ok(void);

/* This should be called by module_speak_sync to notify that the data is not ok,
 * before returning from module_speak_sync */
void module_speak_error(void);

/* This should be called when reaching a mark */
void module_report_index_mark(const char *mark);
/* This should be called when starting to synthesize */
void module_report_event_begin(void);
/* This should be called when finishing synthesizing */
void module_report_event_end(void);
/* This should be called when stopping the speech */
void module_report_event_stop(void);
/* This should be called when pausing the speech */
void module_report_event_pause(void);
/* This should be called when reaching a sound icon */
void module_report_icon(const char *icon);

/* This processes module input, interpreting the SSIP protocol and calling
 * appropriate module-provided functions.
 *
 * This can be either
 * - called with block set to 1, in which case it will only return when the
 * server sends QUIT, i.e. it acts as the main loop for the module,
 * - or called with block set to 0, in which case it will only process what was
 * already sent by the server and not wait for any further requets.
 *
 * typically be called periodically by the module, to let the server
 * tell if it should stop.
 */
int module_process(int fd, int block);

#ifdef __cplusplus
}
#endif

#endif /* _SPEECHD_MODULE_MAIN_H */
