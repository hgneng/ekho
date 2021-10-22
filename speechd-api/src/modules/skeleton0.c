/*
 * skeleton0.c - Trivial module example
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

/*
 * This module example is the simplest that can be used as a basis for writing
 * your module (which can be proprietary since this is provided under the BSD
 * licence).
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "module_main.h"

static int stop_requested;

int module_config(const char *configfile)
{
	/* Optional: Open and parse configfile */
	fprintf(stderr, "opening %s\n", configfile);

	return 0;
}

int module_init(char **msg)
{
	/* TODO: Actually initialize synthesizer */
	fprintf(stderr, "initializing\n");

	*msg = strdup("ok!");

	return 0;
}

SPDVoice **module_list_voices(void)
{
	/* TODO: Return list of voices */
	SPDVoice **ret = malloc(2*sizeof(*ret));

	ret[0] = malloc(sizeof(*(ret[0])));
	ret[0]->name = strdup("foo");
	ret[0]->language = strdup("eo");
	ret[0]->variant = NULL;

	ret[1] = NULL;

	return ret;
}


int module_set(const char *var, const char *val)
{
	/* Optional: accept parameter */

	fprintf(stderr,"got var '%s' to be set to '%s'\n", var, val);

	if (!strcmp(var, "voice")) {
		/* TODO */
		return 0;
	} else if (!strcmp(var, "synthesis_voice")) {
		/* TODO */
		return 0;
	} else if (!strcmp(var, "language")) {
		/* TODO */
		return 0;
	} else if (!strcmp(var, "rate")) {
		/* TODO */
		return 0;
	} else if (!strcmp(var, "pitch")) {
		/* TODO */
		return 0;
	} else if (!strcmp(var, "pitch_range")) {
		/* TODO */
		return 0;
	} else if (!strcmp(var, "volume")) {
		/* TODO */
		return 0;
	} else if (!strcmp(var, "punctuation_mode")) {
		/* TODO */
		return 0;
	} else if (!strcmp(var, "spelling_mode")) {
		/* TODO */
		return 0;
	} else if (!strcmp(var, "cap_let_recogn")) {
		/* TODO */
		return 0;
	}
	return -1;
}

int module_audio_set(const char *var, const char *val)
{
	/* Optional: interpret audio parameter */
	if (!strcmp(var, "audio_output_method")) {
		if (strcmp(val, "oss") != 0 &&
		    strcmp(val, "alsa") != 0 &&
		    strcmp(val, "nas") != 0 &&
		    strcmp(val, "pulse") != 0)
			return -1;
		/* TODO: respect configuration */
		return 0;
	} else if (!strcmp(var, "audio_oss_device")) {
		/* TODO */
		return 0;
	} else if (!strcmp(var, "audio_alsa_device")) {
		/* TODO */
		return 0;
	} else if (!strcmp(var, "audio_nas_server")) {
		/* TODO */
		return 0;
	} else if (!strcmp(var, "audio_pulse_device")) {
		/* TODO */
		return 0;
	} else if (!strcmp(var, "audio_pulse_min_length")) {
		/* TODO */
		return 0;
	}
	return -1;
}

int module_audio_init(char **status)
{
	/* Optional: open audio */
	return 0;
}

int module_loglevel_set(const char *var, const char *val)
{
	/* Optional: accept loglevel change */
	return 0;
}

int module_debug(int enable, const char *file)
{
	/* Optional: if enable == 1, open file to dump debugging */
	/* Otherwise close it */
	return 0;
}

int module_loop(void)
{
	/* Main loop */
	fprintf(stderr, "main loop\n");

	/* Let module_process run the protocol */
	/* You may want to monitor STDIN_FILENO yourself, to be able to also
	 * monitor other FDs. */
	int ret = module_process(STDIN_FILENO, 1);

	if (ret != 0)
		fprintf(stderr, "Broken pipe, exiting...\n");

	return ret;
}

#if 1
/* Synchronous version, when the synthesis doesn't implement asynchronous
 * processing in another thread. */
void module_speak_sync(const char *data, size_t bytes, SPDMessageType msgtype)
{
	/* TODO: first make quick check over data, on error call
	 * module_speak_error and return. */

	stop_requested = 0;

	module_speak_ok();

	fprintf(stderr, "speaking '%s'\n", data);

	/* TODO: start synthesis */

	module_report_event_begin();

	/* TODO: produce wave */

	module_report_event_end();
}
#else
/* Asynchronous version, when the synthesis implements asynchronous
 * processing in another thread. */
int module_speak(const char *data, size_t bytes, SPDMessageType msgtype)
{
	/* TODO: Speak the provided data asynchronously in another thread */
	fprintf(stderr, "speaking '%s'\n", data);
	/* TODO: asynchronous processing should call module_report_event_begin()
	 * when starting to produce audio, and module_report_event_end() when
	 * finished with producing audio. */
	return 1;
}
#endif

size_t module_pause(void)
{
	/* TODO: Pause playing */
	fprintf(stderr, "pausing\n");
	stop_requested = 1;

	return 0;
}

int module_stop(void)
{
	/* TODO: Stop any current synth */
	fprintf(stderr, "stopping\n");
	stop_requested = 1;

	return 0;
}

int module_close(void)
{
	/* TODO: Deinitialize synthesizer */
	fprintf(stderr, "closing\n");

	return 0;
}
