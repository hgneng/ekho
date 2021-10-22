/*
 * skeleton_config.c - Module example using speechd-provided configuration helpers
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
 * This module example shows a simple example of using the speechd-provided
 * configuration helpers in your module.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#define MODULE_NAME	"skeleton_config"
#define MODULE_VERSION	"0.1"

#include "module_main.h"
#include "module_utils.h"

#define DEBUG_MODULE 1
DECLARE_DEBUG();

MOD_OPTION_1_INT(SkeletonRate);

static int stop_requested;

int module_load(void)
{
	fprintf(stderr, "Initializing parameters\n");

	INIT_SETTINGS_TABLES();

	REGISTER_DEBUG();

	MOD_OPTION_1_INT_REG(SkeletonRate, 42);

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

static void skeleton_set_rate(signed int rate)
{
	/* Configure new rate */
	fprintf(stderr, "setting rate %d\n", rate);
}

static void skeleton_set_pitch(signed int pitch)
{
	/* Configure new pitch */
	fprintf(stderr, "setting pitch %d\n", pitch);
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

	/* Update synth parameters according to message parameters */
	UPDATE_PARAMETER(rate, skeleton_set_rate);
	UPDATE_PARAMETER(pitch, skeleton_set_pitch);

	module_report_event_begin();

	/* TODO: produce wave */

	module_report_event_end();
}
#else
/* Asynchronous version, when the synthesis implements asynchronous
 * processing in another thread. */
int module_speak(const char *data, size_t bytes, SPDMessageType msgtype)
{
	/* Update synth parameters according to message parameters */
	UPDATE_PARAMETER(rate, skeleton_set_rate);
	UPDATE_PARAMETER(pitch, skeleton_set_pitch);

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
