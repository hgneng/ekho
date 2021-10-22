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
 * This module is based on skeleton0, and shows how it can be completed easily
 * to run Espeak-NG asynchronously.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <espeak-ng/espeak_ng.h>
#include <espeak-ng/speak_lib.h>

#include "module_main.h"

int module_config(const char *configfile)
{
	/* Optional: Open and parse configfile */
	fprintf(stderr, "opening %s\n", configfile);

	return 0;
}

static int callback(short *, int, espeak_EVENT*);

int module_init(char **msg)
{
	/* Actually initialize synthesizer */
	fprintf(stderr, "initializing\n");

	espeak_ng_ERROR_CONTEXT context = NULL;
	espeak_ng_STATUS result;

	espeak_ng_InitializePath(NULL);
	result = espeak_ng_Initialize(&context);

	if (result == ENS_OK) {
		fprintf(stderr, "initialized, opening audio output\n");
		result = espeak_ng_InitializeOutput(ENOUTPUT_MODE_SPEAK_AUDIO, 0, NULL);
	}

	if (result == ENS_OK) {
		espeak_SetSynthCallback(callback);
	}

	if (result != ENS_OK) {
		char buf[128];
		espeak_ng_GetStatusCodeMessage(result, buf, sizeof(buf));
		fprintf(stderr, "espeak-ng initialization failed: '%s'\n", buf);
		*msg = strdup(buf);
		return -1;
	}

	*msg = strdup("ok!");

	return 0;
}

SPDVoice **module_list_voices(void)
{
	/* Return list of voices */
	SPDVoice **ret = malloc(3*sizeof(*ret));

	ret[0] = malloc(sizeof(*(ret[0])));
	ret[0]->name = strdup("English_(America)");
	ret[0]->language = strdup("en");
	ret[0]->variant = NULL;

	ret[1] = malloc(sizeof(*(ret[0])));
	ret[1]->name = strdup("French_(France)");
	ret[1]->language = strdup("fr");
	ret[1]->variant = NULL;

	ret[2] = NULL;

	return ret;
}


int module_set(const char *var, const char *val)
{
	/* Optional: accept parameter */
	espeak_ng_STATUS result;

	fprintf(stderr,"got var '%s' to be set to '%s'\n", var, val);

	if (!strcmp(var, "voice")) {
		/* TODO */
		return 0;
	} else if (!strcmp(var, "synthesis_voice")) {
		if (strcmp(val, "NULL") != 0) {
			result = espeak_ng_SetVoiceByName(val);
			if (result != ENS_OK) {
				espeak_ng_PrintStatusCodeMessage(result, stderr, NULL);
				return -1;
			}
		}
		return 0;
	} else if (!strcmp(var, "language")) {
		espeak_VOICE voice_select;
		memset(&voice_select, 0, sizeof(voice_select));
		voice_select.languages = val;
		result = espeak_ng_SetVoiceByProperties(&voice_select);
		if (result != ENS_OK) {
			espeak_ng_PrintStatusCodeMessage(result, stderr, NULL);
			return -1;
		}
		return 0;
	} else if (!strcmp(var, "rate")) {
		/* TODO */
		return 0;
	} else if (!strcmp(var, "pitch")) {
		/* convert from [-100, 100] to [0, 100] */
		int pitch = (atoi(val) + 100) / 2;
		result = espeak_SetParameter(espeakPITCH, pitch, 0);
		if (result != ENS_OK) {
			espeak_ng_PrintStatusCodeMessage(result, stderr, NULL);
			return -1;
		}
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

static int began;
/* Asynchronous version, when the synthesis implements asynchronous
 * processing in another thread. */
int module_speak(const char *data, size_t bytes, SPDMessageType msgtype)
{
	/* Speak the provided data asynchronously in another thread */
	fprintf(stderr, "speaking '%s'\n", data);

	began = 0;
	espeak_Synth(data, strlen(data) + 1, 0, POS_CHARACTER, 0,
		     espeakCHARS_AUTO | espeakPHONEMES | espeakENDPAUSE | espeakSSML,
		     NULL, NULL);

	return 1;
}

/* This is getting called in the espeak-ng thread */
static int callback(short *wav, int numsamples, espeak_EVENT *events)
{
	espeak_EVENT *cur = events;

	if (!began) {
		began = 1;
		module_report_event_begin();
	}
	while (cur->type != espeakEVENT_LIST_TERMINATED)
	{
		fprintf(stderr, "got event %d from synth\n", cur->type);
		switch (cur->type) {
			case espeakEVENT_MSG_TERMINATED:
				module_report_event_end();
				break;
			default:
				break;
		}
		cur++;
	}

	return 0;
}

size_t module_pause(void)
{
	/* Pause playing */
	fprintf(stderr, "pausing\n");

	/* Only supports stopping */
	espeak_Cancel();

	return 0;
}

int module_stop(void)
{
	/* Stop any current synth */
	fprintf(stderr, "stopping\n");

	espeak_Cancel();

	return 0;
}

int module_close(void)
{
	/* Deinitialize synthesizer */
	fprintf(stderr, "closing\n");

	espeak_ng_Terminate();

	return 0;
}
