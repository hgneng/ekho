/*
 * module_utils.c - Module utilities
 *           Functions to help writing output modules for Speech Dispatcher
 * Copyright (C) 2003,2006, 2007 Brailcom, o.p.s.
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
 * $Id: module_utils.c,v 1.55 2008-07-10 15:37:18 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <fdsetconv.h>
#include "module_utils.h"
#include "module_main.h"

static char *module_audio_pars[10];

int log_level;

AudioID *module_audio_id;

SPDMsgSettings msg_settings;
SPDMsgSettings msg_settings_old;

int Debug;
FILE *CustomDebugFile;

configfile_t *configfile;
configoption_t *module_dc_options;
int module_num_dc_options;

const char *module_name;

void MSG(int level, const char *format, ...) {
	if (level < 4 || Debug) {
		va_list ap;
		time_t t;
		struct timeval tv;
		char *tstr;
		t = time(NULL);
		tstr = g_strdup(ctime(&t));
		tstr[strlen(tstr)-1] = 0;
		gettimeofday(&tv,NULL);
		fprintf(stderr," %s [%d]",tstr, (int) tv.tv_usec);
		fprintf(stderr, ": ");
		va_start(ap, format);
		vfprintf(stderr, format, ap);
		va_end(ap);
		fprintf(stderr, "\n");
		fflush(stderr);
		if ((Debug==2) || (Debug==3)) {
			fprintf(CustomDebugFile," %s [%d]",tstr, (int) tv.tv_usec);
			fprintf(CustomDebugFile, ": ");
			va_start(ap, format);
			vfprintf(CustomDebugFile, format, ap);
			va_end(ap);
			fprintf(CustomDebugFile, "\n");
			fflush(CustomDebugFile);
		}
		g_free(tstr);
	}
}

#define SET_PARAM_NUM(name, cond) \
	if(!strcmp(cur_item, #name)){ \
		char *tptr; \
		int number; \
		number = strtol(cur_value, &tptr, 10); \
		if(!(cond)){ return -1; } \
		if (tptr == cur_value){ return -1; } \
		msg_settings.name = number; \
	}

#define SET_PARAM_STR(name) \
	if(!strcmp(cur_item, #name)){ \
		g_free(msg_settings.name); \
		if(!strcmp(cur_value, "NULL")) msg_settings.name = NULL; \
		else msg_settings.name = g_strdup(cur_value); \
	}

#define SET_PARAM_STR_C(name, fconv) \
	if(!strcmp(cur_item, #name)){ \
		int ret = fconv(cur_value); \
		if (ret != -1) msg_settings.name = ret; \
		else return -1; \
	}

int module_set(const char *cur_item, const char *cur_value)
{
	SET_PARAM_NUM(rate,
		      ((number >= -100) && (number <= 100)))
	    else
	SET_PARAM_NUM(pitch,
		      ((number >= -100)
		       && (number <= 100)))
	    else
	SET_PARAM_NUM(pitch_range,
		      ((number >= -100)
		       && (number <= 100)))
	    else
	SET_PARAM_NUM(volume,
		      ((number >= -100)
		       && (number <= 100)))
	    else
	SET_PARAM_STR_C(punctuation_mode,
			str2EPunctMode)
	    else
	SET_PARAM_STR_C(spelling_mode, str2ESpellMode)
	    else
	SET_PARAM_STR_C(cap_let_recogn,
			str2ECapLetRecogn)
	    else
	if (!strcmp(cur_item, "voice")) {
		int ret = str2EVoice(cur_value);
		if (ret != -1)
			msg_settings.voice_type = ret;
		else
			return -1;
	} else if (!strcmp(cur_item, "synthesis_voice")) {
		g_free(msg_settings.voice.name);
		if (!strcmp(cur_value, "NULL")) {
			if (msg_settings.voice.name) {
				/* Force to set voice again. */
				msg_settings.voice_type = -1;
			}
			msg_settings.voice.name = NULL;
		} else
			msg_settings.voice.name =
			    g_strdup(cur_value);
	} else if (!strcmp(cur_item, "language")) {
		g_free(msg_settings.voice.language);
		if (!strcmp(cur_value, "NULL"))
			msg_settings.voice.language = NULL;
		else
			msg_settings.voice.language =
			    g_strdup(cur_value);
	} else
		return -1;	/* Unknown parameter */

	return 0;
}

#define SET_AUDIO_STR(name,idx) \
	if(!strcmp(cur_item, #name)){ \
		g_free(module_audio_pars[idx]); \
		if(!strcmp(cur_value, "NULL")) module_audio_pars[idx] = NULL; \
		else module_audio_pars[idx] = g_strdup(cur_value); \
	}

int module_audio_set(const char *cur_item, const char *cur_value) {
	SET_AUDIO_STR(audio_output_method, 0)
	    else
	SET_AUDIO_STR(audio_oss_device, 1)
	    else
	SET_AUDIO_STR(audio_alsa_device, 2)
	    else
	SET_AUDIO_STR(audio_nas_server, 3)
	    else
	      /* TODO: restore AudioPulseServer option
	SET_AUDIO_STR(audio_pulse_server, 4)
	    else
	    */
	SET_AUDIO_STR(audio_pulse_device, 4)
	    else
	SET_AUDIO_STR(audio_pulse_min_length, 5)
	    else
	/* 6 reserved for speech-dispatcher module name */
		return -1;	/* Unknown parameter */
	return 0;
}

#define SET_LOGLEVEL_NUM(name, cond) \
	if(!strcmp(cur_item, #name)){ \
		char *tptr; \
		int number; \
		number = strtol(cur_value, &tptr, 10); \
		if(!(cond)){ return -1; } \
		if (tptr == cur_value){ return -1; } \
		log_level = number; \
		spd_audio_set_loglevel(module_audio_id, number); \
	}

int module_loglevel_set(const char *cur_item, const char *cur_value)
{
	SET_LOGLEVEL_NUM(log_level, 1)
	    else
		return -1;	/* Unknown parameter */
	return 0;
}

int module_debug(int enable, const char *filename)
{
	if (enable) {
		DBG("Additional logging into specific path %s requested",
		    filename);
		FILE *new_CustomDebugFile = fopen(filename, "w+");
		if (new_CustomDebugFile == NULL) {
			DBG("ERROR: Can't open custom debug file for logging: %d (%s)", errno, strerror(errno));
			return -1;
		}
		if (CustomDebugFile != NULL)
			fclose(CustomDebugFile);
		CustomDebugFile = new_CustomDebugFile;
		if (Debug == 1)
			Debug = 3;
		else
			Debug = 2;

		DBG("Additional logging initialized");
	} else {
		if (Debug == 3)
			Debug = 1;
		else
			Debug = 0;

		if (CustomDebugFile != NULL)
			fclose(CustomDebugFile);
		CustomDebugFile = NULL;
		DBG("Additional logging into specific path terminated");
	}

	return 0;
}

#undef SET_PARAM_NUM
#undef SET_PARAM_STR

int module_loop(void)
{
	int ret = module_process(STDIN_FILENO, 1);

	if (ret != 0)
		DBG("Broken pipe, exiting...\n");

	return ret;
}

int
module_get_message_part(const char *message, char *part, unsigned int *pos,
			size_t maxlen, const char *dividers)
{
	int i, n;
	int num_dividers;
	int len;

	assert(part != NULL);
	assert(message != NULL);

	len = strlen(message);

	if (message[*pos] == 0)
		return -1;

	if (dividers != NULL) {
		num_dividers = strlen(dividers);
	} else {
		num_dividers = 0;
	}

	for (i = 0; i <= maxlen - 1; i++) {
		part[i] = message[*pos];

		if (part[i] == 0) {
			return i;
		}
		// DBG("pos: %d", *pos);

		if ((len - 1 - i) > 2) {
			if ((message[*pos + 1] == ' ')
			    || (message[*pos + 1] == '\n')
			    || (message[*pos + 1] == '\r')) {
				for (n = 0; n <= num_dividers - 1; n++) {
					if (part[i] == dividers[n]) {
						part[i + 1] = 0;
						(*pos)++;
						return i + 1;
					}
				}
				if ((message[*pos] == '\n')
				    && (message[*pos + 1] == '\n')) {
					part[i + 1] = 0;
					(*pos)++;
					return i + 1;
				}
				if ((len - 1 - i) > 4) {
					if (((message[*pos] == '\r')
					     && (message[*pos + 1] == '\n'))
					    && ((message[*pos + 2] == '\r')
						&& (message[*pos + 3] ==
						    '\n'))) {
						part[i + 1] = 0;
						(*pos)++;
						return i + 1;
					}
				}
			}
		}

		(*pos)++;
	}
	part[i] = 0;

	return i;
}

void module_strip_punctuation_some(char *message, char *punct_chars)
{
	int len;
	char *p = message;
	int i;
	assert(message != NULL);

	if (punct_chars == NULL)
		return;

	len = strlen(message);
	for (i = 0; i <= len - 1; i++) {
		if (strchr(punct_chars, *p)) {
			DBG("Substitution %d: char -%c- for whitespace\n", i,
			    *p);
			*p = ' ';
		}
		p++;
	}
}

char *module_strip_ssml(const char *message)
{

	int len;
	char *out;
	int i, n;
	int omit = 0;

	assert(message != NULL);

	len = strlen(message);
	out = (char *)g_malloc(sizeof(char) * (len + 1));

	for (i = 0, n = 0; i <= len; i++) {

		if (message[i] == '<') {
			omit = 1;
			continue;
		}
		if (message[i] == '>') {
			omit = 0;
			continue;
		}
		if (!strncmp(&(message[i]), "&lt;", 4)) {
			i += 3;
			out[n++] = '<';
		} else if (!strncmp(&(message[i]), "&gt;", 4)) {
			i += 3;
			out[n++] = '>';
		} else if (!strncmp(&(message[i]), "&amp;", 5)) {
			i += 4;
			out[n++] = '&';
		} else if (!strncmp(&(message[i]), "&quot;", 6)) {
			i += 5;
			out[n++] = '"';
		} else if (!strncmp(&(message[i]), "&apos;", 6)) {
			i += 5;
			out[n++] = '\'';
		} else if (!omit || i == len)
			out[n++] = message[i];
	}
	DBG("In stripping ssml: |%s|", out);

	return out;
}

void module_strip_punctuation_default(char *buf)
{
	assert(buf != NULL);
	module_strip_punctuation_some(buf, "~#$%^&*+=|<>[]_");
}

size_t
module_parent_wfork(TModuleDoublePipe dpipe, const char *message,
		    SPDMessageType msgtype, const size_t maxlen,
		    const char *dividers, int *pause_requested)
{
	unsigned int pos = 0;
	char msg[16];
	char *buf;
	int bytes;
	size_t read_bytes = 0;

	DBG("Entering parent process, closing pipes");

	buf = (char *)g_malloc((maxlen + 1) * sizeof(char));

	module_parent_dp_init(dpipe);

	pos = 0;
	while (1) {
		DBG("  Looping...\n");

		bytes =
		    module_get_message_part(message, buf, &pos, maxlen,
					    dividers);

		DBG("Returned %d bytes from get_part\n", bytes);

		if (*pause_requested) {
			DBG("Pause requested in parent");
			module_parent_dp_close(dpipe);
			*pause_requested = 0;
			return 0;
		}

		if (bytes > 0) {
			DBG("Sending buf to child:|%s| %d\n", buf, bytes);
			module_parent_dp_write(dpipe, buf, bytes);

			DBG("Waiting for response from child...\n");
			while (1) {
				read_bytes =
				    module_parent_dp_read(dpipe, msg, 8);
				if (read_bytes == 0) {
					DBG("parent: Read bytes 0, child stopped\n");
					break;
				}
				if (msg[0] == 'C') {
					DBG("Ok, received report to continue...\n");
					break;
				}
			}
		}

		if ((bytes == -1) || (read_bytes == 0)) {
			DBG("End of data in parent, closing pipes");
			module_parent_dp_close(dpipe);
			break;
		}

	}
	return 0;
}

int module_parent_wait_continue(TModuleDoublePipe dpipe)
{
	char msg[16];
	int bytes;

	DBG("parent: Waiting for response from child...\n");
	while (1) {
		bytes = module_parent_dp_read(dpipe, msg, 8);
		if (bytes == 0) {
			DBG("parent: Read bytes 0, child stopped\n");
			return 1;
		}
		if (msg[0] == 'C') {
			DBG("parent: Ok, received report to continue...\n");
			return 0;
		}
	}
}

void module_parent_dp_init(TModuleDoublePipe dpipe)
{
	close(dpipe.pc[0]);
	close(dpipe.cp[1]);
}

void module_parent_dp_close(TModuleDoublePipe dpipe)
{
	close(dpipe.pc[1]);
	close(dpipe.cp[0]);
}

void module_child_dp_init(TModuleDoublePipe dpipe)
{
	close(dpipe.pc[1]);
	close(dpipe.cp[0]);
}

void module_child_dp_close(TModuleDoublePipe dpipe)
{
	close(dpipe.pc[0]);
	close(dpipe.cp[1]);
}

void
module_child_dp_write(TModuleDoublePipe dpipe, const char *msg, size_t bytes)
{
	int ret;
	assert(msg != NULL);
	ret = write(dpipe.cp[1], msg, bytes);
	assert(ret);
}

int
module_parent_dp_write(TModuleDoublePipe dpipe, const char *msg, size_t bytes)
{
	ssize_t ret;
	assert(msg != NULL);
	DBG("going to write %lu bytes", (long unsigned)bytes);
	ret = write(dpipe.pc[1], msg, bytes);
	DBG("written %ld bytes", (long)ret);
	return ret;
}

int module_child_dp_read(TModuleDoublePipe dpipe, char *msg, size_t maxlen)
{
	int bytes;
	while ((bytes = read(dpipe.pc[0], msg, maxlen)) < 0) {
		if (errno != EINTR) {
			FATAL("Unable to read data");
		}
	}
	return bytes;
}

int module_parent_dp_read(TModuleDoublePipe dpipe, char *msg, size_t maxlen)
{
	int bytes;
	while ((bytes = read(dpipe.cp[0], msg, maxlen)) < 0) {
		if (errno != EINTR) {
			FATAL("Unable to read data");
		}
	}
	return bytes;
}

void module_sigblockall(void)
{
	int ret;
	sigset_t all_signals;

	DBG("Blocking all signals");

	sigfillset(&all_signals);

	ret = sigprocmask(SIG_BLOCK, &all_signals, NULL);
	if (ret != 0)
		DBG("Can't block signals, expect problems with terminating!\n");
}

void module_sigunblockusr(sigset_t * some_signals)
{
	int ret;

	DBG("UnBlocking user signal");

	sigdelset(some_signals, SIGUSR1);
	ret = sigprocmask(SIG_SETMASK, some_signals, NULL);
	if (ret != 0)
		DBG("Can't block signal set, expect problems with terminating!\n");
}

void module_sigblockusr(sigset_t * some_signals)
{
	int ret;

	DBG("Blocking user signal");

	sigaddset(some_signals, SIGUSR1);
	ret = sigprocmask(SIG_SETMASK, some_signals, NULL);
	if (ret != 0)
		DBG("Can't block signal set, expect problems when terminating!\n");

}

int module_terminate_thread(pthread_t thread)
{
	int ret;

	ret = pthread_cancel(thread);
	if (ret != 0) {
		DBG("Cancellation of speak thread failed");
		return 1;
	}
	ret = pthread_join(thread, NULL);
	if (ret != 0) {
		DBG("join failed!\n");
		return 1;
	}

	return 0;
}

char *module_recode_to_iso(const char *data, int bytes, const char *language,
			   const char *fallback)
{
	char *recoded;

	if (language == NULL)
		recoded = g_strdup(data);
	else if (!strcmp(language, "cs") || !strcmp(language, "cs-CZ"))
		recoded =
		    (char *)g_convert_with_fallback(data, bytes, "ISO8859-2",
						    "UTF-8", fallback, NULL,
						    NULL, NULL);
	else
		recoded =
		    (char *)g_convert_with_fallback(data, bytes, "ISO8859-1",
						    "UTF-8", fallback, NULL,
						    NULL, NULL);

	if (recoded == NULL)
		DBG("festival: Conversion to ISO coding failed\n");

	return recoded;
}

/* --- CONFIGURATION --- */
configoption_t *module_add_config_option(configoption_t * options,
					 int *num_options, const char *name, int type,
					 dotconf_callback_t callback,
					 info_t * info, unsigned long context)
{
	configoption_t *opts;
	int num_config_options = *num_options;

	assert(name != NULL);

	num_config_options++;
	opts =
	    (configoption_t *) g_realloc(options,
					 (num_config_options +
					  1) * sizeof(configoption_t));
	opts[num_config_options - 1].name = (char *)g_strdup(name);
	opts[num_config_options - 1].type = type;
	opts[num_config_options - 1].callback = callback;
	opts[num_config_options - 1].info = info;
	opts[num_config_options - 1].context = context;

	*num_options = num_config_options;
	return opts;
}

int module_audio_init(char **status_info)
{
	char *error = 0;
	gchar **outputs;
	int i = 0;

	DBG("Opening audio output system");
	if (NULL == module_audio_pars[0]) {
		*status_info =
		    g_strdup
		    ("Sound output method specified in configuration not supported. "
		     "Please choose 'oss', 'alsa', 'nas', 'libao' or 'pulse'.");
		return -1;
	}

	g_free(module_audio_pars[6]);
	module_audio_pars[6] = strdup(module_name);

	outputs = g_strsplit(module_audio_pars[0], ",", 0);
	while (NULL != outputs[i]) {
		module_audio_id =
		    spd_audio_open(outputs[i], (void **)&module_audio_pars[1],
				   &error);
		if (module_audio_id) {
			DBG("Using %s audio output method", outputs[i]);
			g_strfreev(outputs);

			/* Volume is controlled by the synthesizer. Always play at normal on audio device. */
			if (spd_audio_set_volume(module_audio_id, 85) < 0) {
				DBG("Can't set volume. audio not initialized?");
			}

			*status_info =
			    g_strdup("audio initialized successfully.");
			return 0;
		}
		i++;
	}

	*status_info =
	    g_strdup_printf("Opening sound device failed. Reason: %s. ", error);
	g_free(error);		/* g_malloc'ed, in spd_audio_open. */

	g_strfreev(outputs);
	return -1;

}

int module_tts_output(AudioTrack track, AudioFormat format)
{

	if (spd_audio_play(module_audio_id, track, format) < 0) {
		DBG("Can't play track for unknown reason.");
		return -1;
	}
	return 0;
}

int module_marks_init(SPDMarks *marks)
{
	marks->num = 0;
	marks->allocated = 0;
	marks->samples = NULL;
	marks->names = NULL;
	marks->stop = 0;

	return 0;
}

int module_marks_add(SPDMarks *marks, unsigned sample, const char *name)
{
	marks->num++;
	if (marks->num >= marks->allocated) {
		/* Amortized reallocation */
		marks->allocated = marks->num * 2;
		marks->samples = g_realloc(marks->samples, marks->allocated * sizeof(marks->samples[0]));
		marks->names = g_realloc(marks->names, marks->allocated * sizeof(marks->names[0]));
	}
	marks->samples[marks->num - 1] = sample;
	marks->names[marks->num - 1] = g_strdup(name);

	return 0;
}

int module_marks_clear(SPDMarks *marks)
{
	unsigned i;

	for (i = 0; i < marks->num; i++)
		g_free(marks->names[i]);

	marks->num = 0;
	marks->allocated = 0;
	g_free(marks->samples);
	marks->samples = NULL;
	g_free(marks->names);
	marks->names = NULL;
	marks->stop = 0;

	return 0;
}

/* Strip silence at head of audio track */
void module_strip_head_silence(AudioTrack * track)
{
	assert(track->bits == 16);
	unsigned i;
	float silence_limit = 0.01;

	while (track->num_samples >= track->num_channels) {
		for (i = 0; i < track->num_channels; i++)
			if (abs(track->samples[i])
			    >= silence_limit * (1L<<(track->bits-1)))
				return;
		track->samples += track->num_channels;
		track->num_samples -= track->num_channels;
	}
}
/* Strip silence at tail of audio track */
void module_strip_tail_silence(AudioTrack * track)
{
	assert(track->bits == 16);
	unsigned i;
	float silence_limit = 0.01;

	while (track->num_samples >= track->num_channels) {
		for (i = 0; i < track->num_channels; i++)
			if (abs(track->samples[track->num_samples - i - 1])
			    >= silence_limit * (1L<<(track->bits-1)))
				return;
		track->num_samples -= track->num_channels;
	}
}

void module_strip_silence(AudioTrack * track)
{
	module_strip_head_silence(track);
	module_strip_tail_silence(track);
}

int module_tts_output_marks(AudioTrack track, AudioFormat format, SPDMarks *marks)
{
	AudioTrack cur = track;
	int current_sample = 0;

	/* Loop control */
	int start = 0;
	int end = marks->num;
	int delta = 0; /* loop below figures out the delta */
	int i;
	for (i = 1; i < marks->num; i++) {
		if (marks->samples[i - 1] > marks->samples[i]) {
			/* Decreasing mark order */
			if (delta > 0) {
				DBG("WARNING: Mixed samples order");
			} else {
				start = marks->num - 1;
				end = -1;
				delta = -1;
			}
		} else if (marks->samples[i - 1] < marks->samples[i]) {
			/* Increasing mark order */
			if (delta < 0) {
				DBG("WARNING: Mixed samples order");
			} else {
				delta = 1;
			}
		}
	}
	if (delta == 0) {
		/* All marks are at the same sample */
		delta = 1;
	}

	/* Alternate speaking and reporting mark */
	for (i = start; i != end; i += delta) {
		unsigned end_sample = marks->samples[i];

		cur.samples = &track.samples[current_sample];
		cur.num_samples = end_sample - current_sample;
		current_sample = end_sample;

		if (cur.num_samples && module_tts_output(cur, format))
			return -1;
		if (marks->stop)
			return 1;
		module_report_index_mark(marks->names[i]);
	}

	/* Finish with remaining bits if any */
	if (track.num_samples > current_sample) {
		cur.samples = &track.samples[current_sample];
		cur.num_samples = track.num_samples - current_sample;
		if (module_tts_output(cur, format))
			return -1;
	}

	return 0;
}

int module_marks_stop(SPDMarks *marks)
{
	marks->stop = 1;
	return 0;
}
