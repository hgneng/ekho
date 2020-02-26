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

#include <sndfile.h>

#include <fdsetconv.h>
#include <spd_utils.h>
#include "module_utils.h"

static char *module_audio_pars[10];

extern char *module_index_mark;

pthread_mutex_t module_stdout_mutex = PTHREAD_MUTEX_INITIALIZER;

int log_level;

AudioID *module_audio_id;

SPDMsgSettings msg_settings;
SPDMsgSettings msg_settings_old;

int current_index_mark;

int Debug;
FILE *CustomDebugFile;

configfile_t *configfile;
configoption_t *module_dc_options;
int module_num_dc_options;

const char *module_name;

char *module_index_mark;

char *do_message(SPDMessageType msgtype)
{
	int ret;
	char *cur_line;
	GString *msg;
	size_t n;
	int nlines = 0;

	msg = g_string_new("");

	printf("202 OK RECEIVING MESSAGE\n");
	fflush(stdout);

	while (1) {
		cur_line = NULL;
		n = 0;
		ret = spd_getline(&cur_line, &n, stdin);
		nlines++;
		if (ret == -1)
			return g_strdup("401 ERROR INTERNAL");

		if (!strcmp(cur_line, "..\n")) {
			g_free(cur_line);
			cur_line = g_strdup(".\n");
		} else if (!strcmp(cur_line, ".\n")) {
			/* Strip the trailing \n */
			msg->str[strlen(msg->str) - 1] = 0;
			g_free(cur_line);
			break;
		}
		g_string_append(msg, cur_line);
		g_free(cur_line);
	}

	if ((msgtype != SPD_MSGTYPE_TEXT) && (nlines > 2)) {
		return g_strdup("305 DATA MORE THAN ONE LINE");
	}

	if ((msgtype == SPD_MSGTYPE_CHAR) && (!strcmp(msg->str, "space"))) {
		g_string_free(msg, 1);
		msg = g_string_new(" ");
	}

	/* no sure we need this check here at all */
	if (msg->str == NULL || msg->str[0] == 0) {
		DBG("requested data NULL or empty\n");
		g_string_free(msg, TRUE);
		return g_strdup("301 ERROR CANT SPEAK");
	}

	/* check voice and synthesis_voice settings for consistency */
	if (msg_settings.voice.name == NULL
	    && msg_settings_old.voice.name != NULL
	    && msg_settings.voice_type == msg_settings_old.voice_type) {
		/* force to set voice again, since synthesis_voice changed to NULL */
		msg_settings_old.voice_type = -1;
	}

	/* Volume is controlled by the synthesizer. Always play at normal on audio device. */
	if (spd_audio_set_volume(module_audio_id, 85) < 0) {
		DBG("Can't set volume. audio not initialized?");
	}

	ret = module_speak(msg->str, strlen(msg->str), msgtype);

	g_string_free(msg, 1);
	if (ret <= 0)
		return g_strdup("301 ERROR CANT SPEAK");

	return g_strdup("200 OK SPEAKING");
}

char *do_speak(void)
{
	return do_message(SPD_MSGTYPE_TEXT);
}

char *do_sound_icon(void)
{
	return do_message(SPD_MSGTYPE_SOUND_ICON);
}

char *do_char(void)
{
	return do_message(SPD_MSGTYPE_CHAR);
}

char *do_key(void)
{
	return do_message(SPD_MSGTYPE_KEY);
}

void do_stop(void)
{
	module_stop();
	return;
}

void do_pause(void)
{
	int ret;

	ret = module_pause();
	if (ret) {
		DBG("WARNING: Can't pause");
		return;
	}

	return;
}

#define SET_PARAM_NUM(name, cond) \
	if(!strcmp(cur_item, #name)){ \
		number = strtol(cur_value, &tptr, 10); \
		if(!(cond)){ err = 2; continue; } \
		if (tptr == cur_value){ err = 2; continue; } \
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
		ret = fconv(cur_value); \
		if (ret != -1) msg_settings.name = ret; \
		else err = 2; \
	}

char *do_set(void)
{
	char *cur_item = NULL;
	char *cur_value = NULL;
	char *line = NULL;
	int ret;
	size_t n;
	int number;
	char *tptr;
	int err = 0;		/* Error status */

	printf("203 OK RECEIVING SETTINGS\n");
	fflush(stdout);

	while (1) {
		line = NULL;
		n = 0;
		ret = spd_getline(&line, &n, stdin);
		if (ret == -1) {
			err = 1;
			break;
		}
		if (!strcmp(line, ".\n")) {
			g_free(line);
			break;
		}
		if (!err) {
			cur_item = strtok(line, "=");
			if (cur_item == NULL) {
				err = 1;
				continue;
			}
			cur_value = strtok(NULL, "\n");
			if (cur_value == NULL) {
				err = 1;
				continue;
			}

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
				ret = str2EVoice(cur_value);
				if (ret != -1)
					msg_settings.voice_type = ret;
				else
					err = 2;
			} else if (!strcmp(cur_item, "synthesis_voice")) {
				g_free(msg_settings.voice.name);
				if (!strcmp(cur_value, "NULL"))
					msg_settings.voice.name = NULL;
				else
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
				err = 2;	/* Unknown parameter */
		}
		g_free(line);
	}

	if (err == 0)
		return g_strdup("203 OK SETTINGS RECEIVED");
	if (err == 1)
		return g_strdup("302 ERROR BAD SYNTAX");
	if (err == 2)
		return g_strdup("303 ERROR INVALID PARAMETER OR VALUE");

	return g_strdup("401 ERROR INTERNAL");	/* Can't be reached */
}

#define SET_AUDIO_STR(name,idx) \
	if(!strcmp(cur_item, #name)){ \
		g_free(module_audio_pars[idx]); \
		if(!strcmp(cur_value, "NULL")) module_audio_pars[idx] = NULL; \
		else module_audio_pars[idx] = g_strdup(cur_value); \
	}

char *do_audio(void)
{
	char *cur_item = NULL;
	char *cur_value = NULL;
	char *line = NULL;
	int ret;
	size_t n;
	int err = 0;		/* Error status */
	char *status = NULL;
	char *msg;

	printf("207 OK RECEIVING AUDIO SETTINGS\n");
	fflush(stdout);

	while (1) {
		line = NULL;
		n = 0;
		ret = spd_getline(&line, &n, stdin);
		if (ret == -1) {
			err = 1;
			break;
		}
		if (!strcmp(line, ".\n")) {
			g_free(line);
			break;
		}
		if (!err) {
			cur_item = strtok(line, "=");
			if (cur_item == NULL) {
				err = 1;
				continue;
			}
			cur_value = strtok(NULL, "\n");
			if (cur_value == NULL) {
				err = 1;
				continue;
			}

			SET_AUDIO_STR(audio_output_method, 0)
			    else
				SET_AUDIO_STR(audio_oss_device, 1)
				    else
				SET_AUDIO_STR(audio_alsa_device, 2)
				    else
				SET_AUDIO_STR(audio_nas_server, 3)
				    else
				SET_AUDIO_STR(audio_pulse_server, 4)
				    else
				SET_AUDIO_STR(audio_pulse_min_length, 5)
				    else
				/* 6 reserved for speech-dispatcher module name */
				err = 2;	/* Unknown parameter */
		}
		g_free(line);
	}

	if (err == 1)
		return g_strdup("302 ERROR BAD SYNTAX");
	if (err == 2)
		return g_strdup("303 ERROR INVALID PARAMETER OR VALUE");

	err = module_audio_init(&status);

	if (err == 0)
		msg = g_strdup_printf("203 OK AUDIO INITIALIZED");
	else
		msg = g_strdup_printf("300-%s\n300 UNKNOWN ERROR", status);

	g_free(status);
	return msg;
}

#define SET_LOGLEVEL_NUM(name, cond) \
	if(!strcmp(cur_item, #name)){ \
		number = strtol(cur_value, &tptr, 10); \
		if(!(cond)){ err = 2; continue; } \
		if (tptr == cur_value){ err = 2; continue; } \
		log_level = number; \
		spd_audio_set_loglevel(module_audio_id, number); \
	}

char *do_loglevel(void)
{
	char *cur_item = NULL;
	char *cur_value = NULL;
	char *line = NULL;
	int ret;
	size_t n;
	int number;
	char *tptr;
	int err = 0;		/* Error status */
	char *msg;

	printf("207 OK RECEIVING LOGLEVEL SETTINGS\n");
	fflush(stdout);

	while (1) {
		line = NULL;
		n = 0;
		ret = spd_getline(&line, &n, stdin);
		if (ret == -1) {
			err = 1;
			break;
		}
		if (!strcmp(line, ".\n")) {
			g_free(line);
			break;
		}
		if (!err) {
			cur_item = strtok(line, "=");
			if (cur_item == NULL) {
				err = 1;
				continue;
			}
			cur_value = strtok(NULL, "\n");
			if (cur_value == NULL) {
				err = 1;
				continue;
			}

			SET_LOGLEVEL_NUM(log_level, 1)
			    else
				err = 2;	/* Unknown parameter */
		}
		g_free(line);
	}

	if (err == 1)
		return g_strdup("302 ERROR BAD SYNTAX");
	if (err == 2)
		return g_strdup("303 ERROR INVALID PARAMETER OR VALUE");

	msg = g_strdup_printf("203 OK LOG LEVEL SET");

	return msg;
}

char *do_debug(char *cmd_buf)
{
	/* TODO: Develop the full on/off logic etc. */

	char **cmd;
	char *filename;

	cmd = g_strsplit(cmd_buf, " ", -1);

	if (!cmd[1]) {
		g_strfreev(cmd);
		return g_strdup("302 ERROR BAD SYNTAX");
	}

	if (!strcmp(cmd[1], "ON")) {
		if (!cmd[2]) {
			g_strfreev(cmd);
			return g_strdup("302 ERROR BAD SYNTAX");
		}

		filename = cmd[2];
		DBG("Additional logging into specific path %s requested",
		    filename);
		FILE *new_CustomDebugFile = fopen(filename, "w+");
		if (new_CustomDebugFile == NULL) {
			DBG("ERROR: Can't open custom debug file for logging: %d (%s)", errno, strerror(errno));
			return g_strdup("303 CANT OPEN CUSTOM DEBUG FILE");
		}
		if (CustomDebugFile != NULL)
			fclose(CustomDebugFile);
		CustomDebugFile = new_CustomDebugFile;
		if (Debug == 1)
			Debug = 3;
		else
			Debug = 2;

		DBG("Additional logging initialized");
	} else if (!strcmp(cmd[1], "OFF")) {
		if (Debug == 3)
			Debug = 1;
		else
			Debug = 0;

		if (CustomDebugFile != NULL)
			fclose(CustomDebugFile);
		CustomDebugFile = NULL;
		DBG("Additional logging into specific path terminated");
	} else {
		return g_strdup("302 ERROR BAD SYNTAX");
	}

	g_strfreev(cmd);
	return g_strdup("200 OK DEBUGGING ON");
}

char *do_list_voices(void)
{
	SPDVoice **voices;
	int i;
	char *lang, *variant;
	GString *voice_list;

	voices = module_list_voices();
	if (voices == NULL) {
		return g_strdup("304 CANT LIST VOICES");
	}

	voice_list = g_string_new("");
	for (i = 0; voices[i] != NULL; i++) {
		if (voices[i]->name == NULL) {	/* Shouldn't happen! */
			DBG("Unnamed voice found; ignoring it.");
			continue;
		}
		if (voices[i]->language == NULL)
			lang = "none";
		else
			lang = voices[i]->language;
		if (voices[i]->variant == NULL)
			variant = "none";
		else
			variant = voices[i]->variant;
		g_string_append_printf(voice_list, "200-%s\t%s\t%s\n",
				       voices[i]->name, lang, variant);
	}

	/* check whether we found at least one voice */
	if (voice_list->len == 0) {
		g_string_free(voice_list, TRUE);
		return g_strdup("304 CANT LIST VOICES");
	}

	g_string_append(voice_list, "200 OK VOICE LIST SENT");

	DBG("Voice prepared to  send to speechd");

	return g_string_free(voice_list, FALSE);
}

#undef SET_PARAM_NUM
#undef SET_PARAM_STR

/* This has to return int (although it doesn't return at all) so that we could
 * call it from PROCESS_CMD() macro like the other commands that return
 * something */
void do_quit(void)
{
	printf("210 OK QUIT\n");
	fflush(stdout);

	module_close();

	spd_audio_close(module_audio_id);
	module_audio_id = NULL;
	return;
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
					if ((part[i] == dividers[n])) {
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

char *module_strip_ssml(char *message)
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

void set_speaking_thread_parameters()
{
	int ret;
	sigset_t all_signals;

	ret = sigfillset(&all_signals);
	if (ret == 0) {
		ret = pthread_sigmask(SIG_BLOCK, &all_signals, NULL);
		if (ret != 0)
			DBG("Can't set signal set, expect problems when terminating!\n");
	} else {
		DBG("Can't fill signal set, expect problems when terminating!\n");
	}

	pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
	pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
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

char *module_recode_to_iso(char *data, int bytes, char *language,
			   char *fallback)
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

void module_send_asynchronous(char *text)
{
	pthread_mutex_lock(&module_stdout_mutex);
	DBG("Printing reply: %s", text);
	fputs(text, stdout);
	fflush(stdout);
	DBG("Printed");
	pthread_mutex_unlock(&module_stdout_mutex);
}

void module_report_index_mark(char *mark)
{
	char *reply;
	DBG("Event: Index mark %s", mark);
	if (mark != NULL)
		reply = g_strdup_printf("700-%s\n700 INDEX MARK\n", mark);
	else
		return;

	module_send_asynchronous(reply);

	g_free(reply);
}

void module_report_event_begin(void)
{
	module_send_asynchronous("701 BEGIN\n");
}

void module_report_event_end(void)
{
	module_send_asynchronous("702 END\n");
}

void module_report_event_stop(void)
{
	module_send_asynchronous("703 STOP\n");
}

void module_report_event_pause(void)
{
	module_send_asynchronous("704 PAUSE\n");
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

/* Plays the specified audio file. */
int module_play_file(const char *filename)
{
	int result = 0;
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
		int ret = module_tts_output(track, SPD_AUDIO_LE);
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
