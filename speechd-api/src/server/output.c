/*
 * output.c - Output layer for Speech Dispatcher
 *
 * Copyright (C) 2001, 2002, 2003, 2007 Brailcom, o.p.s.
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
 * $Id: output.c,v 1.38 2008-06-27 12:28:48 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <fdsetconv.h>
#include <safe_io.h>
#include "output.h"
#include "parse.h"
#include "speak_queue.h"
#include "index_marking.h"

#ifndef HAVE_STRNDUP
/*
 * Added by Willie Walker - strndup was a GNU libc extension, later adopted
 * in the POSIX.1-2008 standard, but not yet found on all systems.
 */
char *strndup(const char *s, size_t n)
{
	size_t nAvail;
	char *p;

	if (!s)
		return 0;

	if (strlen(s) > n)
		nAvail = n + 1;
	else
		nAvail = strlen(s) + 1;
	p = g_malloc(nAvail);
	memcpy(p, s, nAvail);
	p[nAvail - 1] = '\0';

	return p;
}
#endif /* HAVE_STRNDUP */

static pthread_t output_thread;
static void *output_thread_func(void *data);
static int output_end_queued;
static int output_stop_requested;
static int output_pause_requested;
static int output_pause_queued;

static void output_open_audio(OutputModule *output)
{
	void *pars[9] = { NULL };
	char min_length[11];
	char *error;
	gchar **outputs;
	int i;

	pars[0] = GlobalFDSet.audio_oss_device;
	pars[1] = GlobalFDSet.audio_alsa_device;
	pars[2] = GlobalFDSet.audio_nas_server;
	pars[3] = GlobalFDSet.audio_pulse_device;
	snprintf(min_length, sizeof(min_length), "%u", GlobalFDSet.audio_pulse_min_length);
	pars[4] = min_length;
	pars[5] = output->name;

	outputs = g_strsplit(GlobalFDSet.audio_output_method, ",", 0);
	for (i = 0; NULL != outputs[i]; i++) {
		output->audio =
		    spd_audio_open(outputs[i], pars, &error);
		if (output->audio) {
			DBG("Using %s audio output method", outputs[i]);
			g_strfreev(outputs);

			/* Volume is controlled by the synthesizer. Always play at normal on audio device. */
			if (spd_audio_set_volume(output->audio, 85) < 0) {
				DBG("Can't set volume. audio not initialized?");
			}

			return;
		}
	}

	MSG(1, "Opening audio failed: %s\n", error);
	g_free(error);
	g_strfreev(outputs);
}

void output_set_speaking_monitor(TSpeechDMessage * msg, OutputModule * output)
{
	/* Set the speaking-monitor so that we know who is speaking */
	speaking_module = output;
	if (output->audio) {
		if (output->audio == AUDIOID_TOOPEN)
			output_open_audio(output);
		module_audio_id = output->audio;
	}
	speaking_uid = msg->settings.uid;
	speaking_gid = msg->settings.reparted;
}

OutputModule *get_output_module_by_name(const char *name)
{
	OutputModule *output;
	int i;

	for (i = 0; i < g_list_length(output_modules); i++) {
		output = g_list_nth_data(output_modules, i);
		if (!strcmp(output->name, name)) {
			if (output->working)
				return output;
			else
				return NULL;
		}
	}

	return NULL;
}

/* get_output_module tries to return a pointer to the
   appropriate output module according to message context.
   If it is not possible to find the required module,
   it will subsequently try to get the default module,
   any of the other remaining modules except dummy and
   at last, the dummy output module.

   Only if not even dummy output module is working
   (serious issues), it will log an error message and return
   a NULL pointer.

*/

OutputModule *get_output_module(const TSpeechDMessage * message)
{
	OutputModule *output = NULL;
	int i, len;

	if (message->settings.output_module != NULL) {
		MSG(5, "Desired output module is %s",
		    message->settings.output_module);
		output =
		    get_output_module_by_name(message->settings.output_module);
		if ((output != NULL) && output->working)
			return output;
	}

	MSG(3, "Warning: Didn't find preferred output module, using default");
	// If the requested module was not found or is not working,
	// first try to use the default output module
	if (GlobalFDSet.output_module != NULL)
		output = get_output_module_by_name(GlobalFDSet.output_module);

	if (output != NULL && output->working)
		return output;

	MSG(3, "Couldn't load default output module, trying other modules");

	/* Try all other output modules other than dummy */
	len = g_list_length(output_modules);
	for (i = 0; i < len; i++) {
		output = g_list_nth_data(output_modules, i);
		if (0 == strcmp(output->name, "dummy"))
			continue;

		if (output->working) {
			MSG(3, "Output module %s seems to be working, using it",
			    output->name);
			return output;
		}
	}

	// if we get here there are no good modules use the dummy
	// a pre-synthesized error message with some hints over and over).
	if (output == NULL || !output->working)
		output = get_output_module_by_name("dummy");

	// Give up....
	if (output == NULL)
		MSG(1,
		    "Error: No output module working, not even dummy, no sound produced!\n");

	return output;
}

/*
 * Note: commands are send to the output modules both from the clients and from
 * the speaking thread. They both use output_lock/unlock around sending a
 * command and getting a reply, to avoid getting the reply for each other.
 *
 * During speech, the output module sends asynchronous events (marks,
 * audio). This is handled along the way. We however need them to be handled
 * also when there is no client or speaking command. We thus also start a thread
 * that merely processes them.
 */

static int oldstate;
void
static output_lock(void)
{
	if (pthread_self() == speak_thread)
		pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &oldstate);
	pthread_mutex_lock(&output_layer_mutex);
}

void
static output_unlock(void)
{
	pthread_mutex_unlock(&output_layer_mutex);
	if (pthread_self() == speak_thread)
		pthread_setcancelstate(oldstate, NULL);
}

#define OL_RET(value) \
	{  output_unlock(); \
		return (value); }

GString *output_read_message(OutputModule * output)
{
	GString *rstr;
	int bytes;
	char *line = NULL;
	size_t N = 0;
	gboolean errors = FALSE;

	rstr = g_string_new("");

	/* Wait for activity on the socket, when there is some,
	   read all the message line by line */
	do {
		bytes = getline(&line, &N, output->stream_out);
		if (bytes == -1) {
			MSG(2, "Error: Broken pipe to module.");
			output->working = 0;
			output_check_module(output);
			errors = TRUE;	/* Broken pipe */
		} else {
			MSG(5, "Got %d bytes from output module over socket",
			    bytes);
			g_string_append_len(rstr, line, bytes);
		}
		/* terminate if we reached the last line (without '-' after numcode) */
	} while (!errors && !((strlen(line) < 4) || (line[3] == ' ')));

	if (line != NULL)
		free(line);

	if (errors) {
		g_string_free(rstr, TRUE);
		rstr = NULL;
	}

	return rstr;
}

/*
 * Read a message.
 * If read_events is 1, we only return event messages
 * If read_events is 0, we only return non-event messages
 */
static pthread_mutex_t output_read_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t output_reply_cond = PTHREAD_COND_INITIALIZER;
static pthread_cond_t output_event_cond = PTHREAD_COND_INITIALIZER;
static GString *output_reply;
static GString *output_event;
static int output_reading_message;

GString *output_read_reply(OutputModule * output)
{
	GString *message;
	pthread_mutex_lock(&output_read_mutex);
	while (!message) {
		while (output_reading_message && !output_reply)
			/* Somebody reading events, wait for it */
			pthread_cond_wait(&output_reply_cond, &output_read_mutex);

		if (output_reply) {
			/* The other thread got a message for us, consume it */
			message = output_reply;
			output_reply = NULL;
			/* And tell the other thread we got it */
			pthread_cond_signal(&output_event_cond);
			break;
		}

		if (!output_reading_message) {
			/* Nobody reading, do read */
			message = output_read_message(output);
			if (!message)
				/* Module broke */
				break;
			if (message->str[0] == '7') {
				/* An event, leave it up to the event thread */
				output_event = message;
				message = NULL;
				/* Wait for it to consume it */
				while (output_event)
					pthread_cond_wait(&output_reply_cond, &output_read_mutex);
			}
		}
	}
	pthread_mutex_unlock(&output_read_mutex);
	return message;
}

GString *output_read_event(OutputModule * output)
{
	GString *message;
	pthread_mutex_lock(&output_read_mutex);
	while (!message) {
		while (output_reading_message && !output_event)
			/* Somebody reading replies, wait for it */
			pthread_cond_wait(&output_event_cond, &output_read_mutex);

		if (output_event) {
			/* The other thread got a message for us, consume it */
			message = output_event;
			output_event = NULL;
			/* And tell the other thread we got it */
			pthread_cond_signal(&output_reply_cond);
			break;
		}

		if (!output_reading_message) {
			/* Nobody reading, do read */
			message = output_read_message(output);
			if (!message)
				/* Module broke */
				break;
			if (message->str[0] != '7') {
				/* A reply, leave it up to the reply thread */
				output_reply = message;
				message = NULL;
				/* Wait for it to consume it */
				while (output_reply)
					pthread_cond_wait(&output_event_cond, &output_read_mutex);
			}
		}
	}
	pthread_mutex_unlock(&output_read_mutex);
	return message;
}

int output_send_data(const char *cmd, OutputModule * output, int wfr)
{
	int ret;
	GString *response;

	if (output == NULL)
		return -1;
	if (cmd == NULL)
		return -1;

	ret = safe_write(output->pipe_in[1], cmd, strlen(cmd));
	fflush(NULL);
	if (ret == -1) {
		MSG(2, "Error: Broken pipe to module.");
		output->working = 0;
		speaking_module = NULL;
		output_check_module(output);
		return -1;	/* Broken pipe */
	}
	MSG2(5, "output_module", "Command sent to output module: |%s| (%d)",
	     cmd, wfr);

	if (wfr) {		/* wait for reply? */
		int ret = 0;
		response = output_read_reply(output);
		if (response == NULL)
			return -1;

		MSG2(5, "output_module", "Reply from output module: |%s|",
		     response->str);

		switch (response->str[0]) {
		case '3':
			MSG(2,
			    "Error: Module reported error in request from speechd (code 3xx): %s.",
			    response->str);
			ret = -2;	/* User (speechd) side error */
			break;

		case '4':
			MSG(2,
			    "Error: Module reported error in itself (code 4xx): %s",
			    response->str);
			ret = -3;	/* Module side error */
			break;

		case '2':
			ret = 0;
			break;
		default:	/* unknown response */
			MSG(3, "Unknown response from output module!");
			ret = -3;
			break;
		}
		g_string_free(response, TRUE);
		return ret;
	}

	return 0;
}

static void free_voice(gpointer data)
{
	SPDVoice *voice = (SPDVoice *)data;

	if (voice != NULL) {
		if (voice->name != NULL)
			g_free(voice->name);
		if (voice->language != NULL)
			g_free(voice->language);
		if (voice->variant != NULL)
			g_free(voice->variant);

		g_free(voice);
	}
}

static SPDVoice **output_get_voices(OutputModule * module)
{
	SPDVoice **voice_dscr;
	SPDVoice *voice;
	GString *reply;
	gchar **lines;
	gchar **atoms;
	GQueue *voices;
	int i;
	int numvoices = 0;
	gboolean errors = FALSE;
	int err;

	output_lock();

	if (module == NULL) {
		MSG(1, "ERROR: Can't list voices for broken output module");
		OL_RET(NULL);
	}
	err = output_send_data("LIST VOICES\n", module, 0);
	if (err < 0) {
		output_unlock();
		return NULL;
	}
	reply = output_read_reply(module);

	if (reply == NULL) {
		output_unlock();
		return NULL;
	}

	lines = g_strsplit(reply->str, "\n", -1);
	g_string_free(reply, TRUE);
	voices = g_queue_new();
	for (i = 0; !errors && (lines[i] != NULL); i++) {
		MSG(1, "LINE here:|%s|", lines[i]);
		if (strlen(lines[i]) <= 4) {
			MSG(1,
			    "ERROR: Bad communication from driver in synth_voices");
			errors = TRUE;
		} else if (lines[i][3] == ' ')
			break;
		else if (lines[i][3] == '-') {
			atoms = g_strsplit(&lines[i][4], "\t", 0);
			// Name, language, variant
			if ((atoms[0] == NULL) || (atoms[1] == NULL)
			    || (atoms[2] == NULL)) {
				errors = TRUE;
			} else {
				//Fill in VoiceDescription
				voice = g_malloc(sizeof(SPDVoice));
				voice->name = g_strdup(atoms[0]);
				voice->language = g_strdup(atoms[1]);
				voice->variant = g_strdup(atoms[2]);
				g_queue_push_tail(voices, voice);
			}
			g_strfreev(atoms);
		}
		/* Should we do something in a final "else" branch? */

	}

	numvoices = g_queue_get_length(voices);

	if (errors == TRUE) {
		g_queue_free_full(voices, (GDestroyNotify)free_voice);
		g_strfreev(lines);
		output_unlock();
		return NULL;
	}

	voice_dscr = g_malloc((numvoices + 1) * sizeof(SPDVoice *));

	for (i = 0; i < numvoices; i++) {
		voice_dscr[i] = g_queue_pop_head(voices);
	}

	voice_dscr[i] = NULL;
	g_queue_free(voices);
	g_strfreev(lines);

	output_unlock();
	return voice_dscr;
}

SPDVoice **output_list_voices(const char *module_name)
{
	OutputModule *module;
	if (module_name == NULL)
		return NULL;
	module = get_output_module_by_name(module_name);
	if (module == NULL) {
		MSG(1, "ERROR: Can't list voices for module %s", module_name);
		return NULL;
	}
	return output_get_voices(module);
}

#define SEND_CMD_N(cmd) \
	{  err = output_send_data(cmd"\n", output, 1); \
		if (err < 0) return (err); }

#define SEND_CMD(cmd) \
	{  err = output_send_data(cmd"\n", output, 1); \
		if (err < 0) OL_RET(err)}

#define SEND_DATA_N(data) \
	{  err = output_send_data(data, output, 0); \
		if (err < 0) return (err); }

#define SEND_DATA(data) \
	{  err = output_send_data(data, output, 0); \
		if (err < 0) OL_RET(err); }

#define SEND_CMD_GET_VALUE(data) \
	{  err = output_send_data(data"\n", output, 1); \
		OL_RET(err); }

#define ADD_SET_INT(name) \
	g_string_append_printf(set_str, #name"=%d\n", msg->settings.name);
#define ADD_SET_STR(name) \
	if (msg->settings.name != NULL && msg->settings.name[0] != '\0') { \
		g_string_append_printf(set_str, #name"=%s\n", msg->settings.name); \
	}else{ \
		g_string_append_printf(set_str, #name"=NULL\n"); \
	}
#define ADD_SET_STR_C(name, fconv) \
	val = fconv(msg->settings.msg_settings.name); \
	if (val != NULL && val[0] != '\0'){ \
		g_string_append_printf(set_str, #name"=%s\n", val); \
	} \
	g_free(val);

int output_send_settings(TSpeechDMessage * msg, OutputModule * output)
{
	GString *set_str;
	char *val;
	int err;

	MSG(4, "Module set parameters.");
	set_str = g_string_new("");
	g_string_append_printf(set_str, "pitch=%d\n",
			       msg->settings.msg_settings.pitch);
	g_string_append_printf(set_str, "pitch_range=%d\n",
			       msg->settings.msg_settings.pitch_range);
	g_string_append_printf(set_str, "rate=%d\n",
			       msg->settings.msg_settings.rate);
	g_string_append_printf(set_str, "volume=%d\n",
			       msg->settings.msg_settings.volume);
	ADD_SET_STR_C(punctuation_mode, EPunctMode2str);
	ADD_SET_STR_C(spelling_mode, ESpellMode2str);
	ADD_SET_STR_C(cap_let_recogn, ECapLetRecogn2str);
	val = EVoice2str(msg->settings.msg_settings.voice_type);
	if (val != NULL && val[0] != '\0') {
		g_string_append_printf(set_str, "voice=%s\n", val);
	}
	g_free(val);
	if (msg->settings.msg_settings.voice.language != NULL
		&& msg->settings.msg_settings.voice.language[0] != '\0') {
		g_string_append_printf(set_str, "language=%s\n",
				       msg->settings.msg_settings.voice.
				       language);
	} else {
		g_string_append_printf(set_str, "language=NULL\n");
	}
	if (msg->settings.msg_settings.voice.name != NULL
		&& msg->settings.msg_settings.voice.name[0] != '\0') {
		g_string_append_printf(set_str, "synthesis_voice=%s\n",
				       msg->settings.msg_settings.voice.name);
	} else {
		g_string_append_printf(set_str, "synthesis_voice=NULL\n");
	}

	SEND_CMD_N("SET");
	SEND_DATA_N(set_str->str);
	SEND_CMD_N(".");

	g_string_free(set_str, 1);

	return 0;
}

#undef ADD_SET_INT
#undef ADD_SET_STR

#define ADD_SET_INT(name) \
	g_string_append_printf(set_str, #name"=%d\n", GlobalFDSet.name);
#define ADD_SET_STR(name) \
	if (GlobalFDSet.name != NULL){ \
		g_string_append_printf(set_str, #name"=%s\n", GlobalFDSet.name); \
	}else{ \
		g_string_append_printf(set_str, #name"=NULL\n"); \
	}

static int output_server_audio(OutputModule * output)
{
	GString *set_str;
	int err;

	MSG(4, "Module set parameters.");
	set_str = g_string_new("");
	g_string_append_printf(set_str, "audio_output_method=server\n");

	SEND_CMD_N("AUDIO");
	SEND_DATA_N(set_str->str);
	SEND_CMD_N(".");

	g_string_free(set_str, 1);

	output->audio = AUDIOID_TOOPEN;

	MSG(3, "Initialized for server audio for %s\n", output->name);
	return 0;

}

int output_send_audio_settings(OutputModule * output)
{
	GString *set_str;
	int err;

	/* First try to get output through server */
	if (output_server_audio(output) == 0)
		/* Went fine, good! */
		return 0;

	output->audio = NULL;
	MSG(4, "Module set parameters.");
	set_str = g_string_new("");
	ADD_SET_STR(audio_output_method);
	ADD_SET_STR(audio_oss_device);
	ADD_SET_STR(audio_alsa_device);
	ADD_SET_STR(audio_nas_server);
	// TODO: restore AudioPulseServer option
	//ADD_SET_STR(audio_pulse_server);
	ADD_SET_STR(audio_pulse_device);
	ADD_SET_INT(audio_pulse_min_length);

	SEND_CMD_N("AUDIO");
	SEND_DATA_N(set_str->str);
	SEND_CMD_N(".");

	g_string_free(set_str, 1);

	return 0;
}

int output_send_loglevel_setting(OutputModule * output)
{
	GString *set_str;
	int err;

	MSG(4, "Module set parameters.");
	set_str = g_string_new("");
	ADD_SET_INT(log_level);

	SEND_CMD_N("LOGLEVEL");
	SEND_DATA_N(set_str->str);
	SEND_CMD_N(".");

	g_string_free(set_str, 1);

	return 0;
}

#undef ADD_SET_INT
#undef ADD_SET_STR

int output_send_debug(OutputModule * output, int flag, const char *log_path)
{
	char *cmd_str;
	int err;

	MSG(4, "Module sending debug flag %d with file %s", flag, log_path);

	output_lock();
	if (flag) {
		cmd_str = g_strdup_printf("DEBUG ON %s \n", log_path);
		err = output_send_data(cmd_str, output, 1);
		g_free(cmd_str);
		if (err) {
			MSG(3,
			    "ERROR: Can't set debugging on for output module %s",
			    output->name);
			OL_RET(-1);
		}
	} else {
		err = output_send_data("DEBUG OFF \n", output, 1);
		if (err) {
			MSG(3,
			    "ERROR: Can't switch debugging off for output module %s",
			    output->name);
			OL_RET(-1);
		}

	}

	OL_RET(0);
}

int output_speak(TSpeechDMessage * msg, OutputModule *output)
{
	int err;
	int ret;
	char *newbuf;

	if (msg == NULL)
		return -1;

	output_lock();

	newbuf = escape_dot(msg->buf);
	if (newbuf != msg->buf) {
		g_free(msg->buf);
		msg->buf = newbuf;
	}
	msg->bytes = -1;

	output_set_speaking_monitor(msg, output);

	if (module_audio_id) {
		if (!module_speak_queue_before_synth()) {
			MSG(3, "Warning: couldn't begin speak queue");
		}
	}

	ret = output_send_settings(msg, output);
	if (ret != 0)
		OL_RET(ret);

	MSG(4, "Module speak!");

	switch (msg->settings.type) {
	case SPD_MSGTYPE_TEXT:
		SEND_CMD("SPEAK") break;
	case SPD_MSGTYPE_SOUND_ICON:
		SEND_CMD("SOUND_ICON");
		break;
	case SPD_MSGTYPE_CHAR:
		SEND_CMD("CHAR");
		break;
	case SPD_MSGTYPE_KEY:
		SEND_CMD("KEY");
		break;
	default:
		MSG(2, "Invalid message type in output_speak()!");
	}

	SEND_DATA(msg->buf)
	    SEND_CMD("\n.")

	/* Start a thread that will process the module events */
	output_end_queued = 0;
	output_stop_requested = 0;
	output_pause_requested = 0;
	output_pause_queued = 0;
	spd_pthread_create(&output_thread, NULL, output_thread_func, output);

	output_unlock();

	return 0;
}

int output_stop()
{
	int err;
	OutputModule *output;

	output_lock();

	if (speaking_module == NULL)
		OL_RET(0)
		    else
		output = speaking_module;

	if (output->audio)
	{
		if (output_end_queued) {
			MSG(4, "module is already done, stop speak_queue directly");
			module_speak_queue_stop();
			OL_RET(0)
		}
		MSG(4, "stopping speak_queue");
		output_stop_requested = 1;
		module_speak_queue_flush();
	}

	MSG(4, "Module stop!");
	SEND_DATA("STOP\n");

	OL_RET(0)
}

size_t output_pause()
{
	static int err;
	static OutputModule *output;

	output_lock();

	if (speaking_module == NULL)
		OL_RET(0)
		    else
		output = speaking_module;

	if (output->audio)
	{
		if (output_end_queued) {
			MSG(4, "module is already done, pause speak_queue directly");
			module_speak_queue_pause();
			OL_RET(0)
		}
		MSG(4, "pausing speak_queue");
		output_pause_requested = 1;
	}

	MSG(4, "Module pause!");
	SEND_DATA("PAUSE\n");

	OL_RET(0)
}

static GSList *playback_events = NULL;
static pthread_mutex_t playback_events_mutex = PTHREAD_MUTEX_INITIALIZER;

static speak_queue_entry *output_new_event(speak_queue_entry_type type)
{
	speak_queue_entry *entry = g_new(speak_queue_entry, 1);
	entry->type = type;
	return entry;
}

static void output_queue_event(speak_queue_entry *entry)
{
	char c = 0;
	int ret;
	pthread_mutex_lock(&playback_events_mutex);
	playback_events = g_slist_append(playback_events, entry);
	pthread_mutex_unlock(&playback_events_mutex);
	ret = write(speaking_module->pipe_speak[1], &c, 1);
	if (ret != 1)
		MSG(1, "Warning: couln't write to pipe_speak: %d returned, (errno = %d, %s)\n", ret, errno, strerror(errno));
}

static void output_queue_new_event(speak_queue_entry_type type)
{
	speak_queue_entry *entry = output_new_event(type);
	output_queue_event(entry);
}

void module_report_index_mark(const char *mark)
{
	speak_queue_entry *entry = output_new_event(SPEAK_QUEUE_QET_INDEX_MARK);
	entry->data.markId = g_strdup(mark);
	output_queue_event(entry);
}
void module_report_event_begin(void)
{
	output_queue_new_event(SPEAK_QUEUE_QET_BEGIN);
}
void module_report_event_end(void)
{
	output_queue_new_event(SPEAK_QUEUE_QET_END);
}
void module_report_event_broken(void)
{
	output_queue_new_event(SPEAK_QUEUE_QET_BROKEN);
}
void module_report_event_stop(void)
{
	output_queue_new_event(SPEAK_QUEUE_QET_STOP);
}
void module_report_event_pause(void)
{
	output_queue_new_event(SPEAK_QUEUE_QET_PAUSE);
}
void module_speak_queue_cancel(void)
{
	/* Not needed */
}

static int output_module_is_speaking(OutputModule * output)
{
	GString *response;
	int retcode = -1;

	MSG(5, "output_module_is_speaking()");

	if (output == NULL) {
		MSG(5, "output==NULL in output_module_is_speaking()");
		module_report_event_broken();
		return -1;
	}

	response = output_read_event(output);
	if (response == NULL) {
		module_report_event_broken();
		return -1;
	}

	MSG2(5, "output_module", "Event from output module while speaking: |%s|",
	     response->str);

	if (response->len < 4) {
		MSG2(2, "output_module",
		     "Error: Wrong communication from output module! Event less than four bytes.");
		g_string_free(response, TRUE);
		module_report_event_broken();
		return -1;
	}

	retcode = 1;
	MSG2(5, "output_module", "Received event:\n %s", response->str);
	if (!strncmp(response->str, "701", 3))
	{
		MSG2(5, "output_module", "got begin");
		if (output->audio) {
			if (!module_speak_queue_before_play())
				MSG(3, "Warning: couldn't add begin to speak queue");
		} else {
			module_report_event_begin();
		}
	}
	else if (!strncmp(response->str, "702", 3))
	{
		MSG2(5, "output_module", "got end");
		if (output->audio) {
			if (output_stop_requested) {
				MSG(4, "we sent STOP too late, now tell the speak queue");
				module_speak_queue_stop();
			} else if (output_pause_requested) {
				MSG(4, "we sent PAUSE too late, now tell the speak queue");
				if (!output_pause_queued)
					module_speak_queue_pause();
			} else {
				if (!module_speak_queue_add_end())
					MSG(3, "Warning: couldn't add end to speak queue");
				/* module is done, if stop is requested we'll have to
				 * tell speak_queue directly */
				output_end_queued = 1;
			}
		} else {
			module_report_event_end();
		}
		retcode = 0;
	}
	else if (!strncmp(response->str, "703", 3))
	{
		MSG2(5, "output_module", "got stopped");
		if (output->audio) {
			if (!output_pause_queued)
				module_speak_queue_stop();
		}
		else
			module_report_event_stop();
		retcode = 0;
	}
	else if (!strncmp(response->str, "704", 3))
	{
		MSG2(5, "output_module", "got paused");
		if (output->audio) {
			if (!output_pause_queued)
				module_speak_queue_pause();
		} else
			module_report_event_pause();
		retcode = 0;
	}
	else if (!strncmp(response->str, "700", 3))
	{
		char *p, *index_mark;
		p = strchr(response->str, '\n');
		MSG2(5, "output_module", "response:|%s|\n p:|%s|",
		     response->str, p);
		index_mark =
		    (char *)strndup(response->str + 4,
				    p - response->str - 4);
		MSG2(5, "output_module", "Detected INDEX MARK: %s",
		     index_mark);
		if (output->audio) {
			if (!(output_stop_requested || (output_pause_requested && output_pause_queued))) {
				if (!module_speak_queue_add_mark(index_mark))
					MSG(3, "Warning: couldn't add mark to speak queue");
				if (output_pause_requested &&
					!strncmp(index_mark, SD_MARK_BODY, SD_MARK_BODY_LEN)) {
					MSG(5, "Pausing the queue at mark %s", index_mark);
					module_speak_queue_pause();
					output_pause_queued = 1;
				}
			}
		} else {
			module_report_index_mark(index_mark);
		}
		free(index_mark);
	}
	else if (!strncmp(response->str, "706", 3))
	{
		char *p, *icon;
		p = strchr(response->str, '\n');
		MSG2(5, "output_module", "response:|%s|\n p:|%s|",
		     response->str, p);
		icon =
		    (char *)strndup(response->str + 4,
				    p - response->str - 4);
		MSG2(5, "output_module", "Detected sound icon: %s",
		     icon);
		if (output->audio &&
			!(output_stop_requested || (output_pause_requested && output_pause_queued))) {
			if (!module_speak_queue_add_sound_icon(icon))
				MSG(3, "Warning: couldn't add icon to speak queue");
		}
		free(icon);
	}
	else if (!strncmp(response->str, "705", 3))
	{
		AudioTrack track = { 0 };
		AudioFormat format = 0;
		char *p = response->str, *q;
		char *end = response->str + response->len;
		size_t size, filled;

		MSG2(5, "output_module",
			"Got audio: %d bytes", (int) response->len);

		if (!output->audio) {
			MSG2(2, "output_module",
				"Audio event but server audio not set up");
			retcode = -5;
			goto out;
		}

		if (output_stop_requested || (output_pause_requested && output_pause_queued)) {
			MSG2(5, "output_module", "Discarding audio still coming from the synth");
			goto out;
		}

		while (1) {
			if (strncmp(p, "705-", 4) != 0) {
				MSG2(2, "output_module",
					"ERROR: bogus audio parameter %s", p);
				retcode = -5;
				break;
			}
			q = memchr(p, '\n', end - p);
			if (!q) {
				MSG2(2, "output_module",
					"ERROR: bogus audio end of line %s", p);
				retcode = -5;
				break;
			}

			if (strncmp(p, "705-AUDIO", strlen("705-AUDIO")) == 0 && p[strlen("705-AUDIO")] == '\0') {
				p += strlen("705-AUDIO") + 1;
				break;
			}

			if (strncmp(p, "705-big_endian=", strlen("705-big_endian=")) == 0) {
				format = atoi(p + strlen("705-big_endian="));
			}
#define SET_AUDIO_TRACK_PARAM(name) \
			else if (strncmp(p, "705-"#name"=", strlen("705-"#name"=")) == 0) { \
				track.name = atoi(p+4+strlen(#name)+1); \
				MSG2(5, "output_module", \
					"Got audio parameter "#name" %d", track.name); \
			}
			SET_AUDIO_TRACK_PARAM(bits)
			SET_AUDIO_TRACK_PARAM(num_channels)
			SET_AUDIO_TRACK_PARAM(sample_rate)
			SET_AUDIO_TRACK_PARAM(num_samples)
			else {
				MSG2(2, "output_module",
					"ERROR: unknown audio parameter %s", p);
				retcode = -5;
				break;
			}
			p = q + 1;
		}

		if (retcode < 0)
			goto out;

		size = track.num_channels * track.num_samples * track.bits / 8;
		track.samples = malloc(size);
		filled = 0;

		end = memchr(p, '\n', end - p);
		if (!end) {
			MSG2(2, "output_module",
				"ERROR: bogus audio end of line %s", p);
			retcode = -5;
			goto out;
		}

		char *data = (char*) track.samples;

		/* HDLC escaping: invert bit 5 of escaped characters. */
		const char escape = 0x7d;
		const char invert = 1<<5;

		while (p < end) {
			size_t piece;

			q = memchr(p, escape, end - p);
			if (!q)
				q = end;

			piece = q - p;

			if (filled + piece > size) {
				MSG2(2, "output_module",
					"ERROR: bogus audio content: %zd > %zd", filled + piece, size);
				retcode = -5;
				break;
			}

			memcpy(data + filled, p, piece);
			filled += piece;
			p = q;

			while (p < end && *p == escape) {
				p++;
				if (p == end) {
					MSG2(2, "output_module",
						"ERROR: bogus audio escape at end");
					retcode = -5;
					break;
				}
				if (filled + 1 > size) {
					MSG2(2, "output_module",
						"ERROR: bogus audio content: %zd > %zd", filled + 1, size);
					retcode = -5;
					break;
				}
				data[filled++] = (*p) ^ invert;
				p++;
			}
			if (retcode < 0)
				break;
		}

		if (filled != size) {
			MSG2(2, "output_module",
				"ERROR: bogus audio content: %zd < %zd", filled, size);
			retcode = -5;
		}

		if (retcode < 0) {
			free(track.samples);
			goto out;
		}

		MSG2(5, "output_module",
			"Got audio: eventually %zd bytes", size);

		gboolean ret = module_speak_queue_add_audio(&track, format);

		free(track.samples);

		if (!ret)
			MSG2(2, "output_module", "Audio interrupted");
	} else {
		MSG2(2, "output_module",
		     "ERROR: Unknown event received from output module");
		retcode = -5;
	}

out:
	if (retcode < 0)
		module_report_event_broken();
	g_string_free(response, TRUE);
	return retcode;
}

/* For server-side audio, this is called in a separate thread, to consume output
 * from the module in parallel of handling audio processing and feedback to client */
static void *output_thread_func(void *data)
{
	OutputModule *output = data;
	int ret;

	while (1) {
		ret = output_module_is_speaking(output);
		if (ret < 0) {
			MSG2(3, "output_module", "output_module_is_speaking error");
			pthread_exit(NULL);
		}
		if (ret == 0) {
			MSG2(4, "output_module", "finished getting data from output module");
			pthread_exit(NULL);
		}
	}
}

int output_is_speaking(char **index_mark)
{
	OutputModule *output = speaking_module;

	speak_queue_entry *entry;
	char c;
	int end = 0, ret;

	/* Wait for next event */
	ret = read(output->pipe_speak[0], &c, 1);
	if (ret != 1)
		MSG(1, "Warning: couln't read from pipe_speak: %d returned, (errno = %d, %s)\n", ret, errno, strerror(errno));

	pthread_mutex_lock(&playback_events_mutex);
	entry = playback_events->data;
	playback_events = g_slist_remove(playback_events, entry);
	pthread_mutex_unlock(&playback_events_mutex);

	/* Process next event */
	switch (entry->type) {
		case SPEAK_QUEUE_QET_AUDIO:
			MSG2(3, "output_module", "audio event ??");
			g_free(entry->data.audio.track.samples);
			*index_mark = (char *)g_strdup("no");
			break;
		case SPEAK_QUEUE_QET_INDEX_MARK:
			*index_mark = entry->data.markId;
			break;
		case SPEAK_QUEUE_QET_SOUND_ICON:
			MSG2(3, "output_module", "audio icon event ??");
			g_free(entry->data.sound_icon_filename);
			*index_mark = (char *)g_strdup("no");
			break;
		case SPEAK_QUEUE_QET_BEGIN:
			*index_mark = (char *)g_strdup("__spd_begin");
			break;
		case SPEAK_QUEUE_QET_END:
			*index_mark = (char *)g_strdup("__spd_end");
			end = 1;
			break;
		case SPEAK_QUEUE_QET_PAUSE:
			*index_mark = (char *)g_strdup("__spd_paused");
			end = 1;
			break;
		case SPEAK_QUEUE_QET_STOP:
			*index_mark = (char *)g_strdup("__spd_stopped");
			end = 1;
			break;
		case SPEAK_QUEUE_QET_BROKEN:
			*index_mark = NULL;
			end = 1;
			break;
	}
	g_free(entry);

	if (end) {
		/* Wait for all audio processing to terminate before cleaning
		 * everything */
		pthread_join(output_thread, NULL);
	}

	return 0;
}

/* Wait until the child _pid_ returns with timeout. Calls waitpid() each 100ms
 until timeout is exceeded. This is not exact and you should not rely on the
 exact time waited. */
int
waitpid_with_timeout(pid_t pid, int *status_ptr, int options, size_t timeout)
{
	size_t i;
	int ret;
	for (i = 0; i <= timeout; i += 100) {
		ret = waitpid(pid, status_ptr, options | WNOHANG);
		if (ret > 0)
			return ret;
		if (ret < 0)
			return ret;
		usleep(100 * 1000);	/* Sleep 100 ms */
	}
	return 0;
}

int output_close(OutputModule * module)
{
	int err;
	int ret;
	OutputModule *output;
	output = module;

	if (output == NULL)
		return -1;

	output_lock();

	assert(output->name != NULL);
	MSG(3, "Closing module \"%s\"...", output->name);
	if (output->working) {
		SEND_DATA("STOP\n");
		SEND_CMD("QUIT");
		usleep(100);
		/* So that the module has some time to exit() correctly */
	}

	MSG(4, "Waiting for module pid %d", module->pid);
	ret = waitpid_with_timeout(module->pid, NULL, 0, 1000);
	if (ret > 0) {
		MSG(4, "Ok, module closed successfully.");
	} else if (ret == 0) {
		int ret2;
		MSG(1, "ERROR: Timed out when waiting for child cancellation");
		MSG(3, "Killing the module");
		kill(module->pid, SIGKILL);
		MSG(4, "Waiting until the child terminates.");
		ret2 = waitpid_with_timeout(module->pid, NULL, 0, 1000);
		if (ret2 > 0) {
			MSG(3, "Module terminated");
		} else {
			MSG(1,
			    "ERROR: Module is not able to terminate, giving up.");
		}
	} else {
		MSG(1,
		    "ERROR: waitpid() failed when waiting for child (module).");
	}

	OL_RET(0)
}

#undef SEND_CMD
#undef SEND_DATA

int output_check_module(OutputModule * output)
{
	int ret;
	int err;
	int status;

	if (output == NULL)
		return -1;

	MSG(4, "Output module working status: %d (pid:%d)", output->working,
	    output->pid);

	if (output->working == 0) {
		/* Investigate on why it crashed */
		ret = waitpid(output->pid, &status, WNOHANG);
		if (ret == 0) {
			MSG(2, "Output module not running.");
			return 0;
		}
		ret = WIFEXITED(status);

		/* TODO: Linux kernel implementation of threads is not very good :(  */
		//        if (ret == 0){
		if (1) {
			/* Module terminated abnormally */
			MSG(2,
			    "Output module terminated abnormally, probably crashed.");
		} else {
			/* Module terminated normally, check status */
			err = WEXITSTATUS(status);
			if (err == 0)
				MSG(2, "Module exited normally");
			if (err == 1)
				MSG(2, "Internal error in output module!");
			if (err == 2) {
				MSG(2,
				    "Output device not working. For software devices, this can mean"
				    "that they are not running or they are not accessible due to wrong"
				    "acces permissions.");
			}
			if (err > 2)
				MSG(2,
				    "Unknown error happened in output module, exit status: %d !",
				    err);
		}
	}
	return 0;
}

char *escape_dot(char *otext)
{
	char *seq;
	GString *ntext;
	char *ootext;
	char *ret = NULL;

	if (otext == NULL)
		return NULL;

	MSG2(5, "escaping", "Incoming text: |%s|", otext);

	ootext = otext;

	ntext = g_string_new("");

	if (otext[0] == '.') {
		g_string_append(ntext, "..");
		otext += 1;
	}

	MSG2(6, "escaping", "Altering text (I): |%s|", ntext->str);

	while ((seq = strstr(otext, "\n."))) {
		*seq = 0;
		g_string_append(ntext, otext);
		g_string_append(ntext, "\n..");
		otext = seq + 2;
	}

	MSG2(6, "escaping", "Altering text (II): |%s|", ntext->str);

	if (otext == ootext) {
		g_string_free(ntext, 1);
		ret = otext;
	} else {
		g_string_append(ntext, otext);
		g_free(ootext);
		ret = ntext->str;
		g_string_free(ntext, 0);
	}

	MSG2(6, "escaping", "Altered text: |%s|", ret);

	return ret;
}
