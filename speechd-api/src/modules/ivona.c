
/*
 * ivona.c - Speech Dispatcher backend for Ivona (IVO Software)
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
 * $Id: ivona.c,v 1.3 2008-06-27 12:29:32 hanke Exp $
 */

/* this file is strictly based on flite.c */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <libdumbtts.h>

#include <speechd_types.h>

#include "module_utils.h"
#include "ivona_client.h"

#define MODULE_NAME     "ivona"
#define MODULE_VERSION  "0.3"

#define DEBUG_MODULE 1
DECLARE_DEBUG();

static int ivona_speaking = 0;


signed int ivona_volume = 0;
signed int ivona_cap_mode = 0;
int ivona_punct_mode = 0;

/* Internal functions prototypes */
static int ivona_get_msgpart(struct dumbtts_conf *conf, SPDMessageType type,
			     char **msg, char *icon, char **buf, int *len,
			     int cap_mode, char *delimeters, int punct_mode,
			     char *punct_some);
static void ivona_set_volume(signed int volume);
static void ivona_set_punctuation_mode(SPDPunctuation punct_mode);
static void ivona_set_cap_let_recogn(SPDCapitalLetters cap_mode);

static void _ivona_speak(char *ivona_message, SPDMessageType ivona_message_type);

int ivona_stop = 0;

MOD_OPTION_1_STR(IvonaDelimiters);
MOD_OPTION_1_STR(IvonaPunctuationSome);
MOD_OPTION_1_INT(IvonaMinCapLet);
MOD_OPTION_1_STR(IvonaSoundIconPath);
MOD_OPTION_1_STR(IvonaServerHost);
MOD_OPTION_1_INT(IvonaServerPort);
MOD_OPTION_1_INT(IvonaSampleFreq);

MOD_OPTION_1_STR(IvonaSpeakerLanguage);
MOD_OPTION_1_STR(IvonaSpeakerName);

static struct dumbtts_conf *ivona_conf;

/* Public functions */

int module_load(void)
{
	INIT_SETTINGS_TABLES();

	REGISTER_DEBUG();

	MOD_OPTION_1_STR_REG(IvonaDelimiters, ".;:,!?");
	MOD_OPTION_1_INT_REG(IvonaMinCapLet, 0);
	MOD_OPTION_1_STR_REG(IvonaSoundIconPath,
			     "/usr/share/sound/sound-icons/");

	MOD_OPTION_1_STR_REG(IvonaServerHost, "127.0.0.1");
	MOD_OPTION_1_INT_REG(IvonaServerPort, 9123);
	MOD_OPTION_1_INT_REG(IvonaSampleFreq, 16000);

	MOD_OPTION_1_STR_REG(IvonaSpeakerLanguage, "pl");
	MOD_OPTION_1_STR_REG(IvonaSpeakerName, "Jacek");

	MOD_OPTION_1_STR_REG(IvonaPunctuationSome, "()");
	ivona_init_cache();

	return 0;
}

int module_init(char **status_info)
{
	DBG("Module init");

	module_audio_set_server();

	*status_info = NULL;

	/* Init Ivona */
	if (ivona_init_sock(IvonaServerHost, IvonaServerPort)) {
		DBG("Couldn't init socket parameters");
		*status_info = g_strdup("Can't initialize socket. "
					"Check server host/port.");
		return -1;
	}
	ivona_conf = dumbtts_TTSInit(IvonaSpeakerLanguage);

	DBG("IvonaDelimiters = %s\n", IvonaDelimiters);

	ivona_speaking = 0;

	*status_info = g_strdup("Ivona initialized successfully.");

	return 0;
}

static SPDVoice voice_jacek;
static SPDVoice *voice_ivona[] = { &voice_jacek, NULL };

SPDVoice **module_list_voices(void)
{
	voice_jacek.name = IvonaSpeakerName;
	voice_jacek.language = IvonaSpeakerLanguage;
	return voice_ivona;
}

void module_speak_sync(const gchar * data, size_t bytes, SPDMessageType msgtype)
{
	char *ivona_message;
	SPDMessageType ivona_message_type;

	DBG("write()\n");

	if (ivona_speaking) {
		DBG("Speaking when requested to write");
		module_speak_error();
		return;
	}

	DBG("Requested data: |%s|\n", data);

	ivona_message = module_strip_ssml(data);
	ivona_message_type = msgtype;
	if ((msgtype == SPD_MSGTYPE_TEXT)
	    && (msg_settings.spelling_mode == SPD_SPELL_ON))
		ivona_message_type = SPD_MSGTYPE_SPELL;

	/* Setting voice */
	UPDATE_PARAMETER(volume, ivona_set_volume);
	UPDATE_PARAMETER(cap_let_recogn, ivona_set_cap_let_recogn);
	UPDATE_PARAMETER(punctuation_mode, ivona_set_punctuation_mode);

	ivona_speaking = 1;

	module_speak_ok();
	_ivona_speak(ivona_message, ivona_message_type);

	DBG("Ivona: leaving write() normally\n\r");
}

int module_stop(void)
{
	DBG("ivona: stop()\n");

	ivona_stop = 1;

	return 0;
}

size_t module_pause(void)
{
	DBG("pause requested\n");
	if (ivona_speaking) {
		DBG("Ivona doesn't support pause, stopping\n");

		module_stop();

		return -1;
	} else {
		return 0;
	}
}

int module_close(void)
{

	DBG("ivona: close()\n");

	DBG("Stopping speech");
	if (ivona_speaking) {
		module_stop();
	}

	return 0;
}

/* Internal functions */
static int get_unichar(char **str)
{
	wchar_t wc;
	int n;
	wc = *(*str)++ & 255;
	if ((wc & 0xe0) == 0xc0) {
		wc &= 0x1f;
		n = 1;
	} else if ((wc & 0xf0) == 0xe0) {
		wc &= 0x0f;
		n = 2;
	} else if ((wc & 0xf8) == 0xf0) {
		wc &= 0x07;
		n = 3;
	} else if ((wc & 0xfc) == 0xf8) {
		wc &= 0x03;
		n = 4;
	} else if ((wc & 0xfe) == 0xfc) {
		wc &= 0x01;
		n = 5;
	} else
		return wc;
	while (n--) {
		if ((**str & 0xc0) != 0x80) {
			wc = '?';
			break;
		}
		wc = (wc << 6) | ((*(*str)++) & 0x3f);
	}
	return wc;
}

static int ivona_get_msgpart(struct dumbtts_conf *conf, SPDMessageType type,
			     char **msg, char *icon, char **buf, int *len,
			     int cap_mode, char *delimeters, int punct_mode,
			     char *punct_some)
{
	int rc;
	int isicon;
	int n, bytes;
	unsigned int pos;
	wchar_t wc;
	char xbuf[1024];

	if (!*msg)
		return 1;
	if (!**msg)
		return 1;
	isicon = 0;
	icon[0] = 0;
	if (*buf)
		**buf = 0;
	DBG("Ivona message %s type %d\n", *msg, type);
	switch (type) {
	case SPD_MSGTYPE_SOUND_ICON:
		if (strlen(*msg) < 63) {
			strcpy(icon, *msg);
			rc = 0;
		} else {
			rc = 1;
		}
		*msg = NULL;
		return rc;

	case SPD_MSGTYPE_SPELL:
		wc = get_unichar(msg);
		if (!wc) {
			*msg = NULL;
			return 1;
		}
		n = dumbtts_WCharString(conf, wc, *buf, *len, cap_mode,
					&isicon);
		if (n > 0) {
			*len = n + 128;
			*buf = g_realloc(*buf, *len);
			n = dumbtts_WCharString(conf, wc, *buf, *len, cap_mode,
						&isicon);
		}
		if (n) {
			*msg = NULL;
			return 1;
		}
		if (isicon)
			strcpy(icon, "capital");
		return 0;

	case SPD_MSGTYPE_KEY:
	case SPD_MSGTYPE_CHAR:

		if (type == SPD_MSGTYPE_KEY) {
			/* TODO: make sure all SSIP cases are supported */
			n = dumbtts_KeyString(conf, *msg, *buf, *len, cap_mode,
					      &isicon);
		} else {
			n = dumbtts_CharString(conf, *msg, *buf, *len, cap_mode,
					       &isicon);
		}
		DBG("Got n=%d", n);
		if (n > 0) {
			*len = n + 128;
			*buf = g_realloc(*buf, *len);
			if (type == SPD_MSGTYPE_KEY) {
				n = dumbtts_KeyString(conf, *msg, *buf, *len,
						      cap_mode, &isicon);
			} else {
				n = dumbtts_CharString(conf, *msg, *buf, *len,
						       cap_mode, &isicon);
			}
		}
		*msg = NULL;

		if (!n && isicon)
			strcpy(icon, "capital");
		return n;

	case SPD_MSGTYPE_TEXT:
		pos = 0;
		bytes =
		    module_get_message_part(*msg, xbuf, &pos, 1023, delimeters);
		DBG("Got bytes %d, %s", bytes, xbuf);
		if (bytes <= 0) {
			*msg = NULL;
			return 1;
		}
		*msg += pos;
		xbuf[bytes] = 0;

		n = dumbtts_GetString(conf, xbuf, *buf, *len, punct_mode,
				      punct_some, ",.;:!?");

		if (n > 0) {
			*len = n + 128;
			*buf = g_realloc(*buf, *len);
			n = dumbtts_GetString(conf, xbuf, *buf, *len,
					      punct_mode, punct_some, ",.;:!?");
		}
		if (n) {
			*msg = NULL;
			return 1;
		}
		DBG("Returning to Ivona |%s|", *buf);
		return 0;

	default:

		*msg = NULL;
		DBG("Unknown message type\n");
		return 1;
	}
}

static void _ivona_speak(char *ivona_message, SPDMessageType ivona_message_type)
{
	AudioTrack track;
	char *buf;
	int len;
	char *msg, *audio;
	char icon[64];
	int samples, offset;
	int fd;
	char *next_audio;
	int next_samples, next_offset;
	char next_icon[64];
	char next_cache[16];

	DBG("ivona: speaking starting.......\n");

	ivona_stop = 0;
	ivona_speaking = 1;

	module_report_event_begin();
	msg = ivona_message;
	DBG("To say: %s\n", msg);
	buf = NULL;
	len = 0;
	fd = -1;
	audio = NULL;
	next_audio = NULL;
	next_icon[0] = 0;
	while (1) {
		/* Process server events in case we were told to stop in between */
		module_process(STDIN_FILENO, 0);

		if (ivona_stop) {
			DBG("Stop in child, terminating");
			ivona_speaking = 0;
			module_report_event_stop();
			break;
		}
		audio = NULL;
		if (next_audio) {
			audio = next_audio;
			samples = next_samples;
			offset = next_offset;
			strcpy(icon, next_icon);
			next_audio = NULL;
			DBG("Got wave from next_audio");
		} else if (fd >= 0) {
			audio =
			    ivona_get_wave_fd(fd, &samples, &offset);
			strcpy(icon, next_icon);
			if (audio && next_cache[0]) {
				ivona_store_wave_in_cache(next_cache,
							  audio +
							  2 * offset,
							  samples);
			}

			fd = -1;
			DBG("Got wave from fd");
		} else if (next_icon[0]) {
			strcpy(icon, next_icon);
			DBG("Got icon");
		}
		if (!audio && !icon[0]) {
			if (!msg || !*msg
			    || ivona_get_msgpart(ivona_conf,
						 ivona_message_type,
						 &msg, icon, &buf, &len,
						 ivona_cap_mode,
						 IvonaDelimiters,
						 ivona_punct_mode,
						 IvonaPunctuationSome))
			{
				ivona_speaking = 0;
				if (ivona_stop)
					module_report_event_stop();
				else
					module_report_event_end();
				break;
			}
			if (buf && *buf) {
				audio =
				    ivona_get_wave(buf, &samples,
						   &offset);
				DBG("Got wave from direct");
			}
		}

		/* tu mamy audio albo icon, mozna gadac */
		if (ivona_stop) {
			DBG("Stop in child, terminating");
			ivona_speaking = 0;
			module_report_event_stop();
			break;
		}

		next_icon[0] = 0;
		if (msg && *msg) {
			if (!ivona_get_msgpart
			    (ivona_conf, ivona_message_type, &msg,
			     next_icon, &buf, &len, ivona_cap_mode,
			     IvonaDelimiters, ivona_punct_mode,
			     IvonaPunctuationSome)) {
				if (buf && *buf) {
					next_offset = 0;
					next_audio =
					    ivona_get_wave_from_cache
					    (buf, &next_samples);
					if (!next_audio) {
						DBG("Sending %s to ivona", buf);
						next_cache[0] = 0;
						if (strlen(buf) <=
						    IVONA_CACHE_MAX_STRLEN)
							strcpy
							    (next_cache,
							     buf);
						fd = ivona_send_string
						    (buf);
					}
				}
			}
		}
		if (ivona_stop) {
			DBG("Stop in child, terminating");
			ivona_speaking = 0;
			module_report_event_stop();
			break;
		}
		if (icon[0]) {
			play_icon(IvonaSoundIconPath, icon);
			if (ivona_stop) {
				ivona_speaking = 0;
				module_report_event_stop();
				break;
			}
			icon[0] = 0;
		}
		if (audio) {
			track.num_samples = samples;
			track.num_channels = 1;
			track.sample_rate = IvonaSampleFreq;
			track.bits = 16;
			track.samples = ((short *)audio) + offset;
			DBG("Got %d samples", track.num_samples);
			module_tts_output_server(&track, SPD_AUDIO_LE);
			g_free(audio);
			audio = NULL;
		}
		if (ivona_stop) {
			ivona_speaking = 0;
			module_report_event_stop();
			break;
		}
	}
	ivona_stop = 0;
	g_free(buf);
	g_free(audio);
	g_free(next_audio);
	if (fd >= 0)
		close(fd);
	fd = -1;
	audio = NULL;
	next_audio = NULL;

	ivona_speaking = 0;
}

static void ivona_set_volume(signed int volume)
{
	assert(volume >= -100 && volume <= +100);
	ivona_volume = volume;
}

static void ivona_set_cap_let_recogn(SPDCapitalLetters cap_mode)
{
	ivona_cap_mode = 0;
	switch (cap_mode) {
	case SPD_CAP_SPELL:
		ivona_cap_mode = 2;
		break;
	case SPD_CAP_ICON:
		ivona_cap_mode = 1;
		break;
	case SPD_CAP_NONE:
		ivona_cap_mode = 0;
		break;
	}
	if (ivona_cap_mode < IvonaMinCapLet) {
		ivona_cap_mode = IvonaMinCapLet;
	}
}

static void ivona_set_punctuation_mode(SPDPunctuation punct_mode)
{
	ivona_punct_mode = 1;
	switch (punct_mode) {
	case SPD_PUNCT_ALL:
		ivona_punct_mode = 2;
		break;
	case SPD_PUNCT_MOST:
		/* XXX approximation */
		ivona_punct_mode = 1;
		break;
	case SPD_PUNCT_SOME:
		ivona_punct_mode = 1;
		break;
	case SPD_PUNCT_NONE:
		ivona_punct_mode = 0;
		break;
	}
}
