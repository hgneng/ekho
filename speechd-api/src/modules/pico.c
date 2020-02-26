/*
 * pico.c - Speech Dispatcher SVOX pico output module
 *
 * A SVOX pico output module
 *
 * Copyright (C) 2010 Brailcom, o.p.s.
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <sys/types.h>
#include <signal.h>
#include <string.h>

#include <glib.h>
#include <semaphore.h>

#include <picoapi.h>

#include "spd_audio.h"
#include <speechd_types.h>
#include "module_utils.h"

#define MODULE_NAME     "pico"
#define MODULE_VERSION  "0.1"

DECLARE_DEBUG();

#define MAX_OUTBUF_SIZE		(128)
#define PICO_MEM_SIZE			(10000000)

#define PICO_VOICE_SPEED_MIN		(20)
#define PICO_VOICE_SPEED_MAX		(500)
#define PICO_VOICE_SPEED_DEFAULT	(100)

#define PICO_VOICE_PITCH_MIN		(50)
#define PICO_VOICE_PITCH_MAX		(200)
#define PICO_VOICE_PITCH_DEFAULT	(100)

#define PICO_VOICE_VOLUME_MIN		(0)
#define PICO_VOICE_VOLUME_MAX		(500)
#define PICO_VOICE_VOLUME_DEFAULT	(100)

static pico_System picoSystem;
static pico_Resource picoTaResource;
static pico_Resource picoSgResource;
static pico_Engine picoEngine;
static pico_Char *picoInp;

static const char *PICO_LINGWARE_PATH = "/usr/share/pico/lang/";
static const int PICO_SAMPLE_RATE = 16000;
static const char *picoInternalTaLingware[] = {
	"en-US_ta.bin",
	"en-GB_ta.bin",
	"de-DE_ta.bin",
	"es-ES_ta.bin",
	"fr-FR_ta.bin",
	"it-IT_ta.bin"
};

static const char *picoInternalSgLingware[] = {
	"en-US_lh0_sg.bin",
	"en-GB_kh0_sg.bin",
	"de-DE_gl0_sg.bin",
	"es-ES_zl0_sg.bin",
	"fr-FR_nk0_sg.bin",
	"it-IT_cm0_sg.bin"
};

static const SPDVoice pico_voices[] = {
	{"samantha", "en-US", NULL},
	{"serena", "en-GB", NULL},
	{"sabrina", "de-DE", NULL},
	{"isabel", "es-ES", NULL},
	{"virginie", "fr-FR", NULL},
	{"silvia", "it-IT", NULL}
};

static const SPDVoice *pico_voices_list[] = {
	&pico_voices[0],
	&pico_voices[1],
	&pico_voices[2],
	&pico_voices[3],
	&pico_voices[4],
	&pico_voices[5],
	NULL
};

static GThread *pico_play_thread;
static sem_t pico_play_semaphore;
static sem_t pico_idle_semaphore;

enum states { STATE_IDLE, STATE_PLAY, STATE_PAUSE, STATE_STOP, STATE_CLOSE };
static enum states pico_state;

/* Module configuration options */
MOD_OPTION_1_STR(PicoLingwarePath)

static int pico_set_rate(signed int value)
{
	int speed;

	if (value < 0)
		speed = PICO_VOICE_SPEED_MIN + (value - (-100))
		    * (PICO_VOICE_SPEED_DEFAULT - PICO_VOICE_SPEED_MIN)
		    / (0 - (-100));
	else
		speed = PICO_VOICE_SPEED_DEFAULT + (value - 0)
		    * (PICO_VOICE_SPEED_MAX - PICO_VOICE_SPEED_DEFAULT)
		    / (100 - 0);

	return speed;
}

static int pico_set_volume(signed int value)
{
	int volume;

	volume = PICO_VOICE_VOLUME_MIN + (value - (-100))
	    * (PICO_VOICE_VOLUME_DEFAULT - PICO_VOICE_VOLUME_MIN)
	    / (100 - (-100));

	return volume;
}

static int pico_set_pitch(signed int value)
{
	int pitch;

	if (value < 0)
		pitch = PICO_VOICE_PITCH_MIN + (value - (-100))
		    * (PICO_VOICE_PITCH_DEFAULT - PICO_VOICE_PITCH_MIN)
		    / (0 - (-100));
	else
		pitch = PICO_VOICE_PITCH_DEFAULT + (value - 0)
		    * (PICO_VOICE_PITCH_MAX - PICO_VOICE_PITCH_DEFAULT)
		    / (100 - 0);

	return pitch;
}

static int pico_process_tts(void)
{
	pico_Int16 bytes_sent, bytes_recv, text_remaining, out_data_type;
	int ret, getstatus;
	short outbuf[MAX_OUTBUF_SIZE];
	pico_Retstring outMessage;
	AudioTrack track;
#if defined(BYTE_ORDER) && (BYTE_ORDER == BIG_ENDIAN)
	AudioFormat format = SPD_AUDIO_BE;
#else
	AudioFormat format = SPD_AUDIO_LE;
#endif
	pico_Char *buf = picoInp;

	text_remaining = strlen((const char *)buf) + 1;

	DBG(MODULE_NAME " Text: %s\n", picoInp);

	/* synthesis loop   */
	while (text_remaining) {
		/* Feed the text into the engine.   */
		if ((ret = pico_putTextUtf8(picoEngine, buf, text_remaining,
					    &bytes_sent))) {
			pico_getSystemStatusMessage(picoSystem, ret,
						    outMessage);
			DBG(MODULE_NAME "Cannot put Text (%i): %s\n", ret,
			    outMessage);
			return -1;
		}

		text_remaining -= bytes_sent;
		buf += bytes_sent;

		do {
			/* Retrieve the samples and add them to the buffer.
			   SVOX pico TTS sample rate is 16K */
			getstatus = pico_getData(picoEngine, (void *)outbuf,
						 MAX_OUTBUF_SIZE, &bytes_recv,
						 &out_data_type);
			if ((getstatus != PICO_STEP_BUSY)
			    && (getstatus != PICO_STEP_IDLE)) {
				pico_getSystemStatusMessage(picoSystem,
							    getstatus,
							    outMessage);
				DBG(MODULE_NAME "Cannot get Data (%i): %s\n",
				    getstatus, outMessage);
				return -1;
			}

			if (bytes_recv) {
				track.num_samples = bytes_recv / 2;
				track.samples =
				    (short *)g_memdup((gconstpointer) outbuf,
						      bytes_recv);
				track.num_channels = 1;
				track.sample_rate = PICO_SAMPLE_RATE;
				track.bits = 16;
				DBG(MODULE_NAME
				    ": Sending %i samples to audio.",
				    track.num_samples);

				if (module_tts_output(track, format) < 0) {
					DBG(MODULE_NAME
					    "Can't play track for unknown reason.");
					return -1;
				}
			}
			if (g_atomic_int_get(&pico_state) != STATE_PLAY) {
				text_remaining = 0;
				break;
			}
		} while (PICO_STEP_BUSY == getstatus);
	}

	g_free(picoInp);
	picoInp = NULL;
	return 0;
}

/* Playback thread. */
static gpointer pico_play_func(gpointer nothing)
{
	DBG(MODULE_NAME ": Playback thread starting");

	set_speaking_thread_parameters();

	while (g_atomic_int_get(&pico_state) != STATE_CLOSE) {

		sem_wait(&pico_play_semaphore);
		if (g_atomic_int_get(&pico_state) != STATE_PLAY)
			continue;

		DBG(MODULE_NAME ": Sending to TTS engine");
		module_report_event_begin();

		if (0 != pico_process_tts()) {
			DBG(MODULE_NAME ": ERROR in TTS");
		}

		if (g_atomic_int_get(&pico_state) == STATE_PLAY) {
			module_report_event_end();
			g_atomic_int_set(&pico_state, STATE_IDLE);
		}

		if (g_atomic_int_get(&pico_state) == STATE_STOP) {
			module_report_event_stop();
			g_atomic_int_set(&pico_state, STATE_IDLE);
			sem_post(&pico_idle_semaphore);

		}

		if (g_atomic_int_get(&pico_state) == STATE_PAUSE) {
			module_report_event_pause();
			g_atomic_int_set(&pico_state, STATE_IDLE);
			sem_post(&pico_idle_semaphore);
		}

		DBG(MODULE_NAME ": state %d", pico_state);

	}
	return 0;
}

/* Public functions */
int module_load(void)
{
	INIT_SETTINGS_TABLES();

	MOD_OPTION_1_INT_REG(Debug, 0);
	MOD_OPTION_1_STR_REG(PicoLingwarePath, PICO_LINGWARE_PATH);

	return 0;
}

int pico_init_voice(int voice_index)
{
	int ret;
	pico_Retstring outMessage;
	pico_Char picoTaFileName[PICO_MAX_DATAPATH_NAME_SIZE +
				 PICO_MAX_FILE_NAME_SIZE];
	pico_Char picoSgFileName[PICO_MAX_DATAPATH_NAME_SIZE +
				 PICO_MAX_FILE_NAME_SIZE];
	pico_Char picoTaResourceName[PICO_MAX_RESOURCE_NAME_SIZE];
	pico_Char picoSgResourceName[PICO_MAX_RESOURCE_NAME_SIZE];

	/* Load the text analysis Lingware resource file.   */
	strcpy((char *)picoTaFileName, PicoLingwarePath);
	strcat((char *)picoTaFileName,
	       (const char *)picoInternalTaLingware[voice_index]);
	if ((ret =
	     pico_loadResource(picoSystem, picoTaFileName, &picoTaResource))) {
		pico_getSystemStatusMessage(picoSystem, ret, outMessage);
		DBG(MODULE_NAME
		    "Cannot load TA Lingware resource file (%i): %s\n", ret,
		    outMessage);
		return -1;
	}

	/* Load the signal generation Lingware resource file.   */
	strcpy((char *)picoSgFileName, PicoLingwarePath);
	strcat((char *)picoSgFileName,
	       (const char *)picoInternalSgLingware[voice_index]);
	if ((ret =
	     pico_loadResource(picoSystem, picoSgFileName, &picoSgResource))) {
		pico_getSystemStatusMessage(picoSystem, ret, outMessage);
		DBG(MODULE_NAME
		    "Cannot load SG Lingware resource file (%i): %s\n", ret,
		    outMessage);
		return -1;
	}

	/* Get the text analysis resource name.     */
	if ((ret = pico_getResourceName(picoSystem, picoTaResource,
					(char *)picoTaResourceName))) {
		pico_getSystemStatusMessage(picoSystem, ret, outMessage);
		DBG(MODULE_NAME
		    "Cannot get TA resource name (%i): %s\n", ret, outMessage);
		return -1;
	}

	/* Get the signal generation resource name. */
	if ((ret = pico_getResourceName(picoSystem, picoSgResource,
					(char *)picoSgResourceName))) {
		pico_getSystemStatusMessage(picoSystem, ret, outMessage);
		DBG(MODULE_NAME
		    "Cannot get SG resource name (%i): %s\n", ret, outMessage);
		return -1;
	}

	/* Create a voice definition.   */
	if ((ret = pico_createVoiceDefinition(picoSystem, (const pico_Char *)
					      pico_voices[voice_index].name))) {
		pico_getSystemStatusMessage(picoSystem, ret, outMessage);
		DBG(MODULE_NAME
		    "Cannot create voice definition (%i): %s\n", ret,
		    outMessage);
		return -1;
	}

	/* Add the text analysis resource to the voice. */
	if ((ret = pico_addResourceToVoiceDefinition(picoSystem,
						     (const pico_Char *)
						     pico_voices
						     [voice_index].name,
						     picoTaResourceName))) {
		pico_getSystemStatusMessage(picoSystem, ret, outMessage);
		DBG(MODULE_NAME
		    "Cannot add TA resource to the voice (%i): %s\n",
		    ret, outMessage);
		return -1;
	}

	/* Add the signal generation resource to the voice. */
	if ((ret = pico_addResourceToVoiceDefinition(picoSystem,
						     (const pico_Char *)
						     pico_voices
						     [voice_index].name,
						     picoSgResourceName))) {
		pico_getSystemStatusMessage(picoSystem, ret, outMessage);
		DBG(MODULE_NAME
		    "Cannot add SG resource to the voice (%i): %s\n",
		    ret, outMessage);
		return -1;
	}

	return 0;
}

int module_init(char **status_info)
{
	int ret, i;
	pico_Retstring outMessage;
	void *pmem;
	GError *error = NULL;

	sem_init(&pico_play_semaphore, 0, 0);
	sem_init(&pico_idle_semaphore, 0, 0);

	if ((pico_play_thread = g_thread_try_new(NULL, (GThreadFunc) pico_play_func,
						NULL, &error)) == NULL) {
		*status_info = g_strdup_printf(MODULE_NAME
					       "Failed to create a play thread : %s\n",
					       error->message);
		DBG(MODULE_NAME ": %s", *status_info);
		g_error_free(error);
		return -1;
	}

	pmem = g_malloc(PICO_MEM_SIZE);
	if ((ret = pico_initialize(pmem, PICO_MEM_SIZE, &picoSystem))) {
		pico_getSystemStatusMessage(picoSystem, ret, outMessage);
		*status_info = g_strdup_printf(MODULE_NAME
					       ": Cannot initialize (%i): %s\n",
					       ret, outMessage);
		g_free(pmem);
		return -1;
	}

	/* load resource for all language, probably need only one */
	for (i = 0; i < sizeof(pico_voices) / sizeof(SPDVoice); i++) {
		if (0 != pico_init_voice(i)) {
			g_free(pmem);
			*status_info = g_strdup_printf(MODULE_NAME
						       ": fail init voice (%s)\n",
						       pico_voices[i].name);
			return -1;
		}
	}

	/* Create a new Pico engine, english default */
	if ((ret = pico_newEngine(picoSystem,
				  (const pico_Char *)pico_voices[0].name,
				  &picoEngine))) {
		pico_getSystemStatusMessage(picoSystem, ret, outMessage);
		*status_info = g_strdup_printf(MODULE_NAME
					       ": Cannot create a new pico engine (%i): %s\n",
					       ret, outMessage);
		return -1;
	}

	*status_info = g_strdup(MODULE_NAME ": Initialized successfully.");

	g_atomic_int_set(&pico_state, STATE_IDLE);
	return 0;
}

SPDVoice **module_list_voices(void)
{
	return (SPDVoice **)pico_voices_list;
}

void pico_set_synthesis_voice(char *voice_name)
{
	int ret;
	pico_Retstring outMessage;

	/* Create a new Pico engine, english default */
	if ((ret = pico_disposeEngine(picoSystem, &picoEngine))) {
		pico_getSystemStatusMessage(picoSystem, ret, outMessage);
		DBG(MODULE_NAME
		    "Cannot dispose pico engine (%i): %s\n", ret, outMessage);
		return;
	}

	/* Create a new Pico engine, english default */
	if ((ret = pico_newEngine(picoSystem, (const pico_Char *)voice_name,
				  &picoEngine))) {
		pico_getSystemStatusMessage(picoSystem, ret, outMessage);
		DBG(MODULE_NAME
		    "Cannot create a new pico engine (%i): %s\n", ret,
		    outMessage);
		/* Try to fallback to english */
		if ((ret = pico_newEngine(picoSystem,
					  (const pico_Char *)pico_voices[0].name,
					  &picoEngine))) {
			pico_getSystemStatusMessage(picoSystem, ret, outMessage);
			DBG(MODULE_NAME
			    "Cannot create default english pico engine (%i): %s\n", ret,
			    outMessage);
			return;
		}

		return;
	}

	return;
}

static void pico_set_language(char *lang)
{
	int i;
	DBG(MODULE_NAME "setting language %s", lang);

	/* get voice name based on language */
	for (i = 0; i < sizeof(pico_voices) / sizeof(SPDVoice); i++) {
		if (!strcasecmp(pico_voices[i].language, lang)) {
			pico_set_synthesis_voice(pico_voices[i].name);
			return;
		}
	}
	/* get voice name based on main part of language */
	for (i = 0; i < sizeof(pico_voices) / sizeof(SPDVoice); i++) {
		if (!strncasecmp(pico_voices[i].language, lang, 2)) {
			pico_set_synthesis_voice(pico_voices[i].name);
			return;
		}
	}
	return;
}

int module_speak(char *data, size_t bytes, SPDMessageType msgtype)
{
	int value;
	static pico_Char *tmp;

	if (g_atomic_int_get(&pico_state) != STATE_IDLE) {
		DBG(MODULE_NAME
		    ": module still speaking state = %d", pico_state);
		return 0;
	}

	/* Setting speech parameters. */

	UPDATE_STRING_PARAMETER(voice.name, pico_set_synthesis_voice);
	/*      UPDATE_PARAMETER(voice_type, pico_set_voice); */
	UPDATE_STRING_PARAMETER(voice.language, pico_set_language);

	picoInp = (pico_Char *) module_strip_ssml(data);

	value = pico_set_rate(msg_settings.rate);
	if (PICO_VOICE_SPEED_DEFAULT != value) {
		tmp = picoInp;
		picoInp = (pico_Char *)
		    g_strdup_printf("<speed level='%d'>%s</speed>", value, tmp);
		g_free(tmp);
	}

	value = pico_set_volume(msg_settings.volume);
	if (PICO_VOICE_VOLUME_DEFAULT != value) {
		tmp = picoInp;
		picoInp = (pico_Char *)
		    g_strdup_printf("<volume level='%d'>%s</volume>", value,
				    tmp);
		g_free(tmp);
	}

	value = pico_set_pitch(msg_settings.pitch);
	if (PICO_VOICE_PITCH_DEFAULT != value) {
		tmp = picoInp;
		picoInp = (pico_Char *)
		    g_strdup_printf("<pitch level='%d'>%s</pitch>", value, tmp);
		g_free(tmp);
	}

	/*      switch (msgtype) {
	   case SPD_MSGTYPE_CHAR:
	   case SPD_MSGTYPE_KEY:
	   case SPD_MSGTYPE_TEXT:
	   case SPD_MSGTYPE_SOUND_ICON:
	   default:
	   DBG(MODULE_NAME
	   ": msgtype = %d", msgtype);
	   break;
	   }
	 */
	g_atomic_int_set(&pico_state, STATE_PLAY);
	sem_post(&pico_play_semaphore);
	return bytes;
}

int module_stop(void)
{
	pico_Status ret;
	pico_Retstring outMessage;

	if (g_atomic_int_get(&pico_state) != STATE_PLAY) {
		DBG(MODULE_NAME ": STOP called when not in PLAY state");
		return -1;
	}

	g_atomic_int_set(&pico_state, STATE_STOP);
	sem_wait(&pico_idle_semaphore);

	/* reset Pico engine. */
	if ((ret = pico_resetEngine(picoEngine, PICO_RESET_SOFT))) {
		pico_getSystemStatusMessage(picoSystem, ret, outMessage);
		DBG(MODULE_NAME
		    "Cannot reset pico engine (%i): %s\n", ret, outMessage);
		return -1;
	}

	return 0;
}

size_t module_pause(void)
{
	pico_Status ret;
	pico_Retstring outMessage;

	if (g_atomic_int_get(&pico_state) != STATE_PLAY) {
		DBG(MODULE_NAME ": PAUSE called when not in PLAY state");
		return -1;
	}

	g_atomic_int_set(&pico_state, STATE_PAUSE);
	sem_wait(&pico_idle_semaphore);

	/* reset Pico engine. */
	if ((ret = pico_resetEngine(picoEngine, PICO_RESET_SOFT))) {
		pico_getSystemStatusMessage(picoSystem, ret, outMessage);
		DBG(MODULE_NAME
		    "Cannot reset pico engine (%i): %s\n", ret, outMessage);
		return -1;
	}

	return 0;
}

int module_close(void)
{

	g_atomic_int_set(&pico_state, STATE_CLOSE);
	sem_post(&pico_play_semaphore);

	g_thread_join(pico_play_thread);

	if (picoSystem) {
		pico_terminate(&picoSystem);
		picoSystem = NULL;
	}

	sem_destroy(&pico_idle_semaphore);
	sem_destroy(&pico_play_semaphore);

	return 0;
}
