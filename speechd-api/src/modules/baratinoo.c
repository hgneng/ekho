/*
 * baratinoo.c - Speech Dispatcher backend for Baratinoo (VoxyGen)
 *
 * Copyright (C) 2016 Brailcom, o.p.s.
 * Copyright (C) 2019 Samuel Thibault <samuel.thibault@ens-lyon.org>
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
 */

/*
 * Input and output choices.
 *
 * - The input is sent to the engine through a BCinputTextBuffer.  There is
 *   a single one of those at any given time, and it is filled in
 *   module_speak() and consumed in the synthesis thread.
 *
 *   This doesn't use an input callback generating a continuous flow (and
 *   blocking waiting for more data) even though it would be a fairly nice
 *   design and would allow not to set speech attributes like volume, pitch and
 *   rate as often.  This is because the Baratinoo engine has 2 limitations on
 *   the input callback:
 *
 *   * It consumes everything (or at least a lot) up until the callbacks
 *     reports the input end by returning 0.  Alternatively one could use the
 *     \flush command followed by a newline, so this is not really limiting.
 *
 *   * More problematic, as the buffer callback is expected to feed a single
 *     input, calling BCpurge() (for handling stop events) unregisters it,
 *     requiring to re-add it afterward.  This renders the continuous flow a
 *     lot less useful, as speech attributes like volume, pitch and rate would
 *     have to be set again.
 *
 * - The output uses the signal buffer instead of callback.
 * The output callback sends sound to the output module phonem by
 * phonem, which cause noise parasits with ALSA due to a reset of
 * parameters for each sound call.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <semaphore.h>

#include "module_utils_speak_queue.h"

#ifdef BARATINOO_ABI_IS_STABLE_ENOUGH_FOR_ME
/* See below why this is problematic.  It can however be useful to get the
 * compiler help to check compatibility */
# define BARATINOO_C_API
# include "baratinoo.h"
# include "baratinooio.h"

#define VOICE_INFO_MEMBER(member_type, struct_p, member) \
	(((BaratinooVoiceInfo *)(struct_p))->member)

#else
/*------------------------------ Baratinoo API ------------------------------*/
/*
 * This file does NOT include baratinoo.h and baratinooio.h on purpose.
 * The reason is that Baratinoo does not provide ABI stability, and various
 * things change in minor versions.  This is a problem for this module that
 * would like to support several versions at once.
 *
 * To work around this, we re-define all the API we need from the Baratinoo
 * headers, and patch compatibility possibly dynamically.
 *
 * This has to be done with *EXTREME CARE* not to slip off face-first into the
 * wall.  What we keep is the lowest common denominator between the supported
 * versions, and for the incompatible bits we use dynamic mapping and offsets.
 *
 * Currently supported versions:
 * - 8.1
 * - 8.4
 *
 * To add a new version, you need to:
 * - First, check the diff between the oldest supported version and the new
 *   version.  Diffing against the newest supported version can be handy but
 *   is not necessarily enough.
 * - Once the incompatibilities are identified and they affect us, amend the
 *   code as necessary, keeping in mind to support older versions.  You'll
 *   have to add a new `BV_` constant for the new version, and make sure
 *   everything that uses these constants handles the new value.
 *   - What to look for:
 *     - Members of structures that changed offsed (reordered, changed type, etc.)
 *     - Enumeration values that changed value (order changed, previous member
 *       getting a different default value, etc.)
 *     - Union that changed size.
 *     - Function arguments that changed type or order.
 * - Update get_baratinoo_supported_version() to return the appropriate
 *   constant in the appropriate situation.  If the new version did not break
 *   compatibility with an already supported version, you might just return
 *   the constant for that other version.
 *
 * GOTCHAS
 * - do NOT access BaratinooVoiceInfo members directly, always use
 *   VOICE_INFO_MEMBER(type, struct_p, member)
 * - Make sure the structures allocated on the stack are large enough for all
 *   versions.
 */

#include "baratinoo_compat.h"

/* Dynamic compatibility part */
#include <glib.h>

typedef enum {
	BV_UNSUPPORTED = -1,
	BV_8_1,
	BV_8_4,
	N_SUPPORTED_BARATINOO_VERSIONS
} SupportedBaratinooVersion;

/* BARATINOO_UTF8 */
static const BARATINOO_TEXT_ENCODING bv_BARATINOO_UTF8[N_SUPPORTED_BARATINOO_VERSIONS] = {
	[BV_8_1] = BARATINOO_UTF8__V8_1,
	[BV_8_4] = BARATINOO_UTF8__V8_4,
};
#define BARATINOO_UTF8 (bv_BARATINOO_UTF8[baratinoo_engine.supported_version])

/* BaratinooVoiceInfo */
enum {
	VI_name,
	VI_language,
	VI_iso639,
	VI_iso3166,
	VI_gender,
	VI_age,
	N_VI_MEMBERS
};
static const size_t bv_VoiceInfo_offsets[N_SUPPORTED_BARATINOO_VERSIONS][N_VI_MEMBERS] = {
#define BVIF_MEMBER_DECL(struct, member) [VI_##member] = G_STRUCT_OFFSET(struct, member)
	[BV_8_1] = {
		BVIF_MEMBER_DECL(BaratinooVoiceInfo__V8_1, name),
		BVIF_MEMBER_DECL(BaratinooVoiceInfo__V8_1, language),
		BVIF_MEMBER_DECL(BaratinooVoiceInfo__V8_1, iso639),
		BVIF_MEMBER_DECL(BaratinooVoiceInfo__V8_1, iso3166),
		BVIF_MEMBER_DECL(BaratinooVoiceInfo__V8_1, gender),
		BVIF_MEMBER_DECL(BaratinooVoiceInfo__V8_1, age),
	},
	[BV_8_4] = {
		BVIF_MEMBER_DECL(BaratinooVoiceInfo__V8_4, name),
		BVIF_MEMBER_DECL(BaratinooVoiceInfo__V8_4, language),
		BVIF_MEMBER_DECL(BaratinooVoiceInfo__V8_4, iso639),
		BVIF_MEMBER_DECL(BaratinooVoiceInfo__V8_4, iso3166),
		BVIF_MEMBER_DECL(BaratinooVoiceInfo__V8_4, gender),
		BVIF_MEMBER_DECL(BaratinooVoiceInfo__V8_4, age),
	},
#undef BVIF_MEMBER_DECL
};
#define VOICE_INFO_MEMBER(member_type, struct_p, member) \
	G_STRUCT_MEMBER(member_type, struct_p, bv_VoiceInfo_offsets[baratinoo_engine.supported_version][VI_##member])

#endif /* ! BARATINOO_ABI_IS_STABLE_ENOUGH_FOR_ME */


/*------------------------ Speech-Dispatcher module ------------------------*/

#include "spd_audio.h"

#include <speechd_types.h>

#include "module_utils.h"

#define MODULE_NAME     "baratinoo"
#define DBG_MODNAME     "Baratinoo: "
#define MODULE_VERSION  "0.1"

#define DEBUG_MODULE 1
DECLARE_DEBUG();

typedef struct {
	/* Thread primitives */
	pthread_t thread;
	sem_t semaphore;

#ifndef BARATINOO_ABI_IS_STABLE_ENOUGH_FOR_ME
	SupportedBaratinooVersion supported_version;
#endif

	BCengine engine;
	/* The buffer consumed by the TTS engine.  It is NULL when the TTS
	 * thread is ready to accept new input.  Otherwise, the thread is in
	 * the process of synthesizing speech. */
	BCinputTextBuffer buffer;

	SPDVoice **voice_list;

	/* settings */
	int voice;

	/* request flags */
	gboolean close_requested;
} Engine;

/* engine and state */
static Engine baratinoo_engine = {
	.engine = NULL,
	.buffer = NULL,
	.voice_list = NULL,
	.voice = 0,
	.close_requested = FALSE
};

/* Internal functions prototypes */
static void *_baratinoo_speak(void *);
static SPDVoice **baratinoo_list_voices(BCengine *engine);
/* Parameters */
static void baratinoo_set_voice_type(SPDVoiceType voice);
static void baratinoo_set_language(char *lang);
static void baratinoo_set_synthesis_voice(char *synthesis_voice);
/* Engine callbacks */
static void baratinoo_trace_cb(BaratinooTraceLevel level, int engine_num, const char *source, const void *data, const char *format, va_list args);
static int baratinoo_output_signal(void *privateData, const void *address, int length);
/* SSML conversion functions */
static void append_ssml_as_proprietary(const Engine *engine, GString *buf, const char *data, gsize size);

/* Module configuration options */
MOD_OPTION_1_STR(BaratinooConfigPath);
MOD_OPTION_1_INT(BaratinooSampleRate);
MOD_OPTION_1_INT(BaratinooResponsiveness);
MOD_OPTION_1_INT(BaratinooQueueSize);
MOD_OPTION_1_INT(BaratinooMinRate);
MOD_OPTION_1_INT(BaratinooNormalRate);
MOD_OPTION_1_INT(BaratinooMaxRate);
MOD_OPTION_1_STR(BaratinooPunctuationList);
MOD_OPTION_1_STR(BaratinooIntonationList);
MOD_OPTION_1_STR(BaratinooNoIntonationList);

/* Public functions */

int module_load(void)
{
	const char *conf_env;
	char *default_config = NULL;

	INIT_SETTINGS_TABLES();

	REGISTER_DEBUG();

	/* BaratinooConfigPath default value comes from the environment or
	 * user XDG configuration location */
	conf_env = getenv("BARATINOO_CONFIG_PATH");
	if (conf_env && conf_env[0] != '\0') {
		default_config = g_strdup(conf_env);
	} else {
		default_config = g_build_filename(g_get_user_config_dir(),
						  "baratinoo.cfg", NULL);
	}
	MOD_OPTION_1_STR_REG(BaratinooConfigPath, default_config);
	g_free(default_config);

	/* Sample rate. 16000Hz is the voices default, not requiring resampling */
	MOD_OPTION_1_INT_REG(BaratinooSampleRate, 16000);

	/* Let Baratinoo handle by default */
	MOD_OPTION_1_INT_REG(BaratinooResponsiveness, -1);

	/* Default to 20s queuing */
	MOD_OPTION_1_INT_REG(BaratinooQueueSize, 20*BaratinooSampleRate);

	/* Speech rate */
	MOD_OPTION_1_INT_REG(BaratinooMinRate, -100);
	MOD_OPTION_1_INT_REG(BaratinooNormalRate, 0);
	MOD_OPTION_1_INT_REG(BaratinooMaxRate, 100);

	/* Punctuation */
	MOD_OPTION_1_STR_REG(BaratinooPunctuationList, "@/+-_");
	MOD_OPTION_1_STR_REG(BaratinooIntonationList, "?!;:,.");
	MOD_OPTION_1_STR_REG(BaratinooNoIntonationList, "");

	return 0;
}

#ifndef BARATINOO_ABI_IS_STABLE_ENOUGH_FOR_ME
static SupportedBaratinooVersion get_baratinoo_supported_version(void)
{
	const BaratinooVersionStruct *version = BCgetBaratinooVersionStruct();

	switch (version->major) {
		case 8: switch (version->minor) {
			case 1: return BV_8_1;
			case 4: return BV_8_4;
		} break;
	}

	return BV_UNSUPPORTED;
}
#endif

int module_init(char **status_info)
{
	Engine *engine = &baratinoo_engine;
	int ret;
	BARATINOOC_STATE state;

	DBG(DBG_MODNAME "Module init");
	INIT_INDEX_MARKING();

	DBG(DBG_MODNAME "BaratinooPunctuationList = %s", BaratinooPunctuationList);
	DBG(DBG_MODNAME "BaratinooIntonationList = %s", BaratinooIntonationList);
	DBG(DBG_MODNAME "BaratinooNoIntonationList = %s", BaratinooNoIntonationList);

	*status_info = NULL;

	engine->close_requested = FALSE;

	/* Init Baratinoo */
	if (BCinitlib(baratinoo_trace_cb) != BARATINOO_INIT_OK) {
		DBG(DBG_MODNAME "Failed to initialize library");
		*status_info = g_strdup("Failed to initialize Baratinoo. "
					"Make sure your installation is "
					"properly set up.");
		return -1;
	}
	DBG(DBG_MODNAME "Using Baratinoo %s", BCgetBaratinooVersion());

#ifndef BARATINOO_ABI_IS_STABLE_ENOUGH_FOR_ME
	engine->supported_version = get_baratinoo_supported_version();
	if (engine->supported_version == BV_UNSUPPORTED) {
		DBG(DBG_MODNAME "Unsupported library version");
		*status_info = g_strdup("Unsupported Baratinoo engine version.");
		return -1;
	}
	DBG(DBG_MODNAME "Using Baratinoo compatibility level %d", engine->supported_version);
#endif

	engine->engine = BCnew(NULL);
	if (!engine->engine) {
		DBG(DBG_MODNAME "Failed to allocate engine");
		*status_info = g_strdup("Failed to create Baratinoo engine.");
		return -1;
	}

	BCinit(engine->engine, BaratinooConfigPath);
	state = BCgetState(engine->engine);
	if (state != BARATINOO_INITIALIZED) {
		DBG(DBG_MODNAME "Failed to initialize engine");
		*status_info = g_strdup("Failed to initialize Baratinoo engine. "
					"Make sure your setup is OK.");
		return -1;
	}

	/* Find voices */
	engine->voice_list = baratinoo_list_voices(engine->engine);
	if (!engine->voice_list) {
		DBG(DBG_MODNAME "No voice available");
		*status_info = g_strdup("No voice found. Make sure your setup "
					"includes at least one voice.");
		return -1;
	}

	/* Setup output (audio) signal handling */
	DBG(DBG_MODNAME "Using PCM output at %dHz", BaratinooSampleRate);
	BCsetOutputSignal(engine->engine, baratinoo_output_signal, engine, BARATINOO_PCM, BaratinooSampleRate);
	if (BCgetState(engine->engine) != BARATINOO_INITIALIZED) {
		DBG(DBG_MODNAME "Failed to initialize output signal handler");
		*status_info = g_strdup("Failed to initialize Baratinoo output "
					"signal handler. Is the configured "
					"sample rate correct?");
		return -1;
	}

	BCsetWantedEvent(engine->engine, BARATINOO_MARKER_EVENT);

	/* Setup TTS thread */
	sem_init(&engine->semaphore, 0, 0);

	DBG(DBG_MODNAME "creating new thread for baratinoo_speak");
	ret = pthread_create(&engine->thread, NULL, _baratinoo_speak, engine);
	if (ret != 0) {
		DBG(DBG_MODNAME "thread creation failed");
		*status_info =
		    g_strdup("The module couldn't initialize threads. "
			     "This could be either an internal problem or an "
			     "architecture problem. If you are sure your architecture "
			     "supports threads, please report a bug.");
		return -1;
	}

	if (module_speak_queue_init(BaratinooQueueSize, status_info)) {
		DBG(DBG_MODNAME "queue creation failed");
		return -1;
	}

	DBG(DBG_MODNAME "Initialization successfully.");
	*status_info = g_strdup("Baratinoo initialized successfully.");

	return 0;
}

SPDVoice **module_list_voices(void)
{
	Engine *engine = &baratinoo_engine;

	return engine->voice_list;
}

int module_speak(gchar *data, size_t bytes, SPDMessageType msgtype)
{
	Engine *engine = &baratinoo_engine;
	GString *buffer = NULL;
	int rate;

	DBG(DBG_MODNAME "Speech requested");

	assert(msg_settings.rate >= -100 && msg_settings.rate <= +100);
	assert(msg_settings.pitch >= -100 && msg_settings.pitch <= +100);
	assert(msg_settings.pitch_range >= -100 && msg_settings.pitch_range <= +100);
	assert(msg_settings.volume >= -100 && msg_settings.volume <= +100);

	if (engine->buffer != NULL) {
		DBG(DBG_MODNAME "WARNING: module_speak() called during speech");
		return 0;
	}

	/* select voice following parameters.  we don't use tags for this as
	 * we need to do some computation on our end anyway and need pass an
	 * ID when creating the buffer too */
	/* NOTE: these functions access the engine, which wouldn't be safe if
	 *       we didn't know that the thread is sleeping.  But we do know it
	 *       is, as @c Engine::buffer is NULL */
	UPDATE_STRING_PARAMETER(voice.language, baratinoo_set_language);
	UPDATE_PARAMETER(voice_type, baratinoo_set_voice_type);
	UPDATE_STRING_PARAMETER(voice.name, baratinoo_set_synthesis_voice);

	engine->buffer = BCinputTextBufferNew(BARATINOO_PROPRIETARY_PARSING,
					      BARATINOO_UTF8, engine->voice, 0);
	if (!engine->buffer) {
		DBG(DBG_MODNAME "Failed to allocate input buffer");
		goto err;
	}

	buffer = g_string_new(NULL);

	/* Apply speech parameters */
	if (msg_settings.rate < 0)
		rate = BaratinooNormalRate + (BaratinooNormalRate - BaratinooMinRate) * msg_settings.rate / 100;
	else
		rate = BaratinooNormalRate + (BaratinooMaxRate - BaratinooNormalRate) * msg_settings.rate / 100;

	if (rate != 0) {
		g_string_append_printf(buffer, "\\rate{%+d%%}", rate);
	}
	if (msg_settings.pitch != 0 || msg_settings.pitch_range != 0) {
		g_string_append_printf(buffer, "\\pitch{%+d%% %+d%%}",
				       msg_settings.pitch,
				       msg_settings.pitch_range);
	}
	if (msg_settings.volume != 0) {
		g_string_append_printf(buffer, "\\volume{%+d%%}",
				       msg_settings.volume);
	}

	switch (msgtype) {
	case SPD_MSGTYPE_SPELL: /* FIXME: use \spell one day? */
	case SPD_MSGTYPE_CHAR:
		g_string_append(buffer, "\\sayas<{characters}");
		g_string_append_len(buffer, data, bytes);
		g_string_append(buffer, "\\sayas>{}");
		break;
	default: /* FIXME: */
	case SPD_MSGTYPE_TEXT:
		append_ssml_as_proprietary(engine, buffer, data, bytes);
		break;
	}

	DBG(DBG_MODNAME "SSML input: %s", data);
	DBG(DBG_MODNAME "Sending buffer: %s", buffer->str);
	if (!BCinputTextBufferInit(engine->buffer, buffer->str)) {
		DBG(DBG_MODNAME "Failed to initialize input buffer");
		goto err;
	}

	g_string_free(buffer, TRUE);

	sem_post(&engine->semaphore);

	DBG(DBG_MODNAME "leaving module_speak() normally");
	return bytes;

err:
	if (buffer)
		g_string_free(buffer, TRUE);
	if (engine->buffer) {
		BCinputTextBufferDelete(engine->buffer);
		engine->buffer = NULL;
	}

	return 0;
}

int module_stop(void)
{
	DBG(DBG_MODNAME "Stop requested");
	module_speak_queue_stop();

	return 0;
}

size_t module_pause(void)
{
	DBG(DBG_MODNAME "Pause requested");
	module_speak_queue_pause();

	return 0;
}

int module_close(void)
{
	Engine *engine = &baratinoo_engine;

	DBG(DBG_MODNAME "close()");

	DBG(DBG_MODNAME "Terminating threads");

	module_speak_queue_terminate();

	/* Politely ask the thread to terminate */
	engine->close_requested = TRUE;
	sem_post(&engine->semaphore);
	/* ...and give it a chance to actually quit. */
	g_usleep(25000);

	/* Make sure the thread has really exited */
	pthread_cancel(engine->thread);
	DBG(DBG_MODNAME "Joining threads.");
	if (pthread_join(engine->thread, NULL) != 0)
		DBG(DBG_MODNAME "Failed to join threads.");

	sem_destroy(&engine->semaphore);

	/* destroy voice list */
	if (engine->voice_list != NULL) {
		int i;
		for (i = 0; engine->voice_list[i] != NULL; i++) {
			g_free(engine->voice_list[i]->name);
			g_free(engine->voice_list[i]->language);
			g_free(engine->voice_list[i]->variant);
			g_free(engine->voice_list[i]);
		}
		g_free(engine->voice_list);
		engine->voice_list = NULL;
	}

	/* destroy engine */
	if (engine->engine) {
	    BCdelete(engine->engine);
	    engine->engine = NULL;
	}

	/* uninitialize */
	BCterminatelib();

	module_speak_queue_free();

	DBG(DBG_MODNAME "Module closed.");

	return 0;
}

/* Internal functions */

/**
 * @brief Lists voices in SPD format
 * @param engine An engine.
 * @returns A NULL-terminated list of @c SPDVoice, or NULL if no voice found.
 */
static SPDVoice **baratinoo_list_voices(BCengine *engine)
{
    SPDVoice **voices;
    int n_voices;
    int i;

    n_voices = BCgetNumberOfVoices(engine);
    if (n_voices < 1)
	return NULL;

    voices = g_malloc_n(n_voices + 1, sizeof *voices);
    DBG(DBG_MODNAME "Got %d available voices:", n_voices);
    for (i = 0; i < n_voices; i++) {
	SPDVoice *voice;
	const char *language;
	BaratinooVoiceInfo voice_info_DO_NO_ACCESS_DIRECTLY = BCgetVoiceInfo(engine, i);
	void *voice_info = &voice_info_DO_NO_ACCESS_DIRECTLY;

	DBG(DBG_MODNAME "\tVoice #%d: name=%s, language=%s, gender=%s",
	    i, VOICE_INFO_MEMBER(char *, voice_info, name),
	       VOICE_INFO_MEMBER(char *, voice_info, language),
	       VOICE_INFO_MEMBER(char *, voice_info, gender));

	voice = g_malloc0(sizeof *voice);
	voice->name = g_strdup(VOICE_INFO_MEMBER(char *, voice_info, name));

	language = VOICE_INFO_MEMBER(char *, voice_info, language);
	voice->language = g_strdup(language);

	voices[i] = voice;
    }
    voices[i] = NULL;

    return voices;
}

void module_speak_queue_cancel(void)
{
	/* We will stop the synth from _baratinoo_speak */
}

/**
 * @brief Internal TTS thread.
 * @param data An Engine structure.
 * @returns NULL.
 *
 * The TTS thread.  It waits on @c Engine::semaphore to consume input data
 * from @c Engine::buffer.
 *
 * @see Engine::close_requested
 */
static void *_baratinoo_speak(void *data)
{
	Engine *engine = data;
	BARATINOOC_STATE state = BARATINOO_READY;

	set_speaking_thread_parameters();

	while (!engine->close_requested) {
		sem_wait(&engine->semaphore);
		DBG(DBG_MODNAME "Semaphore on");

		if (!engine->buffer)
			continue;

		state = BCinputTextBufferSetInEngine(engine->buffer, engine->engine);
		if (state != BARATINOO_READY) {
			DBG(DBG_MODNAME "Failed to set input buffer");
			goto cont;
		}

		if (!module_speak_queue_before_synth())
			goto cont;

		if (module_speak_queue_stop_requested() || engine->close_requested) {
			DBG(DBG_MODNAME "Stop in child, terminating");
			goto cont;
		}

		module_speak_queue_before_play();
		do {
			state = BCprocessLoop(engine->engine, BaratinooResponsiveness);
			if (state == BARATINOO_EVENT) {
				BaratinooEvent event = BCgetEvent(engine->engine);
				if (event.type == BARATINOO_MARKER_EVENT) {
					DBG(DBG_MODNAME "Reached mark '%s' at sample %lu", event.data.marker.name, event.sampleStamp);
					module_speak_queue_add_mark(event.data.marker.name);
				}
			} else if (state == BARATINOO_INPUT_ERROR ||
				   state == BARATINOO_ENGINE_ERROR) {
				module_speak_queue_stop();
			}
		} while (state == BARATINOO_RUNNING || state == BARATINOO_EVENT);

		if (module_speak_queue_stop_requested() || engine->close_requested) {
			DBG(DBG_MODNAME "Stop in child, terminating");
		} else {
			DBG(DBG_MODNAME "Finished synthesizing");
			module_speak_queue_add_end();
		}

cont:
		BCinputTextBufferDelete(engine->buffer);
		engine->buffer = NULL;
	}


	DBG(DBG_MODNAME "leaving thread with state=%d", state);

	pthread_exit(NULL);
}

/* Voice selection */

/**
 * @brief Matches a Baratinoo voice info against a SPD language
 * @param info A voice info to match.
 * @param lang A SPD language to match against.
 * @returns The quality of the match: the higher the better.
 *
 * Gives a score to a voice based on its compatibility with @p lang.
 */
static int lang_match_level(const void *vinfo, const char *lang)
{
	int level = 0;
	const char *language = VOICE_INFO_MEMBER(char *, vinfo, language);
	const char *iso639 = VOICE_INFO_MEMBER(char *, vinfo, iso639);
	const char *iso3166 = VOICE_INFO_MEMBER(char *, vinfo, iso3166);

	if (g_ascii_strcasecmp(lang, language) == 0)
		level += 10;
	else {
		gchar **a = g_strsplit_set(language, "-", 2);
		gchar **b = g_strsplit_set(lang, "-", 2);

		/* language */
		if (g_ascii_strcasecmp(a[0], b[0]) == 0)
			level += 8;
		else if (g_ascii_strcasecmp(iso639, b[0]) == 0)
			level += 8;
		else if (g_ascii_strncasecmp(a[0], b[0], 2) == 0)
			level += 5; /* partial match */
		/* region */
		if (a[1] && b[1] && g_ascii_strcasecmp(a[1], b[1]) == 0)
			level += 2;
		else if (b[1] && g_ascii_strcasecmp(iso3166, b[1]) == 0)
			level += 2;
		else if (a[1] && b[1] && g_ascii_strncasecmp(a[1], b[1], 2) == 0)
			level += 1; /* partial match */

		g_strfreev(a);
		g_strfreev(b);
	}

	DBG(DBG_MODNAME "lang_match_level({language=%s, iso639=%s, iso3166=%s}, lang=%s) = %d",
	    language, iso639, iso3166, lang, level);

	return level;
}

/**
 * @brief Sort two Baratinoo voices by SPD criteria.
 * @param a A voice info.
 * @param b Another voice info.
 * @param lang A SPD language.
 * @param voice_code A SPD voice code.
 * @returns < 0 if @p a is best, > 0 if @p b is best, and 0 if they are equally
 *          matching.  Larger divergence from 0 means better match.
 */
static int sort_voice(const void *voice_a, const void *voice_b, const char *lang, SPDVoiceType voice_code)
{
	int cmp = 0;
	const char *a_gender = VOICE_INFO_MEMBER(char *, voice_a, gender);
	const char *b_gender = VOICE_INFO_MEMBER(char *, voice_b, gender);
	int a_age = VOICE_INFO_MEMBER(int, voice_a, age);
	int b_age = VOICE_INFO_MEMBER(int, voice_b, age);

	cmp -= lang_match_level(voice_a, lang);
	cmp += lang_match_level(voice_b, lang);

	if (strcmp(a_gender, b_gender) != 0) {
		const char *gender;

		switch (voice_code) {
		default:
		case SPD_MALE1:
		case SPD_MALE2:
		case SPD_MALE3:
		case SPD_CHILD_MALE:
			gender = "male";
			break;

		case SPD_FEMALE1:
		case SPD_FEMALE2:
		case SPD_FEMALE3:
		case SPD_CHILD_FEMALE:
			gender = "female";
			break;
		}

		if (strcmp(gender, a_gender) == 0)
			cmp--;
		if (strcmp(gender, b_gender) == 0)
			cmp++;
	}

	switch (voice_code) {
	case SPD_CHILD_MALE:
	case SPD_CHILD_FEMALE:
		if (a_age && a_age <= 15)
			cmp--;
		if (b_age && b_age <= 15)
			cmp++;
		break;
	default:
		/* we expect mostly adult voices, so only compare if age is set */
		if (a_age && b_age) {
			if (a_age > 15)
				cmp--;
			if (b_age > 15)
				cmp++;
		}
		break;
	}

	DBG(DBG_MODNAME "Comparing %s <> %s gives %d",
			VOICE_INFO_MEMBER(char*, voice_a, name),
			VOICE_INFO_MEMBER(char*, voice_b, name),
			cmp);

	return cmp;
}

/* Given a language code and SD voice code, gets the Baratinoo voice. */
static int baratinoo_find_voice(const Engine *engine, const char *lang, SPDVoiceType voice_code)
{
	int i;
	int best_match = -1;
	int nth_match = 0;
	int offset = 0; /* nth voice we'd like */
	BaratinooVoiceInfo best_info;

	DBG(DBG_MODNAME "baratinoo_find_voice(lang=%s, voice_code=%d)",
	    lang, voice_code);

	switch (voice_code) {
	case SPD_MALE3:
	case SPD_FEMALE3:
		offset++;
		/* FALLTHRU */
	case SPD_MALE2:
	case SPD_FEMALE2:
		offset++;
		/* FALLTHRU */
	default:
		break;
	}

	for (i = 0; i < BCgetNumberOfVoices(engine->engine); i++) {
		if (i == 0) {
			best_match = i;
			best_info = BCgetVoiceInfo(engine->engine, i);
			nth_match++;
		} else {
			BaratinooVoiceInfo info = BCgetVoiceInfo(engine->engine, i);
			int cmp = sort_voice(&best_info, &info, lang, voice_code);

			if (cmp >= 0) {
				if (cmp > 0)
					nth_match = 0;
				if (nth_match <= offset) {
					best_match = i;
					best_info = info;
				}
				nth_match++;
			}
		}
	}

	return best_match;
}

/* Given a language code and SD voice code, sets the voice. */
static void baratinoo_set_language_and_voice(Engine *engine, const char *lang, SPDVoiceType voice_code)
{
	int voice = baratinoo_find_voice(engine, lang, voice_code);

	if (voice < 0) {
		DBG(DBG_MODNAME "No voice match found, not changing voice.");
	} else {
		DBG(DBG_MODNAME "Best voice match is %d.", voice);
		engine->voice = voice;
	}
}

/* UPDATE_PARAMETER callback to set the voice type */
static void baratinoo_set_voice_type(SPDVoiceType voice)
{
	Engine *engine = &baratinoo_engine;

	assert(msg_settings.voice.language);
	baratinoo_set_language_and_voice(engine, msg_settings.voice.language, voice);
}

/* UPDATE_PARAMETER callback to set the voice language */
static void baratinoo_set_language(char *lang)
{
	Engine *engine = &baratinoo_engine;

	baratinoo_set_language_and_voice(engine, lang, msg_settings.voice_type);
}

/* UPDATE_PARAMETER callback to set the voice by name */
static void baratinoo_set_synthesis_voice(char *synthesis_voice)
{
	Engine *engine = &baratinoo_engine;
	int i;

	if (synthesis_voice == NULL)
		return;

	for (i = 0; i < BCgetNumberOfVoices(engine->engine); i++) {
		BaratinooVoiceInfo info = BCgetVoiceInfo(engine->engine, i);

		if (g_ascii_strcasecmp(synthesis_voice, VOICE_INFO_MEMBER(char*, &info, name)) == 0) {
			engine->voice = i;
			return;
		}
	}

	DBG(DBG_MODNAME "Failed to set synthesis voice to '%s': not found.",
	    synthesis_voice);
}

/* Engine callbacks */

/**
 * @brief Logs a message from Baratinoo
 * @param level Message importance.
 * @param engine_num ID of the engine that emitted the message, or 0 if it is a
 *                   library message.
 * @param source Message category.
 * @param data Private data, unused.
 * @param format printf-like @p format.
 * @param args arguments for @p format.
 */
static void baratinoo_trace_cb(BaratinooTraceLevel level, int engine_num, const char *source, const void *data, const char *format, va_list args)
{
	const char *prefix = "";

	if (!Debug) {
		switch (level) {
		case BARATINOO_TRACE_INIT:
		case BARATINOO_TRACE_INFO:
		case BARATINOO_TRACE_DEBUG:
			return;
		default:
			break;
		}
	}

	switch (level) {
	case BARATINOO_TRACE_ERROR:
		prefix = "ERROR";
		break;
	case BARATINOO_TRACE_INIT:
		prefix = "INIT";
		break;
	case BARATINOO_TRACE_WARNING:
		prefix = "WARNING";
		break;
	case BARATINOO_TRACE_INFO:
		prefix = "INFO";
		break;
	case BARATINOO_TRACE_DEBUG:
		prefix = "DEBUG";
		break;
	}

	if (engine_num == 0)
		fprintf(stderr, "Baratinoo library: ");
	else
		fprintf(stderr, "Baratinoo engine #%d: ", engine_num);
	fprintf(stderr, "%s: %s ", prefix, source);
	vfprintf(stderr, format, args);
	fprintf(stderr, "\n");
}

/**
 * @brief Output (sound) callback
 * @param private_data An Engine structure.
 * @param address Audio samples.
 * @param length Length of @p address, in bytes.
 * @returns Whether to break out of the process loop.
 *
 * Called by the engine during speech synthesis.
 *
 * @see BCprocessLoop()
 */
static int baratinoo_output_signal(void *private_data, const void *address, int length)
{
	/* If stop is requested during synthesis, abort here to stop speech as
	 * early as possible, even if the engine didn't finish its cycle yet. */
	if (module_speak_queue_stop_requested())
	{
		DBG(DBG_MODNAME "Not playing message because it got stopped");
		return 1;
	}

	/* Engine *engine = private_data; */
	AudioTrack track;
#if defined(BYTE_ORDER) && (BYTE_ORDER == BIG_ENDIAN)
	AudioFormat format = SPD_AUDIO_BE;
#else
	AudioFormat format = SPD_AUDIO_LE;
#endif

	/* We receive 16 bits PCM data */
	track.num_samples = length / 2; /* 16 bits per sample = 2 bytes */
	track.num_channels = 1;
	track.sample_rate = BaratinooSampleRate;
	track.bits = 16;
	track.samples = (short *) address;

	DBG(DBG_MODNAME "Queueing %d samples", length / 2);
	module_speak_queue_add_audio(&track, format);

	return module_speak_queue_stop_requested();
}

/* SSML conversion functions */

typedef struct {
	const Engine *engine;
	GString *buffer;
	/* Voice ID stack for the current element */
	int voice_stack[32];
	unsigned int voice_stack_len;
} SsmlPraserState;

/* Adds a language change command for @p lang if appropriate */
static void ssml2baratinoo_push_lang(SsmlPraserState *state, const char *lang)
{
	int voice;

	if (state->voice_stack_len > 0)
		voice = state->voice_stack[state->voice_stack_len - 1];
	else
		voice = state->engine->voice;

	if (lang) {
		DBG(DBG_MODNAME "Processing xml:lang=\"%s\"", lang);
		int new_voice = baratinoo_find_voice(&baratinoo_engine, lang,
						     msg_settings.voice_type);
		if (new_voice >= 0 && new_voice != voice) {
			g_string_append_printf(state->buffer, "\\vox{%d}", new_voice);
			voice = new_voice;
		}
	}

	if (state->voice_stack_len >= G_N_ELEMENTS(state->voice_stack)) {
		DBG(DBG_MODNAME "WARNING: voice stack exhausted, expect incorrect voices.");
	} else {
		state->voice_stack[state->voice_stack_len++] = voice;
	}
}

/* Pops a language pushed with @c ssml2baratinoo_push_lang() */
static void ssml2baratinoo_pop_lang(SsmlPraserState *state)
{
	if (state->voice_stack_len > 0) {
		int cur_voice = state->voice_stack[--state->voice_stack_len];

		if (state->voice_stack_len > 0) {
			int new_voice = state->voice_stack[state->voice_stack_len - 1];

			if (new_voice != cur_voice)
				g_string_append_printf(state->buffer, "\\vox{%d}", new_voice);
		}
	}
}

/* locates a string in a NULL-terminated array of strings
 * Returns -1 if not found, the index otherwise. */
static int attribute_index(const char **names, const char *name)
{
	int i;

	for (i = 0; names && names[i] != NULL; i++) {
		if (strcmp(names[i], name) == 0)
			return i;
	}

	return -1;
}

/* Markup element start callback */
static void ssml2baratinoo_start_element(GMarkupParseContext *ctx,
					 const gchar *element,
					 const gchar **attribute_names,
					 const gchar **attribute_values,
					 gpointer data, GError **error)
{
	SsmlPraserState *state = data;
	int lang_id;

	/* handle voice changes */
	lang_id = attribute_index(attribute_names, "xml:lang");
	ssml2baratinoo_push_lang(state, lang_id < 0 ? NULL : attribute_values[lang_id]);

	/* handle elements */
	if (strcmp(element, "mark") == 0) {
		int i = attribute_index(attribute_names, "name");
		g_string_append_printf(state->buffer, "\\mark{%s}",
				       i < 0 ? "" : attribute_values[i]);
	} else if (strcmp(element, "emphasis") == 0) {
		int i = attribute_index(attribute_names, "level");
		g_string_append_printf(state->buffer, "\\emph<{%s}",
				       i < 0 ? "" : attribute_values[i]);
	} else if (strcmp(element, "say-as") == 0) {
		int i_as = attribute_index(attribute_names, "interpret-as");
		int i_fmt = attribute_index(attribute_names, "format");
		int i_detail = attribute_index(attribute_names, "detail");

		if (i_as < 0) {
			DBG(DBG_MODNAME "Missing required 'interpret-as' attribute of '<say-as>' tag");
			i_fmt = i_detail = -1;
		} else if (i_fmt < 0 && i_detail >= 0) {
			DBG(DBG_MODNAME "Ignoring 'detail' attribute of '<say-as>' tag because it is "
					"not supported without a 'format' attribute");
			i_detail = -1;
		}

		g_string_append_printf(state->buffer, "\\sayas<{%s %s %s}",
				       i_as < 0 ? "" : attribute_values[i_as],
				       i_fmt < 0 ? "" : attribute_values[i_fmt],
				       i_detail < 0 ? "" : attribute_values[i_detail]);
	} else {
		/* ignore other elements */
		/* TODO: handle more elements */
	}
}

/* Markup element end callback */
static void ssml2baratinoo_end_element(GMarkupParseContext *ctx,
				       const gchar *element,
				       gpointer data, GError **error)
{
	SsmlPraserState *state = data;

	if (strcmp(element, "emphasis") == 0) {
		g_string_append(state->buffer, "\\emph>{}");
	} else if (strcmp(element, "say-as") == 0) {
		g_string_append(state->buffer, "\\sayas>{}");
	}

	ssml2baratinoo_pop_lang(state);
}

/* Markup text node callback.
 *
 * This not only converts to the proprietary format (by escaping things that
 * would be interpreted by it), but also pre-processes the text for some
 * features that are missing from Baratinoo.
 *
 * - Punctuation speaking
 *
 * As the engine doesn't support speaking of the punctuation itself, we alter
 * the input to explicitly tell the engine to do it.  It is kind of tricky,
 * because we want to keep the punctuation meaning of the characters, e.g. how
 * they affect speech as means of intonation and pauses.
 *
 * The approach here is that for every punctuation character included in the
 * selected mode (none/some/all), we wrap it in "\sayas<{characters}" markup
 * so that it is spoken by the engine.  But in order to keep the punctuation
 * meaning of the character, in case it has some, we duplicate it outside the
 * markup with a heuristic on whether it will or not affect speech intonation
 * and pauses, and whether or not the engine would speak the character itself
 * already (as we definitely don't want to get duplicated speech for a
 * character).
 * This heuristic is as follows:
 *   - If the character is listed in BaratinooIntonationList and the next
 *     character is not punctuation or alphanumeric, duplicate the character.
 *   - Always append a space after a duplicated character, hoping the engine
 *     won't consider speaking it.
 *
 * This won't always give the same results as the engine would by itself, but
 * it isn't really possible as the engine behavior is language-dependent in a
 * non-obvious fashion.  For example, a French voice will speak "1.2.3" as
 * "Un. Deux. Trois", while an English one will speak it as "One dot two dot
 * three": the dot here didn't have the same interpretation, and wasn't spoken
 * the same (once altering the voice, the other spoken plain and simple).
 *
 * However, the heuristic here should be highly unlikely to lead to duplicate
 * character speaking, and catch most of the intonation and pause cases.
 *
 * - Why is this done that way?
 *
 * Another, possibly more robust, approach could be using 2 passes in the
 * engine itself, and relying on events to get information on how the engine
 * interprets the input in the first (silent) pass, and alter it as needed for
 * a second (spoken) pass.  This wouldn't guarantee the altered input would be
 * interpreted the same, but it would seem like a safe enough bet.
 *
 * However, the engine is too slow for this to be viable in a real-time
 * processing environment for anything but tiny input.  Even about 25 lines of
 * IRC conversation can easily take several seconds to process in the first
 * pass (even without doing any actual pre-processing on our end), delaying
 * the actual speech by an unacceptable amount of time.
 *
 * Ideally, the engine will some day support speaking punctuation itself, and
 * this part of the pre-processing could be dropped.
 */
static void ssml2baratinoo_text(GMarkupParseContext *ctx,
				const gchar *text, gsize len,
				gpointer data, GError **error)
{
	SsmlPraserState *state = data;
	const gchar *p;

	for (p = text; p < (text + len); p = g_utf8_next_char(p)) {
		if (*p == '\\') {
			/* escape the \ by appending a comment so it won't be
			 * interpreted as a command */
			g_string_append(state->buffer, "\\\\{}");
		} else {
			gboolean say_as_char, do_not_say;
			gunichar ch = g_utf8_get_char(p);

			/* if punctuation mode is not NONE and the character
			 * should be spoken, manually wrap it with \sayas */
			say_as_char = ((msg_settings.punctuation_mode == SPD_PUNCT_SOME &&
					g_utf8_strchr(BaratinooPunctuationList, -1, ch)) ||
				       (msg_settings.punctuation_mode == SPD_PUNCT_ALL &&
					g_unichar_ispunct(ch)));
			do_not_say = ((msg_settings.punctuation_mode == SPD_PUNCT_NONE &&
					g_utf8_strchr(BaratinooNoIntonationList, -1, ch)));

			if (say_as_char)
				g_string_append(state->buffer, "\\sayas<{characters}");
			if (!do_not_say)
				g_string_append_unichar(state->buffer, ch);
			if (say_as_char) {
				g_string_append(state->buffer, "\\sayas>{}");

				/* if the character should influence intonation,
				 * add it back, but *only* if it wouldn't be spoken */
				if (g_utf8_strchr(BaratinooIntonationList, -1, ch)) {
					const gchar *next = g_utf8_next_char(p);
					gunichar ch_next;

					if (next < text + len)
						ch_next = g_utf8_get_char(next);
					else
						ch_next = '\n';

					if (!g_unichar_isalnum(ch_next) &&
					    !g_unichar_ispunct(ch_next)) {
						g_string_append_unichar(state->buffer, ch);
						/* Append an extra space to try and
						 * make sure it's considered as
						 * punctuation and isn't spoken. */
						g_string_append_c(state->buffer, ' ');
					}
				}
			}
		}
	}
}

/**
 * @brief Converts SSML data to Baratinoo's proprietary format.
 * @param buf A buffer to write to.
 * @param data SSML data to convert.
 * @param size Length of @p data
 *
 * @warning Only a subset of the input SSML is currently translated, the rest
 *          being discarded.
 */
static void append_ssml_as_proprietary(const Engine *engine, GString *buf, const char *data, gsize size)
{
	/* FIXME: we could possibly use SSML mode, but the Baratinoo parser is
	 * very strict and *requires* "xmlns", "version" and "lang" attributes
	 * on the <speak> tag, which speech-dispatcher doesn't provide.
	 *
	 * Moreover, we need to add tags for volume/rate/pitch so we'd have to
	 * amend the data anyway. */
	static const GMarkupParser parser = {
		.start_element = ssml2baratinoo_start_element,
		.end_element = ssml2baratinoo_end_element,
		.text = ssml2baratinoo_text,
	};
	SsmlPraserState state = {
		.engine = engine,
		.buffer = buf,
		.voice_stack_len = 0,
	};
	GMarkupParseContext *ctx;
	GError *err = NULL;

	ctx = g_markup_parse_context_new(&parser, G_MARKUP_TREAT_CDATA_AS_TEXT,
					 &state, NULL);
	if (!g_markup_parse_context_parse(ctx, data, size, &err) ||
	    !g_markup_parse_context_end_parse(ctx, &err)) {
		DBG(DBG_MODNAME "Failed to convert SSML: %s", err->message);
		g_error_free(err);
	}

	g_markup_parse_context_free(ctx);
}
