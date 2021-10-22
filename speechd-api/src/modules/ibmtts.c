/*
 * ibmtts.c - Speech Dispatcher backend for IBM TTS / Voxin
 *
 * Copyright (C) 2006, 2007 Brailcom, o.p.s.
 * Copyright (C) 2020 Gilles Casse <gcasse@oralux.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * @author  Gary Cramblitt <garycramblitt@comcast.net> (original author)
 *
 * $Id: ibmtts.c,v 1.30 2008-06-30 14:34:02 gcasse Exp $
 */

/* TODO:
   - Support list_synthesis_voices()
   - Limit amount of waveform data synthesised in advance.
   - Use SSML mark feature of ibmtts instead of handcrafted parsing.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* System includes. */
#include <string.h>
#include <glib.h>
#include <ctype.h>

#ifdef VOXIN
/* Voxin include */
#include "voxin.h"
#else
/* IBM Eloquence Command Interface. */
#include <eci.h>
#endif

/* Speech Dispatcher includes. */
#include <speechd_types.h>
#include "module_utils.h"

typedef enum {
	MODULE_FATAL_ERROR = -1,
	MODULE_OK = 0,
	MODULE_ERROR = 1
} module_status;

/* TODO: These defines are in src/server/index_marking.h, but including that
         file here causes a redefinition error on FATAL macro in speechd.h. */

#define SD_SPEAK "<speak>"
#define SD_ENDSPEAK "</speak>"

#define SD_MARK_HEAD_ONLY "<mark name=\""
#define SD_MARK_HEAD_ONLY2 "<mark name='"
#define SD_MARK_TAIL "\"/>"
#define SD_MARK_TAIL2 "'/>"
#define SD_MARK_TAILTAIL ">"
#define SD_MARK_HEAD_ONLY_LEN 12
#define SD_MARK_TAIL_LEN 3

#ifdef VOXIN
#define MODULE_NAME     "voxin"
#define DBG_MODNAME     "Voxin: "
#else
#define MODULE_NAME     "ibmtts"
#define DBG_MODNAME     "Ibmtts: "
#endif
#define MODULE_VERSION  "0.2"

#define DEBUG_MODULE 1
DECLARE_DEBUG();

/* Define a hash table where each entry is a double-linked list
   loaded from the config file.  Each entry in the config file
   is 3 strings, where the 1st string is used to access a list
   of the 2nd and 3rd strings. */
#define MOD_OPTION_3_STR_HT_DLL(name, arg1, arg2, arg3) \
	typedef struct{ \
		char* arg2; \
		char* arg3; \
	}T ## name; \
	GHashTable *name; \
	\
	DOTCONF_CB(name ## _cb) \
	{ \
		T ## name *new_item; \
		char *new_key; \
		GList *dll = NULL; \
		new_item = (T ## name *) g_malloc(sizeof(T ## name)); \
		new_key = g_strdup(cmd->data.list[0]); \
		if (NULL != cmd->data.list[1]) \
			new_item->arg2 = g_strdup(cmd->data.list[1]); \
		else \
			new_item->arg2 = NULL; \
		if (NULL != cmd->data.list[2]) \
			new_item->arg3 = g_strdup(cmd->data.list[2]); \
		else \
			new_item->arg3 = NULL; \
		dll = g_hash_table_lookup(name, new_key); \
		dll = g_list_append(dll, new_item); \
		g_hash_table_insert(name, new_key, dll); \
		return NULL; \
	}

/* Load a double-linked list from config file. */
#define MOD_OPTION_HT_DLL_REG(name) \
	name = g_hash_table_new(g_str_hash, g_str_equal); \
	module_dc_options = module_add_config_option(module_dc_options, \
	                    &module_num_dc_options, #name, \
	                    ARG_LIST, name ## _cb, NULL, 0);

/* Define a hash table mapping a string to 7 integer values. */
#define MOD_OPTION_6_INT_HT(name, arg1, arg2, arg3, arg4, arg5, arg6, arg7) \
	typedef struct{ \
		int arg1; \
		int arg2; \
		int arg3; \
		int arg4; \
		int arg5; \
		int arg6; \
		int arg7; \
	}T ## name; \
	GHashTable *name; \
	\
	DOTCONF_CB(name ## _cb) \
	{ \
		T ## name *new_item; \
		char* new_key; \
		new_item = (T ## name *) g_malloc(sizeof(T ## name)); \
		if (cmd->data.list[0] == NULL) return NULL; \
		new_key = g_strdup(cmd->data.list[0]); \
		new_item->arg1 = (int) strtol(cmd->data.list[1], NULL, 10); \
		new_item->arg2 = (int) strtol(cmd->data.list[2], NULL, 10); \
		new_item->arg3 = (int) strtol(cmd->data.list[3], NULL, 10); \
		new_item->arg4 = (int) strtol(cmd->data.list[4], NULL, 10); \
		new_item->arg5 = (int) strtol(cmd->data.list[5], NULL, 10); \
		new_item->arg6 = (int) strtol(cmd->data.list[6], NULL, 10); \
		new_item->arg7 = (int) strtol(cmd->data.list[7], NULL, 10); \
		g_hash_table_insert(name, new_key, new_item); \
		return NULL; \
	}

static gboolean stop_requested = FALSE;
static gboolean pause_requested = FALSE;
static gboolean pause_index_sent = FALSE;

/* ECI */
static ECIHand eciHandle = NULL_ECI_HAND;
static int eci_sample_rate = 0;

/* ECI sends audio back in chunks to this buffer.
   The smaller the buffer, the higher the overhead, but the better
   the index mark resolution. */
typedef signed short int TEciAudioSamples;
static TEciAudioSamples *audio_chunk;

/* For some reason, these were left out of eci.h. */
typedef enum {
	eciTextModeDefault = 0,
	eciTextModeAlphaSpell = 1,
	eciTextModeAllSpell = 2,
	eciIRCSpell = 3
} ECITextMode;

/* A lookup table for index mark name given integer id. */
static GHashTable *index_mark_ht = NULL;
#define MSG_END_MARK 0

/* When a voice is set, this is the baseline pitch of the voice.
   SSIP PITCH commands then adjust relative to this. */
static int voice_pitch_baseline;
/* When a voice is set, this the default speed of the voice.
   SSIP RATE commands then adjust relative to this. */
static int voice_speed;

/* Expected input encoding for current language dialect. */
#ifdef VOXIN
static char *input_encoding = "utf-8";
#else
static char *input_encoding = "cp1252";
#endif

/* list of speechd voices */
static SPDVoice **speechd_voice = NULL;
#ifdef VOXIN
#define voice_index(i) i
#else
static int *speechd_voice_index = NULL;
#define voice_index(i) speechd_voice_index[i]
#endif

/* Internal function prototypes. */
static void update_sample_rate();
static void set_language(char *lang);
static void set_voice_type(SPDVoiceType voice_type);
static char *voice_enum_to_str(SPDVoiceType voice);
static void set_language_and_voice(char *lang, SPDVoiceType voice_type, char *name);
static void set_synthesis_voice(char *);
static void set_rate(signed int rate);
static void set_pitch(signed int pitch);
static void set_punctuation_mode(SPDPunctuation punct_mode);
static void set_volume(signed int pitch);
static void set_capital_mode(SPDCapitalLetters cap_mode);

/* locale_index_atomic stores the current index of the voices or eciLocales array. */
static gint locale_index_atomic;

/* Internal function prototypes. */
static char *extract_mark_name(char *mark);
static char *next_part(char *msg, char **mark_name);
static int replace(char *from, char *to, GString * msg);
static void subst_keys_cb(gpointer data, gpointer user_data);
static char *subst_keys(char *key);
static char *search_for_sound_icon(const char *icon_name);
static gboolean add_sound_icon_to_playback_queue(char *filename);
static void load_user_dictionary();

static enum ECICallbackReturn eciCallback(ECIHand hEngine,
					  enum ECIMessage msg,
					  long lparam, void *data);

/* Internal function prototypes. */
static gboolean add_audio_to_playback_queue(TEciAudioSamples *
						      audio_chunk,
						      long num_samples);
static void add_mark_to_playback_queue(long markId);

/* Miscellaneous internal function prototypes. */
static void log_eci_error();
static gboolean alloc_voice_list();
static void free_voice_list();

/* The synthesis routine. */
static void _synth(char *message, SPDMessageType message_type);

/* Module configuration options. */
MOD_OPTION_1_INT(IbmttsUseSSML);
MOD_OPTION_1_INT(IbmttsUsePunctuation);
MOD_OPTION_1_STR(IbmttsPunctuationList);
MOD_OPTION_1_INT(IbmttsUseAbbreviation);
MOD_OPTION_1_STR(IbmttsDictionaryFolder);
MOD_OPTION_1_INT(IbmttsAudioChunkSize);
MOD_OPTION_1_STR(IbmttsSoundIconFolder);
MOD_OPTION_6_INT_HT(IbmttsVoiceParameters,
		    gender, breathiness, head_size, pitch_baseline,
		    pitch_fluctuation, roughness, speed);
MOD_OPTION_3_STR_HT_DLL(IbmttsKeySubstitution, lang, key, newkey);

#ifdef VOXIN
/* Array of installed voices returned by voxGetVoices() */
static vox_t *voices;
static unsigned int number_of_voices;
#define MAX_NB_OF_LANGUAGES number_of_voices
#else
typedef struct _eciLocale {
	char *name;
	char *lang;
	char *variant;
	enum ECILanguageDialect langID;
	char *charset;
} eciLocale, *eciLocaleList;

static eciLocale eciLocales[] = {
	{"American_English", "en-US", NULL, eciGeneralAmericanEnglish, "ISO-8859-1"},
	{"British_English", "en-GB", NULL, eciBritishEnglish, "ISO-8859-1"},
	{"Castilian_Spanish", "es-ES", NULL, eciCastilianSpanish, "ISO-8859-1"},
	{"Mexican_Spanish", "es-MX", NULL, eciMexicanSpanish, "ISO-8859-1"},
	{"French", "fr-FR", NULL, eciStandardFrench, "ISO-8859-1"},
	{"Canadian_French", "fr-CA", NULL, eciCanadianFrench, "ISO-8859-1"},
	{"German", "de-DE", NULL, eciStandardGerman, "ISO-8859-1"},
	{"Italian", "it-IT", NULL, eciStandardItalian, "ISO-8859-1"},
	{"Mandarin_Chinese UCS", "zh-CN", "UCS2", eciMandarinChineseUCS, "UCS2"},
	{"Mandarin_Chinese", "zh-CN", NULL, eciMandarinChinese, "GBK"},
	{"Mandarin_Chinese GB", "zh-CN", "GB", eciMandarinChineseGB, "GBK"},
	{"Mandarin_Chinese PinYin", "zh-CN", "PinYin", eciMandarinChinesePinYin, "GBK"},
	{"Taiwanese_Mandarin UCS", "zh-TW", "UCS", eciTaiwaneseMandarinUCS, "UCS2"},
	{"Taiwanese_Mandarin", "zh-TW", NULL, eciTaiwaneseMandarin, "BIG5"},
	{"Taiwanese_Mandarin Big 5", "zh-TW", "Big5", eciTaiwaneseMandarinBig5, "BIG5"},
	{"Taiwanese_Mandarin ZhuYin", "zh-TW", "ZhuYin", eciTaiwaneseMandarinZhuYin, "BIG5"},
	{"Taiwanese_Mandarin PinYin", "zh-TW", "PinYin", eciTaiwaneseMandarinPinYin, "BIG5"},
	{"Brazilian_Portuguese", "pt-BR", NULL, eciBrazilianPortuguese, "ISO-8859-1"},
	{"Japanese_UCS", "ja-JP", "UCS", eciStandardJapaneseUCS, "UCS2"},
	{"Japanese", "ja-JP", NULL, eciStandardJapanese, "SJIS"},
	{"Japanese_SJIS", "ja-JP", "SJIS", eciStandardJapaneseSJIS, "SJIS"},
	{"Finnish", "fi-FI", NULL, eciStandardFinnish, "ISO-8859-1"},
	{"Korean_UCS", "ko-KR", "UCS", eciStandardKoreanUCS, "UCS2"},
	{"Korean", "ko-KR", NULL, eciStandardKorean, "UHC"},
	{"Korean_UHC", "ko-KR", "UHC", eciStandardKoreanUHC, "UHC"},
	{"Cantonese_UCS", "zh-HK", "UCS", eciStandardCantoneseUCS, "UCS2"},
	{"Cantonese", "zh-HK", NULL, eciStandardCantonese, "GBK"},
	{"Cantonese_GB", "zh-HK", "GB", eciStandardCantoneseGB, "GBK"},
	{"HongKong_Cantonese UCS", "zh-HK", "UCS", eciHongKongCantoneseUCS, "UCS-2"},
	{"HongKong_Cantonese", "zh-HK", NULL, eciHongKongCantonese, "BIG5"},
	{"HongKong_Cantonese Big 5", "zh-HK", "BIG5", eciHongKongCantoneseBig5, "BIG5"},
	{"Dutch", "nl-BE", NULL, eciStandardDutch, "ISO-8859-1"},
	{"Norwegian", "no-NO", NULL, eciStandardNorwegian, "ISO-8859-1"},
	{"Swedish", "sv-SE", NULL, eciStandardSwedish, "ISO-8859-1"},
	{"Danish", "da-DK", NULL, eciStandardDanish, "ISO-8859-1"},
	{"Reserved", "en-US", NULL, eciStandardReserved, "ISO-8859-1"},
	{"Thai", "th-TH", NULL, eciStandardThai, "TIS-620"},
	{"ThaiTIS", "th-TH", "TIS", eciStandardThaiTIS, "TIS-620"},
	{NULL, 0, NULL}
};

#define MAX_NB_OF_LANGUAGES (sizeof(eciLocales)/sizeof(eciLocales[0]) - 1)
#endif

/* dictionary_filename: its index corresponds to the ECIDictVolume enumerate */
static char *dictionary_filenames[] = {
	"main.dct",
	"root.dct",
	"abbreviation.dct",
	"extension.dct"
};

#define NB_OF_DICTIONARY_FILENAMES (sizeof(dictionary_filenames)/sizeof(dictionary_filenames[0]))

/* Public functions */

int module_load(void)
{
	INIT_SETTINGS_TABLES();

	REGISTER_DEBUG();

	MOD_OPTION_1_INT_REG(IbmttsUseSSML, 1);
	MOD_OPTION_1_INT_REG(IbmttsUsePunctuation, 1);
	MOD_OPTION_1_INT_REG(IbmttsUseAbbreviation, 1);
	MOD_OPTION_1_STR_REG(IbmttsPunctuationList, "()?");
	MOD_OPTION_1_STR_REG(IbmttsDictionaryFolder,
			     "/var/opt/IBM/ibmtts/dict");

	MOD_OPTION_1_INT_REG(IbmttsAudioChunkSize, 20000);
	MOD_OPTION_1_STR_REG(IbmttsSoundIconFolder,
			     "/usr/share/sounds/sound-icons/");

	/* Register voices. */
	module_register_settings_voices();

	/* Register voice parameters */
	MOD_OPTION_HT_REG(IbmttsVoiceParameters);

	/* Register key substitutions. */
	MOD_OPTION_HT_DLL_REG(IbmttsKeySubstitution);

	return MODULE_OK;
}

int module_init(char **status_info)
{
	char version[20];

	DBG(DBG_MODNAME "Module init().");

	module_audio_set_server();

	*status_info = NULL;

	/* Report versions. */
	eciVersion(version);
	DBG(DBG_MODNAME "output module version %s, engine version %s", MODULE_VERSION, version);

	/* TODO: according to version, enable SSML and punct by default or not
	 */

	/* Setup TTS engine. */
	DBG(DBG_MODNAME "Creating an engine instance.");
	eciHandle = eciNew();
	if (NULL_ECI_HAND == eciHandle) {
		DBG(DBG_MODNAME "Could not create an engine instance.\n");
		*status_info = g_strdup("Could not create an engine instance. "
					"Is the TTS engine installed?");
		return MODULE_FATAL_ERROR;
	}

	update_sample_rate();

	/* Allocate a chunk for ECI to return audio. */
	audio_chunk =
	    (TEciAudioSamples *) g_malloc((IbmttsAudioChunkSize) *
					  sizeof(TEciAudioSamples));

	DBG(DBG_MODNAME "Registering ECI callback.");
	eciRegisterCallback(eciHandle, eciCallback, NULL);

	DBG(DBG_MODNAME "Registering an ECI audio buffer.");
	if (!eciSetOutputBuffer(eciHandle, IbmttsAudioChunkSize, audio_chunk)) {
		DBG(DBG_MODNAME "Error registering ECI audio buffer.");
		log_eci_error();
	}

	eciSetParam(eciHandle, eciDictionary, !IbmttsUseAbbreviation);

	/* enable annotations */
	eciSetParam(eciHandle, eciInputType, 1);

	/* load possibly the ssml filter */
	if (IbmttsUseSSML)
		eciAddText(eciHandle, " `gfa1 ");

	/* load possibly the punctuation filter */
	if (IbmttsUsePunctuation)
		eciAddText(eciHandle, " `gfa2 ");

	set_punctuation_mode(msg_settings.punctuation_mode);

	if (!alloc_voice_list()) {
		DBG(DBG_MODNAME "voice list allocation failed.");
		*status_info =
			g_strdup
			("The module can't build the list of installed voices.");
		return MODULE_FATAL_ERROR;
	}

	DBG(DBG_MODNAME "IbmttsAudioChunkSize = %d", IbmttsAudioChunkSize);

	*status_info = g_strdup(DBG_MODNAME "Initialized successfully.");

	return MODULE_OK;
}

SPDVoice **module_list_voices(void)
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	return speechd_voice;
}

void module_speak_sync(const gchar * data, size_t bytes, SPDMessageType msgtype)
{
	/* Current message from Speech Dispatcher. */
	char *message;
	SPDMessageType message_type;

	DBG(DBG_MODNAME "module_speak().");

	DBG(DBG_MODNAME "Type: %d, bytes: %lu, requested data: |%s|\n", msgtype,
	    (unsigned long)bytes, data);

	if (!g_utf8_validate(data, bytes, NULL)) {
		DBG(DBG_MODNAME "Input is not valid utf-8.");
		/* Actually, we should just fail here, but let's assume input is latin-1 */
		message =
		    g_convert(data, bytes, "utf-8", "iso-8859-1", NULL, NULL,
			      NULL);
		if (message == NULL) {
			DBG(DBG_MODNAME "Fallback conversion to utf-8 failed.");
			module_speak_error();
			return;
		}
	} else {
		message = g_strndup(data, bytes);
	}

	message_type = msgtype;
	if ((msgtype == SPD_MSGTYPE_TEXT)
	    && (msg_settings.spelling_mode == SPD_SPELL_ON))
		message_type = SPD_MSGTYPE_SPELL;

	/* Setting speech parameters. */
	UPDATE_STRING_PARAMETER(voice.language, set_language);
	UPDATE_PARAMETER(voice_type, set_voice_type);
	UPDATE_STRING_PARAMETER(voice.name, set_synthesis_voice);
	UPDATE_PARAMETER(rate, set_rate);
	UPDATE_PARAMETER(volume, set_volume);
	UPDATE_PARAMETER(pitch, set_pitch);
	UPDATE_PARAMETER(punctuation_mode, set_punctuation_mode);
	UPDATE_PARAMETER(cap_let_recogn, set_capital_mode);
	
	if (!IbmttsUseSSML) {
		/* Strip all SSML */
		char *tmp = message;
		message = module_strip_ssml(message);
		g_free(tmp);
		/* Convert input to suitable encoding for current language dialect */
		tmp =
		    g_convert_with_fallback(message, -1,
					    input_encoding, "utf-8", "?",
					    NULL, NULL, NULL);
		if (tmp != NULL) {
			g_free(message);
			message = tmp;
		}
	}

	stop_requested = FALSE;
	pause_requested = FALSE;
	pause_index_sent = FALSE;

	module_speak_ok();
	module_report_event_begin();
	_synth(message, message_type);
	if (pause_requested)
		module_report_event_pause();
	else if (stop_requested)
		module_report_event_stop();
	else
		module_report_event_end();

	DBG(DBG_MODNAME "Leaving module_speak_sync() normally.");
}

int module_stop(void)
{
	DBG(DBG_MODNAME "module_stop().");

	stop_requested = TRUE;

	return MODULE_OK;
}

size_t module_pause(void)
{
	/* The semantics of module_pause() is the same as module_stop()
	   except that processing should continue until the next index mark is
	   reached before stopping.
	   Note that although IBM TTS offers an eciPause function, we cannot
	   make use of it because Speech Dispatcher doesn't have a module_resume
	   function. Instead, Speech Dispatcher resumes by calling module_speak
	   from the last index mark reported in the text. */
	DBG(DBG_MODNAME "module_pause().");

	pause_requested = TRUE;

	return MODULE_OK;
}

int module_close(void)
{

	DBG(DBG_MODNAME "close().");

	DBG(DBG_MODNAME "Stopping speech");
	module_stop();

	DBG(DBG_MODNAME "De-registering ECI callback.");
	eciRegisterCallback(eciHandle, NULL, NULL);

	DBG(DBG_MODNAME "Destroying ECI instance.");
	eciDelete(eciHandle);
	eciHandle = NULL_ECI_HAND;

	/* Free buffer for ECI audio. */
	g_free(audio_chunk);

	/* Free index mark lookup table. */
	if (index_mark_ht) {
		g_hash_table_destroy(index_mark_ht);
		index_mark_ht = NULL;
	}

	free_voice_list();

	return 0;
}

/* Internal functions */

static void update_sample_rate()
{
	//	DBG(DBG_MODNAME "ENTER %s", __func__);
	int sample_rate;
	/* Get ECI audio sample rate. */
	sample_rate = eciGetParam(eciHandle, eciSampleRate);
	switch (sample_rate) {
	case 0:
		eci_sample_rate = 8000;
		break;
	case 1:
		eci_sample_rate = 11025;
		break;
	case 2:
		eci_sample_rate = 22050;
		break;
	default:
		DBG(DBG_MODNAME "Invalid audio sample rate returned by ECI = %i",
		    sample_rate);
	}
	DBG(DBG_MODNAME "LEAVE %s, eci_sample_rate=%d",  __FUNCTION__, eci_sample_rate);  
}

/* Given a string containing an index mark in the form
   <mark name="some_name"/>, returns some_name.  Calling routine is
   responsible for freeing returned string. If an error occurs,
   returns NULL. */
static char *extract_mark_name(char *mark)
{
	if ((SD_MARK_HEAD_ONLY_LEN + SD_MARK_TAIL_LEN + 1) > strlen(mark))
		return NULL;
	mark = mark + SD_MARK_HEAD_ONLY_LEN;
	char *tail = strstr(mark, SD_MARK_TAIL);
	if (NULL == tail)
		tail = strstr(mark, SD_MARK_TAIL2);
	if (NULL == tail)
		return NULL;
	return (char *)g_strndup(mark, tail - mark);
}

/* Returns the portion of msg up to, but not including, the next index
   mark, or end of msg if no index mark is found.  If msg begins with
   and index mark, returns the entire index mark clause (<mark name="whatever"/>)
   and returns the mark name.  If msg does not begin with an index mark,
   mark_name will be NULL. If msg is empty, returns a zero-length string (not NULL).
   Caller is responsible for freeing both returned string and mark_name (if not NULL). */
/* TODO: This routine needs to be more tolerant of custom index marks with spaces. */
/* TODO: Should there be a MaxChunkLength? Delimiters? */
static char *next_part(char *msg, char **mark_name)
{
	char *mark_head = strstr(msg, SD_MARK_HEAD_ONLY);
	if (NULL == mark_head)
		mark_head = strstr(msg, SD_MARK_HEAD_ONLY2);
	if (NULL == mark_head)
		return (char *)g_strdup(msg);
	else if (mark_head == msg) {
		*mark_name = extract_mark_name(mark_head);
		if (NULL == *mark_name) {
			/* ill-formed, ignore the mark */
			DBG(DBG_MODNAME "Note: ill-formed mark %s", msg);
			char *tail = strstr(msg + SD_MARK_HEAD_ONLY_LEN, SD_MARK_TAILTAIL);
			if (!tail) {
				/* Uh, not even the tail... */
				return (char *)g_strdup(msg);
			}
			tail += strlen(SD_MARK_TAILTAIL);
			char *remainder = next_part(tail, mark_name);
			char *ret = g_strdup_printf("%.*s%s",
					(int) (tail - msg), msg, remainder);
			g_free(remainder);
			return ret;
		}
		else
			return (char *)g_strndup(msg,
						 SD_MARK_HEAD_ONLY_LEN +
						 strlen(*mark_name) +
						 SD_MARK_TAIL_LEN);
	} else
		return (char *)g_strndup(msg, mark_head - msg);
}

static int process_text_mark(char *part, int part_len, char *mark_name)
{
	/* Handle index marks. */
	if (NULL != mark_name) {
		/* Assign the mark name an integer number and store in lookup table. */
		int *markId = (int *)g_malloc(sizeof(int));
		*markId = 1 + g_hash_table_size(index_mark_ht);
		g_hash_table_insert(index_mark_ht, markId, mark_name);
		if (!eciInsertIndex(eciHandle, *markId)) {
			DBG(DBG_MODNAME "Error sending index mark to synthesizer.");
			log_eci_error();
			/* Try to keep going. */
		} else
			DBG(DBG_MODNAME "Index mark |%s| (id %i) sent to synthesizer.", mark_name, *markId);
		return 0;
	}

	/* Handle normal text. */
	if (part_len > 0) {
		DBG(DBG_MODNAME "Returned %d bytes from get_part.", part_len);
		DBG(DBG_MODNAME "Text to synthesize is |%s|", part);
		DBG(DBG_MODNAME "Sending text to synthesizer.");
		if (!eciAddText(eciHandle, part)) {
			DBG(DBG_MODNAME "Error sending text.");
			log_eci_error();
			return 2;
		}
		return 0;
	}

	/* Handle end of text. */
	DBG(DBG_MODNAME "End of data in synthesis.");
	/*
	   Add index mark for end of message.
	   This also makes sure the callback gets called at least once
	 */
	eciInsertIndex(eciHandle, MSG_END_MARK);
	DBG(DBG_MODNAME "Trying to synthesize text.");
	if (!eciSynthesize(eciHandle)) {
		DBG(DBG_MODNAME "Error synthesizing.");
		log_eci_error();
		return 2;;
	}

	/* Audio and index marks are returned in eciCallback(). */
	DBG(DBG_MODNAME "Waiting for synthesis to complete.");
	if (!eciSynchronize(eciHandle)) {
		DBG(DBG_MODNAME "Error waiting for synthesis to complete.");
		log_eci_error();
		return 2;
	}
	DBG(DBG_MODNAME "Synthesis complete.");
	return 3;
}

/* Synthesis. */
static void _synth(char *message, SPDMessageType message_type)
{
	char *pos = NULL;
	char *part = NULL;
	int part_skip_end, part_len;
	int ret;

	/* Allocate a place for index mark names to be placed. */
	char *mark_name = NULL;

	/* This table assigns each index mark name an integer id for fast lookup when
	   ECI returns the integer index mark event. */
	if (index_mark_ht)
		g_hash_table_destroy(index_mark_ht);
	index_mark_ht =
	    g_hash_table_new_full(g_int_hash, g_int_equal, g_free,
				  g_free);

	pos = message;
	load_user_dictionary();

	switch (message_type) {
	case SPD_MSGTYPE_TEXT:
		eciSetParam(eciHandle, eciTextMode, eciTextModeDefault);
		break;
	case SPD_MSGTYPE_SOUND_ICON:
		/* IBM TTS does not support sound icons.
		   If we can find a sound icon file, play that,
		   otherwise speak the name of the sound icon. */
		part = search_for_sound_icon(message);
		if (NULL != part) {
			add_sound_icon_to_playback_queue(part);
			return;
		} else
			eciSetParam(eciHandle, eciTextMode,
				    eciTextModeDefault);
		break;
	case SPD_MSGTYPE_CHAR:
		eciSetParam(eciHandle, eciTextMode,
			    eciTextModeAllSpell);
		break;
	case SPD_MSGTYPE_KEY:
		/* TODO: make sure all SSIP cases are supported */
		/* Map unspeakable keys to speakable words. */
		DBG(DBG_MODNAME "Key from Speech Dispatcher: |%s|", pos);
		pos = subst_keys(pos);
		DBG(DBG_MODNAME "Key to speak: |%s|", pos);
		g_free(message);
		message = pos;
		eciSetParam(eciHandle, eciTextMode, eciTextModeDefault);
		break;
	case SPD_MSGTYPE_SPELL:
		if (SPD_PUNCT_NONE != msg_settings.punctuation_mode)
			eciSetParam(eciHandle, eciTextMode,
				    eciTextModeAllSpell);
		else
			eciSetParam(eciHandle, eciTextMode,
				    eciTextModeAlphaSpell);
		break;
	}

	if (!IbmttsUseSSML)
	{
		process_text_mark(pos, strlen(pos), NULL);
		process_text_mark(NULL, 0, NULL);
		return;
	}

	while (TRUE) {
		/* Process server events in case we were told to stop in between
 */
		module_process(STDIN_FILENO, 0);

		DBG(DBG_MODNAME "Processing synth.");
		if (stop_requested || (pause_requested && pause_index_sent)) {
			DBG(DBG_MODNAME "Stop in synthesis, terminating.");
			break;
		}

		/* TODO: How to map these msg_settings to ibm tts?
		   ESpellMode spelling_mode;
		   SPELLING_ON already handled in module_speak()
		   ECapLetRecogn cap_let_recogn;
		   RECOGN_NONE = 0,
		   RECOGN_SPELL = 1,
		   RECOGN_ICON = 2
		 */

		if (!strncmp(pos, SD_SPEAK, strlen(SD_SPEAK))) {
			DBG(DBG_MODNAME "Drop heading "SD_SPEAK".");
			pos += strlen(SD_SPEAK);
		}

		part = next_part(pos, &mark_name);
		if (NULL == part) {
			DBG(DBG_MODNAME "Error getting next part of message.");
			/* TODO: What to do here? */
			break;
		}
		part_len = strlen(part);
		if (part_len >= strlen(SD_ENDSPEAK) &&
			!strncmp(part + part_len - strlen(SD_ENDSPEAK),
				 SD_ENDSPEAK, strlen(SD_ENDSPEAK))) {
			DBG(DBG_MODNAME "Drop trailing "SD_ENDSPEAK".");
			part_skip_end = strlen(SD_ENDSPEAK);
			part[part_len - part_skip_end] = 0;
		} else {
			part_skip_end = 0;
		}
		pos += part_len;
		ret = process_text_mark(part,
				part_len - part_skip_end,
				mark_name);
		g_free(part);
		part = NULL;
		mark_name = NULL;
		if (ret == 1)
			pos += strlen(pos);
		else if (ret > 1) {
			DBG(DBG_MODNAME "Finished synthesis.");
			break;
		}
	}
}

static void set_rate(signed int rate)
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	/* Setting rate to midpoint is too fast.  An eci value of 50 is "normal".
	   See chart on pg 38 of the ECI manual. */
	assert(rate >= -100 && rate <= +100);
	int speed;
	/* Possible ECI range is 0 to 250. */
	/* Map rate -100 to 100 onto speed 0 to 140. */
	if (rate < 0)
		/* Map -100 to 0 onto 0 to voice_speed */
		speed = ((float)(rate + 100) * voice_speed) / (float)100;
	else
		/* Map 0 to 100 onto voice_speed to 140 */
		speed =
		    (((float)rate * (140 - voice_speed)) / (float)100)
		    + voice_speed;
	assert(speed >= 0 && speed <= 140);
	int ret = eciSetVoiceParam(eciHandle, 0, eciSpeed, speed);
	if (-1 == ret) {
		DBG(DBG_MODNAME "Error setting rate %i.", speed);
		log_eci_error();
	} else
		DBG(DBG_MODNAME "Rate set to %i.", speed);
}

static void set_volume(signed int volume)
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	/* Setting volume to midpoint makes speech too soft.  An eci value
	   of 90 to 100 is "normal".
	   See chart on pg 38 of the ECI manual.
	   TODO: Rather than setting volume in the synth, maybe control volume on playback? */
	assert(volume >= -100 && volume <= +100);
	int vol;
	/* Possible ECI range is 0 to 100. */
	if (volume < 0)
		/* Map -100 to 0 onto 0 to 90 */
		vol = (((float)volume + 100) * 90) / (float)100;
	else
		/* Map 0 to 100 onto 90 to 100 */
		vol = ((float)(volume * 10) / (float)100) + 90;
	assert(vol >= 0 && vol <= 100);
	int ret = eciSetVoiceParam(eciHandle, 0, eciVolume, vol);
	if (-1 == ret) {
		DBG(DBG_MODNAME "Error setting volume %i.", vol);
		log_eci_error();
	} else
		DBG(DBG_MODNAME "Volume set to %i.", vol);
}

static void set_pitch(signed int pitch)
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	/* Setting pitch to midpoint is to low.  eci values between 65 and 89
	   are "normal".
	   See chart on pg 38 of the ECI manual. */
	assert(pitch >= -100 && pitch <= +100);
	int pitchBaseline;
	/* Possible range 0 to 100. */
	if (pitch < 0)
		/* Map -100 to 0 onto 0 to voice_pitch_baseline */
		pitchBaseline =
		    ((float)(pitch + 100) * voice_pitch_baseline) /
		    (float)100;
	else
		/* Map 0 to 100 onto voice_pitch_baseline to 100 */
		pitchBaseline =
		    (((float)pitch * (100 - voice_pitch_baseline)) /
		     (float)100)
		    + voice_pitch_baseline;
	assert(pitchBaseline >= 0 && pitchBaseline <= 100);
	int ret =
	    eciSetVoiceParam(eciHandle, 0, eciPitchBaseline, pitchBaseline);
	if (-1 == ret) {
		DBG(DBG_MODNAME "Error setting pitch %i.", pitchBaseline);
		log_eci_error();
	} else
		DBG(DBG_MODNAME "Pitch set to %i.", pitchBaseline);
}

static void set_punctuation_mode(SPDPunctuation punct_mode)
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	const char *fmt = " `Pf%d%s ";
	char *msg = NULL;
	int real_punct_mode = 0;

	if (!IbmttsUsePunctuation)
		return;

	switch (punct_mode) {
	case SPD_PUNCT_NONE:
		real_punct_mode = 0;
		break;
	case SPD_PUNCT_SOME:
		real_punct_mode = 2;
		break;
	case SPD_PUNCT_MOST:
		/* XXX approximation */
		real_punct_mode = 2;
		break;
	case SPD_PUNCT_ALL:
		real_punct_mode = 1;
		break;
	}

	msg = g_strdup_printf(fmt, real_punct_mode, IbmttsPunctuationList);
	eciAddText(eciHandle, msg);
	g_free(msg);
}

#ifdef VOXIN
static void set_capital_mode(SPDCapitalLetters cap_mode)
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	voxCapitalMode mode = voxCapitalNone;

	switch (cap_mode) {
	case SPD_CAP_NONE:
		mode = voxCapitalNone;
		break;
	case SPD_CAP_SPELL:
		mode = voxCapitalSpell;
		break;
	case SPD_CAP_ICON:
		mode = voxCapitalSoundIcon;
		break;
	}

	voxSetParam(eciHandle, VOX_CAPITALS, mode);
}
#else
static void set_capital_mode(SPDCapitalLetters cap_mode){}
#endif

static char *voice_enum_to_str(SPDVoiceType voice_type)
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	/* TODO: Would be better to move this to module_utils.c. */
	char *voicename;
	switch (voice_type) {
	case SPD_MALE1:
		voicename = g_strdup("male1");
		break;
	case SPD_MALE2:
		voicename = g_strdup("male2");
		break;
	case SPD_MALE3:
		voicename = g_strdup("male3");
		break;
	case SPD_FEMALE1:
		voicename = g_strdup("female1");
		break;
	case SPD_FEMALE2:
		voicename = g_strdup("female2");
		break;
	case SPD_FEMALE3:
		voicename = g_strdup("female3");
		break;
	case SPD_CHILD_MALE:
		voicename = g_strdup("child_male");
		break;
	case SPD_CHILD_FEMALE:
		voicename = g_strdup("child_female");
		break;
	default:
		voicename = g_strdup("no voice");
		break;
	}
	return voicename;
}

/** Set voice parameters (if any are defined for this voice) */
static void set_voice_parameters(SPDVoiceType voice_type)
{
	char *voicename = voice_enum_to_str(voice_type);
	int eciVoice;
	int ret = -1;

	TIbmttsVoiceParameters *params = g_hash_table_lookup(IbmttsVoiceParameters, voicename);
	if (NULL == params) {
		DBG(DBG_MODNAME "Setting default VoiceParameters for voice %s", voicename);

		switch (voice_type) {
		case SPD_MALE1:
			eciVoice = 1;
			break;	/* Adult Male 1 */
		case SPD_MALE2:
			eciVoice = 4;
			break;	/* Adult Male 2 */
		case SPD_MALE3:
			eciVoice = 5;
			break;	/* Adult Male 3 */
		case SPD_FEMALE1:
			eciVoice = 2;
			break;	/* Adult Female 1 */
		case SPD_FEMALE2:
			eciVoice = 6;
			break;	/* Adult Female 2 */
		case SPD_FEMALE3:
			eciVoice = 7;
			break;	/* Elderly Female 1 */
		case SPD_CHILD_MALE:
		case SPD_CHILD_FEMALE:
			eciVoice = 3;
			break;	/* Child */
		default:
			eciVoice = 1;
			break;	/* Adult Male 1 */
		}
		ret = eciCopyVoice(eciHandle, eciVoice, 0);
		if (-1 == ret)
			DBG(DBG_MODNAME "ERROR: Setting default voice parameters (voice %i).", eciVoice);
	} else {
		DBG(DBG_MODNAME "Setting custom VoiceParameters for voice %s", voicename);

		ret = eciSetVoiceParam(eciHandle, 0, eciGender, params->gender);
		if (-1 == ret)
			DBG(DBG_MODNAME "ERROR: Setting gender %i", params->gender);

		ret = eciSetVoiceParam(eciHandle, 0, eciBreathiness, params->breathiness);
		if (-1 == ret)
			DBG(DBG_MODNAME "ERROR: Setting breathiness %i", params->breathiness);

		ret = eciSetVoiceParam(eciHandle, 0, eciHeadSize, params->head_size);
		if (-1 == ret)
			DBG(DBG_MODNAME "ERROR: Setting head size %i", params->head_size);

		ret = eciSetVoiceParam(eciHandle, 0, eciPitchBaseline, params->pitch_baseline);
		if (-1 == ret)
			DBG(DBG_MODNAME "ERROR: Setting pitch baseline %i", params->pitch_baseline);

		ret = eciSetVoiceParam(eciHandle, 0, eciPitchFluctuation, params->pitch_fluctuation);
		if (-1 == ret)
			DBG(DBG_MODNAME "ERROR: Setting pitch fluctuation %i", params->pitch_fluctuation);

		ret = eciSetVoiceParam(eciHandle, 0, eciRoughness, params->roughness);
		if (-1 == ret)
			DBG(DBG_MODNAME "ERROR: Setting roughness %i", params->roughness);

		ret = eciSetVoiceParam(eciHandle, 0, eciSpeed, params->speed);
		if (-1 == ret)
			DBG(DBG_MODNAME "ERROR: Setting speed %i", params->speed);
	}

	g_free(voicename);
}

#ifdef VOXIN
/*
   Convert the supplied arguments to the eciLanguageDialect value and
   sets the eciLanguageDialect parameter.

   The arguments are used in this order:
   - find a matching voice name,
   - otherwise find the first matching language

   EXAMPLES
   1. Using Orca 3.30.1:
   - lang="en", voice=1, name="zuzana"
   name ("zuzana") matches the installed voice Zuzana embedded-compact

   - lang="en", voice=1, name="voxin default voice"
   name does not match any installed voice.
   The first English voice present is returned.


   2. Using spd-say (LC_ALL=C)
   - lang="c", voice=1, name="nathan-embedded-compact"
   name matches the installed voice Nathan embedded-compact

   spd-say command:
   spd-say -o voxin -y nathan-embedded-compact hello

   - lang="en-us", voice=1, name=
   The first American English voice present is returned.

   spd-say command:
   spd-say -o voxin -l en-US hello

*/
#else
/* Given a language, dialect and SD voice codes sets the IBM voice */
#endif
static void set_language_and_voice(char *lang, SPDVoiceType voice_type, char *name)
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	int ret = -1;
	int i = 0, index = -1;

	DBG(DBG_MODNAME "%s, lang=%s, voice_type=%d, name=%s",
	    __FUNCTION__, lang, (int)voice_type, name ? name : "");

	assert(speechd_voice);

	if (name && *name) {
		for (i = 0; speechd_voice[i]; i++) {
			DBG("%d. name=%s", i, speechd_voice[i]->name);
			if (!strcasecmp(speechd_voice[i]->name, name)) {
				index = voice_index(i);
				break;
			}
		}
	}

	if ((index == -1) && lang) {
		char *langbase;	// requested base language + '-'
		char *dash = strchr(lang, '-');
		if (dash)
			langbase = g_strndup(lang, dash-lang+1);
		else
			langbase = g_strdup_printf("%s-", lang);

		for (i = 0; speechd_voice[i]; i++) {
			DBG("%d. language=%s", i, speechd_voice[i]->language);
			if (!strcasecmp(speechd_voice[i]->language, lang)) {
				DBG("strong match!");
				index = voice_index(i);
				break;
			}
			if (index == -1) {
				/* Try base language matching as fallback */
				if (!strncasecmp(speechd_voice[i]->language, langbase, strlen(langbase))) {
					DBG("match!");
					index = voice_index(i);
				}
			}
		}
		g_free(langbase);
	}

	if (index == -1) { // no matching voice: choose the first available voice 
		if (!speechd_voice[0])
			return;
		index = 0;
	}

#ifdef VOXIN
	ret = eciSetParam(eciHandle, eciLanguageDialect, voices[index].id);
#else
	ret = eciSetParam(eciHandle, eciLanguageDialect, eciLocales[index].langID);
#endif
	if (ret == -1) {
		DBG(DBG_MODNAME "Unable to set language");
		log_eci_error();
		return;
	}

#ifdef VOXIN
	DBG(DBG_MODNAME "select speechd_voice[%d]: id=0x%x, name=%s (ret=%d)",
	    index, voices[index].id, voices[index].name, ret);

	input_encoding = voices[index].charset;
#else
	DBG(DBG_MODNAME "set langID=0x%x (ret=%d)",
	    eciLocales[index].langID, ret);

	input_encoding = eciLocales[index].charset;
#endif
	update_sample_rate();		  	
	g_atomic_int_set(&locale_index_atomic, index);

	set_voice_parameters(voice_type);

	/* Retrieve the baseline pitch and speed of the voice. */
	voice_pitch_baseline = eciGetVoiceParam(eciHandle, 0, eciPitchBaseline);
	if (-1 == voice_pitch_baseline)
		DBG(DBG_MODNAME "Cannot get pitch baseline of voice.");

	voice_speed = eciGetVoiceParam(eciHandle, 0, eciSpeed);
	if (-1 == voice_speed)
		DBG(DBG_MODNAME "Cannot get speed of voice.");
}

static void set_voice_type(SPDVoiceType voice_type)
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	if (msg_settings.voice.language) {
		set_language_and_voice(msg_settings.voice.language, voice_type, msg_settings.voice.name);
	}
}

static void set_language(char *lang)
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	set_language_and_voice(lang, msg_settings.voice_type, msg_settings.voice.name);
}

/* sets the voice according to its name. 

   If the voice name is not found, try to select the first available
   voice for the current language.
*/
static void set_synthesis_voice(char *synthesis_voice)
{
	if (synthesis_voice == NULL) {
		return;
	}

	DBG(DBG_MODNAME "ENTER %s(%s)", __FUNCTION__, synthesis_voice);

	set_language_and_voice(msg_settings.voice.language, msg_settings.voice_type, synthesis_voice);
}

static void log_eci_error()
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	/* TODO: This routine is not working.  Not sure why. */
	char buf[100];
	eciErrorMessage(eciHandle, buf);
	DBG(DBG_MODNAME "ECI Error Message: %s", buf);
}

/* The text-to-speech calls back here when a chunk of audio is ready
   or an index mark has been reached.  The good news is that it
   returns the audio up to each index mark or when the audio buffer is
   full. */
static enum ECICallbackReturn eciCallback(ECIHand hEngine,
					  enum ECIMessage msg,
					  long lparam, void *data)
{
	/* If module_stop was called, discard any further callbacks until module_speak is called. */
	if (stop_requested || (pause_requested && pause_index_sent)) {
		DBG(DBG_MODNAME "Stopped or paused, stop synthesizing.");
		return eciDataAbort;
	}

	switch (msg) {
	case eciWaveformBuffer:
		DBG(DBG_MODNAME "%ld audio samples returned from TTS.", lparam);
		/* Add audio to output queue. */
		add_audio_to_playback_queue(audio_chunk, lparam);
		return eciDataProcessed;

	case eciIndexReply:
		DBG(DBG_MODNAME "Index mark id %ld returned from TTS.", lparam);
		if (lparam != MSG_END_MARK) {
			/* Add index mark to output queue. */
			add_mark_to_playback_queue(lparam);
		}
		return eciDataProcessed;

	default:
		return eciDataProcessed;
	}
}

/* Adds a chunk of pcm audio to the audio playback queue. */
static gboolean add_audio_to_playback_queue(TEciAudioSamples * audio_chunk, long num_samples)
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	AudioTrack track = {
		.bits = 16,
		.num_channels = 1,
		.sample_rate = eci_sample_rate,
		.num_samples = num_samples,
		.samples = audio_chunk,
	};
#if defined(BYTE_ORDER) && (BYTE_ORDER == BIG_ENDIAN)
	AudioFormat format = SPD_AUDIO_BE;
#else
	AudioFormat format = SPD_AUDIO_LE;
#endif

	module_tts_output_server(&track, format);
	return 0;
}

/* Adds an Index Mark to the audio playback queue. */
static void add_mark_to_playback_queue(long markId)
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	/* Look up the index mark integer id in lookup table to
	   find string name and emit that name. */
	char *mark_name = g_hash_table_lookup(index_mark_ht, &markId);
	if (NULL == mark_name) {
		DBG(DBG_MODNAME "markId %ld returned by TTS not found in lookup table.", markId);
		return;
	}
	DBG(DBG_MODNAME "reporting index mark |%s|.", mark_name);
	module_report_index_mark(mark_name);
	if (pause_requested && !strncmp(mark_name, INDEX_MARK_BODY, INDEX_MARK_BODY_LEN))
		pause_index_sent = TRUE;
	DBG(DBG_MODNAME "index mark reported.");
}

/* Add a sound icon to the playback queue. */
static gboolean add_sound_icon_to_playback_queue(char *filename)
{
	module_report_icon(filename);
	return 0;
}

/* Replaces all occurrences of "from" with "to" in msg.
   Returns count of replacements. */
static int replace(char *from, char *to, GString * msg)
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	int count = 0;
	int pos;
	int from_len = strlen(from);
	int to_len = strlen(to);
	char *p = msg->str;
	while (NULL != (p = strstr(p, from))) {
		pos = p - msg->str;
		g_string_erase(msg, pos, from_len);
		g_string_insert(msg, pos, to);
		p = msg->str + pos + to_len;
		++count;
	}
	return count;
}

static void subst_keys_cb(gpointer data, gpointer user_data)
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	TIbmttsKeySubstitution *key_subst = data;
	GString *msg = user_data;
	replace(key_subst->key, key_subst->newkey, msg);
}

/* Given a Speech Dispatcher !KEY key sequence, replaces unspeakable
   or incorrectly spoken keys or characters with speakable ones.
   The subsitutions come from the KEY NAME SUBSTITUTIONS section of the
   config file.
   Caller is responsible for freeing returned string. */
static char *subst_keys(char *key)
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	GString *tmp = g_string_sized_new(30);
	g_string_append(tmp, key);

	GList *keyTable = g_hash_table_lookup(IbmttsKeySubstitution,
					      msg_settings.voice.language);

	if (keyTable)
		g_list_foreach(keyTable, subst_keys_cb, tmp);

	/* Hyphen hangs IBM TTS */
	if (0 == strcmp(tmp->str, "-"))
		g_string_assign(tmp, "hyphen");

	return g_string_free(tmp, FALSE);
}

/* Given a sound icon name, searches for a file to play and if found
   returns the filename.  Returns NULL if none found.  Caller is responsible
   for freeing the returned string. */
/* TODO: These current assumptions should be dealt with:
        Sound icon files are in a single directory (IbmttsSoundIconFolder).
        The name of each icon is symlinked to a .wav file.
   If you have installed the free(b)soft sound-icons package under
   Debian, then these assumptions are true, but what about other distros
   and OSes? */
static char *search_for_sound_icon(const char *icon_name)
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	char *fn = NULL;
	if (0 == strlen(IbmttsSoundIconFolder))
		return fn;
	GString *filename = g_string_new(IbmttsSoundIconFolder);
	filename = g_string_append(filename, icon_name);
	if (g_file_test(filename->str, G_FILE_TEST_EXISTS))
		fn = filename->str;
	/*
	   else {
	   filename = g_string_assign(filename, g_utf8_strdown(filename->str, -1));
	   if (g_file_test(filename->str, G_FILE_TEST_EXISTS))
	   fn = filename->str;
	   }
	 */

	/*
	 * if the file was found, the pointer *fn  points to the character data
	 * of the string filename. In this situation the string filename must be
	 * freed but its character data must be preserved.
	 * If the file is not found, the pointer *fn contains NULL. In this
	 * situation the string filename must be freed, including its character
	 * data.
	 */
	g_string_free(filename, (fn == NULL));
	return fn;
}

#ifdef VOXIN
static gboolean vox_to_spd_voice(vox_t *from, SPDVoice *to)
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	if (!from
	    || !to
	    || to->name || to->language || to->variant
	    || from->name[sizeof(from->name)-1]
	    || from->lang[sizeof(from->lang)-1]
	    || from->variant[sizeof(from->variant)-1]
	    ) {
		DBG(DBG_MODNAME "args error");
		return FALSE;
	}

	{ /* set name */
		int i;
		to->name = *from->quality ?
			g_strdup_printf("%s-%s", from->name, from->quality) :
			g_strdup(from->name);
		for (i=0; to->name[i]; i++) {
			to->name[i] = tolower(to->name[i]);
		}
	}
	{ /* set language: language identifier (lower case) + variant/dialect (all caps) */
		if (*from->variant) {
			size_t len = strlen(from->lang);
			int i;
			to->language = g_strdup_printf("%s-%s", from->lang, from->variant);
			for (i=len; to->language[i]; i++) {
				to->language[i] = toupper(to->language[i]);
			}
		} else {
			to->language = g_strdup(from->lang);
		}
	}
	to->variant = g_strdup("none");

	{ /* log the 'from' argument */
		size_t size = 0;
		if (!voxToString(from, NULL, &size)) {
			gchar *str = g_malloc0(size);
			if (!voxToString(from, str, &size)) {
				DBG(DBG_MODNAME "from: %s", str);
			}
			g_free(str);
		}
	}
	DBG(DBG_MODNAME "to: name=%s, variant=%s, language=%s", to->name, to->variant, to->language);
	return TRUE;
}

static gboolean alloc_voice_list()
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	int i = 0;

	/* obtain the list of installed voices */
	number_of_voices = 0;
	if (voxGetVoices(NULL, &number_of_voices) || !number_of_voices) {
		return FALSE;
	}

	voices = g_new0(vox_t, number_of_voices);
	if (voxGetVoices(voices, &number_of_voices) || !number_of_voices)
		goto exit0;

	DBG(DBG_MODNAME "number_of_voices=%u", number_of_voices);

	/* build speechd_voice */
	speechd_voice = g_new0(SPDVoice*, number_of_voices + 1);
	for (i = 0; i < number_of_voices; i++) {
		speechd_voice[i] = g_malloc0(sizeof(SPDVoice));
		if (!vox_to_spd_voice(voices+i, speechd_voice[i]))
			goto exit0;
	}
	speechd_voice[number_of_voices] = NULL;

	for (i = 0; speechd_voice[i]; i++) {
		DBG(DBG_MODNAME "speechd_voice[%d]:name=%s, language=%s, variant=%s",
		    i,
		    speechd_voice[i]->name ? speechd_voice[i]->name : "null",
		    speechd_voice[i]->language ? speechd_voice[i]->language : "null",
		    speechd_voice[i]->variant ? speechd_voice[i]->variant : "null");
	}

	DBG(DBG_MODNAME "LEAVE %s", __func__);
	return TRUE;

 exit0:
	if (voices) {
		g_free(voices);
		voices = NULL;
	}
	free_voice_list();
	return FALSE;
}
#else
gboolean alloc_voice_list()
{
	enum ECILanguageDialect aLanguage[MAX_NB_OF_LANGUAGES];
	int nLanguages = MAX_NB_OF_LANGUAGES;
	int i = 0;

	if (eciGetAvailableLanguages(aLanguage, &nLanguages))
		return FALSE;

	speechd_voice = g_malloc((nLanguages + 1) * sizeof(SPDVoice *));
	speechd_voice_index = g_malloc((nLanguages + 1) * sizeof(SPDVoice *));
	if (!speechd_voice)
		return FALSE;

	DBG(DBG_MODNAME "nLanguages=%d/%lu", nLanguages, (unsigned long)MAX_NB_OF_LANGUAGES);
	for (i = 0; i < nLanguages; i++) {
		/* look for the language name */
		int j;
		speechd_voice[i] = g_malloc(sizeof(SPDVoice));

		DBG(DBG_MODNAME "aLanguage[%d]=0x%08x", i, aLanguage[i]);
		for (j = 0; j < MAX_NB_OF_LANGUAGES; j++) {
			DBG(DBG_MODNAME "eciLocales[%d].langID=0x%08x", j,
			    eciLocales[j].langID);
			if (eciLocales[j].langID == aLanguage[i]) {
				speechd_voice[i]->name = eciLocales[j].name;
				speechd_voice[i]->language =
				    eciLocales[j].lang;
				speechd_voice[i]->variant =
				    eciLocales[j].variant;
				speechd_voice_index[i] = j;
				DBG(DBG_MODNAME "alloc_voice_list %s",
				    speechd_voice[i]->name);
				break;
			}
		}
		assert(j < MAX_NB_OF_LANGUAGES);
	}
	speechd_voice[nLanguages] = NULL;
	DBG(DBG_MODNAME "LEAVE %s", __func__);

	return TRUE;
}
#endif

static void free_voice_list()
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	int i = 0;

#ifndef VOXIN
	if (speechd_voice_index) {
		g_free(speechd_voice_index);
		speechd_voice_index = NULL;
	}
#endif

	if (!speechd_voice)
		return;

	for (i = 0; speechd_voice[i]; i++) {
#ifdef VOXIN
		if (speechd_voice[i]->name) {
			g_free(speechd_voice[i]->name);
			speechd_voice[i]->name = NULL;
		}
		if (speechd_voice[i]->language) {
			g_free(speechd_voice[i]->language);
			speechd_voice[i]->language = NULL;
		}
		if (speechd_voice[i]->variant) {
			g_free(speechd_voice[i]->variant);
			speechd_voice[i]->variant = NULL;
		}
#endif
		g_free(speechd_voice[i]);
		speechd_voice[i] = NULL;
	}

	g_free(speechd_voice);
	speechd_voice = NULL;
}

static void load_user_dictionary()
{
	DBG(DBG_MODNAME "ENTER %s", __func__);
	GString *dirname = NULL;
	GString *filename = NULL;
	int i = 0;
	int dictionary_is_present = 0;
	static guint old_index = G_MAXUINT;
	guint new_index;
	char *language = NULL;
#ifdef VOXIN
	char *region = NULL;
#else
	char *dash;
#endif
	ECIDictHand eciDict = eciGetDict(eciHandle);

	new_index = g_atomic_int_get(&locale_index_atomic);
	if (new_index >= MAX_NB_OF_LANGUAGES) {
		DBG(DBG_MODNAME "%s, unexpected index (0x%x)", __FUNCTION__,
		    new_index);
		return;
	}

	if (old_index == new_index) {
		DBG(DBG_MODNAME "LEAVE %s, no change", __FUNCTION__);
		return;
	}

#ifdef VOXIN
	language = g_strdup(voices[new_index].lang);
	region = voices[new_index].variant;
#else
	language = g_strdup(eciLocales[new_index].lang);
	dash = strchr(language, '-');
	if (dash)
		*dash = '_';
#endif

	if (eciDict) {
		DBG(DBG_MODNAME "delete old dictionary");
		eciDeleteDict(eciHandle, eciDict);
	}
	eciDict = eciNewDict(eciHandle);
	if (eciDict) {
		old_index = new_index;
	} else {
		old_index = MAX_NB_OF_LANGUAGES;
		DBG(DBG_MODNAME "can't create new dictionary");
		g_free(language);
		return;
	}

	/* Look for the dictionary directory */
	dirname = g_string_new(NULL);
#ifdef VOXIN
	g_string_printf(dirname, "%s/%s_%s", IbmttsDictionaryFolder, language,
			region);
	if (!g_file_test(dirname->str, G_FILE_TEST_IS_DIR)) {
		DBG(DBG_MODNAME "%s is not a directory",
		    dirname->str);
		g_string_printf(dirname, "%s/%s", IbmttsDictionaryFolder,
				language);
#else
	g_string_printf(dirname, "%s/%s", IbmttsDictionaryFolder, language);
	if (!g_file_test(dirname->str, G_FILE_TEST_IS_DIR) && dash) {
		*dash = 0;
		g_string_printf(dirname, "%s/%s", IbmttsDictionaryFolder, language);
#endif
		if (!g_file_test(dirname->str, G_FILE_TEST_IS_DIR)) {
			g_string_printf(dirname, "%s", IbmttsDictionaryFolder);
			if (!g_file_test(dirname->str, G_FILE_TEST_IS_DIR)) {
				DBG(DBG_MODNAME "%s is not a directory",
				    dirname->str);
				g_free(language);
				return;
			}
		}
	}
	g_free(language);

	DBG(DBG_MODNAME "Looking in dictionary directory %s", dirname->str);
	filename = g_string_new(NULL);

	for (i = 0; i < NB_OF_DICTIONARY_FILENAMES; i++) {
		g_string_printf(filename, "%s/%s", dirname->str,
				dictionary_filenames[i]);
		if (g_file_test(filename->str, G_FILE_TEST_EXISTS)) {
			enum ECIDictError error =
			    eciLoadDict(eciHandle, eciDict, i, filename->str);
			if (!error) {
				dictionary_is_present = 1;
				DBG(DBG_MODNAME "%s dictionary loaded",
				    filename->str);
			} else {
				DBG(DBG_MODNAME "Can't load %s dictionary (%d)",
				    filename->str, error);
			}
		} else {
			DBG(DBG_MODNAME "No %s dictionary", filename->str);
		}
	}

	g_string_free(filename, TRUE);
	g_string_free(dirname, TRUE);

	if (dictionary_is_present) {
		eciSetDict(eciHandle, eciDict);
	}
}
/* local variables: */
/* c-basic-offset: 8 */
/* end: */
