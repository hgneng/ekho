/*
 * ekho.cc - Speech Dispatcher backend for Ekho
 *
 * Copyright (C) 2012-2013 Cameron Wong (hgneng at gmail.com)
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
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 */

extern "C" {
#include "spd_audio.h"
#include <speechd_types.h>
#include "module_utils.h"
//#include "module_utils_speak_queue.h"
}
#define HAVE_PULSEAUDIO 1
#include <ekho.h>

using namespace ekho;

#define MODULE_NAME     "ekho"
#define MODULE_VERSION  "8.2"

#define DEBUG_MODULE 1
DECLARE_DEBUG();

static char **ekho_message;
static SPDMessageType ekho_message_type;

static int ekho_position = 0;
static int ekho_pause_requested = 0;

signed int ekho_volume = 0;

static Ekho *gpEkho = NULL;
static SPDVoice **gpVoices = NULL;

/* Internal functions prototypes */
static void ekho_set_rate(signed int rate);
static void ekho_set_pitch(signed int pitch);
static void ekho_set_volume(signed int pitch);
static void ekho_set_voice(SPDVoiceType voice);
static void ekho_set_language(char *language);
static void ekho_set_synthesis_voice(char *voice);
static void ekho_set_punctuation_mode(SPDPunctuation punct_mode);

/* Public functions */

extern "C"
int module_load(void) {
   INIT_SETTINGS_TABLES();
   REGISTER_DEBUG();
   return 0;
}

#define ABORT(msg) g_string_append(info, msg); \
        DBG("FATAL ERROR:", info->str); \
	*status_info = info->str; \
	g_string_free(info, 0); \
	return -1;

extern "C"
int module_init(char **status_info) {
    int ret;

    DBG("Module init");
    INIT_INDEX_MARKING();

    *status_info = NULL;

    if (gpEkho)
      return 0;

    gpEkho = new Ekho();
    gpEkho->setStripSsml();
    gpEkho->setSpeakIsolatedPunctuation();

    ekho_message = (char**)malloc (sizeof (char*));
    *ekho_message = NULL;

    // TODO: is this necessary?
    /*
    ret = module_speak_queue_init(441000, status_info);
    if (ret != 0)
      return ret;*/

    *status_info = strdup("ekho initialized successfully.");

    return 0;
}

#undef ABORT

SPDVoice** module_list_voices(void) {
  if (!gpVoices) {
    gpVoices = g_new0(SPDVoice*, 9);

    // Cantonese
    gpVoices[0] = g_new0(SPDVoice, 1);
    gpVoices[0]->name = g_strdup((char*)"wong");
    gpVoices[0]->language = g_strdup((char*)"Cantonese");
    gpVoices[0]->variant = g_strdup((char*)"Guangzhou");

    // Mandarin
    gpVoices[1] = g_new0(SPDVoice, 1);
    gpVoices[1]->name = g_strdup((char*)"yali");
    gpVoices[1]->language = g_strdup((char*)"Mandarin");
    gpVoices[1]->variant = g_strdup((char*)"standard");

    // Hakka
    gpVoices[2] = g_new0(SPDVoice, 1);
    gpVoices[2]->name = g_strdup((char*)"weicheng");
    gpVoices[2]->language = g_strdup((char*)"Hakka");
    gpVoices[2]->variant = g_strdup((char*)"Zhaoan");

    // Tibetan
    gpVoices[3] = g_new0(SPDVoice, 1);
    gpVoices[3]->name = g_strdup((char*)"trinley");
    gpVoices[3]->language = g_strdup((char*)"Tibetan");
    gpVoices[3]->variant = g_strdup((char*)"Lhasa");

    // Yayan
    gpVoices[4] = g_new0(SPDVoice, 1);
    gpVoices[4]->name = g_strdup((char*)"qianshan");
    gpVoices[4]->language = g_strdup((char*)"Ngangien");
    gpVoices[4]->variant = g_strdup((char*)"standard");

    // Korean
    gpVoices[5] = g_new0(SPDVoice, 1);
    gpVoices[5]->name = g_strdup((char*)"haesung");
    gpVoices[5]->language = g_strdup((char*)"Korean");
    gpVoices[5]->variant = g_strdup((char*)"standard");

    // English
    gpVoices[6] = g_new0(SPDVoice, 1);
    gpVoices[6]->name = g_strdup((char*)"kal");
    gpVoices[6]->language = g_strdup((char*)"English");
    gpVoices[6]->variant = g_strdup((char*)"American English");

    // Mandarin
    gpVoices[7] = g_new0(SPDVoice, 1);
    gpVoices[7]->name = g_strdup((char*)"yali+kal");
    gpVoices[7]->language = g_strdup((char*)"Mandarin");
    gpVoices[7]->variant = g_strdup((char*)"standard");

    gpVoices[8] = 0;
  }

  return gpVoices;
}

extern "C"
void ekho_callback(void*) {
}

extern "C"
int module_speak(gchar *data, size_t bytes, SPDMessageType msgtype) {
  DBG("write(%s, %ld, %d)\n", data, bytes, msgtype);

//  if(module_write_data_ok(data) != 0) return -1;

  DBG("Requested data: |%s|\n", data);

  if (*ekho_message != NULL) {
    free(*ekho_message);
    *ekho_message = NULL;
  }
  // TODO: support SSML
  //*ekho_message = module_strip_ssml(data);
  ekho_message_type = SPD_MSGTYPE_TEXT;

  /* Setting voice */
	UPDATE_STRING_PARAMETER(voice.language, ekho_set_language);  
  UPDATE_PARAMETER(voice_type, ekho_set_voice);
  UPDATE_STRING_PARAMETER(voice.name, ekho_set_synthesis_voice);
  UPDATE_PARAMETER(rate, ekho_set_rate);
  UPDATE_PARAMETER(volume, ekho_set_volume);
  UPDATE_PARAMETER(pitch, ekho_set_pitch);
	UPDATE_PARAMETER(punctuation_mode, ekho_set_punctuation_mode);

  DBG("ekho: leaving write() normally\n\r");
  if (msgtype == SPD_MSGTYPE_KEY) {
    // fix issues of always speak some symbols in English
    if (strcmp(data, "double-quote") == 0) {
      gpEkho->speak("\"", ekho_callback);
    } else if (strcmp(data, "underscore") == 0) {
      gpEkho->speak("_", ekho_callback);
    } else {
      gpEkho->speak(data, ekho_callback);
    }
  } else {
    gpEkho->speak(data, ekho_callback);
  }

  /* Send semaphore signal to the speaking thread */

  return bytes;
}

extern "C"
int module_stop(void) {
  DBG("ekho: stop()\n");
  gpEkho->stop();
  return 0;
}

extern "C"
size_t module_pause(void) {
  DBG("pause requested\n");
  DBG("ekho doesn't support pause, stopping\n");
  gpEkho->pause();
  return 0;
}

extern "C"
int module_close(void) {
  int i = 0;

  DBG("ekho: close()\n");
  DBG("Stopping speech");
  module_stop();
  DBG("Closing audio output");

  // free gpVoices
  if (gpVoices) {
    for (; i < 5; i++) {
      if (gpVoices[i]) {
        g_free(gpVoices[i]);
        gpVoices[i] = 0;
      }
    }
  }
  gpVoices = 0;
  gpEkho = 0;

  return 0;
}

static void ekho_set_rate(signed int rate) {
  if (rate < 0) {
    gpEkho->setSpeed(rate / 2);
  } else {
    gpEkho->setSpeed(rate * 3);
  }
}

static void ekho_set_volume(signed int volume) {
  gpEkho->setVolume(volume);
}

static void ekho_set_pitch(signed int pitch) {
  assert(pitch >= -100 && pitch <= +100);
  gpEkho->setPitch(pitch);
}

static void ekho_set_voice(SPDVoiceType voice) {
  DBG("Voice: %d\n", voice);
}

static void ekho_set_language(char *lang) {
  static bool visited = false;
  if (!visited) {
    visited = true;
    DBG("ekho_set_language: %s", lang);
    gpEkho->setVoice(lang);
    if (strcmp(msg_settings.voice.name, "yali+kal") == 0) {
      gpEkho->setEnglishVoice("voice_kal_diphone");
    }
  }
}

static void ekho_set_synthesis_voice(char *synthesis_voice) {
  if (synthesis_voice) {
    DBG("Voice: %s\n", synthesis_voice);
  }
}

static void ekho_set_punctuation_mode(SPDPunctuation punct_mode) {
  DBG("Punctuation mode: %d\n", punct_mode);
	EkhoPuncType mode = EKHO_PUNC_SOME;
	switch (punct_mode)  {
	case SPD_PUNCT_ALL:
		mode = EKHO_PUNC_ALL;
		break;
	case SPD_PUNCT_SOME:
		mode = EKHO_PUNC_SOME;
		break;
	case SPD_PUNCT_NONE:
		mode = EKHO_PUNC_NONE;
    break;
  }

  gpEkho->setPunctuationMode(mode);
}
