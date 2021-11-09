/*
 * ekho.cpp - Speech Dispatcher backend for Ekho
 *
 * Copyright (C) 2012-2021 Cameron Wong (hgneng at gmail.com)
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


#include "spd_audio.h"
#include <speechd_types.h>
#include "module_utils.h"

#define HAVE_PULSEAUDIO 1
#include <ekho.h>

using namespace ekho;

#define MODULE_NAME     "ekho"
#define MODULE_VERSION  "8.8"

#define DEBUG_MODULE 1
DECLARE_DEBUG();

static SPDMessageType ekho_message_type;

static int ekho_position = 0;
static int pause_requested = 0;
static int stop_requested = 0;
static int began = 0;

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
static void ekho_set_cap_let_recogn(SPDCapitalLetters punct_mode);

/* Public functions */

int module_load(void) {
  DBG(MODULE_NAME " module_load");

  INIT_SETTINGS_TABLES();
  REGISTER_DEBUG();
  return 0;
}

int module_init(char **status_info) {
    int ret;

    DBG(MODULE_NAME " module_init");

    module_audio_set_server();

    *status_info = NULL;

    if (gpEkho) {
      DBG("already init. return");
      return 0;
    }

    Ekho::debug(true);
    gpEkho = new Ekho();
    gpEkho->setSpeakIsolatedPunctuation();
    module_list_voices();

    *status_info = g_strdup("ekho initialized successfully.");

    return 0;
}

SPDVoice** module_list_voices(void) {
  DBG(MODULE_NAME " module_list_voices");

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

int ekho_callback(short *wav, int samples, int bits, int channels, int samplerate, int event) {
  DBG("ekho_callback: samples=%d, event=%d", samples, event);

  /* Process server events in case we were told to stop in between */
  module_process(STDIN_FILENO, 0);

  if (stop_requested)
    return 1;

  if (pause_requested)
    return 1;

  if (!began) {
    began = 1;
    module_report_event_begin();
  }

  if (samples > 0) {
    AudioTrack track = {
      .bits = bits,
      .num_channels = channels,
      .sample_rate = samplerate,
      .num_samples = samples,
      .samples = wav,
    };

    module_tts_output_server(&track, SPD_AUDIO_LE);
  } else if (event == 1) {
    // Indicate this speech finish. Without this next speech will not begin.
    //module_speak_queue_add_end();
    module_report_event_end();
  }

  if (stop_requested) {
    return 1;
  }

  return 0;
}

/*
 * int mycallback_mark(const char *mark)
 * {
 *  if (module_speak_queue_stop_requested()) {
 *    return STOP;
 *  }
 *  module_speak_queue_before_play();
 *  module_speak_queue_add_mark(mark);
 * }
 */

void module_speak_sync(const gchar *data, size_t bytes, SPDMessageType msgtype) {
  DBG("module_speak_sync(%s, %ld, %d)\n", data, bytes, msgtype);

  /* Setting voice */
	UPDATE_STRING_PARAMETER(voice.language, ekho_set_language);
  UPDATE_PARAMETER(voice_type, ekho_set_voice);
  UPDATE_STRING_PARAMETER(voice.name, ekho_set_synthesis_voice);
  UPDATE_PARAMETER(rate, ekho_set_rate);
  UPDATE_PARAMETER(volume, ekho_set_volume);
  UPDATE_PARAMETER(pitch, ekho_set_pitch);
	UPDATE_PARAMETER(punctuation_mode, ekho_set_punctuation_mode);
  UPDATE_PARAMETER(cap_let_recogn, ekho_set_cap_let_recogn);

  began = 0;
  stop_requested = 0;
  pause_requested = 0;

  module_speak_ok();
  
  string s;
  if (msgtype == SPD_MSGTYPE_KEY) {
    // fix issues of always speak some symbols in English
    if (strcmp(data, "double-quote") == 0) {
      gpEkho->synth("\"", ekho_callback);
    } else if (strcmp(data, "underscore") == 2) {
      gpEkho->synth("_", ekho_callback);
    } else {
      gpEkho->synth(data, ekho_callback);
    }
  } else if (msgtype == SPD_MSGTYPE_SOUND_ICON) {
    char *msg =
        g_strdup_printf("<audio src=\"%s%s\">%s</audio>",
            "/usr/share/sounds/sound-icons/", data, data);
    gpEkho->synth(msg, ekho_callback);
    g_free(msg);
  } else {
    gpEkho->synth(data, ekho_callback);
  }
}

void module_speak_queue_cancel(void) {
  DBG("module_speak_queue_cancel");
  gpEkho->stop();
}

int module_stop(void) {
  DBG("module_stop");
  stop_requested = 1;
  //gpEkho->stop();
  return 0;
}

size_t module_pause(void) {
  DBG("module_pause");
  pause_requested = 1;
  //gpEkho->pause();
  return 0;
}

int module_close(void) {
  DBG("module_close");
  delete gpEkho;
  gpEkho = 0;

  if (gpVoices != NULL) {
    int i;
    for (i = 0; gpVoices[i] != NULL; i++) {
      g_free(gpVoices[i]->name);
      g_free(gpVoices[i]->language);
      g_free(gpVoices[i]->variant);
      g_free(gpVoices[i]);
    }
    g_free(gpVoices);
    gpVoices = NULL;
  }

  return 0;
}

static void ekho_set_rate(signed int rate) {
  DBG("ekho_set_rate");
  if (rate < 0) {
    gpEkho->setSpeed(rate / 2);
  } else {
    gpEkho->setSpeed(rate * 3);
  }
}

static void ekho_set_volume(signed int volume) {
  DBG("ekho_set_volume");
  gpEkho->setVolume(volume);
}

static void ekho_set_pitch(signed int pitch) {
  assert(pitch >= -100 && pitch <= +100);
  DBG("ekho_set_pitch(%d)", pitch);
  gpEkho->setPitch(pitch);
}

static void ekho_set_voice(SPDVoiceType voice) {
  DBG("ekho_set_voice: %d", voice);
}

static void ekho_set_language(char *lang) {
  static bool visited = false;
  if (!visited) {
    visited = true;
    DBG("ekho_set_language: %s", lang);
    gpEkho->setVoice(lang);
  }
}

static void ekho_set_synthesis_voice(char *synthesis_voice) {
  if (synthesis_voice) {
    DBG("ekho_set_synthesis_voice: %s\n", synthesis_voice);
  }
}

static void ekho_set_punctuation_mode(SPDPunctuation punct_mode) {
  DBG("ekho_set_punctuation_mode: %d\n", punct_mode);
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

static void ekho_set_cap_let_recogn(SPDCapitalLetters cap_mode)
{
  DBG("ekho_set_cap_let_recogn");
  gpEkho->setCapLetterRecognMode((EkhoCapLetterRecognType)cap_mode);
}
