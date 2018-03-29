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
#include "fdset.h"
#include "module_utils.h"
}
#define HAVE_PULSEAUDIO 1
#include <ekho.h>

using namespace ekho;

#define MODULE_NAME     "ekho"
#define MODULE_VERSION  "7.6"

#define DEBUG_MODULE 1
DECLARE_DEBUG();

/* Thread and process control */
static int ekho_speaking = 0;

static pthread_mutex_t ekho_stop_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t ekho_stop_cond = PTHREAD_COND_INITIALIZER;

static pthread_t ekho_speak_thread;
static sem_t *ekho_semaphore;

static char **ekho_message;
static EMessageType ekho_message_type;

static int ekho_position = 0;
static int ekho_pause_requested = 0;

signed int ekho_volume = 0;

static Ekho *gpEkho = NULL;
static VoiceDescription **gpVoices = NULL;

/* Internal functions prototypes */
static void ekho_set_rate(signed int rate);
static void ekho_set_pitch(signed int pitch);
static void ekho_set_volume(signed int pitch);
static void ekho_set_voice(EVoiceType voice);
static void ekho_set_language(char *language);
static void ekho_set_synthesis_voice(char *voice);
static void ekho_set_punctuation_mode(EPunctMode punct_mode);

static void* _ekho_speak(void*);

int ekho_stop = 0;

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

    ekho_semaphore = module_semaphore_init();

    DBG("ekho: creating new thread for ekho_speak\n");
    ekho_speaking = 0;
    ret = pthread_create(&ekho_speak_thread, NULL, _ekho_speak, NULL);
    if (ret != 0) {
        DBG("ekho: thread failed\n");
        *status_info = strdup("The module couldn't initialize threads "
			      "This could be either an internal problem or an "
			      "architecture problem. If you are sure your architecture "
			      "supports threads, please report a bug.");
        return -1;
    }

    module_audio_id = NULL;

    *status_info = strdup("ekho initialized successfully.");

    return 0;
}

#undef ABORT

extern "C"
int module_audio_init(char **status_info) {
  DBG("Opening audio");
  return 0;
}


VoiceDescription** module_list_voices(void) {
  if (!gpVoices) {
    gpVoices = g_new0(VoiceDescription*, 9);

    // Cantonese
    gpVoices[0] = g_new0(VoiceDescription, 1);
    gpVoices[0]->name = "wong";
    gpVoices[0]->language = "Cantonese";
    gpVoices[0]->dialect = "Guangzhou";

    // Mandarin
    gpVoices[1] = g_new0(VoiceDescription, 1);
    gpVoices[1]->name = "yali";
    gpVoices[1]->language = "Mandarin";
    gpVoices[1]->dialect = "standard";

    // Hakka
    gpVoices[2] = g_new0(VoiceDescription, 1);
    gpVoices[2]->name = "weicheng";
    gpVoices[2]->language = "Hakka";
    gpVoices[2]->dialect = "Zhaoan";

    // Tibetan
    gpVoices[3] = g_new0(VoiceDescription, 1);
    gpVoices[3]->name = "trinley";
    gpVoices[3]->language = "Tibetan";
    gpVoices[3]->dialect = "Lhasa";

    // Yayan
    gpVoices[4] = g_new0(VoiceDescription, 1);
    gpVoices[4]->name = "qianshan";
    gpVoices[4]->language = "Ngangien";
    gpVoices[4]->dialect = "standard";

    // Korean
    gpVoices[5] = g_new0(VoiceDescription, 1);
    gpVoices[5]->name = "haesung";
    gpVoices[5]->language = "Korean";
    gpVoices[5]->dialect = "standard";

    // English
    gpVoices[6] = g_new0(VoiceDescription, 1);
    gpVoices[6]->name = "kal";
    gpVoices[6]->language = "English";
    gpVoices[6]->dialect = "American English";

    // Mandarin
    gpVoices[7] = g_new0(VoiceDescription, 1);
    gpVoices[7]->name = "yali+kal";
    gpVoices[7]->language = "Mandarin";
    gpVoices[7]->dialect = "standard";

    gpVoices[8] = 0;
  }

  return gpVoices;
}

extern "C"
void ekho_callback(void*) {
  pthread_mutex_lock(&ekho_stop_mutex);
  pthread_cond_signal(&ekho_stop_cond);
  pthread_mutex_unlock(&ekho_stop_mutex);
}

extern "C"
int module_speak(gchar *data, size_t bytes, EMessageType msgtype) {
  DBG("write(%s, %d, %d)\n", data, bytes, msgtype);

  if (ekho_speaking) {
    DBG("Speaking when requested to write");
    return 0;
  }

//  if(module_write_data_ok(data) != 0) return -1;

  DBG("Requested data: |%s|\n", data);

  if (*ekho_message != NULL) {
    xfree(*ekho_message);
    *ekho_message = NULL;
  }
  // TODO: support SSML
  //*ekho_message = module_strip_ssml(data);
  ekho_message_type = MSGTYPE_TEXT;

  /* Setting voice */
	UPDATE_STRING_PARAMETER(language, ekho_set_language);  
  UPDATE_STRING_PARAMETER(synthesis_voice, ekho_set_synthesis_voice);
  UPDATE_PARAMETER(voice, ekho_set_voice);
  UPDATE_PARAMETER(rate, ekho_set_rate);
  UPDATE_PARAMETER(volume, ekho_set_volume);
  UPDATE_PARAMETER(pitch, ekho_set_pitch);
	UPDATE_PARAMETER(punctuation_mode, ekho_set_punctuation_mode);

  DBG("ekho: leaving write() normally\n\r");
  if (msgtype == MSGTYPE_KEY) {
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
  ekho_speaking = 1;    
  sem_post(ekho_semaphore);    

  return bytes;
}

extern "C"
int module_stop(void) {
  int ret;
  DBG("ekho: stop()\n");

  ekho_stop = 1;
  gpEkho->stop();

  pthread_mutex_lock(&ekho_stop_mutex);
  pthread_cond_signal(&ekho_stop_cond);
  pthread_mutex_unlock(&ekho_stop_mutex);

  return 0;
}

extern "C"
size_t module_pause(void) {
  DBG("pause requested\n");
  if (ekho_speaking) {
    DBG("ekho doesn't support pause, stopping\n");
    gpEkho->pause();
  }
  return 0;
}

extern "C"
void module_close(int status) {
  int i = 0;

  DBG("ekho: close()\n");

  DBG("Stopping speech");
  if (ekho_speaking) {
    module_stop();
  }

  DBG("Terminating threads");
  if (module_terminate_thread(ekho_speak_thread) != 0) {
    exit(1);
  }

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

  exit(status);
}

/* Internal functions */
void* _ekho_speak(void* nothing) {	
  AudioTrack track;
  unsigned int pos;
  int bytes;
  int ret;

  DBG("ekho: speaking thread starting.......\n");

  set_speaking_thread_parameters();

  while (1) {
    sem_wait(ekho_semaphore);
    DBG("Semaphore on\n");

    ekho_stop = 0;
    ekho_speaking = 1;

//    spd_audio_set_volume(module_audio_id, ekho_volume);

    pos = 0;
    module_report_event_begin();
    while (1) {
      pthread_mutex_lock(&ekho_stop_mutex);
      pthread_cond_wait(&ekho_stop_cond, &ekho_stop_mutex);
      if (ekho_stop) {
        module_report_event_stop();
      } else {
        module_report_event_end();
      }
      ekho_speaking = 0;
      pthread_mutex_unlock(&ekho_stop_mutex);
      break;
      /*
      if (ekho_stop){
        DBG("Stop in child, terminating");
        ekho_speaking = 0;
        module_report_event_stop();
        break;
      }

      if (!gpEkho->isSpeaking()){
        DBG("End of message");
        ekho_speaking = 0;
        module_report_event_end();
        break;
      }

      if (ekho_pause_requested && (current_index_mark!=-1)){
        DBG("Pause requested in parent, position %d\n", current_index_mark);
        ekho_pause_requested = 0;
        ekho_position = current_index_mark;
        break;
      }
      */
    }
    ekho_stop = 0;
  }

  ekho_speaking = 0;

  DBG("ekho: speaking thread ended.......\n");

  pthread_exit(NULL);
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

static void ekho_set_voice(EVoiceType voice) {
  DBG("Voice: %d\n", voice);
}

static void ekho_set_language(char *lang) {
  static bool visited = false;
  if (!visited) {
    visited = true;
    DBG("ekho_set_language: %s", lang);
    gpEkho->setVoice(lang);
    if (msg_settings.synthesis_voice &&
        strcmp(msg_settings.synthesis_voice, "yali+kal") == 0) {
      gpEkho->setEnglishVoice("voice_kal_diphone");
    }
  }
}

static void ekho_set_synthesis_voice(char *synthesis_voice) {
  if (synthesis_voice) {
    DBG("Voice: %s\n", synthesis_voice);
  }
}

static void ekho_set_punctuation_mode(EPunctMode punct_mode) {
  DBG("Punctuation mode: %d\n", punct_mode);
	EkhoPuncType mode = EKHO_PUNC_SOME;
	switch (punct_mode)  {
	case PUNCT_ALL:
		mode = EKHO_PUNC_ALL;
		break;
	case PUNCT_SOME:
		mode = EKHO_PUNC_SOME;
		break;
	case PUNCT_NONE:
		mode = EKHO_PUNC_NONE;
    break;
  }

  gpEkho->setPunctuationMode(mode);
}

#include "module_main.c"
