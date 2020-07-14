/***************************************************************************
 * Copyright (C) 2008-2013 by Cameron Wong                                 *
 * name in passport: HUANG GUANNENG                                        *
 * email: hgneng at gmail.com                                              *
 * website: http://www.eguidedog.net                                       *
 *                                                                         *
 * This program is free software; you can redistribute it and/or           *
 * modify it under the terms of the GNU General Public License             *
 * as published by the Free Software Foundation; either version 2          *
 * of the License, or any later version.                                   *
 *                                                                         *
 * This program is distributed in the hope that it will be useful,         *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
 * GNU General Public License for more details.                            *
 *                                                                         *
 * You should have received a copy of the GNU General Public License       *
 * along with this program; if not, write to the Free Software             *
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,              *
 * MA  02110-1301, USA.                                                    *
 **************************************************************************/
#ifndef EKHO_IMPL_H
#define EKHO_IMPL_H

#include <pthread.h>
#include <semaphore.h>
#include <sndfile.h>
#include <queue>
#include "config.h"
#include "ekho_dict.h"
#include "ekho_typedef.h"
#include "audio.h"
#include "espeak-ng/speak_lib.h"
#include "sonic.h"

#ifdef HAVE_PULSEAUDIO
#include <pulse/error.h>
#include <pulse/simple.h>
#endif

#ifdef ANDROID
#include "flite.h"
#define ENABLE_ENGLISH
#endif

#ifdef DEBUG_ANDROID
#define LOG_TAG "Ekho Engine"
#include "Log.h"
#endif

using namespace std;

namespace ekho {

typedef struct {
  string text;
  void (*pCallback)(void *);
  void *pCallbackArgs;
} SpeechOrder;

typedef int(SynthCallback)(short *pcm, int frames, void *arg, OverlapType type);

class EkhoImpl {
 public:
  const static int BUFFER_SIZE = 8192;
  const static int PENDING_PCM_FRAMES = 37192;  // 25000(fullpause) + 8192
  const static int MAX_CLIENTS = 100;
  Dict mDict;
  int mPort;
  bool supportSsml;
  bool mSpeakIsolatedPunctuation;
  bool mIsMale;
  int mOverlap;

  static SpeechdSynthCallback *speechdSynthCallback;
  void setSpeechdSynthCallback(SpeechdSynthCallback *callback);

  static bool mDebug;
#ifdef ANDROID
  cst_voice *mFliteVoice;
#endif

  static void debug(bool flag = true) {
    mDebug = flag;
    Dict::mDebug = flag;
  };

  EkhoImpl(void);
  EkhoImpl(string voice);

  /* Destructor.
  */
  ~EkhoImpl(void);

  /* Set voice
   * voice is the name of voice, which is a directory name under
   * ekho-data and should be begun with jyutping-, pinyin- or
   * hangul-. Cantonese, Mandarin, Korean are alias to jyutping,
   * pinyin, hangul.
   */
  int setVoice(string voice);
  /* Get current voice */
  string getVoice(void);

  /* Speak text
   * text should be in UTF-8 format
   * it will launch a new thread and return immediately
   */
  int speak(string text, void (*pCallback)(void *) = NULL,
            void *pCallbackArgs = NULL);

  void sing(string filepath);

  /* Clear speech queue before speak text
   * text should be in UTF-8 format
   * it will launch a new thread and return immediately
   */
  int stopAndSpeak(string text, void (*pCallback)(void *) = NULL,
                   void *pCallbackArgs = NULL);

  /* Synth speech
   * callback will be called time from time when buffer is ready
   */
  //int synth(string text, SynthCallback *callback, void *userdata = 0);
  int synth2(string text, SynthCallback *callback, void *userdata = 0);

  /* no pause is allowed
   * it will return after all sound is played
   */
  int blockSpeak(string text);

  /* play audio file with external player */
  int play(string file);

  /* output text to WAVE file */
  int saveWav(string text, string filename);

  /* output to OGG file */
  int saveOgg(string text, string filename);

  /* output to MP3 file */
  int saveMp3(string text, string filename);

  /* Pause speaking */
  int pause(void);

  /* Resume speaking */
  int resume(void);

  /* Stop speaking */
  int stop(void);

  /* start TTS server */
  int startServer(int port);

  /* request wave from Ekho TTS server */
  int request(string ip, int port, Command cmd, string text, string outfile);

  inline void setSpeakIsolatedPunctuation(bool b = true) {
    mSpeakIsolatedPunctuation = b;
  }
  inline bool getSpeakIsolatedPunctuation() {
    return mSpeakIsolatedPunctuation;
  }

  /* Set tempo delta
   * Parameter:
   *    tempo_delta (-50 .. 100, in percent)
   *    If input out of range, tempo_delta will restore to 0
   */
  void setSpeed(int tempo_delta);
  int getSpeed(void);
  void setEnglishSpeed(int delta); /* -50 .. 150 */
  int getEnglishSpeed(void);

  /* Set pitch delta
   * Parameter:
   *    pitch_delta (-100 .. 100, in percent)
   *    If input out of range, pitch_delta will restore to 0
   */
  void setPitch(int pitch_delta);
  int getPitch(void);

  /* Set volume delta
   * Parameter:
   *    volume_delta (-100 .. 100, in percent)
   *    If input out of range, volume_delta will restore to 0
   */
  void setVolume(int volume_delta);
  int getVolume(void);

  /* Set rate delta
   * Parameter:
   *    rate_delta (-50 .. 100, in percent)
   *    If input out of range, rate_delta will restore to 0
   * Return: 0
   */
  void setRate(int rate_delta);
  int getRate(void);

  /**
   * Set English Voice
   * Parameter:
   *    voice - voice_kal_diphone (default male voice) or
   *            voice_cmu_us_slt_arctic_hts (female voice) or
   *            other Festival voice name if installed
   */
  void setEnglishVoice(const char *voice) { mEnglishVoice = voice; }
  const char *getEnglishVoice(void);

  void setPcmCache(bool b) { mPcmCache = b; }

  /**
   * Check whether is speaking
   */
  inline bool isSpeaking(void) { return !mSpeechQueue.empty(); }

  string genTempFilename(void);

  sonicStream mSonicStream;

  static void *speechDaemon(void *args);
  static int writePcm(short *pcm, int frames, void *arg, OverlapType type,
                      bool tofile);
  static int writePcm(short *pcm, int frames, void *arg, OverlapType type) {
    return writePcm(pcm, frames, arg, type, true);
  }
  static int speakPcm(short *pcm, int frames, void *arg, OverlapType type) {
    return writePcm(pcm, frames, arg, type, false);
  }
  void finishWritePcm(void);
  int writeToSonicStream(short *pcm, int frames, OverlapType type);
  /*
     static int changeSamplerate(const short *source_data,
     long source_len, // len in 8 bits
     int source_rate,
     short *target_data,
     int target_rate);
     */

  /* get PCM, internal use only */
  const char *getPcmFromFestival(string text, int &size);
  void synthWithEspeak(string text);
  inline const char *getEnglishPcm(string text, int &size) {
#ifdef ENABLE_FESTIVAL
    return getPcmFromFestival(text, size);
#else
    synthWithEspeak(text);
    return 0;
#endif
  }

  void setPunctuationMode(EkhoPuncType mode) { mPuncMode = mode; }

 private:
  int init(void);
  int initPcm(void);
  int initSound(void);
  int initStream(void);
  int initEnglish(void);
  void closeStream(void);
  int outputSpeech(string text);

  bool mPcmCache;
  int tempoDelta;             // -50 .. 100 (%)
  int englishSpeedDelta;      // -50 .. 150 (%)
  int pitchDelta;             // -100 .. 100 (%)
  int volumeDelta;            // -100 .. 100 (%)
  int rateDelta;              // -50 .. 100 (%)
  const char *mEnglishVoice;  // voice_kal_diphone (default) or
                              // voice_cmu_us_slt_arctic_hts

  bool isRecording;
  bool isPaused;
  bool isStopped;
  bool isEnded;
  bool isSoundInited;
  string player;  // "ogg123", "mplayer" or "play"
  SNDFILE *mSndFile;
  queue<SpeechOrder> mSpeechQueue;
  pthread_mutex_t mSpeechQueueMutex;
  pthread_cond_t mSpeechQueueCond;

  short mPendingPcm[PENDING_PCM_FRAMES * 2];
  int mPendingFrames;

  bool isSpeechThreadInited;
  pthread_t speechThread;
  pthread_attr_t speechThreadAttr;

#ifdef HAVE_PULSEAUDIO
  pa_simple *stream;
#endif
  Audio *audio;

  const char *mAlphabetPcmCache[26];
  int mAlphabetPcmSize[26];

  EkhoPuncType mPuncMode;

  void filterSpaces(string &text);
  void translatePunctuations(string &text);
};
}
#endif
