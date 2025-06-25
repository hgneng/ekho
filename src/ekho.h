/***************************************************************************
 * Copyright (C) 2008-2024 by Cameron Wong                                 *
 * name in passport: HUANG GUANNENG                                        *
 * email: hgneng at gmail.com                                              *
 * website: https://eguidedog.net                                       *
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
#ifndef EKHO_H
#define EKHO_H

#include <pthread.h>
#include <semaphore.h>
#include <sndfile.h>
#include <queue>
#include <map>
#include "config.h"

#ifdef DEBUG_ANDROID
#define LOG_TAG "Ekho Engine"
#include "Log.h"
#endif

#define ENABLE_ENGLISH

#ifdef ANDROID
#define ENABLE_FLITE
#else
#define ENABLE_ESPEAK
#endif

#ifdef ENABLE_FLITE
#include "flite.h"
#endif

#include "ekho_dict.h"
#include "ekho_typedef.h"
#include "ekho_impl.h"

#ifdef HAVE_SONIC
#include "sonic.h"
#endif

#ifdef HAVE_PULSEAUDIO
#include <pulse/error.h>
#include <pulse/simple.h>
#endif

#ifdef HAVE_LIBSOXR
#include <soxr.h>
#endif

using namespace std;

namespace ekho {

typedef int (t_ekho_sync_callback)(short*, int);
//  typedef int(SynthCallback)(short *pcm, int frames, void *arg = NULL,
//                             OverlapType type = OVERLAP_QUIET_PART);
typedef int(SynthCallback)(short* pcm, int frames, void* arg, OverlapType type);

class Ekho {
  private:
    EkhoImpl* m_pImpl;
    int musicxmlMinuteRate;
    SNDFILE* sndFile;
    map<string, double> pitchMap;
    map<string, double> pianoPitchMap;

    long getAvailableMemory();
    bool checkEmotiVoiceServerStarted();
    bool checkZhttsServerStarted();

  public:
    const static int BUFFER_SIZE = 40960;
    const static int PENDING_PCM_FRAMES = 20480;
    const static int MAX_CLIENTS = 100;
    const static int EMOTIVOICE_PORT = 20491;
    const static constexpr float EMOTIVOICE_AMPLIFY_RATE = 2;
    const static int ZHTTS_PORT = 20501;
    const static constexpr float ZHTTS_AMPLIFY_RATE = 1;
    const static int COQUI_PORT = 20492;
    const static constexpr float COQUI_AMPLIFY_RATE = 1;
    const static int COQUI_SAMPLE_RATE = 22050;

    static bool mDebug;
    static void debug(bool flag = true);
    static SynthCallback* synthCallback; // callback for synth2
    static SynthCallback* synth4Callback;
    static EkhoImpl* impl;
    static bool emotiVoiceEnabled;
    static bool zhttsEnabled;
    static bool coquiEnabled;

    Ekho();
    Ekho(string voice);
    void init();

    /* Destructor.
    */
    ~Ekho();

    /* Set voice
     * voice is the name of voice, which is a directory name under
     * ekho-data and should be begun with jyutping-, pinyin- or
     * hangul-. Cantonese, Mandarin, Korean are alias to jyutping,
     * pinyin, hangul.
     */
    int setVoice(string voice);
    /* Get current voice */
    string getVoice(void);

    Dict& getDict(void);

    /* Speak text
     * text should be in UTF-8 format
     * it will launch a new thread and return immediately
     */
    int speak(string text, void (*pCallback)(void*) = NULL,
              void* pCallbackArgs = NULL);

    int synth(const char* text, SpeechdSynthCallback* callback);

    void singMusicXml(const string xmlFile, const string outputFile);
    void singCharacter(const Character& c, int duration, string pitch);
    void singSilence(int duration);
    char* convertDurationAndPitch(const char* pcm, int size,
      float tempo, float pitch, int& convertedSize);
    double detectPitch(const short* pcm, int size, int sampleRate);
    int loadPitchFile();
    void loadPianoPitch();

    /* Clear speech queue before speak text
     * text should be in UTF-8 format
     * it will launch a new thread and return immediately
     */
    int stopAndSpeak(string text, void (*pCallback)(void*) = NULL,
                     void* pCallbackArgs = NULL);

    short* synth3(string text, int& pcmSize);

    /* Synth speech for Android
     * callback will be called time from time when buffer is ready
     */
    int synth4(string text, SynthCallback* callback, void* userdata);
    static int postProcess(short* pcm, int frames, void* arg, OverlapType type);

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

    /**
     * Set whether support SSML tags in text
     */
    void enableSsml(); // default
    void disableSsml();

    bool enableEmotiVoice(bool autoStart = true); // use EmotiVoice to synthesize Mandarin
    bool enableZhtts(bool autoStart = true); // use zhtts to synthesize Mandarin

    void setSpeakIsolatedPunctuation(bool b = true);
    bool getSpeakIsolatedPunctuation();

    void setOverlap(int overlap);

    /* Set tempo delta
     * Parameter:
     *    tempo_delta (-50 .. 100, in percent)
     *    If input out of range, tempo_delta will restore to 0
     */
    void setSpeed(int tempo_delta);
    int getSpeed(void);

    // setEnglishSpeed should be called after setVoice
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

    // 需要在setVoice之前调用，因为setVoice的时候就初始化完成音频流的samplerate了。
    void setSampleRate(int sampleRate);
    /* Get sample rate
     * The default rate depends on voice data. For gsm format, it is usually 8000. 44100 is CD quality.
     */
    int getSampleRate(void);

    void setChannels(int channels);

    /**
     * Set English Voice
     * Parameter:
     *    voice - voice_kal_diphone (default male voice) or
     *            voice_cmu_us_slt_arctic_hts (female voice) or
     *            other Festival voice name if installed
     */
    void setEnglishVoice(const char* voice);
    const char* getEnglishVoice(void);

    void setPcmCache(bool b);

    /**
     * Check whether is speaking
     */
    bool isSpeaking();

#ifdef HAVE_SONIC
    sonicStream mSonicStream;
#endif

    static void* speechDaemon(void* args);
    static int writePcm(short* pcm, int frames, void* arg, OverlapType type);
    void finishWritePcm();
    int writeToSonicStream(short* pcm, int frames, OverlapType type);
    void setPunctuationMode(EkhoPuncType mode);
    void setCapLetterRecognMode(EkhoCapLetterRecognType mode);
};
}

#endif
