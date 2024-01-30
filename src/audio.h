/***************************************************************************
 * Copyright (C) 2008-2022 by Cameron Wong                                 *
 * name in passport: HUANG GUANNENG                                        *
 * email: hgneng at gmail.com                                              *
 * website: https://eguidedog.net                                          *
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

#ifndef EKHO_AUDIO_H
#define EKHO_AUDIO_H
#include <string>
#include "ekho_typedef.h"
#include "config.h"

#ifdef HAVE_SONIC
#include "sonic.h"
#endif

#ifdef HAVE_PULSEAUDIO
#include <pulse/error.h>
#include <pulse/simple.h>
#endif

#ifdef HAVE_MPG123
#include <mpg123.h>
#endif

using namespace std;

namespace ekho {
class Audio {
  public:
    Audio(void);
    ~Audio(void);
  
    static bool debug;
    static string tempDirectory;
#ifdef HAVE_SONIC
    sonicStream processorStream;
#endif
    short* pcmFrameBuffer;
    int pcmFrameMaxSize;
    int pcmFrameCurrentSize;
    int pitchDelta;
    int volumeDelta;
    int rateDelta;
    int tempoDelta;
    int sampleRate; // Ekho voice source sample rate
    int currentSampleRate; // sonic processing sample (change for espeak)
    int outputSampleRate; // for readShortFrames
    int channels; // default channels should be 1
    SpeechdSynthCallback* speechdSynthCallback;

#ifdef HAVE_PULSEAUDIO
    pa_simple* pulseAudio;
    void initPulseAudio();
    void destroyPulseAudio();
    void pulseAudioDrain();
    void pulseAudioFlush();
    int pulseAudioWrite(const void* buffer, size_t bytes);
#endif

    // processor
    void initProcessor();
    void initProcessor(int samplerate, int channels);
    void destroyProcessor();
    int setPitch(int delta);
    void setPitchFloat(float factor);
    int setVolume(int delta);
    int setRate(int delta);
    int setTempo(int delta);
    void setTempoFloat(float factor);
    int setSampleRate(int rate);
    void setInputSampleRate(int rate);
    void setOutputSampleRate(int rate);
    void setChannels(int channels);
    int readShortFrames(short buffer[], int size);
    int writeShortFrames(short buffer[], int size);
    void flushFrames();

    // player
    void play(const string& path);

    // tools
    static void setTempDirectory(string dir);
    static string genTempFilename();

    void initMp3Processor();
    // It's caller's responsibility to delete return short space
    short* readPcmFromAudioFile(string filepath, int& size);
    short* readPcmFromMp3File(string filepath, int& size);
    short* amplifyPcm(short* pcm, int size, float rate);

  private:
    bool hasProcessorInited;
#ifdef HAVE_MPG123
    mpg123_handle* mpg123Handle;
#endif
};
}
#endif