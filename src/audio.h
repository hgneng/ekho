/***************************************************************************
 * Copyright (C) 2008-2020 by Cameron Wong                                 *
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

#ifndef EKHO_AUDIO_H
#define EKHO_AUDIO_H
#include <string>
#include "ekho_typedef.h"
#include "config.h"
#include "sonic.h"

#ifdef HAVE_PULSEAUDIO
#include <pulse/error.h>
#include <pulse/simple.h>
#endif

using namespace std;

namespace ekho {
class Audio {
  public:
    ~Audio(void);
  
    static bool debug;
    sonicStream processorStream = 0;
    int pitchDelta = 0;
    int volumeDelta = 0;
    int rateDelta = 0;
    int tempoDelta = 0;
    int sampleRate = 0;
    int currentSampleRate = 0;
    int channels = 0;
    SpeechdSynthCallback *speechdSynthCallback = 0;

#ifdef HAVE_PULSEAUDIO
    pa_simple *pulseAudio = 0;
#endif

    // processor
    void initProcessor(int samplerate, int channels);
    void destroyProcessor();
    int setPitch(int delta);
    int setVolume(int delta);
    int setRate(int delta);
    int setTempo(int delta);
    void setTempoFloat(float value);
    int setSampleRate(int rate);
    int readShortFrames(short buffer[], int size);
    int writeShortFrames(short buffer[], int size);
    void flushFrames();

    // player
    void play(const string& path);

    // tools
    static string genTempFilename();

  private:
    bool hasProcessorInited = false;
};
}
#endif