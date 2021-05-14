/***************************************************************************
 * Copyright (C) 2008-2021 by Cameron Wong                                 *
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

#include <iostream>
#include <string>
#include <sndfile.h>
#include "audio.h"
using namespace std;
using namespace ekho;

bool Audio::debug = false;

Audio::~Audio(void) {
#ifndef ENABLE_SOUNDTOUCH
  if (this->processorStream) {
    sonicDestroyStream(this->processorStream);
    this->processorStream = 0;
  }
#endif
}

void Audio::initProcessor(int samplerate, int channels) {
  this->sampleRate = samplerate;
  this->channels = channels;

#ifdef ENABLE_SOUNDTOUCH
  this->pSoundtouch.setSampleRate(mDict.mSfinfo.samplerate);
  this->pSoundtouch.setChannels(1);
  this->pSoundtouch.setSetting(SETTING_USE_QUICKSEEK, 1);
#else
  this->processorStream = sonicCreateStream(samplerate, 1);
  sonicSetQuality(this->processorStream, 1); // high quality but slower
#endif

  this->hasProcessorInited = true;
}

void Audio::destroyProcessor() {
  if (this->processorStream) {
    sonicDestroyStream(this->processorStream);
    this->processorStream = 0;
  }
}

int Audio::setTempo(int delta) {
#ifdef ENABLE_SOUNDTOUCH
  if (delta >= -50 && delta <= 300) {
    this->tempoDelta = delta;
  } else {
    this->tempoDelta = 0;
  }
  this->pSoundtouch.setTempoChange(this->tempoDelta);
#else
  if (delta >= -50 && delta <= 300) {
    if (this->processorStream) {
      sonicSetSpeed(this->processorStream, (float)(100 + delta) / 100);
    }
    this->tempoDelta = delta;
  }
#endif

  return this->tempoDelta;
}

int Audio::setSampleRate(int rate) {
  flushFrames();
  sonicSetRate(this->processorStream, (float)rate / this->sampleRate);
  this->currentSampleRate = rate;
  return rate;
}

int Audio::setPitch(int delta) {
  if (!hasProcessorInited) {
    cerr << "Audio processor has not initialized." << endl;
    return 0;
  }

  if (Audio::debug) {
    cerr << "Audio::setPitch(" << delta << ")" << endl;
  }

#ifdef ENABLE_SOUNDTOUCH
  if (delta >= -100 && delta <= 100) {
    this->pitchDelta = delta;
  } else {
    this->pitchDelta = 0;
  }
  this->pSoundtouch.setPitchOctaves((float)this->pitchDelta / 100);
#else
  if (this->processorStream) {
    // sonicSetChordPitch(mSonicStream, 1);
    sonicSetPitch(this->processorStream, (float)(100 + delta) / 100);
  }
  this->pitchDelta = delta;
#endif

  return this->pitchDelta;
}

int Audio::setVolume(int delta) {
  if (!hasProcessorInited) {
    cerr << "Audio processor has not initialized." << endl;
    return 0;
  }

  if (delta >= -100 && delta <= 100) {
    this->volumeDelta = delta;
    // @TODO: Using sonic's setVolume doesn't work. Don't know why...
    if (this->processorStream) {
      sonicSetVolume(this->processorStream, (float)(100 + delta) / 100);
    }
  }

  return this->volumeDelta;
}

int Audio::setRate(int delta) {
#ifdef ENABLE_SOUNDTOUCH
  if (delta >= -50 && delta <= 100) {
    this->rateDelta = delta;
  } else {
    this->rateDelta = 0;
  }
  this->pSoundtouch.setRateChange(this->rateDelta);
#else
  if (this->processorStream) {
    sonicSetRate(this->processorStream, (float)(100 + delta) / 100);
  }
  this->rateDelta = delta;
#endif

  return this->rateDelta;
}

int Audio::readShortFrames(short buffer[], int size) {
  if (!this->processorStream) {
    cerr << "processorStream not initialized" << endl;
    return 0;
  }
  // sonic会自动剪去一些空白的frame?
  return sonicReadShortFromStream(this->processorStream, buffer, size);
}

int Audio::writeShortFrames(short buffer[], int size) {
  if (!this->processorStream) {
    cerr << "processorStream not initialized" << endl;
    return 0;
  }
  return sonicWriteShortToStream(this->processorStream, buffer, size);
}

void Audio::flushFrames() {
  if (!this->processorStream) {
    cerr << "processorStream not initialized" << endl;
    return;
  }
  sonicFlushStream(this->processorStream);
}

void Audio::play(const string& path) {
  //cerr << "Audio::play:" << path << endl;

  SF_INFO sfinfo;
  SNDFILE *sndfile = sf_open(path.c_str(), SFM_READ, &sfinfo);
  if (!sndfile) {
    return;
  }

  if (sfinfo.frames <= 0) {
    sf_close(sndfile);
    return;
  }

  short *pcm = new short[sfinfo.frames];
  sf_readf_short(sndfile, pcm, sfinfo.frames);

  if (this->speechdSynthCallback) {
    // @TODO: read real bits
    speechdSynthCallback(pcm, sfinfo.frames, 16, sfinfo.channels, sfinfo.samplerate, 0);
#ifdef HAVE_PULSEAUDIO
  } else if (this->pulseAudio) {
    int error;
    //cerr << "pa_simple_write" << endl;
    int ret = pa_simple_write(this->pulseAudio, pcm, sfinfo.frames * 2, &error);
    if (ret < 0) {
      cerr << "pa_simple_write failed: " << pa_strerror(error) << endl;
    }
    ret = pa_simple_drain(this->pulseAudio, &error);
    if (ret < 0) {
      cerr << "pa_simple_drain failed: " << pa_strerror(error) << endl;
    }
  } else {
    cerr << "pulseAudio not inited: audio=%p, pulseAudio=%p" << this << this->pulseAudio << endl;    
#endif    
  }

  delete[] pcm;
  sf_close(sndfile);
}
