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
#include <stdlib.h>
#include <sndfile.h>
#include <pthread.h>
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


void Audio::initProcessor() {
  if (this->outputSampleRate == 0) {
    cerr << "Audio::initProcessor: Fail! You should setSampleRate first." << endl;
    return;
  }

  this->initProcessor(this->outputSampleRate, 1);
}

void Audio::initProcessor(int samplerate, int channels) {
  if (Audio::debug) {
    cerr << "[" << pthread_self() << "] Audio::initProcessor: " << samplerate << endl;
  }

  this->outputSampleRate = samplerate;
  this->channels = channels;

#ifdef ENABLE_SOUNDTOUCH
  this->pSoundtouch.setSampleRate(mDict.mSfinfo.samplerate);
  this->pSoundtouch.setChannels(1);
  this->pSoundtouch.setSetting(SETTING_USE_QUICKSEEK, 1);
#else
  this->processorStream = sonicCreateStream(samplerate, 1);
  sonicSetQuality(this->processorStream, 1); // high quality but slower
#endif

  if (this->outputSampleRate != this->sampleRate) {
    this->setPitchFloat(1);
  }

  this->hasProcessorInited = true;
}

void Audio::destroyProcessor() {
  if (this->processorStream) {
    sonicDestroyStream(this->processorStream);
    this->processorStream = 0;
  }
}

#ifdef HAVE_PULSEAUDIO
void Audio::initPulseAudio() {
  if (Audio::debug) {
    cerr << "[" << pthread_self() << "] Audio::initPulseAudio" << endl;
  }

  pa_sample_spec ss;
  ss.channels = 1; // uint8_t seems cannot output to console directly
  ss.rate = this->sampleRate; // this->outputSampleRate;
  ss.format = PA_SAMPLE_S16LE;
  int error;

  if (Audio::debug) {
    cerr << "pa_sample_spec(format=" << ss.format << ",rate=" << ss.rate
         << ",channels=" << (int)ss.channels << ")" << endl;
  }

  this->pulseAudio = pa_simple_new(NULL, "Ekho", PA_STREAM_PLAYBACK, NULL,
                                 "playback", &ss, NULL, NULL, &error);

  if (!this->pulseAudio) {
    cerr << "pa_simple_new() failed: " << pa_strerror(error) << endl;
    cerr << "pa_sample_spec(format=" << ss.format << ",rate=" << ss.rate
         << ",channels=" << ss.channels << ")" << endl;
  }
}

void Audio::destroyPulseAudio() {
  if (this->pulseAudio) {
    // flush stream
    int error;
    if (pa_simple_drain(this->pulseAudio, &error) < 0) {
      cerr << "pa_simple_drain() failed: " << pa_strerror(error) << endl;
    }

    pa_simple_free(this->pulseAudio);
    this->pulseAudio = 0;
  }
}

void Audio::pulseAudioDrain() {
  int error;
  pa_simple_drain(this->pulseAudio, &error);
}

void Audio::pulseAudioFlush() {
  int error;
  pa_simple_flush(this->pulseAudio, &error);
}

int Audio::pulseAudioWrite(const void *buffer, size_t bytes) {
  int error;
  int ret = pa_simple_write(this->pulseAudio, buffer, bytes, &error);
  if (ret < 0) {
    cerr << "pa_simple_write failed: " << pa_strerror(error) << endl;
  }

  return ret;
}
#endif

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
  } else {
    cerr << "Audio::setTempo out of range: " << delta << endl;
  }
#endif

  return this->tempoDelta;
}

// 1 means no change. 2 means double speed
void Audio::setTempoFloat(float factor) {
  if (!this->processorStream) {
    cerr << "Audio::processorStream not init" << endl;
  }

  if (Audio::debug) {
    cerr << "Audio::setTempoFloat: " << factor << endl;
  }

  sonicSetSpeed(this->processorStream, factor);
}

// 1 means no change. 2 means double high pitch
void Audio::setPitchFloat(float factor) {
  if (!this->processorStream) {
    cerr << "Audio::processorStream not init" << endl;
    return;
  }

  float finalFactor = factor * this->sampleRate / this->outputSampleRate;

  if (Audio::debug) {
    cerr << "Audio::setPitchFloat: " << factor <<
        ", finalFactor=" << finalFactor << endl;
  }

  sonicSetPitch(this->processorStream, finalFactor);
}

// 设置（英语）输入源的sample rate，sonic需要调整当前PCM流的语速，
// 让后来不同的sample rate PCM流和之前的语速一致
int Audio::setSampleRate(int rate) {
  float r = (float)rate / this->outputSampleRate;
  if (Audio::debug) {
    cerr << "Audio::setSampleRate: " << rate <<
        ", outputSampleRate: " << this->outputSampleRate <<
        ", sampleRate: " << this->sampleRate <<
        ", target rate: " << r << endl;
  }

  flushFrames();
  //sonicSetRate(this->processorStream, r);
  sonicSetRate(this->processorStream, (float)rate / this->sampleRate);
  this->currentSampleRate = rate;
  return rate;
}

void Audio::setInputSampleRate(int rate) {
  this->sampleRate = rate;
  if (this->outputSampleRate == 0) {
    this->outputSampleRate = rate;
  }
}

void Audio::setOutputSampleRate(int rate) {
  if (Audio::debug) {
    cerr << "Audio::setOutputSampleRate: " << rate << endl;
  }

  if (rate > 0) {
    this->outputSampleRate = rate;
  }

  //this->setSampleRate(this->outputSampleRate);
}

int Audio::setPitch(int delta) {
  /*
  if (!hasProcessorInited) {
    cerr << "Audio processor has not initialized." << endl;
    return 0;
  }*/

  if (Audio::debug) {
    cerr << "Audio::setPitch: " << delta << endl;
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
  /*
  if (!hasProcessorInited) {
    cerr << "Audio processor has not initialized." << endl;
    return 0;
  }*/

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
  if (Audio::debug) {
    cerr << "Audio::setRate: " << delta << endl;
  }

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

// generate temp filename
// to be improve...
string Audio::genTempFilename() {
#ifdef ENABLE_WIN32
  string tmpFilePath("\\TEMP\\ekho");
#else
#ifdef ANDROID
  string tmpFilePath("/sdcard/ekho/tmp");
#else
  string tmpFilePath("/tmp/ekho");
#endif
#endif
  static int count = 0;
  count++;
  tmpFilePath.append(to_string(count));

  return tmpFilePath;
}