/***************************************************************************
 * Copyright (C) 2008-2023 by Cameron Wong                                 *
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
#include <dirent.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <fstream>
#include <iostream>
#include "config.h"
#include "ekho.h"
#include "ekho_dict.h"
#include "ekho_impl.h"
#include "ekho_typedef.h"
#include "utf8.h"

#ifdef ENABLE_ESPEAK
#include "espeak-ng/speak_lib.h"
#endif

#ifdef ENABLE_WIN32
#include <windows.h>
#include <winsock2.h>
/* We need the following two to set stdin/stdout to binary */
#include <fcntl.h>
#include <io.h>
#define sleep(seconds) Sleep((seconds)*1000)
#else
#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/wait.h>
#endif

using namespace ekho;
using namespace std;

#ifdef HAVE_MP3LAME
#include <lame/lame.h>
#endif

bool Ekho::mDebug = false;

void Ekho::debug(bool flag) {
  Ekho::mDebug = flag;
  EkhoImpl::debug(flag);
  PhoneticSymbol::debug = flag;
}

Ekho::Ekho() {
  this->m_pImpl = new EkhoImpl();
  Ekho::init();
}

Ekho::Ekho(string voice) {
  this->m_pImpl = new EkhoImpl(voice);
  Ekho::init();
}

void Ekho::init() {
  this->musicxmlMinuteRate = 58; // default is 120. 58 is for demo.xml
  this->sndFile = NULL;
}

Ekho::~Ekho(void) { delete this->m_pImpl; }

Dict& Ekho::getDict() {
  return this->m_pImpl->mDict;
}

int Ekho::saveWav(string text, string filename) {
  return this->m_pImpl->saveWav(text, filename);
}

int Ekho::saveOgg(string text, string filename) {
  return this->m_pImpl->saveOgg(text, filename);
}

void Ekho::setEnglishVoice(const char *voice) {
  this->m_pImpl->setEnglishVoice(voice);
}

void Ekho::setPcmCache(bool b) { this->m_pImpl->setPcmCache(b); }

bool Ekho::isSpeaking() { return this->m_pImpl->isSpeaking(); }

#ifdef HAVE_MP3LAME
int Ekho::saveMp3(string text, string filename) {
  return this->m_pImpl->saveMp3(text, filename);
}
#endif

int Ekho::speakPcm(short *pcm, int frames, void *arg, OverlapType type) {
  return EkhoImpl::speakPcm(pcm, frames, arg, type);
}

int Ekho::writeToSonicStream(short *pcm, int frames, OverlapType type) {
  return this->m_pImpl->writeToSonicStream(pcm, frames, type);
}

void Ekho::finishWritePcm() { this->m_pImpl->finishWritePcm(); }

int Ekho::writePcm(short *pcm, int frames, void *arg, OverlapType type) {
  return EkhoImpl::writePcm(pcm, frames, arg, type);
}

void *Ekho::speechDaemon(void *args) { return EkhoImpl::speechDaemon(args); }

int Ekho::play(string file) { return this->m_pImpl->play(file); }

int Ekho::setVoice(string voice) { return this->m_pImpl->setVoice(voice); }

string Ekho::getVoice() { return this->m_pImpl->getVoice(); }

int Ekho::speak(string text, void (*pCallback)(void *), void *pCallbackArgs) {
  return this->m_pImpl->speak(text, pCallback, pCallbackArgs);
}

int Ekho::stopAndSpeak(string text, void (*pCallback)(void *),
                       void *pCallbackArgs) {
  return this->m_pImpl->stopAndSpeak(text, pCallback, pCallbackArgs);
}

int Ekho::blockSpeak(string text) { return this->m_pImpl->blockSpeak(text); }

int Ekho::pause() { return this->m_pImpl->pause(); }

int Ekho::resume() { return this->m_pImpl->resume(); }

int Ekho::stop() { return this->m_pImpl->stop(); }

void Ekho::enableSsml() {
  this->m_pImpl->supportSsml = true;
}

void Ekho::disableSsml() {
  this->m_pImpl->supportSsml = false;
}

void Ekho::setSpeakIsolatedPunctuation(bool b) {
  this->m_pImpl->setSpeakIsolatedPunctuation(b);
}

bool Ekho::getSpeakIsolatedPunctuation() {
  return this->m_pImpl->getSpeakIsolatedPunctuation();
}

void Ekho::setSpeed(int tempo_delta) {
  return this->m_pImpl->setSpeed(tempo_delta);
}

int Ekho::getSpeed() { return this->m_pImpl->getSpeed(); }

void Ekho::setOverlap(int overlap) {
  this->m_pImpl->mOverlap = overlap;
}

void Ekho::setEnglishSpeed(int delta) {
  return this->m_pImpl->setEnglishSpeed(delta);
}

int Ekho::getEnglishSpeed() { return this->m_pImpl->getSpeed(); }

void Ekho::setPitch(int pitch_delta) { this->m_pImpl->setPitch(pitch_delta); }

int Ekho::getPitch() { return this->m_pImpl->getPitch(); }

void Ekho::setVolume(int volume_delta) {
  this->m_pImpl->setVolume(volume_delta);
}

int Ekho::getVolume() { return this->m_pImpl->getVolume(); }

void Ekho::setRate(int rate_delta) { this->m_pImpl->setRate(rate_delta); }

int Ekho::getRate() { return this->m_pImpl->getRate(); }

int Ekho::startServer(int port) { return this->m_pImpl->startServer(port); }

// the first byte is tempo(speed) delta
int Ekho::request(string ip, int port, Command cmd, string text,
                  string outfile) {
  return this->m_pImpl->request(ip, port, cmd, text, outfile);
}

short* Ekho::synth3(string text, int& samples) {
  string filepath = Audio::genTempFilename();
  this->m_pImpl->saveWav(text, filepath);
#ifdef DEBUG_ANDROID
  LOGD("saved to %s", filepath.c_str());
#endif

  FILE *f = fopen(filepath.c_str(), "rb+");
  short *pcm = NULL;
  if (f) {
    fseek(f, 0L, SEEK_END);
    long filesize = ftell(f); // get file size
    if (filesize > 44) {
      samples = (filesize - 44) / 2;
      fseek(f, 44L ,SEEK_SET); // skip 44 bytes header in WAV file
      pcm = new short[samples]; // allocate the read buf
      fread(pcm, 2, samples, f);
    }
    fclose(f);

    remove(filepath.c_str());
  }

  return pcm;
}

// for Android
SynthCallback* Ekho::synth4Callback = NULL;
EkhoImpl* Ekho::impl = NULL;
int Ekho::postProcess(short* pcm, int frames, void* arg, OverlapType type) {
  EkhoImpl* pEkho = Ekho::impl;

  if (pEkho->isStopped) {
    // 清空缓存
    pEkho->audio->flushFrames();
    short* buffer = new short[65536];

    do {
      frames = pEkho->audio->readShortFrames(buffer, 65536);
    } while (frames > 0);

    delete[] buffer;
    buffer = NULL;
  } else {
    int flush_frames = pEkho->writeToSonicStream(pcm, frames, type);

    if (flush_frames) {
      short* buffer = new short[65536];

      do {
        frames = pEkho->audio->readShortFrames(buffer, 65536);
        if (frames > 0) {
          Ekho::synth4Callback(buffer, frames, arg, type);
        }
      } while (frames > 0);

      delete[] buffer;
      buffer = NULL;
    }
  }

  return 0;
}

// 这里返回给callback的pcm经过speed和pitch的调整
int Ekho::synth4(string text, SynthCallback* callback, void* userdata) {
  Ekho::impl = this->m_pImpl;
  Ekho::synth4Callback = callback;
  this->m_pImpl->isStopped = false;
  return this->m_pImpl->synth2(text, Ekho::postProcess, userdata);
}

void Ekho::setPunctuationMode(EkhoPuncType mode) {
  this->m_pImpl->setPunctuationMode(mode);
}

void Ekho::setSampleRate(int sampleRate) {
  this->m_pImpl->audio->setOutputSampleRate(sampleRate);
}

int Ekho::getSampleRate(void) {
  return this->m_pImpl->audio->outputSampleRate;
}

void Ekho::setChannels(int channels) {
  this->m_pImpl->audio->setChannels(channels);
}

void Ekho::setCapLetterRecognMode(EkhoCapLetterRecognType mode) {
#ifdef ENABLE_ESPEAK
  int espeak_cap_mode = 0;
  switch (mode) {
  case EKHO_CAP_NONE:
    espeak_cap_mode = 800;
    break;
  case EKHO_CAP_SPELL:
    espeak_cap_mode = 2;
    break;
  case EKHO_CAP_ICON:
    espeak_cap_mode = 1;
    break;
  }

  espeak_SetParameter(espeakCAPITALS, espeak_cap_mode, 1);
#endif
}