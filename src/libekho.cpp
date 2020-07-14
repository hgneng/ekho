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
#include "sonic.h"
#include "utf8.h"
#include "espeak-ng/speak_lib.h"

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
}

Ekho::Ekho() {
  this->m_pImpl = new EkhoImpl();
}

Ekho::Ekho(string voice) {
  this->m_pImpl = new EkhoImpl(voice);
}

Ekho::~Ekho(void) { delete this->m_pImpl; }

static bool gsIsFestivalInited = false;

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

// generate temp filename
// to be improve...
string Ekho::genTempFilename() { return this->m_pImpl->genTempFilename(); }

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

// @TODO: remove this deprecared method
/*
int Ekho::synth(string text, SynthCallback *callback, void *userdata) {
  return this->m_pImpl->synth(text, callback, userdata);
}*/

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

int Ekho::synth2(string text, SynthCallback *callback, void *userdata) {
  return this->m_pImpl->synth2(text, callback, userdata);
}

void Ekho::setPunctuationMode(EkhoPuncType mode) {
  this->m_pImpl->setPunctuationMode(mode);
}

void Ekho::sing(string filepath) { this->m_pImpl->sing(filepath); }

int Ekho::synth(const char *text, SpeechdSynthCallback *callback) {
  this->m_pImpl->setSpeechdSynthCallback(callback);
  this->m_pImpl->speak(text);
  return 0;
}

int Ekho::getSampleRate() {
  return this->m_pImpl->mDict.mSfinfo.samplerate;
}

void Ekho::setCapLetterRecognMode(EkhoCapLetterRecognType mode) {
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

  espeak_ERROR ret =
      espeak_SetParameter(espeakCAPITALS, espeak_cap_mode, 1);
}