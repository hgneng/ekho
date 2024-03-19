/***************************************************************************
 * Copyright (C) 2008-2024 by Cameron Wong                                 *
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
#include "config.h"
#include "ekho.h"
#include "ekho_dict.h"
#include "ekho_impl.h"
#include <math.h>

#ifdef ENABLE_ESPEAK
#include "espeak-ng/speak_lib.h"
#endif

#ifdef ENABLE_FLITE
#include "flite.h"
extern "C" cst_voice *register_cmu_us_kal16(const char *voxdir);
#endif

#ifdef DEBUG_ANDROID
#define LOG_TAG "Ekho Engine"
#include "Log.h"
#endif

#ifdef ENABLE_ESPEAK
static int espeakSynthCallback(short* wav, int numsamples,
                               espeak_EVENT* events) {
  return Ekho::synthCallback(wav, numsamples, EkhoImpl::gEkho, OVERLAP_NONE);
}
#endif

namespace ekho {

#ifdef ENABLE_FESTIVAL
static bool gsIsFestivalInited = false;
#endif

SynthCallback* Ekho::synthCallback = NULL;

void EkhoImpl::initEnglish(void) {
  gEkho = this;

#ifdef ENABLE_FESTIVAL
  if (!gsIsFestivalInited) {
    int heap_size = 2100000;  // scheme heap size
    int load_init_files = 0;  // don't load default festival init files
    festival_initialize(load_init_files, heap_size);

    // set libdir of festival
    string path(mDict.mDataPath);
    path += "/festival/lib";
    siod_set_lval("libdir", strintern(path.c_str()));

    path = mDict.mDataPath;
    path += "/festival/lib/init.scm";
    festival_load_file(path.c_str());
    // festival_eval_command("(Parameter.set 'Duration_Stretch 1.5)");

    gsIsFestivalInited = true;

    if (EkhoImpl::mDebug) {
      cerr << mDict.mDataPath << "/festival inited." << endl;
    }
  } else {
    festival_tidy_up();
  }
#endif

#ifdef ENABLE_ESPEAK
  // espeak
  int samplerate = espeak_Initialize(AUDIO_OUTPUT_SYNCHRONOUS, 0, NULL, 1);
  isEspeakInited = true;
  this->setEnglishSpeed(this->getEnglishSpeed());
  if (mDebug) {
    cerr << "espeak init samplerate: " << samplerate << endl;
  }
  espeak_SetSynthCallback(espeakSynthCallback);
  /* 女声好像并不好听，还是用原声吧
  if (!mIsMale) {
    espeak_SetVoiceByName("en+f4");
  } else {
    espeak_SetVoiceByName("en");
  }*/
#endif

#ifdef ENABLE_FLITE
  flite_init();
  mFliteVoice = register_cmu_us_kal16(NULL);
#ifdef DEBUG_ANDROID
  LOGD("EkhoImpl::initEnglish end");
#endif
#endif
}

void EkhoImpl::setEnglishSpeed(int delta) {
#ifdef ENABLE_ESPEAK
  if (mDict.mSfinfo.samplerate == 0) {
    cerr << "Voice sample rate not set. Maybe you have not setVoice first." << endl;
    return;
  }

  int baseDelta = 0;
  if (!Ekho::emotiVoiceEnabled) { // EmotiVoice是正常速度，不需要调整
    baseDelta = (int)round(mDict.mSfinfo.frames * 2 * 44100 * 100 / mDict.mSfinfo.samplerate / 20362) - 100;

    // Changing tempo will add noise, we'd better don't do it.
    if (baseDelta < 10 && baseDelta > -10) {
      baseDelta = 0;
    }
  }

  if (EkhoImpl::mDebug) {
    cerr << "setEnglishSpeed espeakRATE default: " << espeak_GetParameter(espeakRATE, 0) << endl;
    cerr << "setEnglishSpeed espeakRATE current: " << espeak_GetParameter(espeakRATE, 1) << endl;
    cerr << "setEnglishSpeed baseDelta: " << baseDelta << endl;
  }

  if (delta >= -50 && delta <= 150) {
    int ret = espeak_SetParameter(espeakRATE, 175 * (100 + delta - baseDelta) / 100, 0);
    this->englishSpeedDelta = delta;
    if (EkhoImpl::mDebug) {
      cerr << "english speed: " << 175 * (100 + delta) / 100
           << ", result=" << ret << endl;
    }
  }
#endif
}

const char* EkhoImpl::getEnglishPcm(string text, int &size) {
  if (Ekho::coquiEnabled) { 
    this->audio->setSampleRate(Ekho::COQUI_SAMPLE_RATE);
    return (const char*)this->getPcmFromServer(Ekho::COQUI_PORT, text, size, Ekho::COQUI_AMPLIFY_RATE);
  } else {
#ifdef ENABLE_FLITE
  return getPcmFromFlite(text, size);
#endif

#ifdef ENABLE_FESTIVAL
  return getPcmFromFestival(text, size);
#endif

#ifdef ENABLE_ESPEAK
  this->synthWithEspeak(text);
  return NULL;
#endif
  }
}

void EkhoImpl::synthWithEspeak(string text) {
#ifdef ENABLE_ESPEAK
  if (EkhoImpl::mDebug) {
    cerr << "EkhoImpl::synthWithEspeak: " << text << endl;
  }

  if (!isStopped) {
    Ekho::synthCallback(0, 0, gEkho, OVERLAP_NONE);  // flush pending pcm
    this->audio->setSampleRate(22050);
    espeak_Synth(text.c_str(), text.length() + 1, 0, POS_CHARACTER, 0,
                 espeakCHARS_UTF8, 0, 0);
    this->audio->setSampleRate(this->audio->sampleRate);
  }
#endif
}

// It's caller's responsibility to delete the returned pointer
const char* EkhoImpl::getPcmFromFlite(string text, int& size) {
#ifdef ENABLE_FLITE
  cerr << "EkhoImpl::getPcmFromFlite: " << text << endl;
#ifdef DEBUG_ANDROID
  LOGD("Ekho::getPcmFromFlite(%s, %d)", text.c_str(), size);
#endif

  if (!mFliteVoice) {
    initEnglish();
  }

  if (mFliteVoice) {
    //Ekho::synthCallback(0, 0, gEkho, OVERLAP_NONE);  // flush pending pcm
    //this->audio->setSampleRate(8000);
    cst_wave* flite_wave = flite_text_to_wave(text.c_str(), mFliteVoice);
    // 需要在callback里数据处理完后再把sampleRate设回来
    // this->audio->setSampleRate(this->audio->sampleRate);
    short* pcm = flite_wave->samples;
    size = flite_wave->num_samples * 2;
    // free(flite_wave); why free here?
#ifdef DEBUG_ANDROID
    LOGD("flite_text_to_wave got %d samples", size);
#endif
    return (const char*)pcm;
  } else {
#ifdef DEBUG_ANDROID
    LOGD("mFliteVoice not inited");
#endif
    return 0;
  }
#endif
  
  return NULL;
}

// It's caller's responsibility to delete the returned pointer
const char* EkhoImpl::getPcmFromFestival(string text, int& size) {
#ifdef ENABLE_FESTIVAL
  // set voice
  static const char* current_voice = "voice_kal_diphone";
  static int current_samplerate = 16000;
  if (strcmp(current_voice, mEnglishVoice) != 0) {
    current_voice = mEnglishVoice;
    char cmd[256];
    strcpy(cmd + 1, mEnglishVoice);
    cmd[0] = '(';
    cmd[strlen(mEnglishVoice) + 1] = ')';
    cmd[strlen(mEnglishVoice) + 2] = 0;
    festival_eval_command(cmd);
    // festival_eval_command("(voice_kal_diphone)");
    // festival_eval_command("(voice_cmu_us_slt_arctic_hts)");

    // make English slower (only work for voice_kal_diphone)
    // festival_eval_command("(set! hts_duration_stretch 0.4)");
    // festival_eval_command("(Parameter.set 'Duration_Stretch 5)");

    if (strcmp(current_voice, "voice_kal_diphone") == 0)
      current_samplerate = 16000;
    else if (strcmp(current_voice, "voice_cmu_us_slt_arctic_hts") == 0)
      current_samplerate = 32000;
  }

  if (EkhoImpl::mDebug) {
    cerr << "Festival speak: '" << text << "' in voice of " << mEnglishVoice
         << endl;
  }

  EST_Wave wave;
  festival_text_to_wave(text.c_str(), wave);

  if (mDict.mSfinfo.samplerate != current_samplerate) {
    wave.resample(this->mDict.mSfinfo.samplerate);
  }

  EST_TVector<short> tvector;
  wave.channel(tvector, 0);
  size = tvector.p_num_columns * 2;
  char* pPcm = new char[size];
  short* shortPcm = (short*)pPcm;
  tvector.get_values(shortPcm, 1, 0, tvector.p_num_columns);
  return pPcm;
#else
  size = 0;
  return 0;
#endif
}

} // end of namespace ekho