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
#include <dirent.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
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
#include "ssml.h"
#include "audio.h"
#include "utf8.h"

#ifdef ENABLE_ESPEAK
#include "espeak-ng/speak_lib.h"
#endif
#ifdef ENABLE_FESTIVAL
#include "festival/festival.h"
#endif

using namespace ekho;
using namespace std;

bool EkhoImpl::mDebug = false;
SpeechdSynthCallback* EkhoImpl::speechdSynthCallback = 0;
EkhoImpl* EkhoImpl::gEkho = NULL;

EkhoImpl::EkhoImpl() { this->init(); }

int EkhoImpl::init(void) {
  if (mDebug) {
    cerr << "EkhoImpl::init" << endl;
  }

  isEspeakInited = false;
  mIsMale = false;
  mPendingFrames = 0;
  supportSsml = true;
  mSpeakIsolatedPunctuation = true;
  mSpeechQueueMutex = PTHREAD_MUTEX_INITIALIZER;
  mSpeechQueueCond = PTHREAD_COND_INITIALIZER;
  mEnglishVoice = "voice_kal_diphone";
  mOverlap = 4096;

  this->audio = new Audio();

  this->mSndFile = 0;
  this->isRecording = false;
  this->tempoDelta = 0;
  this->pitchDelta = 0;
  this->rateDelta = 0;

  this->isStopped = false;
  this->isPaused = false;
  this->isEnded = false;
  this->isSoundInited = false;
  this->isSpeechThreadInited = false;

  mPcmCache = true;

  memset(mAlphabetPcmCache, 0, 26 * sizeof(const char*));
  memset(mAlphabetPcmSize, 0, 26 * sizeof(int));

  mPuncMode = EKHO_PUNC_SOME;

  return 0;
}

EkhoImpl::EkhoImpl(string voice) {
  this->init();
  this->setVoice(voice);
}

EkhoImpl::~EkhoImpl(void) {
  //pthread_mutex_lock(&mSpeechQueueMutex);
  this->isEnded = true;
  //pthread_mutex_unlock(&mSpeechQueueMutex);
  pthread_cond_signal(&mSpeechQueueCond);
  pthread_cond_destroy(&mSpeechQueueCond);

  if (this->isSpeechThreadInited) {
    void *ret;
    pthread_attr_destroy(&this->speechThreadAttr);
    pthread_join(this->speechThread, &ret);
  }
  closeStream();

  delete this->audio;

#ifdef ENABLE_FESTIVAL
  festival_eval_command("(audio_mode 'close)");
#endif

#ifdef ENABLE_ESPEAK
  if (isEspeakInited) {
    espeak_Terminate();
    isEspeakInited = false;
  }
#endif

  // TODO: free mAlphabetPcmCache
}

int EkhoImpl::initSound(void) {
  if (!this->isSoundInited) {
    // launch speechDaemon
    pthread_attr_init(&this->speechThreadAttr);
    pthread_attr_setdetachstate(&this->speechThreadAttr, PTHREAD_CREATE_JOINABLE);
    pthread_create(&this->speechThread, NULL, speechDaemon, (void *)this);
    this->isSpeechThreadInited = true;
    this->isSoundInited = true;

    // not output sound directly, only return pcm data if mSynthCallback is set.
    if (speechdSynthCallback) {
      return 0;
    }

#ifdef HAVE_PULSEAUDIO
    this->audio->initPulseAudio();
#endif
  }

  return 0;
}

int EkhoImpl::initStream(void) {
  closeStream();
  if (mDict.getLanguage() == ENGLISH) {
    mDict.mSfinfo.samplerate = 16000;
    mDict.mSfinfo.channels = 1;
  } else if (mDict.mSfinfo.samplerate == 0) {
    cerr << "Sample rate not detected: " << mDict.getLanguage() << endl;
    return -1;
  }

  this->audio->setInputSampleRate(mDict.mSfinfo.samplerate);
  this->audio->setChannels(mDict.mSfinfo.channels);
  this->audio->initProcessor();

#ifdef HAVE_PULSEAUDIO
  /* create stream */
  if (this->isSoundInited) {
    this->audio->initPulseAudio();
  }
#endif  // end of HAVE_PULSEAUDIO

  return 0;
}  // end of initStream

void EkhoImpl::closeStream(void) {
  this->audio->destroyProcessor();
#ifdef HAVE_PULSEAUDIO
  this->audio->destroyPulseAudio();
#endif
}

int EkhoImpl::writePcm(short *pcm, int frames, void *arg, OverlapType type,
                       bool tofile) {
  short *buffer = new short[BUFFER_SIZE];

  EkhoImpl *pEkho = (EkhoImpl *)arg;

  pthread_mutex_lock(&(pEkho->mSpeechQueueMutex));
  if (!pEkho->isStopped) {        
    int flush_frames = pEkho->writeToSonicStream(pcm, frames, type);

    if (flush_frames) {
      do {
        if (pEkho->isStopped) {
          break;
        }

        while (pEkho->isPaused) {
          sleep(1);
        }

        frames = pEkho->audio->readShortFrames(buffer, BUFFER_SIZE);

        if (frames > 0) {
          if (tofile) {
            /*int writtenFrames = */sf_writef_short(pEkho->mSndFile, buffer, frames / pEkho->audio->channels);
            /*
            if (frames / pEkho->audio->channels != writtenFrames) {
              cerr << "Fail to write WAV file " << writtenFrames << " out of "
                   << frames << " written" << endl;
              return -1;
            }*/
          } else {
            if (EkhoImpl::speechdSynthCallback) {
              if (EkhoImpl::mDebug) {
                cerr << "EkhoImpl::speechdSynthCallback: " << frames << endl;
              }
              EkhoImpl::speechdSynthCallback(buffer, frames, 16,
                  pEkho->audio->channels, pEkho->audio->sampleRate, 0);
            } else {
  #ifdef HAVE_PULSEAUDIO
              pEkho->audio->pulseAudioWrite((const void*)buffer, frames * 2);
  #endif
            }
          }
        }
      } while (frames > 0);
    }
  }
  pthread_mutex_unlock(&(pEkho->mSpeechQueueMutex));

  delete[] buffer;
  return 0;
}

int EkhoImpl::writeToSonicStream(short* pcm, int frames, OverlapType type) {
  // 把前后音量为0的部分去掉
  int i = 0;
  int minLevel = 512;
  while (i < frames && abs(*pcm) < minLevel) {
    i++;
    pcm++;
  }
  if (i > 0) {
    // cerr << "trim left: " << i << endl;
    frames -= i;
  }

  i = 0;
  while (frames > 0 && abs(*(pcm + frames - 1)) < minLevel) {
    i++;
    frames--;
  }
  if (i > 0) {
    // cerr << "trim right: " << i << endl;
  }

  // 如果未播放的帧太多了，先播放掉（为什么是全部播放掉而不是一部分？？）
  while (frames > PENDING_PCM_FRAMES - mPendingFrames) {
    // cerr << "flush frames: " << PENDING_PCM_FRAMES - mPendingFrames << endl;
    memcpy(mPendingPcm + mPendingFrames, pcm, (PENDING_PCM_FRAMES - mPendingFrames) * 2);
    this->audio->writeShortFrames(mPendingPcm, PENDING_PCM_FRAMES);
    pcm += PENDING_PCM_FRAMES - mPendingFrames;
    frames -= PENDING_PCM_FRAMES - mPendingFrames;
    mPendingFrames = 0;
  }

  const int quiet_level = mOverlap; // 音量低于(quiet_level / 65536)的部分重叠

  int flushframes = 0;  // mPendingFrames里应该输出的frames
  int cpframe = 0;  // 下一段音频里，0到cpframe - 1是已经被合并到mPendingFrames里的，
          // 剩下的直接复制到mPendingFrames尾部
  int startframe = 0; // 下一段音频的开始重叠位置
  int endframe = mPendingFrames - 1; // 上一段音频的结束重叠位置
  int q_level = 0;
  // promise length not less than de5 * 0.8.
  int minFrames = mDict.mSfinfo.frames * 0.8;
  int maxLeftFrames; // mPendingFrames - endframe - 1 + startframe;
  // cerr << "frames:" << frames << ",minFrames:" << minFrames << ",type:" << type << endl;

  switch (type) {
    case OVERLAP_NONE:
      memcpy(mPendingPcm + mPendingFrames, pcm, frames * 2);
      mPendingFrames += frames;
      flushframes = mPendingFrames;
      cpframe = frames;
      break;

    case OVERLAP_QUIET_PART:
      // don't overlap more than 0.3(endframe)+0.3(startframe) of the syllable frames

      // 找出上一段音频尾部音量小的帧
      while (endframe > 0 &&
        mPendingFrames - endframe < frames * 0.3 &&
        mPendingFrames - endframe < (frames - minFrames) * 0.5 &&
        abs(mPendingPcm[endframe]) < quiet_level) {
        endframe--;
      }

      // 找出下一段音频头部音量小的帧
      while (startframe < frames * 0.3 &&
          startframe < (frames - minFrames) * 0.5 &&
          abs(pcm[startframe]) < quiet_level) {
        startframe++;
      }

      // 在上一段音频里再往前找一些帧，
      // 确保下一段音频头重叠起来不能超过最大音量
      i = endframe;
      q_level = 32767 - quiet_level;
      while (i > 0 && endframe - i < startframe &&
          abs(mPendingPcm[i]) < q_level) {
        i--;
      }

      // 下一段音频的帧并非都达到32767 - quiet_level水平，可以再尽可能多重叠一些
      // remember a large frame and search back for small quite frame to overlarp
      q_level = 32767 - abs(mPendingPcm[i]);
      while (i > 0 && endframe - i < startframe &&
          abs(pcm[endframe - i]) < q_level) {
        i--;
      }

      // 有超过最大音量的部分，减少重叠的帧数
      if (startframe > endframe - i) {
        startframe = endframe - i;
      }

      // 上一段（左边）音频剩下的帧数
      maxLeftFrames = mPendingFrames - endframe - 1 + startframe;

      // 下一段音频的开始位置再往后找一些帧，
      // 确保不能和上一段音频重叠起来不能超过最大音量
      i = startframe;
      q_level = 32767 - quiet_level;
      while (i < frames && i < maxLeftFrames &&
          abs(pcm[i]) < q_level) {
        i++;
      }

      // 上一段音频的帧并非都达到32767 - quiet_level水平，可以再尽可能多重叠一些
      if (i < frames) {
        q_level = 32767 - abs(pcm[i]);
        while (i < frames && i < maxLeftFrames &&
            abs(mPendingPcm[mPendingFrames + startframe - i - 1]) < q_level) {
          i++;
        }
      }

      // 有超过最大音量的部分，减少重叠的帧数
      if (i < maxLeftFrames && i < frames) {
        //cerr << "endframe: " << endframe << " to " << mPendingFrames + startframe - i - 1<< endl;
        endframe = mPendingFrames + startframe - i - 1;
      }

      // 拼接上下两段音频里音量小的帧
      for (i = max(0, endframe - startframe);
          i < mPendingFrames && cpframe < frames; i++) {
        mPendingPcm[i] += pcm[cpframe];
        /*
        if (mPendingPcm[i] > 32767) {
          cerr << "overflow: " << mPendingPcm[i] << endl;
        }*/
        cpframe++;
      }

      //cerr << "frames:" << frames << ", startframe: " << startframe <<
      //  ", endframe:" << endframe << ", overlap: " << i - max(0, endframe - startframe) - 1 << endl;

      if (frames == 0) {
        // frames=0 means flush all pending frames
        flushframes = i;
      } else if (mPendingFrames + mPendingFrames > frames) {
        // guaranteer pending frames no more than haft frames
        flushframes = mPendingFrames - frames * 0.5;
      }
/*
      if (endframe < mPendingFrames - 1) {
        cerr << "clip endframe: " << mPendingFrames - endframe + 1 << endl;
      }

      if (startframe > 0) {
        cerr << "clip startframe: " << startframe << endl;
      }

      cerr << "cpframes: " << cpframe << ", flushframes: " << flushframes 
        << ", mPendingFrames: " << mPendingFrames << ", frames: " << frames << endl;
        */
      break;

    case OVERLAP_HALF_PART:
      // find quiet frames of first char
      while (endframe > 0 && abs(mPendingPcm[endframe]) < 32767) {
        endframe--;
      }

      // find half but not too lound part of second char
      while (startframe < frames * 0.5 && abs(pcm[startframe]) < 32767) {
        startframe++;
      }

      for (i = max(endframe, mPendingFrames - startframe);
           i < mPendingFrames && cpframe < frames; i++) {
        mPendingPcm[i] += pcm[cpframe];
        cpframe++;
      }
      flushframes = i;

      // make a liner joining. fade out + fade in
      // Reference: splice.c of sox
      /*
      for (int i = 1; i < mPendingFrames && i <= frames; i++) {
        double p = i / mPendingFrames;
        mPendingPcm[i] = mPendingPcm[i] * (1 - p) + pcm[i] * p;
      }*/
      break;
  }

  this->audio->writeShortFrames(mPendingPcm, flushframes);
  mPendingFrames -= flushframes;
  if (mPendingFrames > 0) {
    memcpy(mPendingPcm, mPendingPcm + flushframes, mPendingFrames * 2);
  }
  memcpy(mPendingPcm + mPendingFrames, pcm + cpframe, (frames - cpframe) * 2);
  mPendingFrames += frames - cpframe;


  return flushframes;
}

void EkhoImpl::finishWritePcm(void) {
  writePcm(0, 0, this, OVERLAP_QUIET_PART);
}

void *EkhoImpl::speechDaemon(void *args) {
  if (EkhoImpl::mDebug) {
    cerr << "EkhoImpl::speechDaemon begin" << endl;
  }
  EkhoImpl *pEkho = (EkhoImpl *)args;

  pEkho->initEnglish();

  while (1) {
    pthread_mutex_lock(&pEkho->mSpeechQueueMutex);
    if (pEkho->mSpeechQueue.empty() && !pEkho->isEnded) {
      if (EkhoImpl::mDebug) {
        cerr << "EkhoImpl::speechDaemon waiting speech queue" << endl;
      }
      pthread_cond_wait(&pEkho->mSpeechQueueCond, &pEkho->mSpeechQueueMutex);
    }

    if (pEkho->isEnded) {
      pthread_mutex_unlock(&pEkho->mSpeechQueueMutex);
      break;
    }

    if (pEkho->mSpeechQueue.empty()) {
      pthread_mutex_unlock(&pEkho->mSpeechQueueMutex);
      continue;
    }

    SpeechOrder order = pEkho->mSpeechQueue.front();
    pEkho->isStopped = false;
    pthread_mutex_unlock(&pEkho->mSpeechQueueMutex);

    // It seems that Sonic doesn't work on threads. Set arguments again. It's
    // fixed before. But it doesn't work again...
    /* @fixme: reset tempoDelta will cause double speed from 139 to 278
    // too quick for pinyin-huang will break with 天才是怎样练成的 赣F54562
    pEkho->setSpeed(pEkho->tempoDelta);
    pEkho->setPitch(pEkho->pitchDelta);
    pEkho->setVolume(pEkho->volumeDelta);
    pEkho->setRate(pEkho->rateDelta);
    */

    if (EkhoImpl::mDebug) {
      cerr << "EkhoImpl::speechDaemon synth2 " << order.text << endl;
    }
    pEkho->synth2(order.text, speakPcm);

    if (!pEkho->isStopped) {
      // FIXME: following statement seems not flush rest PCM
      pEkho->speakPcm(0, 0, pEkho, OVERLAP_QUIET_PART);
      if (EkhoImpl::speechdSynthCallback) {
        EkhoImpl::speechdSynthCallback(0, 0, 0, 0, 0, 1);
      }

      if (EkhoImpl::mDebug) {
        cerr << "EkhoImpl::speechDaemon synth2 end" << endl;
      }

  #ifdef HAVE_PULSEAUDIO
      if (!EkhoImpl::speechdSynthCallback) {
        if (pEkho->isStopped) {
          pEkho->audio->pulseAudioFlush();
        } else {
          pEkho->audio->pulseAudioDrain();
        }
      }
  #endif
    }

    if (!pEkho->isStopped) {
      pthread_mutex_lock(&pEkho->mSpeechQueueMutex);
      if (order.pCallback) {
        order.pCallback(order.pCallbackArgs);
      }

      if (!pEkho->mSpeechQueue.empty()) {
        pEkho->mSpeechQueue.pop();
      }
      pthread_mutex_unlock(&pEkho->mSpeechQueueMutex);
    }

    if (pEkho->isStopped) {
      while (not pEkho->mSpeechQueue.empty()) {
        pEkho->mSpeechQueue.pop();
      }

      #ifdef ENABLE_FESTIVAL
        festival_eval_command("(audio_mode 'shutup)");
      #endif

      #ifdef ENABLE_ESPEAK
        espeak_Cancel();
      #endif
    }
  }

  if (EkhoImpl::mDebug) {
    cerr << "EkhoImpl::speechDaemon end" << endl;
  }

  return 0;
}  // end of speechDaemon

int EkhoImpl::play(string file) {
  system((this->player + " " + file + " 2>/dev/null").c_str());

  return 0;
}

int EkhoImpl::setVoice(string voice) {
  if (mDebug) {
    cerr << "[" << pthread_self() << "] EkhoImpl::setVoice" << endl;
  }

  if (voice.compare(mDict.getVoice()) == 0) return 0;

  if (voice.compare("Cantonese") == 0 || voice.compare("yue") == 0) {
    voice = "jyutping";
    mIsMale = true;
  } else if (voice.compare("Mandarin") == 0 || voice.compare("zh") == 0 ||
             voice.compare("cmn") == 0) {
    voice = "pinyin";
    mIsMale = false;
    setSpeed(0);
    // setEnglishVoice("voice_cmu_us_slt_arctic_hts");
  } else if (voice.compare("Korean") == 0 || voice.compare("ko") == 0) {
    voice = "hangul";
    mIsMale = false;
    // setEnglishVoice("voice_cmu_us_slt_arctic_hts");
  } else if (voice.compare("Toisanese") == 0) {
    voice = "toisanese";
    mIsMale = true;
  } else if (voice.compare("Hakka") == 0) {
    voice = "hakka";
    mIsMale = true;
  } else if (voice.compare("Tibetan") == 0 || voice.compare("bo") == 0) {
    voice = "tibetan";
    mIsMale = false;
    // setEnglishVoice("voice_cmu_us_slt_arctic_hts");
  } else if (voice.compare("Ngangien") == 0) {
    voice = "ngangien";
    mIsMale = true;
  } else if (voice.compare("en") == 0) {
    voice = "English";
    mIsMale = true;
  }

  if (voice.find("jyutping") == 0) {
    mDict.setLanguage(CANTONESE);
  } else if (voice.find("pinyin") == 0) {
    mDict.setLanguage(MANDARIN);
  } else if (voice.find("hangul") == 0) {
    mDict.setLanguage(KOREAN);
  } else if (voice.find("hakka") == 0) {
    mDict.setLanguage(HAKKA);
  } else if (voice.find("toisanese") == 0) {
    mDict.setLanguage(TOISANESE);
  } else if (voice.find("tibetan") == 0) {
    mDict.setLanguage(TIBETAN);
  } else if (voice.find("ngangien") == 0) {
    mDict.setLanguage(NGANGIEN);
  } else if (voice.find("English") == 0) {
    mDict.setLanguage(ENGLISH);
  } else {
    cerr << "Invalid voice: " << voice << ". Fallback to Mandarin." << endl;
    mDict.setLanguage(MANDARIN);
    voice = "pinyin";
  }

  if (mDict.setVoice(voice.c_str()) != 0) {
    cerr << "Fail to setVoice of dictionary" << endl;
    return -2;
  }

  this->initStream();

  return 0;
}

string EkhoImpl::getVoice(void) { return this->mDict.getVoice(); }

int EkhoImpl::speak(string text, void (*pCallback)(void *),
                    void *pCallbackArgs) {
  if (EkhoImpl::mDebug) {
    cerr << "EkhoImpl::speak(" << text << ") begin" << endl;
  }

  this->initSound();
  this->isPaused = false;
  // this->isStopped = false;
  SpeechOrder order;
  order.text = text;
  order.pCallback = pCallback;
  order.pCallbackArgs = pCallbackArgs;
  pthread_mutex_lock(&mSpeechQueueMutex);
  mSpeechQueue.push(order);
  pthread_mutex_unlock(&mSpeechQueueMutex);
  pthread_cond_signal(&mSpeechQueueCond);

  if (EkhoImpl::mDebug) {
    cerr << "EkhoImpl::speak end" << endl;
  }

  return 0;
}

int EkhoImpl::stopAndSpeak(string text, void (*pCallback)(void *),
                           void *pCallbackArgs) {
  this->stop();
  this->speak(text, pCallback, pCallbackArgs);
  return 0;
}

int EkhoImpl::blockSpeak(string text) {
#ifdef HAVE_PULSEAUDIO
  if (initSound() < 0) {
    cerr << "Fail to init sound." << endl;
    return -1;
  }
  
  pthread_mutex_lock(&mSpeechQueueMutex);
  this->isPaused = false;
  this->isStopped = false;
  SpeechOrder order;
  order.text = text;
  order.pCallback = NULL;
  order.pCallbackArgs = NULL;
  mSpeechQueue.push(order);
  pthread_mutex_unlock(&mSpeechQueueMutex);
  pthread_cond_signal(&mSpeechQueueCond);

  while (mSpeechQueue.size() > 0) {
    sleep(1);
  }

  pthread_mutex_lock(&mSpeechQueueMutex);
  this->isStopped = true;
  pthread_mutex_unlock(&mSpeechQueueMutex);
  pthread_cond_signal(&mSpeechQueueCond);

  this->audio->pulseAudioDrain();
#endif
  return 0;
}

int EkhoImpl::pause(void) {
  if (EkhoImpl::mDebug) {
    cerr << "EkhoImpl::pause" << endl;
  }

  if (!this->isPaused) {
    this->isPaused = true;
    return 0;
  } else {
    return 1;
  }
}

int EkhoImpl::resume(void) {
  if (EkhoImpl::mDebug) {
    cerr << "EkhoImpl::resume" << endl;
  }

  if (this->isPaused) {
    this->isPaused = false;
    return 0;
  } else {
    return 1;
  }
}

int EkhoImpl::stop(void) {
  if (EkhoImpl::mDebug) {
    cerr << "EkhoImpl::stop" << endl;
  }
  this->isPaused = false;
  this->isStopped = true;

/*
  pthread_mutex_lock(&mSpeechQueueMutex);
  while (not mSpeechQueue.empty()) {
    mSpeechQueue.pop();
  }

  this->mPendingFrames = 0;
#ifdef ENABLE_FESTIVAL
  festival_eval_command("(audio_mode 'shutup)");
#endif

#ifdef ENABLE_ESPEAK
  espeak_Cancel();
#endif

  pthread_mutex_unlock(&mSpeechQueueMutex);
*/

  return 0;
}

void EkhoImpl::setSpeed(int tempo_delta) {
  // nomralize voice's tempo
  int baseDelta = 0;
  if (mDict.getLanguage() == MANDARIN && mDict.mSfinfo.frames > 0) {
    baseDelta = (int)round(mDict.mSfinfo.frames * 2 * 44100 * 100 / mDict.mSfinfo.samplerate / 20362) - 100;
    if (EkhoImpl::mDebug) {
      cerr << "mDict.mSfinfo.frames=" << mDict.mSfinfo.frames << ", samplerate=" << mDict.mSfinfo.samplerate << endl;
    }

    // Changing tempo will add noise, we'd better don't do it.
    if (baseDelta < 10 && baseDelta > -10) {
      baseDelta = 0;
    }
  }

  if (baseDelta + tempo_delta != 0 || tempo_delta != this->tempoDelta) {
    if (this->mDebug) {
      cerr << "baseDelta=" << baseDelta << ", tempo delta: " << baseDelta + tempo_delta << endl;
    }

    this->tempoDelta = this->audio->setTempo(baseDelta + tempo_delta);
  }
}

void EkhoImpl::translatePunctuations(string &text, EkhoPuncType mode) {
  bool changed = false;

  string text2;
  bool in_chinese_context = true;

  int c;
  string::iterator it = text.begin();
  string::iterator it2 = text.begin();
  string::iterator end = text.end();

  while (it != end) {
    it2 = it;
#ifdef DISABLE_EXCEPTIONS
    c = utf8::next(it, end);
#else
    try {
      c = utf8::next(it, end);
    } catch (utf8::not_enough_room &) {
      text = text2;
      return;
    } catch (utf8::invalid_utf8 &) {
      cerr << "translatePunctuations: Invalid UTF8 encoding" << endl;
      text = text2;
      return;
    }
#endif

    if (in_chinese_context && mDict.isPunctuationChar(c, mode)) {
      text2 += mDict.getPunctuationName(c);
      changed = true;
    } else {
      while (it2 != it) {
        text2.push_back(*it2);
        it2++;
      }
      in_chinese_context = (c > 128 || (c >= '0' && c <= '9'));
    }

    while (it2 != it) it2++;
  }

  if (changed) {
    text = text2;
  }
}

int EkhoImpl::synth2(string text, SynthCallback *callback, void *userdata) {
  Ekho::synthCallback = callback;
#ifdef DEBUG_ANDROID
  LOGD("Ekho::synth2(%s, %p, %p) voiceFileType=%s lang=%d", text.c_str(),
       callback, userdata, mDict.mVoiceFileType, mDict.getLanguage());
#endif
  if (EkhoImpl::mDebug) {
    cerr << "speaking lang(" << mDict.getLanguage() << "): '" << text << "'" << endl;
  }

  if (!userdata) {
    userdata = this;
  }

  //this->isStopped = false;
  this->isPaused = false;
  float pause = 0;
  int size = 0;
  const char *pPcm = 0;

  if (mDict.getLanguage() == ENGLISH) {
#ifdef ENABLE_ENGLISH
    if (EkhoImpl::mDebug) {
      cerr << "speaking '" << text << "' with Festival" << endl;
    }
    pPcm = this->getEnglishPcm(text, size);
    // output pcm data
    if (pPcm && size > 0) {
      callback((short *)pPcm, size / 2, userdata, OVERLAP_NONE);
      this->audio->setSampleRate(this->audio->sampleRate);
      if (pPcm) {
        delete[] pPcm;
        pPcm = NULL;
      }
    }
#endif
    return 0;
  }

  // check whehter voice file available
  if (!mDict.mVoiceFileType && mDict.getLanguage() != ENGLISH) {
    cerr << "Voice file not found." << endl;
    return -1;
  }

  // process SSML
  if (mDebug) {
    cerr << "supportSsml:" << this->supportSsml << endl;
  }

  if (this->supportSsml) {
    if (Ssml::isAudio(text)) {
      if (mDebug) {
        cerr << "isAudio, play" << endl;
      }
      this->audio->play(Ssml::getAudioPath(text));
      return 0;
    }
    text = Ssml::stripTags(text);
  }

  // check punctuation
  if (mSpeakIsolatedPunctuation && text.length() <= 3) {
    const char *c = text.c_str();
    int code = utf8::next(c, c + text.length());
    if (!*c && mDict.isPunctuationChar(code, EKHO_PUNC_ALL))
      text = mDict.getPunctuationName(code);
  }

  // translate punctuations
  translatePunctuations(text, mPuncMode);

  // filter spaces
  Ssml::filterSpaces(text);
  if (EkhoImpl::mDebug) {
    cerr << "filterSpaces: " << text << endl;
  }

#ifdef DEBUG_ANDROID
  LOGD("Ekho::synth2 filtered text=%s", text.c_str());
#endif

  list<Word> wordlist = Word::split(text);
  list<PhoneticSymbol *>::iterator phon_symbol;
  for (list<Word>::iterator word = wordlist.begin(); word != wordlist.end();
       word++) {
    if (this->isStopped) {
      break;
    }

    if (EkhoImpl::mDebug) {
      cerr << "word(" << word->type << "): " << word->text << endl;
    }

    switch (word->type) {
      case FULL_PAUSE:
        pause += 1;
        break;
      case HALF_PAUSE:
        pause += 0.5;
        break;
      case QUATER_PAUSE:
        pause += 0.25;
        break;
      case PHONETIC:
        phon_symbol = word->symbols.begin();
        pPcm = (*phon_symbol)->getPcm(mDict.mVoiceFile, size);
        if (pPcm && size > 0)
          callback((short *)pPcm, size / 2, userdata, OVERLAP_QUIET_PART);
        break;

      case ENGLISH_TEXT:
        if (pause > 0) {
          word--;  // turn back pointer
          if (pause > 1)
            pPcm = this->mDict.getFullPause()->getPcm(size);
          else if (pause >= 0.5)
            pPcm = this->mDict.getHalfPause()->getPcm(size);
          else
            pPcm = this->mDict.getQuaterPause()->getPcm(size);
          pause = 0;
          callback((short *)pPcm, size / 2, userdata, OVERLAP_NONE);
        } else {
          char c;
          if ((word->text.length() == 1) &&
              (c = tolower(word->text.c_str()[0])) && c >= 'a' && c <= 'z') {
      		  /*
                  if (!mAlphabetPcmCache[c - 'a'])
                    mAlphabetPcmCache[c - 'a'] =
                        getEnglishPcm(word->text, mAlphabetPcmSize[c - 'a']);

                  pPcm = mAlphabetPcmCache[c - 'a'];
                  size = mAlphabetPcmSize[c - 'a'];
      	    */

            // use pinyin-huang alphabet
      	    phon_symbol = word->symbols.begin();
      	    pPcm = (*phon_symbol)->getPcm(mDict.mVoiceFile, size);
            callback((short *)pPcm, size / 2, userdata, OVERLAP_NONE);
          } else {
            pPcm = this->getEnglishPcm(word->text, size);
            if (pPcm && size > 0) {
              callback((short *)pPcm, size / 2, userdata, OVERLAP_NONE);
              if (pPcm) {
                delete[] pPcm;
                this->audio->setSampleRate(this->audio->sampleRate);
              }
            }
            pPcm = 0;
          }
        }
        break;

      case NON_ENGLISH:
        if (pause > 0) {
          word--;  // turn back pointer
          if (pause >= 1)
            pPcm = this->mDict.getFullPause()->getPcm(size);
          else if (pause >= 0.5)
            pPcm = this->mDict.getHalfPause()->getPcm(size);
          else
            pPcm = this->mDict.getQuaterPause()->getPcm(size);
          pause = 0;
          callback((short *)pPcm, size / 2, userdata, OVERLAP_NONE);
        } else {
          if (word->bytes) {
            PhoneticSymbol word_symbol("", word->offset, word->bytes);
            pPcm = word_symbol.getPcm(mDict.mVoiceFile, size);
            callback((short *)pPcm, size / 2, userdata, OVERLAP_QUIET_PART);
          } else {
            // speak the word one by one
            list<OverlapType>::iterator type = word->overlapTypes.begin();
            for (list<PhoneticSymbol *>::iterator symbol =
                     word->symbols.begin();
                 symbol != word->symbols.end(); symbol++) {
#ifdef DEBUG_ANDROID
              LOGD("Ekho::synth2 speak %s", (*symbol)->symbol);
#endif
              Language lang = mDict.getLanguage();
              if (lang == MANDARIN || lang == CANTONESE) {
                pPcm = (*symbol)->getPcm(mDict.mVoiceFile, size);
                if (pPcm && size > 0) {
                  //cerr << (*symbol)->symbol << ": " << size << endl;
                  callback((short *)pPcm, size / 2, userdata, *type);
                }
              } else {
                string path = mDict.mDataPath + "/" + mDict.getVoice();
                pPcm =
                    (*symbol)->getPcm(path.c_str(), mDict.mVoiceFileType, size);
                if (pPcm && size > 0)
                  callback((short *)pPcm, size / 2, userdata, *type);
              }

              // speak Mandarin for Chinese
              if (!pPcm && lang == TIBETAN) {
                string path = mDict.mDataPath + "/pinyin";
                pPcm =
                    (*symbol)->getPcm(path.c_str(), mDict.mVoiceFileType, size);
                if (pPcm && size > 0)
                  callback((short *)pPcm, size / 2, userdata, *type);
              }

              if (!mPcmCache) (*symbol)->setPcm(0, 0);

              type++;
            }
          }
        }
        break;

      case RECORDING:
        short* pcm = this->audio->readPcmFromAudioFile(mDict.mDataPath + "/" +
           mDict.getVoice() + "/" + word->text, size);
        if (pcm) {
          callback(pcm, size, userdata, OVERLAP_QUIET_PART);
          free(pcm);
          pcm = NULL;
        }
        break;
    }
  }  // end of for

  // send a signal to abort for Android
  if (userdata) {
    callback(0, 0, userdata, OVERLAP_NONE);
  } else {
    callback(0, 0, this, OVERLAP_NONE);
  }

  return 0;
}

void EkhoImpl::setSpeechdSynthCallback(SpeechdSynthCallback *callback) {
  EkhoImpl::speechdSynthCallback = callback;
  this->audio->speechdSynthCallback = callback;
}
