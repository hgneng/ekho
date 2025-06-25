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

// 这里返回给callback的pcm未经speed和pitch的调整
int EkhoImpl::synth2(string text, SynthCallback* callback, void* userdata) {
  if (text.empty()) {
    return 0;
  }

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
  const char* pPcm = NULL;
  short* shortPcm = NULL;

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
    if (text.empty()) {
      return 0;
    }
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
            finishWritePcm(); // flush Chinese pcm in case of different sample rate
            pPcm = this->getEnglishPcm(word->text, size);
            if (pPcm && size > 0) {
              callback((short *)pPcm, size / 2, userdata, OVERLAP_NONE);
              delete[] pPcm;
              pPcm = NULL;
              this->audio->setSampleRate(this->audio->sampleRate);
            }
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
        shortPcm = this->audio->readPcmFromAudioFile(mDict.mDataPath + "/" +
           mDict.getVoice() + "/" + word->text, size);
        if (shortPcm) {
          callback(shortPcm, size, userdata, OVERLAP_QUIET_PART);
          free(shortPcm);
          shortPcm = NULL;
        }
        break;

      case EMOTIVOICE:
        // 通过EmotiVoice合成中文
        shortPcm = this->getPcmFromServer(Ekho::EMOTIVOICE_PORT, word->text, size, Ekho::EMOTIVOICE_AMPLIFY_RATE);
        if (shortPcm) {
          callback(shortPcm, size, userdata, OVERLAP_QUIET_PART);
          free(shortPcm);
          shortPcm = NULL;
        }
        break;

      case ZHTTS:
        shortPcm = this->getPcmFromServer(Ekho::ZHTTS_PORT, word->text, size, Ekho::ZHTTS_AMPLIFY_RATE);
        if (shortPcm) {/*
          if (mDebug) {
    std::cerr << "Array values: ";
    for (size_t i = 0; i < size; ++i) {
        std::cerr << shortPcm[i] << ' ';
    }
    std::cerr << '\n';
          }*/
          callback(shortPcm, size, userdata, OVERLAP_QUIET_PART);
          free(shortPcm);
          shortPcm = NULL;
        }
        break;
    }
  }  // end of for

  // send a signal to abort for Android
  if (userdata) {
    callback(0, 0, userdata, OVERLAP_NONE);
  }
  /* there is flush logic outside synth2, no need here
  else {
    callback(0, 0, this, OVERLAP_NONE);
  }*/

  return 0;
}
 
#include <iostream>  
#include <cstring>  
#include <sys/socket.h>  
#include <arpa/inet.h>  
#include <unistd.h>
// It's caller's responsibility to delete the returned pointer
short* EkhoImpl::getPcmFromServer(int port, string text, int& size, float amplifyRate) {
  if (mDebug) {
    cerr << "getPcmFromServer(" << port << ", " << text << ")" << endl;
  }

  // 创建socket  
  int sock = socket(AF_INET, SOCK_STREAM, 0);  
  if (sock == -1) {  
    std::cerr << "Failed to create socket" << std::endl;  
    return NULL;
  }  

  // 设置服务器地址结构体  
  struct sockaddr_in server_addr;  
  server_addr.sin_family = AF_INET;  
  server_addr.sin_port = htons(port); // 服务器端口号  
  server_addr.sin_addr.s_addr = inet_addr("127.0.0.1"); // 服务器IP地址  
  bzero(&(server_addr.sin_zero), 8);  

  // 连接服务器  
  if (connect(sock, (struct sockaddr *)&server_addr, sizeof(server_addr)) == -1) {  
    std::cerr << "Failed to connect to server at port " << port << std::endl;  
    close(sock);  
    return NULL;
  }  

  // 发送数据  
  if (send(sock, text.c_str(), strlen(text.c_str()), 0) == -1) {  
      std::cerr << "Failed to send message" << std::endl;  
      close(sock);  
      return NULL;
  }  

  // 接收数据  
  char buffer[1024];  
  memset(buffer, 0, sizeof(buffer));  
  if (recv(sock, buffer, sizeof(buffer), 0) == -1) {  
      std::cerr << "Failed to receive message" << std::endl;  
      close(sock);  
      return NULL;
  }  
  //std::cout << "Received message: " << buffer << std::endl;  

  // 关闭socket连接  
  close(sock);

  if (strlen(buffer) > 0) {
    // @TODO: convert samplerate from 16000 to ...
    short* pcm = this->audio->readPcmFromAudioFile(buffer, size);

    if (mDebug) {
      cerr << "getPcmFromServer: path=" << buffer << ", size=" << size << endl;
      /*
      std::cerr << "Array values: ";
      for (size_t i = 0; i < size; ++i) {
          std::cerr << pcm[i] << ' ';
      }
      std::cerr << '\n';*/
    }

    // amplify
    if (amplifyRate != 1) {
      short* amplifiedPcm = this->audio->amplifyPcm(pcm, size, amplifyRate);
      delete[] pcm;
      pcm = NULL;
      return amplifiedPcm;
    } else {
      return pcm;
    }
  }

  return NULL;
}