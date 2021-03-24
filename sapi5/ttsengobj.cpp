/***************************************************************************
 * Copyright (C) 2008-2013 by Cameron Wong                                 *
 * name in passport: HUANG GUANNENG                                        *
 * email: hgneng at gmail.com                                              *
 * website: http://www.eguidedog.net                                       *
 *                                                                         *
 * This program is free software; you can redistribute it and/or           *
 * modify it under the terms of the GNU General Public License             *
 * as published by the Free Software Foundation; either version 2          *
 * of the License, or (at your option) any later version.                  *
 *                                                                         *
 * This program is distributed in the hope that it will be useful,         *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
 * GNU General Public License for more details.                            *
 * (http://www.gnu.org/licenses/gpl-2.0.html)                              *
 *                                                                         *
 **************************************************************************/

//--- Additional includes
#include "stdafx.h"
#include "TtsEngObj.h"
#include "ekho_dict.h"
#include "festival.h"
#define OUTPUT16BIT
#include <iostream>
#include <fstream>
#include "ssml.h"

using namespace std;
using namespace ekho;

//--- Local

/*****************************************************************************
* CTTSEngObj::FinalConstruct *
*----------------------------*
*   Description:
*       Constructor
*****************************************************************************/
HRESULT CTTSEngObj::FinalConstruct()
{
  HRESULT hr = S_OK;

    //--- Init vars
  m_hVoiceData = NULL;
  m_pVoiceData = NULL;
  m_pWordList  = NULL;
  m_ulNumWords = 0;

  mSonicStream = 0;
  mOutputSite = 0;
  mPcmCache = true;
  mOverlap = 4096;
  this->audio = new Audio();
  supportSsml = true;
  this->isStopped = false;
  this->isPaused = false;

  memset(mAlphabetPcmCache, 0, 26);
  memset(mAlphabetPcmSize, 0, 26);

  mDebug = true;
  mDict.mDebug = true;
  mDebugFile = "d:/ekho/sapi5/debug/debug.txt";

  return hr;
} /* CTTSEngObj::FinalConstruct */

/*****************************************************************************
* CTTSEngObj::FinalRelease *
*--------------------------*
*   Description:
*       destructor
*****************************************************************************/
void CTTSEngObj::FinalRelease()
{
//    delete m_pWordList;
	if (mSonicStream) {
		sonicDestroyStream(mSonicStream);
		mSonicStream = 0;
	}

  if( m_pVoiceData ) {
    ::UnmapViewOfFile( (void*)m_pVoiceData );
  }

  if( m_hVoiceData ) {
    ::CloseHandle( m_hVoiceData );
  }
} /* CTTSEngObj::FinalRelease */

/*****************************************************************************
* CTTSEngObj::MapFile *
*---------------------*
*   Description:
*       Helper function used by SetObjectToken to map file.  This function
*   assumes that m_cpToken has been initialized.
*****************************************************************************/
HRESULT CTTSEngObj::MapFile( const WCHAR * pszTokenVal,  // Value that contains file path
                            HANDLE * phMapping,          // Pointer to file mapping handle
                            void ** ppvData )            // Pointer to the data
{
    USES_CONVERSION;
    HRESULT hr = S_OK;
    CSpDynamicString dstrFilePath;
    hr = m_cpToken->GetStringValue( pszTokenVal, &dstrFilePath );
    if ( SUCCEEDED( hr ) )
    {
        bool fWorked = false;
        *phMapping = NULL;
        *ppvData = NULL;
        HANDLE hFile;
#ifdef _WIN32_WCE
        hFile = CreateFileForMapping( dstrFilePath, GENERIC_READ,
                                      FILE_SHARE_READ, NULL, OPEN_EXISTING,
                                      FILE_ATTRIBUTE_NORMAL, NULL );
#else
        hFile = CreateFile( W2T(dstrFilePath), GENERIC_READ,
                            FILE_SHARE_READ, NULL, OPEN_EXISTING,
                            FILE_ATTRIBUTE_NORMAL, NULL );
#endif
        if (hFile != INVALID_HANDLE_VALUE)
        {
            *phMapping = ::CreateFileMapping( hFile, NULL, PAGE_READONLY, 0, 0, NULL );
            if (*phMapping)
            {
                *ppvData = ::MapViewOfFile( *phMapping, FILE_MAP_READ, 0, 0, 0 );
                if (*ppvData)
                {
                    fWorked = true;
                }
            }
            ::CloseHandle( hFile );
        }
        if (!fWorked)
        {
            hr = HRESULT_FROM_WIN32(::GetLastError());
            if (*phMapping)
            {
                ::CloseHandle(*phMapping);
                *phMapping = NULL;
            }
        }
    }
    return hr;
} /* CTTSEngObj::MapFile */

//
//=== ISpObjectWithToken Implementation ======================================
//

/*****************************************************************************
* CTTSEngObj::SetObjectToken *
*----------------------------*
*   Description:
*       This function performs the majority of the initialization of the voice.
*   Once the object token has been provided, the filenames are read from the
*   token key and the files are mapped.
*****************************************************************************/
STDMETHODIMP CTTSEngObj::SetObjectToken(ISpObjectToken * pToken)
{
  HRESULT hr = SpGenericSetObjectToken(pToken, m_cpToken);


  //--- Map the voice data so it will be shared among all instances
  //  Note: This is a good example of how to memory map and share
  //        your voice data across instances.
  if( SUCCEEDED( hr ) )
  {
      //hr = MapFile( L"VoiceData", &m_hVoiceData, &m_pVoiceData );
	  hr = m_cpToken->GetStringValue( L"EKHO_DATA_PATH", &m_dstrDirPath );
	  hr = m_cpToken->GetStringValue( L"Voice", &m_dstrVoice );
  }

  char buffer[256] = {0};
  WCHAR *path = m_dstrDirPath.Copy();
  WideCharToMultiByte(CP_OEMCP,NULL,path,m_dstrDirPath.Length(),buffer, sizeof(buffer) - 1,NULL,FALSE);
  mDict.mDataPath = buffer;

  memset(buffer, 0, sizeof(buffer));
  path = m_dstrVoice.Copy();
  WideCharToMultiByte(CP_OEMCP,NULL,path,m_dstrVoice.Length(),buffer, sizeof(buffer) - 1,NULL,FALSE);
  if (strcmp(buffer, "pinyin") == 0) {
	  mDict.setLanguage(MANDARIN);
  } else if (strcmp(buffer, "jyutping") == 0) {
	  mDict.setLanguage(CANTONESE);
  } else if (strcmp(buffer, "hakka") == 0) {
	  mDict.setLanguage(HAKKA);
	  setEnglishVoice("voice_kal_diphone");
  } else if (strcmp(buffer, "tibetan") == 0) {
	  mDict.setLanguage(TIBETAN);
  } else if (strcmp(buffer, "hangul") == 0) {
	  mDict.setLanguage(KOREAN);
  } else if (strcmp(buffer, "ngangien") == 0) {
	  mDict.setLanguage(NGANGIEN);
  } else if (strcmp(buffer, "English") == 0) {
	  mDict.setLanguage(ENGLISH);
  } else {
	  return -1;
  }

  if (mDebug) {
    cerr << "data path: " << mDict.mDataPath << ", voice=" << buffer << endl;
  }
  mDict.setVoice(buffer);

  mSonicStream = sonicCreateStream(mDict.mSfinfo.samplerate, 1);
  mPendingFrames = 0;

  initFestival();

  return hr;
} /* CTTSEngObj::SetObjectToken */

//
//=== ISpTTSEngine Implementation ============================================
//

int CTTSEngObj::writeToSonicStream(short *pcm, int frames, OverlapType type) {
  if (!mSonicStream) return 0;
  if (frames * 2 > 819200 - mPendingFrames) {
    // a bad fix for overflow issue
    frames = (int)(819200 - mPendingFrames) / 2;
  }

  const int quiet_level = mOverlap;
  int flushframes = 0;
  int cpframe = 0;
  int startframe = 0;
  int endframe = mPendingFrames - 1;
  int i = 0;
  int q_level = 0;
  // promise length not less than de5 * 0.8.
  int minFrames = (int)mDict.mSfinfo.frames * 0.8;
  //cerr << "frames:" << frames << ",minFrames:" << minFrames << ",type:" << type << endl;

  switch (type) {
    case OVERLAP_NONE:
      memcpy(mPendingPcm + mPendingFrames, pcm, frames * 2);
      mPendingFrames += frames;
      flushframes = mPendingFrames;
      cpframe = frames;
      break;

    case OVERLAP_QUIET_PART:
      // don't overlap more than 0.3(endframe)+0.3(startframe) of the syllable frames

      // find quiet frames
      while (endframe > 0 &&
        mPendingFrames - endframe < frames * 0.3 &&
        mPendingFrames - endframe < (frames - minFrames) * 0.5 &&
        abs(mPendingPcm[endframe]) < quiet_level) {
        endframe--;
      }

      while (startframe < frames * 0.3 &&
          startframe < (frames - minFrames) * 0.5 &&
          abs(pcm[startframe]) < quiet_level) {
        startframe++;
      }

      // prevent valume over max
      // search for a proper startframe position
      i = endframe;
      while (i > 0 && endframe - i < startframe &&
          abs(mPendingPcm[i]) < 32767 - quiet_level) {
        i--;
      }

      if (endframe - i < startframe) {
        // remember a large frame and search back for small quite frame to overlarp
        q_level = 32767 - abs(mPendingPcm[i]);
        while (i > 0 && endframe - i < startframe &&
            abs(pcm[endframe - i]) < q_level) {
          i--;
        }

        if (endframe - i < startframe) {
          //cerr << "startframe: " << startframe << " to " << endframe - i << endl;
          startframe = endframe - i;
        }
      }

      // search for a proper endframe position
      i = startframe;
      while (i < frames && i - startframe < mPendingFrames - endframe - 1 &&
          abs(pcm[i]) < 32767 - quiet_level) {
        i++;
      }

      if (i - startframe < mPendingFrames - endframe - 1) {
        q_level = 32767 - abs(pcm[i]);
        while (i < frames && i - startframe < mPendingFrames - endframe - 1 &&
            abs(mPendingPcm[mPendingFrames + startframe - i - 1]) < q_level) {
          i++;
        }

        if (i - startframe < mPendingFrames - endframe - 1) {
          //cerr << "endframe: " << endframe << " to " << mPendingFrames + startframe - i - 1<< endl;
          endframe = mPendingFrames + startframe - i - 1;
        }
      }

      for (i = max(0, endframe - startframe);
          i < mPendingFrames && cpframe < frames; i++) {
        mPendingPcm[i] += pcm[cpframe];
        if (mPendingPcm[i] > 32000) {
          //cerr << "overflow: " << mPendingPcm[i] << endl;
        }
        cpframe++;
      }

      if (frames == 0) {
        // frames=0 means flush all pending frames
        flushframes = i;
      } else if (mPendingFrames + mPendingFrames > frames) {
        // guaranteer pending frames no more than haft frames
        flushframes = (int)mPendingFrames - frames * 0.5;
      }

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

      break;
  }

  sonicWriteShortToStream(mSonicStream, mPendingPcm, flushframes);
  mPendingFrames -= flushframes;
  if (mPendingFrames > 0) {
    memcpy(mPendingPcm, mPendingPcm + flushframes, mPendingFrames * 2);
  }
  memcpy(mPendingPcm + mPendingFrames, pcm + cpframe, (frames - cpframe) * 2);
  mPendingFrames += frames - cpframe;

  return flushframes;
}

int CTTSEngObj::writePcm(short *pcm, int frames, void *arg, OverlapType type,
                       bool tofile) {
  const int BUFFER_SIZE = 40960;
  short *buffer = new short[BUFFER_SIZE];

  CTTSEngObj *pEkho = (CTTSEngObj *)arg;
  if (!pEkho->mSonicStream) {
    return -1;
  }

  int flush_frames = pEkho->writeToSonicStream(pcm, frames, type);

  if (flush_frames) {
    do {
      // sonic会自动剪去一些空白的frame
      frames = sonicReadShortFromStream(pEkho->mSonicStream, buffer, BUFFER_SIZE);
      //cerr << "sonicReadShortFromStream: " << frames << endl;

      if (frames > 0/* && !pEkho->isStopped*/) {
        if ( SP_IS_BAD_INTERFACE_PTR( pEkho->mOutputSite )) {
          return -2;
        }
        pEkho->mOutputSite->Write((const void*)buffer, (ULONG)(frames * 2), 0);
      }
    } while (frames > 0);
  }

  if (!frames) {
    sonicFlushStream(pEkho->mSonicStream);  // TODO: needed?
  }

  delete[] buffer;
  return 0;
}

void CTTSEngObj::translatePunctuations(string &text, EkhoPuncType mode) {
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

/*****************************************************************************
* CTTSEngObj::Speak *
*-------------------*
*   Description:
*       This is the primary method that SAPI calls to render text.
*-----------------------------------------------------------------------------
*   Input Parameters
*
*   pUser
*       Pointer to the current user profile object. This object contains
*       information like what languages are being used and this object
*       also gives access to resources like the SAPI master lexicon object.
*
*   dwSpeakFlags
*       This is a set of flags used to control the behavior of the
*       SAPI voice object and the associated engine.
*
*   VoiceFmtIndex
*       Zero based index specifying the output format that should
*       be used during rendering.
*
*   pTextFragList
*       A linked list of text fragments to be rendered. There is
*       one fragement per XML state change. If the input text does
*       not contain any XML markup, there will only be a single fragment.
*
*   pOutputSite
*       The interface back to SAPI where all output audio samples and events are written.
*
*   Return Values
*       S_OK - This should be returned after successful rendering or if
*              rendering was interrupted because *pfContinue changed to FALSE.
*       E_INVALIDARG 
*       E_OUTOFMEMORY
*
*****************************************************************************/
STDMETHODIMP CTTSEngObj::Speak( DWORD dwSpeakFlags,
                                REFGUID rguidFormatId,
                                const WAVEFORMATEX * pWaveFormatEx,
                                const SPVTEXTFRAG* pTextFragList,
                                ISpTTSEngineSite* pOutputSite )
{
	HRESULT hr = S_OK;
  SynthCallback *callback = writePcm;
  mOutputSite = pOutputSite;

	//--- Check args
	if( SP_IS_BAD_INTERFACE_PTR( pOutputSite ) ||
		SP_IS_BAD_READ_PTR( pTextFragList )  )
	{
		hr = E_INVALIDARG;
    return hr;
	}

	long rate; // [-10,10]
	float rateDelta = 0;
	if (pOutputSite->GetRate(&rate) == S_OK) {
		if (rate >= 0)
			rateDelta = (float)rate * 3 / 10;
		else
			rateDelta = (float)rate * 0.8 / 10;
	}

	USHORT volume = 100; // [0, 100]
	pOutputSite->GetVolume(&volume);

  //cerr << "rate: " << rateDelta << endl;
  if (!mSonicStream) {
      return 2;
  }
  sonicSetSpeed(mSonicStream, 1 + rateDelta);
  sonicSetVolume(mSonicStream, (float)volume / 100);

	//--- Init some vars
  m_pCurrFrag   = pTextFragList;
  while (m_pCurrFrag) {
	  m_pNextChar   = m_pCurrFrag->pTextStart;
	  m_pEndChar    = m_pNextChar + m_pCurrFrag->ulTextLen;
	  m_ullAudioOff = 0;

	  // skip bookmark
	  if (m_pCurrFrag->State.eAction == SPVA_Bookmark) {
		  m_pCurrFrag = m_pCurrFrag->pNext;
		  continue;
	  }

	  // get pitch
	  long pitch = m_pCurrFrag->State.PitchAdj.MiddleAdj; // [-10, 10]
	  float pitchDelta = 0;
	  if (pitch >= 0)
		  pitchDelta = (float)pitch / 10;
	  else
		  pitchDelta = (float)pitch * 0.5 / 10;
	  sonicSetPitch(mSonicStream, 1 + pitchDelta);

	  list<Character> char_list;
	  LPCWSTR c = m_pCurrFrag->pTextStart;
	  for (unsigned int i = 0; i < m_pCurrFrag->ulTextLen; i++) {
		  char_list.push_back(unsigned short(*c));
		  c++;
	  }

    string text = Character::join(char_list);
    if (text.empty()) {
        m_pCurrFrag = m_pCurrFrag->pNext;
        continue;
    }

    if (mDebug) {
      cerr << "speaking lang(" << mDict.getLanguage() << "): '" << text << "'" << endl;
      std::ofstream file(mDebugFile, std::ios_base::app);
      file << "speaking lang(" << mDict.getLanguage() << "): '" << text << "'" << endl;
      file.close();
    }

    void *userdata = this;
    this->isStopped = false;
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
          m_pCurrFrag = m_pCurrFrag->pNext;
          continue;
      }
    }

    // check punctuation
    if (mSpeakIsolatedPunctuation && text.length() <= 3) {
      const char *c = text.c_str();
      int code = 0;
      try {
          utf8::next(c, c + text.length());
      }
      catch (...) {
          cerr << "tail to parse utf8:" << text << endl;
      }

      if (!*c && mDict.isPunctuationChar(code, EKHO_PUNC_ALL)) {
        text = mDict.getPunctuationName(code);
        if (text.empty()) {
            m_pCurrFrag = m_pCurrFrag->pNext;
            continue;
        }
      }
    }

    // translate punctuations
    translatePunctuations(text, mPuncMode);

    // filter spaces
    Ssml::filterSpaces(text);
    if (mDebug) {
      cerr << "filterSpaces: " << text << endl;
    }

  #ifdef DEBUG_ANDROID
    LOGD("Ekho::synth2 filtered text=%s", text.c_str());
  #endif

    list<Word> wordlist = mDict.lookupWord(text);

    list<PhoneticSymbol *>::iterator phon_symbol;
    for (list<Word>::iterator word = wordlist.begin(); word != wordlist.end();
         word++) {
      if (mDebug) {
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
            callback((short *)pPcm, size / 2, userdata, OVERLAP_NONE);
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
            if ((word->text.length() == 1) && (c = tolower(word->text.c_str()[0])) && c >= 'a' && c <= 'z') {
              if (!mAlphabetPcmCache[c - 'a']) {
                mAlphabetPcmCache[c - 'a'] =
                    getEnglishPcm(word->text, mAlphabetPcmSize[c - 'a']);
              }

              pPcm = mAlphabetPcmCache[c - 'a'];
              size = mAlphabetPcmSize[c - 'a'];
              if (pPcm) {
                callback((short *)pPcm, size / 2, userdata, OVERLAP_NONE);
              }
            } else {
              pPcm = this->getEnglishPcm(word->text, size);
              if (pPcm && size > 0) {
                callback((short *)pPcm, size / 2, userdata, OVERLAP_NONE);
                delete[] pPcm;
              }
            }
            pPcm = 0;
            size = 0;
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
              //list<OverlapType>::iterator type = word->overlapTypes.begin();
              for (list<PhoneticSymbol *>::iterator symbol =
                       word->symbols.begin();
                   symbol != word->symbols.end(); symbol++) {
  #ifdef DEBUG_ANDROID
                LOGD("Ekho::synth2 speak %s", (*symbol)->symbol);
  #endif
                Language lang = mDict.getLanguage();
                string path = mDict.mDataPath + "/" + mDict.getVoice();
                pPcm =
                    (*symbol)->getPcm(path.c_str(), mDict.mVoiceFileType, size);
                if (pPcm && size > 0)
                  callback((short *)pPcm, size / 2, userdata, OVERLAP_QUIET_PART); // type

                // speak Mandarin for Chinese
                if (!pPcm && lang == TIBETAN) {
                  string path = mDict.mDataPath + "/pinyin";
                  pPcm =
                      (*symbol)->getPcm(path.c_str(), mDict.mVoiceFileType, size);
                  if (pPcm && size > 0)
                    callback((short *)pPcm, size / 2, userdata, OVERLAP_QUIET_PART); // type
                }

                if (!mPcmCache) (*symbol)->setPcm(0, 0);

                // FIXME: there is a bug here number of overlapTypes and symbol may not equal.
                // ex. 21 has 3 symbols(2 shi 1) and 2 types
                //type++;
              }
            }
          }
          break;
      }
    }  // end of for

    if (userdata)
      callback(0, 0, userdata, OVERLAP_NONE);
    else
      callback(0, 0, this, OVERLAP_NONE);

	  m_pCurrFrag = m_pCurrFrag->pNext;
  }

  return hr;
}

	/*
STDMETHODIMP CTTSEngObj::Speak( DWORD dwSpeakFlags,
                                REFGUID rguidFormatId,
                                const WAVEFORMATEX * pWaveFormatEx,
                                const SPVTEXTFRAG* pTextFragList,
                                ISpTTSEngineSite* pOutputSite )
{
	// TODO: although the version is advanced to 6.0. But it's still 5.8's code. not integrate synth2 yet.
	SPDBG_FUNC( "CTTSEngObj::Speak" );
	HRESULT hr = S_OK;
  
  Dict& mDict = *m_dict;

	//--- Check args
	if( SP_IS_BAD_INTERFACE_PTR( pOutputSite ) ||
		SP_IS_BAD_READ_PTR( pTextFragList )  )
	{
		hr = E_INVALIDARG;
	}
	else
	{
		//--- Init some vars
	  m_pCurrFrag   = pTextFragList;
	  while (m_pCurrFrag) {
		m_pNextChar   = m_pCurrFrag->pTextStart;
		m_pEndChar    = m_pNextChar + m_pCurrFrag->ulTextLen;
		m_ullAudioOff = 0;

		// skip bookmark
		if (m_pCurrFrag->State.eAction == SPVA_Bookmark) {
			m_pCurrFrag = m_pCurrFrag->pNext;
			continue;
		}

		// get pitch
		long pitch = m_pCurrFrag->State.PitchAdj.MiddleAdj; // [-10, 10]
		float pitchDelta = 0;
		if (pitch >= 0)
			pitchDelta = (float)pitch / 10;
		else
			pitchDelta = (float)pitch * 0.5 / 10;

		list<Character> char_list;
		LPCWSTR c = m_pCurrFrag->pTextStart;
		for (unsigned int i = 0; i < m_pCurrFrag->ulTextLen; i++) {
			char_list.push_back(unsigned short(*c));
			c++;
		}

		list<PhoneticSymbol*> phons = this->mDict.lookup(char_list);
		bool has_unknown_char = false;
		bool has_festival_char = false;
		float pause = 0;
		string unknown_str = "";
		const char *pPcm = NULL;

		int last_pos = -1;
		int pos = 0;
		for (list<PhoneticSymbol*>::iterator li = phons.begin(); li != phons.end() || has_unknown_char; pos++) {
			//    cout << (*li)->symbol << " ";

			if( !(pOutputSite->GetActions() & SPVES_ABORT) )
			{

				//--- Fire begin sentence event
				CSpEvent Event;
				Event.eEventId             = SPEI_SENTENCE_BOUNDARY;
				Event.elParamType          = SPET_LPARAM_IS_UNDEFINED;
				Event.ullAudioStreamOffset = m_ullAudioOff;
				Event.lParam               = (LPARAM)0;
				Event.wParam               = (WPARAM)m_pCurrFrag->ulTextLen;
				hr = pOutputSite->AddEvents( &Event, 1 );

				//--- Output
				if( SUCCEEDED( hr ) )
				{
					bool speaknow = false;
					if (li != phons.end() && ((!*li) ||
						((*li)->symbol && (((*li)->symbol[0] == '\\') ||
						(has_unknown_char && strstr((*li)->symbol, "pause") > 0))))) {
							has_unknown_char = true;

						if (*li && strstr((*li)->symbol, "pause") > 0) {
							unknown_str += " ";

							// break ,. etc. for Festival
							if (strstr((*li)->symbol, "fullpause") > 0 ||
								strstr((*li)->symbol, "halfpause") > 0) {
								speaknow = true;
							}
						} else {
							if (*li && strlen((*li)->symbol) == 2 && (*li)->symbol[1] >= 65 && (*li)->symbol[1] <= 122) {
								unknown_str += (*li)->symbol + 1;
								has_festival_char = true;
							} else if (*li && strlen((*li)->symbol) == 3) {
								int code = utf8::peek_next((*li)->symbol + 1, (*li)->symbol + 3);
								if (code <= 0xff) {
									unknown_str += (unsigned char)code;
									has_festival_char = true;
								} else {
									unknown_str += " ";
								}
							} else {
								// illegal characters for Festival
								unknown_str += " ";
							}
						}
					} else if (li != phons.end() && (*li) && strstr((*li)->symbol, "fullpause") > 0) {
					  pause += 1;
					} else if (li != phons.end() && (*li) && strstr((*li)->symbol, "halfpause") > 0) {
					  pause += 0.5;
					} else if (li != phons.end() && (*li) && strstr((*li)->symbol, "quaterpause") > 0) {
					  pause += 0.25;
					} else {
					  speaknow = true;
					}

					if (speaknow) {
						int size;
						pPcm = 0;
						if (has_unknown_char) {
							if (has_festival_char) {
								pPcm = getPcmFromFestival(unknown_str, size);
								if (! pPcm) {
									has_unknown_char = false;
									pPcm = mDict.getFullPause()->getPcm(size);
								}
								pause = 0;
								unknown_str.clear();
								li--;
								pos--;
							} else if (unknown_str.size() > 1) {
								pause += 0.25;
							} else {
								unknown_str.clear();
								li--;
								pos--;
							}
						}
						
						if (pause > 0) {
							if (pause >= 1)
							  pPcm = mDict.getFullPause()->getPcm(size);
							else if (pause >= 0.5)
							  pPcm = mDict.getHalfPause()->getPcm(size);
							else
							  pPcm = mDict.getQuaterPause()->getPcm(size);
							pause = 0;
							if (!has_unknown_char) {
  								li--;
								pos--;
							}
						} else if (!pPcm && li != phons.end() && (*li)) {
							string path = mDict.mDataPath + "/" + mDict.getVoice();
							pPcm = (*li)->getPcm(path.c_str(), mDict.mVoiceFileType, size);
												        
							// speak Mandarin for Chinese
							if (!pPcm && mDict.getLanguage() == TIBETAN) {
						      path = mDict.mDataPath + "/pinyin";
							  pPcm = (*li)->getPcm(path.c_str(), mDict.mVoiceFileType, size);
							}
						}

						if (pPcm) {
							//--- We don't say anything for punctuation or control characters
							//    in this sample. 
							//--- Queue the event

							CSpEvent Event;
							Event.eEventId             = SPEI_WORD_BOUNDARY;
							Event.elParamType          = SPET_LPARAM_IS_UNDEFINED;
							Event.ullAudioStreamOffset = m_ullAudioOff;
							Event.lParam               = (LPARAM)pos;
							Event.wParam               = (WPARAM)(pos - last_pos);
							last_pos = pos;
							hr = pOutputSite->AddEvents( &Event, 1 );

							//--- Queue the audio data
							long rate; // [-10,10]
							float rateDelta = 0;
							if (pOutputSite->GetRate(&rate) == S_OK) {
								if (rate >= 0)
									rateDelta = (float)rate * 3 / 10;
								else
									rateDelta = (float)rate * 0.8 / 10;
							}

							USHORT volume = 100; // [0, 100]
							pOutputSite->GetVolume(&volume);

							sonicSetSpeed(mSonicStream, 1 + rateDelta);
							sonicSetVolume(mSonicStream, (float)volume / 100);
							sonicSetPitch(mSonicStream, 1 + pitchDelta);
							//int frames = sonicWriteShortToStream(mSonicStream, (short*)pPcm,
							//	size / 2);
							int frames = writeToSonicStream((short*)pPcm, size / 2);
							if (frames == 0) {
								sonicFlushStream(mSonicStream);
							}

							const size_t BUFFER_SIZE = 65536;
							short buffer[BUFFER_SIZE]; 
							do {
								frames = sonicReadShortFromStream(mSonicStream, buffer,
									BUFFER_SIZE);
								hr = pOutputSite->Write((const void*)buffer, (ULONG)(frames * 2), 0);
								//--- Update the audio offset
								m_ullAudioOff += frames * 2;
							} while (frames == BUFFER_SIZE);
						}

						if (has_unknown_char) {
							if (has_festival_char) {
								has_festival_char = false;
								if (pPcm) {
								  delete[] pPcm;
								  pPcm = 0;
								}
							}
							has_unknown_char = false;
						}
					}
				}
			}
			if (li != phons.end())
				li++;
		} // end of for

		// flush pcm
		int frames = writeToSonicStream(0, 0);
		if (frames == 0) {
			sonicFlushStream(mSonicStream);
		}

		const size_t BUFFER_SIZE = 65536;
		short buffer[BUFFER_SIZE]; 
		do {
			frames = sonicReadShortFromStream(mSonicStream, buffer,
				BUFFER_SIZE);
			hr = pOutputSite->Write((const void*)buffer, (ULONG)(frames * 2), 0);
			//--- Update the audio offset
			m_ullAudioOff += frames * 2;
		} while (frames == BUFFER_SIZE);
		// end of flush pcm

		m_pCurrFrag = m_pCurrFrag->pNext;
	  }
	}

    return hr;
}*/ /* CTTSEngObj::Speak */

/*****************************************************************************
* CTTSEngObj::GetVoiceFormat *
*----------------------------*
*   Description:
*       This method returns the output data format associated with the
*   specified format Index. Formats are in order of quality with the best
*   starting at 0.
*****************************************************************************/
STDMETHODIMP CTTSEngObj::GetOutputFormat( const GUID * pTargetFormatId, const WAVEFORMATEX * pTargetWaveFormatEx,
                                          GUID * pDesiredFormatId, WAVEFORMATEX ** ppCoMemDesiredWaveFormatEx )
{
    HRESULT hr = S_OK;

	if (this->mDict.mSfinfo.samplerate == 16000) {
		hr = SpConvertStreamFormatEnum(SPSF_16kHz16BitMono, pDesiredFormatId, ppCoMemDesiredWaveFormatEx);
	} else if (this->mDict.mSfinfo.samplerate == 44100) {
	    hr = SpConvertStreamFormatEnum(SPSF_44kHz16BitMono, pDesiredFormatId, ppCoMemDesiredWaveFormatEx);
	} else if (this->mDict.mSfinfo.samplerate == 8000) {
		hr = SpConvertStreamFormatEnum(SPSF_8kHz16BitMono, pDesiredFormatId, ppCoMemDesiredWaveFormatEx);
	}

    return hr;
} /* CTTSEngObj::GetVoiceFormat */

//
//=== This code is just a simplified parser ==================================
//
/*****************************************************************************
* CTTSEngObj::GetNextSentence *
*-----------------------------*
*   This method is used to create a list of items to be spoken.
****************************************************************************/
HRESULT CTTSEngObj::GetNextSentence( CItemList& ItemList )
{
    HRESULT hr = S_OK;

    //--- Clear the destination
    ItemList.RemoveAll();

    //--- Is there any work to do
    if( m_pCurrFrag == NULL )
    {
        hr = S_FALSE;
    }
    else
    {
        BOOL fSentDone = false;
        BOOL fGoToNextFrag = false;

        while( m_pCurrFrag && !fSentDone )
        {
            if( m_pCurrFrag->State.eAction == SPVA_Speak )
            {
                fSentDone = AddNextSentItem( ItemList );

                //--- Advance fragment?
                if( m_pNextChar >= m_pEndChar )
                {
                    fGoToNextFrag = true;
                }
            }
            else
            {
                //--- Add non spoken fragments
                CSentItem Item;
                Item.pItem           = m_pCurrFrag->pTextStart;
                Item.ulItemLen       = m_pCurrFrag->ulTextLen;
                Item.ulItemSrcOffset = m_pCurrFrag->ulTextSrcOffset;
                Item.ulItemSrcLen    = Item.ulItemLen;
                Item.pXmlState       = &m_pCurrFrag->State;
                ItemList.AddTail( Item );
                fGoToNextFrag = true;
            }

            if( fGoToNextFrag )
            {
                fGoToNextFrag = false;
                m_pCurrFrag = m_pCurrFrag->pNext;
                if( m_pCurrFrag )
                {
                    m_pNextChar = m_pCurrFrag->pTextStart;
                    m_pEndChar  = m_pNextChar + m_pCurrFrag->ulTextLen;
                }
                else
                {
                    m_pNextChar = NULL;
                    m_pEndChar  = NULL;
                }
            }
        } // end while

        if( ItemList.IsEmpty() )
        {
            hr = S_FALSE;
        }
    }
    return hr;
} /* CTTSEngObj::GetNextSentence */

/*****************************************************************************
* IsSpace *
*---------*
*   Returns true if the character is a space, tab, carriage return, or line feed.
****************************************************************************/
static BOOL IsSpace( WCHAR wc )
{
    return ( ( wc == 0x20 ) || ( wc == 0x9 ) || ( wc == 0xD  ) || ( wc == 0xA ) );
}

/*****************************************************************************
* SkipWhiteSpace *
*----------------*
*   Returns the position of the next non-whitespace character.
****************************************************************************/
static const WCHAR* SkipWhiteSpace( const WCHAR* pPos )
{
    while( IsSpace( *pPos ) ) ++pPos;
    return pPos;
}

/*****************************************************************************
* FindNextToken *
*---------------*
*   Locates the next space delimited token in the stream
****************************************************************************/
static const WCHAR* 
    FindNextToken( const WCHAR* pStart, const WCHAR* pEnd, const WCHAR*& pNext )
{
    const WCHAR* pPos = SkipWhiteSpace( pStart );
    pNext = pPos;
    if( pNext == pEnd )
    {
        pPos = NULL;
    }
    else
    {
        while( *pNext && !IsSpace( *pNext ) )
        {
            if( ++pNext == pEnd )
            {
                //--- This can happen when a text fragment is
                //    tight up against a following xml tag.
                break;
            }
        }
    }
    return pPos;
} /* FindNextToken */

/*****************************************************************************
* SearchSet *
*-----------*
*   Finds the character in the specified array.
****************************************************************************/
BOOL SearchSet( WCHAR wc, const WCHAR* Set, ULONG Count, ULONG* pIndex )
{
    for( ULONG i = 0; i < Count; ++i )
    {
        if( wc == Set[i] )
        {
            *pIndex = i;
            return true;
        }
    }
    return false;
}

/*****************************************************************************
* CTTSEngObj::AddNextSentItem *
*-----------------------------*
*   Locates the next sentence item in the stream and adds it to the list.
*   Returns true if the last item added is the end of the sentence.
****************************************************************************/
BOOL CTTSEngObj::AddNextSentItem( CItemList& ItemList )
{
    //--- Get the token
    ULONG ulIndex;
    CSentItem Item;
    Item.pItem = FindNextToken( m_pNextChar, m_pEndChar, m_pNextChar );

    //--- This case can occur when we hit the end of a text fragment.
    //    Returning at this point will cause advancement to the next fragment.
    if( Item.pItem == NULL )
    {
        return false;
    }

    const WCHAR* pTrailChar = m_pNextChar-1;
    ULONG TokenLen = m_pNextChar - Item.pItem;

    //--- Split off leading punction if any
    static const WCHAR LeadItems[] = { L'(', L'\"', L'{', L'\'', L'[' };
    while( TokenLen > 1 )
    {
        if( SearchSet( Item.pItem[0], LeadItems, sp_countof(LeadItems), &ulIndex ) )
        {
            CSentItem LItem;
            LItem.pItem           = Item.pItem;
            LItem.ulItemLen       = 1;
            LItem.pXmlState       = &m_pCurrFrag->State;
            LItem.ulItemSrcLen    = LItem.ulItemLen;
            LItem.ulItemSrcOffset = m_pCurrFrag->ulTextSrcOffset +
                                    ( LItem.pItem - m_pCurrFrag->pTextStart );
            ItemList.AddTail( LItem );
            ++Item.pItem;
            --TokenLen;
        }
        else
        {
            break;
        }
    }

    //--- Get primary item insert position
    SPLISTPOS ItemPos = ItemList.AddTail( Item );

    //--- Split off trailing punction if any.
    static const WCHAR EOSItems[] = { L'.', L'!', L'?' };
    static const WCHAR TrailItems[] = { L',', L'\"', L';', L':', L')', L'}', L'\'', L']' };
    SPLISTPOS NextPos = NULL;
    BOOL fIsEOS = false;
    while( TokenLen > 1 )
    {
        BOOL fAddTrailItem = false;
        if( SearchSet( *pTrailChar, EOSItems, sp_countof(EOSItems), &ulIndex ) )
        {
            fIsEOS = true;
            fAddTrailItem = true;
        }
        else if( SearchSet( *pTrailChar, TrailItems, sp_countof(TrailItems), &ulIndex ) )
        {
            fAddTrailItem = true;
        }

        if( fAddTrailItem )
        {
            CSentItem TItem;
            TItem.pItem           = pTrailChar;
            TItem.ulItemLen       = 1;
            TItem.pXmlState       = &m_pCurrFrag->State;
            TItem.ulItemSrcLen    = TItem.ulItemLen;
            TItem.ulItemSrcOffset = m_pCurrFrag->ulTextSrcOffset +
                                    ( TItem.pItem - m_pCurrFrag->pTextStart );
            NextPos = ItemList.InsertAfter( ItemPos, TItem );
            --TokenLen;
            --pTrailChar;
        }
        else
        {
            break;
        }
    }

    //--- Abreviation or sentence end?
    //    If we are at the end of the buffer then EOS is implied.
    if( *m_pNextChar == NULL )
    {
        fIsEOS = true;
        if( !SearchSet( *(m_pNextChar-1), EOSItems, sp_countof(EOSItems), &ulIndex ) )
        {
            //--- Terminate with a period if we are at the end of a buffer
            //    and no end of sentence punction has been added.
            static const WCHAR* pPeriod = L".";
            CSentItem EOSItem;
            EOSItem.pItem           = pPeriod;
            EOSItem.ulItemLen       = 1;
            EOSItem.pXmlState       = &m_pCurrFrag->State;
            EOSItem.ulItemSrcLen    = EOSItem.ulItemLen;
            EOSItem.ulItemSrcOffset = m_pCurrFrag->ulTextSrcOffset +
                                    ( (m_pNextChar-1) - m_pCurrFrag->pTextStart );
            ItemList.AddTail( EOSItem );
        }
    }
    else if( pTrailChar[1] == L'.' )
    {
        //--- Here is where you would try to prove that it's not EOS
        //    It might be an abreviation. That's a hard problem that
        //    we are not going to attempt here.
    }
    
    //--- Substitute underscore for apostrophe
    for( ULONG i = 0; i < TokenLen; ++i )
    {
        if( Item.pItem[i] == L'\'' )
        {
            ((WCHAR)Item.pItem[i]) = L'_';
        }
    }

    //--- Add the main item
    if( TokenLen > 0 )
    {
        Item.ulItemLen       = TokenLen;
        Item.pXmlState       = &m_pCurrFrag->State;
        Item.ulItemSrcLen    = Item.ulItemLen;
        Item.ulItemSrcOffset = m_pCurrFrag->ulTextSrcOffset +
                               ( Item.pItem - m_pCurrFrag->pTextStart );
        ItemList.SetAt( ItemPos, Item );
    }

    return fIsEOS;
} /* CTTSEngObj::AddNextSentItem */

const char* CTTSEngObj::getEnglishPcm(string text, int &size) {
#ifdef ENABLE_FESTIVAL
  return getPcmFromFestival(text, size);
#else
  //synthWithEspeak(text);
  return 0;
#endif
}

// It's caller's responsibility to delete the returned pointer
const char* CTTSEngObj::getPcmFromFestival(string text, int& size) { 
#ifdef ENABLE_FESTIVAL
  // set voice
  static const char *current_voice = "voice_kal_diphone";
  static int current_samplerate = 16000;
  /* voice_cmu_us_slt_arctic_hts is too slow
  if (strcmp(current_voice, mEnglishVoice) != 0) {
    current_voice = mEnglishVoice;
    char cmd[256];
    strcpy(cmd + 1, mEnglishVoice);
    cmd[0] = '(';
    cmd[strlen(mEnglishVoice) + 1] = ')';
    cmd[strlen(mEnglishVoice) + 2] = 0;
    festival_eval_command(cmd);
    //festival_eval_command("(voice_kal_diphone)");
    //festival_eval_command("(voice_cmu_us_slt_arctic_hts)");

    if (strcmp(current_voice, "voice_kal_diphone") == 0)
      current_samplerate = 16000;
    else if (strcmp(current_voice, "voice_cmu_us_slt_arctic_hts") == 0)
      current_samplerate = 32000;
  }*/

  EST_Wave wave;
  festival_text_to_wave(text.c_str(), wave);

  if (mDict.mSfinfo.samplerate != current_samplerate) {
    wave.resample(mDict.mSfinfo.samplerate);
  }

  EST_TVector<short> tvector;
  wave.channel(tvector, 0);
  size = tvector.p_num_columns * 2;
  char *pPcm = new char[size];
  short *shortPcm = (short*)pPcm;
  tvector.get_values(shortPcm, 1, 0, tvector.p_num_columns);

  // turn up volume for voice_JuntaDeAndalucia_es_pa_diphone
  /*
  short *p = shortPcm;
  short *pend = shortPcm + tvector.p_num_columns;
  while (p < pend) {
    *p = (short)(*p * 1.5);
    p++;
  }*/

  return pPcm;
#else
  return NULL;
#endif
}

int CTTSEngObj::initFestival(void) {
#ifdef ENABLE_FESTIVAL
  static bool isFestivalInited = false;
  if (isFestivalInited) {
    festival_tidy_up();
    return 1;
  }

  int heap_size = 2100000; // scheme heap size
  int load_init_files = 0; // don't load default festival init files
  festival_initialize(load_init_files, heap_size);

  // set libdir of festival
  string path(mDict.mDataPath);
  path += "/festival/lib";
  siod_set_lval("libdir", strintern(path.c_str()));

  path = mDict.mDataPath + "/festival/lib/init.scm";
  festival_load_file(path.c_str());

  // TODO: should change following line for custome language and voice
  //mEnglishVoice = "voice_kal_diphone";
  // mEnglishVoice = "voice_cmu_us_slt_arctic_hts"; // female voice
  mEnglishVoice = "voice_JuntaDeAndalucia_es_pa_diphone"; // mEnglishVoice has no use
  //festival_eval_command("(voice_JuntaDeAndalucia_es_sf_diphone)"); // Spanish female voice

  //path = mDict.mDataPath + "/festival/lib/voices/spanish/JuntaDeAndalucia_es_sf_diphone/festvox/JuntaDeAndalucia_es_sf_diphone.scm";
  //festival_load_file(path.c_str());
  festival_eval_command("(voice_JuntaDeAndalucia_es_pa_diphone)"); // Spanish male voice

  isFestivalInited = true;
#endif

  return 0;
}
