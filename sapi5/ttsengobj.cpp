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
#include "sr-convert.cpp"
#include <iostream>

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
    SPDBG_FUNC( "CTTSEngObj::FinalConstruct" );
    HRESULT hr = S_OK;

    //--- Init vars
    m_hVoiceData = NULL;
    m_pVoiceData = NULL;
    m_pWordList  = NULL;
    m_ulNumWords = 0;

	m_dict = NULL;
	mSonicStream = 0;

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
    SPDBG_FUNC( "CTTSEngObj::FinalRelease" );

//    delete m_pWordList;
	delete m_dict;
	if (mSonicStream) {
		sonicDestroyStream(mSonicStream);
		mSonicStream = 0;
	}

    if( m_pVoiceData )
    {
        ::UnmapViewOfFile( (void*)m_pVoiceData );
    }

    if( m_hVoiceData )
    {
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
    SPDBG_FUNC( "CTTSEngObj::SetObjectToken" );
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

	mEnglishVoice = "voice_kal_diphone";

	m_dict = new Dict();
	char buffer[256] = {0};
	WCHAR *path = m_dstrDirPath.Copy();
	WideCharToMultiByte(CP_OEMCP,NULL,path,m_dstrDirPath.Length(),buffer, sizeof(buffer) - 1,NULL,FALSE);
	m_dict->mDataPath = buffer;

	memset(buffer, 0, sizeof(buffer));
	path = m_dstrVoice.Copy();
	WideCharToMultiByte(CP_OEMCP,NULL,path,m_dstrVoice.Length(),buffer, sizeof(buffer) - 1,NULL,FALSE);
	if (strcmp(buffer, "pinyin") == 0) {
		m_dict->setLanguage(MANDARIN);
		setEnglishVoice("voice_cmu_us_slt_arctic_hts");
	} else if (strcmp(buffer, "jyutping") == 0) {
		m_dict->setLanguage(CANTONESE);
		setEnglishVoice("voice_kal_diphone");
	} else if (strcmp(buffer, "hakka") == 0) {
		m_dict->setLanguage(HAKKA);
		setEnglishVoice("voice_kal_diphone");
	} else if (strcmp(buffer, "tibetan") == 0) {
		m_dict->setLanguage(TIBETAN);
		setEnglishVoice("voice_cmu_us_slt_arctic_hts");
	} else if (strcmp(buffer, "hangul") == 0) {
		m_dict->setLanguage(KOREAN);
		setEnglishVoice("voice_cmu_us_slt_arctic_hts");
	} else if (strcmp(buffer, "ngangien") == 0) {
		m_dict->setLanguage(NGANGIEN);
		setEnglishVoice("voice_kal_diphone");
	} else if (strcmp(buffer, "English") == 0) {
		m_dict->setLanguage(ENGLISH);
	} else {
		return -1;
	}

	m_dict->setVoice(buffer);

//	static bool isSonicInited = false;
//	if (!isSonicInited) 
	mSonicStream = sonicCreateStream(m_dict->mSfinfo.samplerate, 1);
	mPendingFrames = 0;
//		isSonicInited = true;
//	}

	static bool isFestivalInited = false;
	if (!isFestivalInited) {
		initFestival();
		isFestivalInited = true;
	}

    return hr;
} /* CTTSEngObj::SetObjectToken */

//
//=== ISpTTSEngine Implementation ============================================
//

int CTTSEngObj::writeToSonicStream(short *pcm, int frames) {
  float percent = 0.1;
  const static int PENDING_PCM_FRAMES = 4096;

  if (mPendingFrames > 0) {
    // make a liner joining. fade out + fade in
    // Reference: splice.c of sox
    for (int i = 1; i < mPendingFrames && i <= frames; i++) {
      double p = i / mPendingFrames;
      mPendingPcm[i] = mPendingPcm[i] * (1 - p) + pcm[i] * p;
    }
  }

  if (mPendingFrames > frames) {
    frames = sonicWriteShortToStream(mSonicStream, mPendingPcm, mPendingFrames);
    mPendingFrames = 0;
  } else {
    int ori_frames = frames;
    frames = sonicWriteShortToStream(mSonicStream, mPendingPcm, mPendingFrames);
    int new_pending_frames = (ori_frames - mPendingFrames) * percent;
    if (new_pending_frames > PENDING_PCM_FRAMES)
      new_pending_frames = PENDING_PCM_FRAMES;

    frames += sonicWriteShortToStream(mSonicStream, pcm + mPendingFrames,
        ori_frames - mPendingFrames - new_pending_frames);
    mPendingFrames = new_pending_frames;
    memcpy(mPendingPcm, &pcm[ori_frames - mPendingFrames - 1], mPendingFrames * 2);
  }

  return frames;
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

		list<PhoneticSymbol*> phons = this->m_dict->lookup(char_list);
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
									pPcm = m_dict->getFullPause()->getPcm(size);
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
							  pPcm = m_dict->getFullPause()->getPcm(size);
							else if (pause >= 0.5)
							  pPcm = m_dict->getHalfPause()->getPcm(size);
							else
							  pPcm = m_dict->getQuaterPause()->getPcm(size);
							pause = 0;
							if (!has_unknown_char) {
  								li--;
								pos--;
							}
						} else if (!pPcm && li != phons.end() && (*li)) {
							string path = m_dict->mDataPath + "/" + m_dict->getVoice();
							pPcm = (*li)->getPcm(path.c_str(), m_dict->mVoiceFileType, size);
												        
							// speak Mandarin for Chinese
							if (!pPcm && mDict.getLanguage() == TIBETAN) {
						      path = m_dict->mDataPath + "/pinyin";
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
} /* CTTSEngObj::Speak */

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
    SPDBG_FUNC( "CTTSEngObj::GetVoiceFormat" );
    HRESULT hr = S_OK;

	if (this->m_dict->mSfinfo.samplerate == 16000) {
		hr = SpConvertStreamFormatEnum(SPSF_16kHz16BitMono, pDesiredFormatId, ppCoMemDesiredWaveFormatEx);
	} else if (this->m_dict->mSfinfo.samplerate == 44100) {
	    hr = SpConvertStreamFormatEnum(SPSF_44kHz16BitMono, pDesiredFormatId, ppCoMemDesiredWaveFormatEx);
	} else if (this->m_dict->mSfinfo.samplerate == 8000) {
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

  if (m_dict->mSfinfo.samplerate != current_samplerate) {
    wave.resample(m_dict->mSfinfo.samplerate);
  }

  EST_TVector<short> tvector;
  wave.channel(tvector, 0);
  size = tvector.p_num_columns * 2;
  char *pPcm = new char[size];
  short *shortPcm = (short*)pPcm;
  tvector.get_values(shortPcm, 1, 0, tvector.p_num_columns);
  return pPcm;
#else
  return NULL;
#endif
}

int CTTSEngObj::initFestival(void) {
#ifdef ENABLE_FESTIVAL
    int heap_size = 2100000; // scheme heap size
    int load_init_files = 0; // don't load default festival init files
    festival_initialize(load_init_files, heap_size);

    // set libdir of festival
    string path(m_dict->mDataPath);
    path += "/festival/lib";
    siod_set_lval("libdir", strintern(path.c_str()));

    path = m_dict->mDataPath;
    path += "/festival/lib/init.scm";
    festival_load_file(path.c_str());
#endif
  return 0;
}
