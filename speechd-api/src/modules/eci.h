/*
 * Copyright (c) 2005, 2006, IBM Corp. All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 *     * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.  *
 *     Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *     * Neither the name of IBM Corp. nor the names of its
 *     contributors may be used to endorse or promote products derived from
 *     this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 *
 * eci.h APIs for IBM Text To Speech
 *
 */

#ifndef __ECI_H
#define __ECI_H


#ifndef MOTIF
  typedef int Boolean;
#endif

#define ECITrue 1
#define ECIFalse 0

#ifndef ECIFNDECLARE
        typedef signed long ECIint32;
#ifdef _MSC_VER
        #ifdef _WIN32_WCE
                typedef char ECIsystemChar;
                #define ECIFNDECLARE __stdcall
        #elif defined _WIN32
                #include <tchar.h>
                typedef _TCHAR ECIsystemChar;
                #define ECIFNDECLARE __stdcall
        #endif

#elif defined __TURBOC__
        #ifdef __WIN32__
                #include <tchar.h>
                #define ECIFNDECLARE __stdcall
                typedef _TCHAR ECIsystemChar;
        #endif
#else
        #define ECIFNDECLARE
        typedef char ECIsystemChar;
#endif
#endif

#ifndef NULL_ECI_HAND
 #define NULL_ECI_HAND  0
#endif

#define ECI_PRESET_VOICES  8
#define ECI_USER_DEFINED_VOICES  8

#define ECI_VOICE_NAME_LENGTH  30

#define ECI_NOERROR				0x00000000
#define ECI_SYSTEMERROR			0x00000001
#define ECI_MEMORYERROR			0x00000002
#define ECI_MODULELOADERROR		0x00000004
#define ECI_DELTAERROR			0x00000008
#define ECI_SYNTHERROR			0x00000010
#define ECI_DEVICEERROR			0x00000020
#define ECI_DICTERROR			0x00000040
#define ECI_PARAMETERERROR		0x00000080
#define ECI_SYNTHESIZINGERROR	0x00000100
#define ECI_DEVICEBUSY			0x00000200
#define ECI_SYNTHESISPAUSED		0x00000400
#define ECI_REENTRANTCALL		0x00000800
#define ECI_ROMANIZERERROR		0x00001000
#define ECI_SYNTHESIZING		0x00002000

#define eciPhonemeLength (4)

#ifdef __cplusplus
extern "C" {
#endif

typedef void* ECIHand;

typedef const void* ECIInputText;

enum ECIParam {
	eciSynthMode,
	eciInputType,
	eciTextMode,
	eciDictionary,
	eciSampleRate = 5,
    eciWantPhonemeIndices = 7,
	eciRealWorldUnits,
	eciLanguageDialect,
    eciNumberMode,
    eciWantWordIndex = 12,
    eciNumDeviceBlocks,
    eciSizeDeviceBlocks,
    eciNumPrerollDeviceBlocks,
    eciSizePrerollDeviceBlocks,
	eciNumParams
};

enum ECIVoiceParam {
	eciGender,
	eciHeadSize,
	eciPitchBaseline,
	eciPitchFluctuation,
	eciRoughness,
	eciBreathiness,
	eciSpeed,
	eciVolume,
	eciNumVoiceParams
};

enum ECIDictError {
  DictNoError,
  DictFileNotFound,
  DictOutOfMemory,
  DictInternalError,
  DictNoEntry,
  DictErrLookUpKey,
  DictAccessError,
  DictInvalidVolume
};

enum ECIVoiceError {
	VoiceNoError,
	VoiceSystemError,
	VoiceNotRegisteredError,
	VoiceInvalidFileFormatError
};

typedef void* ECIDictHand;
#define NULL_DICT_HAND 0
enum ECIDictVolume {
	eciMainDict = 0,
	eciRootDict = 1,
    eciAbbvDict = 2,
	eciMainDictExt = 3
};

enum ECILanguageDialect {
    NODEFINEDCODESET                = 0x00000000,
    eciGeneralAmericanEnglish       = 0x00010000,
    eciBritishEnglish               = 0x00010001,
    eciCastilianSpanish             = 0x00020000,
    eciMexicanSpanish               = 0x00020001,
    eciStandardFrench               = 0x00030000,
    eciCanadianFrench               = 0x00030001,
    eciStandardGerman               = 0x00040000,
    eciStandardItalian              = 0x00050000,
    eciMandarinChinese              = 0x00060000,
    eciMandarinChineseGB            = eciMandarinChinese,
    eciMandarinChinesePinYin        = 0x00060100,
    eciMandarinChineseUCS           = 0x00060800,
    eciTaiwaneseMandarin            = 0x00060001,
    eciTaiwaneseMandarinBig5        = eciTaiwaneseMandarin,
    eciTaiwaneseMandarinZhuYin      = 0x00060101,
    eciTaiwaneseMandarinPinYin      = 0x00060201,
    eciTaiwaneseMandarinUCS         = 0x00060801,
    eciBrazilianPortuguese          = 0x00070000,
    eciStandardJapanese             = 0x00080000,
    eciStandardJapaneseSJIS         = eciStandardJapanese,
    eciStandardJapaneseUCS          = 0x00080800,
    eciStandardFinnish              = 0x00090000,
    eciStandardKorean               = 0x000A0000,
    eciStandardKoreanUHC            = eciStandardKorean,
    eciStandardKoreanUCS            = 0x000A0800,
    eciStandardCantonese            = 0x000B0000,
    eciStandardCantoneseGB          = eciStandardCantonese,
    eciStandardCantoneseUCS         = 0x000B0800,
    eciHongKongCantonese            = 0x000B0001,
    eciHongKongCantoneseBig5        = eciHongKongCantonese,
    eciHongKongCantoneseUCS         = 0x000B0801,
    eciStandardDutch                = 0x000C0000,
    eciStandardNorwegian            = 0x000D0000,
    eciStandardSwedish              = 0x000E0000,
    eciStandardDanish               = 0x000F0000,
    eciStandardReserved             = 0x00100000,
    eciStandardThai                 = 0x00110000,
    eciStandardThaiTIS              = eciStandardThai

};

enum ECIPartOfSpeech {
    eciUndefinedPOS = 0,
    eciFutsuuMeishi = 1,
    eciKoyuuMeishi,
    eciSahenMeishi,
    eciMingCi
};

#if defined(WIN32)
#pragma pack(push, 1)
#elif defined(UNDER_CE) && (defined(MIPS) || defined(SH3))
#pragma pack(push, 4)
#endif
typedef struct {
   union {
	   unsigned char  sz[eciPhonemeLength+1];
      unsigned short wsz[eciPhonemeLength+1];
   } phoneme;
   enum ECILanguageDialect eciLanguageDialect;
	unsigned char mouthHeight;
	unsigned char mouthWidth;
	unsigned char mouthUpturn;
	unsigned char jawOpen;
	unsigned char teethUpperVisible;
	unsigned char teethLowerVisible;
	unsigned char tonguePosn;
	unsigned char lipTension;
} ECIMouthData;

typedef struct ECIVoiceAttrib {
   int eciSampleRate;   
   enum ECILanguageDialect languageID;
} ECIVoiceAttrib;

#if defined(WIN32) || ( defined(UNDER_CE) && (defined(MIPS) || defined(SH3)) )
#pragma pack(pop)
#endif

enum ECIMessage {
	eciWaveformBuffer, eciPhonemeBuffer, eciIndexReply, eciPhonemeIndexReply, eciWordIndexReply, eciStringIndexReply,
	eciAudioIndexReply, eciSynthesisBreak
};

enum ECICallbackReturn {
	eciDataNotProcessed, eciDataProcessed, eciDataAbort
};

typedef enum ECICallbackReturn (*ECICallback)(ECIHand hEngine, enum ECIMessage Msg, long lParam, void *pData);

#if defined(_WIN32) || defined(_Windows)
enum ECIDialogBox {
	eciGeneralDB,
	eciAboutDB,
	eciVoicesDB,
	eciReadingDB,
	eciMainDictionaryDB,
	eciRootDictionaryDB,
	eciNumDialogBoxes
};
#endif


#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
extern "C" {
#endif

ECIHand ECIFNDECLARE eciNew(void);
ECIHand ECIFNDECLARE eciNewEx(enum ECILanguageDialect Value);
int ECIFNDECLARE eciGetAvailableLanguages(enum ECILanguageDialect *aLanguages, int *nLanguages);
ECIHand ECIFNDECLARE eciDelete(ECIHand hEngine);
Boolean ECIFNDECLARE eciReset(ECIHand hEngine);
Boolean ECIFNDECLARE eciIsBeingReentered(ECIHand hEngine);
void ECIFNDECLARE eciVersion(char *pBuffer);
int ECIFNDECLARE eciProgStatus(ECIHand hEngine);
void ECIFNDECLARE eciErrorMessage(ECIHand hEngine, void* buffer);
void ECIFNDECLARE eciClearErrors(ECIHand hEngine);
Boolean ECIFNDECLARE eciTestPhrase(ECIHand hEngine);
Boolean ECIFNDECLARE eciSpeakText(ECIInputText pText, Boolean bAnnotationsInTextPhrase);
Boolean ECIFNDECLARE eciSpeakTextEx(ECIInputText pText, Boolean bAnnotationsInTextPhrase, enum ECILanguageDialect Value);
int ECIFNDECLARE eciGetParam(ECIHand hEngine, enum ECIParam Param);
int ECIFNDECLARE eciSetParam(ECIHand hEngine, enum ECIParam Param, int iValue);
int ECIFNDECLARE eciGetDefaultParam(enum ECIParam parameter);
int ECIFNDECLARE eciSetDefaultParam(enum ECIParam parameter, int value);
Boolean ECIFNDECLARE eciCopyVoice(ECIHand hEngine, int iVoiceFrom, int iVoiceTo);
Boolean ECIFNDECLARE eciGetVoiceName(ECIHand hEngine, int iVoice, void *pBuffer);
Boolean ECIFNDECLARE eciSetVoiceName(ECIHand hEngine, int iVoice, const void *pBuffer);
int ECIFNDECLARE eciGetVoiceParam(ECIHand hEngine, int iVoice, enum ECIVoiceParam Param);
int ECIFNDECLARE eciSetVoiceParam(ECIHand hEngine, int iVoice,
                                  enum ECIVoiceParam Param, int iValue);
Boolean ECIFNDECLARE eciAddText(ECIHand hEngine, ECIInputText pText);
Boolean ECIFNDECLARE eciInsertIndex(ECIHand hEngine, int iIndex);
Boolean ECIFNDECLARE eciSynthesize(ECIHand hEngine);
Boolean ECIFNDECLARE eciSynthesizeFile(ECIHand hEngine, const void *pFilename);
Boolean ECIFNDECLARE eciClearInput(ECIHand hEngine);
Boolean ECIFNDECLARE eciGeneratePhonemes(ECIHand hEngine, int iSize, void *pBuffer);
int ECIFNDECLARE eciGetIndex(ECIHand hEngine);
Boolean ECIFNDECLARE eciStop(ECIHand hEngine);
Boolean ECIFNDECLARE eciSpeaking(ECIHand hEngine);
Boolean ECIFNDECLARE eciSynchronize(ECIHand hEngine);
Boolean ECIFNDECLARE eciSetOutputBuffer(ECIHand hEngine, int iSize, short *psBuffer);
Boolean ECIFNDECLARE eciSetOutputFilename(ECIHand hEngine, const void *pFilename);
Boolean ECIFNDECLARE eciSetOutputDevice(ECIHand hEngine, int iDevNum);
Boolean ECIFNDECLARE eciPause(ECIHand hEngine, Boolean On);
void ECIFNDECLARE eciRegisterCallback(ECIHand hEngine, ECICallback Callback, void *pData);
ECIDictHand ECIFNDECLARE eciNewDict(ECIHand hEngine);
ECIDictHand ECIFNDECLARE eciGetDict(ECIHand hEngine);
enum ECIDictError ECIFNDECLARE eciSetDict(ECIHand hEngine, ECIDictHand hDict);
ECIDictHand ECIFNDECLARE eciDeleteDict(ECIHand hEngine, ECIDictHand hDict);
enum ECIDictError ECIFNDECLARE eciLoadDict(ECIHand hEngine, ECIDictHand hDict, enum ECIDictVolume DictVol, ECIInputText pFilename);
enum ECIDictError ECIFNDECLARE eciSaveDict(ECIHand hEngine, ECIDictHand hDict, enum ECIDictVolume DictVol, ECIInputText pFilename);
enum ECIDictError ECIFNDECLARE eciUpdateDict(ECIHand hEngine, ECIDictHand hDict,
                                        enum ECIDictVolume DictVol, ECIInputText pKey, ECIInputText pTranslationValue);
enum ECIDictError ECIFNDECLARE eciDictFindFirst(ECIHand hEngine,
                                           ECIDictHand hDict, enum ECIDictVolume DictVol,
                                           ECIInputText *ppKey, ECIInputText *ppTranslationValue);
enum ECIDictError ECIFNDECLARE eciDictFindNext(ECIHand hEngine,
                                          ECIDictHand hDict, enum ECIDictVolume DictVol,
                                          ECIInputText *ppKey, ECIInputText *ppTranslationValue);
const char *ECIFNDECLARE eciDictLookup(ECIHand hEngine,
                                       ECIDictHand hDict, enum ECIDictVolume DictVol,
                                       ECIInputText pKey);
enum ECIDictError ECIFNDECLARE eciUpdateDictA(ECIHand hEngine,
                                         ECIDictHand hDict, enum ECIDictVolume DictVol,
                                         ECIInputText pKey, ECIInputText pTranslationValue, enum ECIPartOfSpeech PartOfSpeech);
enum ECIDictError ECIFNDECLARE eciDictFindFirstA(ECIHand hEngine,
                                            ECIDictHand hDict, enum ECIDictVolume DictVol,
                                            ECIInputText *ppKey, ECIInputText *ppTranslationValue, enum ECIPartOfSpeech *pPartOfSpeech);
enum ECIDictError ECIFNDECLARE eciDictFindNextA(ECIHand hEngine,
                                           ECIDictHand hDict, enum ECIDictVolume DictVol,
                                           ECIInputText *ppKey, ECIInputText *ppTranslationValue, enum ECIPartOfSpeech *pPartOfSpeech);
enum ECIDictError ECIFNDECLARE eciDictLookupA(ECIHand hEngine,
                                         ECIDictHand hDict, enum ECIDictVolume DictVol,
                                         ECIInputText pKey, ECIInputText *ppTranslationValue, enum ECIPartOfSpeech *pPartOfSpeech);


#define eciDictFindFirst(eciHandle, dictHandle, dictVolume, ppkey, pptranslation) \
        eciDictFindFirst((eciHandle), (dictHandle), (dictVolume), (ECIInputText *) (ppkey), (ECIInputText *) (pptranslation))
#define eciDictFindNext(eciHandle, dictHandle, dictVolume, ppkey, pptranslation) \
        eciDictFindNext((eciHandle), (dictHandle), (dictVolume), (ECIInputText *) (ppkey), (ECIInputText *) (pptranslation))
        
enum ECIVoiceError ECIFNDECLARE eciRegisterVoice(ECIHand eciHand, int voiceNumber, void *vData, ECIVoiceAttrib *vAttrib);
enum ECIVoiceError ECIFNDECLARE eciUnregisterVoice(ECIHand eciHand, int voiceNumber, ECIVoiceAttrib *vAttrib, void **vData);

#ifndef __ECIFILTER_H
#define __ECIFILTER_H

typedef void *ECIFilterHand;

#define NULL_FILTER_HAND 0

/*typedef*/ enum ECIFilterError {
  FilterNoError,					
  FilterFileNotFound,        	
  FilterOutOfMemory,          
  FilterInternalError,			
  FilterAccessError				
}; 

enum ECIFilterError ECIFNDECLARE eciDeactivateFilter(ECIHand eciHandle, ECIFilterHand pFilter);
#ifdef __cplusplus
ECIFilterHand  ECIFNDECLARE eciNewFilter(ECIHand eciHandle, unsigned int filterNum = 0, Boolean bGlobal = false);
#else
ECIFilterHand  ECIFNDECLARE eciNewFilter(ECIHand eciHandle, unsigned int filterNum, Boolean bGlobal);
#endif
enum ECIFilterError ECIFNDECLARE eciActivateFilter(ECIHand eciHandle, ECIFilterHand whichFilterHand);
ECIFilterHand  ECIFNDECLARE eciDeleteFilter(ECIHand eciHandle, ECIFilterHand whichFilterHand);
enum ECIFilterError ECIFNDECLARE eciUpdateFilter(ECIHand eciHandle, ECIFilterHand whichFilterHand,
																 ECIInputText key, ECIInputText translation);
enum ECIFilterError ECIFNDECLARE eciGetFilteredText(ECIHand eciHandle, ECIFilterHand whichFilterHand,
																	 ECIInputText input, ECIInputText *filteredText);
#endif 

#ifdef __cplusplus
}
#endif

#endif
