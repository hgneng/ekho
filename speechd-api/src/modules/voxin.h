/*
  Copyright 2016-2020, Gilles Casse <gcasse@oralux.org>

  This is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation; either version 2.1, or
  (at your option) any later version.

  This software is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  Lesser General Public License for more details.

*/

/**
   @file
   @brief Voxin API 
   
   This API extends the ECI API.
   
   Voxin offers the following features:
   
   - a choice of voices among IBM TTS and Vocalizer Embedded,    
   - complementary features added to the original proprietary text-to-speech,
   - compatibility of the legacy x86 IBM TTS binaries in an x86_64 environment.
   
   Links:
   - Libvoxin sources: https://github.com/Oralux/libvoxin 
   - Voxin: https://voxin.oralux.net
   
*/

#ifndef VOXIN_H
#define VOXIN_H

#include <stdlib.h>
#include <stdint.h>
#include "eci.h"

#define LIBVOXIN_VERSION_MAJOR 1
#define LIBVOXIN_VERSION_MINOR 5
#define LIBVOXIN_VERSION_PATCH 2

/**
   @brief Extends ECIParam (eci.h)
*/
typedef enum {
  VOX_SYNTH_MODE = 0, /**< eciSynthMode */
  VOX_INPUT_TYPE = 1, /**< eciInputType */
  VOX_TEXT_MODE = 2, /**< eciTextMode */
  VOX_DICTIONARY = 3, /**< eciDictionary */
  VOX_SAMPLE_RATE = 5, /**< eciSampleRate */
  VOX_WANT_PHONEME_INDICES = 7, /**< eciWantPhonemeIndices */
  VOX_REAL_WORLD_UNITS = 8, /**< eciRealWorldUnits */
  VOX_LANGUAGE_DIALECT = 9, /**< eciLanguageDialect */
  VOX_NUMBER_MODE = 10, /**< eciNumberMode */
  VOX_WANT_WORD_INDEX = 12, /**< eciWantWordIndex */
  VOX_CAPITALS = 17, /**< new param extending the eci interface */
} voxParam;

typedef enum {voxFemale, voxMale} voxGender;
typedef enum {voxAdult, voxChild, voxSenior} voxAge;
typedef enum {voxCapitalNone=0, voxCapitalSoundIcon=1, voxCapitalSpell=2, voxCapitalPitch=3} voxCapitalMode;

#define VOX_STR_MAX 128

#define VOX_OK 0
#define VOX_PARAM_OUT_OF_RANGE -1

/**
   @brief Describe a voice.

   Each string (name, lang,...) is appended by null characters up to
   the end of its array.
   
*/
typedef struct {
  uint32_t id; /**< voice identifier, e.g.: 0x2d0002 */
  char name[VOX_STR_MAX]; /**< optional: 'Yelda',... */
  char lang[VOX_STR_MAX]; /**< ietf sub tag, iso639-1, 2 letters code: 'en', 'tr',... */
  char variant[VOX_STR_MAX]; /**< ietf sub tag, optional: 'scotland', 'CA',... */
  uint32_t rate; /**< sample rate in Hertz: 11025, 22050 */
  uint32_t  size; /**< sample size e.g. 16 bits */
  /* channels = 1 */
  /* encoding = signed-integer PCM */
  char charset[VOX_STR_MAX]; /**< "UTF-8", "ISO-8859-1",... */
  voxGender gender;
  voxAge age;
  char multilang[VOX_STR_MAX]; /**< optional, e.g. "en,fr" */
  char quality[VOX_STR_MAX]; /**< optional, e.g. "embedded-compact" */
  uint32_t tts_id;
} vox_t;

/**
   @brief Supply the version of this API.
   
   @param[out] major  e.g. set to 1 if version is 1.2.3
   @param[out] minor  e.g. set to 2 if version is 1.2.3
   @param[out] patch  e.g. set to 3 if version is 1.2.3
   @return int  VOX_OK on success
*/
int voxGetVersion(int *major, int *minor, int *patch);

/**
   @brief Supply the list of available languages.
   
   This function depreciates eciGetAvailableLanguages() from the ECI
   API.
   
   Note : for backward compatibility, eciGetAvailableLanguages() is
   still available, and may now return identifiers extending those
   defined in ECILanguageDialect.
   
   @param[out] list  An array of nbVoices identifiers allocated by the
   caller. The array is updated according to the installed voices.
   @param[in,out] nbVoices  If list is not NULL the caller indicates
   the number of items of the list array; the function updates it with
   the effective number of voice identifiers copied into the list
   array.  If list is NULL, nbVoices returns the number of voices
   installed.
   @return int  VOX_OK on success
*/
int voxGetVoices(vox_t *list, unsigned int *nbVoices);

/**
   @brief Set param to the specified value.

   This function extends eciSetParam() to process this new parameter:   

   * VOX_CAPITALS: define the action to execute with capital
   letters. Similar to espeakCAPITALS in the eSpeak API.

   Expected value for VOX_CAPITALS: see enum voxCapitalMode.
   Value greater than voxCapitalPitch should be accepted and raise pitch.

   @param handle  instance created by eciNew() or eciNewEx()
   @param param
   @param value
   @return int  previous voxParam value on success, VOX_PARAM_OUT_OF_RANGE otherwise 
*/
int voxSetParam(void *handle, voxParam param, int value);

/**
   @brief convert vox_t to string

   @param[in] data  The vox_t data to be converted to string
   @param[out] string  Pointer allocated by the caller where the
   function has to copy the null terminated string
   @param[in,out] size  If size is not NULL the caller indicates the
   size allocated for string including the null terminator; the
   function updates size with the effective size of the string
   (including the null terminator).
   If string is NULL or size too small, size is updated with the
   expected value (including the null terminator).
   @return int  VOX_OK on success
*/
int voxToString(vox_t *data, char *string, size_t *size);

/**
   @brief Deprecated, kept for backward compatibility 
*/
int voxString(vox_t *v, char *s, size_t len);

#define VOX_ECI_VOICES 22
#define VOX_RESERVED_VOICES 30
#define VOX_MAX_NB_OF_LANGUAGES (VOX_ECI_VOICES + VOX_RESERVED_VOICES)
#define VOX_LAST_ECI_VOICE eciStandardFinnish

#endif

