/*
 * baratinoo.h - Shim for Baratinoo (VoxyGen)
 * to be able to build the Baratinoo module without the Baratinoo SDK.
 *
 * Copyright (C) 2018 Hypra
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include <stdarg.h>

typedef enum {
    BARATINOO_ALL_PARSING,         /* Proprietary parsing and XML parsing both activated */
    BARATINOO_NO_PARSING,          /* No parsing at all */
    BARATINOO_PROPRIETARY_PARSING, /* Proprietary parsing only */
    BARATINOO_XML_PARSING          /* XML parsing only */
} BARATINOO_PARSING;

typedef enum {
    /* Enum changed in 8.4. */

    /* BARATINOO_UTF8 is defined dynamically below, the 2 values below are
     * just here to be excessively rigorous and make sure possible values are
     * included in the enumeration */
    BARATINOO_UTF8__V8_1 = 11,
    BARATINOO_UTF8__V8_4 = 13,
} BARATINOO_TEXT_ENCODING;

typedef enum {
    BARATINOO_MARKER_EVENT,
    BARATINOO_WAITMARKER_EVENT,
} BARATINOO_EVENT_TYPE;

typedef enum {
    BARATINOO_UNINITIALIZED,
    BARATINOO_INITIALIZED,
    BARATINOO_READY,
    BARATINOO_RUNNING,
    BARATINOO_EVENT,
    BARATINOO_INPUT_ERROR,
    BARATINOO_ENGINE_ERROR
} BARATINOOC_STATE;

typedef enum
{
    BARATINOO_INIT_OK,
    BARATINOO_INIT_ERROR
} BARATINOO_INIT_RETURN;

typedef enum {
    BARATINOO_TRACE_ERROR,
    BARATINOO_TRACE_INIT,
    BARATINOO_TRACE_WARNING,
    BARATINOO_TRACE_INFO,
    BARATINOO_TRACE_DEBUG
} BaratinooTraceLevel;

typedef void (*BaratinooTraceCB)(BaratinooTraceLevel level, int engineNumber, const char *source, const void *privatedata, const char *format, va_list args);
typedef int (*BaratinooOutputSignalCB)(void *privateData, const void *address, int length);

typedef void* BCengine;
typedef void* BCinputTextBuffer;
typedef void* BCoutputSignalBuffer;

typedef struct
{
    int major;
    int minor;
    /* we don't care about possible extras */
} BaratinooVersionStruct;

typedef struct {
    BARATINOO_EVENT_TYPE type;
    float         timeStamp;            /* millisecond unit */
    unsigned long byteStamp;            /* byte unit */
    unsigned long sampleStamp;          /* sample unit */
    union   {
        struct    {
            const char *name;
        } marker;
        struct    {
            const char *name;
            float duration;             /* millisecond unit */
            unsigned int samples;       /* sample unit */
        } waitMarker;
        struct    {			/* UTF-8 encoding */
            const char *tts;
            const char *input;
        } word;
        struct    {			/* UTF-8 encoding */
            const char *tts;
            const char *input;
        } punctuation;
	struct {			/* UTF-8 encoding */
	    const char *input;
	} separator;
        struct    {
            const char *symbol;
            float duration;             /* millisecond unit */
            unsigned int samples;       /* sample unit */
        } phoneme;
        /* viseme field is 8.4 and later, but doesn't affect union size */
        struct    {
            const char *name;
            const char *language;
        } newVoice;
        struct    {
            int type;
            int size;
            const char *datas;
        } raw;
    } data;
} BaratinooEvent;

typedef struct {
    const char *name;
    const char *language;
    const char *iso639;
    const char *iso3166;
    const char *variant;
    const char *accent;
    const char *gender;
    const char *version;
    const char *modules;
    int         age;
    int         expire_days;
} BaratinooVoiceInfo__V8_1;
typedef struct {
    const char *name;
    const char *language;
    const char *iso639;
    const char *iso3166;
    const char *variant;
    const char *accent;
    const char *gender;
    const char *version;
    const char *speech_modes;
    const char *modules;
    int         age;
    int         expire_days;
} BaratinooVoiceInfo__V8_4;
/* we use the biggest as default impl as we have to allocate it on the stack */
typedef BaratinooVoiceInfo__V8_4 BaratinooVoiceInfo;

/* lib and version */
extern BARATINOO_INIT_RETURN BCinitlib(BaratinooTraceCB traceCB);
extern void BCterminatelib(void);
extern const char *BCgetBaratinooVersion(void);
extern const BaratinooVersionStruct *BCgetBaratinooVersionStruct(void);
/* engine */
extern BCengine BCnew(const void *privatedata);
extern void BCinit(BCengine engine, const char *config);
extern void BCdelete(BCengine engine);
extern BARATINOOC_STATE BCgetState(BCengine engine);
extern BARATINOOC_STATE BCprocessLoop(BCengine engine, int count);
extern BARATINOOC_STATE BCpurge(BCengine engine);
extern void BCsetWantedEvent(BCengine engine, BARATINOO_EVENT_TYPE type);
extern BaratinooEvent BCgetEvent(BCengine engine);
extern int BCgetNumberOfVoices(BCengine engine);
extern BaratinooVoiceInfo BCgetVoiceInfo(BCengine engine,int voiceNumber);
/* I/O */
typedef enum
{
    BARATINOO_PCM = 0,
} BARATINOO_SIGNAL_CODING;

extern BCinputTextBuffer BCinputTextBufferNew(BARATINOO_PARSING parsing, BARATINOO_TEXT_ENCODING encoding, int voiceIndex, char *voiceModules);
extern void BCinputTextBufferDelete(BCinputTextBuffer inputTextBuffer);
extern int BCinputTextBufferInit(BCinputTextBuffer inputTextBuffer, const char *text);
extern BARATINOOC_STATE BCinputTextBufferSetInEngine(BCinputTextBuffer inputTextBuffer, BCengine engine);

extern void BCsetOutputSignal(BCengine engine, BaratinooOutputSignalCB cb, void *privateData, BARATINOO_SIGNAL_CODING coding, int frequency);
extern BCoutputSignalBuffer BCoutputSignalBufferNew(BARATINOO_SIGNAL_CODING coding, int frequency);
extern void BCoutputSignalBufferDelete(BCoutputSignalBuffer outputSignalBuffer);
extern void BCoutputTextBufferSetInEngine(BCoutputSignalBuffer outputSignalBuffer, BCengine engine);
extern int BCoutputSignalBufferIsError(BCoutputSignalBuffer outputSignalBuffer);
extern char * BCoutputSignalBufferGetSignalBuffer(BCoutputSignalBuffer outputSignalBuffer);
extern int BCoutputSignalBufferGetSignalLength(BCoutputSignalBuffer outputSignalBuffer);
extern void BCoutputSignalBufferResetSignal(BCoutputSignalBuffer outputSignalBuffer);

