#ifndef EKHO_TYPEDEF_H
#define EKHO_TYPEDEF_H

namespace ekho {

enum Command {
    SPEAK,
    SAVEMP3,
    SAVEOGG,
    GETPHONSYMBOLS
};

enum EkhoPuncType {
    EKHO_PUNC_NONE = 1,
    EKHO_PUNC_SOME,
    EKHO_PUNC_ALL
};

enum EkhoCapLetterRecognType {
	EKHO_CAP_NONE = 0,
	EKHO_CAP_SPELL = 1,
	EKHO_CAP_ICON = 2
};

typedef enum {
  ENGLISH_TEXT = 1,
  NON_ENGLISH = 2,
  FULL_PAUSE = 3,
  HALF_PAUSE = 4,
  QUATER_PAUSE = 5,
  PHONETIC = 6,
  RECORDING = 7,
  EMOTIVOICE = 8,
  ZHTTS = 9,
} TextType;

typedef enum {
  OVERLAP_DEFAULT = 0,
  OVERLAP_NONE = 1,
  OVERLAP_QUIET_PART = 2,  // should be default
  OVERLAP_HALF_PART = 3,
} OverlapType;

/**
 * event meaning:
 * 0: play
 * 1: end of this speech
 */
typedef int (SpeechdSynthCallback)(short *pcm, int frames, int bits, int channels, int samplerate, int event);

}

#endif

