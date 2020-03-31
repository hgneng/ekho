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

/**
 * event meaning:
 * 0: play
 * 1: end of this speech
 */
typedef int (SpeechdSynthCallback)(short *pcm, int frames, int bits, int channels, int samplerate, int event);

}

#endif

