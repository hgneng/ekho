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

typedef int (SpeechdSynthCallback)(short *pcm, int frames, int bits, int channels, int samplerate);

}

#endif

