#include <string.h>
#include <malloc.h>
#include "speak_lib.h"

// build command: g++ -g -I. sampleSpeak.cpp -lpulse -L. -lespeak -o sampleSpeak
int main(int argc, char* argv[]) {
    espeak_Initialize(AUDIO_OUTPUT_PLAYBACK,
      500, /* buffer length in ms */
      NULL, /* espeak-data path */
      0); 
    espeak_SetVoiceByName("en+f1");
    const char *text = "hello";
    espeak_Synth(text, strlen(text) + 1, 0, POS_CHARACTER, 0, espeakCHARS_UTF8, 0, 0);
    espeak_Synchronize();
    return 0;
}
