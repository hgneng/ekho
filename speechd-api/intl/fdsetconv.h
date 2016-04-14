
#include "fdset.h"

#ifndef FDSETCONV_H
#define FDSETCONV_H

#include <stdio.h>
#include <string.h>
#include "fdset.h"

char* EVoice2str(EVoiceType voice);

EVoiceType str2EVoice(char* str);

char* EPunctMode2str(EPunctMode punct);

EPunctMode str2EPunctMode(char* str);

char* ESpellMode2str(ESpellMode spell);

ESpellMode str2ESpellMode(char* str);

char* ECapLetRecogn2str(ECapLetRecogn recogn);

ECapLetRecogn ECapLetRecognstr2ECapLetRecogn(char* str);

EVoiceType str2intpriority(char* str);

ECapLetRecogn str2ECapLetRecogn(char* str);

#endif
