
/*
 * fdsetconv.c - Conversion of types for Speech Dispatcher
 *
 * Copyright (C) 2001, 2002, 2003 Brailcom, o.p.s.
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
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $Id: fdsetconv.c,v 1.5 2007-06-21 20:09:45 hanke Exp $
 */

#include "fdsetconv.h"

char*
EVoice2str(EVoiceType voice)
{
    char *str;

    switch (voice)
        {
        case MALE1: str = strdup("male1"); break;
        case MALE2: str = strdup("male2"); break;
        case MALE3: str = strdup("male3"); break;
        case FEMALE1: str = strdup("female1"); break;
        case FEMALE2: str = strdup("female2"); break;
        case FEMALE3: str = strdup("female3"); break;
        case CHILD_MALE: str = strdup("child_male"); break;
        case CHILD_FEMALE: str = strdup("child_female"); break;
        default: str = NULL;
        }

    return str;
}

EVoiceType
str2EVoice(char* str)
{
    EVoiceType voice;

    if (!strcmp(str, "male1")) voice = MALE1;
    else if (!strcmp(str, "male2")) voice = MALE2;
    else if (!strcmp(str, "male3")) voice = MALE3;
    else if (!strcmp(str, "female1")) voice = FEMALE1;
    else if (!strcmp(str, "female2")) voice = FEMALE2;
    else if (!strcmp(str, "female3")) voice = FEMALE3;
    else if (!strcmp(str, "child_male")) voice = CHILD_MALE;
    else if (!strcmp(str, "child_female")) voice = CHILD_FEMALE;
    else voice = NO_VOICE;

    return voice;
}

char*
EPunctMode2str(EPunctMode punct)
{
    char *str;

    switch (punct)
        {
        case PUNCT_NONE: str = strdup("none"); break;
        case PUNCT_ALL: str = strdup("all"); break;
        case PUNCT_SOME: str = strdup("some"); break;
        default: str = NULL;
        }

    return str;
}

EPunctMode
str2EPunctMode(char* str)
{
    EPunctMode punct;

    if (!strcmp(str, "none")) punct = PUNCT_NONE;
    else if (!strcmp(str, "all")) punct = PUNCT_ALL;
    else if (!strcmp(str, "some")) punct = PUNCT_SOME;
    else punct = -1;

    return punct;
}

char*
ESpellMode2str(ESpellMode spell)
{
    char *str;

    switch (spell)
        {
        case SPELLING_ON: str = strdup("on"); break;
        case SPELLING_OFF: str = strdup("off"); break;
        default: str = NULL;
        }

    return str;
}

ESpellMode
str2ESpellMode(char* str)
{
    ESpellMode spell;

    if (!strcmp(str, "on")) spell = SPELLING_ON;
    else if (!strcmp(str, "off")) spell = SPELLING_OFF;
    else spell = -1;

    return spell;
}

char*
ECapLetRecogn2str(ECapLetRecogn recogn)
{
    char *str;

    switch (recogn)
        {
        case RECOGN_NONE: str = strdup("none"); break;
        case RECOGN_SPELL: str = strdup("spell"); break;
        case RECOGN_ICON: str = strdup("icon"); break;
        default: str = NULL;
        }

    return str;
}

ECapLetRecogn
str2ECapLetRecogn(char* str)
{
    ECapLetRecogn recogn;

    if (!strcmp(str, "none")) recogn = RECOGN_NONE;
    else if (!strcmp(str, "spell")) recogn = RECOGN_SPELL;
    else if (!strcmp(str, "icon")) recogn = RECOGN_ICON;
    else recogn = -1;

    return recogn;
}


EVoiceType
str2intpriority(char* str)
{
    int priority;

    if (!strcmp(str, "important"))  priority = 1;
    else if (!strcmp(str, "text")) priority = 2;
    else if (!strcmp(str, "message")) priority = 3;
    else if (!strcmp(str, "notification")) priority = 4;
    else if (!strcmp(str, "progress")) priority = 5;
    else priority = -1;

    return priority;
}
