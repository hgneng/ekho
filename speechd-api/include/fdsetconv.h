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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef FDSETCONV_H
#define FDSETCONV_H

#include <stdio.h>
#include <string.h>
#include <speechd_types.h>

char *EVoice2str(SPDVoiceType voice);

SPDVoiceType str2EVoice(const char *str);

char *EPunctMode2str(SPDPunctuation punct);

SPDPunctuation str2EPunctMode(const char *str);

char *ESpellMode2str(SPDSpelling spell);

SPDSpelling str2ESpellMode(const char *str);

char *ECapLetRecogn2str(SPDCapitalLetters recogn);

SPDCapitalLetters str2ECapLetRecogn(const char *str);

SPDPriority str2intpriority(const char *str);

#endif
