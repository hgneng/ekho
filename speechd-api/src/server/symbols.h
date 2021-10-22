/*
 * symbols.h -- Implements functions handling symbols conversion,
 *              including punctuation, for Speech Dispatcher (header)
 *
 * Copyright (C) 2001,2002,2003,2017 Brailcom, o.p.s
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

#include "speechd.h"

#ifndef SYMBOLS_H
#define SYMBOLS_H

/* Load symbols from this file */
void symbols_preprocessing_add_file(const char *name);

/* Converts symbols to words corresponding to a level into a message. */
void insert_symbols(TSpeechDMessage *msg, int punct_missing);

/* Speech symbols punctuation levels */
typedef enum {
	SYMLVL_INVALID = -1,
	SYMLVL_NO = 0,
	SYMLVL_NONE = 100,
	SYMLVL_SOME = 200,
	SYMLVL_MOST = 300,
	SYMLVL_ALL = 500,
	SYMLVL_CHAR = 1000
} SymLvl;

/* Convert a string to a symbol level */
extern SymLvl str2SymLvl(const char *str);

#endif /* SYMBOLS_H */
