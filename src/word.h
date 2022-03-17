/***************************************************************************
 * Copyright (C) 2008-2022 by Cameron Wong                                 *
 * name in passport: HUANG GUANNENG                                        *
 * email: hgneng at gmail.com                                              *
 * website: https://eguidedog.net                                          *
 *                                                                         *
 * This program is free software; you can redistribute it and/or           *
 * modify it under the terms of the GNU General Public License             *
 * as published by the Free Software Foundation; either version 2          *
 * of the License, or any later version.                                   *
 *                                                                         *
 * This program is distributed in the hope that it will be useful,         *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
 * GNU General Public License for more details.                            *
 *                                                                         *
 * You should have received a copy of the GNU General Public License       *
 * along with this program; if not, write to the Free Software             *
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,              *
 * MA  02110-1301, USA.                                                    *
 **************************************************************************/
#ifndef EKHO_WORD
#define EKHO_WORD

#include <map>
#include <list>
#include "ekho_typedef.h"
#include "character.h"
#include "zh_symbol_map.h"
#include "zhy_symbol_map.h"

using namespace std;
using namespace ekho;

typedef list<string> WordPinyin;

class Word {
public:
  TextType type;
  string text;
  list<PhoneticSymbol *> symbols;  // void for English
  list<OverlapType> overlapTypes;
  unsigned int offset;
  unsigned short bytes;

  Word(string txt, TextType t)
      : type(t), text(txt), symbols(0), overlapTypes(0), offset(0), bytes(0){};
  Word(string txt, TextType t, list<PhoneticSymbol *> sym)
      : type(t),
        text(txt),
        symbols(sym),
        overlapTypes(0),
        offset(0),
        bytes(0){};
  Word(string txt, TextType t, list<PhoneticSymbol *> sym,
       list<OverlapType> types)
      : type(t),
        text(txt),
        symbols(sym),
        overlapTypes(types),
        offset(0),
        bytes(0){};
  Word(string txt, TextType t, list<PhoneticSymbol *> sym, unsigned int off,
       unsigned short b)
      : type(t),
        text(txt),
        symbols(sym),
        overlapTypes(0),
        offset(off),
        bytes(b){};

  static map<string, list<WordPinyin> > voiceFilesMap;
  static void loadWordVoiceFiles(string dir);
};

#endif