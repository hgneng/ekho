/***************************************************************************
 * Copyright (C) 2008-2024 by Cameron Wong                                 *
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

// 注意：这里不一定是一个词，它是一个合成单位。
// 例如所有连续的英文字符串会被划分为一个Word，交给eSpeak-ng去合成
// 如果启用了EmotiVoice，会把连续的中文字符串划分为一个Word，交给EmotiVoice去合成
// 只有在某些词被录制了对应的语音，才会被独立划分为一个名副其实的Word，用对应的录音去合成词的音频
class Word {
public:
  TextType type;
  string text;
  list<PhoneticSymbol *> symbols;  // void for English
  list<OverlapType> overlapTypes;
  unsigned int offset;
  unsigned short bytes;
  static bool emotiVoiceEnabled;
  static bool zhttsEnabled;

  Word(string txt, TextType t)
      : type(t), text(txt), symbols(0), overlapTypes(0), offset(0), bytes(0){};
  Word(string txt, TextType t, list<PhoneticSymbol*> sym)
      : type(t),
        text(txt),
        symbols(sym),
        overlapTypes(0),
        offset(0),
        bytes(0) {};
  Word(string txt, TextType t, list<PhoneticSymbol*> sym,
       list<OverlapType> types)
      : type(t),
        text(txt),
        symbols(sym),
        overlapTypes(types),
        offset(0),
        bytes(0) {};
  Word(string txt, TextType t, list<PhoneticSymbol*> sym, unsigned int off,
       unsigned short b)
      : type(t),
        text(txt),
        symbols(sym),
        overlapTypes(0),
        offset(off),
        bytes(b) {};
  Word(list<PhoneticSymbol*> sym)
      : type(NON_ENGLISH),
        symbols(sym),
        overlapTypes(OVERLAP_QUIET_PART),
        offset(0),
        bytes(0) {};

  static map<string, list<WordPinyin> > voiceFilesMap;
  static void loadWordVoiceFiles(string dir);
  static list<Word> split(string text);
  static void addChinese(list<Word>& wordList, const string& text);

  static string getNextPinyin(const list<Character>& charList,
    list<Character>::iterator& itor);

  static string findPinyinVoiceFile(list<WordPinyin>& wordPinyinList,
    const list<Character>& charList, list<Character>::iterator& itor);
  static string findPinyinVoiceFile(list<WordPinyin>& wordPinyinList,
    const list<PhoneticSymbol*>& phonList,
    list<PhoneticSymbol*>::iterator& itor);

  static string findMatchedPinyin(WordPinyin& wordPinyin,
    const list<Character>& charList, list<Character>::iterator& itor);
  static string findMatchedPinyin(WordPinyin& wordPinyin,
    const list<PhoneticSymbol*>& phonList,
    list<PhoneticSymbol*>::iterator& itor);
};

#endif