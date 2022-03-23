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

#include <map>
#include <list>
#include <dirent.h>
#include "word.h"
#include "ekho_dict.h"

using namespace std;

map<string, list<WordPinyin> > Word::voiceFilesMap;

void Word::loadWordVoiceFiles(string dir) {
  // scan files in voice dir
  DIR *dirp;
  struct dirent *dp;
  if ((dirp = opendir(dir.c_str())) == NULL) {
    cerr << "Fail to open dir " << dir << endl;
    return;
  }

  list<WordPinyin> wordPinyinList;
  int wordCount = 0;

  do {
    if ((dp = readdir(dirp)) != NULL) {
      cerr << "processing " << dp->d_name << endl;
      char *c = dp->d_name;
      string charPinyin;
      WordPinyin wordPinyin;
      while (*c > 0) {
        charPinyin += *c;
        if (*c >= '0' && *c <= '7') {
          wordPinyin.push_back(charPinyin);
          charPinyin.clear();
        }
        c++;
      }

      if (!charPinyin.empty()) {
        wordPinyin.push_back(charPinyin); // 最后一个其实不是拼音，而是文件后缀
      }

      if (wordPinyin.back() != ".wav" && wordPinyin.back() != ".mp3") {
        // 非法文件类型，跳过
        cerr << "bad file format: " << charPinyin << endl;
        continue;
      }

      charPinyin = wordPinyin.front();

      wordPinyinList = Word::voiceFilesMap[charPinyin];

      list<WordPinyin>::iterator it = wordPinyinList.begin();
      while (it != wordPinyinList.end()) {
        if (it->size() <= wordPinyin.size()) {
          wordPinyin.pop_front(); // 不重复存储第一个字
          wordPinyinList.insert(it, wordPinyin);
          break;
        }
      }

      if (it == wordPinyinList.end()) {
        wordPinyinList.push_back(wordPinyin);
      }
      
      wordCount++;
    }
  } while (dp != NULL);

  closedir(dirp);
  cerr << "map size: " << Word::voiceFilesMap.size() << endl;
  cerr << "word count: " << wordCount << endl;
}

string Word::getNextPinyin(list<Character>& charList, list<Character>::iterator& itor) {
  string pinyin;
  int length = 0;
  while (itor != charList.end() && itor->code != ' ' &&
      itor->code != ']' && itor->code < 128 && length < 10) {
    length++;
    pinyin += itor->getUtf8();
    itor++;
  }

  if (itor->code == ' ' || itor->code == ']') {
    return pinyin;
  } else {
    // 格式异常，将抛弃10个字符
    return "";
  }
}

string Word::findVoiceFile(list<WordPinyin>& wordPinyinList, list<Character>& charList, list<Character>::iterator& itor) {
  cerr << "findVoiceFile not implemented" << endl;
  return "";
}

list<Word> Word::split(string text) {
  list<Word> wordlist;
  TextType type;
  string lastword;  // ENGLISH_TEXT
  string last_chinese_word;
  list<Character> char_list = Dict::me->replaceNumbers(Character::split(text));
  list<Character>::iterator itor = char_list.begin();
  list<Character>::iterator itor2 = itor;
  string symbol;

  for (; itor != char_list.end(); itor++) {
    int code = itor->code;

    // check [[pin1 yin1]] style
    if (code == '[') {
      itor2 = itor;
      itor2++;
      if (itor2 != char_list.end() && itor2->code == '[') {
        do {
          PhoneticSymbol *phon_symbol = NULL;

          if (itor2 != char_list.end() && itor2->code == ']') {
            itor2++;
            if (itor2 != char_list.end() && itor2->code == ']') {
              itor2++;
            }

            // end of [[pinyin ]] style
            itor = itor2;
            if (itor == char_list.end()) {
              break;
            }
            else {
              code = itor->code;
            }
          }

          itor2++;
          symbol = Word::getNextPinyin(char_list, itor2);
          if (!symbol.empty()) {
            string filename;

            list<Character>::iterator itor3;
            if (Word::voiceFilesMap.find(symbol) != Word::voiceFilesMap.end()) {
              itor3 = itor2;
              filename = Word::findVoiceFile(Word::voiceFilesMap[symbol], char_list, itor3);
            }

            if (filename.empty()) {
              // 单个拼音
              phon_symbol = Dict::me->getPhoneticSymbol(symbol);
            }

            if (!filename.empty() || phon_symbol) {
              // submit pedning English and Chinese word
              if (!lastword.empty()) {
                wordlist.push_back(Word(lastword, ENGLISH_TEXT));
                lastword.clear();
              }

              if (!last_chinese_word.empty()) {
                wordlist.push_back(Word(last_chinese_word, NON_ENGLISH,
                    Dict::me->lookup(last_chinese_word)));
                last_chinese_word.clear();
              }
            }

            if (!filename.empty()) {
              // 找到多个拼音的预录音文件
              wordlist.push_back(Word(filename, RECORDING));
              itor2 = itor3;
            } else if (phon_symbol) {
              // 单个拼音
              // submit raw phonetic symbol
              list<PhoneticSymbol *> phon_list;
              phon_list.push_back(phon_symbol);
              wordlist.push_back(Word(symbol, PHONETIC, phon_list));
            }
          } else {
            // 格式异常，将抛弃10个字符
          }
        } while (!symbol.empty());

        if (itor == char_list.end()) {
          break;
        }
      }
    } // end of [[pin1 yin1]] style

    if (code < 65536 && Dict::me->mDictItemArray[code].character.phonSymbol &&
        strstr(Dict::me->mDictItemArray[code].character.phonSymbol->symbol, "pause") != NULL) {
      if (lastword.empty()) {
        // it's a symbol, not including space
        if (strcmp(Dict::me->mDictItemArray[code].character.phonSymbol->symbol,
                   "fullpause") == 0)
          type = FULL_PAUSE;
        else if (strcmp(Dict::me->mDictItemArray[code].character.phonSymbol->symbol,
                        "halfpause") == 0)
          type = HALF_PAUSE;
        else if (strcmp(Dict::me->mDictItemArray[code].character.phonSymbol->symbol,
                        "quaterpause") == 0)
          type = QUATER_PAUSE;
        else {
          cerr << "Warning: " << Dict::me->mDictItemArray[code].character.phonSymbol->symbol << endl;
        }

        // submit pending Chinese word
        if (!last_chinese_word.empty()) {
          wordlist.push_back(Word(last_chinese_word, NON_ENGLISH,
              Dict::me->lookup(last_chinese_word)));
          last_chinese_word.clear();
        }

        wordlist.push_back(Word(itor->getUtf8(), type));
      } else {
        lastword += itor->getUtf8();
      }
    } else if (itor->code < 65536 && (!Dict::me->mDictItemArray[itor->code].character.phonSymbol ||
          (itor->code >= 'A' && itor->code <='Z') || (itor->code >= 'a' && itor->code <= 'z') || (itor->code >= 128 && itor->code < 256))) {
      // it's not a Chinese character
      if ((itor->code >= 'A' && itor->code <= 'Z') ||
          (itor->code >= 'a' && itor->code <= 'z') ||
          (itor->code >= 128 && itor->code < 256) ||
          !lastword.empty()) {
        // it's alphabet
        lastword += itor->getUtf8();
        if (!last_chinese_word.empty()) {
          // 把中文文本拆分成“词”（有独立音频的拼音组合独立成词）
          wordlist.push_back(Word(last_chinese_word, NON_ENGLISH,
              Dict::me->lookup(last_chinese_word)));
          last_chinese_word.clear();
        }
      } else {
        // it's other symbols
        last_chinese_word += itor->getUtf8();
      }
    } else if (itor->code == 32 && !lastword.empty()) {
      // If it's space and there is pending English text, this space is treated as part of English text.
      // Otherwise, the space is treated as part of Chinese text.
      lastword += " ";
    } else {
      // it's a Chinese character
      //cout << "found chinese character: " << itor->getUtf8() << ", " << mDictItemArray[itor->code].character.getUtf8() << endl;
      last_chinese_word += itor->getUtf8();
      if (!lastword.empty()) {
        unsigned char c = lastword[0];
        if (lastword.length() == 1 &&
            ((c >= 'A' && c <= 'Z') ||
             (c >= 'a' && c <= 'z') ||
             (c >= 128 /* && c < 256 */))) {
          wordlist.push_back(Word(lastword, ENGLISH_TEXT,
              Dict::me->lookup(lastword)));
        } else {
          wordlist.push_back(Word(lastword, ENGLISH_TEXT));
        }
        lastword.clear();
      }
    }
  }

  if (!lastword.empty()) {
    wordlist.push_back(Word(lastword, ENGLISH_TEXT,
      Dict::me->lookup(lastword)));
  }

  if (!last_chinese_word.empty()) {
    wordlist.push_back(Word(last_chinese_word, NON_ENGLISH,
        Dict::me->lookup(last_chinese_word)));
  }

  return wordlist;
}