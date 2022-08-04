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
    // cerr << "Fail to open dir " << dir << endl;
    return;
  }

  int wordCount = 0;

  do {
    if ((dp = readdir(dirp)) != NULL) {
      // cerr << "processing " << dp->d_name << endl;
      char *c = dp->d_name;

      if (*c == '.') {
        continue;
      }

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
        // cerr << "bad file format: " << dp->d_name << endl;
        continue;
      }

      charPinyin = wordPinyin.front();
      wordPinyin.pop_front(); // 不重复存储第一个字

      list<WordPinyin>::iterator it = Word::voiceFilesMap[charPinyin].begin();
      while (it != Word::voiceFilesMap[charPinyin].end()) {
        if (it->size() <= wordPinyin.size()) {
          Word::voiceFilesMap[charPinyin].insert(it, wordPinyin);
          break;
        }

        it++;
      }

      if (it == Word::voiceFilesMap[charPinyin].end()) {
        Word::voiceFilesMap[charPinyin].push_back(wordPinyin);
      }

      wordCount++;
    }
  } while (dp != NULL);

  closedir(dirp);
  // cerr << "map size: " << Word::voiceFilesMap.size() << endl;
  // cerr << "word count: " << wordCount << endl;
}

string Word::getNextPinyin(const list<Character>& charList, list<Character>::iterator& itor) {
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

string Word::findMatchedPinyin(WordPinyin& wordPinyin,
    const list<Character>& charList, list<Character>::iterator& itor) {
  string suffix;
  list<Character>::iterator itor2 = itor;
  int count = wordPinyin.size() - 1; // 最后一个是.mp3，不匹配
  int i = 0;

  for (WordPinyin::iterator pinyinItor = wordPinyin.begin();
      i < count; pinyinItor++) {
    string pinyin = Word::getNextPinyin(charList, itor2);
    // cerr << "compareing " << *pinyinItor << " and " << pinyin << endl;
    if (pinyin.empty() || pinyinItor->compare(pinyin) != 0) {
      return "";
    }

    i++;
    itor2++;
    suffix += pinyin;
  }

  // cerr << "findMatchedPinyin " << suffix << endl;
  itor = itor2;
  return suffix;
}

string Word::findPinyinVoiceFile(list<WordPinyin>& wordPinyinList,
    const list<Character>& charList, list<Character>::iterator& itor) {
  list<Character>::iterator itor2 = itor;

  list<WordPinyin>::iterator wordItor = wordPinyinList.begin();
  for (; wordItor != wordPinyinList.end(); wordItor++) {
    string suffix = Word::findMatchedPinyin(*wordItor, charList, itor2);
    if (!suffix.empty()) {
      itor2--;
      itor = itor2;
      return suffix + wordItor->back();
    }
  }

  return "";
}

string Word::findMatchedPinyin(WordPinyin& wordPinyin,
    const list<PhoneticSymbol*>& phonList,
    list<PhoneticSymbol*>::iterator& itor) {
  // cerr << "findMatchedPinyin " << (*itor)->symbol << endl;
  string suffix;
  list<PhoneticSymbol*>::iterator itor2 = itor;
  int count = wordPinyin.size() - 1; // 最后一个是.mp3，不匹配
  int i = 0;

  for (WordPinyin::iterator pinyinItor = wordPinyin.begin();
      i < count; pinyinItor++) {
    // cerr << "comparing " << *pinyinItor << " and " << (*itor2)->symbol << endl;
    if (itor2 == phonList.end() || *itor2 == NULL ||
        pinyinItor->compare((*itor2)->symbol) != 0) {
      return "";
    }

    suffix += (*itor2)->symbol;
    i++;
    itor2++;
  }

  // cerr << "findMatchedPinyin found " << suffix << endl;
  itor = itor2;
  return suffix;
}

string Word::findPinyinVoiceFile(list<WordPinyin>& wordPinyinList,
    const list<PhoneticSymbol*>& phonList,
    list<PhoneticSymbol*>::iterator& itor) {

  list<WordPinyin>::iterator wordItor = wordPinyinList.begin();
  for (; wordItor != wordPinyinList.end(); wordItor++) {
    list<PhoneticSymbol*>::iterator itor2 = itor;
    string suffix = Word::findMatchedPinyin(*wordItor, phonList, itor2);
    if (!suffix.empty()) {
      itor2--;
      itor = itor2;
      // cerr << "found " << suffix + wordItor->back() << endl;
      return suffix + wordItor->back();
    }
  }

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

/*
#ifdef ENABLE_FRISO
    filtered_text = Character::join(charlist2);
    friso_set_text(mFrisoTask, (char *)filtered_text.c_str());
    const char *friso_text = filtered_text.c_str();
    while (friso_next(mFriso, mFrisoConfig, mFrisoTask)) {
      string word(mFrisoTask->hits->word);
          first_word_phons =
              lookupFirstWord(friso_text + mFrisoTask->hits->offset);
#endif
}*/
  
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
              itor3++;
              filename = Word::findPinyinVoiceFile(Word::voiceFilesMap[symbol], char_list, itor3);
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
                Word::addChinese(wordlist, last_chinese_word);
                last_chinese_word.clear();
              }
            }

            if (!filename.empty()) {
              // 找到多个拼音的预录音文件
              if (Dict::me->mDebug) {
                cerr << "found voice file: " << symbol + filename << endl;
              }
              wordlist.push_back(Word(symbol + filename, RECORDING));
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
          Word::addChinese(wordlist, last_chinese_word);
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
          Word::addChinese(wordlist, last_chinese_word);
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
      // cerr << "found chinese character: " << itor->getUtf8() << ", " << mDictItemArray[itor->code].character.getUtf8() << endl;
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
    Word::addChinese(wordlist, last_chinese_word);
  }

  return wordlist;
}

void Word::addChinese(list<Word>& wordList, const string& text) {
  list<PhoneticSymbol*> phonList = Dict::me->lookup(text);
  list<PhoneticSymbol*> phonList2;

  list<PhoneticSymbol*>::iterator itor = phonList.begin();
  list<PhoneticSymbol*>::iterator itor2 = itor;
  for (; itor != phonList.end(); itor++) {
    string filename;
    if (Word::voiceFilesMap.find((*itor)->symbol) != Word::voiceFilesMap.end()) {
      // cerr << "findWordVoiceFile " << (*itor)->symbol << endl;
      itor2 = itor;
      itor2++;
      filename = Word::findPinyinVoiceFile(
          Word::voiceFilesMap[(*itor)->symbol],
          phonList, itor2);
    }

    if (!filename.empty()) {
      if (!phonList2.empty()) {
        wordList.push_back(Word(phonList2));
        phonList2.clear();
      }

      // cerr << "push " << (*itor)->symbol + filename << endl;
      wordList.push_back(Word((*itor)->symbol + filename, RECORDING));
      itor = itor2;
    } else {
      phonList2.push_back(*itor);
    }
  }

  if (!phonList2.empty()) {
    wordList.push_back(Word(phonList2));
  }
}
