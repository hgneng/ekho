/***************************************************************************
 * Copyright (C) 2008-2019 by Cameron Wong                                 *
 * name in passport: HUANG GUANNENG                                        *
 * email: hgneng at gmail.com                                              *
 * website: http://www.eguidedog.net                                       *
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
#include "ekho_dict.h"
#include <dirent.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <fstream>
#include <iostream>
#include "character.h"
#include "phonetic_symbol.h"
#include "stdafx.h"
#include "zh_symbol_map.h"
#include "zhy_symbol_map.h"

#ifdef _WIN32_WINNT
#include <io.h>
#include <process.h>
#define F_OK 0
#include <direct.h>
#define GetCurrentDir _getcwd
#else
#include <unistd.h>
#define GetCurrentDir getcwd
#endif
using namespace std;

//#define DEBUG_PERF
#ifdef DEBUG_PERF
#include <time.h>
#endif

char g_friso_lex_dir[256];

namespace ekho {
static bool isDir(const char *path) {
  struct stat st;
  if (path && stat(path, &st) == 0 && (st.st_mode & S_IFDIR)) {
    return true;
  } else {
    return false;
  }
}

Dict *Dict::me = 0;
bool Dict::mDebug = false;

Dict::Dict(void) { init(); }

Dict::Dict(Language lang) {
  init();
  setLanguage(lang);
}

void Dict::init(void) {
  me = this;
  mLanguage = ENGLISH;

  mVoiceFile = 0;

  mFullPausePcmSize = 25000;
  memset(mDictItemArray, 0, sizeof(mDictItemArray));

  mFullPausePcm = new char[mFullPausePcmSize];
  memset(mFullPausePcm, 0, mFullPausePcmSize);
  mFullPause = new PhoneticSymbol("fullpause");
  mFullPause->setPcm(mFullPausePcm, mFullPausePcmSize);

  mHalfPausePcm = new char[mFullPausePcmSize / 2];
  memset(mHalfPausePcm, 0, mFullPausePcmSize / 2);
  mHalfPause = new PhoneticSymbol("halfpause");
  mHalfPause->setPcm(mHalfPausePcm, mFullPausePcmSize / 2);

  mQuaterPausePcm = new char[mFullPausePcmSize / 4];
  memset(mQuaterPausePcm, 0, mFullPausePcmSize / 4);
  mQuaterPause = new PhoneticSymbol("quaterpause");
  mQuaterPause->setPcm(mQuaterPausePcm, mFullPausePcmSize / 4);

  memset(mKaSymbolLetter, 0, sizeof(mKaSymbolLetter));
  mKaSymbolIndex = 0;

  mDataPath = getDefaultDataPath();

#ifdef ENABLE_FRISO
  string friso_dict_path = mDataPath + "/friso-dict/";
  realpath(friso_dict_path.c_str(), g_friso_lex_dir);

  // append / to end
  int len = strlen(g_friso_lex_dir);
  g_friso_lex_dir[len] = '/';
  g_friso_lex_dir[len + 1] = 0;

  friso_dict_path += "friso.ini";
  mFriso = friso_new();
  mFrisoConfig = friso_new_config();
  if (friso_init_from_ifile(mFriso, mFrisoConfig,
                            (char *)friso_dict_path.c_str()) != 1)
    cerr << "fail to initialize friso and config." << endl;

  mFrisoTask = friso_new_task();
#endif
}

Dict::~Dict(void) {
  delete mFullPause;
  delete mHalfPause;
  delete mQuaterPause;
  if (mVoiceFile) fclose(mVoiceFile);
  mVoiceFile = 0;
  Dict::me = 0;
  // TODO: detete mKaSymbolLetter

  for (list<PhoneticSymbol *>::iterator li = mSpecialSymbols.begin();
       li != mSpecialSymbols.end(); li++) {
    delete *li;
  }
  mSpecialSymbols.clear();

  for (list<char *>::iterator li = mSpecialChars.begin();
       li != mSpecialChars.end(); li++) {
    delete[] *li;
  }
  mSpecialChars.clear();
}

std::string GetCurrentWorkingDir( void ) {
  char buff[FILENAME_MAX];
  GetCurrentDir( buff, FILENAME_MAX );
  std::string current_working_dir(buff);
  return current_working_dir;
}

/**
 * Detect path to ekho-data
 * 1. check PATH_EKHO_DATA
 * 2. check ./ekho-data
 * 3. check ../ekho-data
 * 4. set to EKHO_DATA_PATH (like /usr/local/share/ekho-data)
 * 5. set to EKHO_DATA_PATH (like /usr/local/share/ekho-data)
 */
string Dict::getDefaultDataPath(void) {
  const char *path = getenv("EKHO_DATA_PATH");
  if (!path) {
#ifdef EKHO_DATA_PATH
    path = EKHO_DATA_PATH;
#endif
    if (isDir(path)) {
      // ok
    } else if (isDir("ekho-data")) {
      path = "ekho-data";
    } else if (isDir("../ekho-data")) {
      path = "../ekho-data";
    } else if (isDir("/usr/local/share/ekho-data")) {
      path = "/usr/local/share/ekho-data";
    } else if (isDir("/usr/share/ekho-data")) {
      path = "/usr/share/ekho-data";
    } else {
      path = "";
    }
  }

  if (mDebug) {
    cerr << "current dir: " << GetCurrentWorkingDir() << endl;
    cerr << "EKHO_DATA_PATH: " << path << endl;
  }

  return path;
}

/**
 * Helper function for loading Mandarin in setLanguage
 */
int Dict::loadMandarin(void) {
  struct stat statBuf;
  string zhDict(mDataPath);
  zhDict += "/zh.dict";
  if (stat(zhDict.c_str(), &statBuf) == 0) {
    this->loadEkhoDict(zhDict);
  } else {
    string zhList(mDataPath);
    zhList += "/zh_list";
    if (this->loadEspeakDict(zhList) != 0) {
      cerr << "Fail to load espeak dictionary: " << zhList << endl;
      return -1;
    }

    // load zh_listx
    zhList = mDataPath;
    zhList += "/zh_listx";
    if (this->loadEspeakDict(zhList) != 0) {
      cerr << "Fail to load espeak dictionary: " << zhList << endl;
      return -1;
    }

    // load zh_patch
    zhList = mDataPath + "/zh_patch";
    if (this->loadEspeakDict(zhList) != 0) {
      cerr << "Fail to load zh_patch: " << zhList << endl;
    }

    // save to zh.dict
    this->saveEkhoDict(zhDict);
  }

  return 0;
}

/**
 * Set language
 * @Return:
 *  0   - success
 *  -1  - fail to load dictionary
 *  -2  - fail to load voice
 */
int Dict::setLanguage(Language lang) {
  if (lang == mLanguage) return 0;

  struct stat statBuf;

  // clear old dictionary
  for (int i = 0; i < 65536; ++i) {
    mDictItemArray[i].character.phonSymbol = 0;
    if (mDictItemArray[i].wordList) {
      delete mDictItemArray[i].wordList;
      mDictItemArray[i].wordList = 0;
    }
  }
  mExtraDictItemMap.clear();

  mLanguage = lang;

  if (lang == CANTONESE) {
    string zhyDict(mDataPath);
    zhyDict += "/zhy.dict";
    if (stat(zhyDict.c_str(), &statBuf) == 0) {
      this->loadEkhoDict(zhyDict.c_str());
    } else {
      string zhyList(mDataPath);
      zhyList += "/zhy_list";
      if (loadEspeakDict(zhyList) != 0) {
        cerr << "Fail to load espeak dictionary: " << zhyList << endl;
        return -1;
      }

      // save to zhy.dict
      this->saveEkhoDict(zhyDict);
    }
  } else if (lang == MANDARIN) {
    if (loadMandarin()) return -1;
  } else if (lang == ENGLISH) {
    // Nothing to do
  } else if (lang == TOISANESE) {
    string toisaneseList(mDataPath);
    toisaneseList += "/toisanese_list";
    if (loadEspeakDict(toisaneseList) != 0) {
      cerr << "Fail to load dictionary: " << toisaneseList << endl;
      return -1;
    }
  } else if (lang == HAKKA) {
    string kaList(mDataPath);
    kaList += "/ka_list";
    if (loadEspeakDict(kaList) != 0) {
      cerr << "Fail to load dictionary: " << kaList << endl;
      return -1;
    }
  } else if (lang == TIBETAN) {
    string boList(mDataPath);
    boList += "/bo_list";
    if (loadEspeakDict(boList) != 0) {
      cerr << "Fail to load espeak dictionary: " << boList << endl;
      return -1;
    }
    if (loadMandarin()) return -1;
  } else if (lang == NGANGIEN) {
    string yayanList(mDataPath);
    yayanList += "/ngangien_list";
    if (loadEspeakDict(yayanList) != 0) {
      cerr << "Fail to load espeak dictionary: " << yayanList << endl;
      return -1;
    }
  } else if (lang == KOREAN) {
    string koList(mDataPath);
    koList += "/ko_list";
    if (loadEspeakDict(koList) != 0) {
      cerr << "Fail to load espeak dictionary: " << koList << endl;
      return -1;
    }
  }

  addSpecialSymbols();

  return 0;
}

void Dict::addSpecialSymbols(void) {
  // add alphabets
  string voicePath = mDataPath + "/alphabet";

  for (char c = 'a'; c <= 'z'; c++) {
    char *cs = new char[3];
    cs[0] = '\\';
    cs[1] = c;
    cs[2] = 0;
    PhoneticSymbol *ps = new PhoneticSymbol(cs);
    mSpecialSymbols.push_back(ps);
    mSpecialChars.push_back(cs);
    int size = 0;
    const char *pcm = ps->getPcm(voicePath.c_str(), "wav", size);
    addDictItem(c, ps);
    addDictItem(c - ('a' - 'A'), ps);
  }

  // add numbers
  if (mLanguage == CANTONESE) {
    addDictItem(48, getZhyPhon("ling4"));
    addDictItem(49, getZhyPhon("jat1"));
    addDictItem(50, getZhyPhon("ji6"));
    addDictItem(51, getZhyPhon("saam7"));
    addDictItem(52, getZhyPhon("sei3"));
    addDictItem(53, getZhyPhon("ng5"));
    addDictItem(54, getZhyPhon("luk6"));
    addDictItem(55, getZhyPhon("cat1"));
    addDictItem(56, getZhyPhon("baat3"));
    addDictItem(57, getZhyPhon("gau2"));
  } else if (mLanguage == MANDARIN) {
    addDictItem(48, getZhPhon("ling2"));
    addDictItem(49, getZhPhon("yi1"));
    addDictItem(50, getZhPhon("er4"));
    addDictItem(51, getZhPhon("san1"));
    addDictItem(52, getZhPhon("si4"));
    addDictItem(53, getZhPhon("wu3"));
    addDictItem(54, getZhPhon("liu4"));
    addDictItem(55, getZhPhon("qi1"));
    addDictItem(56, getZhPhon("ba1"));
    addDictItem(57, getZhPhon("jiu3"));
  }

  // full pauses
  addDictItem(10, mFullPause);  // "\n"
  addDictItem(59, mFullPause);  // ";"
  mPunctuationNameMap[59] = "分号";
  addDictItem(65307, mFullPause);  // Chinese ";"
  mPunctuationNameMap[65307] = "分号";
  addDictItem(12290, mFullPause);  // Chinese "."
  mPunctuationNameMap[12290] = "句号";
  addDictItem(63, mFullPause);  // ?
  mPunctuationNameMap[63] = "问号";
  addDictItem(65311, mFullPause);  // Chinese ?
  mPunctuationNameMap[65311] = "问号";

  // "." "..."
  addDictItem(46, mFullPause);  // "."
  list<list<Character> > *wordList = new list<list<Character> >();
  list<Character> charList;
  charList.push_back(Character(46, mQuaterPause));
  charList.push_back(Character(46, mQuaterPause));
  charList.push_back(Character(46, mQuaterPause));
  wordList->push_back(charList);
  mDictItemArray[46].wordList = wordList;
  mPunctuationNameMap[46] = "点";

  // half pauses
  addDictItem(44, mHalfPause);  // ","
  mPunctuationNameMap[44] = "逗号";
  addDictItem(65292, mHalfPause);  // Chinese ","
  mPunctuationNameMap[65292] = "逗号";
  addDictItem(58, mHalfPause);  // ":"
  mPunctuationNameMap[58] = "冒号";
  addDictItem(65306, mHalfPause);  // Chinese ":"
  mPunctuationNameMap[65306] = "冒号";
  addDictItem(8230, mHalfPause);  // Chinese "..."
  mPunctuationNameMap[8230] = "省略号";

  // quater pauses
  addDictItem(12289, mQuaterPause);
  mPunctuationNameMap[12289] = "顿号";
  addDictItem(45, mQuaterPause);  // "-"
  mPunctuationNameMap[45] = "减号";
  addDictItem(8212, mQuaterPause);  // Chinese "-"
  mPunctuationNameMap[8212] = "减号";
  //  addDictItem(32, mQuaterPause); // " "
  //  addDictItem(12288, mQuaterPause); // Chinese " "
  addDictItem(39, mQuaterPause);  // "'"
  mPunctuationNameMap[39] = "单引号";
  addDictItem(8216, mQuaterPause);  // Chinese "'"
  mPunctuationNameMap[8216] = "单引号";
  addDictItem(34, mQuaterPause);  // '"'
  mPunctuationNameMap[34] = "双引号";
  addDictItem(8220, mQuaterPause);  // Chinese '"'
  mPunctuationNameMap[8220] = "双引号";
  addDictItem(40, mQuaterPause);  // "("
  mPunctuationNameMap[40] = "左括号";
  addDictItem(65288, mQuaterPause);  // Chinese "("
  mPunctuationNameMap[65288] = "左括号";
  addDictItem(41, mQuaterPause);  // ")"
  mPunctuationNameMap[41] = "右括号";
  addDictItem(65289, mQuaterPause);  // Chinese ")"
  mPunctuationNameMap[65289] = "右括号";
  addDictItem(12298, mQuaterPause);  // Chinese "<<"
  mPunctuationNameMap[12298] = "左书名号";
  addDictItem(12299, mQuaterPause);  // Chinese ">>"
  mPunctuationNameMap[12299] = "右书名号";
  addDictItem(91, mQuaterPause);  // Chinese "["
  mPunctuationNameMap[91] = "左中括号";
  addDictItem(12300, mQuaterPause);  // Chinese "["
  mPunctuationNameMap[12300] = "左中括号";
  addDictItem(93, mQuaterPause);  // Chinese "]"
  mPunctuationNameMap[93] = "右中括号";
  addDictItem(12301, mQuaterPause);  // Chinese "]"
  mPunctuationNameMap[12301] = "右中括号";
  addDictItem(12302, mQuaterPause);  // Chinese "[["
  mPunctuationNameMap[12302] = "左书名号";
  addDictItem(12303, mQuaterPause);  // Chinese "]]"
  mPunctuationNameMap[12303] = "右书名号";

  // for Tibetan
  mPunctuationNameMap[3853] = "ཤད";    // "shad";
  mPunctuationNameMap[3851] = "ཚེ��?";  // "tseg";

  // + - * / ? ... ｛｝“”{}#$%^&*()_+| backslash <>~`
  mPunctuationNameMap[43] = "加号";
  mPunctuationNameMap[42] = "乘号";
  mPunctuationNameMap[47] = "除号";
  mPunctuationNameMap[61] = "等号";
  mPunctuationNameMap[33] = "叹号";
  mPunctuationNameMap[65371] = "左大括号";
  mPunctuationNameMap[65373] = "右大括号";
  mPunctuationNameMap[8221] = "右双引号";
  mPunctuationNameMap[123] = "左大括号";
  mPunctuationNameMap[125] = "右大括号";
  mPunctuationNameMap[35] = "井号";
  mPunctuationNameMap[36] = "美元符号";
  mPunctuationNameMap[37] = "百分号";
  mPunctuationNameMap[94] = "乘方符号";
  mPunctuationNameMap[38] = "符号与";
  mPunctuationNameMap[95] = "下划线";
  mPunctuationNameMap[124] = "符号或";
  mPunctuationNameMap[92] = "反斜杠";
  mPunctuationNameMap[60] = "小于号";
  mPunctuationNameMap[62] = "大于号";
  mPunctuationNameMap[126] = "波浪号";
  mPunctuationNameMap[96] = "左单引号";
}

int Dict::setVoice(string voice) {
  if (mVoice.compare(voice) == 0) return 0;
  mVoice = "English";

  int key_char = 0x7684;
  if (voice == "English") {
    mSfinfo.samplerate = 16000;
    mSfinfo.channels = 1;
    return 0;
  } else if (mLanguage == TIBETAN) {
    key_char = 3904;  // the first char in bo_list
  } else if (mLanguage == KOREAN) {
    key_char = 44032;  // the first char in ko_list
  }

  string path = mDataPath;
  path += "/";
  path += voice;

#ifndef _WIN32_WINNT
  if (mLanguage == CANTONESE || mLanguage == MANDARIN || mLanguage == TIBETAN) {
    string index_file = path;
    if (mLanguage == TIBETAN)
      index_file = mDataPath + "/pinyin.index";
    else
      index_file += ".index";
    if (access(index_file.c_str(), F_OK) != -1) {
      if (mLanguage == TIBETAN) {
        loadEkhoVoiceFile(mDataPath + "/pinyin");
      } else {
        loadEkhoVoiceFile(path);
        mVoice = voice;
        return 0;
      }
    }
  }
#endif

  // index file not exist. generate it
  if (isDir(path.c_str())) {
    mVoice = voice;
    PhoneticSymbol *ps = lookup(key_char);
    if (!ps) {
      ps = lookup(22986);
      if (!ps) {
#ifdef DEBUG_ANDROID
        LOGD("Fail to lookup(de)");
#endif
        cerr << "Fail to lookup char of " << key_char << endl;
        return -1;
      }
    }
    int size;

    // set sfinfo
    ps->setPcm(0, 0);
    const char *pPcm = ps->getPcm(path.c_str(), "wav", size, mSfinfo);
    if (pPcm) {
      mVoiceFileType = "wav";
    } else {
      pPcm = ps->getPcm(path.c_str(), "gsm", size, mSfinfo);
      if (pPcm) {
        mVoiceFileType = "gsm";
      } else {
        pPcm = ps->getPcm(path.c_str(), "ogg", size, mSfinfo);
        if (pPcm) {
          mVoiceFileType = "ogg";
        } else {
          cerr << "No voice data file is found in " << path << endl;
          return -1;
        }
      }
    }

    //    cerr << "samplerate: " << mSfinfo.samplerate <<
    //    ", voice file type: " << mVoiceFileType << endl;

    if (size < 25000) {
      mFullPausePcmSize = size;
    }

#ifndef _WIN32_WINNT
    // FIXME: the index and voice file is very slow and not usable for the
    // second time
    if (mLanguage == CANTONESE || mLanguage == MANDARIN) {
      saveEkhoVoiceFile();
      loadEkhoVoiceFile(path);
    }
#endif

    return 0;
  } else {
    cerr << "Fail to find voice data directory: " << path << endl;
    return -1;
  }
}

PhoneticSymbol *Dict::lookup(Character &c) {
  if (c.code < 65536) {
    return mDictItemArray[c.code].character.phonSymbol;
  } else {
    DictItem *di = &mExtraDictItemMap[c.code];
    if (di) {
      return di->character.phonSymbol;
    } else {
      return 0;
    }
  }
}

list<OverlapType> Dict::lookupOverlap(list<Character> &charList) {
  list<OverlapType> ret;
  list<Character>::iterator cItor = charList.begin();
  list<Character>::iterator end = charList.end();
  while (cItor != end) {
    ret.push_back(OVERLAP_QUIET_PART);
    /*
    string s = cItor->getUtf8();
    if (  // 助词、量词等
        s == "的" || s == "得" || s == "着" || s == "所" || s == "了" ||
        s == "过" || s == "吗" || s == "呢" || s == "吧" || s == "啊" ||
        s == "呀" || s == "么" || s == "与" || s == "且" || s == "之" ||
        s == "为" || s == "兮" || s == "其" || s == "到" || s == "云" ||
        s == "阿" || s == "却" || s == "个" || s == "以" || s == "们" ||
        s == "似" || s == "夫" || s == "只" || s == "向" || s == "呗" ||
        s == "呃" || s == "呵" || s == "哇" || s == "咦" || s == "哟" ||
        s == "哉" || s == "哩" || s == "啵" || s == "唻" || s == "啰" ||
        s == "嘛" || s == "子" || s == "焉" || s == "然" || s == "是" ||
        s == "罢" || s == "而") {
      ret.push_back(OVERLAP_HALF_PART);
    } else {
      ret.push_back(OVERLAP_QUIET_PART);
    }*/

    cItor++;
  }

  return ret;
}

list<PhoneticSymbol *> Dict::lookup(list<Character> &charList, bool firstWord) {
  list<PhoneticSymbol *> phonList;
  list<Character>::iterator cItor = charList.begin();
  list<Character>::iterator cItor2 = charList.begin();
  list<Character> convertedCharList;
  list<Character>::iterator end = charList.end();
  DictItem *di = 0;

  // handle rules of numbers
  if (mLanguage == MANDARIN || mLanguage == CANTONESE) {
    if (hasNumbers(charList)) {
      replaceNumbers(charList, convertedCharList);
      cItor = convertedCharList.begin();
      cItor2 = cItor;
      end = convertedCharList.end();
    }
  }

  while (cItor != end) {
    // treat alphabet number as a whole symbol, should be Pinyin, Jyutping or
    // English
    /*
    if (cItor->code >= 65 && cItor->code <= 122 &&
        (cItor->code <= 90 || cItor->code >= 97)) {
    }*/

    // get DictItem
    if (cItor->code < 65536) {
      di = &mDictItemArray[cItor->code];
    } else {
      di = &mExtraDictItemMap[cItor->code];
    }

    if (!(di->character.phonSymbol)) {
#ifdef WIN32
      // backward compatable to version before 6.0 which call sync instead of
      // sync2
      // output \unknownchar
      di->character.code = cItor->code;  // needed?
      string s = di->character.getUtf8();
      const char *c = s.c_str();
      char *sym = new char[strlen(c) + 2];
      sym[0] = '\\';
      strcpy(sym + 1, c);
      // @TODO: memory leak, but don't care
      PhoneticSymbol *unknownSymbol = new PhoneticSymbol(sym);
      di->character.phonSymbol = unknownSymbol;
#else
      di->character.phonSymbol = PhoneticSymbol::getUnknownPhoneticSymbol();
#endif
    }

    // check word list
    bool foundMatchedWord = false;
    list<list<Character> >::iterator matchedWordItor;
    if (di->wordList) {
      unsigned int matchedWordLen = 0;
      list<list<Character> >::iterator wordItor = di->wordList->begin();
      for (; wordItor != di->wordList->end(); ++wordItor) {
        if (wordItor->size() > matchedWordLen) {  // only check longer word
          // check whether current word is matched
          list<Character>::iterator charItor = wordItor->begin();
          cItor2 = cItor;
          ++cItor2;
          ++charItor;
          while (charItor != wordItor->end() && cItor2 != end &&
                 charItor->code == cItor2->code) {
            ++charItor;
            ++cItor2;
          }

          if (charItor == wordItor->end()) {
            // found a longer matched word
            foundMatchedWord = true;
            matchedWordLen = wordItor->size();
            matchedWordItor = wordItor;
          }
        }
      }  // end of word matching for loop
    }

    if (foundMatchedWord) {
      cItor2 = matchedWordItor->begin();
      for (; cItor2 != matchedWordItor->end(); ++cItor2) {
        phonList.push_back(cItor2->phonSymbol);
        ++cItor;
      }
    } else {
      phonList.push_back(di->character.phonSymbol);
      ++cItor;
    }

    if (firstWord) break;
  }

  if (mLanguage == MANDARIN || mLanguage == TIBETAN) {
    // tone 3 rules: 333->223, 33->23, 3333->2323
    list<PhoneticSymbol *>::reverse_iterator psIt = phonList.rbegin();
    while (psIt != phonList.rend()) {
      while (psIt != phonList.rend() &&
             (*psIt)->symbol[strlen((*psIt)->symbol) - 1] != '3') {
        psIt++;
      }

      if (psIt != phonList.rend()) {
        psIt++;
        if (psIt != phonList.rend() &&
            (*psIt)->symbol[strlen((*psIt)->symbol) - 1] == '3') {
          list<PhoneticSymbol *>::reverse_iterator psNextIt = psIt;
          psNextIt++;
          if (psNextIt != phonList.rend() &&
              (*psNextIt)->symbol[strlen((*psNextIt)->symbol) - 1] == '3') {
            char sym[10] = {0};
            strcpy(sym, (*psIt)->symbol);
            sym[strlen(sym) - 1] = '2';
            *psIt = getZhPhon(sym);
            psIt++;
          }
          char sym[10] = {0};
          strcpy(sym, (*psIt)->symbol);
          sym[strlen(sym) - 1] = '2';
          *psIt = getZhPhon(sym);
        }
      }
    }
  }

  return phonList;
}

PhoneticSymbol *Dict::getPhoneticSymbol(string &symbol) {
  SymbolCode *sym_code = 0;
  if (mLanguage == MANDARIN) {
    sym_code = ZH_PHash::in_word_set(symbol.c_str(), symbol.size());
  } else if (mLanguage == CANTONESE) {
    sym_code = ZHY_PHash::in_word_set(symbol.c_str(), symbol.size());
  }

  //cerr << "getPhoneticSymbol:" << sym_code << endl;

  if (sym_code)
    return &mSymbolArray[sym_code->code];
  else
    return 0;
}

list<Word> Dict::lookupWord(const char *text) {
  list<Word> wordlist;
  TextType type;
  string lastword;  // ENGLISH_TEXT

#ifdef ENABLE_FRISO
  if (mLanguage == MANDARIN || mLanguage == CANTONESE) {
    string filtered_text(text);
    list<Character> charlist = Character::split(filtered_text);
    list<Character> charlist2;
    replaceNumbers(charlist, charlist2);

    filtered_text = Character::join(charlist2);
    friso_set_text(mFrisoTask, (char *)filtered_text.c_str());
    const char *friso_text = filtered_text.c_str();
    list<PhoneticSymbol *> first_word_phons;

    if (mDebug) cerr << "friso: ";
    while (friso_next(mFriso, mFrisoConfig, mFrisoTask)) {
      string word(mFrisoTask->hits->word);
      if (mDebug) cerr << word << "/";
      list<Character> char_list = Character::split(word);
      // use first character's code to decide whether it's English
      int code = char_list.begin()->code;
      if (code < 65536 && !mDictItemArray[code].character.phonSymbol) {
        type = ENGLISH_TEXT;
        if (lastword.empty())
          lastword = word;
        else
          lastword += " " + word;

        // limit length of lastword to avoid festival run too long time
        if (lastword.length() > 256) {
          wordlist.push_back(Word(lastword, type));
          lastword.clear();
        }
      } else if (code < 65536 && mDictItemArray[code].character.phonSymbol &&
                 strstr(mDictItemArray[code].character.phonSymbol->symbol,
                        "pause") > 0) {
        if (strcmp(mDictItemArray[code].character.phonSymbol->symbol,
                   "fullpause") == 0)
          type = FULL_PAUSE;
        else if (strcmp(mDictItemArray[code].character.phonSymbol->symbol,
                        "halfpause") == 0)
          type = HALF_PAUSE;
        else if (strcmp(mDictItemArray[code].character.phonSymbol->symbol,
                        "quaterpause") == 0)
          type = QUATER_PAUSE;

        if (!lastword.empty()) {
          wordlist.push_back(Word(lastword, ENGLISH_TEXT));
          lastword.clear();
        }
        wordlist.push_back(Word(word, type));
      } else {
        type = NON_ENGLISH;
        if (!lastword.empty()) {
          wordlist.push_back(Word(lastword, ENGLISH_TEXT));
          lastword.clear();
        }

        list<PhoneticSymbol *> word_phons = lookup(word);
        if (first_word_phons.empty()) {
          first_word_phons =
              lookupFirstWord(friso_text + mFrisoTask->hits->offset);
          if (first_word_phons.size() < word_phons.size())
            first_word_phons.clear();
        }

        if (!first_word_phons.empty()) {
          list<PhoneticSymbol *>::iterator itor2 = word_phons.begin();
          for (list<PhoneticSymbol *>::iterator itor = first_word_phons.begin();
               itor != first_word_phons.end() && itor2 != word_phons.end();
               itor2++) {
            itor++;
            list<PhoneticSymbol *>::iterator itor3 =
                word_phons.insert(itor2, *first_word_phons.begin());
            first_word_phons.erase(first_word_phons.begin());
            word_phons.erase(itor2);
            itor2 = itor3;
          }
        }

        unsigned int offset = 0;
        unsigned short bytes = 0;

        getWordPcm(word_phons, offset, bytes);
        wordlist.push_back(Word(word, type, word_phons, offset, bytes));
      }
    }

    if (!lastword.empty()) wordlist.push_back(Word(lastword, ENGLISH_TEXT));

    if (mDebug) cerr << endl;
  } else {
#endif
    string last_chinese_word;
    string t(text);
    list<Character> char_list2 = Character::split(t);
    list<Character> char_list;
    replaceNumbers(char_list2, char_list);
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
          symbol.clear();
          itor2++;
          int length = 0;
          while (itor2 != char_list.end() && itor2->code != ' ' &&
                 itor2->code != ']' && itor2->code < 128 && length < 10) {
            length++;
            symbol += itor2->getUtf8();
            itor2++;
          }

          if (itor2 != char_list.end() &&
              (itor2->code == ' ' || itor2->code == ']')) {
            // found symbol
            PhoneticSymbol *phon_symbol = getPhoneticSymbol(symbol);
            if (phon_symbol) {
              // submit pedning English and Chinese word
              if (!lastword.empty()) {
                wordlist.push_back(Word(lastword, ENGLISH_TEXT));
                lastword.clear();
              }

              if (!last_chinese_word.empty()) {
                wordlist.push_back(Word(last_chinese_word, NON_ENGLISH,
                                        lookup(last_chinese_word),
                                        lookupOverlap(last_chinese_word)));
                last_chinese_word.clear();
              }

              // submit raw phonetic symbol
              list<PhoneticSymbol *> phon_list;
              phon_list.push_back(phon_symbol);
              wordlist.push_back(Word(symbol, PHONETIC, phon_list));
              itor2++;

              length = 0;
              symbol.clear();
              while (itor2 != char_list.end() && itor2->code != ']' &&
                     itor2->code < 128 && length < 10) {
                if (itor2->code == ' ') {
                  // found next symbol
                  // cerr << symbol << endl;
                  phon_symbol = getPhoneticSymbol(symbol);
                  if (phon_symbol) {
                    list<PhoneticSymbol *> phon_list;
                    phon_list.push_back(phon_symbol);
                    wordlist.push_back(Word(symbol, PHONETIC, phon_list));
                    length = 0;
                    symbol.clear();
                  }
                } else {
                  length++;
                  symbol += itor2->getUtf8();
                }
                itor2++;
              }

              if (itor2 != char_list.end() && itor2->code == ']') {
                // handle last symbol
                phon_symbol = getPhoneticSymbol(symbol);
                if (phon_symbol) {
                  list<PhoneticSymbol *> phon_list;
                  phon_list.push_back(phon_symbol);
                  wordlist.push_back(Word(symbol, PHONETIC, phon_list));
                }
              }

              // end of [[pinyin ]] style
              itor = itor2;
              if (itor == char_list.end()) break;
            }
          }
        }
      }

      if (code < 65536 && mDictItemArray[code].character.phonSymbol &&
          strstr(mDictItemArray[code].character.phonSymbol->symbol, "pause") >
              0) {
        if (lastword.empty()) {
          // it's a symbol, not including space
          if (strcmp(mDictItemArray[code].character.phonSymbol->symbol,
                     "fullpause") == 0)
            type = FULL_PAUSE;
          else if (strcmp(mDictItemArray[code].character.phonSymbol->symbol,
                          "halfpause") == 0)
            type = HALF_PAUSE;
          else if (strcmp(mDictItemArray[code].character.phonSymbol->symbol,
                          "quaterpause") == 0)
            type = QUATER_PAUSE;

          // submit pending Chinese word
          if (!last_chinese_word.empty()) {
            wordlist.push_back(Word(last_chinese_word, NON_ENGLISH,
                                    lookup(last_chinese_word),
                                    lookupOverlap(last_chinese_word)));
            last_chinese_word.clear();
          }

          wordlist.push_back(Word(itor->getUtf8(), type));
        } else {
          lastword += itor->getUtf8();
        }
      } else if (itor->code < 65536 && (!mDictItemArray[itor->code].character.phonSymbol ||
            (itor->code >= 'A' && itor->code <='Z') || (itor->code >= 'a' && itor->code <= 'z'))) {
        // it's not a Chinese character
        if ((itor->code >= 'A' && itor->code <= 'Z') ||
            (itor->code >= 'a' && itor->code <= 'z') || !lastword.empty()) {
          // it's alphabet
          lastword += itor->getUtf8();
          if (!last_chinese_word.empty()) {
            wordlist.push_back(Word(last_chinese_word, NON_ENGLISH,
                                    lookup(last_chinese_word),
                                    lookupOverlap(last_chinese_word)));
            last_chinese_word.clear();
          }
        } else {
          // it's space or other symbols
          last_chinese_word += itor->getUtf8();
        }
      } else {
        // it's a Chinese character
	//cout << "found chinese character: " << itor->getUtf8() << ", " << mDictItemArray[itor->code].character.getUtf8() << endl;
        last_chinese_word += itor->getUtf8();
        if (!lastword.empty()) {
	  char c = lastword[0];
          if (lastword.length() == 1 && ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))) {
	    wordlist.push_back(Word(lastword, ENGLISH_TEXT, lookup(lastword), lookupOverlap(lastword)));
          } else {
            wordlist.push_back(Word(lastword, ENGLISH_TEXT));
	  }
          lastword.clear();
        }
      }
    }

    if (!lastword.empty()) {
      wordlist.push_back(Word(lastword, ENGLISH_TEXT, lookup(lastword), lookupOverlap(lastword)));
    }

    if (!last_chinese_word.empty()) {
      wordlist.push_back(Word(last_chinese_word, NON_ENGLISH,
                              lookup(last_chinese_word),
                              lookupOverlap(last_chinese_word)));
    }
#ifdef ENABLE_FRISO
  }
#endif

  return wordlist;
}

void Dict::getWordPcm(list<PhoneticSymbol *> &word_phon, unsigned int &offset,
                      unsigned short &bytes) {
  string symbols;
  int i = 0;
  int len = word_phon.size();
  for (list<PhoneticSymbol *>::iterator itor = word_phon.begin();
       itor != word_phon.end(); itor++) {
    symbols += (*itor)->symbol;
    i++;
    if (i < len) symbols += "-"; // 多字录音的文件名以-分隔拼音
  }

  map<string, PhoneticSymbol>::iterator sym = mWordSymbolMap.find(symbols);

  // 如果文件没有找到，尝试读其它声调的文件顶替
  /*
  for (char c = '1'; sym == mWordSymbolMap.end() && c <= '7'; c++) {
    string s = symbols.substr(0, symbols.length() - 1) + c;
    cerr << "try " << s << endl;
    sym = mWordSymbolMap.find(symbols);
  }*/

  if (sym != mWordSymbolMap.end()) {
    offset = sym->second.offset;
    bytes = sym->second.bytes;
  }
}

/**
 * Get phonetic symbol code in mKaSymbolArray
 * Generate one if not exists, mKaSymbolIndex will add one too
 * @Return: index in mSymbolArray
 */
int Dict::getSymbolCode(SymbolLetter *root, const char *symbol) {
  const char *c = symbol;
  char code = 0;
  SymbolLetter *sym = root;
  while (c && *c) {
    code = *c;
    if (code >= 97)
      code -= 97;
    else if (code >= 48)
      code = code - 48 + 26;

    if (code >= 36) {
      cerr << "Fail to getSymbolCode: " << symbol << endl;
      return 0;
    }

    if (!sym[code].next) {
      sym[code].next = (SymbolLetter *)malloc(sizeof(SymbolLetter) * 36);
      memset(sym[code].next, 0, sizeof(SymbolLetter) * 36);
    }

    sym = sym[code].next;
    c++;
  }

  if (!sym[code].index) {
    mKaSymbolIndex++;
    char *s = (char *)malloc(strlen(symbol) + 1);
    memcpy(s, symbol, strlen(symbol) + 1);
    // TODO: free s
    mKaSymbolArray[mKaSymbolIndex] = new PhoneticSymbol(s);
    sym[code].index = mKaSymbolIndex;
  }

  return sym[code].index;
}

/**
 * Load eSpeak dictionary
 * @Return:
 *   0  - success
 *  -1  - fail to open dictionary
 */
int Dict::loadEspeakDict(const char *path) {
  string line;
  bool has_syntax_error = false;
  map<int, DictItem>::iterator it;
  map<string, PhoneticSymbol>::iterator symbol_it;
  bool debug = false;

  ifstream fs(path);
  if (!fs.is_open()) {
    cerr << "Fail to open file " << path << " at " << __LINE__ << endl;
    return -1;
  }

  getline(fs, line);
  int linecount = 1;
  while (!fs.eof() && !fs.fail()) {
    list<Character> char_list = Character::split(line);
    if (char_list.size() > 0) {
      list<Character>::iterator c = char_list.begin();
      list<Character> word;

      // skip space
      while (c != char_list.end() && (c->code == ' ' || c->code == '\t')) {
        c++;
      }

      // skip comment line and number definition line
      // number will be defined manually
      list<Character>::iterator c2 = c;
      c2++;
      if (c != char_list.end() && c2 != char_list.end() &&
          ((c->code == '/' && c2->code == '/') || (c->code == '_'))) {
        c = char_list.end();
      }

      // check whether it's a word line
      else if (c != char_list.end() && c->code == '(' &&
               c2 != char_list.end()) {
        c++;
        c2++;
        word.push_back(c->code);
        c++;
        c2++;

        while (c != char_list.end() && c->code == ' ' &&
               c2 != char_list.end()) {
          c++;
          c2++;
          word.push_back(c->code);
          c++;
          c2++;
        }

        if (c != char_list.end() && c->code == ')' && c2 != char_list.end()) {
          // skip space
          c++;
          while (c != char_list.end() && (c->code == ' ' || c->code == '\t')) {
            c++;
          }

          list<Character>::iterator ch = word.begin();

          while (c != char_list.end()) {
            // get next phonetic symbol
            string symbol = "";
            while (c != char_list.end() &&
                   ((c->code >= 'a' && c->code <= 'z') ||
                    (c->code >= '0' && c->code <= '9') ||
                    (c->code >= 'A' && c->code <= 'Z'))) {
              symbol += c->getUtf8();
              if (c->code >= '0' && c->code <= '9') {
                c++;
                while (c != char_list.end() && c->code >= '0' &&
                       c->code <= '9') {
                  symbol += c->getUtf8();
                  c++;
                }
                break;
              }
              c++;
            }

            if (symbol.length() > 0) {
              // asign phonetic symbol to character
              if (ch != word.end()) {
                SymbolCode *pSymCode;
                if (mLanguage == CANTONESE) {
                  pSymCode =
                      ZHY_PHash::in_word_set(symbol.c_str(), symbol.size());
                  if (pSymCode) {
                    ch->phonSymbol = &mSymbolArray[pSymCode->code];
                  } else {
                    cerr << "Unknown symbol " << symbol << " at line "
                         << linecount << endl;
                    has_syntax_error = true;
                  }
                } else if (mLanguage == MANDARIN) {
                  pSymCode =
                      ZH_PHash::in_word_set(symbol.c_str(), symbol.size());
                  if (pSymCode) {
                    ch->phonSymbol = &mSymbolArray[pSymCode->code];
                  } else {
                    cerr << "Unknown symbol " << symbol << " at line "
                         << linecount << endl;
                    has_syntax_error = true;
                  }
                } else if (mLanguage == HAKKA || mLanguage == KOREAN ||
                           mLanguage == TOISANESE || mLanguage == TIBETAN ||
                           mLanguage == NGANGIEN) {
                  int code = getSymbolCode(mKaSymbolLetter, symbol.c_str());
                  if (code) {
                    ch->phonSymbol = mKaSymbolArray[code];
                  } else {
                    cerr << "Unknown symbol " << symbol << " at line "
                         << linecount << endl;
                    has_syntax_error = true;
                  }
                }

                ch++;
              } else {
                cerr << "Bad espeak dictionary format at line " << linecount
                     << "(symbols more than characters): " << line << endl;
                has_syntax_error = true;
                break;
              }

              // skip "|" if exists
              if (c != char_list.end() && (c->code == '|' || c->code == '\r')) {
                c++;
              }

            } else {
              cerr << "Bad espeak dictionary format at line " << linecount
                   << "(symbol not ended with number): " << line << endl;
              has_syntax_error = true;
              break;
            }
          }

          if (ch != word.end()) {
            cerr << "Bad espeak dictionary format at line " << linecount
                 << "(characters more than symbols): " << line << endl;
            has_syntax_error = true;
          }

          if (!has_syntax_error) {
            DictItem *di = 0;
            int code = word.begin()->code;
            if (code < 65536) {
              di = &mDictItemArray[code];
            } else {
              di = &mExtraDictItemMap[code];
            }
            if (!di->wordList) {
              di->wordList = new list<list<Character> >();
            }
            di->wordList->push_back(word);
          }
        } else {
          cerr << "Bad espeak dictionary format at line " << linecount
               << "(word not quoted by ')'): " << line << endl;
        }
      }

      // a char line
      else if (c2 != char_list.end() &&
               /*        c->code > 256 && // ignore ASCII temporary */
               (c2->code == ' ' || c2->code == '\t')) {
        Character c3 = *c;

        // skip space
        c++;
        while (c != char_list.end() && (c->code == ' ' || c->code == '\t')) {
          c++;
        }

        // get next phonetic symbol
        string symbol = "";
        while (c != char_list.end() && ((c->code >= 'a' && c->code <= 'z') ||
                                        (c->code >= '0' && c->code <= '9') ||
                                        (c->code >= 'A' && c->code <= 'Z'))) {
          symbol += c->getUtf8();
          if (c->code >= '0' && c->code <= '9') {
            c++;
            while (c != char_list.end() && c->code >= '0' && c->code <= '9') {
              symbol += c->getUtf8();
              c++;
            }
            break;
          }
          c++;
        }

        // asign phonetic symbol to character
        if (symbol.length() > 0) {
          SymbolCode *pSymCode = 0;
          if (mLanguage == CANTONESE) {
            pSymCode = ZHY_PHash::in_word_set(symbol.c_str(), symbol.size());
          } else if (mLanguage == MANDARIN) {
            pSymCode = ZH_PHash::in_word_set(symbol.c_str(), symbol.size());
          }

          if (pSymCode) {
            c3.phonSymbol = &mSymbolArray[pSymCode->code];
            if (c3.code < 65536) {
              mDictItemArray[c3.code].character = c3;
            } else {
              mExtraDictItemMap[c3.code].character = c3;
            }
            //          cout << c3.getUtf8() << "(" << c3.code << "): " <<
            //              c3.phonSymbol->symbol << endl; // debug code
          } else if (mLanguage == HAKKA || mLanguage == KOREAN ||
                     mLanguage == TOISANESE || mLanguage == TIBETAN ||
                     mLanguage == NGANGIEN) {
            int code = getSymbolCode(mKaSymbolLetter, symbol.c_str());
            if (code) {
              c3.phonSymbol = mKaSymbolArray[code];
              if (c3.code < 65536) {
                mDictItemArray[c3.code].character = c3;
              } else {
                mExtraDictItemMap[c3.code].character = c3;
              }
            } else {
              cerr << "Unknown symbol " << symbol << " at line " << linecount
                   << endl;
            }
          } else {
            cerr << "Unknown symbol " << symbol << " at line " << linecount
                 << endl;
          }
        }

        //    cout << c3.code << c3.phonSymbol->symbol << endl; // debug
        // code
      }

      // bad line
      else if (c != char_list.end()) {
        cerr << "Bad espeak dictionary format at line " << linecount << ": "
             << line << endl;  // debug code
      }
    }

    getline(fs, line);
    linecount++;
  }

  return 0;
}  // end of loadEspeakDict

int Dict::saveEkhoDict(const char *path) {
  ofstream os(path, ifstream::binary);
  unsigned short max_char_count = 0;

  map<int, DictItem>::iterator diMapItor = mExtraDictItemMap.begin();
  DictItem *di = 0;
  for (int i = 0; i < 65536 || diMapItor != mExtraDictItemMap.end(); ++i) {
    if (i < 65536) {
      di = &mDictItemArray[i];
    } else {
      di = &diMapItor->second;
      ++diMapItor;
    }

    // write character code
    unsigned int code = di->character.code;
    if (code > 0 && di->character.phonSymbol) {
      if (code < 65536) {
        os.put((unsigned char)(code & 0xFF));
        os.put((unsigned char)((code >> 8) & 0xFF));
      } else {
        os.put(1);
        os.put(0);
        os.put((unsigned char)(code & 0xFF));
        os.put((unsigned char)((code >> 8) & 0xFF));
        os.put((unsigned char)((code >> 16) & 0xFF));
        os.put((unsigned char)((code >> 24) & 0xFF));
      }

      // write character symbol
      SymbolCode *pSymCode;
      const char *symbol = di->character.phonSymbol->symbol;
      if (mLanguage == CANTONESE) {
        pSymCode = ZHY_PHash::in_word_set(symbol, strlen(symbol));
      } else if (mLanguage == MANDARIN) {
        pSymCode = ZH_PHash::in_word_set(symbol, strlen(symbol));
      } else {
        cerr << "not implemented" << endl;
      }
      unsigned short symbolCode = pSymCode->code;
      os.put((unsigned char)(symbolCode & 0xFF));
      os.put((unsigned char)((symbolCode >> 8) & 0xFF));

      //      cout << di->character.getUtf8() << "(" << code << "): " <<
      //          mSymbolArray[symbolCode].symbol << endl; // debug code

      // write number of words
      if (di->wordList) {
        list<list<Character> > *wordList = di->wordList;
        int size = wordList->size();
        os.put((unsigned char)(size & 0xFF));
        os.put((unsigned char)((size >> 8) & 0xFF));

        // write words
        list<list<Character> >::iterator wordItor = wordList->begin();
        for (; wordItor != wordList->end(); ++wordItor) {
          // write number of character in the word
          size = wordItor->size();
          if (size > max_char_count) max_char_count = size;
          os.put((unsigned char)(size & 0xFF));
          os.put((unsigned char)((size >> 8) & 0xFF));

          // write characters
          list<Character>::iterator charItor = wordItor->begin();
          for (; charItor != wordItor->end(); ++charItor) {
            // write character code
            code = charItor->code;
            if (code < 65536) {
              os.put((unsigned char)(code & 0xFF));
              os.put((unsigned char)((code >> 8) & 0xFF));
            } else {
              os.put(1);
              os.put(0);
              os.put((unsigned char)(code & 0xFF));
              os.put((unsigned char)((code >> 8) & 0xFF));
              os.put((unsigned char)((code >> 16) & 0xFF));
              os.put((unsigned char)((code >> 24) & 0xFF));
            }

            // write character symbol
            symbol = charItor->phonSymbol->symbol;
            if (mLanguage == CANTONESE) {
              pSymCode = ZHY_PHash::in_word_set(symbol, strlen(symbol));
            } else if (mLanguage == MANDARIN) {
              pSymCode = ZH_PHash::in_word_set(symbol, strlen(symbol));
            } else {
              cerr << "not implemented" << endl;
            }
            symbolCode = pSymCode->code;
            if (symbolCode > SYMBOL_ARRAY_SIZE) {
              cerr << "symbol code out of range: " << symbolCode << endl;
              os.close();
              return -1;
            }
            os.put((unsigned char)(symbolCode & 0xFF));
            os.put((unsigned char)((symbolCode >> 8) & 0xFF));
          }
        }
      } else {
        os.put(0);
        os.put(0);
      }
    }
  }

  os.close();

  cerr << "max character count in word: " << max_char_count << endl;

  return 0;
}

int Dict::loadEkhoDict(const char *path) {
  ifstream infile(path, ifstream::binary);
  Character c;
  list<Character> charList;
  list<list<Character> > *wordList;
  unsigned char lowbyte;
  unsigned int tmpInt;
  unsigned int code;
  unsigned short symbolCode;
  unsigned short wordCount;
  unsigned short charCount;

#ifdef DEBUG_PERF
  clock_t begin_clock = clock();
#endif

  while (infile.good()) {
    // get character code
    lowbyte = (unsigned char)infile.get();
    if (!infile.good()) break;
    code = (unsigned char)infile.get();
    code = (code << 8) + lowbyte;
    if (code == 1) {
      code = (unsigned char)infile.get();
      tmpInt = (unsigned char)infile.get();
      tmpInt <<= 8;
      code += tmpInt;
      tmpInt = (unsigned char)infile.get();
      tmpInt <<= 16;
      code += tmpInt;
      tmpInt = (unsigned char)infile.get();
      tmpInt <<= 24;
      code += tmpInt;
    }

    // get character symbol
    lowbyte = (unsigned char)infile.get();
    symbolCode = (unsigned char)infile.get();
    symbolCode = (symbolCode << 8) + lowbyte;
    /*  if (! infile.good()) {
                    cerr << "fail to get at line " << __LINE__ << endl;
                    return -1;
            }*/
    if (symbolCode > SYMBOL_ARRAY_SIZE) {
      cerr << "Corrupted dictionary (phonetic symbol code out of range): "
           << symbolCode << " at line " << __LINE__ << endl;
      infile.close();
      return -1;
    }

    // add DictItem
    DictItem *di = 0;
    if (code < 65536) {
      di = &mDictItemArray[code];
    } else {
      di = &mExtraDictItemMap[code];
    }
    di->character.code = code;
    di->character.phonSymbol = &mSymbolArray[symbolCode];
    //    cout << di->character.getUtf8() << "(" << code << "): " <<
    //            mSymbolArray[symbolCode].symbol << endl; // debug code

    // get number of words
    lowbyte = (unsigned char)infile.get();
    wordCount = (unsigned char)infile.get();
    wordCount = (wordCount << 8) + lowbyte;

    if (wordCount > 0) {
      wordList = new list<list<Character> >();
      for (int i = 0; i < wordCount; ++i) {
        // add a word
        charList.clear();
        lowbyte = (unsigned char)infile.get();
        charCount = (unsigned char)infile.get();
        charCount = (charCount << 8) + lowbyte;

        // add characters of word
        for (int j = 0; j < charCount; ++j) {
          // get character code
          lowbyte = (unsigned char)infile.get();
          code = (unsigned char)infile.get();
          code = (code << 8) + lowbyte;
          if (code == 1) {
            code = infile.get();
            tmpInt = infile.get();
            tmpInt <<= 8;
            code += tmpInt;
            tmpInt = infile.get();
            tmpInt <<= 16;
            code += tmpInt;
            tmpInt = infile.get();
            tmpInt <<= 24;
            code += tmpInt;
          }

          // get symbol
          lowbyte = (unsigned char)infile.get();
          symbolCode = (unsigned char)infile.get();
          symbolCode = (symbolCode << 8) + lowbyte;
          /*        if (! infile.good()) {
                              cerr << "fail to get at line " << __LINE__ <<
             endl;
                              return -1;
                        }*/
          if (symbolCode > SYMBOL_ARRAY_SIZE) {
            cerr << "Corrupted dictionary (phonetic symbol code out of range): "
                 << symbolCode << " at line " << __LINE__ << endl;
            infile.close();
            return -1;
          }

          // add Character to list
          c.code = code;
          c.phonSymbol = &mSymbolArray[symbolCode];
          charList.push_back(c);
          // cout << c.getUtf8() << c.phonSymbol->symbol; // debug code
        }

        // add word to list
        wordList->push_back(charList);
        //        cout << " "; // debug code
      }

      // add word list
      di->wordList = wordList;
      // cout << endl; // debug code
    }  // end of if (wordCount > 0)
  }    // end of while (!infile.eof())

#ifdef DEBUG_PERF
  clock_t end_clock = clock();
  cout << "clocks: " << end_clock - begin_clock
       << ", CLOCKS_PER_SEC=" << CLOCKS_PER_SEC << endl;
#endif

  infile.close();
  return 0;
}

void Dict::getWordContext(const char *text, char *in_word_context,
                          int phons_len) {
#ifdef ENABLE_FRISO
  if (mLanguage != MANDARIN && mLanguage != CANTONESE) return;
  friso_set_text(mFrisoTask, (char *)text);
  int ii = 0;
  while (ii < phons_len && friso_next(mFriso, mFrisoConfig, mFrisoTask)) {
    string word(mFrisoTask->hits->word);
    list<Character> char_list = Character::split(word);
    int len = char_list.size();
    for (int i = 0; i < len - 1 && ii < phons_len; i++) {
      in_word_context[ii] = 1;
      ii++;
    }
    in_word_context[ii] = 0;
    ii++;
  }

  if (ii != phons_len) {
    memset(in_word_context, 0, phons_len);
    if (mDebug) {
      cerr << "friso len not match phons len" << endl;
    }
  }
#else
  memset(in_word_context, 0, phons_len);
#endif
}

unsigned short Dict::getCodeOfSymbol(string &symbol) {
  SymbolCode *symbol_code = 0;
  if (mLanguage == CANTONESE) {
    symbol_code = ZHY_PHash::in_word_set(symbol.c_str(), symbol.size());
    if (symbol_code) return symbol_code->code;
  } else if (mLanguage == MANDARIN) {
    symbol_code = ZH_PHash::in_word_set(symbol.c_str(), symbol.size());
    if (symbol_code) return symbol_code->code;
  }

  return 0;
}

void Dict::saveEkhoVoiceFile() {
  // only support Cantonese and Mandarin
  if (mLanguage == HAKKA || mLanguage == KOREAN || mLanguage == TIBETAN ||
      mLanguage == NGANGIEN || mLanguage == TOISANESE) {
    return;
  }

  string index_file = mDataPath;
  index_file += "/";
  index_file += mVoice;

  string voice_dir = index_file;

  string voice_file = index_file;
  voice_file += ".voice";

  index_file += ".index";

  ofstream os(index_file.c_str(), ifstream::binary);
  os.put((unsigned char)(mSfinfo.samplerate & 0xFF));
  os.put((unsigned char)((mSfinfo.samplerate >> 8) & 0xFF));

  unsigned char type = 0;
  if (strcmp(mVoiceFileType, "wav") == 0)
    type = 1;
  else if (strcmp(mVoiceFileType, "gsm") == 0)
    type = 2;
  os.put(type);

  // reserve for symbo count
  os.put(0);
  os.put(0);

  // scan files in voice_dir
  DIR *dirp;
  struct dirent *dp;
  if ((dirp = opendir(voice_dir.c_str())) == NULL) {
    cerr << "Fail to open " << voice_dir << endl;
    return;
  }

  string symbol;
  SymbolCode *symbol_code = 0;

  // prepare output file
  FILE *file = fopen(voice_file.c_str(), "w");
  // SNDFILE *sndfile = sf_open(voice_file.c_str(), SFM_WRITE, &mSfinfo);
  unsigned int total_bytes = 0;

  do {
    if ((dp = readdir(dirp)) != NULL) {
      char *suffix = strstr(dp->d_name, mVoiceFileType);
      if (!suffix || suffix - dp->d_name < 2) continue;

      symbol = dp->d_name;
      symbol = symbol.substr(0, suffix - dp->d_name - 1);

      if (symbol.find("-") == string::npos) {
        // cerr << "index " << symbol << endl;
        // single phonetic symbol
        os.put(1);
        unsigned short code = getCodeOfSymbol(symbol);
        os.put(code & 0xFF);
        os.put((code >> 8) & 0xFF);

        // get file content
        char buffer[128000];
        string path = voice_dir + "/" + symbol + "." + mVoiceFileType;
        FILE *gsmfile = fopen(path.c_str(), "r");

        int bytes = 0;
        int b = 0;
        do {
          b = fread(buffer, 1, 128000, gsmfile);
          bytes += b;
          fwrite(buffer, 1, b, file);
        } while (b == 128000);

	/*
	fseek(gsmfile, 0L, SEEK_END);
        int size = ftell(gsmfile);
	if (size != bytes) {
	  cout << "bytes=" << bytes << ", size=" << size << endl;
	}*/

        fclose(gsmfile);

        os.put(total_bytes & 0xFF);
        os.put((total_bytes >> 8) & 0xFF);
        os.put((total_bytes >> 16) & 0xFF);
        os.put((total_bytes >> 24) & 0xFF);
        total_bytes += bytes;

        os.put(bytes & 0xFF);
        os.put((bytes >> 8) & 0xFF);
        os.put((bytes >> 16) & 0xFF);

        //cerr << "code:" << code << ", offset=" << total_bytes <<
        // ", bytes=" << bytes << endl;
      } else {
        // multiple symbols (word)
        cerr << "found word:" << symbol << endl;
        list<string> symbols;
        string symbol0 = symbol;
        int pos = 0;
        do {
          pos = symbol.find("-");
          symbols.push_back(symbol.substr(0, pos));
          symbol.erase(0, pos + 1);
        } while (pos == string::npos);
        symbols.push_back(symbol);

        os.put(symbols.size());
        for (list<string>::iterator sym = symbols.begin(); sym != symbols.end();
             sym++) {
          unsigned short code = getCodeOfSymbol(*sym);
          os.put(code & 0xFF);
          os.put((code >> 8) & 0xFF);
        }

        // get file content
        char buffer[512000];
        string path = voice_dir + "/" + symbol0 + "." + mVoiceFileType;
        FILE *gsmfile = fopen(path.c_str(), "r");

        int bytes = 0;
        int b = 0;
        do {
          b = fread(buffer, 1, 512000, gsmfile);
          bytes += b;
          fwrite(buffer, 1, b, file);
        } while (b == 512000);

        fclose(gsmfile);

        os.put(total_bytes & 0xFF);
        os.put((total_bytes >> 8) & 0xFF);
        os.put((total_bytes >> 16) & 0xFF);
        os.put((total_bytes >> 24) & 0xFF);
        total_bytes += bytes;

        os.put(bytes & 0xFF);
        os.put((bytes >> 8) & 0xFF);
        os.put((bytes >> 16) & 0xFF);
      }
    }
  } while (dp != NULL);

  closedir(dirp);
  fclose(file);
  os.close();
}

void Dict::loadEkhoVoiceFile(string path) {
  // only support Cantonese and Mandarin
  if (mLanguage == HAKKA || mLanguage == TOISANESE || mLanguage == KOREAN ||
      mLanguage == NGANGIEN) {
    return;
  }

  unsigned char lowbyte;
  unsigned short code;
  unsigned int offset;
  int bytes;
  unsigned int tmpint;

  string index_file = path + ".index";
  string voice_file = path + ".voice";

  // get samplerate
  ifstream is(index_file.c_str(), ifstream::binary);
  lowbyte = (unsigned char)is.get();
  unsigned short samplerate = (unsigned char)is.get();
  samplerate = (samplerate << 8) + lowbyte;

  // get audio type
  lowbyte = is.get();
  if (lowbyte == 1)
    mVoiceFileType = "wav";
  else if (lowbyte == 2)
    mVoiceFileType = "gsm";

  // get symbol count
  is.get();
  is.get();

  // get code count
  int code_count;
  while ((code_count = is.get()) != std::char_traits<char>::eof()) {
    if (code_count == 1) {
      // code
      lowbyte = (unsigned char)is.get();
      code = (unsigned char)is.get();
      code = (code << 8) + lowbyte;

      // offset
      offset = (unsigned char)is.get();
      tmpint = (unsigned char)is.get();
      offset += (tmpint << 8);
      tmpint = (unsigned char)is.get();
      offset += (tmpint << 16);
      tmpint = (unsigned char)is.get();
      offset += (tmpint << 24);

      // bytes
      bytes = (unsigned char)is.get();
      tmpint = (unsigned char)is.get();
      bytes += (tmpint << 8);
      tmpint = (unsigned char)is.get();
      bytes += (tmpint << 16);

      //cerr << code << ":" << offset << "," << bytes << endl;

      mSymbolArray[code].offset = offset;
      mSymbolArray[code].bytes = bytes;
    } else {
      // bytes for word
      char symbols[256] = {0};
      for (int i = 0; i < code_count; i++) {
        // code
        lowbyte = (unsigned char)is.get();
        code = (unsigned char)is.get();
        code = (code << 8) + lowbyte;
        //mSymbolArray[code].symbol;
        //cerr << mSymbolArray[code].symbol << endl;
        strcat(symbols, mSymbolArray[code].symbol);
        if (i < code_count - 1) {
          strcat(symbols, "-");
        }
      }

      // offset
      offset = (unsigned char)is.get();
      tmpint = (unsigned char)is.get();
      offset += (tmpint << 8);
      tmpint = (unsigned char)is.get();
      offset += (tmpint << 16);
      tmpint = (unsigned char)is.get();
      offset += (tmpint << 24);

      // bytes
      bytes = (unsigned char)is.get();
      tmpint = (unsigned char)is.get();
      bytes += (tmpint << 8);
      tmpint = (unsigned char)is.get();
      bytes += (tmpint << 16);

      //cerr << symbols << offset << bytes << endl;

      mWordSymbolMap[symbols] = PhoneticSymbol(symbols, offset, bytes);
    }
  }

  is.close();

  if (mVoiceFile) {
    fclose(mVoiceFile);
  }

  memset(&mSfinfo, 0, sizeof(mSfinfo));
  mSfinfo.samplerate = samplerate;
  mSfinfo.channels = 1;
  if (strcmp(mVoiceFileType, "gsm")) {
    mSfinfo.format = SF_FORMAT_WAV | SF_FORMAT_GSM610;
  }
  //mVoiceFile = sf_open(voice_file.c_str(), SFM_READ, &mSfinfo);
  mVoiceFile = fopen(voice_file.c_str(), "r");

  SymbolCode *sym_code;
  if (mLanguage == MANDARIN) {
    sym_code = ZH_PHash::in_word_set("de5", 3);
    int size = 0;
    mSymbolArray[sym_code->code].getPcm(mVoiceFile, size);
    mSfinfo.frames = size / 2;
  } else {
    mSfinfo.frames = 0;
  }

  if (mDebug) {
    cerr << "sampleRate=" << samplerate << ", fileType=" << mVoiceFileType << endl;
  }
}

PhoneticSymbol* Dict::getPhoneticSymbol(char *symbol) {
  if (Dict::me) {
    string s(symbol);
    unsigned short code = Dict::me->getCodeOfSymbol(s);
    return &Dict::me->mSymbolArray[code];
  }

  return 0;
}

}
