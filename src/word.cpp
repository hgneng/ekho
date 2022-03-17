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

  list<WordPinyin> emptyList;
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
        } else if (*c == '.') {
          break;
        }
        c++;
      }

      if (charPinyin != ".") {
        // 非法的拼音，跳过
        cerr << "bad pinyin: " << dp->d_name << endl;
        continue;
      }

      charPinyin = wordPinyin.front();

      if (Word::voiceFilesMap.find(charPinyin) == Word::voiceFilesMap.end()) {
        Word::voiceFilesMap[charPinyin] = emptyList;
      }

      list<WordPinyin>::iterator it = Word::voiceFilesMap[charPinyin].begin();
      while (it != Word::voiceFilesMap[charPinyin].end()) {
        if (it->size() <= wordPinyin.size()) {
          Word::voiceFilesMap[charPinyin].insert(it, wordPinyin);
          break;
        }
      }

      if (it == Word::voiceFilesMap[charPinyin].end()) {
        Word::voiceFilesMap[charPinyin].push_back(wordPinyin);
      }
      
      wordCount++;
    }
  } while (dp != NULL);

  closedir(dirp);
  cerr << "map size: " << Word::voiceFilesMap.size() << endl;
  cerr << "word count: " << wordCount << endl;
}