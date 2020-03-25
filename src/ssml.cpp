/***************************************************************************
 * Copyright (C) 2008-2020 by Cameron Wong                                 *
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

#include <string>
#include <iostream>
#include "ssml.h"
#include "utf8.h"
using namespace std;
using namespace ekho;

/**
 * Filter tags:
 * Ex: <speak><mark name="0:8"/>屏幕阅读器启用。</speak>
 * <speak><mark name="0:1"/>左 <mark name="2:7"/>Shift </speak>
 */
string Ssml::stripTags(const string& text) {
  int first_lt = text.find("<speak");
  int first_gt;
  if (first_lt == 0) {
    first_gt = text.find('>');
    if (first_gt > 0) {
      string tag = text.substr(first_lt + 1, first_gt - first_lt - 1);
      string endtag("</");
      endtag += tag + ">";
      int last_endtag = text.rfind(endtag);
      if (last_endtag == text.length() - endtag.length())
        // recursively process
        return Ssml::stripTags(text.substr(first_gt + 1,
                           last_endtag - first_gt - 1));
    }
  }

  first_lt = text.find("<mark");
  if (first_lt >= 0) {
    first_gt = text.find("/>");
    if (first_gt > first_lt) {
      return Ssml::stripTags(text.substr(0, first_lt) + text.substr(first_gt + 2, text.length() - first_gt - 2));
    }
  }

  return text;
}

// sample: <audio src=\"%s%s\">%s</audio>
bool Ssml::isAudio(const string& text) {
  int p = text.find("<audio src=");
	return text.find("<audio src=") == 0;
}

string Ssml::getAudioPath(const string& text) {
	int start = text.find('"');
	int end = text.rfind('"');
	if (start > 1 && end > 2 && end > start) {
		return text.substr(start + 1, end - start - 1);
	}

	return "";
}

void Ssml::filterSpaces(string &text) {
  bool changed = false;

  string text2;
  bool in_chinese_context = true;

  int c;
  string::iterator it = text.begin();
  string::iterator it2 = text.begin();
  string::iterator end = text.end();

  while (it != end) {
    it2 = it;
#ifdef DISABLE_EXCEPTIONS
    c = utf8::next(it, end);
#else
    try {
      c = utf8::next(it, end);
    } catch (utf8::not_enough_room &) {
      text = text2;
      return;
    } catch (utf8::invalid_utf8 &) {
      cerr << "Invalid UTF8 encoding" << endl;
      text = text2;
      return;
    }
#endif

    if (in_chinese_context && (c == 32 || c == 12288)) {
      changed = true;
    } else {
      while (it2 != it) {
        text2.push_back(*it2);
        it2++;
      }
      in_chinese_context = (c > 128);
    }

    while (it2 != it) it2++;
  }

  if (changed) {
    text = text2;
  }
}