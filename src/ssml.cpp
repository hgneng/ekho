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
#include "ssml.h"
using namespace std;
using namespace ekho;

/**
 * Filter tags:
 * Ex: <speak><mark name="0:8"/>屏幕阅读器启用。</speak>
 */
string Ssml::stripTags(const string& text) {
  int first_lt = text.find_first_of("<speak");
  int first_gt;
  if (first_lt == 0) {
    first_gt = text.find_first_of('>');
    if (first_gt > 0) {
      string tag = text.substr(first_lt + 1, first_gt - first_lt - 1);
      string endtag("</");
      endtag += tag + ">";
      int last_endtag = text.find_last_of(endtag);
      if (last_endtag == text.length() - 1)
        // recursively process
        return Ssml::stripTags(text.substr(first_gt + 1,
                           last_endtag - first_gt - endtag.length()));
    }
  }

  first_lt = text.find_first_of("<mark");
  if (first_lt == 0) {
    first_gt = text.find_first_of("/>");
    if (first_gt > first_lt) {
      return text.substr(first_gt + 2, text.length() - first_gt - 2);
    }
  }

  return text;
}

// sample: <audio src=\"%s%s\">%s</audio>
bool Ssml::isAudio(const string& text) {
	return text.find_first_of("<audio src=") == 0;
}

string Ssml::getAudioPath(const string& text) {
	int start = text.find_first_of('"');
	int end = text.find_last_of('"');
	if (start > 1 && end > 2 && end > start) {
		return text.substr(start + 1, end - start - 1);
	}

	return "";
}
