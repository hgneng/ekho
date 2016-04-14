/***************************************************************************
 * Copyright (C) 2008-2013 by Cameron Wong                                 *
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
#ifndef CHARACTER
#define CHARACTER

#include <string>
#include <list>
#include "phonetic_symbol.h"
#include "utf8.h"
using namespace std;

namespace ekho {
  class Character {
    public:
      Character(void): code(0), phonSymbol(0) { };
      Character(unsigned int code): code(code), phonSymbol(0) {
      };
      Character(const Character &c): code(c.code), phonSymbol(c.phonSymbol) {
      };
      Character(unsigned int code, PhoneticSymbol *symbol):
        code(code),
        phonSymbol(symbol) {};
      Character(string &utf8, PhoneticSymbol *symbol):
        phonSymbol(symbol) {
        setUtf8(utf8);
      };

      unsigned int code;
      PhoneticSymbol *phonSymbol;

      string getUtf8(void) {
        char buf[5] = {0};
#ifdef DISABLE_EXCEPTIONS
        utf8::append(code, buf);
#else
        try {
          utf8::append(code, buf);
        } catch (...) {
          cerr << "code point:" << code << endl;
        }
#endif
        return string(buf);
      };

      void setUtf8(string &utf8) {
        code = utf8::peek_next(utf8.begin(), utf8.end());
      };

      static string join(list<Character> &charlist) {
        string s;
        list<Character>::iterator itor = charlist.begin();
        for (; itor != charlist.end(); itor++) {
          s += itor->getUtf8();
        }

        return s;
      }

      static list<Character> split(string &text, int charLimit = 0) {
        list<Character> char_list;
        int c;
        bool is_finish = false;
        string::iterator it = text.begin();
        string::iterator end = text.end();
        int count = 0;

        while (!is_finish && it != end) {
#ifdef DISABLE_EXCEPTIONS
          c = utf8::next(it, end);
          char_list.push_back(c);
#else
          try {
            c = utf8::next(it, end);
            char_list.push_back(c);
          } catch (utf8::not_enough_room &) {
            is_finish = true;
          } catch (utf8::invalid_utf8 &) {
            cerr << "Invalid UTF8 encoding" << endl;
            is_finish = true;
          }
#endif
          count++;
          if (charLimit > 0 && count >= charLimit)
            break;
        }

        return char_list;
      };
  };
}

#endif
