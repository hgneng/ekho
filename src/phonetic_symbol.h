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
#ifndef PHONETIC_SYMBOL
#define PHONETIC_SYMBOL

//#define DEBUG_ANDROID
#ifdef DEBUG_ANDROID
#include "Log.h"
#define LOG_TAG "Ekho Engine"
#endif

#include <iostream>
#include <sndfile.h>
#include <string.h>

using namespace std;

namespace ekho {
  typedef struct SymbolCode {
    const char* name;
    unsigned short code;
  } SymbolCode;

  class PhoneticSymbol {
    public:
      static bool debug;
      PhoneticSymbol(void): symbol(0), offset(0), bytes(0), mPcm(0), mSize(0) {};
      PhoneticSymbol(const char *sym):
        symbol(sym), offset(0), bytes(0), mPcm(0), mSize(0) {};
      PhoneticSymbol(const char *sym, unsigned int off, unsigned short b):
        symbol(sym), offset(off), bytes(b), mPcm(0), mSize(0) {};
      /*
      PhoneticSymbol(const PhoneticSymbol &ps) {
        symbol = ps.symbol; // copy the content, or there will be double free
        wavFile = ps.wavFile;
        mPcm = ps.getPcm(mSize);
      }*/

      ~PhoneticSymbol(void) {
        /*
        if (symbol) {
          delete symbol;
          symbol = 0;
        }*/

        if (mPcm) {
          delete mPcm;
          mPcm = 0;
        }
      };

      const char* symbol;
      unsigned int offset; // bytes' offset
      unsigned int bytes;

      void setPcm(char* pcm, const int size) {
        if (mPcm) {
          delete mPcm;
        }
        mPcm = pcm;
        mSize = size;
      };

      inline const char* getPcm(int& size) {
        return getPcm("", "wav", size);
      };

      inline const char* getPcm(const char* wavDir,
          const char* postfix, int& size) {
        SF_INFO sfinfo;
        return getPcm(wavDir, postfix, size, sfinfo);
      };

      const char* getPcm(FILE* file, int &size);
      const char* getPcm(const char* wavDir, const char* postfix, int& size, SF_INFO &sfinfo);

      void readSndfile(SNDFILE *sndfile, SF_INFO sfinfo);
 
      static PhoneticSymbol* getUnknownPhoneticSymbol() {
        static PhoneticSymbol* ps = new PhoneticSymbol(" ");
        return ps;
      };

    private:
      char* mPcm;
      int mSize;
  };
}
#endif
