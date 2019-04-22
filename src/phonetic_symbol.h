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
    const char *name;
    unsigned short code;
  } SymbolCode;

  class PhoneticSymbol {
    public:
      PhoneticSymbol(void): symbol(0), mPcm(0), mSize(0), offset(0), bytes(0) {};
      PhoneticSymbol(const char *sym):
        symbol(sym), mPcm(0), mSize(0), offset(0), bytes(0) {};
      PhoneticSymbol(const char *sym, unsigned int off, unsigned short b):
        symbol(sym), mPcm(0), mSize(0), offset(off), bytes(b) {};
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
          delete[] mPcm;
          mPcm = 0;
        }
      };

      const char *symbol;
      unsigned int offset; // bytes' offset
      unsigned short bytes;

      void setPcm(char *pcm, const int size) {
        if (mPcm) {
          delete mPcm;
        }
        mPcm = pcm;
        mSize = size;
      };

      inline const char* getPcm(int &size) {
        return getPcm("", "wav", size);
      };

      inline const char* getPcm(const char *wavDir, const char *postfix, int &size) {
        SF_INFO sfinfo;
        return getPcm(wavDir, postfix, size, sfinfo);
      };

      const char* getPcm(FILE *file, int &size) {
#ifdef DEBUG_ANDROID
        LOGV("getPcm(%p, %d) offset=%d bytes=%d", file, size, offset, bytes);
#endif
        if (!mPcm && fseek(file, offset, SEEK_SET) == 0) {
#ifdef ANDROID
          FILE *gsmfile = fopen("/data/data/net.eguidedog.ekho.cantonese/cache/tmpfile", "wb+");
          if (!gsmfile)
            gsmfile = fopen("/data/data/net.eguidedog.ekho.cantonese/tmpfile", "wb+");
#else
          FILE *gsmfile = tmpfile();
#endif

          if (!gsmfile) {
#ifdef DEBUG_ANDROID
            LOGV("Fail to open /data/data/net.eguidedog.ekho.cantonese/cache/tmpfile");
#endif
            cerr << "Fail to create temparary file." << endl;
            size = 0;
            return 0;
          }
          
          char buffer[128000];
          int b = bytes;
          while (b > 0) {
            if (b <= 128000) {
              fread(buffer, 1, b, file);
              fwrite(buffer, 1, b, gsmfile);
              b = 0;
            } else {
              b -= 128000;
              fread(buffer, 128000, b, file);
              fwrite(buffer, 128000, b, gsmfile);
            }
          }

#ifdef DEBUG_ANDROID
          LOGV("finish writting gsmfile");
#endif

          rewind(gsmfile);
          SF_INFO sfinfo;
          memset(&sfinfo, 0, sizeof(SF_INFO));
          SNDFILE *sndfile = sf_open_fd(fileno(gsmfile), SFM_READ, &sfinfo, 1);
          readSndfile(sndfile, sfinfo);
        }

        size = mSize;
        return mPcm;
      }

      const char* getPcm(const char *wavDir, const char *postfix, int &size, SF_INFO &sfinfo) {
        if (!mPcm) {
          memset(&sfinfo, 0, sizeof(SF_INFO));

          string wav_file = wavDir;
          // char | 32 means get lower case
          if (this->symbol[0] == '\\')
          {
            char c = this->symbol[1] | 32;
            if (c >= 'a' && c <= 'z') {
              wav_file += "/../alphabet/";
              wav_file += c;
              wav_file += ".wav";
            } else {
              return 0;
            }
          } else {
            wav_file += "/";
            wav_file += this->symbol;
            wav_file += ".";
            wav_file += postfix;
          }

          SNDFILE *sndfile = sf_open(wav_file.c_str(), SFM_READ, &sfinfo);
          readSndfile(sndfile, sfinfo);
        }

        size = mSize;
        return mPcm;
      };

      void readSndfile(SNDFILE *sndfile, SF_INFO sfinfo) {
#ifdef DEBUG_ANDROID
                LOGV("readSndfile(%p, %p)", sndfile, &sfinfo);
#endif
          if (!sndfile) {
//            cerr << "Fail to open file " << wav_file << " at " << __LINE__ <<
  //              " of " << __FILE__ << endl;                
          } else {
//            sfinfo.channels = 1; // this->sfinfo.channels is corrupted
            if (sfinfo.channels > 2) {
              static bool show_channel_error = true;
              if (show_channel_error) {
                cerr << "Invalid channels: " << sfinfo.channels << endl;
                show_channel_error = false;
              }

              sfinfo.channels = 1;
            }
            int samples = 0;

            /* sfinfo.channels has not been taken into account .... */
            switch (sfinfo.format & SF_FORMAT_SUBMASK) {
              case SF_FORMAT_VORBIS:
              case SF_FORMAT_GSM610:
              case SF_FORMAT_PCM_16:
                mSize = (int)sfinfo.frames * 2 * sfinfo.channels;
                mPcm = new char[mSize];
                samples = (int)sf_readf_short(sndfile, (short int*)mPcm, sfinfo.frames);
#ifdef DEBUG_ANDROID
                LOGV("read samples: %d, %p", samples, mPcm);
#endif
                break;
              case SF_FORMAT_PCM_S8:
              case SF_FORMAT_PCM_U8:
              default:
                cerr << "Unknown soundfile format: " << (sfinfo.format & SF_FORMAT_SUBMASK) << endl;
            }

            if (samples != sfinfo.frames) {
              cerr << "Fail to read : " << samples <<
                " frames out of " << sfinfo.frames << " have been read." << endl;
            }

            sf_close(sndfile);
          }
      };
 
      static PhoneticSymbol* getUnknownPhoneticSymbol() {
        static PhoneticSymbol *ps = new PhoneticSymbol(" ");
        return ps;
      }

    private:
      char *mPcm;
      int mSize;
  };
}
#endif
