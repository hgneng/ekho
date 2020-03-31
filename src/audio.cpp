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

#include <iostream>
#include <string>
#include <sndfile.h>
#include "audio.h"
using namespace std;
using namespace ekho;

void Audio::play(const string& path) {
  //cerr << "Audio::play:" << path << endl;

  SF_INFO sfinfo;
  SNDFILE *sndfile = sf_open(path.c_str(), SFM_READ, &sfinfo);
  if (!sndfile) {
    return;
  }

  if (sfinfo.frames <= 0) {
    sf_close(sndfile);
    return;
  }

  short *pcm = new short[sfinfo.frames];
  sf_readf_short(sndfile, pcm, sfinfo.frames);

  if (this->speechdSynthCallback) {
    // @TODO: read real bits
    speechdSynthCallback(pcm, sfinfo.frames, 16, sfinfo.channels, sfinfo.samplerate, 0);
#ifdef HAVE_PULSEAUDIO
  } else if (this->pulseAudio) {
    int error;
    //cerr << "pa_simple_write" << endl;
    int ret = pa_simple_write(this->pulseAudio, pcm, sfinfo.frames * 2, &error);
    if (ret < 0) {
      cerr << "pa_simple_write failed: " << pa_strerror(error) << endl;
    }
    ret = pa_simple_drain(this->pulseAudio, &error);
    if (ret < 0) {
      cerr << "pa_simple_drain failed: " << pa_strerror(error) << endl;
    }
  } else {
    cerr << "pulseAudio not inited: audio=%p, pulseAudio=%p" << this << this->pulseAudio << endl;    
#endif    
  }

  delete[] pcm;
  sf_close(sndfile);
}
