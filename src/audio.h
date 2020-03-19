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

#ifndef EKHO_AUDIO_H
#define EKHO_AUDIO_H
#include <string>
#include "ekho_typedef.h"
#include "config.h"

#ifdef HAVE_PULSEAUDIO
#include <pulse/error.h>
#include <pulse/simple.h>
#endif

using namespace std;

namespace ekho {
class Audio {
  public:
    SpeechdSynthCallback *speechdSynthCallback = 0;

#ifdef HAVE_PULSEAUDIO
    pa_simple *pulseAudio = 0;
#endif

    void play(const string& path);
};
}
#endif