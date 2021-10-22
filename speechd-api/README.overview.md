Overview of Speech Dispatcher
=============================

Key features:
-------------

  * Common interface to different Text To Speech (TTS) engines
  * Handling concurrent synthesis requests — requests may come asynchronously
  from multiple sources within an application and/or from different applications
  * Subsequent serialization, resolution of conflicts and priorities of incoming
  requests
  * Context switching — state is maintained for each client connection
  independently, event for connections from within one application
  * High-level client interfaces for popular programming languages
  * Common sound output handling — audio playback is handled by Speech
  Dispatcher rather than the TTS engine, since most engines have limited sound
  output capabilities

What is a very high level GUI library to graphics, Speech Dispatcher is to
speech synthesis. The application neither needs to talk to the devices directly
nor to handle concurrent access, sound output and other tricky aspects of the
speech subsystem.

Supported TTS engines:
----------------------

  * Festival
  * Flite
  * Espeak
  * Cicero
  * IBM TTS
  * Espeak+MBROLA (through a generic driver)
  * Epos (through a generic driver)
  * DecTalk software (through a generic driver)
  * Cepstral Swift (through a generic driver)
  * Ivona
  * Pico (possibly through a generic driver)
  * Espeak NG
  * Kali TTS
  * Baratinoo (Voxygen)
  * Mary-TTS

Supported sound output subsystems:
----------------------------------

  * OSS
  * ALSA
  * PulseAudio
  * NAS
  * Libao

The architecture is based on a client/server model. The clients are all the
applications in the system that want to produce speech (typically assisting
technologies). The basic means of client communication with Speech Dispatcher
is through a Unix socket or Inet TCP connection using the Speech Synthesis
Interface Protocol (See the SSIP documentation for more information). High-level
client libraries for many popular programming languages implement this protocol
to make its usage as simple as possible.

Supported client interfaces:
----------------------------

  * C/C++ API
  * Python 3 API
  * Java API
  * Emacs Lisp API
  * Common Lisp API
  * Guile API
  * Simple command line client

A golang API is also available on https://github.com/ilyapashuk/go-speechd

Existing assistive technologies known to work with Speech Dispatcher:

  * speechd-el (see https://devel.freebsoft.org/speechd-el)
  * Orca (see http://live.gnome.org/Orca/SpeechDispatcher)
  * Yasr (see http://yasr.sourceforge.net/)
  * BrlTTY (see http://brltty.com)
  * Chromevox (extension of the Chrome and Chromium browsers)

Voices settings
---------------
The available voices depend on the TTS engines and voices installed.

The voice to use can be set in speech-dispatcher itself, at the system and user
level, and from the client application, like Orca, speechd-el or Chromevox.
The settings in each application and in speech dispatcher are independent of
each others.

The settings in speech-dispatcher at the user level override those
made at the system level.


In speech-dispatcher, the system settings are recorded in the file
/etc/speech-dispatcher/speechd.conf among which a default synthesizer, a voice
type or symbolic name (e.g. MALE1) and a default language.

In turn, each installed voice is associated to a voice type and a language, thus
with this default setting a voice available with these characteristics (voice
type, language, synthesizer) will be chosen if available.


The default values of theses voices parameters can also be set at the system
level customized at the user level: rate, pitch, pitch range and volume.

It is also possible to make the synthesizer depend on the language used.


The user settings are written in the file ~/.config/speech-dispatcher/spd.conf
using the application spd-conf, which also allows to modify the system settings.

spd-conf allows to set the synthesizer, the language and other voice parameters
but not select directly a specific voice. 


Instead a specific voice can be chosen from the client application, selecting it
by name in a proposed list that depends on the synthesizer chosen.

The voice name can be a first name like 'bob' or 'viginie", a locale code in the
form language_COUNTRY or a language code followed by a number, for instance.

The language code associated to each name is listed alongside it between
parenthesis, like (fr-FR) for French from France.

Where to look at in case of a sound or speech issue
---------------------------------------------------

Speech dispatcher links together all the components that contribute to speak a
text, so if you don't get speech at all or something is not spoken, or not the
way you expect, this can come from speech dispatcher itself or from any of those
components (or lack of) and their settings:
- the audio subsystem in use, e.g. alsa or pulseaudio,
- the synthesizer in use, e.g espeak-ng or pico,
- the client application, like Orca or speechd-el or an underlying software like
  at-spi,
- the application that provides the text to be spoken, like Firefox.

How to investigate a specific issue goes far beyond this document, but bear in
mind that all listed components can be involved, as the audio equipment in use
and the way it is linked to the computer.


Copyright (C) 2001-2009 Brailcom, o.p.s
Copyright (C) 2018-2020 Samuel Thibault <samuel.thibault@ens-lyon.org>
Copyright (C) 2018 Didier Spaier <didier@slint.fr>
Copyright (C) 2018 Alex ARNAUD <alexarnaud@hypra.fr>

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details (file
COPYING in the root directory).

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
