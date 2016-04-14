;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
;;;                       Copyright (c) 1996,1997                         ;;
;;;                        All Rights Reserved.                           ;;
;;;                                                                       ;;
;;;  Permission is hereby granted, free of charge, to use and distribute  ;;
;;;  this software and its documentation without restriction, including   ;;
;;;  without limitation the rights to use, copy, modify, merge, publish,  ;;
;;;  distribute, sublicense, and/or sell copies of this work, and to      ;;
;;;  permit persons to whom this work is furnished to do so, subject to   ;;
;;;  the following conditions:                                            ;;
;;;   1. The code must retain the above copyright notice, this list of    ;;
;;;      conditions and the following disclaimer.                         ;;
;;;   2. Any modifications must be clearly marked as such.                ;;
;;;   3. Original authors' names are not deleted.                         ;;
;;;   4. The authors' names are not used to endorse or promote products   ;;
;;;      derived from this software without specific prior written        ;;
;;;      permission.                                                      ;;
;;;                                                                       ;;
;;;  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        ;;
;;;  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      ;;
;;;  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   ;;
;;;  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     ;;
;;;  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    ;;
;;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ;;
;;;  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ;;
;;;  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ;;
;;;  THIS SOFTWARE.                                                       ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Initialisation file -- loaded before anything else
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  Basic siod library (need this before load_library or require works)
(load (path-append libdir "siod.scm"))

(defvar home-directory (or (getenv "HOME") "/")
  "home-directory
   Place looked at for .festivalrc etc.")

;;;  User startup initialization, can be used to override load-path
;;;  to allow alternate basic modules to be loaded.
(if (probe_file (path-append home-directory ".siodvarsrc"))
    (load (path-append home-directory ".siodvarsrc")))

(if (probe_file (path-append home-directory ".festivalvarsrc"))
    (load (path-append home-directory ".festivalvarsrc")))

;;;  A chance to set various variables to a local setting e.g.
;;;  lexdir, voices_dir audio etc etc.
(if (probe_file (path-append libdir "sitevars.scm"))
    (load (path-append libdir "sitevars.scm")))

;;; CSTR siod extensions
(require 'cstr)

;;;  Festival specific definitions
(require 'festival)

;;;  Dealing with module descriptions
(require 'module_description)

;;;  Web related definitions
(require 'web)

;;;  Utterance types and support
(require 'synthesis)

;;;  Some default parameters
(Parameter.def 'Wavefiletype 'riff)

;;; Set default audio method
(cond
 ((member 'nas *modules*)
  (Parameter.def 'Audio_Method 'netaudio))
 ((member 'esd *modules*)
  (Parameter.def 'Audio_Method 'esdaudio))
 ((member 'sun16audio *modules*)
  (Parameter.def 'Audio_Method 'sun16audio))
 ((member 'freebsd16audio *modules*)
  (Parameter.def 'Audio_Method 'freebsd16audio))
 ((member 'linux16audio *modules*)
  (Parameter.def 'Audio_Method 'linux16audio))
 ((member 'irixaudio *modules*)
  (Parameter.def 'Audio_Method 'irixaudio))
 ((member 'macosxaudio *modules*)
  (Parameter.def 'Audio_Method 'macosxaudio))
 ((member 'win32audio *modules*)
  (Parameter.def 'Audio_Method 'win32audio))
 ((member 'os2audio *modules*)
  (Parameter.def 'Audio_Method 'os2audio))
 ((member 'mplayeraudio *modules*)
  (Parameter.def 'Audio_Method 'mplayeraudio))
 (t  ;; can't find direct support so guess that /dev/audio for 8k ulaw exists
  (Parameter.def 'Audio_Method 'sunaudio)))
;;;  If you have an external program to play audio add its definition
;;;  in siteinit.scm

;;; The audio spooler doesn't work under Windows so redefine audio_mode
(if (member 'mplayeraudio *modules*)
      (define (audio_mode param) param)
)

;;; Intonation
(require 'intonation)

;;; Duration
(require 'duration)

;;;  A large lexicon
(require 'lexicons)
(require 'pauses)

;;;  Part of speech prediction
(require 'pos)

;;; Phrasing (dependent on pos)
(require 'phrase)

;;; POstlexical rules
(require 'postlex)

;;; Different voices
(require 'voices)  ;; sets voice_default
(require 'languages)

;;; Some higher level functions
(require 'token)
(require 'tts)

;;;
;;;  Local site initialization, if the file exists load it
;;;
(if (probe_file (path-append libdir "siteinit.scm"))
    (load (path-append libdir "siteinit.scm")))

;;;  User initialization, if a user has a personal customization
;;;  file loaded it
(if (probe_file (path-append home-directory ".siodrc"))
    (load (path-append home-directory ".siodrc")))

(if (probe_file (path-append home-directory ".festivalrc"))
    (load (path-append home-directory ".festivalrc")))

;;; Default voice (have to do something cute so autoloads still work)
(eval (list voice_default))

(provide 'init)





