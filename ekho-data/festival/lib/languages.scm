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
;;;  Specification of voices and some major choices of synthesis
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  This should use some sort of database description for voices so
;;;  new voices will become automatically available.
;;;

(define (language_british_english)
"(language_british_english)
Set up language parameters for British English."
  (require 'voices)
  ;;  Will get more elaborate, with different choices of voices in language

  (set! male1 voice_rab_diphone)
  (set! male2 voice_don_diphone)
  (if (symbol-bound? 'voice_gsw_diphone)
      (set! male3 voice_gsw_diphone))
  (if (symbol-bound? 'voice_gsw_450)
      (set! male4 voice_gsw_450))

  (male1)
  (Parameter.set 'Language 'britishenglish)
)

(define (language_american_english)
"(language_american_english)
Set up language parameters for Aemerican English."

  (if (symbol-bound? 'voice_kal_diphone)
      (set! female1 voice_kal_diphone))
  (set! male1 voice_ked_diphone)

  (male1)
  (Parameter.set 'Language 'americanenglish)
)

(define (language_scots_gaelic)
"(language_scots_gaelic)
Set up language parameters for Scots Gaelic."
  (error "Scots Gaelic not yet supported.")

  (Parameter.set 'Language 'scotsgaelic)
)

(define (language_welsh)
"(language_welsh)
Set up language parameters for Welsh."

  (set! male1 voice_welsh_hl)

  (male1)
  (Parameter.set 'Language 'welsh)
)

(define (language_castillian_spanish)
"(language_spanish)
Set up language parameters for Castillian Spanish."

  (voice_el_diphone)
  (set! male1 voice_el_diphone)

  (Parameter.set 'Language 'spanish)
)

(define (select_language language)
  (cond
   ((or (equal? language 'britishenglish)
	(equal? language 'english))  ;; we all know its the *real* English
    (language_british_english))
   ((equal? language 'americanenglish)
    (language_american_english))
   ((equal? language 'scotsgaelic)
    (language_scots_gaelic))
   ((equal? language 'welsh)
    (language_welsh))
   ((equal? language 'spanish)
    (language_castillian_spanish))
   ((equal? language 'klingon)
    (language_klingon))
   (t
    (print "Unsupported language, using English")
    (language_british_english))))

(defvar language_default language_british_english)

(provide 'languages)
