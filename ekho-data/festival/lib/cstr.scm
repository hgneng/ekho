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
;;; CSTR siod extensions.

;(defvar Parameter nil
;  "Parameter
;  An assoc-list of parameters and values for various parts of the speech
;  synthesis system.  This is used by the functions Parameter.set 
;  Parameter.def and Parameter.get as well as internal C++ functions.")

(defvar Param (feats.make)
  "Param
  A feature set for arbitrary parameters for modules.")

(define (Param.set name val)
"(Param.set NAME VAL)
  Set parameter NAME to VAL (deleting any previous setting)"
  (feats.set Param name val))

(define (Parameter.set name val)
"(Parameter.set NAME VAL)
  Set parameter NAME to VAL (deleting any previous setting).  This is
  an old function and you should use Param.set instead."
  (Param.set name val)
  val
  )

(define (Parameter.def name val)
"(Parameter.def NAME VAL)
  Set parameter NAME to VAL if not already set.  This is an OLD function
  you shold use Param.def instead."
   (Param.def name val)
  )

(define (Param.def name val)
"(Param.def NAME VAL)
  Set parameter NAME to VAL if not already set"
   (if (not (feats.present Param name))
       (feats.set Param name val)))

(define (Parameter.get name)
"(Parameter.get NAME)
  Get parameter NAME's value (nil if unset).  This is an OLD function
  and may not exist in later versions (or change functionality).  This
  function (unlike Param.get) may return sylbols (rather than strings
  if the val doesn't contain whitespace (to allow older functions to 
  still work."
   (let ((val (Param.get name)))
     (if (and (eq? 'string (typeof val))
	      (not (string-matches val ".*[ \t\r\n].*")))
	 (intern val)
	 val))
  )

(define (Param.get name)
"(Param.get NAME)
  Get parameter NAME's value (nil if unset)"
  (feats.get Param name))

(define (get_param name params default)
  "(get_param name params default)
Get named parameters in params returning default if its not present."
  (let ((pair (assoc name params)))
    (if pair
	(car (cdr pair))
	default)))

(provide 'cstr)
