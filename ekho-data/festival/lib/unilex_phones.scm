;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
;;;                       Copyright (c) 2003, 2004                        ;;
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
;;; unilex phoneset
;;;


(defPhoneSet
  unilex
  ;;;  Phone Features
  (;; vowel or consonant
   (vc + -)  
   ;; vowel length: short long dipthong schwa
   (vlng s l d a 0)
   ;; vowel height: high mid low
   (vheight 1 2 3 0)
   ;; vowel frontness: front mid back
   (vfront 1 2 3 0)
   ;; lip rounding
   (vrnd + - 0)
   ;; consonant type: stop fricative affricative nasal liquid approximant
   (ctype s f a n l t r 0)
   ;; place of articulation: labial alveolar palatal labio-dental
   ;;                         dental velar glottal
   (cplace l a p b d v g 0)
   ;; consonant voicing
   (cvox + - 0)
   )
  (
   (SIL  - 0 0 0 0 0 0 -)  ;; slience ... 
   (#  - 0 0 0 0 0 0 -)  ;; slience ... 
   (B_10  - 0 0 0 0 0 0 -)  ;; Pauses
   (B_20 - 0 0 0 0 0 0 -)  ;; Pauses
   (B_30 - 0 0 0 0 0 0 -)  ;; Pauses
   (B_40 - 0 0 0 0 0 0 -)  ;; Pauses
   (B_50 - 0 0 0 0 0 0 -)  ;; Pauses
   (B_100 - 0 0 0 0 0 0 -)  ;; Pauses
   (B_150 - 0 0 0 0 0 0 -)  ;; Pauses
   (B_200 - 0 0 0 0 0 0 -)  ;; Pauses
   (B_250 - 0 0 0 0 0 0 -)  ;; Pauses
   (B_300 - 0 0 0 0 0 0 -)  ;; Pauses
   (B_400 - 0 0 0 0 0 0 -)  ;; Pauses
   (IGNORE - 0 0 0 0 0 0 -)  ;; Pauses


   ;; insert the phones here, see examples in 
   ;; festival/lib/*_phones.scm

   ;(name vc vling vheight vfront vrnd ctype cplace cvox)

   ;;; Rob guesed these values for Edinburgh English
   ;;; Not to be taken too seriously.

   (p    -   0   0   0   0   s   l   -)
   (t    -   0   0   0   0   s   a   -)
   (?    -   0   0   0   0   s   g   +) ;;; ???
   (t^   -   0   0   0   0   t   a   +) ;;; ???
   (k    -   0   0   0   0   s   v   -)
   (x    -   0   0   0   0   f   v   -)
   (b    -   0   0   0   0   s   l   +)
   (d    -   0   0   0   0   s   a   +)
   (g    -   0   0   0   0   s   v   +)
   (ch   -   0   0   0   0   a   p   -)
   (jh   -   0   0   0   0   a   p   +)
   (s    -   0   0   0   0   f   a   -)
   (z    -   0   0   0   0   f   a   +)
   (sh   -   0   0   0   0   f   p   -)
   (zh   -   0   0   0   0   f   p   +)
   (f    -   0   0   0   0   f   b   -)
   (v    -   0   0   0   0   f   b   +)
   (th   -   0   0   0   0   f   d   -)
   (dh   -   0   0   0   0   f   d   +)
   (h    -   0   0   0   0   f   0   -) ;;; ???
   (m    -   0   0   0   0   n   l   +)
   (m!   -   0   0   0   0   n   l   +)
   (n    -   0   0   0   0   n   a   +)
   (n!   -   0   0   0   0   n   a   +)
   (ng   -   0   0   0   0   n   v   +)
   (l    -   0   0   0   0   r   a   +)
   (ll   -   0   0   0   0   r   a   +)
   (lw   -   0   0   0   0   r   a   +)
   (l!   -   0   0   0   0   r   a   +)
   (r    -   0   0   0   0   r   a   +)
   (y    -   0   0   0   0   l   p   +)
   (w    -   0   0   0   0   l   l   +)
   (hw   -   0   0   0   0   l   l   +)
   (e    +   s   2   1   -   0   0   0)
   (ao   +   s   3   1   -   0   0   0)
   (a    +   s   3   1   -   0   0   0)
   (ah   +   s   3   1   -   0   0   0)
   (oa   +   s   3   1   -   0   0   0)
   (aa   +   s   3   1   -   0   0   0)
   (ar   +   s   3   1   -   0   0   0)
   (eh   +   s   3   1   -   0   0   0)   ;;; ?
   (oul  +   d   2   3   +   0   0   0)   ;;; ?
   (ou   +   d   2   3   +   0   0   0)
   (ouw  +   d   2   3   +   0   0   0)
   (oou  +   l   3   3   +   0   0   0)
   (o    +   l   3   3   +   0   0   0)
   (au   +   l   3   3   +   0   0   0)
   (oo   +   l   3   3   +   0   0   0)
   (or   +   l   3   3   +   0   0   0)
   (our  +   d   2   3   +   0   0   0)
   (ii   +   l   1   1   -   0   0   0)
   (ihr  +   s   1   1   -   0   0   0)
   (iy   +   l   1   1   -   0   0   0)
   (i    +   s   1   1   -   0   0   0)
   (ie   +   l   1   1   -   0   0   0)   ;;; ?
   (iii  +   s   1   1   -   0   0   0)   ;;; was ii;
   (@r   +   a   2   2   -   r   a   +)
   (@    +   a   2   2   -   0   0   0)
   (uh   +   s   2   2   -   0   0   0) 
   (uhr  +   s   2   2   -   0   0   0)
   (u    +   l   1   3   +   0   0   0)
   (uu   +   l   1   3   +   0   0   0)
   (iu   +   l   1   3   +   0   0   0)
   (uuu  +   l   1   3   +   0   0   0)   ;;; was uu;
   (uw   +   l   1   3   +   0   0   0)   ;;; ???
   (uul  +   l   1   3   +   0   0   0)   ;;; ???
   (ei   +   d   2   1   -   0   0   0)
   (ee   +   d   2   1   -   0   0   0)
   (ai   +   d   3   2   -   0   0   0)    ;;; ???
   (ae   +   d   3   2   -   0   0   0)    ;;; ???
   (aer  +   d   3   2   -   0   0   0)    ;;; ???
   (aai  +   d   3   2   -   0   0   0)    ;;; ???
   (oi   +   d   2   3   +   0   0   0)    ;;; ???
   (oir  +   d   2   3   +   0   0   0)    ;;; ???
   (ow   +   d   3   2   -   0   0   0)
   (owr  +   d   3   2   -   0   0   0)    ;;; ???
   (oow  +   d   3   2   -   0   0   0)    ;;; ???
   (i@   +   l   1   1   -   0   0   0)    ;;; iy + @ ?
   (ir   +   s   1   1   -   0   0   0)
   (irr  +   s   1   1   -   0   0   0)    ;;; was ir;
   (iir  +   s   1   1   -   0   0   0)
   (@@r  +   a   2   2   -   0   0   0)
   (er   +   s   2   1   -   0   0   0)
   (eir  +   s   2   1   -   0   0   0)    ;;; ???
   (ur   +   s   1   3   +   0   0   0)    ;;; ???
   (urr  +   s   1   3   +   0   0   0)    ;;; ???
   (iur  +   s   1   3   +   0   0   0)    ;;; ???
  )
)

(PhoneSet.silences '( # SIL))

(define (unilex::select_phoneset)
  "(unilex::select_phoneset)
Set up phone set for unilex"
  (Parameter.set 'PhoneSet 'unilex)
  (PhoneSet.select 'unilex)
)

(define (unilex::reset_phoneset)
  "(unilex::reset_phoneset)
Reset phone set for unilex."
  t
)

(provide 'unilex_phones)
