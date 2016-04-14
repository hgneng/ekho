;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                     Carnegie Mellon University                      ;;;
;;;                  and Alan W Black and Kevin Lenzo                   ;;;
;;;                      Copyright (c) 1998-2000                        ;;;
;;;                        All Rights Reserved.                         ;;;
;;;                                                                     ;;;
;;; Permission is hereby granted, free of charge, to use and distribute ;;;
;;; this software and its documentation without restriction, including  ;;;
;;; without limitation the rights to use, copy, modify, merge, publish, ;;;
;;; distribute, sublicense, and/or sell copies of this work, and to     ;;;
;;; permit persons to whom this work is furnished to do so, subject to  ;;;
;;; the following conditions:                                           ;;;
;;;  1. The code must retain the above copyright notice, this list of   ;;;
;;;     conditions and the following disclaimer.                        ;;;
;;;  2. Any modifications must be clearly marked as such.               ;;;
;;;  3. Original authors' names are not deleted.                        ;;;
;;;  4. The authors' names are not used to endorse or promote products  ;;;
;;;     derived from this software without specific prior written       ;;;
;;;     permission.                                                     ;;;
;;;                                                                     ;;;
;;; CARNEGIE MELLON UNIVERSITY AND THE CONTRIBUTORS TO THIS WORK        ;;;
;;; DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING     ;;;
;;; ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT  ;;;
;;; SHALL CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE     ;;;
;;; FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   ;;;
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  ;;;
;;; AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,         ;;;
;;; ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF      ;;;
;;; THIS SOFTWARE.                                                      ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  A definition of the cmusphinx2 phone set used in the BU RADIO FM
;;;  corpus, some people call this the darpa set.  This one
;;;  has the closures removed
;;;

(defPhoneSet
  cmusphinx2
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
   ;; consonant type: stop fricative affricate nasal lateral approximant
   (ctype s f a n l r 0)
   ;; place of articulation: labial alveolar palatal labio-dental
   ;;                         dental velar glottal
   (cplace l a p b d v g 0)
   ;; consonant voicing
   (cvox + - 0)
   )
  ;; Phone set members
  (

   ;; Note these features were set by awb so they are wrong !!!

; phone vc  vl  vh  vf  vr  ct  cp  cv
   (AA  +   l   3   3   -   0   0   0) ;; father
   (AE  +   s   3   1   -   0   0   0) ;; fat
   (AH  +   s   2   2   -   0   0   0) ;; but
   (AO  +   l   3   3   +   0   0   0) ;; lawn
   (AW  +   d   3   2   -   0   0   0) ;; how
   (AX  +   a   2   2   -   0   0   0) ;; about
   (AXR +   a   2   2   -   r   a   +)
   (AY  +   d   3   2   -   0   0   0) ;; hide
   (B   -   0   0   0   0   s   l   +)
   (CH  -   0   0   0   0   a   p   -)
   (D   -   0   0   0   0   s   a   +)
   (DH  -   0   0   0   0   f   d   +)
   (DX  -   0   0   0   0   s   a   +)
   (EH  +   s   2   1   -   0   0   0) ;; get
   (ER  +   a   2   2   -   r   0   0) 
   (EY  +   d   2   1   -   0   0   0) ;; gate
   (F   -   0   0   0   0   f   b   -)
   (G   -   0   0   0   0   s   v   +)
   (HH  -   0   0   0   0   f   g   -)
   (IH  +   s   1   1   -   0   0   0) ;; bit
   (IY  +   l   1   1   -   0   0   0) ;; beet
   (JH  -   0   0   0   0   a   p   +)
   (K   -   0   0   0   0   s   v   -)
   (L   -   0   0   0   0   l   a   +)
   (M   -   0   0   0   0   n   l   +)
   (N   -   0   0   0   0   n   a   +)
   (NG  -   0   0   0   0   n   v   +)
   (OW  +   d   2   3   +   0   0   0) ;; lone
   (OY  +   d   2   3   +   0   0   0) ;; toy
   (P   -   0   0   0   0   s   l   -)
   (R   -   0   0   0   0   r   a   +)
   (S   -   0   0   0   0   f   a   -)
   (SH  -   0   0   0   0   f   p   -)
   (T   -   0   0   0   0   s   a   -)
   (TH  -   0   0   0   0   f   d   -)
   (UH  +   s   1   3   +   0   0   0) ;; full
   (UW  +   l   1   3   +   0   0   0) ;; fool
   (V   -   0   0   0   0   f   b   +)
   (W   -   0   0   0   0   r   l   +)
   (Y   -   0   0   0   0   r   p   +)
   (Z   -   0   0   0   0   f   a   +)
   (ZH  -   0   0   0   0   f   p   +)
   (SIL -   0   0   0   0   0   0   -) ; added
  )
)

(PhoneSet.silences '(SIL))

(provide 'cmusphinx2_phones)


  

