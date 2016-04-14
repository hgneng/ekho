;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                       ;;
;;                Centre for Speech Technology Research                  ;;
;;                     University of Edinburgh, UK                       ;;
;;                       Copyright (c) 1996,1997                         ;;
;;                        All Rights Reserved.                           ;;
;;                                                                       ;;
;;  Permission is hereby granted, free of charge, to use and distribute  ;;
;;  this software and its documentation without restriction, including   ;;
;;  without limitation the rights to use, copy, modify, merge, publish,  ;;
;;  distribute, sublicense, and/or sell copies of this work, and to      ;;
;;  permit persons to whom this work is furnished to do so, subject to   ;;
;;  the following conditions:                                            ;;
;;   1. The code must retain the above copyright notice, this list of    ;;
;;      conditions and the following disclaimer.                         ;;
;;   2. Any modifications must be clearly marked as such.                ;;
;;   3. Original authors' names are not deleted.                         ;;
;;   4. The authors' names are not used to endorse or promote products   ;;
;;      derived from this software without specific prior written        ;;
;;      permission.                                                      ;;
;;                                                                       ;;
;;  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        ;;
;;  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      ;;
;;  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   ;;
;;  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     ;;
;;  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    ;;
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ;;
;;  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ;;
;;  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ;;
;;  THIS SOFTWARE.                                                       ;;
;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  A definition of the Holmes phone set used by the Donovan LPC 
;;  diphone synthesizer, the rest of the synthesis process will 
;;  typically use mrpa phones and map to these.
;;
;;  Hmm not sure I've got the right mapping (as usual)

(defPhoneSet
  holmes
  ;;;  Phone Features
  (;; vowel or consonant
   (vc + -)  
   ;; vowel length: short long dipthong schwa
   (vlng s l d a 0)
   ;; vowel height: high mid low
   (vheight 1 2 3 - 0)
   ;; vowel frontness: front mid back
   (vfront 1 2 3 - 0)
   ;; lip rounding
   (vrnd + - 0)
   ;; consonant type: stop fricative affricative nasal lateral approximant
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
   (ee  +   l   1   1   -   0   0   0) ;; beet
   (i   +   s   1   1   -   0   0   0) ;; bit
   (ai  +   d   2   1   -   0   0   0) ;; gate
   (e   +   s   2   1   -   0   0   0) ;; get
   (aa  +   s   3   1   -   0   0   0) ;; fat
   (ar  +   l   3   3   -   0   0   0) ;; father
   (aw  +   l   3   3   +   0   0   0) ;; lawn
   (oa  +   d   2   2   -   0   0   0) ;; lone
   (oo  +   s   1   3   +   0   0   0) ;; full
   (uu  +   l   1   3   +   0   0   0) ;; fool
   (o   +   s   2   3   +   0   0   0)
   (er  +   l   2   2   -   0   0   0) ;; murder
   (a   +   a   2   2   -   0   0   0) ;; about
   (u   +   s   2   3   -   0   0   0) ;; but
   (ie  +   d   3   2   -   0   0   0) ;; hide
   (ou  +   d   3   2   +   0   0   0) ;; how
   (oi  +   d   3   3   +   0   0   0) ;; toy
   (eer +   d   2   1   -   0   0   0)
   (air +   d   1   1   -   0   0   0)
   (oor +   d   3   1   +   0   0   0)
;;   (yu  +   l   2   3   +   0   0   +) ;; you ???

   (p   -   0   0   0   0   s   l   -)
   (b   -   0   0   0   0   s   l   +)
   (t   -   0   0   0   0   s   a   -)
   (d   -   0   0   0   0   s   a   +)
   (k   -   0   0   0   0   s   v   -)
   (g   -   0   0   0   0   s   v   +)
   (f   -   0   0   0   0   f   b   -)
   (v   -   0   0   0   0   f   b   +)
   (th  -   0   0   0   0   f   d   -)
   (dh  -   0   0   0   0   f   d   +)
   (s   -   0   0   0   0   f   a   -)
   (z   -   0   0   0   0   f   a   +)
   (sh  -   0   0   0   0   f   p   -)
   (zh  -   0   0   0   0   f   p   +)
   (h  -   0   0   0   0   f   g   -)
   (m   -   0   0   0   0   n   l   +)
   (n   -   0   0   0   0   n   a   +)
   (ng  -   0   0   0   0   n   v   +)
   (ch  -   0   0   0   0   a   p   -)
   (j   -   0   0   0   0   a   p   +)
   (l   -   0   0   0   0   l   a   +)
   (w   -   0   0   0   0   r   l   +)
   (y   -   0   0   0   0   r   p   +)
   (r   -   0   0   0   0   r   a   +)
;;   (wh  -   0   -   -   +   l   l   -) ;; ??
;;   (wh  -   0   -   -   +   l   l   +) ;; map to w
   (# -   0   0   0   0   0   0   -)
   )
  )

(PhoneSet.silences '(#))

(provide 'holmes_phones)
