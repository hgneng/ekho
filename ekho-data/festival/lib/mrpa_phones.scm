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
;;  A definition of the mrpa phone set 
;;

(defPhoneSet
  mrpa
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
   (uh  +   s   2   3   -   0   0   0)
   (e   +   s   2   1   -   0   0   0)
   (a   +   s   3   1   -   0   0   0)
   (o   +   s   2   3   +   0   0   0)
   (i   +   s   1   1   -   0   0   0)
   (u   +   s   1   3   +   0   0   0)
   (ii  +   l   1   1   -   0   0   0)
   (uu  +   l   1   3   +   0   0   0)
   (oo  +   l   3   3   +   0   0   0)
   (aa  +   l   3   3   -   0   0   0)
   (@@  +   l   2   2   -   0   0   0)
   (ai  +   d   3   2   -   0   0   0)
   (ei  +   d   2   1   -   0   0   0)
   (oi  +   d   3   3   +   0   0   0)
   (au  +   d   3   2   +   0   0   0)
   (ou  +   d   2   2   -   0   0   0)
   (e@  +   d   2   1   -   0   0   0)
   (i@  +   d   1   1   -   0   0   0)
   (u@  +   d   3   1   +   0   0   0)
   (@   +   a   2   2   -   0   0   0)
   (p   -   0   0   0   0   s   l   -)
   (t   -   0   0   0   0   s   a   -)
   (k   -   0   0   0   0   s   v   -)
   (b   -   0   0   0   0   s   l   +)
   (d   -   0   0   0   0   s   a   +)
   (g   -   0   0   0   0   s   v   +)
   (s   -   0   0   0   0   f   a   -)
   (z   -   0   0   0   0   f   a   +)
   (sh  -   0   0   0   0   f   p   -)
   (zh  -   0   0   0   0   f   p   +)
   (f   -   0   0   0   0   f   b   -)
   (v   -   0   0   0   0   f   b   +)
   (th  -   0   0   0   0   f   d   -)
   (dh  -   0   0   0   0   f   d   +)
   (ch  -   0   0   0   0   a   p   -)
   (jh  -   0   0   0   0   a   p   +)
   (h   -   0   0   0   0   f   g   -)
   (m   -   0   0   0   0   n   l   +)
   (n   -   0   0   0   0   n   a   +)
   (ng  -   0   0   0   0   n   v   +)
   (l   -   0   0   0   0   l   a   +)
   (y   -   0   0   0   0   r   p   +)
   (r   -   0   0   0   0   r   a   +)
   (w   -   0   0   0   0   r   l   +)
   (#   -   0   0   0   0   0   0   -)
   )
  )

(PhoneSet.silences '(#))

(provide 'mrpa_phones)




