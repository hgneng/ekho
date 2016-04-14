;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                      JUNTA DE ANDALUCÍA                             ;;;
;;;                      Copyright (c) 2007                             ;;;
;;;                      All Rights Reserved.                           ;;;
;;;                                                                     ;;;
;;;  Distribution policy.                                               ;;;
;;;                                                                     ;;;
;;;  Free for any use.                                                  ;;;
;;;                                                                     ;;;
;;;  All the work is based on the Festvox Toolkit, provided by:         ;;;
;;;    - Carnegie Mellon University (http://www.festvox.org)            ;;;
;;;                                                                     ;;;
;;;  The copyright below belongs to the original Festvox project; it    ;;;
;;;  therefore applies to the present work.                             ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;  Standard Spanish female voice                                      ;;;
;;;                                                                     ;;;
;;;  Contractor: Consejería de Innovación, Ciencia y Empresa            ;;;
;;;              de la Junta de Andalucía                               ;;;
;;;                                                                     ;;;
;;;  Developed by: MP Sistemas and                                      ;;;
;;;                Intelligent Dialogue Systems S.L. (INDISYS)          ;;;
;;;                                                                     ;;;
;;;  Authors:   Del Solar, Carmen <c.delsolar@indisys.es>               ;;;
;;;             González, Jesús   <j.gonzalez@indisys.es>               ;;;
;;;             Manchón, Pilar    <p.manchon@indisys.es>                ;;;
;;;             Martín, Antonio   <amam@mpsistemas.es>                  ;;;
;;;             Martínez, Diego   <d.martinez@indisys.es>               ;;;
;;;             Pérez, Guillermo  <g.perez@indisys.es>                  ;;;
;;;             Varela, Víctor    <vmvr@mpsistemas.es>                  ;;;
;;;                                                                     ;;;
;;;  Voice Talent:  Silvia Fernández                                    ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;
;;; Phoneset for JuntaDeAndalucia_es
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defPhoneSet
  JuntaDeAndalucia_es
  ;;;  Phone Features
  (;; vowel or consonant
   (vc + -)  
   ;; vowel length: short long diphthong schwa
   (vlng s l d a 0)
   ;; vowel height: high mid low
   (vheight 1 2 3 -)
   ;; vowel frontness: front mid back
   (vfront 1 2 3 -)
   ;; lip rounding
   (vrnd + -)
   ;; consonant type: stop fricative affricative nasal liquid lateral
   (ctype s f a n l t 0)
   ;; place of articulation: labial alveolar palatal labio-dental
   ;;                         dental velar
   (cplace l a p b d v 0)
   ;; consonant voicing
   (cvox + -)
   )

  ;; [CSV] Indisys-MP extra phonemes added (wrt el_diphone): Spanish (B,D,G) and English (ax,dh,hh,sh,v,z,zh)
  ;; Spanish B,D,G are now predicted through letter-to-sound rules
  ;; English ax,dh,hh,sh,v,zh are only triggered through the addenda (never generated through letter-to-sound rules)
  (
   (#  - 0 - - - 0 0 -)
   (a  + l 3 2 - 0 0 -)
   (e  + l 2 1 - 0 0 -)
   (i  + l 1 1 - 0 0 -)
   (o  + l 2 3 + 0 0 -)
   (u  + l 1 3 + 0 0 -)
   (i0  + s 1 1 - 0 0 -)  ;; weak vowels in dipthongs -- not used
   (u0  + s 1 3 + 0 0 -)  ;; weak vowels in dipthongs -- not used
   (ax  + a 2 2 - 0 0 -)  ;; english weak vowel, schwa -- added

   (a1 + l 3 2 - 0 0 -)
   (e1 + l 2 1 - 0 0 -)
   (i1 + l 1 1 - 0 0 -)
   (o1 + l 2 3 + 0 0 -)
   (u1 + l 1 3 + 0 0 -)

   (p  - 0 - - - s l -)
   (t  - 0 - - - s d -)
   (k  - 0 - - - s v -)
   (b  - 0 - - - s l +)
   (d  - 0 - - - s d +)
   (g  - 0 - - - s v +)

   (B  - 0 - - - f l +)  ;; voiced fricative labial /b/ (intervocalic) -- added
   (D  - 0 - - - f d +)  ;; voiced fricative dental /d/ (intervocalic) -- added
   (G  - 0 - - - f v +)  ;; voiced fricative velar /g/ (intervocalic) -- added

   (f  - 0 - - - f b -)
   (th - 0 - - - f d -)
   (s  - 0 - - - f a -)
   (x  - 0 - - - f v -)

   (ch - 0 - - - a p -)

   (m  - 0 - - - n l +)
   (n  - 0 - - - n a +)
   (ny - 0 - - - n p +)

   (l  - 0 - - - l a +)
   (ll - 0 - - - l p +)

   (r  - 0 - - - l a +)
   (rr - 0 - - - l a +)

   (dh - 0 - - - f d +)  ;; english voiced fricative dental /dh/ (the) -- added
   (hh - 0 - - - f v -)  ;; english voiceless fricative glottal /hh/ (he) -- added
   (sh - 0 - - - f a -)  ;; english voiceless fricative post-alveolar /sh/ (she) -- added
   (v  - 0 - - - f b +)  ;; english voiced fricative labio-dental /v/ (voice) -- added
   (z  - 0 - - - f a +)  ;; english voiced fricative alveolar /z/ (zoo) -- added
   (zh - 0 - - - f a +)  ;; english voiced fricative post-alveolar /zh/ -- added
  )
)
(PhoneSet.silences '(#))

(define (JuntaDeAndalucia_es_sf::select_phoneset)
  "(JuntaDeAndalucia_es_sf::select_phoneset)
Set up phone set for JuntaDeAndalucia_es."
  (Parameter.set 'PhoneSet 'JuntaDeAndalucia_es)
  (PhoneSet.select 'JuntaDeAndalucia_es)
)

(define (JuntaDeAndalucia_es_sf::reset_phoneset)
  "(JuntaDeAndalucia_es_sf::reset_phoneset)
Reset phone set for JuntaDeAndalucia_es."
  t
)

(provide 'JuntaDeAndalucia_es_sf_phoneset)
