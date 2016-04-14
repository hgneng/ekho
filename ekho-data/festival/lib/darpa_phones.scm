;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
;;;                         Copyright (c) 1999                            ;;
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
;;;                         Author: Alan W Black
;;;                         Date:   April 1999
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  
;;;  (yet another) darpa definition
;;;

(require 'phoneset)

(set! darpa_fs (cadr
(defPhoneSet
  darpa
  (Features
   (vowel (syllabic + -)
	  (length long short diphthong schwa)
	  (height high mid low)
	  (front front mid back)
	  (round + -))
   (consonant
    (syllabic + -)
    (manner stop affricate fricative approximant nasal)
    (place alveolar dental labial palatal velar)
    (voicing + -))
   (silence
    (syllabic -)))
  (Phones
   ;;  type syl length  height front round
   (aa  vowel + long      low   back    -)
   (ae  vowel + short     low   front   -)
   (ah  vowel + short     mid   mid     -)
   (ao  vowel + long      low   front   +)
   (aw  vowel + diphthong low   mid     -)
   (ax  vowel + schwa     mid   mid     -)
   (axr vowel + schwa     mid   mid     -)
   (ay  vowel + diphthong low   mid     -)
   (eh  vowel + short     mid   front   -)
   (ey  vowel + diphthong mid   front   -)
   (ih  vowel + short     high  front   -)
   (iy  vowel + long      high  front   -)
   (ow  vowel + diphthong mid   back    +)
   (oy  vowel + diphthong mid   back    +)
   (uh  vowel + short     high  back    +)
   (uw  vowel + long      high  back    +)
   ;;   type     syl manner     place   voicing
   (b   consonant -   stop      labial     +)
   (ch  consonant - affricate   alveolar   -)
   (d   consonant -   stop      alveolar   +)
   (dh  consonant - fricative   dental     +)
   (dx  consonant -   stop      alveolar   +)
   (el  consonant + approximant alveolar   +)
   (em  consonant +   nasal     labial     +)
   (en  consonant +   stop      alveolar   +)
   (er  consonant + approximant alveolar   +)
   (f   consonant - fricative   labial     -)
   (g   consonant -   stop      velar      +)
   (hh  consonant - fricative   velar      -)
   (jh  consonant - affricate   alveolar   +)
   (k   consonant -   stop      velar      -)
   (l   consonant - approximant alveolar   +)
   (m   consonant -   nasal     labial     +)
   (n   consonant -   nasal     alveolar   +)
   (nx  consonant -   nasal     alveolar   +)
   (ng  consonant -   nasal     velar      +)
   (p   consonant -   stop      labial     -)
   (r   consonant - approximant alveolar   +) 
   (s   consonant - fricative   alveolar   -)
   (sh  consonant - fricative   palatal    -)
   (t   consonant -   stop      alveolar   -)
   (th  consonant - fricative   dental     -) 
   (v   consonant - fricative   labial     +)
   (w   consonant - approximant velar      +)
   (y   consonant - approximant palatal    +)
   (z   consonant - fricative   alveolar   +)
   (zh  consonant - fricative   palatal    +)
   (pau silence   -)
;   (sil silence   -)
   ))))

(provide 'darpa_phones)




