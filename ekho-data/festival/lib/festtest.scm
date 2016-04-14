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
;;;   Some basic functions used in tests for Festival
;;;   

(define (test_words text)
"(test_words TEXT)
prints TEXT, Synthesizes TEXT and outputs the words in it."
  (format t "Word test: %s\n  " text)
  (set! utt1 (utt.synth (eval (list 'Utterance 'Text text))))
  (mapcar
   (lambda (word) (format t "%s " (car word)))
   (utt.features utt1 'Word '(name)))
  (format t "\n")
  t)

(define (test_segments text)
"(test_segments TEXT)
prints TEXT, Synthesizes TEXT and outputs the segments in it."
  (format t "Segment test: %s\n  " text)
  (set! utt1 (utt.synth (eval (list 'Utterance 'Text text))))
  (mapcar
   (lambda (word) (format t "%s " (car word)))
   (utt.features utt1 'Segment '(name)))
  (format t "\n")
)

(define (test_phrases text)
"(test_phrases TEXT)
prints TEXT, Synthesizes TEXT and outputs the words and phrase breaks."
  (format t "Phrase test: %s \n  " text)
  (set! utt1 (utt.synth (eval (list 'Utterance 'Text text))))
  (mapcar
   (lambda (phrase) 
     (mapcar (lambda (w) (format t "%s " (car (car w)))) (cdr phrase))
     (format t "%s\n  " (car (car phrase))))
   (utt.relation_tree utt1 'Phrase))
  (format t "\n")
  t)

(provide 'festtest)
