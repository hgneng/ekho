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
;;; Multisyn Pause module (Rob Clark and Korin Richmond)
;;;
;;;

(defvar BB_Pause "B_300")
(defvar B_Pause "B_150")
(defvar mB_Pause "B_150") ; shouldn't be used

(define (MultiSyn_Pauses utt)
  "(MultiSyn_Pauses UTT)
Predict pause insertion in a Multisyn unit selection utterance structure."
  (let ((words (utt.relation.items utt 'Word)) lastword tpname)
    (if words
        (begin
          (insert_initial_pause utt)   ;; always have a start pause
          (set! lastword (car (last words)))
          (mapcar
           (lambda (w)
             (let ((pbreak (item.feat w "pbreak"))
                   (emph (item.feat w "R:Token.parent.EMPH")))
               (cond
		((string-equal pbreak "BB")
		 (unitselection_pause_insert w BB_Pause))
		((string-equal pbreak "mB")
		 (unitselection_pause_insert w mB_Pause))
		((string-equal pbreak "B")
		 (unitselection_pause_insert w B_Pause)))))
	  words)
	;; The embarassing bit.  Remove any words labelled as punc or fpunc
	(mapcar
	 (lambda (w)
	   (let ((pos (item.feat w "pos")))
	     (if (or (string-equal "punc" pos)
		     (string-equal "fpunc" pos))
		 (let ((pbreak (item.feat w "pbreak"))
		       (wp (item.relation w 'Phrase)))
		   (if (and (string-matches pbreak "BB?")
			    (item.relation.prev w 'Word))
		       (item.set_feat
                          (item.relation.prev w 'Word) "pbreak" pbreak))
		   (item.relation.remove w 'Word)
		   ;; can't refer to w as we've just deleted it
		   (item.relation.remove wp 'Phrase)))))
           words)))
    (utt.relation.print utt 'Word)
    (utt.relation.print utt 'Segment)
    utt))

(define (unitselection_pause_insert word pause)
  "(pause_insert word pause)
  Insert segments needed for a pause."
(let ((silence (car (cadr (car (PhoneSet.description '(silences))))))
      (seg (item.relation (find_last_seg word) 'Segment))
      pause_item)
  (format t "  inserting pause after: %s.\n" (item.name seg))
  (format t "  Inserting pause\n")
; if next seg is not silence insert one.
  (if (or (not (item.next seg))
	  (not (string-equal (item.name (item.next seg)) silence)))
      (item.insert seg (list silence) 'after))
; insert pause after that if not the end.
  (if (item.next (item.next seg))
      (begin
	(set! pause_item (item.insert (item.next seg) (list pause) 'after))
;if next seg after that is not silence add one.
	(if (not (string-equal (item.name (item.next pause_item)) silence))
	    (item.insert pause_item (list silence) 'after))))))

(provide 'multisyn_pauses)
