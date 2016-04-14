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
;;;  Predicting pause insertion

(define (Pauses utt)
"(Pauses utt)                                
Insert pauses where required."
  (let ((rval (apply_method 'Pause_Method utt)))
    (cond
     (rval rval) ;; new style
     (t
      (Classic_Pauses utt))))
  (Pause_optional_deleting_B_X utt))

(define (Classic_Pauses utt)
  "(Pauses UTT)
Predict pause insertion."
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
		((or (string-equal "B" pbreak)
		     (string-equal "BB" pbreak))
		 (insert_pause utt w))
;		((string-equal emph "1")
;		 (insert_pause utt w))
		((equal? w lastword)
		 (insert_pause utt w)))))
	   words)
	  ;; The embarrassing bit.  Remove any words labelled as punc or fpunc
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
	   words)
          ;; 12/01/2006 V.Strom: Even more embarrasing: Delete all silences
          ;; that are followed by a silence.  These silence sequences 
          ;; emerge if 'punc of phrase-final words consists of more than one 
          ;; character, e.g. period+quote.  That in turn causes problems in 
          ;; build_utts: the 2nd silence ends up with no features but its name, 
          ;; because there is no corresponding 2nd silence in the phone 
          ;; segmentation to align with.
          ;; This schould be fixed in the functions below, but it is easier for
          ;; me to clean up at the end:
          (set! sil (car (car (cdr (car (PhoneSet.description '(silences)))))))
          (set! seg (item.next(utt.relation.first utt 'Segment)))
          (while seg
             (if(and(equal? sil (item.name seg))
                    (equal? sil (item.name (item.prev seg))))
                (item.delete (item.prev seg)))
             (set! seg (item.next seg)))))
  utt))

(define (insert_pause utt word)
"(insert_pause UTT WORDITEM)
Insert a silence segment after the last segment in WORDITEM in UTT."
  (let ((lastseg (find_last_seg word))
	(silence (car (car (cdr (car (PhoneSet.description '(silences))))))))
    (if lastseg
	(item.relation.insert 
	 lastseg 'Segment (list silence) 'after))))

(define (insert_initial_pause utt)
"(insert_initial_pause UTT)
Always have an initial silence if the utterance is non-empty.
Insert a silence segment after the last segment in WORDITEM in UTT."
  (let ((firstseg (car (utt.relation.items utt 'Segment)))
	(silence (car (car (cdr (car (PhoneSet.description '(silences))))))))
    (if firstseg
	(item.relation.insert 
	 firstseg 'Segment (list silence) 'before))))

(define (insert_final_pause utt)
"(insert_final_pause UTT)
Always have a final silence if the utterance is non-empty."
  (let ((lastseg (utt.relation.last utt 'Segment))
        (silence (car (car (cdr (car (PhoneSet.description '(silences))))))))
    (set! silence (format nil "%l" silence)) ; to make the symbol a string
    ;(format t "silence is %l\n" silence)
    ;(format t "lastseg is %l\n" (item.name lastseg))
    (if lastseg
       (if (not(equal? (item.name lastseg) silence))
          (begin
             (format t "iserted final pause %s\n" silence)
             (item.relation.insert lastseg 'Segment (list silence) 'after))))))
     

(define (find_last_seg word)
;;; Find the segment that is immediately at this end of this word
;;; If this word is punctuation it might not have any segments
;;; so we have to check back until we find a word with a segment in it
  (cond
   ((null word)
    nil)  ;; there are no segs (don't think this can happen)
   (t
    (let ((lsyl (item.relation.daughtern word 'SylStructure)))
    (if lsyl
	(item.relation.daughtern lsyl 'SylStructure)
	(find_last_seg (item.relation.prev word 'Word)))))))

(define (Unisyn_Pauses utt)
  "(Unisyn_Pauses UTT)
Predict pause insertion in a Unisyn utterance structure."
  (let ((words (utt.relation.items utt 'Word)) lastword tpname)
    (if words
	(begin
	  (us_insert_initial_pause utt)   ;; always have a start pause
	  (set! lastword (car (last words)))
	  (mapcar
	   (lambda (w)
	     (let ((pbreak (item.feat w "pbreak"))
		   (emph (item.feat w "R:Token.parent.EMPH")))
	       (cond
		((or (string-equal "B" pbreak)
		     (string-equal "BB" pbreak))
		 (us_insert_pause utt w))
;		((string-equal emph "1")
;		 (us_insert_pause utt w))
		((equal? w lastword)
		 (us_insert_pause utt w)))))
	   words)
	  ;; The embarrassing bit.  Remove any words labelled as punc or fpunc
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
  utt))

(define (us_insert_pause utt word)
"(us_insert_pause UTT WORDITEM)
Insert a silence segment after the last segment in WORDITEM in UTT."
  (let ((lastseg (us_find_last_seg word))
	(silence "pau"))
    (if lastseg
	(item.relation.insert 
	 lastseg 'Segment (list silence) 'after))))

(define (us_insert_initial_pause utt)
"(us_insert_initial_pause UTT)
Always have an initial silence if the utterance is non-empty.
Insert a silence segment after the last segment in WORDITEM in UTT."
  (let ((firstseg (utt.relation.first utt 'Segment))
	(silence "pau"))
    (if firstseg
	(item.relation.insert 
	 firstseg 'Segment (list silence) 'before))))

(define (us_find_last_seg word)
;;; Find the segment that is immediately at this end of this word
;;; If this word is punctuation it might not have any segments
;;; so we have to check back until we find a word with a segment in it
  (cond
   ((null word)
    nil)  ;; there are no segs (don't think this can happen)
   (t
    (if (item.daughtern_to (item.relation word 'WordStructure) 'Syllable)
	(item.daughtern_to 
	 (item.relation
	  (item.daughtern_to (item.relation word 'WordStructure) 'Syllable)
	  'SylStructure)
	 'Segment)
	(us_find_last_seg (item.relation.prev word 'Word))))))

(define (Pause_optional_deleting_B_X utt)
"(Pause_optional_deleting_B_X utt)

Delete all phone symbols starting with 'B_' from the segemt relation 
(a B_150 e.g. is a 150ms pause) if symbol 'Pause_delete_B_X is defined.  
"
; The B_X never occur in the phone segmentation but are predicted by 
; some pause methods, in particular the default I used to produce the 
; .utt files for the 2009 test sentences for the  Blizzard challange.
; Some participants complained about them and I had to fix it quickly.
   (if (symbol-bound? 'Pause_delete_B_X)
      (let(seg )
         (set! seg (item.next(utt.relation.first utt 'Segment)))
         (while seg
             (set! next_seg (item.next seg))
             ;(format t "segment %l\n" (item.name seg))
             (if(string-matches (item.name seg) "B_[0-9]*")
                 (item.delete seg))
             (set! seg next_seg)))))

(provide 'pauses)
