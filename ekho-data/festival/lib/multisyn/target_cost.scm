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
;;; Multisyn scheme target cost (Rob Clark and Korin Richmond)
;;;
;;;

(define (Default_Target_Cost targ cand)
"(Default_Target_Cost targ cand)
A Default Target Cost function."
(let ((cost 0))
  (mapcar
   (lambda (row)
     (set! cost (+ cost (tc_eval_row row targ cand))))
   target_matrix)
  (set! cost (/ cost target_matrix_weight))
  cost))


(define (tc_eval_row row targ cand)
  "(tc_eval_row row targ cand)
Evaluate a target matrix row."
(let ((weight (car row))
      (func (cadr row))
      (result 0))
  (set! result (* weight (eval (list func targ cand))))
  result))

;;
;; Target cost Matrix
;;  '(weight function)

(define (get_matrix_weight m)
  (let ((w 0))
    (mapcar
     (lambda (x)
       (set! w (+ w (car x))))
     m)
    w))


(set! test_matrix_max_weight 1)
(set! test_matrix
'(
  (10 tc_stress )
  (5 tc_syl_pos )
  (5 tc_word_pos)
  (6 tc_partofspeech)
  (7 tc_phrase_pos) 
  (4 tc_left_context)
  (3 tc_right_context)
  (25 tc_bad_f0) ;; set to equal 1/3 of total cost (so high because interaction with join) 
;  (0 tc_segment_score) ;; was 4. turned off until utterances are built for this.
  (10 tc_bad_duration) ;; was 6
))

(set! test_matrix_weight (* test_matrix_max_weight (get_matrix_weight test_matrix)))

(set! target_matrix test_matrix)
(set! target_matrix_weight test_matrix_weight)



;;
;; tc_stress
;;
;; Compares stress on any vowel which form part of the diphone. stress
;; conditions must match for a zero target cost.
;; 

(define (tc_stress targ cand)
"(tc_stress targ cand)
Target Cost stressed. 0    - stress patterns match  [ compares: 0 unstressed vs. > 0 stressed ]
                      1    - stress miss-match.
"
(let ((c 0)
      cand_stress targ_stress)
  ;(format t "my_is_vowel %l\n" (my_is_vowel targ))
  ;(format t "phone_is_silence %l\n" (phone_is_silence (item.feat targ 'name)))
  ;; For first segment
  (if (and (not (phone_is_silence (item.feat targ 'name)))
	   (my_is_vowel targ))
      (begin 
	(set! cand_stress (item.feat cand "R:SylStructure.parent.stress"))
	(set! targ_stress (item.feat targ "R:SylStructure.parent.stress"))
	(if (or (and (eq? cand_stress 0) (> targ_stress 0))
		(and (eq? targ_stress 0) (> cand_stress 0)))
	    (set! c 1))))
  ;; For second segment
  ;(format t "n.my_is_vowel %l\n" (my_is_vowel (item.next targ)))
  ;(format t "n.phone_is_silence %l\n" (phone_is_silence (item.feat targ 'n.name)))
  (if (and (not (phone_is_silence (item.feat targ 'n.name)))
	   (my_is_vowel (item.next targ)))
      (begin 
	(set! cand_stress (item.feat cand "n.R:SylStructure.parent.stress"))
	(set! targ_stress (item.feat targ "n.R:SylStructure.parent.stress"))
	(if (or (and (eq? cand_stress 0) (> targ_stress 0))
		(and (eq? targ_stress 0) (> cand_stress 0)))
	    (set! c 1))))
;  (format t "tc_stress: %l\n" c)
c))


;;
;; tc_syl_position
;;
;; Find and compare diphone position in syllabic structure.
;; Values are: inter - diphone crosses syllable boundary. 
;;             initial - diphone is syllable initial.
;;             medial - diphone is syllable medial
;;             final  - diphone is syllable final
;; returns 0 for a match 1 for a mismatch.
;;
(define (tc_syl_pos targ cand)
"(tc_syl_pos targ cand)
Score position in syllable."
(let ((targ_pos "medial")
      (cand_pos "medial")
      (targ_syl (get_syl targ))
      (targ_next_syl (get_syl (item.next targ)))
      (cand_syl (get_syl cand))
      (cand_next_syl (get_syl (item.next cand))))
  ;; target 
  (cond
   ((not (equal? targ_syl targ_next_syl))
    (set! targ_pos "inter"))
   ((not (equal? targ_syl (get_syl (item.prev targ))))
    (set! targ_pos "initial"))
   ((not (equal? targ_next_syl (get_syl (item.next (item.next targ)))))
    (set! targ_pos "final")))
  ;; candidate
  (cond
   ((not (equal? cand_syl cand_next_syl))
    (set! cand_pos "inter"))
   ((not (equal? cand_syl (get_syl (item.prev cand))))
    (set! cand_pos "initial"))
   ((not (equal? cand_next_syl (get_syl (item.next (item.next cand)))))
    (set! cand_pos "final")))
;  (format t "targ_syl: %l   cand_syl %l\n" targ_pos cand_pos)
   (if (equal? targ_pos cand_pos) 0 1)))

;;
;; tc_word_position
;;
;; Find and compare diphone position in word structure
;; Values are: inter - diphone crosses word boundary. 
;;             initial - diphone is word initial.
;;             medial - diphone is word medial
;;             final  - diphone is word final
;; returns 0 for a match 1 for a mismatch.
;;
(define (tc_word_pos targ cand)
"(tc_word_pos targ cand)
Score position in word."
(let ((targ_pos "medial")
      (cand_pos "medial")
      (targ_word (get_word targ))
      (targ_next_word (get_word (item.next targ)))
      (cand_word (get_word cand))
      (cand_next_word (get_word (item.next cand))))
  ;; target 
  (cond
   ((not (equal? targ_word targ_next_word))
    (set! targ_pos "inter"))
   ((not (equal? targ_word (get_word (item.prev targ))))
    (set! targ_pos "initial"))
   ((not (equal? targ_next_word (get_word (item.next (item.next targ)))))
    (set! targ_pos "final")))
  ;; candidate
  (cond
   ((not (equal? cand_word cand_next_word))
    (set! cand_pos "inter"))
   ((not (equal? cand_word (get_word (item.prev cand))))
    (set! cand_pos "initial"))
   ((not (equal? cand_next_word (get_word (item.next (item.next cand)))))
    (set! cand_pos "final")))
;  (format t "targ_word: %l   cand_word %l\n" targ_pos cand_pos)
   (if (equal? targ_pos cand_pos) 0 1)))



;;
;; tc_phrase_position
;;
;; Position (of word) in phrase
;; initial/medial/final
;;
;; 0 - match, 1 - mismatch
;;
(define (tc_phrase_pos targ cand)
"(tc_phrase_pos targ cand)
 Score position in phrase."
(let ((targ_word (get_word targ))
      (cand_word (get_word cand)))
  (cond
   ((and (null targ_word)
	 (null cand_word))
    0)
   ((or (null targ_word)
	(null cand_word))
    1)
   ((string-equal (item.feat targ_word 'pbreak)
		  (item.feat cand_word 'pbreak))
    0)
   (t 1))))

;;
;; tc_partofspeech
;;
;; 
;;
(define (tc_partofspeech targ cand)
"(tc_partofspeech targ cand)
 Score part of speech."
(let ((targ_word (get_word targ))
      (cand_word (get_word cand))
      targ_pos cand_pos)
(if targ_word
    (set! targ_pos (simple_pos (item.feat targ_word 'pos))))
(if cand_word
    (set! cand_pos (simple_pos (item.feat cand_word 'pos))))
  ;(format t "targ_pos %l cand_pos %l\n" targ_pos cand_pos)
  (if (equal? targ_pos cand_pos) 0 1)))

(define (score_contexts targ_context cand_context)
  "(score_contexts targ_context cand_context)
If both context items are nil, then score is 0.
If both context items are not nil, and are the same, then
score is 0. Otherwise, score is 1."
  (if (and targ_context cand_context)
	(if (equal? (item.feat targ_context "name")
		    (item.feat cand_context "name"))
	    0
	    1)
      (if (and (equal? targ_context nil)
	       (equal? cand_context nil))
	  0
	  1)))


(define (tc_left_context targ cand)
"(tc_left_context targ cand)
Score left phonetic context."
(let ((targ_context (item.prev targ))
      (cand_context (item.prev cand)))
  (score_contexts targ_context cand_context)))

;;
;; tc_right_context
;;
;;
;;
(define (tc_right_context targ cand)
"(tc_right_context targ cand)
Score right phonetic context."
(let ((targ_context (item.next (item.next targ)))
      (cand_context (item.next (item.next cand))))
  (score_contexts targ_context cand_context)))


;;
;; tc_segment_score
;;
;; This currently thresholds based on looking at the distributions of the scores.
;; A nice exp function may be better.
(define (tc_segment_score targ cand)
"tc_segment_score targ cand)
A bad alignment score make a bad segment."
(let ((score 0))
  (if (not (phone_is_silence (item.feat cand "name")))
      (set! score  (+ score (item.feat cand 'score))))
  (if (not (phone_is_silence (item.feat (item.next cand) "name")))
      (set! score  (+ score (item.feat (item.next cand) 'score))))
  (cond
   ((> score -4000)    ;2000 (x2) is 7.5% 
    0)
   ((> score -5000)    ;2500 (x2) is 5.0%
    0.5)
   (t 1))))
   
;;
;; tc_bad_duration
;;
;; If the segment is marked as having a weird duration penalise it.
;; We allow bad_dur to be set on the target so resynthesis works 
;; and so you could ask for really long/short segments.
;;
(define (tc_bad_duration targ cand)
  (if (equal? (item.feat targ "bad_dur")
	      (item.feat cand "bad_dur"))
      0
      1))


;;
;; tc_bad_f0
;;
;; If the candidate is deemed to have an inappropriate f0, then penalise it.
;; 
;; Specifically, if the targ/cand segment type is expected to be voiced, then
;; an f0 of zero is bad (results from poor pitch tracking).  In such a case,
;; the join cost would then favour other units with f0 (since the euclidean
;; distance between two zeros is very small ;)
;; We want to avoid that.
;;
;; Presumeably, we also want to penalise cases where supposedly voiceless 
;; candidates have an f0 != 0 (either a consequence of bad pitch tracking
;; or bad labelling) but that's not done here yet...
;;
;; (the function itself has been implemented in C for convenience, and
;; this stub is left here just for this note ;)

(define (tc_bad_f0 targ cand)
  (let ((score (temp_tc_bad_f0 targ cand))
	(name (format nil "%s_%s" 
		      (item.feat targ "name")
		      (item.feat (item.next targ) "name"))))
    (if (not (equal? score 0.0))
	(format t "f0 score for %s is %f\n" name score))
    score))

;;
;; Is a segment a vowel?  ( ph_is_a_vowel doesn't seem to work)
;;
(define (my_is_vowel seg)
  (if seg
      (if (equal? (item.feat seg 'ph_vc) "+")
	  t
	  nil)))



;; get the syllable from sysstructure in normal utterance
;;
(define (get_syl seg)
  (let (syl)
    (if seg 
	(set! syl (item.relation.parent seg 'SylStructure)))
    syl))

;; get the word from sylstructure in normal utterance
;;
(define (get_word seg)
  (let ((syl (get_syl seg))
	word)
    (if syl
	(set! word (item.parent syl)))
    word))
   

;; simple pos
;;
(define (simple_pos pos)
(let (spos)
  (cond
   ((member_string pos '(vbd vb vbn vbz vbp vbg))
    (set! spos "v"))
   ((member_string pos '(nn nnp nns nnps fw sym ls))
    (set! spos "n"))
   ((member_string pos '(dt gin prp cc of to cd md pos wdt wp wrb ex uh pdt))
    (set! spos "func"))
   ((member_string pos '(jj jjr jjs 1 2 rb rp rbr rbs))
    (set! spos "other")))
  spos))


;; debugging

(define (test_target_cost utt1 utt2)
(let ((segs1 (utt.relation.items utt1 'Segment))
      (segs2 (utt.relation.items utt2 'Segment))
      (tc 0))
  (while (and segs1 segs2)
	 (set! tc (Default_Target_Cost (car segs1) (car segs2)))
	 (format t "targ: %l cand: %l cost: %l\n" (item.name (car segs1)) (item.name (car segs2)) tc)
	 (set! segs1 (cdr segs1))
	 (set! segs2 (cdr segs2)))))


(provide 'target_cost)
