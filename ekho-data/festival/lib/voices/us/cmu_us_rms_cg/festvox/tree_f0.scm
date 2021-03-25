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
;;; Using CART models rather than LR models generate target points
;;; in a way similar to the way that Int_Targets_LR does
;;;
;;; To use this train three CART trees that predict start, mid and
;;; end values F0 (cf. the features syl_startpitch, syl_midpitch and
;;; syl_endpitch).  Then in your voice definition 
;;;  
;;; (set! F0start_tree f2b_F0start_tree)
;;; (set! F0mid_tree f2b_F0mid_tree)
;;; (set! F0end_tree f2b_F0end_tree)
;;; (set! int_params
;;; 	'((target_f0_mean 110) (target_f0_std 10)
;;; 	  (model_f0_mean 170) (model_f0_std 40)))
;;; (Parameter.set 'Int_Target_Method Int_Targets_Tree)

(define (Int_Targets_Tree utt)
  "(Int_Targets_Tree utt)
For each syllable in a phrase add start mid and end F0 targets."
  (utt.relation.create utt 'Target)
  (mapcar
   (lambda (syl)
     (Tree_Predict_Targets utt syl))
   (utt.relation.items utt 'Syllable))
  utt)

(define (Tree_Predict_Targets utt syl)
  "(Tree_Predict_Targets utt syl)
Add targets to start (if immediately after a pause) mid vowel
and end for this syllable."
  (if (tpt_after_pause syl)
      (tpt_add_target
       utt
       (item.relation.daughter1 syl 'SylStructure)
       0
       (wagon_predict syl F0start_tree)))
  (tpt_add_target utt (tpt_find_syl_vowel syl) 50
	      (wagon_predict syl F0mid_tree))
  (tpt_add_target utt (item.relation.daughtern syl 'SylStructure) 100
	      (wagon_predict syl F0end_tree)))

(define (tpt_after_pause syl)
  "(tpt_after_pause syl)
Returns t if segment immediately before this is a pause (or utterance
start).  nil otherwise."
  (let ((pseg (item.relation.prev (item.relation.daughter1 syl 'SylStructure)
				  'Segment)))
    (if (or (not pseg)
	    (member_string
	     (item.name pseg)
	     (car (cdr (car (PhoneSet.description '(silences)))))))
	t
	nil)))

(define (tpt_find_syl_vowel syl)
  "(tpt_find_syl_vowel syl)
Find the item that is the vowel in syl."
  (let ((v (item.relation.daughtern syl 'SylStructure)))
    (mapcar
     (lambda (s)
       (if (string-equal "+" (item.feat s "ph_vc"))
	   (set! v s)))
     (item.relation.daughters syl 'SylStructure))
    v))

(define (tpt_f0_map_value value)
  "(tpt_f0_map_value value)
Map F0 vlaue through means and standard deviations in int_params."
  (let ((target_f0_mean (get_param 'target_f0_mean int_params 110))
	(target_f0_stddev (get_param 'target_f0_stddev int_params 15))
	(model_f0_mean (get_param 'model_f0_mean int_params 110))
	(model_f0_stddev (get_param 'model_f0_stddev int_params 15)))
    (+ (* (/ (- value model_f0_mean) model_f0_stddev)
	  target_f0_stddev) target_f0_mean)))

(define (tpt_add_target utt seg pos value)
  "(tpt_add_target utt seg pos value)
Add Target at pos and value related to seg."
  (let ((tseg (item.relation seg 'Target))
	(ntarg))
    (if (null tseg)
	(set! tseg (utt.relation.append utt 'Target seg)))
    (set! ntarg (item.append_daughter tseg))
    (item.set_feat ntarg 'f0 (tpt_f0_map_value value))
    (item.set_feat ntarg 'pos 
		   (+ (item.feat seg "segment_start")
		      (* (/ pos 100) (item.feat seg "segment_duration"))))))

(provide 'tree_f0)
