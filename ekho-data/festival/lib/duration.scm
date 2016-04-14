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
;;;  Basic Duration module which will call appropriate duration
;;;  (C++) modules based on set parameter
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  These modules should predict intonation events/labels
;;;  based on information in the phrase and word streams

(define (Duration utt)
"(Duration utt)
Predict segmental durations using Duration_Method defined in Parameters.
Four methods are currently available: averages, Klatt rules, CART tree
based, and fixed duration."
  (let ((rval (apply_method 'Duration_Method utt)))
    (cond
     (rval rval) ;; new style
     ;; 1.1.1 voices still use other names
     ((eq 'Averages (Parameter.get 'Duration_Method))
      (Duration_Averages utt))
     ((eq 'Klatt (Parameter.get 'Duration_Method))
      (Duration_Klatt utt))
     ((eq 'Tree_ZScores (Parameter.get 'Duration_Method))
      (Duration_Tree_ZScores utt))
     ((eq 'Tree (Parameter.get 'Duration_Method))
      (Duration_Tree utt))
     (t
      (Duration_Default utt)))))

(define (Duration_LogZScores utt)
"(Duration_LogZScores utt)
Predicts duration to segments using the CART tree in duration_logzscore_tree
and duration_logzscore_tree_silence which produces a zscore of the log
duration.  The variable duration_logzscore_ph_info contains (log) means
and std for each phone in the set."
  (let ((silence (car (car (cdr (assoc 'silences (PhoneSet.description))))))
	ldurinfo)
    (mapcar
     (lambda (s)
       (if (string-equal silence (item.name s))
	   (set! ldurinfo
		 (wagon s duration_logzscore_tree_silence))
	   (set! ldurinfo
		 (wagon s duration_logzscore_tree)))
       (set! dur (exp (duration_unzscore 
		       (item.name s)
		       (car (last ldurinfo))
		       duration_logzscore_ph_info)))
       (set! dur (* dur (duration_find_stretch s)))
       (item.set_feat 
	s "end" (+ dur (item.feat s "start_segment"))))
     (utt.relation.items utt 'Segment))
    utt))

(define (duration_unzscore phname zscore table)
"(duration_unzscore phname zscore table)
Look up phname in table and convert xscore back to absolute domain."
  (let ((phinfo (assoc phname table))
	mean std)
    (if phinfo
	(begin
	  (set! mean (car (cdr phinfo)))
	  (set! std (car (cdr (cdr phinfo)))))
	(begin
	  (format t "Duration: unzscore no info for %s\n" phname)
	  (set! mean 0.100)
	  (set! std 0.25)))
    (+ mean (* zscore std))))

(define (duration_find_stretch seg)
"(duration_find_stretch utt seg)
Find any relavant duration stretch."
  (let ((global (Parameter.get 'Duration_Stretch))
	(local (item.feat
		seg "R:SylStructure.parent.parent.R:Token.parent.dur_stretch")))
    (if (or (not global)
	    (equal? global 0.0))
	(set! global 1.0))
    (if (string-equal local 0.0)
	(set! local 1.0))
    (* global local)))

;; These provide lisp level functions, some of which have
;; been converted in C++ (in festival/src/modules/base/ff.cc)
(define (onset_has_ctype seg type)
  ;; "1" if onset contains ctype
  (let ((syl (item.relation.parent seg 'SylStructure)))
    (if (not syl)
	"0" ;; a silence 
	(let ((segs (item.relation.daughters syl 'SylStructure))
	      (v "0"))
	  (while (and segs 
		      (not (string-equal 
			    "+" 
			    (item.feat (car segs) "ph_vc"))))
		 (if (string-equal 
		      type
		      (item.feat (car segs) "ph_ctype"))
		     (set! v "1"))
		 (set! segs (cdr segs)))
	  v))))

(define (coda_has_ctype seg type)
  ;; "1" if coda contains ctype
  (let ((syl (item.relation.parent seg 'SylStructure)))
    (if (not syl)
	"0" ;; a silence 
	(let ((segs (reverse (item.relation.daughters
			      syl 'SylStructure)))
	      (v "0"))
	  (while (and segs 
		      (not (string-equal 
			    "+" 
			    (item.feat (car segs) "ph_vc"))))
		 (if (string-equal 
		      type
		      (item.feat (car segs) "ph_ctype"))
		     (set! v "1"))
		 (set! segs (cdr segs)))
	  v))))

(define (onset_stop seg)
  (onset_has_ctype seg "s"))
(define (onset_fric seg)
  (onset_has_ctype seg "f"))
(define (onset_nasal seg)
  (onset_has_ctype seg "n"))
(define (onset_glide seg)
  (let ((l (onset_has_ctype seg "l")))
    (if (string-equal l "0")
	(onset_has_ctype seg "r")
	"1")))
(define (coda_stop seg)
  (coda_has_ctype seg "s"))
(define (coda_fric seg)
  (coda_has_ctype seg "f"))
(define (coda_nasal seg)
  (coda_has_ctype seg "n"))
(define (coda_glide seg)
  (let ((l (coda_has_ctype seg "l")))
    (if (string-equal l "0")
	(coda_has_ctype seg "r")
	"1")))

(define (Unisyn_Duration utt)
  "(UniSyn_Duration utt)
predicts Segment durations is some speficied way but holds the
result in a way necessary for other Unisyn code."
  (let ((end 0))
    (mapcar
     (lambda (s)
       (item.get_utt s)
       (let ((dur (wagon_predict s duration_cart_tree)))
	 (set! dur (* (Parameter.get 'Duration_Stretch) dur))
	 (set! end (+ dur end))
	 (item.set_feat s "target_dur" dur)
	 (item.set_function s "start" "unisyn_start")
	 (item.set_feat s "end" end)
	 (item.set_feat s "dur" dur)
	 ))
     (utt.relation.items utt 'Segment))
    utt))

(provide 'duration)
