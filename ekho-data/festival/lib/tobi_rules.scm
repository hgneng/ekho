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
;;;                Authors: Robert A. J. Clark and Alan W Black
;;;                Modifications and Checking: 
;;;                         Gregor Moehler (moehler@ims.uni-stuttgart.de)
;;;                         Matthew Stone (mdstone@cs.rutgers.edu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generate F0 points from tobi labels using rules given in:
;;; Jilka, Moehler & Dogil (forthcomming in Speech Communications)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  *** Converted to new Relation architecture -- but not checked yet -- awb
;;;      -> crude (beta) checking: gm in Dec. 98
;;;
;;;      -> fixed TAKEOVER bug that used time value 
;;;         as pitch target (!) - MDS 1/02
;;;      -> hacked around bunches of target overlap problems - MDS 1/02
;;;      -> added primitive pitch range controls
;;;      
;;;  Known problems and bugs:
;;;      Can't currently use voicing intervals which cross syllable boundaries,
;;;      so pre/post-nuclear tones are currently places 0.2s before/after the 
;;;      nuclear tone even if no voicing occurs. Failing this they default a
;;;      percentage of the voicing for that syllable. 
;;; 
;;;      Don't know about target points ahead of the current syllable.
;;;      (As you need to know what comes before them to calculate them)
;;;      So: post accent tones are placed 0.2 ahead if following syllable exists
;;;          ends before 0.2 from starred target and is not accented
;;;      The H-target of the H+!H* is 0.2 sec instead of 0.15 sec before 
;;;      starred tone.
;;;      
;;;      Multi-utterance input has not been tested. 
;;;      
;;;      !H- does not generate any targets
;;;      
;;;      Unfortunaltely some other modules may decide to put pauses in the 
;;;      middle of a phrase
;;;      
;;;      valleys are not tested yet
;;;      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  To use this in a voice 
;;;     (require 'tobi_rules)
;;;  And in the voice call
;;;     (setup_tobi_f0_method)
;;;  Set the following for your speaker's F0 range
;;;  (Parameter.set 'Default_Topline 146)
;;;  (Parameter.set 'Default_Start_Baseline 61)
;;;  (Parameter.set 'Valley_Dip 75)

;; level of debug printout
(set! printdebug 0)

(define (setup_tobi_f0_method)
  "(setup_tobi_f0_method)
Set up parameters for current voice to use the implementaion
of ToBI labels to F0 targets by rule."
  (Parameter.set 'Int_Method Intonation_Tree)
  (Parameter.set 'Int_Target_Method Int_Targets_General)
  (set! int_accent_cart_tree no_int_cart_tree) ; NONE always
  (set! int_tone_cart_tree   no_int_cart_tree) ; NONE always
  (set! int_general_params
	(list 
	 (list 'targ_func tobi_f0_targets)))   ; we will return a list of f0 targets here

  (Parameter.set 'Phrase_Method 'cart_tree)
  (set! phrase_cart_tree tobi_label_phrase_cart_tree) ; redefines the phrasebreak tree
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
;;;;;; Define and set the new f0 rules
;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set global parameters
;;; You may want to reset these for different speakers

(Parameter.set 'Default_Topline 146) ;146
(Parameter.set 'Default_Start_Baseline 61) ;61
(Parameter.set 'Current_Topline        (Parameter.get 'Default_Topline))
(Parameter.set 'Current_Start_Baseline (Parameter.get 'Default_Start_Baseline))
(Parameter.set 'Current_End_Baseline   (Parameter.get 'Current_Start_Baseline))
(Parameter.set 'Downstep_Factor 0.70)
(Parameter.set 'Valley_Dip 75)
;;; function to add target points on a given syllable and fill in 
;;; targets where necessary

(define (tobi_f0_targets utt syl)
  "(tobi_f0_targets UTT ITEM)
   Returns a list of targets for the given syllable."
  (if (and (>= printdebug  1)
	   (not(equal? 0 (item.feat syl "R:Intonation.daughter1.name"))))
      (format t "### %l (%.2f %.2f) %l ptarg: %l ###\n" (item.name syl)
	      (item.feat syl "syllable_start")(item.feat syl "syllable_end")
	      (item.feat syl "R:Intonation.daughter1.name") (ttt_last_target_time syl)))
  
  ;; only continue if there is a Word related to this syllable
  ;; I know there always should be, but there might be a bug elsewhere
  (cond 
   ((not(equal? 0 (item.feat syl "R:SylStructure.parent.name")))

    ; get current label. This assumes that there is only one accent and
    ; one endtone on a syllable. Although there can be one of each.
    (let ((voicing  (ttt_get_voice_times syl))                ; voicing interval
	  (pvoicing (ttt_get_voice_times                      ; previous voicing
		     (item.relation.prev syl 'Syllable)))
	  (nvoicing (ttt_get_voice_times                      ; next voicing
		     (item.relation.next syl 'Syllable))))

    ; if first syl of phrase set Phrase_Start and Phrase_End parameters
    ; and reset downstep (currently does so on big and little breaks.)
    ; only assignes Default values at this stage 
    ; maybe trained from CART later - first steps now - MDS
    ; following Moehler and Mayer, SSW 2001 
    (if   (eq 0 (item.feat syl 'syl_in)) ;; GM maybe something better needed here?
	(progn
	 (Parameter.set 'Phrase_Start (item.feat syl 'R:SylStructure.parent.R:Phrase.last.word_start))
	 (Parameter.set 'Phrase_End (item.feat syl 'R:SylStructure.parent.R:Phrase.last.word_end))
	 (Parameter.set 'Current_Topline 
			(/ (* (wagon syl ttt_topline_tree) 
			      (Parameter.get 'Default_Topline)) 100))
	 (Parameter.set 'Current_Start_Baseline
			(/ (* (wagon syl ttt_baseline_tree)
			      (Parameter.get 'Default_Start_Baseline)) 100))
	 (Parameter.set 'Current_End_Baseline 
			(Parameter.get 'Current_Start_Baseline))
	 (if (>= printdebug  3)
	     (begin 
	       (print (format nil "new range: %f %f %f" 
			      (Parameter.get 'Current_Topline) 
			      (Parameter.get 'Current_Start_Baseline)
			      (Parameter.get 'Current_End_Baseline) ))))  ))

    ; do stuff (should go only if there is an accent/boundary?)
    (let ((new_targets 
	   (ttt_to_targets syl (wagon syl ttt_starttone_tree)
			   voicing
			   pvoicing
			   nvoicing
			   'Starttones)))

    (set! new_targets (append new_targets 
	   (ttt_to_targets syl (wagon syl ttt_accent_tree)
			   voicing 
			   pvoicing 
			   nvoicing 
			   'Accents)))

    (set! new_targets (append new_targets 
	   (ttt_to_targets syl (wagon syl ttt_endtone_tree)
			   voicing
			   pvoicing
			   nvoicing
			   'Endtones)))

    (if (and(not(equal? new_targets nil))
	    (>= printdebug  2))
	(begin
	  (format t ">> Targets: %l\n" new_targets)
	  (format t ">> LastTarget: %l\n" (last new_targets))
	  ))

      new_targets)))))


;;; CART tree to specify no accents

(set! no_int_cart_tree
'
((NONE)))

;;;
;;; Relate phrasing to boundary tones.
;;;   Added downstepped tones - MDS

(set! tobi_label_phrase_cart_tree
'
((tone in ("L-" "H-" "!H-"))
 ((B))
 ((tone in ("H-H%" "H-L%" "!H-L%" "L-L%" "L-H%"))
  ((BB))
  ((NB)))))

;;;
;;;  The other functions
;;;

;;; process a list of relative targets and convert to actual targets

(define (ttt_to_targets syl rlist voicing pvoicing nvoicing type)
  "Takes a list of target sets and returns a list of targets."
  (if (or (and (>= printdebug  2)
	       rlist (atom (caar rlist)) 
	       (not (equal? 'NONE (caar rlist))) (not (equal? '(NONE) (caar rlist))))
	  (>= printdebug  3)) 
       (begin (print "Entering ttt_to_targets with:")
	(print (format nil "rlist: %l vc: %l pvc: %l nvc: %l type: %s" rlist voicing pvoicing nvoicing type))))
(cond 
 ;; nowt
 ((eq (length rlist) 0) ())
 ;; a single target set
 ((atom (car (car rlist)))
  (cond
   ((eq type 'Accents)
    (ttt_accent_set_to_targets syl rlist voicing pvoicing nvoicing))
   ((eq type 'Starttones)
    (ttt_bound_set_to_targets syl rlist voicing pvoicing))
   ((eq type 'Endtones)
    (ttt_bound_set_to_targets syl rlist voicing pvoicing))
   (t (error "unknown target set encountered in ttt_to_targets"))))
 ;; list of target sets
 ((atom (car (car (car rlist))))
  (append (ttt_to_targets syl (cdr rlist) voicing pvoicing nvoicing type)
	  (ttt_to_targets syl (car rlist) voicing pvoicing nvoicing type)))
 ;; error
 (t (error "something strange has happened in ttt_to_targets"))))


;; process a starttone/endtone target set.

(define (ttt_bound_set_to_targets syl tset voicing pvoicing)
  "takes a start/endtone target set and returns a list of target points."
  (if (>= printdebug  3) (begin
      (print "Entering ttt_bound_set_to_targets with:")
      (pprintf (format nil "tset: %l vc: %l pvc: %l" tset voicing pvoicing))))
  (cond
   ;; usually target given is NONE. (also ignore unknown!)
   ((or (eq (car (car tset)) 'NONE)
	(eq (car (car tset)) 'UNKNOWN))
    nil)
   ;; a pair of target pairs
   ((eq (length tset) 2)
    (list (ttt_get_target (car tset) voicing) 
	  (ttt_get_target (car (cdr tset)) voicing)))
   ;; single target pair
   ((eq (length tset) 1)
    (cond
     ;; an actual target pair
     ((not (null (cdr (car tset))))
      (list (ttt_get_target (car tset) voicing)))
     ;; a TAKEOVER marker
     ((eq (car (car tset)) 'TAKEOVER)
      (list (list (ttt_interval_percent voicing 0) 
		  (ttt_last_target_value syl))))
     (t (error "unknown target pair in ttt_bound_set_to_targets"))))
   (t (error "unknown target set type in ttt_bound_set_to_targets"))))


;; process an accent target set.

(define (ttt_accent_set_to_targets syl tset voicing pvoicing nvoicing)
  "takes a accent target set and returns a list of target points."
  (if (>= printdebug  3) (begin
      (print "Entering ttt_accent_set_to_targets with:")
      (pprintf (format nil "tset: %l vc: %l pvc: %l nvc: %l" tset voicing pvoicing nvoicing))))
  (cond
   ;; single target in set
   ((null (cdr tset)) 
    (cond
     ; target given is NONE.
     ((or (eq (car (car tset)) 'NONE)
	  (eq (car (car tset)) 'UNKNOWN)) nil) 
     ; V1 marker
     ((eq (car (car tset)) 'V1)
      (let ((target_time (+ (/ (- (next_accent_start syl)
				  (ttt_last_target_time syl))
			       2.0)
			    (ttt_last_target_time syl))))
	(list (list target_time (ttt_accent_pitch (Parameter.get 'Valley_Dip) target_time)))))
     ; V2 marker
     ((eq (car (car tset)) 'V2)
      (let ((target_time (+ (ttt_last_target_time syl) 0.25)))
	(list (list target_time (ttt_accent_pitch (Parameter.get 'Valley_Dip) target_time)))))
     ; V3 marker
     ((eq (car (car tset)) 'V3)
      (let ((target_time (- (next_accent_start syl) 0.25)))
	(list (list target_time (ttt_accent_pitch (Parameter.get 'Valley_Dip) target_time)))))     
     ; single target pair
     (t (list (ttt_get_target (car tset) voicing)))))
   ;; a pair of targets
   ((length tset 2)
    (cond
     ;; a *ed tone with PRE type tone (as in L+H*)
     ((eq (car (car tset)) 'PRE)
      (let ((star_target (ttt_get_target (car (cdr tset)) voicing))
	    (last_target (parse-number(ttt_last_target_time syl))))
	(cond
	 ; normal 0.2s case (currently doesn't check for voicing)
	 ((and (eqv? 0 (ip_initial syl))
	       (> (- (car star_target) 0.2) last_target))
	  (list  (list (- (car star_target) 0.2)
	   	       (ttt_accent_pitch (car (cdr (car tset)))
					 (- (car star_target) 0.2))) ; the time
	   	 star_target))

	 ; 90% prev voiced if not before last target - Added back in MDS,
	 ; with parse-number added and new check for ip_initial
	 ((and (eqv? 0 (ip_initial syl))
	       (> (parse-number (ttt_interval_percent pvoicing 90))
		  (parse-number (ttt_last_target_time syl))))
	  (list (list (ttt_interval_percent pvoicing 90)
		      (ttt_accent_pitch (car (cdr (car tset)))
					(ttt_interval_percent pvoicing 90)))
		star_target))

	 ;  otherwise (UNTESTED) [NOTE: Voicing for this syllable only]
	 (t 
	  (list (list (ttt_interval_percent voicing 20)
	 		(ttt_accent_pitch (car (cdr (car tset)))
	 				  (ttt_interval_percent voicing 20)))
	 	  star_target)))))
     ; a *ed tone with POST type tone (as L*+H)
     ((eq (car(car(cdr tset))) 'POST)
      (let ((star_target (ttt_get_target (car tset) voicing))
	    (next_target nil ) ; interesting problem
	    (next_syl (item.next syl)))

	(cond
	 ; normal 0.2s case (UNTESTED)
	 ((and (not (equal? next_syl nil))
	       (eq 0 (item.feat next_syl "accented")))
	  (cond
	   ((< (+ (car star_target) 0.2) (item.feat next_syl "syllable_end"))
	    (list star_target 
		  (list (+ (car star_target) 0.2) 
			(ttt_accent_pitch (car (cdr (car (cdr tset))))
					  (+ (car star_target) 0.2) ))))
	   (t 
	    
	    (list star_target
		    (list (ttt_interval_percent nvoicing 90)
			  (ttt_accent_pitch (car (cdr (car (cdr tset))))
					    (ttt_interval_percent nvoicing 90) ))))))

	 ; 20% next voiced (BUG: Can't do this as the next target hasn't been
	 ;                                                     calculated yet!)
	 (nil nil)
	 ;otherwise (UNTESTED)
	 (t (list star_target
		  (list (ttt_interval_percent voicing 90)
			(ttt_accent_pitch (car (cdr (car (cdr tset))))
					  (ttt_interval_percent voicing 90) )))))))
     
     (t 
      ;; This case didn't use to happen, but now must 
      ;; to avoid +H's clobbering endtones - MDS's hack.
      (list (ttt_get_target (car tset) voicing)
	    (ttt_get_target (cadr tset) voicing)))))

   
   ;; something else...
   (t (error (format nil "unknown accent set in ttt_accent_set_to_targets: %l" tset)))))



(define (ttt_get_target pair voicing)
  "Returns actual target pair, usually for a stared tone."
  (if (>= printdebug  4) (begin
      (print "Entering ttt_get_target with:")
      (pprintf pair) (pprintf voicing)))
  (list (ttt_interval_percent voicing (car pair))
	(ttt_accent_pitch (car (cdr pair))
			  (ttt_interval_percent voicing (car pair)))))

(define (ttt_accent_pitch value time)
  "Converts a accent pitch entry to a pitch value."
  (if (>= printdebug  4) (begin
      (print "Entering ttt_accent_pitch with:")
      (pprintf value)))
  (cond
   ;; a real value
   ((number? value) 
    (ttt_interval_percent (list (ttt_get_current_baseline time)
				(Parameter.get 'Current_Topline))
			  value))
   ;; Downstep then Topline
   ((eq value 'DHIGH)
    (progn
     (Parameter.set 'Current_Topline (+ (ttt_get_current_baseline time)
					(* (Parameter.get 'Downstep_Factor)
					   (- (Parameter.get 'Current_Topline)
					      (ttt_get_current_baseline time)))))
     (ttt_interval_percent (list (ttt_get_current_baseline time)
				 (Parameter.get 'Current_Topline))
			   100)))
     
   ;; Unknown
   (t  (error "Unknown accent pitch value encountered"))))


(define (ttt_get_current_baseline v)
  "Returns the current declined baseline at time v."
  (if (>= printdebug  4) (begin
      (print "Entering  ttt_get_current_baseline with:")
      (pprintf v)))
  (let ((h (Parameter.get 'Current_Start_Baseline))
	(l (Parameter.get 'Current_End_Baseline))
	(e (Parameter.get 'Phrase_End))
	(s (Parameter.get 'Phrase_Start)))
    (- h (* (/ (- h l) (- e s)) (- v s)))))

;;; find the time n% through an inteval

(define (ttt_interval_percent pair percent)
  "Returns the time that is percent percent thought the pair."
  (if (>= printdebug  4) (begin
      (print "Entering ttt_interval_percent with:")
      (pprintf (format nil "%l, %l" pair percent))))
  (cond
   ; no pair given: just return nil
   ((null pair) nil)
   ; otherwise do the calculation
   (t (let ((start (car pair))
	    (end (car(cdr pair))))
	(+ start (* (- end start) (/ percent 100)))))))


;;;  Getting start and end voicing times in a syllable

(define (ttt_get_voice_times syl_item)
  "Returns a pair of start time of first voiced phone in syllable and
end of last voiced phone in syllable, or nil if syllable is nil"
  (cond
   ((null syl_item) nil)
   (t (let ((segs (item.relation.daughters syl_item "SylStructure")))
	(list
	 (item.feat (ttt_first_voiced segs) "segment_start")
	 (item.feat (ttt_first_voiced (reverse segs)) "end"))))))

(define (ttt_first_voiced segs)
  "Returns first segment that is voiced (vowel or voiced consonant)
returns last segment if all are unvoiced."
  (cond
   ((null (cdr segs))
    (car segs))  ;; last possibility
   ((equal? "+" (item.feat (car segs) "ph_vc"))
    (car segs))
   ((equal? "+" (item.feat (car segs) "ph_cvox"))
    (car segs))
   (t
    (ttt_first_voiced (cdr segs)))))

;;; ttt_last_target has bifurcated into
;;;   ttt_last_target_time and
;;;   ttt_last_target_value 
;;; to fix a place where f0 was set to last target time!
;;;   - MDS

(define (ttt_last_target_time syl)
  "Returns the end of the most recent previous target 
in the utterance or nil if there is not one present
"
  (if (>= printdebug  3)
      (begin (print "Entering  ttt_last_target_time")
	     (print syl))
      )
  (let ((target (ttt_last_target syl)))
    (if (null? target)
	nil
	(item.feat target "R:Target.daughter1.pos"))))

(define (ttt_last_target_value syl)
  "Returns the pitch of the most recent previous target 
in the utterance or nil if there is not one present
"
  (if (>= printdebug  3)
      (begin (print "Entering  ttt_last_target_time")
	     (print syl))
      )
  (let ((target (ttt_last_target syl)))
    (if (null? target)
	nil
	(item.feat target "R:Target.daughter1.f0"))))

;; Changed to scan through segments in the segment relation,
;; to catch (notional) targets on pauses.  - MDS
;;
;;; associated segments are:
;;; - the segments in the word
;;; - subsequent segments not in the syllable structure
;;; and on the first word, preceding segments
;;; not in the syllable structure 

(define (ttt_collect_following seg accum)
  (if (or (null? seg)
	  (not (null? (item.relation seg 'SylStructure))))
      accum
      (ttt_collect_following (item.next seg) 
			     (cons seg accum))))


(define (ttt_last_target syl)
  "Returns the most recent previous target 
in the utterance or nil if there is not one present
"
(if (>= printdebug  3)
    (begin (print "Entering  ttt_last_target")
    (print syl))
    )
  (let ((prev_syl (item.relation.prev syl 'Syllable)))
    (cond
;     ((symbol-bound? 'new_targets) (last (caar new_targets)))
     ((null prev_syl) nil)
     ((ttt_last_target_segs 
       (ttt_collect_following 
	(item.relation.next 
	 (item.relation.daughtern prev_syl "SylStructure")
	 "Segment")
	(reverse (item.relation.daughters prev_syl "SylStructure")))))
					;list of segments of prev. syllable
					;in reverse order, with pauses
					;prepended.
     (t (ttt_last_target prev_syl)))))

(define (ttt_last_target_segs segs)
  "Returns the first target no earlier than seg
or nil if there is not one
"
(if (>= printdebug  4)
    (begin (print "Entering  ttt_last_target_segs with:")
	   (pprintf (format nil "%l" segs))
))
  (cond
   ((null segs) nil)
   ((and  (> (parse-number 
	      (item.feat  (car segs) "R:Target.daughter1.f0")) 0)
	  (eq 0 (item.feat (car segs) "R:SylStructure.parent.lisp_lh_condition"))
	  (eq 0 (item.feat (car segs) "R:SylStructure.parent.lisp_hl_condition"))
	  (eq 0 (item.feat (car segs) "R:SylStructure.parent.lisp_valley_condition")))
    (car segs))
   
   (t (ttt_last_target_segs (cdr segs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
;;;;;; CART TREES                           (ttt - tobi to target)
;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Return a list of target lists. A target list comprises of a list
;;; of related targets (ie for the L and H in L+H*), just to confuse
;;; matters each target is also a list! (pos pitch)
;;;


(set! ttt_endtone_tree  ; BUG: does it check the current syl for last accent?
      '
      ((tobi_endtone is NONE)        ; ususally none
       ((((NONE))))
       ((tobi_endtone is "H-H%")     ; H-H%
	((((100 120))))
	((tobi_endtone is "L-L%")    ; L-L%
	 ((((100 -20))))
	 ((tobi_endtone is "L-H%")   ; L-H%
	  ((lisp_last_accent > 2)
	   ((lisp_last_accent_type is "L*") 
	    ((((0 25) (100 40))))    ; paper says 80 but AWB had 40
	    ((((0 0) (100 40)))))
	   ((lisp_last_accent_type is "L*")
	    ((((100 40))))
	    ((((50 0) (100 40))))))
	  ((tobi_endtone is "H-L%")  ; H-L%
	   ((lisp_last_accent_type is "L*")
	    ((tobi_accent is"L*")
	     ((((50 100) (100 100))))
	     ((((0 100) (100 100)))))
	    ((((100 100)))))
	  ((tobi_endtone is "!H-L%")  ; !H-L%
	   ((lisp_last_accent_type is "L*")
	    ((tobi_accent is"L*")
	     ((((50 DHIGH) (100 100))))
	     ((((0 DHIGH) (100 100)))))
	    ((((100 DHIGH)))))
	   ((tobi_endtone is "H-")
	    ((((100 100))))
	    ((tobi_endtone is "!H-")
	     ((((100 DHIGH))))
	     ((tobi_endtone is "L-")
	      ((((100 0))))
	      ((((UNKNOWN))))))))))))))

(set! ttt_starttone_tree
      '
      ((lisp_ip_initial = 1)
       ((tobi_endtone is "%H")
	((((0 100))))
	((p.tobi_endtone in ("H-" "!H-" "L-"))
	 ((((TAKEOVER))))       ; takeover case
	 ((tobi_accent is NONE)  
	  ((lisp_next_accent > 2) ; default cases  (dep. on whether next target is low)
	   ((lisp_next_accent_type in ("L*" "L*+H" "L*+!H" "L+H*" "L+!H*" "L-" "L-H%" "L-L%"))
	    ((((0 50)(100 25))))
	    ((((0 50)(100 75)))))
	   ((lisp_next_accent_type in ("L*" "L*+H" "L*+!H" "L+H*" "L+!H*" "L-" "L-H%" "L-L%"))
	    ((((0 30))))
	    ((((0 70))))))
	  ((tobi_accent in ("L*" "L*+H" "L*+!H" "L+H*" "L+!H*" "L-" "L-H%" "L-L%"))
	    ((((0 30))))
	    ((((0 70))))))))
       ((((NONE))))))     ; otherwise (and usually) nothing.  

;; Redone after Jilka, Moehler and Dogil
;; - But treating one-syllable-ip's like
;; last-syllable-of-ip's in cases of 
;; two tone switches per syllable (e.g. H* L-H%). 
;; - And (hack) a 70% target for the initial 
;; H*'s of phrases when the next accent is L+H*
;; - MDS

(set! ttt_accent_tree
      '
      ((tobi_accent is "H*" )    ; H*
       ((lisp_ip_final = 1)
	((lisp_ip_one_syllable_case = 1)
	 ((((50 100))))
	 ((((25 100)))))
	((lisp_hstar_weak_target = 1)
	 ((((60 70))))
	 ((lisp_ip_initial = 1) 
	  ((((85 100))))
	  ((((60 100)))))))

      ((tobi_accent is "!H*" )    ; !H*
       ((lisp_ip_final = 1)
	((lisp_ip_one_syllable_case = 1)
	 ((((50 DHIGH))))
	 ((((25 DHIGH)))))
       ((lisp_ip_initial = 1) 
	((((85 DHIGH))))
	((((60 DHIGH))))))

	((tobi_accent is "L*" )    ; L*
	 ((lisp_ip_final = 1)
	  ((lisp_ip_one_syllable_case = 1)
	   ((((50 0))))
	   ((((25 0)))))
	  ((lisp_ip_initial = 1) 
	   ((((85 0))))
	   ((((60 0))))))

	((tobi_accent is "L+H*")   ; L+H*
	 ((lisp_ip_final = 1)
	  ((lisp_ip_one_syllable_case = 1)
	   ((((PRE 20) (50 100))))  ; JMD estimated 70
	   ((((PRE 20) (25 100)))))
	  ((lisp_ip_initial = 1) 
	   ((((PRE 20) (90 100))))
	   ((((PRE 20) (75 100))))))

	 ((tobi_accent is "L+!H*")   ; L+!H*
	 ((lisp_ip_final = 1)
	  ((lisp_ip_one_syllable_case = 1)
	   ((((PRE 20) (70 DHIGH))))
	   ((((PRE 20) (25 DHIGH)))))
	  ((lisp_ip_initial = 1) 
	   ((((PRE 20) (90 DHIGH))))
	   ((((PRE 20) (75 DHIGH))))))

	  ((tobi_accent is "L*+H")   ; L*+H
	   ((lisp_ip_final = 1)
	    ((lisp_ip_one_syllable_case = 1)
	     ((((35 0) (80 100))))     ; POST would clobber endtones
	     ((((15 0) (40 100)))))    ; POST would clobber endtones - MDS
	    ((lisp_ip_initial = 1) 
	     ((((55 0) (POST 100))))
	     ((((40 0) (POST 100))))))

	  ((tobi_accent is "L*+!H")   ; L*+!H
	   ((lisp_ip_final = 1)
	    ((lisp_ip_one_syllable_case = 1)
	     ((((35 0) (80 DHIGH))))    ; POST would clobber endtones - MDS
	     ((((15 0) (40 DHIGH)))))   ; POST would clobber endtones - MDS
	    ((lisp_ip_initial = 1) 
	     ((((55 0) (POST DHIGH))))
	     ((((40 0) (POST DHIGH))))))

	   ((tobi_accent is "H+!H*")    ; H+!H* 
	    ((lisp_ip_final = 1)
	     ((lisp_ip_one_syllable_case = 1)
	      ((((PRE 143) (60 DHIGH)))) ; the 143 is a hack to level out the downstep
	      ((((PRE 143) (20 DHIGH)))))
	     ((lisp_ip_initial = 1) 
	      ((((PRE 143) (90 DHIGH))))
	      ((((PRE 143) (60 DHIGH))))))

	    ((lisp_lh_condition = 1) 
	     ((((100 75))))
	     ((lisp_lh_condition = 2)
	      ((((0 90))))    
	      ((lisp_hl_condition = 1)
	       ((((100 25))))
	       ((lisp_valley_condition = 1)
		((((V1 85))))
		((lisp_valley_condition = 2)
		 ((((V2 70))))
		 ((lisp_valley_condition = 3)
		  ((((V3 70))))
		  ((tobi_accent is NONE)   ; usually we find no accent
		   ((((NONE))))
		   ((((UNKNOWN))))))))))))))))))))     ; UNKNOWN TARGET FOUND

;;; Cart tree to "predict" pitch range
;;; Right now just accesses a feature
;;; "register" following Moehler & Mayer 2001.
;;; Register must be one of
;;;   H    - primary high register (default): 133% lowest, 92% highest
;;;   H-H  - expanded high register: 134% lowest, 100% highest
;;;   H-L  - lowered high register: 128% lowest, 87% highest
;;;   L    - primary low register: 100% lowest, 73% highest
;;;   L-L  and HL-L - low compressed: 100% lowest, 66% highest
;;;   HL   - expanded register:   100% lowest, 84% highest
;;;   HL-H - complete register:   100% lowest, 96% highest
;;; For their speaker, ,BASELINE was 42% of PEAK

(set! ttt_topline_tree 
      '
      ((R:SylStructure.parent.register is "H")
       (92)
       ((R:SylStructure.parent.register is "H-H")
	(100)
	((R:SylStructure.parent.register is "H-L")
	 (87)
	 ((R:SylStructure.parent.register is "L")
	  (73)
	  ((R:SylStructure.parent.register is "L-L")
	   (66)
	   ((R:SylStructure.parent.register is "HL")
	    (84)
	    ((R:SylStructure.parent.register is "HL-H")
	     (96)
	     (92)))))))))

(set! ttt_baseline_tree 
      '
      ((R:SylStructure.parent.register is "H")
       (133)
       ((R:SylStructure.parent.register is "H-H")
	(134)
	((R:SylStructure.parent.register is "H-L")
	 (128)
	 ((R:SylStructure.parent.register is "L")
	  (100)
	  ((R:SylStructure.parent.register is "L-L")
	   (100)
	   ((R:SylStructure.parent.register is "HL")
	    (100)
	    ((R:SylStructure.parent.register is "HL-H")
	     (100)
	     (133)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
;;;;;;   Lisp Feature functions.
;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (valley_condition syl)
"(valley_condition syl)
Function to determine if a lowered target between two high target points
is needed in this syllable.
Returns:  0 - no target required
          1 - the single target case
          2 - the first of the two target case
          3 - the second of the two target case
"
(if (>= printdebug  4)
    (begin (print "Entering valley_condition")))
(cond
 ((and (eq 0 (item.feat syl 'accented))
       (string-matches (next_accent_type syl)
		       "\\(H\\*\\|H\\-\\|H\\-L\\%\\|H\\-H\\%\\|\\!H\\*\\|\\!H\\-\\|\\!H\\-L\\%\\|\\!H\\-H\\%\\)")
       (string-matches (last_accent_type syl)
		       "\\(H\\*\\|L\\+H\\*\\|L\\*\\+H\\\\|\\!H\\*\\|L\\+\\!H\\*\\|L\\*\\+\\!H\\)")) 
                       ;GM: excluded %H (returns nil for last target)
  (let ((nas (next_accent_start syl))
	(syls (item.feat syl 'syllable_start))
	(syle (item.feat syl 'syllable_end))
	(las (ttt_last_target_time syl)))
    (if (>= printdebug  3)
	(begin (print (format nil "nas: %l syls: %l syle %l las %l" nas syls syle las))))
    (cond
     ((and (< (- nas las) 0.5)
	   (> (- nas las) 0.25)
	   (< syls (+ (/ (- nas las) 2.0) (ttt_last_target_time syl)))
	   (> syle (+ (/ (- nas las) 2.0) (ttt_last_target_time syl)))) 1)
     ((and (> (- nas las) 0.5)
	   (< syls (+ (ttt_last_target_time syl) 0.25))
	   (> syle (+ (ttt_last_target_time syl) 0.25))) 2)
     ((and (> (- nas las) 0.5)
	   (< syls (- nas 0.25))
	   (> syle (- nas 0.25))) 3)
     (t 0))))
 (t 0))) 
   
       

(define (lh_condition syl)
"(lh_condition syl)
Function to determine the need for extra target points between an L and an H
Returns: 1 - first extra target required
         2 - second extra target required
         0 - no target required.
"
(if (>= printdebug  4)
    (begin (print "Entering LH_condition")))
(cond
 ((and (eq 0 (item.feat syl 'accented))
       (string-matches (last_accent_type syl) "\\(L\\*\\)")
       (string-matches (next_accent_type syl)
		       "\\(H\\*\\|H\\-\\|H\\-L\\%\\|H\\-H\\%\\)"))
  (cond
   ((and (eq 1 (last_accent syl))
	 (< 2 (next_accent syl))) 1)
   ((and (< 2 (last_accent syl))
	 (eq 1 (next_accent syl))) 2)
   (t 0)))
 (t 0)))

(define (hl_condition syl)
"(lh_condition syl)
Function to determine the need for extra target points between an H and an L
Returns: 1 - extra target required
         0 - no target required.
"
(if (>= printdebug  4) 
    (begin (print "Entering HL_condition")))
(cond
 ((and (eq 0 (item.feat syl 'accented))
       (string-matches (next_accent_type syl)
           "\\(L\\*\\|L\\+H\\*\\|L\\+\\!H\\*\\|L\\*\\+H\\|L\\*\\+!H\\|L\\-\\|L\\-L\\%\\|L-H\\%\\)")
       (string-matches (last_accent_type syl)
		       "\\(H\\*\\|L\\+H\\*\\|L\\*\\+H\\\\|\\!H\\*\\|L\\+\\!H\\*\\|L\\*\\+\\!H\\|\\%H\\)")
                       ;MDS: added !H's
       (eq 1 (last_accent syl))

       ;; fall faster! -MDS
       (<= 2 (next_accent syl))) 1)
 (t 0)))

(define (next_accent syl)
"(next_accent syl)
Wrapper for c++ func ff_next_accent.
Returns the number of the syllables to the next accent in the following format.
0 - no next accent
1 - next syllable
2 - next next syllable
etc..."
(if (>= printdebug  4) 
    (begin (print "Entering next_accent")))
(cond
 ((eq 0 (next_accent_type syl)) 0)
 (t (+ (item.feat syl 'next_accent) 1))))

;; Fixed bug that crashed complex phrase tones. - MDS
;; Not sure how else to get a big number...
(define infinity (/ 1 0))

;; Modified to include current accent as well -MDS

(define (last_accent syl)
"(last_accent syl)
Wrapper for c++ func ff_last_accent.
Returns the number of the syllables to the previous accent in the following format.
0 - accent on current syllable
1 - prev syllable
2 - prev to prev syllable
etc...
infinity - no previous syllable"
(if (>= printdebug  4) 
    (begin (print "Entering last_accent")))
(cond
 ((not (equal? "NONE" (item.feat syl 'tobi_accent))) 0)
 ((equal? 0 (last_accent_type syl)) infinity)
 (t (+  (item.feat syl 'last_accent) 1))))

(define (next_accent_type syl)
"(next_accent_type syl)
Returns the type of the next accent."
(cond 
 ((not (eq 0 (item.feat syl "n.R:Intonation.daughter1.name")))
  (item.feat syl "n.R:Intonation.daughter1.name"))
 ((eq 0 (item.feat syl 'syl_out)) 0)  ;;GM real ip_final would be better
 (t (next_accent_type (item.relation.next syl 'Syllable)))))

(define (last_accent_type syl)
"(last_accent_type syl)
Returns the type of the last (previous)  accent."
(if (>= printdebug  4) 
    (begin (print "Entering last_accent_type")))
(cond
  ((not (equal? "NONE"  (item.feat syl 'p.tobi_endtone)))
   (item.feat syl 'R:Syllable.p.tobi_endtone))
  ((not (equal? "NONE"  (item.feat syl 'p.tobi_accent)))
   (item.feat syl 'R:Syllable.p.tobi_accent))
  ((eq 0 (item.feat syl 'syl_in)) 0)  ;;GM real ip_initial would be better
  (t (last_accent_type (item.prev syl 'Syllable)))))

(define (next_accent_start syl)
"(next_accent_start syl)
Returns the start time  of the vowel of next accented syllable"
(if (>= printdebug 4) 
    (begin (print "Entering next_accent_start")))
(cond 
 ((not (eq 0 (item.feat syl "n.R:Intonation.daughter1.name")))
  (item.feat syl "R:Syllable.n.syllable_start")) ;;GM vowel start would be better
 ((eq 0 (item.feat syl 'syl_out)) 0)
 (t (next_accent_start (item.relation.next syl 'Syllable)))))

; new features (not used yet)

(define (ip_final syl)
  "(ip_final SYL)
  returns 1 if the syllable is the final syllable of an 
  ip (intermediate phrase)"
  (cond  
   ((or (equal? 0 (item.feat syl "syl_out"))
       (equal? "L-" (item.feat syl "tobi_endtone"))
       (equal? "H-" (item.feat syl "tobi_endtone"))
       (equal? "!H-" (item.feat syl "tobi_endtone"))) 1)
   (t 0)))

(define (ip_initial syl)
  "(ip_initial SYL)
  returns 1 if the syllable is the initial syllable of an 
  ip (intermediate phrase)"
  (cond
   ((equal? 0 (item.feat syl "syl_in"))
    1)
   ((equal? 1 (ip_final (item.relation.prev syl 'Syllable)))
    1)
   (t 0)))

;; NEXT TWO FUNCTIONS ARE NEW - MDS
(define (ip_one_syllable_case syl)
  "(ip_one_syllable_case SYL)
  returns true if the syllable is the initial syllable of an 
  ip (intermediate phrase) and doesn't itself contain a complex
  tone that starts opposite this syllable's accent"
  (if (eqv? 0 (ip_initial syl))
      0
      (let ((accent (item.feat syl "tobi_accent"))
	    (tone (item.feat syl "tobi_endtone")))
	(cond
	  ((and (equal? tone "L-H%")
		(or (equal? accent "H*")
		    (equal? accent "!H*")
		    (equal? accent "L+H*")
		    (equal? accent "L+!H*")
		    (equal? accent "L*+H")
		    (equal? accent "L*+!H*")
		    (equal? accent "H+!H*")))
	   0)
	  ((and (or (equal? tone "H-L%")
		    (equal? tone "!H-L%"))
		(equal? accent "L*"))
	   0)
	  (t
	   1)))))

(define (hstar_weak_target syl)
  (if (and (equal? 0 (item.feat syl 'asyl_in))
	   (member (next_accent_type syl)
		   (list "L*" "L*+H" "L*+!H" "L+H*" "L+!H*")))
      1
      0))
       
(provide 'tobi_rules)
