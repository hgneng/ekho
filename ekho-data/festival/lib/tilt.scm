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
;;;          Author: Alan W Black, Kurt Dusterhoff, Janet Hitzeman
;;;          Date: April 1999
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tilt intonation modules, accent/boundary preditions and F0 generation
;;;   The F0 generation is done using models as described in 
;;;   Dusterhoff, K. and Black, A. (1997). "Generating F0 contours for 
;;;   speech synthesis using the Tilt intonation theory"
;;;   (http://www.cstr.ed.ac.uk/awb/papers/esca-int97.ps) 
;;;   Proceedings of ESCA Workshop of Intonation, pp 107-110, September, 
;;;   Athens, Greece.
;;;
;;;   Intonation_Tilt assigns accents and boundaries by a CART tree
;;;   the c and sil nodes are derived directly duration creation
;;;
;;;   Int_Targets_Tilt generates the F0 using the CART trees as
;;;   described in the paper referenced above.
;;;
;;;   THIS CONTAINS *VERY* EXPERIMENTAL CODE
;;;   it requires a thoroughly clean up and probably split into
;;;   multiple files

(defvar int_tilt_params nil
  "int_tilt_params
Parameters for tilt intonation model.")

(Parameter.def 'tilt_method 'cart)

(define (Intonation_Tilt utt)
  "(Intonation_Tilt utt)
Assign accent and boundary IntEvents to each syllable, and fill in
spaces with silence and connections."
 (let (accent boundary)
   ;; Create basic intonation relations
   (utt.relation.create utt 'Intonation)
   (utt.relation.create utt 'IntonationSyllable)
   (mapcar
    (lambda (syl)
      ;; If first syllable in phrase add phrase_start
      (if (string-equal "pau"
	   (item.feat syl "R:SylStructure.daughter1_to.Segment.p.name"))
	  (tilt_add_intevent utt syl 'phrase_start))

      (set! accent (wagon_predict syl tilt_a_cart_tree))
      (set! boundary (wagon_predict syl tilt_b_cart_tree))
;      (format t "%s: accent %s boundary %s\n"
;	      (item.feat syl "R:WordStructure.root.name")
;	      accent
;	      boundary)
      (if (not (string-equal accent "0"))
	  (tilt_add_intevent utt syl accent))
      (if (not (string-equal boundary "0"))
	  (if (and (string-equal boundary "afb")
		   (not (string-equal accent "0")))
	      (tilt_add_intevent utt syl "fb")  ;; can't have a/afb
	      (tilt_add_intevent utt syl boundary)))

      ;; If last syllable in phrase add phrase_end
      (if (string-equal "pau"
  	   (item.feat syl "R:SylStructure.daughtern_to.Segment.n.name"))
	  (tilt_add_intevent utt syl 'phrase_end)))
    (utt.relation.items utt 'Syllable))
;;   (utt.relation.print utt 'Intonation)
   utt))

(define (tilt_add_intevent utt syl name)
"(tilt_add_intevent utt syl name)
Add a new IntEvent related to syl with name."
  (let (ie)
    (set! ie (utt.relation.append utt 'Intonation (list name)))
    (if (not (item.relation syl 'IntonationSyllable))
	(utt.relation.append utt 'IntonationSyllable syl))
    (item.relation.append_daughter syl 'IntonationSyllable ie)
    (if (not (string-matches name "phrase_.*"))
	(item.set_feat ie "int_event" 1))
    ie))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Fo generate through tilt parameters and F0 rendering
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (Int_Targets_Tilt utt)
  "(Int_Targets_Tilt utt)
Assign Tilt parameters to each IntEvent and then generate the 
F0 contour and assign targets."
  (utt.relation.set_feat utt "Intonation" "intonation_style" "tilt")
  (tilt_assign_parameters utt)
;  (tilt_F0_and_targets utt)  ;; this has to be C++, sorry
;  (tilt_map_f0_range utt)
  (tilt_to_f0 utt "f0")
  (tilt_validate utt)
  utt
)

(define (tilt_validate utt)
  "(tilt_validate utt)
Checks that the predicted tilt parameter fall with reasonable
limits and modify them where possible to be more reasonable."
  (mapcar
   (lambda (ie)
     (cond
      ((string-equal (item.name ie) "phrase_end")
       ;; check previous event does overflow segments
       )
      (t
       t))
     )
   (utt.relation.items utt 'Intonation))
)

(define (tilt_map_f0_range utt)
  "(tilt_map_f0_range utt)
In order fo better trained models to be used for voices which don't
have the necessary data to train models from the targets may be mapped
to a different pitch range.  Note this is not optimal as pitch ranges
don't map that easily, but the the results can sometimes be better than
using a less sophisticated F0 generation model.  The method used
is to define the mean and standard deviation of the speaker the
model was trained on and the mean and standard deciation of the
desired speaker.  Mapping is by converting the actual F0 value
to zscores (distance from mean in number of stddev) and back into
the other domain.  The variable int_tilt_params is used to find
the values."
  (let ((target_f0_mean (car (cdr (assoc 'target_f0_mean int_tilt_params))))
	(target_f0_std (car (cdr (assoc 'target_f0_std int_tilt_params))))
	(model_f0_std (car (cdr (assoc 'model_f0_std int_tilt_params))))
	(model_f0_mean (car (cdr (assoc 'model_f0_mean int_tilt_params)))))
    (if target_f0_mean  ;; only if one is specified
	 (lambda (targ)
	   (item.set_name
	    targ
	    (+ target_f0_mean
	       (* target_f0_std
		  (/ (- (parse-number (item.name targ))
			model_f0_mean)
		     model_f0_std)))))
	 (utt.relation.leafs utt 'Target))))

(define (tilt_assign_parameters utt) 
  "(tilt_assign_parameters utt)
Assigned tilt parameters to IntEvents, depending on the value
of the Parameter tilt_method uses wagon trees (cart) or linear
regression models (lr)."
  (let ((method (Parameter.get 'tilt_method)))
  (cond
   ((equal? method 'cart)
    (tilt_assign_parameters_wagon utt))
   ((equal? method 'lr)
    (tilt_assign_parameters_lr utt))
   (t
    (error "Tilt: unknown tilt param prediction method: " tilt_method)))))

(define (tilt_assign_parameters_wagon utt)
 "(tilt_assign_parameters_wagon utt)
Assing parameters (start_f0, tilt, amplitude, peak_pos and duration)
to each IntEvent.  Uses Wagon trees to predict values"
  (mapcar
   (lambda (ie)
     (let ((param_trees (cdr (assoc_string (item.name ie)
					   tilt_param_trees))))
       (item.set_feat ie "time_path" "IntonationSyllable")
       (if (string-equal "1" (item.feat ie "int_event"))
	   (item.set_function ie "time" "unisyn_tilt_event_position")
	   (item.set_function ie "time" "unisyn_tilt_phrase_position"))
       (cond
	((null param_trees)  
	 (format stderr "Tilt: unknown Intonation type %s, ignored\n"
		 (item.name ie))
	 ;; *need* to assign default values 
	 (item.set_feat ie "ev.f0" 100)
	 (item.set_feat ie "tilt.amp" 20.0)
	 (item.set_feat ie "tilt.dur" 0.25)
	 (item.set_feat ie "tilt.tilt" -0.2)
	 (item.set_feat ie "rel_pos" 0.0)
	 )
	(t
	 (tilt_assign_params_wagon ie param_trees)))))
   (utt.relation.items utt 'Intonation)))

(define (tilt_assign_params_wagon ie trees)
  "(tilt_assign_params_wagon ie trees)
Assign the names parameters to ie using the trees and names in
trees."
  (mapcar
   (lambda (tree)
     (let ((val (wagon_predict ie (car (cdr tree)))))
       (item.set_feat ie (car tree) val)))
   trees))

(define (tilt_assign_parameters_lr utt)
  "(tilt_assign_parameters_lr utt)
Assing parameters (start_f0, tilt, amplitude, peak_pos and duration)
to each IntEvent. Prediction by linear regression models"
  (mapcar
   (lambda (ie)
     (let ((param_lrmodels (cdr (assoc_string (item.name ie)
				    tilt_param_lrmodels))))
       (cond
	((null param_lrmodels)  
	 (format stderr "Tilt: unknown IntEvent type %s, ignored\n"
		 (item.name ie))
	 ;; *need* to assign default values 
	 (item.set_feat ie "ev.f0" 100)
	 (item.set_feat ie "tilt.amp" 20.0)
	 (item.set_feat ie "tilt.dur" 0.25)
	 (item.set_feat ie "tilt.tilt" -0.2)
	 (item.set_feat ie "rel_pos" 0.0)
	 )
	(t
	 (tilt_assign_params_lr ie param_lrmodels)))))
   (utt.relation.items utt 'IntEvent)))

(define (tilt_assign_params_lr ie lrmodels)
  "(tilt_assign_params_lr ie lrmodels)
Assign the names parameters to ie using the trees and names in
trees."
  (mapcar
   (lambda (lrm)
     (let ((val (lr_predict ie (cdr lrm))))
       (item.set_feat ie (car lrm) val)))
   lrmodels))

(define (utt.save.tilt_events utt filename)
"(utt.save.til_events UTT FILENAME)
Save tilt events in UTT to FILENAME in a format suitable for
ev_synth."
  (let ((fd (fopen filename "w")))
    (format fd "#\n")
    (mapcar
     (lambda (ie)
       (let ((name (item.name ie)))
	 (cond
	  ((or (string-equal name "sil")
	       (string-equal name "c"))
	   (format fd "   %2.4f   100 %s; tilt: %2.6f\n" 
		   (item.feat ie 'end)
		   name 
		   (item.feat ie "tilt_start_f0")))
	  (t ;; accent or boundary
	   (format fd "   %2.4f   100 %s; tilt: %2.6f %2.6f %2.6f %2.6f %2.6f\n" 
		   (item.feat ie 'end)
		   name 
		   (item.feat ie "ev.f0")
		   (item.feat ie "tilt.amp")
		   (item.feat ie "tilt.dur")
		   (item.feat ie "tilt.tilt")
		   (item.feat ie "rel_pos"))))))
     (utt.relation.items utt 'IntEvent))
    (fclose fd)
    utt))


;;;;;
;;;  Some features which should be pruned
;;;;;

(def_feature_docstring 'Syllable.lisp_time_to_next_vowel
  "Syllable.lisp_time_to_next_vowel syl
  The time from vowel_start to next vowel_start")
(define (time_to_next_vowel syl)
  "(time_to_next_vowel syl)
  The time from vowel_start to next vowel_start"
  (let (ttnv)
    (if (string-equal "0" (item.feat syl "n.vowel_start"))
	(set! ttnv 0.00)
	(set! ttnv (- (item.feat syl "n.vowel_start")
		      (item.feat syl "vowel_start"))))
    ttnv))

(def_feature_docstring 'Syllable.lisp_next_stress
  "Syllable.lisp_next_stress
  Number of syllables to next stressed syllable. 0 if this syllable is
  stressed.  It is effectively assumed the syllable after the last syllable
  is stressed.")
(define (next_stress syl)
  (cond 
   ((null syl) 0)
   ((string-equal (item.feat syl 'stress_num) "1")
    0)
   (t
    (+ 1 (next_stress (item.relation.next syl 'Syllable))))))

(def_feature_docstring 'Syllable.lisp_last_stress
  "Syllable.lisp_last_stress
  Number of syllables from previous stressed syllable.  0 if this syllable
  is stressed.  It is effectively assumed that the syllable before the 
  first syllable is stressed.")
(define (last_stress syl)
  (cond 
   ((null syl) 0)
   ((string-equal (item.feat syl 'stress_num) "1")
    0)
   (t
    (+ 1 (last_stress (item.relation.prev syl 'Syllable))))))


(def_feature_docstring 'SylStructure.lisp_length_to_last_seg
  "SylStructure.lisp_length_to_last_seg
  Length from start of the vowel to start of last segment of syllable.")
(define (length_to_last_seg syl)
  (- (item.feat syl "daughtern_to.Segment.start")
     (item.feat syl "vowel_start")))

(def_feature_docstring 'SylStructure.lisp_get_rhyme_length
  "Syllable.lisp_get_rhyme_length
  Length from start of the vowel to end of syllable.")
(define (get_rhyme_length syl)
  (- (item.feat syl 'end)
     (item.feat syl 'vowel_start syl)))

(def_feature_docstring 'SylStructure.lisp_get_onset_length
  "Syllable.lisp_get_onset_length
  Length from start of syllable to start of vowel.")
(define (get_onset_length syl)
  (cond
   ((< (- (item.feat syl 'vowel_start)
	  (item.feat syl 'start))
       0.000)
    0.000)  ;; just in case
   (t
    (- (item.feat syl 'vowel_start)
       (item.feat syl 'start)))))

(def_feature_docstring 'Syllable.lisp_tilt_accent
  "Syllable.lisp_tilt_accent
  Returns \"a\" if there is a tilt accent related to this syllable, 0 
  otherwise.")
(define (tilt_accent syl)
  (let ((events (item.relation.daughters syl 'IntonationSyllable))
	(r "0"))
    (mapcar
     (lambda (i)
       (if (member_string (item.name i) tilt_accent_list)
	   (set! r "a")))
     events)
    r))

(def_feature_docstring 'Syllable.lisp_tilt_boundary
  "Syllable.lisp_tilt_boundary
  Returns boundary label if there is a tilt boundary related to this 
syllable, 0 otherwise.")
(define (tilt_boundary syl)
  (let ((events (item.relation.daughters syl 'IntonationSyllable))
	(r "0"))
    (mapcar
     (lambda (i)
       (let ((name (item.name i)))
       (if (member_string name tilt_boundary_list)
	   (cond
	    ((string-matches name "a.*")
	     (set! r (string-after name "a")))
	    ((string-matches name "m.*")
	     (set! r (string-after name "m")))
	    (t
	     (set! r name))))))
     events)
    r))

(def_feature_docstring 'Syllable.lisp_tilt_accented
  "Syllable.lisp_tilt_accented
  Returns 1 if there is a tilt accent related to this syllable, 0 
  otherwise.")
(define (tilt_accented syl)
  (let ((events (item.relation.daughters syl 'IntonationSyllable))
	(r "0"))
    (mapcar
     (lambda (i)
       (if (member_string (item.name i) tilt_accent_list)
	   (set! r "1")))
     events)
    r))

(def_feature_docstring 'Syllable.lisp_tilt_boundaried
  "Syllable.lisp_tilt_boundaried
  Returns 1 if there is a tilt boundary related to this syllable, 0 
  otherwise.")
(define (tilt_boundaried syl)
  (let ((events (item.relation.daughters syl 'IntonationSyllable))
	(r "0"))
    (mapcar
     (lambda (i)
       (if (member_string (item.name i) tilt_boundary_list)
	   (set! r "1")))
     events)
    r))

(def_feature_docstring 'SylStructure.lisp_vowel_height
  "SylStructure.lisp_vowel_height syl
Classifies vowels as high, low or mid")
(define (vowel_height syl)
  (let ((vh (item.feat syl "daughtern.daughter1.daughter1.df.height")))
    vh)
)

(def_feature_docstring 'SylStructure.lisp_vowel_frontness
  "SylStructure.vowel_frontness syl
Classifies vowels as front, back or mid")
(define (vowel_frontness syl)
  (let ((vf (item.feat syl "daughtern.daughter1.daughter1.df.front")))
    vf)
)

(def_feature_docstring 'SylStructure.lisp_vowel_length
  "SylStructure.vowel_length syl
Returns the df.length feature of a syllable's vowel")
(define (vowel_length syl)
  (let ((vl (item.feat syl "daughtern.daughter1.daughter1.df.length")))
    vl)
)

(defvar sonority_vless_obst '("f" "h" "hh" "k" "p" "s" "sh" "t" "th" "ch")
  "sonority_vless_obst
List of voiceless obstruents for use in sonority scaling (only good w/ radio_speech)"
  )
(defvar sonority_v_obst '("v" "b" "g" "z" "zh" "d" "dh" "jh")
  "sonority_v_obst
List of voiced obstruents for use in sonority scaling (only good w/ radio_speech)"
  )
(defvar sonority_nas '("m" "n" "ng" "nx" "em" "en")
  "sonority_nas
List of nasals (only good w/ radio_speech)"
  )
(defvar sonority_liq '("r" "l" "er" "el" "axr")
  "sonority_liq
List of liquids (only good w/ radio_speech)"
  )
(defvar sonority_glides '("y" "w")
  "sonority_glides
List of glides (only good w/ radio_speech)"
  )

(def_feature_docstring 'SylStructure.lisp_sonority_scale_coda
  "SylStructure.sonority_scale_coda syl
Returns value on sonority scale (1 -6, where 6 is most sonorous)
for the coda of a syllable, based on least sonorant portion.")
(define (sonority_scale_coda syl)
  (let ((segs (item.daughters (item.daughtern (item.daughtern syl))))
	(scale 6))
    (mapcar
     (lambda (seg)
       (cond
	((member_string (item.name seg) sonority_vless_obst)
	 (if (> scale 1)
	     (set! scale 1)))
	((member_string (item.name seg) sonority_v_obst)
	 (if (> scale 2)
	     (set! scale 2)))
	((member_string (item.name seg) sonority_nas)
	 (if (> scale 3)
	     (set! scale 3)))
	((member_string (item.name seg) sonority_liq)
	 (if (> scale 4)
	     (set! scale 4)))
	((member_string (item.name seg) sonority_glides)
	 (if (> scale 5)
	     (set! scale 5)))
	(t
	 (if (> scale 6)
	     (set! scale 6)))
	)
       )
    segs)
  scale))

(def_feature_docstring 'SylStructure.lisp_sonority_scale_onset
  "SylStructure.sonority_scale_onset syl
Returns value on sonority scale (1 -6, where 6 is most sonorous)
for the onset of a syllable, based on least sonorant portion.")
(define (sonority_scale_onset syl)
  (if (string-equal "Onset" (item.feat (item.daughter1 syl) "sylval"))
      (let ((segs (item.daughters (item.daughter1 syl)))
	    (scale 6))
	(mapcar
	 (lambda (seg)
	   (cond
	    ((member_string (item.name seg) sonority_vless_obst)
	     (if (> scale 1)
		 (set! scale 1)))
	    ((member_string (item.name seg) sonority_v_obst)
	     (if (> scale 2)
		 (set! scale 2)))
	    ((member_string (item.name seg) sonority_nas)
	     (if (> scale 3)
		 (set! scale 3)))
	    ((member_string (item.name seg) sonority_liq)
	     (if (> scale 4)
		 (set! scale 4)))
	    ((member_string (item.name seg) sonority_glides)
	     (if (> scale 5)
		 (set! scale 5)))
	    (t (set! scale 6))
	    )
	   )
	 segs)
	scale)
      0))

(def_feature_docstring 'SylStructure.lisp_num_postvocalic_c
  "SylStructure.lisp_num_postvocalic_c
Finds the number of postvocalic consonants in a syllable.")
(define (num_postvocalic_c syl)
  "(num_postvocalic_c syl)
Finds the number of postvocalic consonants in a syllable."
  (let (segs (npc 0))
    (set! segs (item.daughters (item.daughtern (item.daughtern syl))))
    (mapcar
     (lambda (seg)
       (set! npc (+ npc 1))
       )
     segs)
    npc))


(def_feature_docstring 'SylStructure.lisp_syl_numphones
  "SylStructure.lisp_syl_numphones syl
Finds the number segments in a syllable.")
(define (syl_numphones syl)
  (length (mt_segs_from_syl syl))
  )

(def_feature_docstring 'Segment.lisp_pos_in_syl
  "Segment.lisp_pos_in_syl seg
Finds the position in a syllable of a segment - returns a number.")
(define (pos_in_syl seg)
  (let ((segments (mt_segs_from_syl 
		   (item.relation (item.parent_to
				   (item.relation seg 'SylStructure)
				   'Syllable)
				  'SylStructure)))
	(seg_count 1))
    (mapcar
     (lambda (s)
       (if (not (eqv? s seg))
	   (set! seg_count (+ 1.0 seg_count))
	   nil))
     segs)
    seg_count))

(def_feature_docstring 'Intonation.lisp_peak_anchor_segment_type
  "Intonation.peak_anchor_segment_type ie
Determines whether the segment anchor for a peak
is the first consonant of a syl - C0 -, the
vowel of a syl - V0 -, or segments after that
- C1->X,V1->X. If the segment is in a following syl,
the return value will be preceded by a 1 - e.g. 1V1")
(define (peak_anchor_segment_type ie)
  (let ( syl peak_anchor_num numsegs peak_anchor_type)
    (set! peak_anchor_num (peak_segment_anchor ie))


    (if (> 9 peak_anchor_num)
	(set! syl (item.relation
		   (item.parent (item.relation ie "IntonationSyllable")) 
		   "SylStructure")))
    (if (> 9 peak_anchor_num)
	(set! numsegs (item.feat syl "syl_numphones")))

    (cond
     ((< 9 peak_anchor_num)
      (set! peak_anchor_type "none"))
     ((> 0 peak_anchor_num)
      (set! peak_anchor_type
	    (string-append
	     "-1" (get_anchor_value (item.prev syl)
				    (+ peak_anchor_num 
				       (item.feat syl "p.syl_numphones"))))))
     ((< peak_anchor_num numsegs)
      (set! peak_anchor_type (get_anchor_value syl numsegs)))
     ((> peak_anchor_num numsegs)
      (set! peak_anchor_type
	    (string-append
	     "1" (get_anchor_value (item.next syl) (- peak_anchor_num numsegs)))))
      (set! peak_anchor_type "none"))
;    (format stderr "pat: %s\n" peak_anchor_type)
    peak_anchor_type))

(define (get_anchor_value sylSyl seg_num)
  "(get_anchor_value sylSyl seg_num)
Gets the c/v value of the segment within a syllable."
  (let ((syl (item.relation sylSyl "SylStructure"))
	(seg_val "none") segs (ccnt -1) (vcnt -1) (vpis 0))
    (set! segs (mt_segs_from_syl sylSyl))
    (mapcar
     (lambda (seg)
       (cond
	((string-equal "consonant" (item.feat seg "df.type"))
	   (set! vcnt (+ 1 vcnt))
	   (set! vpis (item.feat seg "pos_in_syl")))
	(t
	   (set! ccnt (+ 1 ccnt))))
       (cond
	((and
	  (eq (- seg_num 1.0) (item.feat seg "pos_in_syl"))
	  ( string-equal "consonant" (item.feat seg "df.type")))
	 (set! seg_val (string-append "V" vcnt)))
	((and
	  (eq (- seg_num 1.0) (item.feat seg "pos_in_syl"))
	  ( string-equal "vowel" (item.feat seg "df.type")))
	 (set! seg_val (string-append "C" (- (item.feat seg "pos_in_syl")
					     vpis) "V" vcnt)))
	(t nil))
       )
     segs)
  seg_val))

(define (peak_segment_anchor ie)
  "peak_segment_anchor ie
Determines what segment acts as the anchor for a peak.
Returns number of segments from start of accented syllable
to peak."
;  (format stderr "accent: %s\n"
;	  (item.name ie))
  (let ((pk_pos (item.feat ie "position"))
	(peak_seg_anchor 11))
    (if
     (or
       (string-equal "phrase_start" (item.name ie))
       (string-equal "phrase_end" (item.name ie))
       (string-equal "pause" (item.name ie)))
     (set! peak_seg_anchor 10)
     (set! peak_seg_anchor (find_peak_seg_anchor ie pk_pos)))
    peak_seg_anchor))

(define (find_peak_seg_anchor ie pk_pos)
  "find_peak_seg_anchor ie pk_pos
Part of the workings of peak_segment_anchor."
  (let (( syl (item.relation
		(item.parent (item.relation ie 'IntonationSyllable))
		'SylStructure))
	(seg_anchor 11))
    (cond
     ((not (eq 9.0 (segs_to_peak syl pk_pos)))
      (set! seg_anchor (segs_to_peak syl pk_pos)))

     ((and (item.prev syl)
	   (not (eq 9.0 (segs_to_peak (item.prev syl) pk_pos))))
;      (format stderr "%s\n" (item.name (item.prev syl)))
      (set! seg_anchor (* -1
			     (- (+ 1 (item.feat syl "p.syl_numphones"))
			     (segs_to_peak (item.prev syl) pk_pos)))))

     ((and (item.next syl)
	   (> pk_pos (item.feat syl "n.start")))
;      (format stderr "%s\n" (item.name (item.next syl)))
      (set! seg_anchor (+ 1 
			  (item.feat syl "syl_numphones")
			  (segs_to_peak (item.next syl) pk_pos))))
     (t
       (format stderr "No seg anchor could be found\n")))
;    (format stderr "seg_anchor: %f\n" seg_anchor)
    seg_anchor))

(define (segs_to_peak sylSyl pk_pos)
  "(segs_to_peak sylSyl pk_pos)
Determines the number of segments from the start of a syllable
to an intonation peak"
  (let ((syl (item.relation sylSyl "SylStructure"))
	(segs_2_peak 9) segs)
    (set! segs (mt_segs_from_syl syl))
    (mapcar
     (lambda (seg)
;    (format stderr "seg_end: %f  pk: %f\n" (item.feat seg "end")
;	     pk_pos)
     (if (eq 1.0 (peak_wi_seg seg pk_pos))
	 (set! segs_2_peak (item.feat seg "pos_in_syl")))
;     (format stderr "segs_2_peak: %f\n" segs_2_peak)
     )
     segs)
    segs_2_peak))

(define (peak_wi_seg segment pk_pos)
  "peak_wi_seg segment pk_pos
Finds if a peak occurs w/i a segment"
  (let ((s_start (item.feat segment "start"))
	(s_end (item.feat segment "end"))
	(ret 0.0))
    (if (and (< s_start pk_pos)
	     (< pk_pos s_end))
	(set! ret 1.0)
	nil)
    ret))

(defvar tilt_accent_list '("a" "arb" "afb" "m" "mfb" "mrb")
  "tilt_accent_list
List of events containing accents in tilt model.")
(defvar tilt_boundary_list '("rb" "arb" "afb" "fb" "mfb" "mrb")
  "tilt_boundary_list
List of events containing boundaries in tilt model.")

(def_feature_docstring 'Intonation.lisp_last_tilt_accent
  "Intonation.lisp_last_tilt_accent
  Returns the most recent tilt accent.")
(define (last_tilt_accent intev)
  (let ((pie (item.relation.prev intev 'Intonation)))
    (cond
     ((not pie)
      "0")
     ((member_string (item.name pie) tilt_accent_list)
      (item.name pie))
     (t (last_tilt_accent pie)))))

(def_feature_docstring 'Intonation.lisp_next_tilt_accent
  "Intonation.lisp_next_tilt_accent
  Returns the next tilt accent.")
(define (next_tilt_accent intev)
  (let ((nie (item.relation.next intev 'Intonation)))
    (cond
     ((not nie) "0")
     ((member_string (item.name nie) tilt_accent_list)
      (item.name nie))
     (t (next_tilt_accent nie)))))

(def_feature_docstring 'Intonation.lisp_last_tilt_boundary
  "Intonation.lisp_last_tilt_boundary
  Returns the most recent tilt boundary.")
(define (last_tilt_boundary intev)
  (let ((pie (item.relation.prev intev 'Intonation)))
    (cond
     ((not pie) "0")
     ((member_string (item.name pie) tilt_boundary_list)
      (item.name pie))
     (t (last_tilt_boundary pie)))))

(def_feature_docstring 'Intonation.lisp_next_tilt_boundary
  "Intonation.lisp_next_tilt_boundary
  Returns the next tilt boundary.")
(define (next_tilt_boundary intev)
  (let ((nie (item.relation.next intev 'Intonation)))
    (cond
     ((not nie) "0")
     ((member_string (item.name nie) tilt_boundary_list)
      (item.name nie))
     (t (next_tilt_boundary nie)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Some basic function to metrical tree structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (mt_syl_from_seg seg)
  (if seg
      (item.root (item.relation seg 'SylStructure))
      nil))
(define (mt_word_from_syl syl)
  (if syl
      (item.root (item.relation syl 'WordStructure))
      nil))
(define (mt_word_from_seg seg)
  (mt_word_from_syl (mt_syl_from_seg seg)))

(define (mt_segs_from_syl s)
  (cond
   ((null s) nil)
   ((member_string 'Segment (item.relations s))
    (list s))
   (t
    (apply
     append
     (mapcar mt_segs_from_syl (item.relation.daughters s 'SylStructure))))))

(define (sylmtval s)
  (let ((syl (mt_syl_from_seg s)))
    (if syl
	(item.feat syl "MetricalValue")
	"0")))

(define (sylpmtval s)
  (let ((syl (mt_syl_from_seg s)))
    (if syl
	(item.feat syl "R:MetricalTree.parent.MetricalValue")
	"0")))

(define (mt_numsyls w)
  (let ((s1 (item.daughter1_to (item.relation w 'WordStructure) 'Syllable))
	(sn (item.daughtern_to (item.relation w 'WordStructure) 'Syllable))
	(count 1))
    (while (and s1 (not (equal? s1 sn)))
	   (set! count (+ 1 count))
	   (set! s1 (item.next s1)))
    (if s1
	count
	0)))

(define (mt_seg_numsyls s)
  (let ((w (mt_word_from_seg s)))
    (if w
	(mt_num_syls w)
	0)))


;;; These functions should be sort out some time
    
;;; Difference between this syl and the next
;;;  number of closing brackets, number of opening brackets
;;;  difference between them

(define (mt_close n)
  "(mt_close n)
The number of consituents this is the end of, Effectively the
number of closing brackets after this word."
  (if (or (not n) (item.next n))
      0
      (+ 1 (mt_close (item.parent n)))))

(define (mt_open n)
  "(mt_open n)
The number of consituents this is the start of, Effectively the
number of opening brackets before this word."
  (if (or (not n) (item.prev n))
      0
      (+ 1 (mt_open (item.parent n)))))
    
(define (mt_postype syl)
  "(mt_postype syl)
Returns single, initial, final or middle."
  (let ((w (mt_word_from_syl syl))
	(psw (mt_word_from_syl (item.relation.prev syl 'Syllable)))
	(nsw (mt_word_from_syl (item.relation.next syl 'Syllable))))
    (cond
     ((and (equal? w psw)
	   (equal? w nsw))
      'middle)
     ((and (not (equal? w psw))
	   (not (equal? w nsw)))
      'single)
     ((equal? w psw)
      'final)
     (t
      'initial))))

(define (mt_accent syl)
  "(mt_accent syl)
Accent or 0 if none."
  (let ((a 0))
    (mapcar
     (lambda (i)
       (if (string-matches (item.name i) "^a.*")
	   (set! a "a")))
     (item.relation.daughters syl 'IntonationSyllable))
    a))

(define (mt_break syl)
  "(mt_break syl)
Break or 0 if none."
  (let ((a 0))
    (mapcar
     (lambda (i)
       (if (string-matches (item.name i) ".*b$")
	   (set! a (item.name i))))
     (item.relation.daughters syl 'IntonationSyllable))
    a))

(define (mt_ssyl_out s)
  (cond
   ((null s) 0)
   ((not (string-equal 
	  "0" (item.feat s "R:WordStructure.root.lisp_word_mt_break")))
    0)
   ((string-equal "s" (item.feat s "MetricalValue"))
    (+ 1 (mt_ssyl_out (item.relation.next s 'Syllable))))
   (t
    (mt_ssyl_out (item.relation.next s 'Syllable)))))

(define (mt_num_s s)
  "(mt_num_s s)
The number of s MetricalValues from here to a w or top."
  (cond
   ((null s) 0)
   ((string-equal "w" (item.feat s "MetricalValue"))
    0)
   (t
    (+ 1 (mt_num_s (item.parent s))))))

(define (mt_num_w s)
  "(mt_num_w s)
The number of w MetricalValues from here to a s or top."
  (cond
   ((null s) 0)
   ((string-equal "s" (item.feat s "MetricalValue"))
    0)
   (t
    (+ 1 (mt_num_w (item.parent s))))))

(define (mt_strong s)
  "(mt_strong s)
1 if all MetricalValues a s to a word, 0 otherwise."
  (cond
   ((string-equal "w" (item.feat s "MetricalValue"))
    "0")
   ((member_string 'Word (item.relations s)) "1")
   (t
    (mt_strong (item.relation.parent s 'MetricalTree)))))

(define (mt_lssp s)
  "(mt_lssp s)
1 if last stressed syllable in phrase, 0 otherwise."
  (if (and (string-equal "s" (item.feat s "MetricalValue"))
	   (equal? 0 (mt_ssyl_out s)))
      "1"
      "0"))

(define (mt_fssw s)
  "(mt_fssw s)
1 if first stressed syllable in word, 0 otherwise."
  (if (and (string-equal "s" (item.feat s "MetricalValue"))
	   (mt_no_stress_before (item.relation.prev s 'Syllable)))
      "1"
      "0"))

(define (mt_nfssw s)
  "(nfssw s)
1 if second or later stressed syllable in word, 0 otherwise."
  (if (and (string-equal "s" (item.feat s "MetricalValue"))
	   (null (mt_no_stress_before (item.relation.prev s 'Syllable))))
      "1"
      "0"))

(define (mt_no_stress_before ss)
  (cond
   ((null ss) t)
   ((not (string-equal 
	  (item.feat ss "R:WordStructure.root.addr")
	  (item.feat (item.next ss) "R:WordStructure.root.addr")))
    t)
   ((string-equal "s" (item.feat ss "MetricalValue"))
    nil)
   (t
    (mt_no_stress_before (item.prev ss)))))

(define (word_mt_break w)
  (cond
   ((string-equal "1" (item.feat w "sentence_end"))
    "BB")
   ((string-equal "1" (item.feat w "phrase_end"))
    "B")
   (t
    "0")))
  
(provide 'tilt)
