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
;;;                                                                      ;;
;;;  A generic voice definition file for a clunits synthesizer           ;;
;;;  Customized for: cmu_us_slt_arctic                                       ;;
;;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Ensure this version of festival has been compiled with clunits module
(require_module 'clunits)
(require 'clunits) ;; runtime scheme support

;;; Try to find the directory where the voice is, this may be from
;;; .../festival/lib/voices/ or from the current directory
(if (assoc 'cmu_us_slt_arctic_clunits voice-locations)
    (defvar cmu_us_slt_arctic::clunits_dir 
      (cdr (assoc 'cmu_us_slt_arctic_clunits voice-locations)))
    (defvar cmu_us_slt_arctic::clunits_dir (string-append (pwd) "/")))

;;; Did we succeed in finding it
(if (not (probe_file (path-append cmu_us_slt_arctic::clunits_dir "festvox/")))
    (begin
     (format stderr "cmu_us_slt_arctic::clunits: Can't find voice scm files they are not in\n")
     (format stderr "   %s\n" (path-append  cmu_us_slt_arctic::clunits_dir "festvox/"))
     (format stderr "   Either the voice isn't linked in Festival library\n")
     (format stderr "   or you are starting festival in the wrong directory\n")
     (error)))

;;;  Add the directory contains general voice stuff to load-path
(set! load-path (cons (path-append cmu_us_slt_arctic::clunits_dir "festvox/") 
		      load-path))

;;; Voice specific parameter are defined in each of the following
;;; files
(require 'cmu_us_slt_arctic_phoneset)
(require 'cmu_us_slt_arctic_tokenizer)
(require 'cmu_us_slt_arctic_tagger)
(require 'cmu_us_slt_arctic_lexicon)
(require 'cmu_us_slt_arctic_phrasing)
(require 'cmu_us_slt_arctic_intonation)
(require 'cmu_us_slt_arctic_duration)
(require 'cmu_us_slt_arctic_f0model)
(require 'cmu_us_slt_arctic_other)
;; ... and others as required

;;;
;;;  Code specific to the clunits waveform synthesis method
;;;

;;; Flag to save multiple loading of db
(defvar cmu_us_slt_arctic::clunits_loaded nil)
;;; When set to non-nil clunits voices *always* use their closest voice
;;; this is used when generating the prompts
(defvar cmu_us_slt_arctic::clunits_prompting_stage nil)
;;; Flag to allow new lexical items to be added only once
(defvar cmu_us_slt_arctic::clunits_added_extra_lex_items nil)

;;; You may wish to change this (only used in building the voice)
(set! cmu_us_slt_arctic::closest_voice 'voice_kal_diphone)

;;;  These are the parameters which are needed at run time
;;;  build time parameters are added to his list in cmu_us_slt_arctic_build.scm
(set! cmu_us_slt_arctic::dt_params
      (list
       (list 'db_dir cmu_us_slt_arctic::clunits_dir)
       '(name cmu_us_slt_arctic)
       '(index_name cmu_us_slt_arctic)
       '(f0_join_weight 0.0)
       '(join_weights
         (0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ))
       '(trees_dir "festival/trees/")
       '(catalogue_dir "festival/clunits/")
       '(coeffs_dir "mcep/")
       '(coeffs_ext ".mcep")
       '(clunit_name_feat lisp_cmu_us_slt_arctic::clunit_name)
       ;;  Run time parameters 
       '(join_method windowed)
       ;; if pitch mark extraction is bad this is better than the above
;       '(join_method smoothedjoin)
;       '(join_method modified_lpc)
       '(continuity_weight 5)
;       '(log_scores 1)  ;; good for high variance joins (not so good for ldom)
       '(optimal_coupling 1)
       '(extend_selections 2)
       '(pm_coeffs_dir "mcep/")
       '(pm_coeffs_ext ".mcep")
       '(sig_dir "wav/")
       '(sig_ext ".wav")
;       '(pm_coeffs_dir "lpc/")
;       '(pm_coeffs_ext ".lpc")
;       '(sig_dir "lpc/")
;       '(sig_ext ".res")
;       '(clunits_debug 1)
))

(define (cmu_us_slt_arctic::clunit_name i)
  "(cmu_us_slt_arctic::clunit_name i)
Defines the unit name for unit selection for us.  The can be modified
changes the basic classification of unit for the clustering.  By default
this we just use the phone name, but you may want to make this, phone
plus previous phone (or something else)."
  (let ((name (item.name i)))
    (cond
     ((and (not cmu_us_slt_arctic::clunits_loaded)
	   (or (string-equal "h#" name) 
	       (string-equal "1" (item.feat i "ignore"))
	       (and (string-equal "pau" name)
		    (or (string-equal "pau" (item.feat i "p.name"))
			(string-equal "h#" (item.feat i "p.name")))
		    (string-equal "pau" (item.feat i "n.name")))))
      "ignore")
     ((string-matches name "[aeiou].*")
      (string-append
       name
       (item.feat i "R:SylStructure.parent.stress")))
     ((string-equal name "pau")
      name)
     (t
      (string-append
       name
       (item.feat i "seg_onsetcoda"))))))

(define (cmu_us_slt_arctic::clunits_load)
  "(cmu_us_slt_arctic::clunits_load)
Function that actual loads in the databases and selection trees.
SHould only be called once per session."
  (set! dt_params cmu_us_slt_arctic::dt_params)
  (set! clunits_params cmu_us_slt_arctic::dt_params)
  (clunits:load_db clunits_params)
  (load (string-append
	 (string-append 
	  cmu_us_slt_arctic::clunits_dir "/"
	  (get_param 'trees_dir dt_params "trees/")
	  (get_param 'index_name dt_params "all")
	  ".tree")))
  (set! cmu_us_slt_arctic::clunits_clunit_selection_trees clunits_selection_trees)
  (set! cmu_us_slt_arctic::clunits_loaded t))

(define (cmu_us_slt_arctic::voice_reset)
  "(cmu_us_slt_arctic::voice_reset)
Reset global variables back to previous voice."
  (cmu_us_slt_arctic::reset_phoneset)
  (cmu_us_slt_arctic::reset_tokenizer)
  (cmu_us_slt_arctic::reset_tagger)
  (cmu_us_slt_arctic::reset_lexicon)
  (cmu_us_slt_arctic::reset_phrasing)
  (cmu_us_slt_arctic::reset_intonation)
  (cmu_us_slt_arctic::reset_duration)
  (cmu_us_slt_arctic::reset_f0model)
  (cmu_us_slt_arctic::reset_other)

  t
)

;; This function is called to setup a voice.  It will typically
;; simply call functions that are defined in other files in this directory
;; Sometime these simply set up standard Festival modules othertimes
;; these will be specific to this voice.
;; Feel free to add to this list if your language requires it

(define (voice_cmu_us_slt_arctic_clunits)
  "(voice_cmu_us_slt_arctic_clunits)
Define voice for us."
  ;; *always* required
  (voice_reset)

  ;; Select appropriate phone set
  (cmu_us_slt_arctic::select_phoneset)

  ;; Select appropriate tokenization
  (cmu_us_slt_arctic::select_tokenizer)

  ;; For part of speech tagging
  (cmu_us_slt_arctic::select_tagger)

  (cmu_us_slt_arctic::select_lexicon)
  ;; For clunits selection you probably don't want vowel reduction
  ;; the unit selection will do that
  (if (string-equal "americanenglish" (Param.get 'Language))
      (set! postlex_vowel_reduce_cart_tree nil))

  (cmu_us_slt_arctic::select_phrasing)

  (cmu_us_slt_arctic::select_intonation)

  (cmu_us_slt_arctic::select_duration)

  (cmu_us_slt_arctic::select_f0model)

  ;; Waveform synthesis model: clunits

  ;; Load in the clunits databases (or select it if its already loaded)
  (if (not cmu_us_slt_arctic::clunits_prompting_stage)
      (begin
	(if (not cmu_us_slt_arctic::clunits_loaded)
	    (cmu_us_slt_arctic::clunits_load)
	    (clunits:select 'cmu_us_slt_arctic))
	(set! clunits_selection_trees 
	      cmu_us_slt_arctic::clunits_clunit_selection_trees)
	(Parameter.set 'Synth_Method 'Cluster)))

  ;; This is where you can modify power (and sampling rate) if desired
  (set! after_synth_hooks nil)
;  (set! after_synth_hooks
;      (list
;        (lambda (utt)
;          (utt.wave.rescale utt 2.1))))

  (set! current_voice_reset cmu_us_slt_arctic::voice_reset)

  (set! current-voice 'cmu_us_slt_arctic_clunits)
)

(define (is_pau i)
  (if (phone_is_silence (item.name i))
      "1"
      "0"))

(provide 'cmu_us_slt_arctic_clunits)

