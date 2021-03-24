;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                     Carnegie Mellon University                      ;;;
;;;                      Copyright (c) 1998-2008                        ;;;
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
;;;  A generic voice definition file for a clustergen synthesizer        ;;
;;;  Customized for: cmu_us_rms                                       ;;
;;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Try to find the directory where the voice is, this may be from
;;; .../festival/lib/voices/ or from the current directory
(if (assoc 'cmu_us_rms_cg voice-locations)
    (defvar cmu_us_rms::dir 
      (cdr (assoc 'cmu_us_rms_cg voice-locations)))
    (defvar cmu_us_rms::dir (string-append (pwd) "/")))

;;; Did we succeed in finding it
(if (not (probe_file (path-append cmu_us_rms::dir "festvox/")))
    (begin
     (format stderr "cmu_us_rms::clustergen: Can't find voice scm files they are not in\n")
     (format stderr "   %s\n" (path-append  cmu_us_rms::dir "festvox/"))
     (format stderr "   Either the voice isn't linked in Festival library\n")
     (format stderr "   or you are starting festival in the wrong directory\n")
     (error)))

;;;  Add the directory contains general voice stuff to load-path
(set! load-path (cons (path-append cmu_us_rms::dir "festvox/") 
		      load-path))

(require 'clustergen)  ;; runtime scheme support

;;; Voice specific parameter are defined in each of the following
;;; files
(require 'cmu_us_rms_phoneset)
(require 'cmu_us_rms_tokenizer)
(require 'cmu_us_rms_tagger)
(require 'cmu_us_rms_lexicon)
(require 'cmu_us_rms_phrasing)
(require 'cmu_us_rms_intonation)
(require 'cmu_us_rms_durdata_cg) 
(require 'cmu_us_rms_f0model)
(require 'cmu_us_rms_other)

(require 'cmu_us_rms_statenames)
;; ... and others as required

;;;
;;;  Code specific to the clustergen waveform synthesis method
;;;

;(set! cluster_synth_method 
;  (if (boundp 'mlsa_resynthesis)
;      cg_wave_synth
;      cg_wave_synth_external ))

;;; Flag to save multiple loading of db
(defvar cmu_us_rms::cg_loaded nil)
;;; When set to non-nil clunits voices *always* use their closest voice
;;; this is used when generating the prompts
(defvar cmu_us_rms::clunits_prompting_stage nil)

;;; You may wish to change this (only used in building the voice)
(set! cmu_us_rms::closest_voice 'voice_kal_diphone_us)

(set! us_phone_maps
      '(
;        (M_t t)
;        (M_dH d)
;        ...
        ))

(define (voice_kal_diphone_us_phone_maps utt)
  (mapcar
   (lambda (s) 
     (let ((m (assoc_string (item.name s) us_phone_maps)))
       (if m
           (item.set_feat s "us_diphone" (cadr m))
           (item.set_feat s "us_diphone"))))
   (utt.relation.items utt 'Segment))
  utt)

(define (voice_kal_diphone_us)
  (voice_kal_diphone)
  (set! UniSyn_module_hooks (list voice_kal_diphone_us_phone_maps ))

  'kal_diphone_us
)

;;;  These are the parameters which are needed at run time
;;;  build time parameters are added to this list from build_clunits.scm
(set! cmu_us_rms_cg::dt_params
      (list
       (list 'db_dir 
             (if (string-matches cmu_us_rms::dir ".*/")
                 cmu_us_rms::dir
                 (string-append cmu_us_rms::dir "/")))
       '(name cmu_us_rms)
       '(index_name cmu_us_rms)
       '(trees_dir "festival/trees/")
       '(clunit_name_feat lisp_cmu_us_rms::cg_name)
))

(if cg:mixed_excitation
    (set! me_mix_filters
          (load 
           (string-append cmu_us_rms::dir "/etc/mix_excitation_filters.txt")
           t)))

;; So as to fit nicely with existing clunit voices we check need to 
;; prepend these params if we already have some set.
(if (boundp 'cmu_us_rms::dt_params)
    (set! cmu_us_rms::dt_params
          (append 
           cmu_us_rms_cg::dt_params
           cmu_us_rms::dt_params))
    (set! cmu_us_rms::dt_params cmu_us_rms_cg::dt_params))

(define (cmu_us_rms::nextvoicing i)
  (let ((nname (item.feat i "n.name")))
    (cond
;     ((string-equal nname "pau")
;      "PAU")
     ((string-equal "+" (item.feat i "n.ph_vc"))
      "V")
     ((string-equal (item.feat i "n.ph_cvox") "+")
      "CVox")
     (t
      "UV"))))

(define (cmu_us_rms::cg_name i)
  (let ((x nil))
  (if (assoc 'cg::trajectory clustergen_mcep_trees)
      (set! x i)
      (set! x (item.relation.parent i 'mcep_link)))

  (let ((ph_clunit_name 
         (cmu_us_rms::clunit_name_real
          (item.relation
           (item.relation.parent x 'segstate)
           'Segment))))
    (cond
     ((string-equal ph_clunit_name "ignore")
      "ignore")
     (t
      (item.name i)))))
)

(define (cmu_us_rms::clunit_name_real i)
  "(cmu_us_rms::clunit_name i)
Defines the unit name for unit selection for us.  The can be modified
changes the basic classification of unit for the clustering.  By default
this we just use the phone name, but you may want to make this, phone
plus previous phone (or something else)."
  (let ((name (item.name i)))
    (cond
     ((and (not cmu_us_rms::cg_loaded)
	   (or (string-equal "h#" name) 
	       (string-equal "1" (item.feat i "ignore"))
	       (and (string-equal "pau" name)
		    (or (string-equal "pau" (item.feat i "p.name"))
			(string-equal "h#" (item.feat i "p.name")))
		    (string-equal "pau" (item.feat i "n.name")))))
      "ignore")
     ;; Comment out this if you want a more interesting unit name
     ((null nil)
      name)

     ;; Comment out the above if you want to use these rules
     ((string-equal "+" (item.feat i "ph_vc"))
      (string-append
       name
       "_"
       (item.feat i "R:SylStructure.parent.stress")
       "_"
       (cmu_us_rms::nextvoicing i)))
     ((string-equal name "pau")
      (string-append
       name
       "_"
       (cmu_us_rms::nextvoicing i)))
     (t
      (string-append
       name
       "_"
;       (item.feat i "seg_onsetcoda")
;       "_"
       (cmu_us_rms::nextvoicing i))))))

(define (cmu_us_rms::cg_load)
  "(cmu_us_rms::cg_load)
Function that actual loads in the databases and selection trees.
SHould only be called once per session."
  (set! dt_params cmu_us_rms::dt_params)
  (set! clustergen_params cmu_us_rms::dt_params)
  (if cg:multimodel
      (begin
        ;; Multimodel: separately trained statics and deltas
        (set! cmu_us_rms::static_param_vectors
              (track.load
               (string-append 
                cmu_us_rms::dir "/"
                (get_param 'trees_dir dt_params "trees/")
                (get_param 'index_name dt_params "all")
                "_mcep_static.params")))
        (set! cmu_us_rms::clustergen_static_mcep_trees
              (load (string-append 
                     cmu_us_rms::dir "/"
                     (get_param 'trees_dir dt_params "trees/")
                     (get_param 'index_name dt_params "all")
                     "_mcep_static.tree") t))
        (set! cmu_us_rms::delta_param_vectors
              (track.load
               (string-append 
                cmu_us_rms::dir "/"
                (get_param 'trees_dir dt_params "trees/")
                (get_param 'index_name dt_params "all")
                "_mcep_delta.params")))
        (set! cmu_us_rms::clustergen_delta_mcep_trees
              (load (string-append 
                     cmu_us_rms::dir "/"
                     (get_param 'trees_dir dt_params "trees/")
                     (get_param 'index_name dt_params "all")
                     "_mcep_delta.tree") t))
        (if (null (assoc 'cg::trajectory cmu_us_rms::clustergen_static_mcep_trees))
            (set! cmu_us_rms::clustergen_f0_trees
                  (load (string-append 
                          cmu_us_rms::dir "/"
                          (get_param 'trees_dir dt_params "trees/")
                          (get_param 'index_name dt_params "all")
                          "_f0.tree") t)))
        )
      (begin
        ;; Single joint model 
        (set! cmu_us_rms::param_vectors
              (track.load
               (string-append 
                cmu_us_rms::dir "/"
                (get_param 'trees_dir dt_params "trees/")
                (get_param 'index_name dt_params "all")
                "_mcep.params")))
        (set! cmu_us_rms::clustergen_mcep_trees
              (load (string-append 
                      cmu_us_rms::dir "/"
                      (get_param 'trees_dir dt_params "trees/")
                      (get_param 'index_name dt_params "all")
                      "_mcep.tree") t))
        (if (null (assoc 'cg::trajectory cmu_us_rms::clustergen_mcep_trees))
            (set! cmu_us_rms::clustergen_f0_trees
                  (load (string-append 
                         cmu_us_rms::dir "/"
                         (get_param 'trees_dir dt_params "trees/")
                         (get_param 'index_name dt_params "all")
                         "_f0.tree") t)))))

  (set! cmu_us_rms::cg_loaded t)
)

(define (cmu_us_rms::voice_reset)
  "(cmu_us_rms::voice_reset)
Reset global variables back to previous voice."
  (cmu_us_rms::reset_phoneset)
  (cmu_us_rms::reset_tokenizer)
  (cmu_us_rms::reset_tagger)
  (cmu_us_rms::reset_lexicon)
  (cmu_us_rms::reset_phrasing)
  (cmu_us_rms::reset_intonation)
  (cmu_us_rms::reset_f0model)
  (cmu_us_rms::reset_other)

  t
)

;; This function is called to setup a voice.  It will typically
;; simply call functions that are defined in other files in this directory
;; Sometime these simply set up standard Festival modules othertimes
;; these will be specific to this voice.
;; Feel free to add to this list if your language requires it

(define (voice_cmu_us_rms_cg)
  "(voice_cmu_us_rms_cg)
Define voice for us."
  ;; *always* required
  (voice_reset)

  ;; Select appropriate phone set
  (cmu_us_rms::select_phoneset)

  ;; Select appropriate tokenization
  (cmu_us_rms::select_tokenizer)

  ;; For part of speech tagging
  (cmu_us_rms::select_tagger)

  (cmu_us_rms::select_lexicon)

  (cmu_us_rms::select_phrasing)

  (cmu_us_rms::select_intonation)

  ;; For CG voice there is no duration modeling at the seg level
  (Parameter.set 'Duration_Method 'Default)
  (set! duration_cart_tree_cg cmu_us_rms::zdur_tree)
  (set! duration_ph_info_cg cmu_us_rms::phone_durs)
  (Parameter.set 'Duration_Stretch 1.0)

  (cmu_us_rms::select_f0model)

  ;; Waveform synthesis model: cluster_gen
  (set! cg:initial_frame_offset 0.0)
  (set! cg:frame_shift 0.005)
  (set! mlsa_alpha_param 0.42)
  (set! mlsa_beta_param 0.35)
  (set! cg:F0_smooth t)
  (set! cg:param_smooth nil)
  (set! cg:mlpg t)
  (set! cg:gv nil)
  (set! cg:vuv nil)
  (set! cg:with_v t)
  (set! cg:deltas t)
  (set! cg:debug nil)
  (set! cg:save_param_track nil)
  (set! cg:multimodel nil)
  (set! cg:mcep_clustersize 50)
  (set! cg:gmm_transform nil)
  (set! cg:mixed_excitation t)

  (set! phone_to_states cmu_us_rms::phone_to_states)
  (if (not cmu_us_rms::clunits_prompting_stage)
      (begin
	(if (not cmu_us_rms::cg_loaded)
	    (cmu_us_rms::cg_load))
        (if cg:multimodel
            (begin
              (set! clustergen_param_vectors cmu_us_rms::static_param_vectors)
              (set! clustergen_mcep_trees cmu_us_rms::clustergen_static_mcep_trees)
              (set! clustergen_delta_param_vectors cmu_us_rms::delta_param_vectors)
              (set! clustergen_delta_mcep_trees cmu_us_rms::clustergen_delta_mcep_trees)
              )
            (begin
              (set! clustergen_param_vectors cmu_us_rms::param_vectors)
              (set! clustergen_mcep_trees cmu_us_rms::clustergen_mcep_trees)
              ))
        (if (boundp 'cmu_us_rms::clustergen_f0_trees)
            (set! clustergen_f0_trees cmu_us_rms::clustergen_f0_trees))
	(Parameter.set 'Synth_Method 'ClusterGen)
      ))

  ;; This is where you can modify power (and sampling rate) if desired
  (set! after_synth_hooks nil)
;  (set! after_synth_hooks
;      (list
;        (lambda (utt)
;          (utt.wave.rescale utt 2.1))))

  (set! current_voice_reset cmu_us_rms::voice_reset)

  (set! current-voice 'cmu_us_rms_cg)
)

(define (is_pau i)
  (if (phone_is_silence (item.name i))
      "1"
      "0"))

(provide 'cmu_us_rms_cg)

