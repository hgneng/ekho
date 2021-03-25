;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                     Carnegie Mellon University                        ;;
;;;                      Copyright (c) 2005-2006                          ;;
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
;;;  CARNEGIE MELLON UNIVERSITY AND THE CONTRIBUTORS TO THIS WORK         ;;
;;;  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      ;;
;;;  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO         ;;
;;;  EVENT SHALL CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS BE       ;;
;;;  LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY     ;;
;;;  DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,      ;;
;;;  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS       ;;
;;;  ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR              ;;
;;;  PERFORMANCE OF THIS SOFTWARE.                                        ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;  Author: Alan W Black (awb@cs.cmu.edu) Nov 2005                       ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;  Run Time Synthesis support for clustergen (HMM-generation) voices    ;;
;;;                                                                       ;;
;;;  This is voice-independant, and should be in festival/lib but is      ;;
;;;  currently copied into each voice                                     ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cluster_synth_pre_hooks nil)
(defvar cluster_synth_post_hooks nil)
(defvar clustergen_mcep_trees nil)
(defvar cg:initial_frame_offset 0.0)
(defvar cg:frame_shift 0.005)
(defvar mlsa_alpha_param 0.42)
(set! mlsa_beta_param 0.35)
;;; deltas/mlpg
(defvar cg:F0_smooth t)
(defvar cg:param_smooth nil)
(defvar cg:mlpg t)
(defvar cg:gv nil)
(defvar cg:vuv nil)
(defvar cg:with_v t)
(defvar cg:deltas nil)
(defvar cg:debug t)
(defvar cg:save_param_track nil)
(set! cg:multimodel nil)
(set! cg:mcep_clustersize 50)
(defvar cg:gmm_transform nil)
(set! cg:mixed_excitation t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The main CG synthesis voice, voices using CG should set
;;     (Parameter.set 'Synth_Method 'ClusterGen)
;;  which is done in INST_LANG_VOX_cg.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defSynthType ClusterGen

    (apply_hooks cluster_synth_pre_hooks utt)

    (set! clustergen_utt utt) ;; a global variable for debugging

    ;; Build the state relation
    (ClusterGen_make_HMMstate utt)
    ;; Predict number of frames
    (ClusterGen_make_mcep utt) ;; durations for # of vectors
    ;; Then predict the frame values
    (if (assoc 'cg::trajectory clustergen_mcep_trees)
        (ClusterGen_predict_trajectory utt) ;; predict trajectory (or ola)
        (ClusterGen_predict_mcep utt) ;; predict vector types
        )
;    (ClusterGen_predict_cgv utt) ;; predict vectors by viterbi

    ;; Convert predicted mcep track into a waveform
    (if cg:gmm_transform
        (cg_do_gmm_transform utt)  ;; GMM (MLPG again) and MLSA
        )

    (cluster_synth_method utt) ;; standard MLSA only

    (if cg:save_param_track
        (track.save (utt.feat utt "param_track") "param.track"))

    (apply_hooks cluster_synth_post_hooks utt)
    utt
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Various waveform resynthesis wraparound functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cg_wave_synth_external utt)
  ;; before we had it built-in to Festival
  (let ((trackname (make_tmp_filename))
        (wavename (make_tmp_filename))
        )
    (track.save (utt.feat utt "param_track") trackname "est")
    (system
     (format nil "$FESTVOXDIR/src/clustergen/cg_resynth %s %s"
             trackname wavename))
    (utt.import.wave utt wavename)
    (delete-file trackname)
    (delete-file wavename)
    utt)
)

(define (cg_wave_synth utt)
  (utt.relation.create utt 'Wave)
  (if cg:mixed_excitation
    (item.set_feat 
     (utt.relation.append utt 'Wave) 
     "wave" 
     (me_mlsa 
      (utt.feat utt "param_track")
      (utt.feat utt "str_params")))
    ;; Not mixed excitation
    (item.set_feat 
     (utt.relation.append utt 'Wave) 
     "wave" 
     (mlsa_resynthesis (utt.feat utt "param_track"))))
    utt)

(define (cg_wave_synth_sptk utt)
  ;; before we had it built-in to Festival
  (let ((trackname (make_tmp_filename))
        (wavename (make_tmp_filename))
        )
    (track.save (utt.feat utt "param_track") trackname "est")
    (system
     (format nil "$FESTVOXDIR/src/clustergen/cg_mlsa2 %s %s"
             trackname wavename))
    (utt.import.wave utt wavename)
    (delete-file trackname)
    (delete-file wavename)
    utt)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CGA is a basic voice morphing/adaptation  technique using cg -- 
;; it is very much experimental and incomplete
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cg_wave_synth_cga utt)
  ;; Use loaded cga model to predict a new map_track
  (format t "In Synth CGA\n")

  (cga:create_map utt)  ;; generate map_track
  (cga:predict_map utt)
  
  (utt.relation.create utt 'Wave)
  (item.set_feat 
   (utt.relation.append utt 'Wave) 
   "wave" 
   (mlsa_resynthesis (utt.feat utt "map_track")))
  utt)

(define (cga:create_map utt)
  ;; predict the map_param converted parameters

  ;; Need to do better duration stuff
  (set! map_track (track.copy (utt.feat utt "param_track")))
  (utt.set_feat utt "map_track" map_track)

  (utt.relation.create utt "param_map")
  (utt.relation.create utt "param_map_link")

  (set! pseg (utt.relation.first utt "mcep"))
  (set! m 0)
  (while pseg
     (set! mcep_parent (utt.relation.append utt "param_map_link" pseg))
     (set! mseg (utt.relation.append utt "param_map"))
     (item.append_daughter mcep_parent mseg)
     (item.set_feat mseg "frame_number" m)
     (item.set_feat mseg "name"
                         (item.feat mseg "R:param_map_link.parent.name"))
     (set! m (+ 1 m))
     (set! pseg (item.next pseg)))
  utt
)

(define (cga:predict_map utt)
  (let (i j f map_track num_channels
          s_f0_mean s_f0_stddev
          t_f0_mean t_f0_stddev)

  (set! i 0)
  (set! map_track (utt.feat utt "map_track"))
  (set! num_channels (track.num_channels map_track))

  (set! s_f0_mean (get_param 'cga::source_f0_mean clustergen_cga_trees 140))
  (set! s_f0_stddev (get_param 'cga::source_f0_stddev clustergen_cga_trees 20))
  (set! t_f0_mean (get_param 'cga::target_f0_mean clustergen_cga_trees 140))
  (set! t_f0_stddev (get_param 'cga::target_f0_stddev clustergen_cga_trees 20))

  (mapcar
   (lambda (x)
     (let ((map_tree (assoc_string (item.name x) clustergen_cga_trees)))
       (if (null map_tree)
           (format t "ClusterGenCGA: can't find cluster tree for %s\n"
                   (item.name x))
           (begin
             (set! frame (wagon x (cadr map_tree)))
             ;; Convert f0
             (if (> (track.get map_track i 0) 0)
                 (track.set 
                  map_track i 0
                  (+ t_f0_mean
                     (* t_f0_stddev
                        (/ (- (track.get map_track i 0) s_f0_mean)
                           s_f0_stddev)))))
             (set! j 1)
             (set! f (car frame))
             (while (< j num_channels)
                (track.set map_track i j 
                   (track.get clustergen_cga_vectors f (* 2 j)))
                (set! j (+ 1 j)))))
       (set! i (+ 1 i))))
   (utt.relation.items utt "param_map"))

  utt))

(define (ClusterGen_predict_states seg)
  ;; The names may change
  (cdr (assoc_string (item.name seg) phone_to_states)))

(define (ClusterGen_make_HMMstate utt)
  (let ((states)
        (segstate)
        (statepos))
    ;; Make HMMstate relation and items (three per phone)
    (utt.relation.create utt "HMMstate")
    (utt.relation.create utt "segstate")
    
    (mapcar
     (lambda (seg)
       (set! statepos 1)
       (set! states (ClusterGen_predict_states seg))
       (set! segstate (utt.relation.append utt 'segstate seg))
       (while states
          (set! state (utt.relation.append utt 'HMMstate))
          (item.append_daughter segstate state)
          (item.set_feat state "name" (car states))
          (item.set_feat state "statepos" statepos)
          (set! statepos (+ 1 statepos))
          (set! states (cdr states)))
       )
     (utt.relation.items utt 'Segment))
    )
)

(define (ClusterGen_state_duration state)
  (let ((zdur (wagon_predict state duration_cart_tree_cg))
        (ph_info (assoc_string (item.name state) duration_ph_info_cg))
        (seg_stretch (item.feat state "R:segstate.parent.dur_stretch"))
        (syl_stretch (item.feat state "R:segstate.parent.R:SylStructure.parent.dur_stretch"))
        (tok_stretch (parse-number (item.feat state "R:segstate.parent.R:SylStructure.parent.parent.R:Token.parent.dur_stretch")))
        (global_stretch (Parameter.get 'Duration_Stretch))
        (stretch 1.0))
    (if (string-matches (item.name state) "pau_.*")
        ;; Its a pau so explicitly set the duration
        ;; Note we want sentence internal pauses to be about 100ms
        ;; and sentence final pauses to be 150ms, but there will also
        ;; sentence initial pauses of 150ms so we can treat all pauses as
        ;; 100ms, there are three states so we use 50ms
        (set! zdur 
              (/ (- 0.05 (car (cdr ph_info)))
                 (car (cdr (cdr ph_info))))))
    (if (not (string-equal seg_stretch "0"))
        (setq stretch (* stretch seg_stretch)))
    (if (not (string-equal syl_stretch "0"))
        (setq stretch (* stretch syl_stretch)))
    (if (not (string-equal tok_stretch "0"))
        (setq stretch (* stretch tok_stretch)))
    (if (not (string-equal global_stretch "0"))
        (setq stretch (* stretch global_stretch)))
    (if ph_info
        (* stretch
           (+ (car (cdr ph_info)) ;; mean
              (* (car (cdr (cdr ph_info))) ;; stddev
                 zdur)))
        (begin
          (format t "ClusterGen_state_duration: no dur phone info for %s\n"
                  (item.name state))
          0.1))))

(define (ClusterGen_make_mcep utt)
  ;; Well its really make params (whatever type they are), 
  ;; they might not be mceps 
  ;; Note this just makes the vectors, it doesn't predict the
  ;; values of the vectors -- see predict_mcep below
  (let ((num_frames 0)
        (frame_advance cg:frame_shift)
        (end 0.0))

    ;; Make HMMstate relation and items (three per phone)
    (utt.relation.create utt "mcep")
    (utt.relation.create utt "mcep_link")
    (mapcar
     (lambda (state)
       ;; Predict Duration
       (set! start end)
       (set! end (+ start (ClusterGen_state_duration state)))
       (item.set_feat state "end" end)
       ;; create that number of mcep frames up to state end
       (set! mcep_parent (utt.relation.append utt 'mcep_link state))
       (while (<= (* (+ 0 num_frames) frame_advance) end)
              (set! mcep_frame (utt.relation.append utt 'mcep))
              (item.append_daughter mcep_parent mcep_frame)
              (item.set_feat mcep_frame "frame_number" num_frames)
              (item.set_feat mcep_frame "name" (item.name mcep_parent))
              (set! num_frames (+ 1 num_frames))
              )
       )
     
     (utt.relation.items utt 'HMMstate))

    ;; Copy the final state end back up on to the segment for consistency
    (mapcar
     (lambda (seg)
       (item.set_feat seg "end" (item.feat seg "R:segstate.daughtern.end")))
     (utt.relation.items utt 'Segment))

    (utt.set_feat utt "param_track_num_frames" num_frames)
    utt)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some feature functions specific to CG, some of these are just
;; experimental
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (mcep_12 i)
  (track.get
   (utt.feat (item.get_utt i) "param_track")
   (item.feat i "frame_number")
   12))
(define (mcep_11 i)
  (track.get
   (utt.feat (item.get_utt i) "param_track")
   (item.feat i "frame_number")
   11))
(define (mcep_10 i)
  (track.get
   (utt.feat (item.get_utt i) "param_track")
   (item.feat i "frame_number")
   10))
(define (mcep_9 i)
  (track.get
   (utt.feat (item.get_utt i) "param_track")
   (item.feat i "frame_number")
   9))
(define (mcep_8 i)
  (track.get
   (utt.feat (item.get_utt i) "param_track")
   (item.feat i "frame_number")
   8))
(define (mcep_7 i)
  (track.get
   (utt.feat (item.get_utt i) "param_track")
   (item.feat i "frame_number")
   7))
(define (mcep_6 i)
  (track.get
   (utt.feat (item.get_utt i) "param_track")
   (item.feat i "frame_number")
   6))
(define (mcep_5 i)
  (track.get
   (utt.feat (item.get_utt i) "param_track")
   (item.feat i "frame_number")
   5))
(define (mcep_4 i)
  (track.get
   (utt.feat (item.get_utt i) "param_track")
   (item.feat i "frame_number")
   4))
(define (mcep_3 i)
  (track.get
   (utt.feat (item.get_utt i) "param_track")
   (item.feat i "frame_number")
   3))
(define (mcep_2 i)
  (track.get
   (utt.feat (item.get_utt i) "param_track")
   (item.feat i "frame_number")
   2))
(define (mcep_1 i)
  (track.get
   (utt.feat (item.get_utt i) "param_track")
   (item.feat i "frame_number")
   1))
(define (mcep_0 i)
  (track.get
   (utt.feat (item.get_utt i) "param_track")
   (item.feat i "frame_number")
   0))

(define (cg_break s)
  "(cg_break s)
0, if word internal, 1 if word final, 4 if phrase final, we ignore 
3/4 distinguinction in old syl_break"
  (let ((x (item.feat s "syl_break")))
    (cond
     ((string-equal "0" x)
      (string-append x))
     ((string-equal "1" x)
      (string-append x))
     ((string-equal "0" (item.feat s "R:SylStructure.parent.n.name"))
      "4")
     (t
      "3"))))

(define (cg_frame_voiced s)
  (if (and (string-equal "-"
            (item.feat 
             s "R:mcep_link.parent.R:segstate.parent.ph_vc"))
           (string-equal "-"
            (item.feat 
             s "R:mcep_link.parent.R:segstate.parent.ph_cvox")))
      0
      1)
)

(define (cg_duration i)
  (if (item.prev i)
      (- (item.feat i "end") (item.feat i "p.end"))
      (item.feat i "end")))

(define (cg_state_pos i)
  (let ((n (item.name i)))
  (cond
   ((not (string-equal n (item.feat i "p.name")))
    "b")
   ((string-equal n (item.feat i "n.name"))
    "m")
   (t
    "e"))))

(define (cg_state_place i)
  (let ((start (item.feat i "R:mcep_link.parent.daughter1.frame_number"))
        (end (item.feat i "R:mcep_link.parent.daughtern.frame_number"))
        (this (item.feat i "frame_number")))
    (if (eq? 0.0 (- end start))
        0
        (/ (- this start)
           (- end start)))))

(define (cg_state_index i)
  (let ((start (item.feat i "R:mcep_link.parent.daughter1.frame_number"))
        (this (item.feat i "frame_number")))
    (- this start)))

(define (cg_state_rindex i)
  (let ((end (item.feat i "R:mcep_link.parent.daughtern.frame_number"))
        (this (item.feat i "frame_number")))
    (- end this)))

(define (cg_phone_place i)
  (let ((start (item.feat i "R:mcep_link.parent.R:segstate.parent.daughter1.R:mcep_link.daughter1.frame_number"))
        (end (item.feat i "R:mcep_link.parent.R:segstate.parent.daughtern.R:mcep_link.daughtern.frame_number"))
        (this (item.feat i "frame_number")))
    (if (eq? 0.0 (- end start))
        0
        (/ (- this start)
           (- end start)))))

(define (cg_phone_index i)
  (let ((start (item.feat i "R:mcep_link.parent.R:segstate.parent.daughter1.R:mcep_link.daughter1.frame_number"))
        (this (item.feat i "frame_number")))
    (- this start)))

(define (cg_phone_rindex i)
  (let ((end (item.feat i "R:mcep_link.parent.R:segstate.parent.daughtern.R:mcep_link.daughtern.frame_number"))
        (this (item.feat i "frame_number")))
    (- end this)))

(define (cg_utt_fileid i)
  (utt.feat (item.get_utt i) "fileid"))

(define (cg_position_in_sentenceX x)
  (/ (item.feat x "R:mcep_link.parent.end")
     (item.feat x "R:mcep_link.parent.R:segstate.parent.R:Segment.last.end")))

(define (cg_position_in_sentence x)
  (let ((sstart (item.feat 
                 x 
                 "R:mcep_link.parent.R:segstate.parent.R:SylStructure.parent.parent.R:Word.first.R:SylStructure.daughter1.daughter1.R:Segment.p.end"))
        (send (item.feat 
               x 
               "R:mcep_link.parent.R:segstate.parent.R:SylStructure.parent.parent.R:Word.last.R:SylStructure.daughtern.daughtern.R:Segment.end")))
    (set! xyx
    (if (eq? 0.0 (- send sstart))
        -1
        (/ (- (* cg:frame_shift (item.feat x "frame_number")) sstart)
           (- send sstart))))
;    (format t "cg_position_in_sentence2 %f\n" xyx)
    xyx
    ))

(define (cg_find_phrase_number x)
  (cond
   ((item.prev x)
    (+ 1 (cg_find_phrase_number (item.prev x))))
   (t
    0)))

(define (cg_find_rphrase_number x)
  (cond
   ((item.next x)
    (+ 1 (cg_find_rphrase_number (item.next x))))
   (t
    0)))

(define (cg_position_in_phrase x)
  (let ((pstart (item.feat 
                 x 
                 "R:mcep_link.parent.R:segstate.parent.R:SylStructure.parent.parent.R:Phrase.parent.daughter1.R:SylStructure.daughter1.daughter1.R:Segment.p.end"))
        (pend (item.feat 
               x 
               "R:mcep_link.parent.R:segstate.parent.R:SylStructure.parent.parent.R:Phrase.parent.daughtern.R:SylStructure.daughtern.daughtern.R:Segment.end"))
        (phrasenumber 
         (item.feat 
          x
          "R:mcep_link.parent.R:segstate.parent.R:SylStructure.parent.parent.R:Phrase.parent.lisp_cg_find_phrase_number")))
    (set! xyx
    (if (eq? 0.0 (- pend pstart))
        -1
        (+ 0 ;phrasenumber
        (/ (- (* cg:frame_shift (item.feat x "frame_number")) pstart)
           (- pend pstart)))))
;    (format t "cg_position_in_phrase %f\n" xyx)
    xyx
    )
    )

(define (cg_position_in_phrasep x)
  (let ((pstart (item.feat 
                 x 
                 "R:mcep_link.parent.R:segstate.parent.R:SylStructure.parent.parent.R:Phrase.parent.daughter1.R:SylStructure.daughter1.daughter1.R:Segment.p.end"))
        (pend (item.feat 
               x 
               "R:mcep_link.parent.R:segstate.parent.R:SylStructure.parent.parent.R:Phrase.parent.daughtern.R:SylStructure.daughtern.daughtern.R:Segment.end"))
        (phrasenumber 
         (item.feat 
          x
          "R:mcep_link.parent.R:segstate.parent.R:SylStructure.parent.parent.R:Phrase.parent.lisp_cg_find_phrase_number")))
    (set! xyx
    (if (eq? 0.0 (- pend pstart))
        -1
        (+ phrasenumber
        (/ (- (* cg:frame_shift (item.feat x "frame_number")) pstart)
           (- pend pstart)))))
;    (format t "cg_position_in_phrase %f\n" xyx)
    xyx
    )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Smoothing functions (sort of instead of mlpg)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cg_F0_smooth track j)
  (let ((p 0.0)
        (i 0)
        (num_frames (- (track.num_frames track) 1)))

    (set! i 1)
    (while (< i num_frames)
      (set! this (track.get track i j))
      (set! next (track.get track (+ i 1) j))
      (if (> this 0.0)
          (track.set 
           track i j
           (/ (+ (if (> p 0.0) p this)
                 this
                 (if (> next 0.0) next this))
              3.0)))
      (set! p this)
      (set! i (+ 1 i)))
    )
)

(define (cg_mcep_smooth track j)
  (let ((p 0.0)
        (i 0)
        (num_frames (- (track.num_frames track) 1)))

    (set! i 1)
    (while (< i num_frames)
      (set! this (track.get track i j))
      (set! next (track.get track (+ i 1) j))
      (track.set track i j (/ (+ p this next) 3.0))
      (set! p this)
      (set! i (+ 1 i)))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For normal synthesis make unvoiced states unvoiced, but we don't
;; do this during testing

(defvar cg_predict_unvoiced t)

(define (ClusterGen_predict_F0 mcep f0_val param_track)
  "(ClusterGen_predict_F0 mcep frame param_track)
Predict the F0 (or not)."
  (if (and cg_predict_unvoiced
           (string-equal "-"
            (item.feat 
             mcep "R:mcep_link.parent.R:segstate.parent.ph_vc"))
           (string-equal "-"
            (item.feat 
             mcep "R:mcep_link.parent.R:segstate.parent.ph_cvox")))
      (track.set param_track i 0 0.0) ;; make it unvoiced
      (track.set param_track i 0 f0_val)) ;; make it voiced
  )

(define (ClusterGen_mcep_voiced mcep)
  (if (and cg_predict_unvoiced
           (string-equal "-"
            (item.feat 
             mcep "R:mcep_link.parent.R:segstate.parent.ph_vc"))
           (string-equal "-"
            (item.feat 
             mcep "R:mcep_link.parent.R:segstate.parent.ph_cvox")))
      nil
      t))

(define (ClusterGen_voicing_v mcep)
  (let ((vvv 
         (track.get 
          clustergen_param_vectors 
          (item.feat mcep "clustergen_param_frame")
;         (+ 1 (* (if cg:mlpg 1 2) 51))
;          102
          (- (track.num_channels clustergen_param_vectors) 2)
          )))
;  (format t "%s %f\n" (item.name mcep) vvv)
    (cond
     ((string-equal "pau" (item.feat mcep "R:mcep_link.parent.R:segstate.parent.name"))
      ;; pauses are always unvoiced
      nil)
     ((string-equal 
       "+" 
       (item.feat mcep "R:mcep_link.parent.R:segstate.parent.ph_vc"))
      ;; vowels are always voiced
      t)
     ((> vvv 0.5)
      ;; consonants are what they are
      t)
     (t
      nil))))

(define (ClusterGen_voicing_v_traj mcep i params)
  (let ((vvv (track.get params i 102)))
;  (format t "%s %f\n" (item.name mcep) vvv)
    (cond
     ((string-equal "pau" (item.feat mcep "R:mcep_link.parent.R:segstate.parent.name"))
      ;; pauses are always unvoiced
      nil)
     ((string-equal 
       "+" 
       (item.feat mcep "R:mcep_link.parent.R:segstate.parent.ph_vc"))
      ;; vowels are always voiced
      t)
     ((> vvv 0.4)
      ;; consonants are what they are
      t)
     (t
      nil))))

(define (cg_do_gmm_transform utt)
  "(cmu_us_rms_transform::convfilter utt)
Filter synthesized voice with transformation filter and reload waveform."
   (let ((wfile1 (make_tmp_filename))
	 (wfile2 (make_tmp_filename))
         (wfile3 (make_tmp_filename))
         (wfile4 (make_tmp_filename))
         )

     (utt.save utt wfile3)
     (track.save (utt.feat utt "param_track") wfile4)

     (system
      (format 
       nil
       "(cd %s && csh $FESTVOXDIR/vc/scripts/VConvFestival_cg.csh $FESTVOXDIR/src/vs/src param/source-target_param.list %s %s %s %s)"
       "vc"  ;; Need a way set this with the voice dir
       wfile1  ;; input file
       wfile3  ;; utterance  
       wfile4  ;; predicted param file
       wfile2))

     (set! new_track (track.load wfile2))
     (utt.set_feat utt "param_track" new_track)
     (delete-file wfile1)
     (delete-file wfile2)
     (delete-file wfile3)
     (delete-file wfile4)

     utt
     ))

(define (cg_do_mlpg param_track)
  ;; do mlpg on the params
  (if (boundp 'mlpg)
      (begin
        (mlpg param_track))
      (begin ;; old version with external mlpg script
        (let ((trackname (make_tmp_filename))
              (mlpgtrack (make_tmp_filename)) )
          (track.save param_track trackname "est")
          (if cg:gv
              (begin
                (format t "with gv\n")
                (system
                 (format nil "$FESTVOXDIR/src/clustergen/cg_mlpg %s %s %s %s"
                         trackname mlpgtrack 
                         cg_gv_vm_filename cg_gv_vv_filename )))
              (system
               (format nil "$FESTVOXDIR/src/clustergen/cg_mlpg %s %s"
                       trackname mlpgtrack)))
          (set! postmlpg (track.load mlpgtrack))
          (delete-file trackname)
          (delete-file mlpgtrack)
          postmlpg)
        )))
      
(define (cg_all_f0 m)
  ;; global prediction of F0, not unit specific
  (let ((all_f0 (wagon m (cadr (assoc_string "all" clustergen_f0_all)))))
    (cadr all_f0)))

(define (cg_all_f0f0 m)
  ;; global prediction of F0, difference
  (let ((all_f0 (wagon m (cadr (assoc_string "all" clustergen_f0_all))))
        (all_f0f0 (wagon m (cadr (assoc_string "all_f0_diff" clustergen_f0_all)))))
    (- (cadr all_f0) (cadr all_f0f0))))

(define (ClusterGen_predict_mcep utt)
  (let ((param_track nil)
        (frame_advance cg:frame_shift)
        (frame nil) (f nil) (f0_val)
        (cg_name_feature "name")
        (num_channels 
         (/ (track.num_channels clustergen_param_vectors)
            (if cg:mlpg 1 2)))
        (num_frames (utt.feat utt "param_track_num_frames"))
        )

    ;; Predict mcep values
    (set! i 0)
    (set! param_track (track.resize nil num_frames num_channels))
    (utt.set_feat utt "param_track" param_track)

    (mapcar
     (lambda (mcep)
       ;; Predict mcep frame
       (let ((mcep_tree 
              (assoc_string 
               (item.feat mcep cg_name_feature) 
               clustergen_mcep_trees))
             (mcep_tree_delta
              (assoc_string 
               (item.feat mcep cg_name_feature) 
               (if cg:multimodel
                   clustergen_delta_mcep_trees nil)))
             (f0_tree 
              (assoc_string 
;               "all"
               (item.feat mcep cg_name_feature) 
               clustergen_f0_trees))
)
         (if (null mcep_tree)
             (format t "ClusterGen: can't find cluster tree for %s\n"
                     (item.name mcep))
             (begin
               ;; F0 prediction
               (set! f0_val 
                     (wagon mcep (cadr f0_tree))
;                     (list 1.0 (cg_all_f0 mcep))
                     )
               (track.set param_track i 0 (cadr f0_val))

               ;; MCEP prediction
               (set! frame (wagon mcep (cadr mcep_tree)))
               (if cg:multimodel
                   (set! dframe (wagon mcep (cadr mcep_tree_delta))))
               (set! j 1)
               (set! f (car frame))
               (item.set_feat mcep "clustergen_param_frame" f)
               (if cg:multimodel
                   (track.set param_track i 0 
                              (/ (+ (cadr f0_val)
                                    (track.get clustergen_delta_param_vectors 
                                               (car dframe) 0)
                                    (track.get clustergen_param_vectors 
                                               f 0))
                                 3.0)))
               (while (< j num_channels)
                  (if cg:multimodel
                      (begin
                        (track.set param_track i j
                                   (/
                                    (+
                                 (track.get clustergen_delta_param_vectors 
                                       (car dframe) (* (if cg:mlpg 1 2) j))
                                 (track.get clustergen_param_vectors 
                                            f (* (if cg:mlpg 1 2) j))
                                 ) 2.0))
                        )
                      (begin
                        (track.set param_track i j
                                   (track.get clustergen_param_vectors 
                                              f (* (if cg:mlpg 1 2) j)))
                        ))

                  (set! j (+ 1 j)))
               (set! j (- num_channels 1))
               (track.set param_track i j
                          (track.get clustergen_param_vectors 
                                     f (* (if cg:mlpg 1 2) j)))
               ))
         
         (track.set_time 
          param_track i 
          (+ cg:initial_frame_offset (* i frame_advance)))
         (set! i (+ 1 i))))
     (utt.relation.items utt 'mcep))

    (if cg:mixed_excitation
        (let ((nf (track.num_frames param_track))
              (f 0) (c 0))
          (set! str_params (track.resize nil nf 5))
          (set! f 0)
          (while (< f nf)
             (track.set_time str_params f (track.get_time param_track f)) 
             (set! c 0)
             (while (< c 5)
              (track.set str_params f c 
                         (track.get param_track f (* 2 (+ c 51))))
              (set! c (+ 1 c)))
             (set! f (+ 1 f)))
          (utt.set_feat utt "str_params" str_params)))

    (if (or cg:vuv cg:with_v)
           ;; need to get rid of the vuv coefficient (last one)
        (let ((nf (track.num_frames param_track))
              (nc (- (track.num_channels param_track) 2))
              (f 0) (c 0))
          (set! nnn_track (track.resize nil nf nc))
          (while (< f nf)
             (track.set_time nnn_track f (track.get_time param_track f)) 
             (set! c 0)
             (while (< c nc)
                (track.set nnn_track f c (track.get param_track f c))
                (set! c (+ 1 c)))
             (set! f (+ 1 f)))
          (set! param_track nnn_track)
          ))
    ;; MLPG
    (if cg:mlpg  ;; assume cg:deltas too
          (let ((nf (track.num_frames param_track))
              (nc (* 2 (+ 1 25 25))) ;; f0 static delta (mean and stddev)
              (f 0) (c 0))
            (if cg:debug (format t "cg:debug calling mlpg\n"))
            (set! nnn_track (track.resize nil nf nc))
            (while (< f nf)
               (track.set_time nnn_track f (track.get_time param_track f)) 
               (set! c 0)
               (while (< c nc)
                  (track.set nnn_track f c (track.get param_track f c))
                  (set! c (+ 1 c)))
               (set! f (+ 1 f)))
            (set! param_track nnn_track)
            (set! new_param_track (cg_do_mlpg param_track))
            (utt.set_feat utt "param_track" new_param_track)
            (set! param_track new_param_track)))
    (if (and (not cg:mlpg) cg:deltas)
        (begin   ;; have to reduce param_track to remove deltas
          (set! new_param_track 
                (track.resize 
                 param_track
                 (track.num_frames param_track)
                 26)) ;; not very portable
          (utt.set_feat utt "param_track" new_param_track)
          (set! param_track new_param_track)))
    
    (if cg:F0_smooth (cg_F0_smooth param_track 0))
    (if cg_predict_unvoiced
        (begin
          (set! i 0)
          (mapcar
           (lambda (frame)
             (if ;(not (ClusterGen_mcep_voiced frame))
                 (not (ClusterGen_voicing_v frame))
                 (track.set param_track i 0 0.0))
             (set! i (+ 1 i)))
           (utt.relation.items utt 'mcep))))
    (if cg:param_smooth
        (mapcar
         (lambda (x) (cg_mcep_smooth param_track x))
         '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)))
    utt
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CGV: prediction with viterbi
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cgv_reverse_probs pdf)
  (cond
   ((null pdf) nil)
   ((eq (car (cdr (car pdf))) 0)
    (cgv_reverse_probs (cdr pdf)))
   (t
    (cons 
     (list (car (car pdf))
           (/ (car (cdr (car pdf)))
              (cgv_prob (car (car pdf)))))
     (cgv_reverse_probs (cdr pdf))))))

(define (cgv_prob c)
  (let ((xxx (assoc_string c cgv_class_probs)))
    (if xxx
        (car (cdr xxx))
        0.000012)))

(define (cgv_cand_function s)
;  (format t "cand_function %s\n" (item.name s))
  (let ((mcep_tree (assoc_string (item.name s) clustergen_mcep_trees))
        (probs nil))
    (cond
     ((string-equal "S" (item.name s))
      (set! probs (cgv_reverse_probs '((S 1)))))
     ((string-equal "E" (item.name s))
      (set! probs (cgv_reverse_probs '((E 1)))))
     (mcep_tree
      (set! probs 
            (cgv_reverse_probs (cdr (reverse (wagon s (cadr mcep_tree)))))))
     (t
      (format t "ClusterGen: cgv can't find cluster tree for %s\n"
              (item.name s))
      (set! probs nil)))
;    (format t "%s %l\n" (item.name s) probs)
    probs))

(define (ClusterGen_predict_cgv utt)
  (format t "predict cgv\n")
  (let ((param_track nil)
        (frame_advance cg:frame_shift)
        (frame nil) (f nil) (f0_val)
        (num_channels 
         (/ (track.num_channels clustergen_param_vectors)
            (if cg:mlpg 1 2)))
        (num_frames (utt.feat utt "param_track_num_frames"))
        )

    ;; Predict mcep values
    (set! i 0)
    (set! param_track (track.resize nil num_frames num_channels))
    (utt.set_feat utt "param_track" param_track)

    (utt.relation.create utt 'cseq)
    (set! citem (utt.relation.append utt 'cseq nil))
    (item.set_feat citem 'name 'S)
    (mapcar
     (lambda (m) (set! citem (utt.relation.append utt 'cseq m)))
     (utt.relation.items utt 'mcep))
    (set! citem (utt.relation.append utt 'cseq nil))
    (item.set_feat citem 'name 'E)

    (set! gen_vit_params
	  (list
	   (list 'Relation "cseq")
	   (list 'return_feat "clustergen_class")
	   (list 'p_word "S")
	   (list 'pp_word "S")
;	   (list 'ngramname 'cgv_ngram)
	   (list 'wfstname 'cgv_wfst)
	   (list 'cand_function 'cgv_cand_function)))
    (Gen_Viterbi utt)

    (mapcar
     (lambda (mcep)
       ;; Predict mcep frame
       (let ((f0_tree (assoc_string (item.name mcep) clustergen_f0_trees)))
         (if (null f0_tree)
             (format t "ClusterGen: can't find cluster tree for %s\n"
                     (item.name mcep))
             (begin
               ;; F0 prediction
               (set! f0_val (wagon mcep (cadr f0_tree)))
               (if (eq (cadr f0_val) 0.0)
                   (track.set param_track i 0 0.0)
                   ;; Wave exp() but its worse
                   (track.set param_track i 0 (cadr f0_val)))

               ;; MCEP prediction
               (set! j 1)
               (set! f (parse-number
                        (string-after 
                         (item.feat mcep "clustergen_class")
                         "c")))
               (item.set_feat mcep "clustergen_param_frame" f)
               (while (< j num_channels)
                  (track.set param_track i j 
                    (track.get clustergen_param_vectors 
                               f (* (if cg:mlpg 1 2) j))
                    )
                  (set! j (+ 1 j)))))
         
         (track.set_time 
          param_track i 
          (+ cg:initial_frame_offset (* i frame_advance)))
         (set! i (+ 1 i))))
     (utt.relation.items utt 'mcep))

    ;; MLPG
    (if cg:mlpg  ;; assume cg:deltas too
        (begin
          (if cg:debug (format t "cg:debug calling mlpg\n"))
          (set! new_param_track (cg_do_mlpg param_track))
          (utt.set_feat utt "param_track" new_param_track)
          (set! param_track new_param_track)))
    (if (and (not cg:mlpg) cg:deltas)
        (begin   ;; have to reduce param_track to remove deltas
          (set! new_param_track 
                (track.resize 
                 param_track
                 (track.num_frames param_track)
                 26)) ;; not very portable
          (utt.set_feat utt "param_track" new_param_track)
          (set! param_track new_param_track)))
    
    (if cg:F0_smooth (cg_F0_smooth param_track 0))
    (if cg_predict_unvoiced
        (begin
          (set! i 0)
          (mapcar
           (lambda (frame)
             (if (not (ClusterGen_mcep_voiced frame))
                 (track.set param_track i 0 0.0))
             (set! i (+ 1 i)))
           (utt.relation.items utt 'mcep))))
    (if cg:param_smooth
        (mapcar
         (lambda (x) (cg_mcep_smooth param_track x))
         '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)))
    utt
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Trajectory prediction functions (including ola)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cg_voiced state)
  "(cg_voiced state)
t if this state is voiced, nil otherwise."
  (if (and cg_predict_unvoiced
           (string-equal "-" (item.feat state "R:segstate.parent.ph_vc"))
           (string-equal "-" (item.feat state "R:segstate.parent.ph_cvox")))
      nil
      t))

(define (ClusterGen_predict_trajectory utt)
  (let ((param_track nil)
        (frame_advance cg:frame_shift)
        (frame nil) (f nil) (f0_val)
        (num_channels 
         (/ (track.num_channels clustergen_param_vectors)
            (if cg:mlpg 1 2)))
        )

    ;; Predict mcep values
    (set! i 0)
    (set! param_track
          (track.resize nil
           (utt.feat utt "param_track_num_frames")
           num_channels))
    (utt.set_feat utt "param_track" param_track)
;    (set! param_track (utt.feat utt "param_track"))
    (mapcar
     (lambda (state)
       ;; Predict mcep frame
;joint       (let ((mcep_tree (assoc_string (item.name state) traj::clustergen_mcep_trees))
       (let ((mcep_tree (assoc_string (item.name state) clustergen_mcep_trees))
             ;(f0_tree (assoc_string (item.name mcep) clustergen_f0_trees))
             )
         (if (null mcep_tree)
             (format t "ClusterGen: can't find cluster tree for %s\n"
                     (item.name state))
             (begin
               ;; feature prediction (F0 and mcep)
               (set! trajectory (wagon state (cadr mcep_tree)))
               (if (item.relation.daughters state 'mcep_link)
                   (begin
                    (if (assoc 'cg::trajectory_ola clustergen_mcep_trees)
;joint                    (if (assoc 'cg::trajectory_ola traj::clustergen_mcep_trees)
                     (cg:add_trajectory_ola
                      (caar trajectory)
                      (cadr (car trajectory))
                      state
                      num_channels
                      param_track
                      frame_advance)
                     (cg:add_trajectory
                      (caar trajectory)
                      (cadr (car trajectory))
                      state
                      num_channels
                      param_track
                      frame_advance))))
               ))))
     (utt.relation.items utt 'HMMstate))

    (if (or cg:vuv cg:with_v)
           ;; need to get rid of the vuv coefficient (last one)
        (let ((nf (track.num_frames param_track))
              (nc (- (track.num_channels param_track) 2))
              (f 0) (c 0))
          (set! full_param_track param_track)
          (set! nnn_track (track.resize nil nf nc))
          (while (< f nf)
             (track.set_time nnn_track f (track.get_time param_track f)) 
             (set! c 0)
             (while (< c nc)
                (track.set nnn_track f c (track.get param_track f c))
                (set! c (+ 1 c)))
             (set! f (+ 1 f)))
          (set! param_track nnn_track)
          ))
    ;; MLPG
    (if cg:mlpg
        (begin
          (if cg:debug (format t "cg:debug calling mlpg\n"))
          (set! new_param_track (cg_do_mlpg param_track))
          (utt.set_feat utt "param_track" new_param_track)
          (set! param_track new_param_track)))

    (if (and (not cg:mlpg) cg:deltas)
        (begin   ;; have to reduce param_track to remove deltas
          (set! new_param_track 
                (track.resize 
                 param_track
                 (track.num_frames param_track)
                 26)) ;; not very portable
          (utt.set_feat utt "param_track" new_param_track)
          (set! param_track new_param_track)))
    (if cg:F0_smooth (cg_F0_smooth param_track 0))
    (if cg_predict_unvoiced
        (begin
          (set! i 0)
          (mapcar
           (lambda (frame)
             (if ;(not (ClusterGen_mcep_voiced frame))
                 (not (ClusterGen_voicing_v_traj frame i full_param_track))
                 (track.set param_track i 0 0.0))
             (set! i (+ 1 i)))
           (utt.relation.items utt 'mcep))))
    (if cg:param_smooth
        (mapcar
         (lambda (x) (cg_mcep_smooth param_track x))
     ;     '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)
         '( 1 2 3 )
         ))
    utt
  )
)

(define (cg:add_trajectory s_start s_frames state num_channels
                           param_track frame_advance)
"(cg:add_trajectory start n state num_channels)
Add trajectory to daughters of state, interpolating as necessary."
  (let ((j 0) (i 0)
        (mceps (item.relation.daughters state 'mcep_link)))

    (set! t_start (item.feat (car mceps) "frame_number"))
    (set! t_frames (length mceps))
    (set! m (/ (- s_frames 1) t_frames))
    (set! f 0)

    (while (< i t_frames)
       ;; find f
       (set! s_pos (nint (+ s_start f)))

       (track.set param_track (+ i t_start) 0
                  (track.get clustergen_param_vectors s_pos 0))

       (set! j 1)
       (while (< j num_channels)
              (track.set param_track (+ i t_start) j 
;joint               (+ (* 0.35 (track.get param_track (+ i t_start) j))
;                        (* 0.65 (track.get traj::clustergen_param_vectors 
;                                          s_pos (* 2 j)))))
                         (track.get clustergen_param_vectors 
                                    s_pos (* (if cg:mlpg 1 2) j)))
              (set! j (+ 1 j)))
       (set! f (+ m f))
       (track.set_time 
        param_track (+ i t_start) 
        (+ cg:initial_frame_offset (* (+ i t_start) frame_advance)))
       (set! i (+ i 1))
       )
    )
  )

(define (cg:add_trajectory_ola s_start s_frames state num_channels
                           param_track frame_advance)
"(cg:add_trajectory start n state num_channels)
Add trajectory to daughters of state, interpolating as necessary."
  (let ((j 0) (i 0) (s1l 0) (s2l 0) (m 0.0) (w 0.0)
        (t_start 0) (t_frames 0) (s_offset 0)
        (mceps1 nil) (mceps2 nil))

    (set! i 0)
    (while (< i s_frames)
     (if (equal? -1.0 (track.get clustergen_param_vectors (+ s_start i) 0))
         (set! s1l i))
     (set! i (+ i 1)))

    (if (and (item.prev state) 
             (item.relation.daughters (item.prev state) 'mcep_link)
             (> s1l 0))
        (begin ;; do overlap on previous 
          (set! mceps1 (item.relation.daughters (item.prev state) 'mcep_link))
          (set! first_half_delta (/ 1.0 (length mceps1)))
          (set! t_start (item.feat (car mceps1) "frame_number"))
          (set! t_frames (length mceps1))
          (set! m (/ s1l t_frames))
          (set! i 0)
          (set! w 0.0)
          (while (< i t_frames)
           (set! s_offset (nint (* i m)))
           (if (not (< s_offset s1l))
               (begin
;                 (format t "boing pre\n")
                 (set! s_offset (- s1l 1))))
           (set! s_pos (+ s_start s_offset))
           (if (< (track.get clustergen_param_vectors s_pos 0) 0)
               (format t "assigning pre -1/-2 %d %d %f\n" s_pos i m))
           ;; F0 Prediction
           (track.set param_track (+ i t_start) 0
                      (+ (* (- 1.0 w) (track.get param_track (+ i t_start) 0))
                         (* w (track.get clustergen_param_vectors s_pos 0))))

           ;; MCEP Prediction
           (set! j 1)
           (while (< j num_channels)
             (track.set param_track (+ i t_start) j 
              (+ (* (- 1.0 w) (track.get param_track (+ i t_start) j))
                 (* w 
                    (track.get clustergen_param_vectors s_pos 
                               (* (if cg:mlpg 1 2) j))
                    )
                 )
              )
             (set! j (+ 1 j)))
           (set! i (+ 1 i))
           (set! w (+ w first_half_delta))
           (if (> w 1.0) (set! w 1.0))
           )
          ))

    ;; do assignment on current unit 
    (set! mceps2 (item.relation.daughters state 'mcep_link))
    (set! t_start (item.feat (car mceps2) "frame_number"))
    (set! t_frames (length mceps2))
    (set! s2l (- s_frames (+ s1l 2)))
    (set! s2_start (+ s_start s1l 1))
    (set! m (/ s2l t_frames))
    (set! i 0)
    (while (< i t_frames)
     (set! s_offset (nint (* i m)))
     (if (not (< s_offset s2l))
         (set! s_offset (- s2l 1)))
     (set! s_pos (+ s2_start s_offset))
     (if (< (track.get clustergen_param_vectors s_pos 0) 0)
         (format t "assigning -1/-2 %d %d %f %f\n" s_pos i m
                 (track.get clustergen_param_vectors s_pos 0)))
     ;; F0 Prediction
     (track.set param_track (+ i t_start) 0
                (track.get clustergen_param_vectors s_pos 0))
     ;; MCEP Prediction
     (set! j 1)
     (while (< j num_channels)
      (track.set param_track (+ i t_start) j 
                 (track.get clustergen_param_vectors s_pos 
                            (* (if cg:mlpg 1 2) j)))
      (set! j (+ 1 j)))
     (track.set_time 
      param_track (+ i t_start) 
      (+ cg:initial_frame_offset (* (+ i t_start) frame_advance)))
     (set! i (+ 1 i))
    )
  )
)

;;; For ClusterGen_predict_mcep
;;;   take into account actual and delta and try to combine both
;                   (if (and nil (> i 0))
;                       (begin ;; something a little fancier
;                   (set! m1 (track.get cpv f (* 2 j)))         ;; mean1
;                   (set! s1 (track.get cpv f (+ (* 2 j) 1)))   ;; sdev1
;                   (set! m2 (track.get cpv f (+ 26 (* 2 j))))  ;; mean2 (delta)
;                   (set! s2 (track.get cpv f (+ 26 (* 2 j) 1)));; sdev2 (delta)
;                   (set! p1 (track.get param_track (- i 1) j)) ;; p.value

;                   (if (equal? s2 0)
;                       (set! p m1)
;                       (set! p (/ (+ m1 (+ m2 p1)) 2.0))
; ;                      (set! p (/ (+ (/ m1 s1) (/ (+ m2 p1) s2))
; ;                                 (+ (/ 1.0 s1) (/ 1.0 s2))))
;                       )
;                   (track.set param_track i j p)
; ;                  (format t "m1 %f s1 %f m2 %f s2 %f p %f\n"
; ;                          m1 s1 (+ p1 m2) s2 p)
;                   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  For VC adpatation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (build_cg_vc_source datafile)
  (mapcar
   (lambda (x)
     (format t "%s Build source files for VC adaptation\n" (car x))
     (set! utt1 (SynthText (cadr x)))
     (utt.save.wave utt1 (format nil "vc/wav/source/%s.wav" (car x)))
     (track.save (utt.feat utt1 "param_track") "param.track")
     (system (format nil "$FESTVOXDIR/src/vc/scripts/get_f0mcep %s param.track vc\n" (car x)))
     )
   (load datafile t))
  t
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sort of historical it should be set in INST_LANG_VOX_cg.scm
;; but maybe not in old instantiations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defvar cluster_synth_method 
;  (if (boundp 'mlsa_resynthesis)
;      cg_wave_synth
;      cg_wave_synth_external ))

; (require 'hsm_cg)

(define (cg_wave_synth_deltas utt)
  ;; before we had it built-in to Festival
  (let ((trackname (make_tmp_filename))
        (wavename (make_tmp_filename))
        )
    (track.save (utt.feat utt "param_track") trackname "est")
    (system
     (format nil "$FESTVOXDIR/src/clustergen/cg_resynth_deltas %s %s"
             trackname wavename))
    (utt.import.wave utt wavename)
    (delete-file trackname)
    (delete-file wavename)
    utt)
)
(set! cluster_synth_method cg_wave_synth)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'clustergen)
