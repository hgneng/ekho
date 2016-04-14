;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                   Carnegie Mellon University and                      ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
;;;                       Copyright (c) 1998-2001                         ;;
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
;;;  THE UNIVERSITY OF EDINBURGH, CARNEGIE MELLON UNIVERSITY AND THE      ;;
;;;  CONTRIBUTORS TO THIS WORK DISCLAIM ALL WARRANTIES WITH REGARD TO     ;;
;;;  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY   ;;
;;;  AND FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF EDINBURGH, CARNEGIE ;;
;;;  MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE FOR ANY SPECIAL,    ;;
;;;  INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER          ;;
;;;  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  AN ACTION   ;;
;;;  OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF     ;;
;;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.       ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Cluster Unit selection support (Black and Taylor Eurospeech '97)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Run-time support, selection and synthesis and some debugging functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require_module 'clunits)

(defvar cluster_synth_pre_hooks nil)
(defvar cluster_synth_post_hooks nil)

(defvar clunits_time time)  ;; some old voices might use this

(defSynthType Cluster
    (apply_hooks cluster_synth_pre_hooks utt)
    (Clunits_Select utt)
    (Clunits_Get_Units utt)
    (Clunits_Join_Units utt)
    (apply_hooks cluster_synth_post_hooks utt)
    utt
)

(define (Clunits_Join_Units utt)
  "(Clunits_Join_Units utt)
Join the preselected and gotten units into a waveform."
  (let ((join_method (get_param 'join_method clunits_params 'simple)))
    ;; Choice of function to put them together
    (cond
     ((string-equal join_method 'windowed)
      (Clunits_Windowed_Wave utt)
      (clunits::fix_segs_durs utt))
     ((string-equal join_method 'smoothedjoin)
      (Clunits_SmoothedJoin_Wave utt)
      (clunits::fix_segs_durs utt))
     ((string-equal join_method 'none)
      t)
     ((string-equal join_method 'modified_lpc)
      (defvar UniSyn_module_hooks nil)
      (Param.def "unisyn.window_name" "hanning")
      (Param.def "unisyn.window_factor" 1.0)
      (Parameter.def 'us_sigpr 'lpc)
      (mapcar 
       (lambda (u s)
	 (item.set_feat s "source_end" (item.feat u "end")))
       (utt.relation.items utt 'Unit)
       (utt.relation.items utt 'Segment))
      (us_unit_concat utt)
      (if (not (member 'f0 (utt.relationnames utt)))
	  (targets_to_f0 utt))
      (if (utt.relation.last utt 'Segment)
	  (set! pm_end (+ (item.feat (utt.relation.last utt 'Segment) "end")
			  0.02))
	  (set! pm_end 0.02))
      (us_f0_to_pitchmarks  utt 'f0 'TargetCoef pm_end)
      (us_mapping utt 'segment_single)
      (us_generate_wave utt (Parameter.get 'us_sigpr)
			'analysis_period))
     ((string-equal join_method 'smoothed_lpc)
;      (format t "smoothed_lpc\n")
      (defvar UniSyn_module_hooks nil)
      (Param.def "unisyn.window_name" "hanning")
      (Param.def "unisyn.window_factor" 1.0)
      (Parameter.def 'us_sigpr 'lpc)
      (mapcar 
       (lambda (u s)
	 (item.set_feat s "source_end" (item.feat u "end"))
	 (item.set_feat s "unit_duration" 
			(- (item.feat u "seg_end") (item.feat u "seg_start")))
	 )
       (utt.relation.items utt 'Unit)
       (utt.relation.items utt 'Segment))
      (us_unit_concat utt)
      (mapcar 
       (lambda (u s)
	 (item.set_feat s "num_frames" (item.feat u "num_frames")))
       (utt.relation.items utt 'Unit)
       (utt.relation.items utt 'Segment))
      (if (not (member 'f0 (utt.relationnames utt)))
	  (targets_to_f0 utt))
      (if (utt.relation.last utt 'Segment)
	  (set! pm_end (+ (item.feat (utt.relation.last utt 'Segment) "end")
			  0.02))
	  (set! pm_end 0.02))
      (us_f0_to_pitchmarks  utt 'f0 'TargetCoef pm_end)
      (cl_mapping utt clunits_params)
      (us_generate_wave utt (Parameter.get 'us_sigpr)
			'analysis_period))
     (t
      (Clunits_Simple_Wave utt)))
    utt
  )
)

(define (clunits::units_selected utt filename)
  "(clunits::units_selected utt filename)
Output selected unitsfile indexes for each unit in the given utterance.
Results saved in given file name, or stdout if filename is \"-\"."
  (let ((fd (if (string-equal filename "-")
		t
		(fopen filename "w")))
	(end 0)
	(sample_rate
	 (cadr (assoc 'sample_rate (wave.info (utt.wave utt))))))
    (format fd "#\n")
    (mapcar
     (lambda (s)
       (let ((dur (/ (- (item.feat s "samp_end")
		      (item.feat s "samp_start"))
		   sample_rate))
	     (start (/ (item.feat s "samp_start") sample_rate)))
	 (set! end (+ end dur))
	 (format fd "%f 125 %s ; %s %10s %f %f %f\n"
		 end
		 (string-before (item.name s) "_")
		 (item.name s)
		 (item.feat s "fileid")
		 (item.feat s "unit_start")
		 (item.feat s "unit_middle")
		 (item.feat s "unit_end"))
	 ))
     (utt.relation.items utt 'Unit))
    (if (not (string-equal filename "-"))
	(fclose fd))
    t))

(define (clunits::units_segs utt filename)
  "(clunits::units_segs utt filename)
Svaes the unit selections (alone) for display."
  (let ((fd (if (string-equal filename "-")
		t
		(fopen filename "w")))
	(end 0)
	(sample_rate
	 (cadr (assoc 'sample_rate (wave.info (utt.wave utt))))))
    (format fd "#\n")
    (mapcar
     (lambda (s)
       (let ((dur (/ (- (item.feat s "samp_end")
		      (item.feat s "samp_start"))
		   sample_rate))
	     (start (/ (item.feat s "samp_start") sample_rate)))
	 (set! end (+ end dur))
	 (format fd "%f 125 %s \n"
		 end
		 (string-before (item.name s) "_")
;		 (item.name s)
		 )
	 ))
     (utt.relation.items utt 'Unit))
    (if (not (string-equal filename "-"))
	(fclose fd))
    t))

(define (clunits::fix_segs_durs utt)
  "(clunits::fix_segs_durs utt)
Takes the actual unit times and places then back on the segs."
  (let ((end 0)
	(sample_rate
	 (cadr (assoc 'sample_rate (wave.info (utt.wave utt))))))
    (mapcar
     (lambda (u s)
       (let ((dur (/ (- (item.feat u "samp_end")
		      (item.feat u "samp_start"))
		   sample_rate))
	     (seg_start (/ (- (item.feat u "samp_seg_start")
			    (item.feat u "samp_start"))
			 sample_rate)))
	 (if (item.prev s)
	     (item.set_feat (item.prev s) "end" 
			    (+ (item.feat s "p.end") seg_start)))
	 (set! end (+ end dur))
	 (item.set_feat s "end" end)))
     (utt.relation.items utt 'Unit)
     (utt.relation.items utt 'Segment)
     )
    utt))

(define (clunits::display utt)
  "(clunits::display utt)
Display utterance with emulabel.  Note this saves files in
scratch/wav/ and scratch/lab/."
  (let ((id "cl01"))
    (utt.save.wave utt (format nil "scratch/wav/%s.wav" id))
    (utt.save.segs utt (format nil "scratch/lab/%s.lab" id))
    (system "cd scratch; emulabel ../etc/emu_lab cl01 &")
    t))

; (define (clunits::debug_resynth_units utt)
;   "(clunits::debug_resynth_units utt)
; Check each of the units in utt against the related label
; files and re-synth with any given new boundaries.  Note this is 
; will only work if the segment still overlaps with its original and
; also note that with a rebuild of the clunits db a complete different
; set of units may be selected for this utterance."
;   (let ()
;     (mapcar 
;      (lambda (unit)
;        (clunits::check_unit_boundaries unit))
;      (utt.relation.items utt 'Unit))
;     ;; This can't be done like this ... 
;     (Clunits_Get_Units utt)  ;; get unit signal/track stuff
;     (Clunits_Join_Units utt) ;; make a complete waveform
;     (apply_hooks cluster_synth_post_hooks utt)
;     utt)
; )

(define (clunits::join_parameters utt)
  "(clunits::join_parameters utt)
Join selected paremeters (rather than the signal), used in F0 and
Articulatory selection."
  (let ((params nil)
	(num_channels 0)
	(num_frames 0 ))

    (mapcar
     (lambda (unit)
       (set! num_frames 
	     (+ num_frames
		(track.num_frames (item.feat unit "coefs"))))
       (set! num_channels (track.num_channels (item.feat unit "coefs")))
       (format t "coounting %d %d\n" num_frames num_channels)
       )
     (utt.relation.items utt 'Unit))
    
    (set! params (track.resize nil 0 num_channels))
    
    (mapcar
     (lambda (unit)
       (set! frames 0)
       (format t "inserting \n")
       (format t "%l %l %l %l %l\n"
	       params (track.num_frames params)
	       (item.feat unit "coefs") 0
	       (track.num_frames (item.feat unit "coefs")))
       (track.insert
	params (track.num_frames params)
	(item.feat unit "coefs") 0
	(track.num_frames (item.feat unit "coefs")))
       )
     (utt.relation.items utt 'Unit))

    (utt.relation.create utt "AllCoefs")
    (set! coefs_item (utt.relation.append utt "AllCoefs"))
    (item.set_feat coefs_item "name" "AllCoefs")
    (item.set_feat coefs_item "AllCoefs" params)
  
    utt
))


(provide 'clunits)
