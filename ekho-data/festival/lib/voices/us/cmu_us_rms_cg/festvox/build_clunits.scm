;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                     Carnegie Mellon University                      ;;;
;;;                  and Alan W Black and Kevin Lenzo                   ;;;
;;;                      Copyright (c) 1998-2005                        ;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;; Code for building data for prompts, aligning and unit selection     ;;;
;;; synthesizer                                                         ;;;
;;;                                                                     ;;;
;;; This file is only used at database build time                       ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cmu_us_rms::dir ".")

(require 'clunits_build)

;;; Basic voice definition file with voice defines and clunit
;;; parameter definition for run time.
(load "festvox/cmu_us_rms_clunits.scm")

(defvar cluster_feature_filename "all.desc")
(defvar split_long_silences t)  ;; good for unit selection

;;; Add Build time parameters
(set! cmu_us_rms::dt_params
      (cons
       ;; in case cmu_us_rms_clunits defines this too, put this at start
       (list 'db_dir (string-append cmu_us_rms::dir "/"))
       (append
	cmu_us_rms::dt_params
	(list
	;;; In cmu_us_rms_clunits.scm
	 ;;'(coeffs_dir "lpc/")
	 ;;'(coeffs_ext ".lpc")
	 '(disttabs_dir "festival/disttabs/")
	 '(utts_dir "festival/utts/")
	 '(utts_ext ".utt")
	 '(dur_pen_weight 0.0)
	 '(f0_pen_weight 0.0)
	 '(get_stds_per_unit t)
	 '(ac_left_context 0.8)
	 '(ac_weights
	   (0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5))
	 ;; Join weights in cmu_us_rms_clunits.scm
	 ;; Features for extraction
	 '(feats_dir "festival/feats/")
         ;; Feats as defined in all.desc
         (list
          'feats
          (mapcar car (car (load (format nil "festival/clunits/%s" 
                                    cluster_feature_filename) t))))
	 ;; Wagon tree building params
;	 (trees_dir "festival/trees/")  ;; in cmu_us_rms_clunits.scm
         (list
          'wagon_field_desc 
          (format nil "festival/clunits/%s" 
                  cluster_feature_filename))
	 '(wagon_progname "$ESTDIR/bin/wagon")
	 '(wagon_cluster_size 20)
	 '(prune_reduce 0)
	 '(cluster_prune_limit 40)
	 ;; The dictionary of units used at run time
;	 (catalogue_dir "festival/clunits/")  ;; in cmu_us_rms_clunits.scm
	 ;;  Run time parameters 
	 ;; all in cmu_us_rms_clunits.scm
	 ;; Files in db, filled in at build_clunits time
	 ;; (files ("time0001" "time0002" ....))
))))

(define (build_clunits file)
  "(build_clunits file)
Build cluster synthesizer for the given recorded data and domain."
  (build_clunits_init file)
  (do_all)  ;; someday I'll change the name of this function
)

(define (build_clunits_init file)
  "(build_clunits_init file)
Get setup ready for (do_all) (or (do_init))."
  (eval (list cmu_us_rms::closest_voice))
  (cmu_us_rms::select_phoneset)
  (cmu_us_rms::select_tokenizer)
  (cmu_us_rms::select_tagger)
  (cmu_us_rms::select_lexicon)

  ;; Add specific fileids to the list for this run
  (set! cmu_us_rms::dt_params
	(append
	 cmu_us_rms::dt_params
	 (list
	  (list
	   'files
	   (mapcar car (load file t))))))
  
  (set! dt_params cmu_us_rms::dt_params)
  (set! clunits_params cmu_us_rms::dt_params)
)

(define (do_prompt name text) 
  "(do_prompt name text) 
Synthesize given text and save waveform and labels for prompts."
  (let ((utt1 (utt.synth (eval (list 'Utterance 'Text text)))))
    (utt.save utt1 (format nil "prompt-utt/%s.utt" name))
    (utt.save.segs utt1 (format nil "prompt-lab/%s.lab" name))
    (if (member_string "Wave" (utt.relationnames utt1))
        (utt.save.wave utt1 (format nil "prompt-wav/%s.wav" name)))
    t))

(define (build_prompts_waves file)
  "(build_prompt file) 
For each utterances in prompt file, synth and save waveform and
labels for prompts and aligning."
  (set! cmu_us_rms::clunits_prompting_stage t)
  (voice_cmu_us_rms_clunits)
 (let ((p (load file t)))
    (mapcar
     (lambda (l)
       (format t "%s PROMPTS with waves\n" (car l))
       (unwind-protect
        (do_prompt (car l) (cadr l))
        nil)
       t)
     p)
    t))

(define (find_silence_name)
  (set! cmu_us_rms::clunits_prompting_stage t)
  (voice_cmu_us_rms_clunits)
  (set! silence (car (cadr (car (PhoneSet.description '(silences))))))
  (set! sfd (fopen "etc/silence" "w"))
  (format sfd "%s\n" silence)
  (fclose sfd)
)

(define (build_prompts file)
  "(build_prompt file) 
For each utterances in prompt file, synth and save waveform and
labels for prompts and aligning."
  (set! cmu_us_rms::clunits_prompting_stage t)
  (voice_cmu_us_rms_clunits)
  (Parameter.set 'Synth_Method 'None)
 (let ((p (load file t)))
    (mapcar
     (lambda (l)
       (format t "%s PROMPTS\n" (car l))
       (unwind-protect
        (do_prompt (car l) (cadr l))
        nil)
       t)
     p)
    t))

(define (build_utts file)
  "(build_utts file) 
For each utterances in prompt file, synthesize and merge aligned labels
to predicted labels building a new utetrances and saving it."
  (set! cmu_us_rms::clunits_prompting_stage t)
  (voice_cmu_us_rms_clunits)
  (let ((p (load file t)))
    (mapcar
     (lambda (l)
       (format t "%s UTTS\n" (car l))
       (unwind-protect
        (align_utt (car l) (cadr l))
        nil)
       t)
     p)
    t))

(define (align_utt name text)
  "(align_utts file) 
Synth an utterance and load in the actualed aligned segments and merge
them into the synthesizer utterance."
  (let ((utt1 (utt.load nil (format nil "prompt-utt/%s.utt" name)))
	;(utt1 (utt.synth (eval (list 'Utterance 'Text text))))
	(silence (car (cadr (car (PhoneSet.description '(silences))))))
	segments actual-segments)

    (set! my_silence silence)
    (utt.relation.load utt1 'actual-segment 
		       (format nil "lab/%s.lab" name))
    (set! segments (utt.relation.items utt1 'Segment))
    (set! actual-segments (utt.relation.items utt1 'actual-segment))

    ;; These should align, but if the labels had to be hand edited
    ;; then they may not, we cater here for insertions and deletions
    ;; of silences int he corrected hand labelled files (actual-segments)
    ;; If you need to something more elaborate you'll have to change the
    ;; code below.
    (while (and segments actual-segments)
      (cond
       ((string-equal (string-append "#" (item.name (car segments)))
                      (item.name (car actual-segments)))
        ;; junk unit that is to be ignored
        (item.set_feat (car segments) "end"
                       (item.feat (car actual-segments) "end"))
        (item.set_feat (car segments) "ignore" "1")
        (set! segments (cdr segments))
        (set! actual-segments (cdr actual-segments)))
       ((and (not (string-equal (item.name (car segments))
				(item.name (car actual-segments))))
	     (or (string-equal (item.name (car actual-segments)) silence)
                 (string-equal (item.name (car actual-segments)) "ssil")
		 (string-equal (item.name (car actual-segments)) "H#")
		 (string-equal (item.name (car actual-segments)) "h#")))
	(item.insert
	 (car segments)
	 (list silence (list (list "end" (item.feat 
					(car actual-segments) "end"))))
	 'before)
	(set! actual-segments (cdr actual-segments)))
       ((and (not (string-equal (item.name (car segments))
				(item.name (car actual-segments))))
             (string-equal (item.name (car segments)) silence))
	(item.delete (car segments))
	(set! segments (cdr segments)))
       ((string-equal (item.name (car segments))
		      (item.name (car actual-segments)))
	(item.set_feat (car segments) "end" 
		       (item.feat (car actual-segments) "end"))
	(set! segments (cdr segments))
	(set! actual-segments (cdr actual-segments)))
       (t
	(format stderr
		"align missmatch at %s (%f) %s (%f)\n"
		(item.name (car segments))
		(item.feat (car segments) "end")
		(item.name (car actual-segments))
		(item.feat (car actual-segments) "end"))
	(error)))
      )

    (mapcar
     (lambda (a)
      ;; shorten and split sliences
      (while (and (string-equal (item.name a) silence)
		  (> (item.feat a "segment_duration") 0.300))
;              (format t "splitting %s silence of %f at %f\n"
;		      (item.name a)
;                      (item.feat a "segment_duration")
;                      (item.feat a "end"))
              (cond
               ((string-equal "h#" (item.feat a "p.name"))
                (item.set_feat (item.prev a) "end"
                               (+ 0.150 (item.feat a "p.end"))))
               ((and (string-equal silence (item.feat a "p.name"))
                     (string-equal silence (item.feat a "p.p.name")))
                (item.set_feat (item.prev a) "end"
                               (+ 0.150 (item.feat a "p.end")))
                (item.set_feat (item.prev a) "name" silence))
               (t
                (item.insert a
                             (list silence
                                   (list 
                                    (list "end" 
				      (+ 0.150 
					(item.feat a "p.end")))))
                             'before)))))
     (if split_long_silences
         (utt.relation.items utt1 'Segment)
         nil))

    (utt.relation.delete utt1 'actual-segment)
    (utt.set_feat utt1 "fileid" name)
    ;; If we have an F0 add in targets too
    (if (probe_file (format nil "f0/%s.f0" name))
	(build::add_targets utt1))
    (rephrase utt1)
    (utt.save utt1 (format nil "festival/utts/%s.utt" name))
    t))

(defvar my_silence "pau")
(define (pau_duration s)
  (cond 
   ((null s) 0.0)
   ((string-equal my_silence (item.name s))
    (+ (item.feat s "segment_duration")
       (pau_duration (item.next s))))
   (t
    0.0)))

(define (rephrase utt)
  "(rephrase utt)
remove phrasing and recreate it based on the silences in the segment stream."
  (let ((silence (car (cadr (car (PhoneSet.description '(silences)))))))

    (utt.relation.delete utt 'Phrase)
    (utt.relation.create utt 'Phrase)
    (set! topphrase nil)

    (mapcar
     (lambda (w)
       (if (null topphrase)
           (begin
             (set! topphrase (utt.relation.append utt 'Phrase nil))
             (item.set_feat topphrase "name" "B")))
       (item.relation.append_daughter topphrase 'Phrase w)
       (if (and (item.next w)
                (string-equal 
                 silence 
                 (item.feat 
                  w "R:SylStructure.daughtern.daughtern.R:Segment.n.name"))
                (> (item.feat
                    w
                    "R:SylStructure.daughtern.daughtern.R:Segment.n.lisp_pau_duration")
                    0.080))
           (set! topphrase nil))
       )
     (utt.relation.items utt 'Word))

    ;; Not sure if last phrase should get a BB or not
    (if topphrase
        (item.set_feat topphrase "name" "BB"))
  )
)

(define (rebuild_utts file)
  "(rebuild_utts file) 
Rebuild the utterances from the label files (lab, syl, wrd, phr). Used
after hand correction, or when files come from somewhere else."
  (set! cmu_us_rms::clunits_prompting_stage t)
  (voice_cmu_us_rms_clunits)
  (Parameter.set 'Synth_Method 'None)
  (let ((p (load file t)))
    (mapcar
     (lambda (l)
       (format t "rebuild %s UTTS\n" (car l))
       (unwind-protect
        (align_utt_rebuild (car l) (cadr l))
        nil)
       t)
     p)
    t))

(define (align_utt_rebuild name text)
  "(align_utts file) 
Synth an utterance and load in the actualed aligned segments and merge
them into the synthesizer utterance."
  (let (; (utt1 (utt.load nil (format nil "prompt-utt/%s.utt" name)))
	(utt1 (utt.synth (eval (list 'Utterance 'Text text))))
	(silence (car (cadr (car (PhoneSet.description '(silences))))))
	segments actual-segments)
	
    (utt.relation.load utt1 'actual-segment 
		       (format nil "lab/%s.lab" name))

    (if (probe_file (format nil "wrd/%s.wrd" name))
	;; There more structure already on disk so adopt it
	(build_word_structure utt1 name))

    (if (probe_file (format nil "phr/%s.phr" name))
	;; There more structure already on disk so adopt it
	(build_phrase_structure utt1 name))

    (set! segments (utt.relation.items utt1 'Segment))
    (set! actual-segments (utt.relation.items utt1 'actual-segment))

    ;; These should align, but if the labels had to be hand edited
    ;; then they may not, we cater here for insertions and deletions
    ;; of silences in the corrected hand labelled files (actual-segments)
    ;; If you need to something more elaborate you'll have to change the
    ;; code below.
    (while (and segments actual-segments)
      (cond
       ((string-equal (string-append "#" (item.name (car segments)))
                      (item.name (car actual-segments)))
        ;; junk unit that is to be ignored
        (item.set_feat (car segments) "end"
                       (item.feat (car actual-segments) "end"))
        (item.set_feat (car segments) "ignore" "1")
        (set! segments (cdr segments))
        (set! actual-segments (cdr actual-segments)))
       ((and (not (string-equal (item.name (car segments))
				(item.name (car actual-segments))))
	     (or (string-equal (item.name (car actual-segments)) silence)
		 (string-equal (item.name (car actual-segments)) "H#")
		 (string-equal (item.name (car actual-segments)) "h#")))
	(item.insert
	 (car segments)
	 (list silence (list (list "end" (item.feat 
					(car actual-segments) "end"))))
	 'before)
	(set! actual-segments (cdr actual-segments)))
       ((and (not (string-equal (item.name (car segments))
				(item.name (car actual-segments))))
	     (string-equal (item.name (car segments)) silence))
	(item.delete (car segments))
	(set! segments (cdr segments)))
       ((string-equal (item.name (car segments))
		      (item.name (car actual-segments)))
	(item.set_feat (car segments) "end" 
		       (item.feat (car actual-segments) "end"))
	(set! segments (cdr segments))
	(set! actual-segments (cdr actual-segments)))
       (t
	(format stderr
		"align missmatch at %s (%f) %s (%f)\n"
		(item.name (car segments))
		(item.feat (car segments) "end")
		(item.name (car actual-segments))
		(item.feat (car actual-segments) "end"))
	(error)))
      )

    (mapcar
     (lambda (a)
      ;; shorten and split sliences
      (while (and (string-equal (item.name a) silence)
		  (> (item.feat a "segment_duration") 0.300))
;              (format t "splitting %s silence of %f at %f\n"
;		      (item.name a)
;                      (item.feat a "segment_duration")
;                      (item.feat a "end"))
              (cond
               ((string-equal "h#" (item.feat a "p.name"))
                (item.set_feat (item.prev a) "end"
                               (+ 0.150 (item.feat a "p.end"))))
               ((and (string-equal silence (item.feat a "p.name"))
                     (string-equal silence (item.feat a "p.p.name")))
                (item.set_feat (item.prev a) "end"
                               (+ 0.150 (item.feat a "p.end")))
                (item.set_feat (item.prev a) "name" silence))
               (t
                (item.insert a
                             (list silence
                                   (list 
                                    (list "end" 
				      (+ 0.150 
					(item.feat a "p.end")))))
                             'before)))))
     (utt.relation.items utt1 'Segment))

    (utt.relation.delete utt1 'actual-segment)
    (utt.set_feat utt1 "fileid" name)
    ;; If we have an F0 add in targets too
    (if (probe_file (format nil "f0/%s.f0" name))
	(build::add_targets utt1))
    (utt.save utt1 (format nil "festival/utts/%s.utt" name))
    (cl.utt.save.syllables utt1 (format nil "syl/%s.syl" name))
    (cl.utt.save.words utt1 (format nil "wrd/%s.wrd" name))
    (cl.utt.save.phr utt1 (format nil "phr/%s.phr" name))
    t))


(define (build_labs file)
  "(build_utts file) 
For each utterances in prompt file, synthesize and merge aligned labels
to predicted labels building a new utetrances and saving it."
  (let ((p (load file t)))
    (mapcar
     (lambda (l)
       (let ((name (car l)))
	 (format t "%s\n" (car l))
	 (set! utt1 (utt.load nil (format nil "festival/utts/%s.utt" name)))
	 (cl.utt.save.labs utt1 (format nil "lab/%s.lab" name))
	 (cl.utt.save.syllables utt1 (format nil "syl/%s.syl" name))
	 (cl.utt.save.words utt1 (format nil "wrd/%s.wrd" name))
	 (cl.utt.save.phr utt1 (format nil "phr/%s.phr" name))))
     p)))

(define (cl.utt.save.labs utt filename)
"(utt.save.syllables UTT FILE)
  Save syllables of UTT in a FILE in xlabel format."
  (let ((fd (fopen filename "w")))
    (format fd "#\n")
    (mapcar
     (lambda (s)
       (format fd "%2.4f 100 %s%s\n"
               (item.feat s "segment_end")
	       (if (assoc 'ignore (item.features s))
		   "#"
		   "")
               (item.name s))
               )
     (utt.relation.items utt 'Segment))
    (fclose fd)
    utt))

(define (cl.safe_end i endname)
  (let ((e (item.feat i endname)))
    (if (and (equal? 0 e) (item.prev i))
	(cl.safe_end (item.prev i) endname)
	e)))

(define (build_word_structure utt name)
  "(build_word_structure utt)
Build Word and Syllable, SylStructure and Segments from the labels
on disk."
  (let (wrd syl seg)
  
    (utt.relation.delete utt 'Word)
    (utt.relation.delete utt 'SylSructure)
    (utt.relation.delete utt 'Syllable)
    (utt.relation.delete utt 'Segment)
    (utt.relation.delete utt 'Phrase)

    (utt.relation.load utt 'Word (format nil "wrd/%s.wrd" name))
    (utt.relation.load utt 'Syllable (format nil "syl/%s.syl" name))
    (utt.relation.load utt 'Segment (format nil "lab/%s.lab" name))

    (mapcar
     (lambda (syl)
       (let ((s (item.name syl)))
         (item.set_feat syl "stress" s)
	 (item.set_name syl "syl")))
     (utt.relation.items utt 'Syllable))

    ;; Syllable and Word have h# so drop them
    (if (string-equal "h#" (item.name (utt.relation.first utt 'Syllable)))
        (item.delete (utt.relation.first utt 'Syllable)))
    (if (string-equal "h#" (item.name (utt.relation.first utt 'Word)))
        (item.delete (utt.relation.first utt 'Word)))
    (if (string-equal "h#" (item.name (utt.relation.first utt 'Segment)))
        (item.delete (utt.relation.first utt 'Segment)))

    (utt.relation.create utt 'SylStructure)
    (utt.relation.create utt 'Phrase)
    (set! phr nil)
    (set! syl (utt.relation.first utt 'Syllable))
    (set! seg (utt.relation.first utt 'Segment))
    (set! p_sylend 0)
    (set! p_segend 0)
    (mapcar
     (lambda (w)
       (if (not phr)
	   (set! phr (utt.relation.append utt 'Phrase)))
       (item.relation.append_daughter phr 'Phrase w)
       (set! end (item.feat w "end"))
       (item.remove_feature w "end")
       (set! sw (utt.relation.append utt 'SylStructure w))
       (while (and syl (< (/ (+ (item.feat syl "end") p_sylend) 2.0) end))
	 (set! sylend (item.feat syl "end"))
	 (item.remove_feature syl "end")
	 (set! ss (item.relation.append_daughter sw 'SylStructure syl))
	 (while (and seg (< (/ (+ (item.feat seg "end") p_segend) 2.0) sylend))
	    (if (string-matches (item.name seg) "#.*")
		(begin
		  (item.set_feat seg "ignore" "1")
		  (item.set_name seg (string-after (item.name seg) "#"))))
	    (if (string-matches (item.name seg) "%.*")
		(begin
		  (item.set_feat seg "ignore" "1")
		  (item.set_name seg (string-after (item.name seg) "%"))))
	    (if (not (phone_is_silence (item.name seg)))
		(item.relation.append_daughter ss 'SylStructure seg))
	    (set! p_segend (item.feat seg "end"))
	    (set! seg (item.next seg)))
	 (set! p_sylend sylend)
	 (set! syl (item.next syl)))
       (if (or (null seg) (phone_is_silence (item.name seg)))
	   (set! phr nil))
       )
     (utt.relation.items utt 'Word))

    ;; We need to fix up *all* segment names to remove #, not just
    ;; ones that fall within syllables (it was breaking for pauses)
    (mapcar
     (lambda (seg)
       (if (string-matches (item.name seg) "#.*")
	   (begin
	     (item.set_feat seg "ignore" "1")
	     (item.set_name seg (string-after (item.name seg) "#"))))
       (if (string-matches (item.name seg) "%.*")
	   (begin
	     (item.set_feat seg "ignore" "1")
	     (item.set_name seg (string-after (item.name seg) "%")))))
     (utt.relation.items utt 'Segment))

    (set! ls (utt.relation.last utt 'Segment))
    (if (or (not ls) (not (phone_is_silence (item.name ls))))
	(format t "final phone is not silence %s\n" (item.name ls)))

    ;; Some day we'll get these from files
    (utt.relation.delete utt 'Intonation)
    (utt.relation.delete utt 'IntEvent)

;    (find_lexical_stress utt)

    (Intonation utt)
;    (Duration utt)
    (Int_Targets utt)
    )
)  

(define (build_phrase_structure utt name)
  "(build_phrase_structure utt)
This builds phrasing, but is done in a different function from 
from word structure as this may (for historical reasons) not
exist on all dbs."
  (let (phr wrd)
  
    (utt.relation.delete utt 'Phrase)

    (utt.relation.load utt 'Phrase (format nil "phr/%s.phr" name))
    (item.delete (utt.relation.first utt 'Phrase)) ;; delete h#

    (mapcar
     (lambda (p)
       (item.set_feat p "phrase_type" (item.name p))
       (item.set_name p "B"))
     (utt.relation.items utt 'Phrase))
    (item.set_name (utt.relation.last utt 'Phrase) "BB")

    (set! wrd (utt.relation.first utt 'Word))
    (set! phr (utt.relation.first utt 'Phrase))
    (set! p_wordend 0)
    (while phr
      (while (and wrd (< (/ (+ (item.feat wrd "word_end") 
			       (item.feat wrd "p.word_end")) 2.0)
			 (item.feat phr "end")))
	     (item.relation.append_daughter phr 'Phrase wrd)
	     (set! wrd (item.next wrd)))
      (item.remove_feature phr "end")
      (set! phr (item.next phr)))

))

(define (cl.utt.save.syllables utt filename)
"(utt.save.syllables UTT FILE)
  Save syllables of UTT in a FILE in xlabel format."
  (let ((fd (fopen filename "w")))
    (format fd "#\n")
    (format fd "%2.4f 100 h#\n" 
	    (item.feat (utt.relation.first utt 'Syllable) "syllable_start"))
    (mapcar
     (lambda (syl)
       (format fd "%2.4f 100 %s%s\n"
               (cl.safe_end syl "syllable_end")
               (item.feat syl "stress")
               (cond
                ((or (string-equal "none" (item.feat syl "accentedness"))
		     (string-equal "0" (item.feat syl "accentedness")))
                 "")
                (t
                  (item.feat syl "accentedness")))))
     (utt.relation.items utt 'Syllable))
    (fclose fd)
    utt))

(define (cl.utt.save.words utt filename)
"(utt.save.words UTT FILE)
  Save words of UTT in a FILE in xlabel format."
  (let ((fd (fopen filename "w")))
    (format fd "#\n")
    (format fd "%2.4f 100 h#\n" 
	    (item.feat (utt.relation.first utt 'Word) "word_start"))
    (mapcar
     (lambda (w)
       (format fd "%2.4f 100 %s\n" 
	       (cl.safe_end w "word_end")
	       (downcase (item.name w))))
     (utt.relation.items utt 'Word))
    (fclose fd)
    utt))

(define (cl.utt.save.phr utt filename)
"(utt.save.phr UTT FILE)
  Save phrases of UTT in a FILE in xlabel format."
  (let ((fd (fopen filename "w"))
	(phrs (utt.relation.first utt 'Phrase)))
    (format fd "#\n")
    (format fd "%2.4f 100 h#\n" 
	    (item.feat phrs 
	     "daughter1.R:SylStructure.daughter1.daughter1.segment_start"))
    (while phrs
      (if (member_string (item.name phrs) '("H" "L"))
	  (set! plab (item.name phrs))
	  (set! plab "L"))
      (format fd "%2.4f 100 %s\n" 
	      (item.feat phrs "daughtern.word_end")
	      plab)
      (set! phrs (item.next phrs)))
    (fclose fd)
    utt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Some prosody modeling code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build::add_targets utt)
  "(build::add_targets utt)
Adds targets based on the F0 in f0/*.f0.  Adds a point to each vowel."
  (let ((fileid (utt.feat utt "fileid"))
	(f0_points))
    (set! f0_points (build::load_f0_points fileid))
    (set! awb_f0_points f0_points)
    ;; Get rid of the old one
    (utt.relation.delete utt 'Target)
    ;; Create a new one
    (utt.relation.create utt 'Target)
    (build::add_target 
     utt
     f0_points)
    utt))

(define (build::add_target utt f0_points)
  "(build::add_target utt f0_points)
Add F0 points at start or syllable, mid point of each vowel, and
last segment before silence.  The F0 continued over non-voiced
periods is such a naive and hopless way its embarrassing."
  (let ((s (utt.relation.first utt 'Segment))
	(f0s f0_points)
	targ)
    (while s
     (if (and (not (member_string
		    (item.name s)
		    (cadr (car (PhoneSet.description '(silences))))))
	      (or (string-equal "1" (item.feat s "syl_initial"))
		  (string-equal "+" (item.feat s "ph_vc"))
		  (member_string 
		   (item.feat s "n.name")
		   (cadr (car (PhoneSet.description '(silences)))))))
	 (begin
	   (set! targ (utt.relation.append utt 'Target s))
	   (if (string-equal "1" (item.feat s "syl_initial"))
	       (item.relation.append_daughter
		targ
		'Target
		(list
		 "0"
		 (list
		  (list 'f0 (build::get_f0_at f0s (item.feat s "segment_start")))
		  (list 'pos (item.feat s "segment_start"))))))
	   (if (string-equal "+" (item.feat s "ph_vc"))
	       (item.relation.append_daughter
		targ
		'Target
		(list
		 "0"
		 (list
		  (list 'f0 (build::get_f0_at f0s (item.feat s "segment_mid")))
		  (list 'pos (item.feat s "segment_mid"))))))
	   (if (member_string 
		(item.feat s "n.name")
		(cadr (car (PhoneSet.description '(silences)))))
	       (item.relation.append_daughter
		targ
		'Target
		(list
		 "0"
		 (list
		  (list 'f0 (build::get_f0_at f0s (item.feat s "segment_end")))
		  (list 'pos (item.feat s "segment_end"))))))))
     (set! s (item.next s))
     ))
)

(define (build::get_f0_at f0s position)
  "(build::get_f0_at f0s position)
Returns the non-zero F0 nearest to position."
  (build::get_f0_at_2
   -1
   f0s
   position))

(define (build::get_f0_at_2 f0 f0s position)
  "(build::get_f0_at f0 f0s position)
Returns the non-zero F0 nearest to position."
  (cond
   ((null f0s)
    (if (> f0 0)
	f0
	110 ;; aint nothing there at all at all
	))
   (t
    (if (> 0 (cadr (car f0s)))
	(set! f0 (cadr (car f0s))))
    (cond
     ((and (>= position (car (car f0s)))
	   (<= position (car (cadr f0s))))
      (if (< f0 1)
	  (build::find_first_f0 f0s)
	  f0))
     (t
      (build::get_f0_at_2 f0 (cdr f0s) position))))))

(define (build::find_first_f0 f0s)
  (cond
   ((null f0s) 
    110  ;; last resort
    )
   ((> (cadr (car f0s)) 0)
    (cadr (car f0s)))
   (t
    (build::find_first_f0 (cdr f0s)))))

(define (build::load_f0_points fileid)
  "(build::load_f0_points fileid)
Extract F0 as ascii times and values from the F0 file and load
it as a simple assoc list."
  (let ((f0asciifile (make_tmp_filename))
	f0fd point points
	(time 0))
    (system
     (format nil "$EST%s/bin/ch_track -otype ascii -o %s f0/%s.f0"
	     "DIR"  ;; to stop that var name being mapped.
	     f0asciifile 
	     fileid))
    (set! f0fd (fopen f0asciifile "r"))
    (while (not (equal? (set! point (readfp f0fd)) (eof-val)))
      (set! points 
	    (cons
	     (list time point) points))
      (set! time (+ 0.005 time))
      ;; skip the second field.
      (readfp f0fd))
    (fclose f0fd)
    (delete-file f0asciifile)
    (reverse points)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Code to try to find bad labelling by looking at duration distribution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  A simple sufficient statistics class
(define (suffstats.new)
  (list
   0    ;; n
   0    ;; sum
   0    ;; sumx
   ))

(define (suffstats.set_n x n)
  (set-car! x n))
(define (suffstats.set_sum x sum)
  (set-car! (cdr x) sum))
(define (suffstats.set_sumx x sumx)
  (set-car! (cdr (cdr x)) sumx))
(define (suffstats.n x)
  (car x))
(define (suffstats.sum x)
  (car (cdr x)))
(define (suffstats.sumx x)
  (car (cdr (cdr x))))
(define (suffstats.reset x)
  (suffstats.set_n x 0)
  (suffstats.set_sum x 0)
  (suffstats.set_sumx x 0))
(define (suffstats.add x d)
  (suffstats.set_n x (+ (suffstats.n x) 1))
  (suffstats.set_sum x (+ (suffstats.sum x) d))
  (suffstats.set_sumx x (+ (suffstats.sumx x) (* d d)))
)
(define (suffstats.add_count x d c)
  (suffstats.set_n x (+ (suffstats.n x) c))
  (suffstats.set_sum x (+ (suffstats.sum x) (* c d)))
  (suffstats.set_sumx x (+ (suffstats.sumx x) (* c (* d d))))
)

(define (suffstats.mean x)
  (/ (suffstats.sum x) (suffstats.n x)))
(define (suffstats.variance x)
  (/ (- (* (suffstats.n x) (suffstats.sumx x))
        (* (suffstats.sum x) (suffstats.sum x)))
     (* (suffstats.n x) (- (suffstats.n x) 1))))
(define (suffstats.stddev x)
  (sqrt (suffstats.variance x)))

(define (cummulate_stats stats phone duration)
  (let ((pstat (car (cdr (assoc_string phone stats))))
	(newstats stats))
    (if (null pstat)
	(begin
	  (set! pstat (suffstats.new))
	  (set! newstats (cons (list phone pstat) stats))))
    (suffstats.add pstat duration)
    newstats))

(define (collect_dur_stats utts)
  (let ((stats nil))
    (mapcar
     (lambda (u)
       (mapcar 
	(lambda (s)
	  (set! stats (cummulate_stats
		       stats
		       (item.name s)
		       (item.feat s "segment_duration"))))
	(utt.relation.items u 'Segment)))
     utts)
    stats))

(define (score_utts utts durstats ofile)
  (let ((ofd (fopen ofile "w")))
    (mapcar
     (lambda (u)
       (let ((score 0) (tot 0))
	 (format ofd "%s " (utt.feat u "fileid"))
	 (mapcar 
	  (lambda (s)
	    (let ((stats (car (cdr (assoc_string (item.name s) durstats))))
		  (dur (item.feat s "segment_duration"))
		  (zscore))
	      (set! tot (+ 1 tot))
	      (set! zscore (/ (- dur (suffstats.mean stats))
			      (suffstats.stddev stats)))
	      (if (< zscore 0)
		  (set! zscore (* -1 zscore)))
	      (if (or (< dur 0.011)
		      (> zscore 3))
		  (set! score (+ 1 score)))))
	  (utt.relation.items u 'Segment))
	 (format ofd "%0.4f %d %d\n"
		 (/ score tot)
		 score
		 tot)))
     utts)))

(define (make_simple_utt fileid)
  (let ((utt (Utterance Text "")))
    (utt.relation.load utt 'Segment
		       (format nil "lab/%s.lab" fileid))
    (utt.set_feat utt "fileid" fileid)
    utt))

(define (find_outlier_utts file ofile)
  (voice_kal_diphone)
  (let ((p (load file t))
	utts dur_states)
    (set! utts (mapcar (lambda (l) (make_simple_utt (car l))) p))
    (set! dur_stats (collect_dur_stats utts))
    (score_utts utts dur_stats ofile)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Display selected units linked back to the units they came from
;;;
;;;

(define (clunits::display_selected utt)

  (clunits::display utt)
  (system (format nil "rm -rf scratch/cl\n"))
  (system (format nil "mkdir scratch/cl\n"))
  (system (format nil "(cd scratch/cl && mkdir wav lab emu emulab_hlb)"))
  (set! unum 0)
  (mapcar
   (lambda (unit)
     (set! unit_utt_name 
           (format nil "%03d_%s_%1.3f_%s"
                   unum
                   (item.name unit)
                   (item.feat unit "end")
                   (item.feat unit "fileid")))
     (set! unum (+ 1 unum))

     (system
      (format nil "ln lab/%s.lab scratch/cl/lab/%s.lab\n"
              (item.feat unit "fileid") unit_utt_name))
     (system
      (format nil "ln wav/%s.wav scratch/cl/wav/%s.wav\n"
              (item.feat unit "fileid") unit_utt_name))
     )
   (utt.relation.items utt 'Unit))

  (system (format nil "(cd scratch/cl && emulabel ../../etc/emu_lab &)\n"))
  t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Unit Selection measures
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (usm id text)
  (set! utts (SynthText text))
  (set! utta (utt.load nil (format nil "festival/utts/%s.utt" id )))
;  (set! utta (Wave_Synth utta))
;  (mapcar
;   (lambda (x y)
;     (if (not (string-equal (item.name x) (item.name y)))
;         (format t "%s %s %s %f\n" id (item.name x) (item.name y)
;                 (item.feat y "end"))))
;   (utt.relation.items utts 'Segment)
;   (utt.relation.items utta 'Segment))

  (apply
   +
   (mapcar
    (lambda (u)
      (if (and (string-equal id (item.feat u "fileid"))
               (or (null (item.prev u))
                   (and (string-equal id (item.feat u "p.fileid"))
                        (equal? (item.feat u "unit_start")
                                (item.feat u "p.unit_end")))))
          1
          0
      ))
    (utt.relation.items utts 'Unit)))
)

(define (usm_file ttd odir)
  (let ((usm_count 0) (usm_total 0))

  (mapcar
   (lambda (x)
     (set! c (usm (car x) (cadr x)))
     (set! us (length (utt.relation.items utts 'Unit)))
     (format t "USM %s %2.2f %2.2f %2.2f\n" (car x) c 
             us
             (* 100.0 (/ c us)))
     (utt.save.wave utts (format nil "%s/%s.wav" odir (car x)))
     (set! usm_count (+ c usm_count))
     (set! usm_total (+ us usm_total))
     )
   (load ttd t))

  (format t "USM_TOTAL %2.2f %2.2f %2.2f\n"
          usm_count
          usm_total
          (* 100.0 (/ usm_count usm_total)))
  )
)

(provide 'build_clunits)

