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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;; Support Code for building prosody models                            ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar feat_float_types
  '(segment_duration 
    lisp_zscore_dur 
    R:SylStructure.parent.syl_onsetsize
    R:SylStructure.parent.syl_codasize
    R:SylStructure.parent.R:Syllable.n.syl_onsetsize
    R:SylStructure.parent.R:Syllable.p.syl_codasize
    R:SylStructure.parent.parent.word_numsyls
    pos_in_syl
    R:SylStructure.parent.pos_in_word
    R:SylStructure.parent.syl_in
    R:SylStructure.parent.syl_out
    R:SylStructure.parent.ssyl_in
    R:SylStructure.parent.ssyl_out
    R:SylStructure.parent.asyl_in
    R:SylStructure.parent.asyl_out
    R:SylStructure.parent.last_accent
    R:SylStructure.parent.next_accent
    R:SylStructure.parent.sub_phrases
    syl_startpitch
    syl_midpitch
    syl_endpitch
    syl_numphones
    pos_in_word
    syl_in
    syl_out
    ssyl_in
    ssyl_out
    sub_phrases
R:segstate.parent.R:SylStructure.parent.syl_onsetsize
R:segstate.parent.R:SylStructure.parent.syl_codasize
R:segstate.parent.R:SylStructure.parent.R:Syllable.n.syl_onsetsize
R:segstate.parent.R:SylStructure.parent.R:Syllable.p.syl_codasize
R:segstate.parent.R:SylStructure.parent.parent.word_numsyls
R:segstate.parent.R:SylStructure.parent.pos_in_word
R:segstate.parent.R:SylStructure.parent.syl_in
R:segstate.parent.R:SylStructure.parent.syl_out
R:segstate.parent.R:SylStructure.parent.ssyl_in
R:segstate.parent.R:SylStructure.parent.ssyl_out
R:segstate.parent.R:SylStructure.parent.asyl_in
R:segstate.parent.R:SylStructure.parent.asyl_out
R:segstate.parent.R:SylStructure.parent.last_accent
R:segstate.parent.R:SylStructure.parent.next_accent
R:segstate.parent.R:SylStructure.parent.sub_phrases
R:segstate.parent.R:SylStructure.parent.R:Syllable.pp.lisp_cg_break
R:segstate.parent.R:SylStructure.parent.R:Syllable.p.lisp_cg_break
R:segstate.parent.R:SylStructure.parent.lisp_cg_break
R:segstate.parent.R:SylStructure.parent.R:Syllable.n.lisp_cg_break
R:segstate.parent.R:SylStructure.parent.R:Syllable.nn.lisp_cg_break
    ))
(defvar feat_int_types
  '())


(define (build_dur_feats_desc)
  "(build_dur_feats_desc)
Replaces the huge list of numbers in the dur.desc file with
floats as appropriate."
  (build_fix_desc_file "festival/dur/etc/dur.desc"))

(define (build_f0_feats_desc)
  "(build_f0_feats_desc)
Replaces the huge list of numbers in the f0.desc file with
floats as appropriate."
  (build_fix_desc_file "festival/f0/etc/f0.desc"))

(define (build_fix_desc_file descfile)
  (let ((desc (car (load descfile t)))
	(ofd (fopen descfile "w")))
    (format ofd "(\n")
    (mapcar
     (lambda (fd)
       (if (not (cddr fd))
	   (set-cdr! fd (cons 'ignore)))
       (cond
	((member_string (car fd) feat_float_types)
	 (set-cdr! fd (cons 'float)))
	((member_string (car fd) feat_int_types)
	 (set-cdr! fd (cons 'float)))
	)
       (format ofd "%l\n" fd)
       t)
     desc)
    (format ofd ")\n")
    (fclose ofd)))

(define (build_dur_vec_feats_desc)
  "(build_dur_feats_desc)
Replaces the huge list of numbers in the dur.desc file with
floats as appropriate."
  (build_fix_desc_vec_file "festival/dur/etc/dur.desc"))

(define (build_fix_desc_vec_file descfile)
  (let ((desc (car (load descfile t)))
	(ofd (fopen descfile "w")))
    (format ofd "(\n")
    (format ofd "(durvec vector)\n")
    (mapcar
     (lambda (fd)
       (if (not (cddr fd))
	   (set-cdr! fd (cons 'ignore)))
       (cond
	((member_string (car fd) feat_float_types)
	 (set-cdr! fd (cons 'float)))
	((member_string (car fd) feat_int_types)
	 (set-cdr! fd (cons 'float)))
	)
       (format ofd "%l\n" fd)
       t)
     (cdr desc))
    (format ofd ")\n")
    (fclose ofd)))

(define (finalize_dur_model modelname treename)
  "(finalize_dur_model modelname treename)
Take the tree and means/dur and create a scheme file which can
be useds as a duration model."
  (let ((ofd (fopen (format nil "festvox/%s_dur.scm" modelname) "w"))
	(silence (car (cadr (car (PhoneSet.description '(silences)))))))
    (if (string-matches modelname "^(.*")
	(set! modelname (string-after modelname "(")))
    (if (string-matches modelname ".*)$")
	(set! modelname (string-before modelname ")")))
    (format ofd ";; Duration models autotrained by festvox\n")
    (format ofd ";; %s\n" treename)
    (format ofd "(set! %s::phone_durs '\n" modelname)
    (pprintf 
     (cons
      (list silence 0.200 0.100)
      (mapcar
       (lambda (x)
         (if (string-equal (car (cddr x)) "nan")
             (list (car x) (cadr x) 0.001)
             x)
         )
       (load "festival/dur/etc/durs.meanstd" t)))
     ofd)
    (format ofd ")\n")
    (format ofd "\n\n")
    ;; The tree wasn't trained with silence so we need to add that
    (format ofd "(set! %s::zdur_tree '\n" modelname)
    (format ofd "((name is %s)\n" silence)
    (format ofd " ((p.R:SylStructure.parent.parent.pbreak is BB)\n")
    (format ofd "  ((0.0 2.0))\n")
    (format ofd "  ((0.0 0.0)))\n")
    (pprintf
     (car (load (format nil "festival/dur/tree/%s" treename) t)) ofd)
    (format ofd ")\n")
    (format ofd ")\n")
    (format ofd "\n\n")
    (format ofd "(provide '%s)\n" (string-append modelname "_dur"))
    (format ofd "\n\n")
    (fclose ofd)
    )
)

(define (finalize_f0_model modelname treename)
  "(finalize_f0_model modelname treename)
Take the F0 trees and create a scheme file which can
be used as a F0 model."
  (let ((ofd (fopen (format nil "festvox/%s_f0.scm" modelname) "w")))
    (format ofd ";; F0 models autotrained by festvox\n")
    (format ofd ";; %s\n" treename)
    (format ofd "(set! %s::start_f0 '\n" modelname)
    (pprintf
     (car (load (format nil "festival/f0/tree/start.%s" treename) t)) ofd)
    (format ofd ")\n")
    (format ofd "\n\n")

    (format ofd "(set! %s::mid_f0 '\n" modelname)
    (pprintf
     (car (load (format nil "festival/f0/tree/mid.%s" treename) t)) ofd)
    (format ofd ")\n")
    (format ofd "\n\n")

    (format ofd "(set! %s::end_f0 '\n" modelname)
    (pprintf
     (car (load (format nil "festival/f0/tree/end.%s" treename) t)) ofd)
    (format ofd ")\n")
    (format ofd "\n\n")
    (format ofd "(provide '%s)\n" (string-append modelname "_f0"))
    (format ofd "\n\n")
    (fclose ofd)
    )
)

(provide 'build_prosody)