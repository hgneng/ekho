;;  ----------------------------------------------------------------  ;;
;;                 Nagoya Institute of Technology and                 ;;
;;                     Carnegie Mellon University                     ;;
;;                         Copyright (c) 2002                         ;;
;;                        All Rights Reserved.                        ;;
;;                                                                    ;;
;;  Permission is hereby granted, free of charge, to use and          ;;
;;  distribute this software and its documentation without            ;;
;;  restriction, including without limitation the rights to use,      ;;
;;  copy, modify, merge, publish, distribute, sublicense, and/or      ;;
;;  sell copies of this work, and to permit persons to whom this      ;;
;;  work is furnished to do so, subject to the following conditions:  ;;
;;                                                                    ;;
;;    1. The code must retain the above copyright notice, this list   ;;
;;       of conditions and the following disclaimer.                  ;;
;;                                                                    ;;
;;    2. Any modifications must be clearly marked as such.            ;;
;;                                                                    ;;
;;    3. Original authors' names are not deleted.                     ;;
;;                                                                    ;;
;;    4. The authors' names are not used to endorse or promote        ;;
;;       products derived from this software without specific prior   ;;
;;       written permission.                                          ;;
;;                                                                    ;;
;;  NAGOYA INSTITUTE OF TECHNOLOGY, CARNEGIE MELLON UNIVERSITY AND    ;;
;;  THE CONTRIBUTORS TO THIS WORK DISCLAIM ALL WARRANTIES WITH        ;;
;;  REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF      ;;
;;  MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL NAGOYA INSTITUTE   ;;
;;  OF TECHNOLOGY, CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS    ;;
;;  BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR   ;;
;;  ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR        ;;
;;  PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    ;;
;;  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR  ;;
;;  PERFORMANCE OF THIS SOFTWARE.                                     ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      Generic HTS support code and specific features                ;;
;;           http://hts.ics.nitech.ac.jp                              ;;
;;             Author :  Alan W Black <awb@cs.cmu.edu>                ;;
;;             Date   :  August 2002 (and April 2004)                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    ;;
;;  Still has language specific features in here, that will have to   ;;
;;  move out to the voices                                            ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar hts_synth_pre_hooks nil)
(defvar hts_synth_post_hooks nil)
(defvar hts_engine_params nil)

(defvar hts_duration_stretch 0)
(defvar hts_f0_mean 0)
(defvar hts_f0_std 1)
(defvar hts_fw_factor 0.42)
(defvar hts_total_length 0.0)
(defvar hts_uv_threshold 0.5)
(defvar hts_use_phone_align 0)

(defSynthType HTS
  (let ((featfile (make_tmp_filename))
	(mcepfile (make_tmp_filename))
	(f0file (make_tmp_filename))
	(wavfile (make_tmp_filename))
	(labfile (make_tmp_filename)))

    (apply_hooks hts_synth_pre_hooks utt)

    (set! hts_output_params
	  (list
	   (list "-labelfile" featfile)
	   (list "-om" mcepfile)
	   (list "-of" f0file)
	   (list "-or" wavfile)
		 (list "-od" labfile))
		)

    (hts_dump_feats utt hts_feats_list featfile)

    (HTS_Synthesize utt)

    (delete-file featfile)
    (delete-file mcepfile)
    (delete-file f0file)
    (delete-file wavfile)
    (delete-file labfile)

    (apply_hooks hts_synth_post_hooks utt)
    utt)
)

(define (hts_feats_output ofd s)
  "This  is bad as it makes decisions about what the feats are"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  SEGMENT

;  boundary
  (format ofd "%10.0f %10.0f " 
	  (* 10000000 (item.feat s "segment_start"))
	  (* 10000000 (item.feat s "segment_end")))

;  pp.name
  (format ofd "%s" (if (string-equal "0" (item.feat s "p.p.name"))
		       "x" (item.feat s "p.p.name")))
;  p.name
  (format ofd "^%s" (if (string-equal "0" (item.feat s "p.name"))
			"x" (item.feat s "p.name")))
;  c.name
  (format ofd "-%s" (if (string-equal "0" (item.feat s "name"))
			"x" (item.feat s "name")))
;  n.name
  (format ofd "+%s" (if (string-equal "0" (item.feat s "n.name"))
			"x" (item.feat s "n.name")))
;  nn.name
  (format ofd "=%s" (if (string-equal "0" (item.feat s "n.n.name"))
			"x" (item.feat s "n.n.name")))

;  position in syllable (segment)
  (format ofd "@")
  (format ofd "%s" (if (string-equal "pau" (item.feat s "name"))
		       "x" (+ 1 (item.feat s "pos_in_syl"))))
  (format ofd "_%s" (if (string-equal "pau" (item.feat s "name"))
			"x" (- (item.feat s "R:SylStructure.parent.R:Syllable.syl_numphones") 
			       (item.feat s "pos_in_syl"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  SYLLABLE

;; previous syllable

;  p.stress
  (format ofd "/A:%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      (item.feat s "p.R:SylStructure.parent.R:Syllable.stress")
	      (item.feat s "R:SylStructure.parent.R:Syllable.p.stress")))
;  p.accent
  (format ofd "_%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      (item.feat s "p.R:SylStructure.parent.R:Syllable.accented")
	      (item.feat s "R:SylStructure.parent.R:Syllable.p.accented")))
;  p.length
  (format ofd "_%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      (item.feat s "p.R:SylStructure.parent.R:Syllable.syl_numphones")
	      (item.feat s "R:SylStructure.parent.R:Syllable.p.syl_numphones")))
;; current syllable

;  c.stress
  (format ofd "/B:%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (item.feat s "R:SylStructure.parent.R:Syllable.stress")))
;  c.accent
  (format ofd "-%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (item.feat s "R:SylStructure.parent.R:Syllable.accented")))
;  c.length
  (format ofd "-%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (item.feat s "R:SylStructure.parent.R:Syllable.syl_numphones")))

;  position in word (syllable)
  (format ofd "@%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (+ 1 (item.feat s "R:SylStructure.parent.R:Syllable.pos_in_word"))))
  (format ofd "-%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (- 
	       (item.feat s "R:SylStructure.parent.parent.R:Word.word_numsyls")
	       (item.feat s "R:SylStructure.parent.R:Syllable.pos_in_word"))))

;  position in phrase (syllable)
    (format ofd "&%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (+ 1
		 (item.feat s "R:SylStructure.parent.R:Syllable.syl_in"))))
    (format ofd "-%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (+ 1
		 (item.feat s "R:SylStructure.parent.R:Syllable.syl_out"))))

;  position in phrase (stressed syllable)
    (format ofd "#%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (+ 1
		 (item.feat s "R:SylStructure.parent.R:Syllable.ssyl_in"))))
    (format ofd "-%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (+ 1
		 (item.feat s "R:SylStructure.parent.R:Syllable.ssyl_out"))))

;  position in phrase (accented syllable)
    (format ofd "$%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (+ 1
		 (item.feat s "R:SylStructure.parent.R:Syllable.asyl_in"))))
    (format ofd "-%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (+ 1
		 (item.feat s "R:SylStructure.parent.R:Syllable.asyl_out"))))

;  distance from stressed syllable
    (format ofd "!%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (item.feat s "R:SylStructure.parent.R:Syllable.lisp_distance_to_p_stress")))
    (format ofd "-%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (item.feat s "R:SylStructure.parent.R:Syllable.lisp_distance_to_n_stress")))

;  distance from accented syllable 
    (format ofd ";%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (item.feat s "R:SylStructure.parent.R:Syllable.lisp_distance_to_p_accent")))
    (format ofd "-%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (item.feat s "R:SylStructure.parent.R:Syllable.lisp_distance_to_n_accent")))

;  name of the vowel of current syllable
    (format ofd "|%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (item.feat s "R:SylStructure.parent.R:Syllable.syl_vowel")))

;; next syllable
  (format ofd "/C:%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      (item.feat s "n.R:SylStructure.parent.R:Syllable.stress")
	      (item.feat s "R:SylStructure.parent.R:Syllable.n.stress")))
;  n.accent
  (format ofd "+%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      (item.feat s "n.R:SylStructure.parent.R:Syllable.accented")
	      (item.feat s "R:SylStructure.parent.R:Syllable.n.accented")))
;  n.length
  (format ofd "+%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      (item.feat s "n.R:SylStructure.parent.R:Syllable.syl_numphones")
	      (item.feat s "R:SylStructure.parent.R:Syllable.n.syl_numphones"))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  WORD

;;;;;;;;;;;;;;;;;;
;; previous word

;  p.gpos
  (format ofd "/D:%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      (item.feat s "p.R:SylStructure.parent.parent.R:Word.gpos")
	      (item.feat s "R:SylStructure.parent.parent.R:Word.p.gpos")))
;  p.lenght (syllable)
  (format ofd "_%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      (item.feat s "p.R:SylStructure.parent.parent.R:Word.word_numsyls")
	      (item.feat s "R:SylStructure.parent.parent.R:Word.p.word_numsyls")))

;;;;;;;;;;;;;;;;;
;; current word

;  c.gpos
  (format ofd "/E:%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (item.feat s "R:SylStructure.parent.parent.R:Word.gpos")))
;  c.lenght (syllable)
  (format ofd "+%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (item.feat s "R:SylStructure.parent.parent.R:Word.word_numsyls")))

;  position in phrase (word)
  (format ofd "@%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (+ 1 (item.feat s "R:SylStructure.parent.parent.R:Word.pos_in_phrase"))))
  (format ofd "+%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (item.feat s "R:SylStructure.parent.parent.R:Word.words_out")))

;  position in phrase (content word)
  (format ofd "&%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (+ 1 (item.feat s "R:SylStructure.parent.parent.R:Word.content_words_in"))))
  (format ofd "+%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (item.feat s "R:SylStructure.parent.parent.R:Word.content_words_out")))

;  distance from content word in phrase
  (format ofd "#%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (item.feat s "R:SylStructure.parent.parent.R:Word.lisp_distance_to_p_content")))
  (format ofd "+%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (item.feat s "R:SylStructure.parent.parent.R:Word.lisp_distance_to_n_content")))

;;;;;;;;;;;;;;
;; next word

;  n.gpos
  (format ofd "/F:%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      (item.feat s "n.R:SylStructure.parent.parent.R:Word.gpos")
	      (item.feat s "R:SylStructure.parent.parent.R:Word.n.gpos")))
;  n.lenghte (syllable)
  (format ofd "_%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      (item.feat s "n.R:SylStructure.parent.parent.R:Word.word_numsyls")
	      (item.feat s "R:SylStructure.parent.parent.R:Word.n.word_numsyls")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  PHRASE

;;;;;;;;;;;;;;;;;;;;
;; previous phrase

;  length of previous phrase (syllable)
  (format ofd "/G:%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      (item.feat s "p.R:SylStructure.parent.parent.R:Phrase.parent.lisp_num_syls_in_phrase")
	      (item.feat s "R:SylStructure.parent.parent.R:Phrase.parent.p.lisp_num_syls_in_phrase")))

;  length of previous phrase (word)
  (format ofd "_%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      (item.feat s "p.R:SylStructure.parent.parent.R:Phrase.parent.lisp_num_words_in_phrase")
	      (item.feat s "R:SylStructure.parent.parent.R:Phrase.parent.p.lisp_num_words_in_phrase")))

;;;;;;;;;;;;;;;;;;;;
;; current phrase

;  length of current phrase (syllable)
  (format ofd "/H:%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (item.feat s "R:SylStructure.parent.parent.R:Phrase.parent.lisp_num_syls_in_phrase")))

;  length of current phrase (word)
  (format ofd "=%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      "x"
	      (item.feat s "R:SylStructure.parent.parent.R:Phrase.parent.lisp_num_words_in_phrase")))

;  position in major phrase (phrase)
  (format ofd "@%s" 
	  (+ 1 (item.feat s "R:SylStructure.parent.R:Syllable.sub_phrases")))
  (format ofd "=%s" 
	  (- 
	   (item.feat s "lisp_total_phrases")
	   (item.feat s "R:SylStructure.parent.R:Syllable.sub_phrases")))

;  type of tobi endtone of current phrase
  (format ofd "|%s" 
	  (item.feat s "R:SylStructure.parent.parent.R:Phrase.parent.daughtern.R:SylStructure.daughtern.tobi_endtone"))

;;;;;;;;;;;;;;;;;;;;
;; next phrase

;  length of next phrase (syllable)
  (format ofd "/I:%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      (item.feat s "n.R:SylStructure.parent.parent.R:Phrase.parent.lisp_num_syls_in_phrase")
	      (item.feat s "R:SylStructure.parent.parent.R:Phrase.parent.n.lisp_num_syls_in_phrase")))

;  length of next phrase (word)
  (format ofd "=%s" 
	  (if (string-equal "pau" (item.feat s "name"))
	      (item.feat s "n.R:SylStructure.parent.parent.R:Phrase.parent.lisp_num_words_in_phrase")
	      (item.feat s "R:SylStructure.parent.parent.R:Phrase.parent.n.lisp_num_words_in_phrase")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  UTTERANCE

;  length (syllable)
  (format ofd "/J:%s" (item.feat s "lisp_total_syls"))

;  length (word)
  (format ofd "+%s" (item.feat s "lisp_total_words"))

;  length (phrase)
  (format ofd "-%s" (item.feat s "lisp_total_phrases"))

  (format ofd "\n")

)

(define (hts_dump_feats utt feats ofile)
  (let ((ofd (fopen ofile "w")))
    (mapcar
     (lambda (s)
       (hts_feats_output ofd s))
     (utt.relation.items utt 'Segment))
    (fclose ofd)
    ))


;;
;;  Extra features
;;  From Segment items refer by 
;;
;;  R:SylStructure.parent.parent.R:Phrase.parent.lisp_num_syls_in_phrase
;;  R:SylStructure.parent.parent.R:Phrase.parent.lisp_num_words_in_phrase
;;  lisp_total_words
;;  lisp_total_syls
;;  lisp_total_phrases
;;
;;  The last three will act on any item

(define (distance_to_p_content i)
  (let ((c 0) (rc 0 ) (w (item.relation.prev i "Phrase")))
    (while w
      (set! c (+ 1 c))
      (if (string-equal "1" (item.feat w "contentp"))
      (begin
        (set! rc c)
        (set! w nil))
      (set! w (item.prev w)))
      )
    rc))

(define (distance_to_n_content i)
  (let ((c 0) (rc 0) (w (item.relation.next i "Phrase")))
    (while w
      (set! c (+ 1 c))
      (if (string-equal "1" (item.feat w "contentp"))
      (begin
        (set! rc c)
        (set! w nil))
      (set! w (item.next w)))
      )
    rc))

(define (distance_to_p_accent i)
  (let ((c 0) (rc 0 ) (w (item.relation.prev i "Syllable")))
    (while (and w (member_string (item.feat w "syl_break") '("0" "1")))
      (set! c (+ 1 c))
      (if (string-equal "1" (item.feat w "accented"))
      (begin
        (set! rc c)
        (set! w nil))
        (set! w (item.prev w)))
        )
        rc))

(define (distance_to_n_accent i)
  (let ((c 0) (rc 0 ) (w (item.relation.next i "Syllable")))
    (while (and w (member_string (item.feat w "p.syl_break") '("0" "1")))
      (set! c (+ 1 c))
      (if (string-equal "1" (item.feat w "accented"))
      (begin
        (set! rc c)
        (set! w nil))
        (set! w (item.next w)))
        )
        rc))

(define (distance_to_p_stress i)
  (let ((c 0) (rc 0 ) (w (item.relation.prev i "Syllable")))
    (while (and w (member_string (item.feat w "syl_break") '("0" "1")))
      (set! c (+ 1 c))
      (if (string-equal "1" (item.feat w "stress"))
      (begin
        (set! rc c)
        (set! w nil))
        (set! w (item.prev w)))
        )
        rc))

(define (distance_to_n_stress i)
  (let ((c 0) (rc 0 ) (w (item.relation.next i "Syllable")))
    (while (and w (member_string (item.feat w "p.syl_break") '("0" "1")))
      (set! c (+ 1 c))
      (if (string-equal "1" (item.feat w "stress"))
      (begin
        (set! rc c)
        (set! w nil))
        (set! w (item.next w)))
        )
        rc))

(define (num_syls_in_phrase i)
  (apply 
   +
   (mapcar
    (lambda (w)
      (length (item.relation.daughters w 'SylStructure)))
    (item.relation.daughters i 'Phrase))))

(define (num_words_in_phrase i)
  (length (item.relation.daughters i 'Phrase)))

(define (total_words w)
  (length
   (utt.relation.items (item.get_utt w) 'Word)))

(define (total_syls s)
  (length
   (utt.relation.items (item.get_utt s) 'Syllable)))

(define (total_phrases s)
  (length
   (utt.relation_tree (item.get_utt s) 'Phrase)))

(provide 'hts)
