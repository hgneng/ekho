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
;;;  Definition of various lexicons
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  If there exists a sudirectory of the lib-path called dicts then that 
;;;  is used as the lexicon directory by default.  If it doesn't exist 
;;;  we set lexdir to the directory in CSTR where our lexicons are.  
;;;  In non-CSTR installations where lexicons are not in lib/dicts, 
;;;  you should set lexdir in sitevars.scm

(defvar lexdir 
  (if (probe_file (path-append libdir "dicts"))
      (path-append libdir "dicts/")
      ;; else we'll guess we're in the CSTR filespace
      (path-as-directory "/projects/festival/lib/dicts/"))
  "lexdir
  The directory where the lexicon(s) are, by default.")

(require 'pos)        ;; for part of speech mapping 

(define (setup_cstr_lex)
"(setup_cstr_lexicon)
Define and setup the CSTR lexicon.  The CSTR lexicon consists
of about 25,000 entries in the mrpa phone set.  A large number of
specific local entries are also added to the addenda."
  (if (not (member_string "mrpa" (lex.list)))
      (begin
	(lex.create "mrpa")
	(lex.set.compile.file (path-append lexdir "cstrlex.out"))
	(lex.set.phoneset "mrpa")
	(lex.set.lts.method 'lts_rules)
	(lex.set.lts.ruleset 'nrl)
	(lex.set.pos.map english_pos_map_wp39_to_wp20)
	(mrpa_addenda)
	(lex.add.entry
	 '("previous" nil (((p r ii) 1) ((v ii) 0) ((@ s) 0))))
	(lex.add.entry
	 '("audio" () (((oo d) 1) ((ii) 0) ((ou) 0))))
	(lex.add.entry
	 '("modules" () (((m o d) 1) ((uu l s) 0))))
	)))

(define (setup_oald_lex)
"(setup_oald_lexicon)
Define and setup the CUVOALD lexicon.  This is derived from the
Computer Users Version of the Oxford Advanced Learners' Dictionary
of Current English.  This version includes a trained set of letter
to sound rules which have also been used to reduce the actual lexicon
size by over half, for those entries that the lts model gets exactly
the same."
  (if (not (member_string "oald" (lex.list)))
      (load (path-append lexdir "oald/oaldlex.scm"))))

(define (setup_cmu_lex)
  "(setup_cmu_lex)
Lexicon derived from the CMU lexicon (cmudict-0.4), around 100,000 entries,
in the radio phoneset (sort of darpa-like).  Includes letter to sound
rule model trained from this data, and uses the lexical stress predictor
from OALD."
  (if (not (member_string "cmu" (lex.list)))
      (load (path-append lexdir "cmu/cmulex.scm"))))

(define (setup_cmumt_lex)
  "(setup_cmumt_lex)
Lexicon derived from the CMU lexicon (cmudict-0.4), around 100,000 entries,
in the radio phoneset (sort of darpa-like).  Includes letter to sound
rule model trained from this data, and uses the lexical stress predictor
from OALD."
  (if (not (member_string "cmumt" (lex.list)))
      (load (path-append lexdir "cmu_mt/cmumtlex.scm"))))

(define (setup_cmu6_lex)
  "(setup_cmu6_lex)
Lexicon derived from the CMU lexicon (cmudict-0.6), around 100,000 entries,
in the radio phoneset (sort of darpa-like).  Includes letter to sound
rule model trained from this data, the format of this lexicon is suitable
for the UniSyn metrical phonology modules.  That is the entries are
not syllabified,"
  (if (not (member_string "cmu6" (lex.list)))
      (load (path-append lexdir "cmu6/cmu6lex.scm"))))

(define (setup_moby_lex)
"(setup_moby_lexicon)
Define and setup the MOBY lexicon.  This is derived from the public
domain version of the Moby (TM) Pronunciator II lexicon.  It can be
converted automatically to British English mrpa phoneset which of
course is sub-optimal.  It contains around 120,000 entries and has part
of speech information for homographs."
  (if (not (member_string "moby" (lex.list)))
      (begin
	(lex.create "moby")
	;  (lex.set.compile.file (path-append lexdir "mobylex.out"))
	(lex.set.compile.file "/home/awb/src/mobypron/mobylex.out")
	(lex.set.phoneset "mrpa")
	(lex.set.lts.method 'lts_rules)
	(lex.set.lts.ruleset 'nrl)
	(lex.set.pos.map english_pos_map_wp39_to_wp20)
	(lex.add.entry 
	 '("a" dt (((@) 0))))
	(lex.add.entry 
	 '("the" dt (((dh @) 0))))
	(lex.add.entry
	 '("taylor" n (((t ei) 1) ((l @) 0))))
	(lex.add.entry
	 '("who" prp ((( h uu ) 0))))
	(mrpa_addenda))))

(define (setup_beep_lex)
  "(setup_beep_lex)
Lexicon derived from the British English Example Pronunciation dictionary
(BEEP) from Tony Robinson  ajr@eng.cam.ac.uk.  Around 160,000 entries."
  (if (not (member_string "beep" (lex.list)))
      (begin
	(lex.create "beep")
	(lex.set.compile.file (path-append lexdir "beep_lex.out"))
	(lex.set.phoneset "mrpa")
	(lex.set.lts.method 'lts_rules)
	(lex.set.lts.ruleset 'nrl)
	(lex.set.pos.map english_pos_map_wp39_to_wp20)
	(lex.add.entry
	 '("taylor" nil (((t ei) 1) ((l @) 0))))
	(mrpa_addenda))))

;;; The nrl letter to sound rules produce mrpa phone set so we need
;;; to do some fancy things to make them work for American English
(define (f2b_lts word features)
"(f2b_lts WORD FEATURES)
Letter to sound rule system for f2b (American English), uses the NRL
LTS ruleset and maps the result to the radio phone set."
 '("unknown" nil (((ah n) 0) ((n ow n) 1)))
)

;;; A CART tree for predicting lexical stress for strings of phones
;;; generated by the LTS models.  This was actually trained from
;;; OALD as that's the only lexicon with stress and part of speech information
;;; It trained in a phoneset independent way and may be used be either
;;; OALD or CMU models (and probably MOBY and OGI lex too).
;;; On held out data it gets 
;;;           07390  378 7768      [7390/7768]      95.134
;;;           1 512 8207 8719      [8207/8719]      94.128
;;;            7902 8585 
;;; total 16487 correct 15597.000 94.602%
;;;  
(set! english_stress_tree
'((sylpos < 1.7)
 ((1))
 ((ph_vlng is a)
  ((0))
  ((ph_vheight is 1)
   ((num2end < 1.5)
    ((ph_vfront is 1)
     ((ph_vlng is s) ((0)) ((pos is v) ((1)) ((0))))
     ((pos is n) ((0)) ((sylpos < 2.2) ((1)) ((0)))))
    ((ph_vlng is l)
     ((1))
     ((ph_vfront is 1)
      ((num2end < 2.4)
       ((0))
       ((pos is a)
        ((num2end < 3.3) ((sylpos < 2.3) ((1)) ((0))) ((0)))
        ((sylpos < 3.2)
         ((num2end < 3.3) ((0)) ((pos is v) ((1)) ((0))))
         ((0)))))
      ((0)))))
   ((num2end < 1.5)
    ((pos is n)
     ((0))
     ((sylpos < 2.4)
      ((pos is v)
       ((1))
       ((ph_vlng is d)
        ((ph_vheight is 2) ((ph_vfront is 1) ((1)) ((0))) ((0)))
        ((1))))
      ((ph_vlng is d)
       ((sylpos < 3.3)
        ((pos is v)
         ((ph_vheight is 2) ((ph_vfront is 1) ((0)) ((1))) ((0)))
         ((0)))
        ((0)))
       ((ph_vheight is 2)
        ((1))
        ((ph_vrnd is +) ((1)) ((ph_vlng is l) ((0)) ((1))))))))
    ((ph_vlng is d)
     ((pos is v)
      ((sylpos < 2.4) ((1)) ((0)))
      ((ph_vfront is 2)
       ((pos is n)
        ((num2end < 2.4)
         ((ph_vrnd is +)
          ((0))
          ((sylpos < 2.2) ((1)) ((ph_vheight is 2) ((1)) ((0)))))
         ((sylpos < 2.4) ((ph_vheight is 2) ((0)) ((1))) ((0))))
        ((1)))
       ((ph_vheight is 2) ((1)) ((ph_vfront is 1) ((0)) ((1))))))
     ((pos is n)
      ((num2end < 2.4)
       ((ph_vfront is 3)
        ((sylpos < 2.3) ((1)) ((ph_vlng is l) ((1)) ((0))))
        ((1)))
       ((1)))
      ((1)))))))))

(define (lex_user_unknown_word word feats)
  "(lex_user_unknown_word WORD FEATS)
Function called by lexicon when 'function type letter to sound rules
is defined.  It is the user's responsibility to defined this function
themselves when they want to deal with unknown words themselves."
  (error "lex_user_unknown_word: has not been defined by user"))

(define (Word utt)
"(Word utt)                                
Construct (synthesis specific) syllable/segments from Word relation
using current lexicon and specific module."
  (let ((rval (apply_method 'Word_Method utt)))
    (cond
     (rval rval) ;; new style
     (t
      (Classic_Word utt)))))

(define (find_oovs vocab oovs)
  (let ((fd (fopen vocab "r"))
        (ofd (fopen oovs "w"))
        (e 0)
        (oov 0)
        (entry))

    (while (not (equal? (set! entry (readfp fd)) (eof-val)))
       (set! e (+ 1 e))
       (if (not (lex.lookup_all entry))
           (begin
             (set! oov (+ 1 oov))
             (format ofd "%l\n" (lex.lookup entry nil))))
       )
    (format t ";; %d words %d oov %2.2f oov_rate\n"
            e oov (/ (* oov 100.0) e))
    )
)


(provide 'lexicons)

