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
;;;  Set up rab_diphones using the standard UniSyn diphone synthesizer
;;;
;;;  Roger diphones: male RP English collected October 1996
;;;

(defvar rab_diphone_dir (cdr (assoc 'rab_diphone voice-locations))
  "rab_diphone_dir
  The default directory for the rab diphone database.")

(require 'mrpa_phones)
(require 'pos)
(require 'phrase)
(require 'tobi)
(require 'f2bf0lr)
(require 'mrpa_durs)
(require 'gswdurtreeZ)
(require_module 'UniSyn)

(setup_oald_lex)

;; set this to lpc or psola
(defvar rab_sigpr 'lpc)
;; Rset this to ungroup for ungrouped version
(defvar rab_groupungroup 'group)

(if (probe_file (path-append rab_diphone_dir "group/rablpc16k.group"))
    (defvar rab_index_file 
      (path-append rab_diphone_dir "group/rablpc16k.group"))
    (defvar rab_index_file 
      (path-append rab_diphone_dir "group/rablpc8k.group")))

(set! rab_psola_sep 
      (list
       '(name "rab_psola_sep")
       (list 'index_file (path-append rab_diphone_dir "dic/diphdic_full.est"))
       '(grouped "false")
       (list 'coef_dir (path-append rab_diphone_dir "pm"))
       (list 'sig_dir (path-append rab_diphone_dir "wav"))
       '(coef_ext ".pm")
       '(sig_ext ".wav")))

(set! rab_lpc_sep 
      (list
       '(name "rab_lpc_sep")
       (list 'index_file (path-append rab_diphone_dir "dic/diphdic_full.est"))
       '(grouped "false")
       (list 'coef_dir (path-append rab_diphone_dir "lpc"))
       (list 'sig_dir  (path-append rab_diphone_dir "lpc"))
       '(coef_ext ".lpc")
       '(sig_ext ".res")))

(set! rab_psola_group 
      (list
       '(name "rab_psola_group")
       (list 'index_file 
	     (path-append rab_diphone_dir "group/rab.group"))
       '(grouped "true")))

(set! rab_lpc_group 
      (list
       '(name "rab_lpc_group")
       (list 'index_file rab_index_file)
       '(alternates_left ((i ii) (ll l) (u uu) (i@ ii) (uh @) (a aa)
				 (u@ uu) (w @) (o oo) (e@ ei) (e ei)
				 (r @)))
       '(alternates_right ((i ii) (ll l) (u uu) (i@ ii) 
				  (y i) (uh @) (r @) (w @)))
       '(default_diphone @-@@)
       '(grouped "true")))

;;; Setup the desried DB 
(cond
 ((and (eq rab_sigpr 'psola)
       (eq rab_groupungroup 'group))
  (set! rab_db_name (us_diphone_init rab_psola_group)))
 ((and (eq rab_sigpr 'psola)
       (eq rab_groupungroup 'ungroup))
  (set! rab_db_name (us_diphone_init rab_psola_sep)))
 ((and (eq rab_sigpr 'lpc)
       (eq rab_groupungroup 'group))
  (set! rab_db_name (us_diphone_init rab_lpc_group)))
 ((and (eq rab_sigpr 'lpc)
       (eq rab_groupungroup 'ungroup))
  (set! rab_db_name (us_diphone_init rab_lpc_sep))))

(define (rab_postlex_syllabics utt)
"(rab_postlex_syllabics utt)
Because the lexicon is somewhat random in its used of syllable l n and
m this is designed to post process the output inserting schwa before
them.  Ideally the lexicon should be fixed."
  (mapcar
   (lambda (s)
     (if (and (member_string (item.name s) '("l" "n" "m"))
	      (string-equal "coda" (item.feat s "seg_onsetcoda"))
	      (not (member_string (item.feat s "p.name") '(l r)))
	      (string-equal "-" (item.feat s "p.ph_vc")))
	 (item.relation.insert 
	  s 'SylStructure
	  (item.insert s (list "@") 'before)
	  'before)))
   (utt.relation.items utt 'Segment)))

(define (rab_diphone_const_clusters utt)
"(rab_diphone_const_clusters UTT)
Identify consonant clusters, dark ls etc in the segment item
ready for diphone resynthesis.  This may be called as a post lexical
rule through poslex_rule_hooks."
  (mapcar
   (lambda (s) (rab_diphone_fix_phone_name utt s))
   (utt.relation.items utt 'Segment))
  utt)

(define (rab_diphone_fix_phone_name utt seg)
"(rab_diphone_fix_phone_name UTT SEG)
Add the feature diphone_phone_name to given segment with the appropriate
name for constructing a diphone.  Basically adds _ if either side is part
of the same consonant cluster, adds $ either side if in different
syllable for preceding/succeeding vowel syllable, and converts l to ll
in coda part of syllables."
  (let ((name (item.name seg)))
    (cond
     ((string-equal name "#") t)
     ((string-equal "-" (item.feat seg 'ph_vc))
      (if (and (member_string name '(r w y l))
	       (member_string (item.feat seg "p.name") '(p t k b d g))
	       (item.relation.prev seg "SylStructure"))
	  (item.set_feat seg "us_diphone_right" (format nil "_%s" name)))
      (if (and (member_string name '(w y l m n p t k))
	       (string-equal (item.feat seg "p.name") 's)
	       (item.relation.prev seg "SylStructure"))
	  (item.set_feat seg "us_diphone_right" (format nil "_%s" name)))
      (if (and (string-equal name 's)
	       (member_string (item.feat seg "n.name") '(w y l m n p t k))
	       (item.relation.next seg "SylStructure"))
	  (item.set_feat seg "us_diphone_left" (format nil "%s_" name)))
      (if (and (member_string name '(p t k b d g))
	       (member_string (item.feat seg "n.name") '(r w y l))
	       (item.relation.next seg "SylStructure"))
	  (item.set_feat seg "us_diphone_left" (format nil "%s_" name)))
      (if (and (member_string name '(p k b d g))
	       (string-equal "+" (item.feat seg 'p.ph_vc))
	       (not (member_string (item.feat seg "p.name") '(@ aa o)))
	       (not (item.relation.prev seg "SylStructure")))
	  (item.set_feat seg "us_diphone_right" (format nil "$%s" name)))
      (if (and (member_string name '(p t k b d g))
	       (string-equal "+" (item.feat seg 'n.ph_vc))
	       (not (member_string (item.feat seg "n.name") '(@ aa)))
	       (not (item.relation.next seg "SylStructure")))
	  (item.set_feat seg "us_diphone_left" (format nil "%s$" name)))
      (if (and (string-equal "l" name)
	       (string-equal "+" (item.feat seg "p.ph_vc"))
	       (not (string-equal "a" (item.feat seg "p.ph_vlng")))
	       (item.relation.prev seg 'SylStructure))
	  (item.set_feat seg "us_diphone_right" "ll"))
      (if (and (member_string name '(ch jh))
	       (string-equal "+" (item.feat seg 'p.ph_vc)))
	  (item.set_feat seg "us_diphone_right" "t"))
      )
      )))

(define (voice_rab_diphone)
"(voice_rab_diphone)
 Set up the current voice to be a British male RP (Roger) speaker using
 the rab diphone set."
  (voice_reset)
  (Parameter.set 'Language 'britishenglish)
  ;; Phone set
  (Parameter.set 'PhoneSet 'mrpa)
  (PhoneSet.select 'mrpa)
  ;; Tokenization rules
  (set! token_to_words english_token_to_words)
  ;; POS tagger
  (set! pos_lex_name "english_poslex")
  (set! pos_ngram_name 'english_pos_ngram)
  (set! pos_supported t)
  (set! guess_pos english_guess_pos)   ;; need this for accents
  ;; Lexicon selection
  (lex.select "oald")
  (set! postlex_rules_hooks (list postlex_apos_s_check
				  rab_postlex_syllabics))
  ;; Phrase prediction
  (Parameter.set 'Phrase_Method 'prob_models)
  (set! phr_break_params english_phr_break_params)
  ;; Accent and tone prediction
  (set! int_tone_cart_tree f2b_int_tone_cart_tree)
  (set! int_accent_cart_tree f2b_int_accent_cart_tree)
  ;; F0 prediction
  (set! f0_lr_start f2b_f0_lr_start)
  (set! f0_lr_mid f2b_f0_lr_mid)
  (set! f0_lr_end f2b_f0_lr_end)
  (Parameter.set 'Int_Method Intonation_Tree)
  (set! int_lr_params
	'((target_f0_mean 105) (target_f0_std 14)
	  (model_f0_mean 170) (model_f0_std 34)))
  (Parameter.set 'Int_Target_Method Int_Targets_LR)
  ;; Duration prediction -- use gsw durations
  (set! duration_cart_tree gsw_duration_cart_tree)
  (set! duration_ph_info gsw_durs)
  (Parameter.set 'Duration_Method Duration_Tree_ZScores)
  (Parameter.set 'Duration_Stretch 1.05)
  ;; Waveform synthesizer: Roger diphones
  ;; This assigned the diphone names from their context (_ $ etc)
  (set! UniSyn_module_hooks (list rab_diphone_const_clusters ))
  (set! us_abs_offset 0.0)
  (set! window_factor 1.0)
  (set! us_rel_offset 0.0)
  (set! us_gain 0.9)

  (Parameter.set 'Synth_Method 'UniSyn)
  (Parameter.set 'us_sigpr rab_sigpr)
  (us_db_select rab_db_name)

  (set! current-voice 'rab_diphone)
)

(proclaim_voice
 'rab_diphone
 '((language english)
   (gender male)
   (dialect british)
   (description
    "This voice provides a British RP English male voice using a
     residual excited LPC diphone synthesis method.  It uses a 
     modified Oxford Advanced Learners' Dictionary for pronunciations.
     Prosodic phrasing is provided by a statistically trained model
     using part of speech and local distribution of breaks.  Intonation
     is provided by a CART tree predicting ToBI accents and an F0 
     contour generated from a model trained from natural speech.  The
     duration model is also trained from data using a CART tree.")))

(provide 'rab_diphone)
