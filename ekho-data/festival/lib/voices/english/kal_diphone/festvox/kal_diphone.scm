;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;-*-mode:scheme-*-
;;;                                                                       ;;
;;;                    Alan W Black and Kevin Lenzo                       ;;
;;;                         Copyright (c) 1998                            ;;
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
;;;  THE AUTHORS OF THIS WORK DISCLAIM ALL WARRANTIES WITH REGARD TO      ;;
;;;  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY   ;;
;;;  AND FITNESS, IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY         ;;
;;;  SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES            ;;
;;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ;;
;;;  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ;;
;;;  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ;;
;;;  THIS SOFTWARE.                                                       ;;
;;;                                                                       ;;
;;;  This file is part "Building Voices in the Festival Speech            ;;
;;;  Synthesis System" by Alan W Black and Kevin Lenzo written at         ;;
;;;  Robotics Institute, Carnegie Mellon University, fall 98              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  A US English diphone voice based on KAL
;;;
;;;  Much of the front end is based on KED
;;;

(defvar kal_diphone_dir (cdr (assoc 'kal_diphone voice-locations))
  "kal_diphone_dir
  The default directory for the kal diphone database.")
(set! load-path (cons (path-append kal_diphone_dir "festvox/") load-path))

(require 'radio_phones)
(require_module 'UniSyn)

;; set this to lpc or psola
(defvar kal_sigpr 'lpc)
;; Rset this to ungroup for ungrouped version
(defvar kal_groupungroup 'group)

(if (probe_file (path-append kal_diphone_dir "group/kallpc16k.group"))
    (defvar kal_index_file 
      (path-append kal_diphone_dir "group/kallpc16k.group"))
    (defvar kal_index_file 
      (path-append kal_diphone_dir "group/kallpc8k.group")))

(set! kal_psola_sep 
      (list
       '(name "kal_psola_sep")
       (list 'index_file (path-append kal_diphone_dir "dic/diphdic.est"))
       '(grouped "false")
       (list 'coef_dir (path-append kal_diphone_dir "pm"))
       (list 'sig_dir (path-append kal_diphone_dir "wav"))
       '(coef_ext ".pm")
       '(sig_ext ".wav")
;       '(alternates_left ((er ax)))
;       '(alternates_right (($p p) ($k k) ($g g) ($d d) ($b b) ($t t)
;				  (y ih) (ax ah) (ll l)))
       '(alternates_right ((er ax)))
       '(default_diphone "ax-ax")))

(set! kal_lpc_sep 
      (list
       '(name "kal_lpc_sep")
       (list 'index_file (path-append kal_diphone_dir "dic/diphdic.est"))
       '(grouped "false")
       (list 'coef_dir (path-append kal_diphone_dir "lpc"))
       (list 'sig_dir  (path-append kal_diphone_dir "lpc"))
       '(coef_ext ".lpc")
       '(sig_ext ".res")
       '(alternates_right ((er ax)))
       '(default_diphone "ax-ax")))

(set! kal_lpc_group 
      (list
       '(name "kal_lpc_group")
       (list 'index_file kal_index_file)
       '(grouped "true")
       '(alternates_right ((er ax)))
       '(default_diphone "ax-ax")))

(defvar kal_di_16k nil)
(defvar kal_di_8k nil)

(cond
 (kal_di_16k
  (require 'kal_di)
  (setup_kal_diphone_lpc_16k_grouped))
 (kal_di_8k
  (require 'kal_di)
  (setup_kal_diphone_lpc_8k_grouped))
 ((and (eq kal_sigpr 'psola)
       (eq kal_groupungroup 'group))
  (set! kal_db_name (us_diphone_init kal_psola_group)))
 ((and (eq kal_sigpr 'psola)
       (eq kal_groupungroup 'ungroup))
  (set! kal_db_name (us_diphone_init kal_psola_sep)))
 ((and (eq kal_sigpr 'lpc)
       (eq kal_groupungroup 'group))
  (set! kal_db_name (us_diphone_init kal_lpc_group)))
 ((and (eq kal_sigpr 'lpc)
       (eq kal_groupungroup 'ungroup))
  (set! kal_db_name (us_diphone_init kal_lpc_sep))))

;;;;
;;;;  Our general diphone scheme allows identification of consonant
;;;   clusters etc the follow rules should work for American English
;;;;
(define (kal_diphone_const_clusters utt)
"(kal_diphone_const_clusters UTT)
Identify consonant clusters, dark ls etc in the segment stream
ready for diphone resynthesis.  This may be called as a post lexical
rule through poslex_rule_hooks."
  (mapcar
   (lambda (s) (kal_diphone_fix_phone_name utt s))
   (utt.relation.items utt 'Segment))
  utt)

(define (kal_diphone_fix_phone_name utt seg)
"(kal_diphone_fix_phone_name UTT SEG)
Add the feature diphone_phone_name to given segment with the appropriate
name for constructing a diphone.  Basically adds _ if either side is part
of the same consonant cluster, adds $ either side if in different
syllable for preceding/succeeding vowel syllable."
  (let ((name (item.name seg)))
    (cond
     ((string-equal name "pau") t)
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
      (if (and (string-equal name 'hh)
	       (string-equal (item.feat seg "n.name") 'y)
	       (item.relation.next seg "SylStructure"))
	  (item.set_feat seg "us_diphone_left" (format nil "%s_" name)))
      (if (and (string-equal name 'y)
	       (string-equal (item.feat seg "p.name") 'hh)
	       (item.relation.next seg "SylStructure"))
	  (item.set_feat seg "us_diphone_right" (format nil "_%s" name)))
      (if (and (member_string name '(p t k b d g))
	       (member_string (item.feat seg "n.name") '(r w y l))
	       (item.relation.next seg "SylStructure"))
	  (item.set_feat seg "us_diphone_left" (format nil "%s_" name)))
      )
     ((string-equal "ah" (item.name seg))
      (item.set_feat seg "us_diphone" "aa"))

   )))

;;;  Set up the CMU lexicon
(setup_cmu_lex)

(define (voice_kal_diphone)
"(voice_kal_diphone)
 Set up the current voice to be male  American English (Kevin) using
 the standard diphone code."
  ;; Phone set
  (voice_reset)
  (Parameter.set 'Language 'americanenglish)
  (require 'radio_phones)
  (Parameter.set 'PhoneSet 'radio)
  (PhoneSet.select 'radio)
  ;; Tokenization rules
  (set! token_to_words english_token_to_words)
  ;; POS tagger
  (require 'pos)
  (set! pos_lex_name "english_poslex")
  (set! pos_ngram_name 'english_pos_ngram)
  (set! pos_supported t)
  (set! guess_pos english_guess_pos)   ;; need this for accents
  ;; Lexicon selection
  (lex.select "cmu")
  (set! postlex_rules_hooks (list postlex_apos_s_check))
  ;; Phrase prediction
  (require 'phrase)
  (Parameter.set 'Phrase_Method 'prob_models)
  (set! phr_break_params english_phr_break_params)
  ;; Accent and tone prediction
  (require 'tobi)
  (set! int_tone_cart_tree f2b_int_tone_cart_tree)
  (set! int_accent_cart_tree f2b_int_accent_cart_tree)

  (set! postlex_vowel_reduce_cart_tree 
	postlex_vowel_reduce_cart_data)
  ;; F0 prediction
  (require 'f2bf0lr)
  (set! f0_lr_start f2b_f0_lr_start)
  (set! f0_lr_mid f2b_f0_lr_mid)
  (set! f0_lr_end f2b_f0_lr_end)
  (Parameter.set 'Int_Method Intonation_Tree)
  (set! int_lr_params
	'((target_f0_mean 105) (target_f0_std 14)
	  (model_f0_mean 170) (model_f0_std 34)))
  (Parameter.set 'Int_Target_Method Int_Targets_LR)
  ;; Duration prediction
  (require 'kaldurtreeZ)
  (set! duration_cart_tree kal_duration_cart_tree)
  (set! duration_ph_info kal_durs)
  (Parameter.set 'Duration_Method Duration_Tree_ZScores)
  (Parameter.set 'Duration_Stretch 1.1)
  ;; Waveform synthesizer: kal diphones
  ;; This assigned the diphone names from their context (_ $ etc)
  (cond
   ((or kal_di_16k kal_di_8k)
    ;; Waveform synthesizer: kal diphones
    (Parameter.set 'Synth_Method Diphone_Synthesize)
    ;; This assigned the diphone names from their context (_ $ etc)
    (set! diphone_module_hooks (list kal_di_const_clusters ))
    (Diphone.select 'kal))
   (t
    (set! UniSyn_module_hooks (list kal_diphone_const_clusters ))
    (set! us_abs_offset 0.0)
    (set! window_factor 1.0)
    (set! us_rel_offset 0.0)
    (set! us_gain 0.9)

    (Parameter.set 'Synth_Method 'UniSyn)
    (Parameter.set 'us_sigpr kal_sigpr)
    (us_db_select kal_db_name)))

  (set! after_synth_hooks 
	(lambda (utt) 
	  (utt.wave.rescale utt 2.6)))

  (set! current-voice 'kal_diphone)
)

(proclaim_voice
 'kal_diphone
 '((language english)
   (gender male)
   (dialect american)
   (description
    "This voice provides an American English male voice using a
     residual excited LPC diphone synthesis method.  It uses 
     the CMU Lexicon pronunciations.  Prosodic phrasing is provided 
     by a statistically trained model using part of speech and local 
     distribution of breaks.  Intonation is provided by a CART tree 
     predicting ToBI accents and an F0 contour generated from a model 
     trained from natural speech.  The duration model is also trained 
     from data using a CART tree.")))

(provide 'kal_diphone)

