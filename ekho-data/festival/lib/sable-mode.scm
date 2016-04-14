;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
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
;;;                                                                       ;;
;;;  Festival (1.3.X) support for SABLE 0.2 the SGML/XML based mark up    ;;
;;;  language.                                                            ;;
;;;                                                                       ;;
;;;  This is XML version requiring Edinburgh's LTG's rxp XML parser as    ;;
;;;  distributed with Festival                                            ;;
;;;                                                                       ;;

(require_module 'rxp)

;;(set! auto-text-mode-alist
;;      (cons
;;       (cons "\\.sable$" 'sable)
;;       auto-text-mode-alist))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                                       ;;
 ;; Remember where to find these two XML entities.                        ;;
 ;;                                                                       ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(xml_register_id "-//SABLE//DTD SABLE speech mark up//EN"
		(path-append libdir "Sable.v0_2.dtd")
		)

(xml_register_id "-//SABLE//ENTITIES Added Latin 1 for SABLE//EN"
		 (path-append libdir  "sable-latin.ent")
		 )

;; (print (xml_registered_ids))

(defvar SABLE_RXDOUBLE "-?\\(\\([0-9]+\\.[0-9]*\\)\\|\\([0-9]+\\)\\|\\(\\.[0-9]+\\)\\)\\([eE][---+]?[0-9]+\\)?")

(defvar sable_pitch_base_map
  '((highest 1.2)
    (high 1.1)
    (medium 1.0)
    (default 1.0)
    (low 0.9)
    (lowest 0.8)))
(defvar sable_pitch_med_map
  '((highest 1.2)
    (high 1.1)
    (medium 1.0)
    (default 1.0)
    (low 0.9)
    (lowest 0.8)))
(defvar sable_pitch_range_map
  '((largest 1.2)
    (large 1.1)
    (medium 1.0)
    (default 1.0)
    (small 0.9)
    (smallest 0.8)))
(defvar sable_rate_speed_map
  '((fastest 1.5)
    (fast 1.2)
    (medium 1.0)
    (default 1.0)
    (slow 0.8)
    (slowest 0.6)))
(defvar sable_volume_level_map
  '((loudest 2.0) 
    (loud 1.5)
    (default 1.0)
    (medium 1.0)
    (quiet 0.5)))

(define (sable_init_globals)
  (set! utts nil)
  (set! sable_omitted_mode nil)
  (set! sable_word_features_stack nil)
  (set! sable_pitch_context nil)
  (set! sable_vol_context nil)
  (set! sable_vol_type 'no_change)
  (set! sable_vol_factor 1.0)
  (set! sable_current_language 'britishenglish)
  (set! sable_unsupported_language nil)
  (set! sable_language_stack nil)
  (set! sable_current_speaker 'voice_kal_diphone)
  (set! sable_speaker_stack nil)
)

(define (sable_token_to_words token name)
  "(sable_token_to_words utt token name)
SABLE mode token specific analysis."
  (cond
   ((or sable_omitted_mode sable_unsupported_language)
    ;; don't say anything (whole utterance)
    nil)
   ((string-equal "1" (item.feat token "done_sable_sub"))
    ;; to catch recursive calls this when splitting up sub expressions
    (sable_previous_token_to_words token name))
   ((and (not (string-equal "0" (item.feat token "sable_sub")))
	 (string-equal "0" (item.feat token "p.sable_sub")))
    (let (words (sub (item.feat token "sable_sub")))
      (item.set_feat token "done_sable_sub" "1")
      (set! words 
	    (apply append
		   (mapcar
		    (lambda (w)
		      (set! www (sable_previous_token_to_words token w))
		      www)
		    (read-from-string sub))))
      (item.set_feat token "done_sable_sub" "0")
      words))
   ((string-equal "1" (item.feat token "sable_ignore"))
    ;; don't say anything (individual word)
    nil)
   ((string-equal "1" (item.feat token "sable_ipa"))
    ;; Each token is an IPA phone
    (item.set_feat token "phonemes" (sable-map-ipa name))
    (list name))
   ((string-equal "1" (item.feat token "sable_literal"))
    ;; Only deal with spell here
    (let ((subwords) (subword))
      (item.set_feat token "pos" token.letter_pos)
      (mapcar
       (lambda (letter)
	 ;; might be symbols or digits
	 (set! subword (sable_previous_token_to_words token letter))
	 (if subwords
	     (set! subwords (append subwords subword))
	     (set! subwords subword)))
       (symbolexplode name))
      subwords))
   ((not (string-equal "0" (item.feat token "token_pos")))
    ;; bypass the prediction stage, if English
    (if (member_string (Parameter.get 'Language)
		       '(britishenglish americanenglish))
	(builtin_english_token_to_words token name)
	(sable_previous_token_to_words token name)))
   ;; could be others here later
   (t  
    (sable_previous_token_to_words token name))))

(defvar sable_elements
'(
  ("(SABLE" (ATTLIST UTT)
    (eval (list sable_current_speaker))  ;; so we know what state we start in
    (sable_setup_voice_params)
    nil
  )
  (")SABLE" (ATTLIST UTT)
    (xxml_synth UTT)  ;;  Synthesis the remaining tokens
    nil
  )
  ;; Utterance break elements
  ("(LANGUAGE" (ATTLIST UTT)
   ;; Status: probably complete 
   (xxml_synth UTT)
   (set! sable_language_stack 
	 (cons 
	  (list sable_current_language sable_unsupported_language)
	  sable_language_stack))
   ;; Select a new language
   (let ((language (upcase (car (xxml_attval "ID" ATTLIST)))))
     (cond
      ((or (string-equal language "SPANISH")
	   (string-equal language "ES"))
       (set! sable_current_language 'spanish)
       (set! sable_unsupported_language nil)
       (select_language 'spanish))
      ((or (string-equal language "ENGLISH")
	   (string-equal language "EN"))
       (set! sable_current_language 'britishenglish)
       (set! sable_unsupported_language nil)
       (select_language 'britishenglish))
      (t  ;; skip languages you don't know
       ;; BUG: if current language isn't English this wont work
       (apply_hooks tts_hooks
		    (eval (list 'Utterance 'Text
				(string-append "Some text in " language))))
       (set! sable_unsupported_language t)))
     nil))
  (")LANGUAGE" (ATTLIST UTT)
   (xxml_synth UTT)
   (set! sable_unsupported_language (car (cdr (car sable_language_stack))))
   (set! sable_current_language (car (car sable_language_stack)))
   (set! sable_language_stack (cdr sable_language_stack))
   (if (not sable_omitted_mode)
       (begin
	 (select_language sable_current_language)
	 (sable_setup_voice_params)))
   nil)
  ("(SPEAKER" (ATTLIST UTT)
   ;; Status: GENDER/AGE ignored, should be done by sable-def-speaker 
   ;;         function to define Festival voices to SABLE
   (xxml_synth UTT)
   (set! sable_speaker_stack (cons sable_current_speaker sable_speaker_stack))
   (cond
    ((not equal? sable_current_language 'britishenglish)
     (print "SABLE: choosen unknown voice, current voice unchanged"))
    ((equal? (car (xxml_attval "NAME" ATTLIST)) 'male1)
     (set! sable_current_speaker 'voice_kal_diphone)
     (voice_kal_diphone))
    ((equal? (car (xxml_attval "NAME" ATTLIST)) 'male2)
     (set! sable_current_speaker 'voice_cmu_us_rms_cg)
     (voice_cmu_us_rms_cg))
    ((equal? (car (xxml_attval "NAME" ATTLIST)) 'male3)
     (set! sable_current_speaker 'voice_ked_diphone)
     (voice_ked_diphone))
    ((equal? (car (xxml_attval "NAME" ATTLIST)) 'male4)
     (set! sable_current_speaker 'voice_rab_diphone)
     (voice_rab_diphone))
    ((equal? (car (xxml_attval "NAME" ATTLIST)) 'male5)
     (set! sable_current_speaker 'voice_cmu_us_awb_cg)
     (voice_cmu_us_awb_cg))
    ((equal? (car (xxml_attval "NAME" ATTLIST)) 'female1)
     (set! sable_current_speaker 'voice_cmu_us_slt_arctic_hts)
     (voice_us1_mbrola))
   (t
      (set! sable_current_speaker (intern (string-append "voice_" (car (xxml_attval "NAME" ATTLIST)))))
      (eval (list sable_current_speaker))))
    (sable_setup_voice_params)
   nil)
  (")SPEAKER" (ATTLIST UTT)
   (xxml_synth UTT)
   (set! sable_utt UTT)
   (set! sable_current_speaker (car sable_speaker_stack))
   (set! sable_speaker_stack (cdr sable_speaker_stack))
   (eval (list sable_current_speaker))
   (sable_setup_voice_params)
   nil)
  ("BREAK" (ATTLIST UTT)
   ;; Status: probably complete
   ;; may cause an utterance break
   (let ((level (upcase (car (xxml_attval "LEVEL" ATTLIST)))))
     (cond
      ((null UTT) nil)
      ((string-equal "LARGE" level)
       (xxml_synth UTT)
       nil)
      (t
       (let ((last_token (utt.relation.last UTT'Token)))
	 (if last_token
	     (item.set_feat last_token "pbreak" "B"))
	 UTT)))))
  ("(DIV" (ATLIST UTT)
   ;; Status: probably complete
   (xxml_synth UTT)
   nil)
  ("AUDIO" (ATTLIST UTT)
   ;; Status: MODE (background) ignored, only insertion supported
   ;; mime type of file also ignored, as its LEVEL
   (let ((tmpfile (make_tmp_filename)))
     ;; ignoring mode-background (and will for sometime)
     ;; ignoring level option
     (xxml_synth UTT)  ;; synthesizing anything ready to be synthesized
     (get_url (car (xxml_attval "SRC" ATTLIST)) tmpfile)
     (apply_hooks tts_hooks
		  (eval (list 'Utterance 'Wave tmpfile)))
     (delete-file tmpfile)
     nil))
  ("(EMPH" (ATTLIST UTT)
   ;; Status: nesting makes no difference, levels ignored
   ;; Festival is particularly bad at adding specific emphasis
   ;; that's what happens when you use statistical methods that
   ;; don't include any notion of emphasis
   ;; This is *not* recursive and only one level of EMPH supported
   (sable_push_word_features)
   (set! xxml_word_features 
	 (cons (list "dur_stretch" 1.6)
	       (cons
		(list "EMPH" "1") xxml_word_features)))
   UTT)
  (")EMPH" (ATTLIST UTT)
   (set! xxml_word_features (sable_pop_word_features))
   UTT)
  ("(PITCH" (ATTLIST UTT)
   ;; Status: probably complete
   ;; At present festival requires an utterance break here
   (xxml_synth UTT)
   (set! sable_pitch_context (cons int_lr_params sable_pitch_context))
   (let ((base (sable_interpret_param
		(car (xxml_attval "BASE" ATTLIST))
		sable_pitch_base_map
		(cadr (assoc 'target_f0_mean int_lr_params))
		sable_pitch_base_original))
	 (med (sable_interpret_param
	       (car (xxml_attval "MED" ATTLIST))
	       sable_pitch_med_map
	       (cadr (assoc 'target_f0_mean int_lr_params))
	       sable_pitch_med_original))
	 (range (sable_interpret_param
		 (car (xxml_attval "RANGE" ATTLIST))
		 sable_pitch_range_map
		 (cadr (assoc 'target_f0_std int_lr_params))
		 sable_pitch_range_original))
	 (oldmean (cadr (assoc 'target_f0_mean int_lr_params))))
     ;; Festival (if it supports anything) supports mean and std
     ;; so we treat base as med if med doesn't seem to do anything
     (if (equal? med oldmean)
	 (set! med base))
     (set! int_lr_params
	   (cons
	    (list 'target_f0_mean med)
	    (cons
	     (list 'target_f0_std range)
	     int_lr_params)))
   nil))
  (")PITCH" (ATTLIST UTT)
   (xxml_synth UTT)
   (set! int_lr_params (car sable_pitch_context))
   (set! sable_pitch_context (cdr sable_pitch_context))
   nil)
  ("(RATE" (ATTLIST UTT)
   ;; Status: can't deal with absolute word per minute SPEED.
   (sable_push_word_features)
   ;; can't deal with words per minute value
   (let ((rate (sable_interpret_param
		(car (xxml_attval "SPEED" ATTLIST))
		sable_rate_speed_map
		(sable_find_fval "dur_stretch" xxml_word_features 1.0)
		sable_rate_speed_original)))
     (set! xxml_word_features 
	   (cons (list "dur_stretch" (/ 1.0 rate)) xxml_word_features))
     UTT))
  (")RATE" (ATTLIST UTT)
   (set! xxml_word_features (sable_pop_word_features))
   UTT)
  ("(VOLUME" (ATTLIST UTT)
   ;; Status: probably complete
   ;; At present festival requires an utterance break here
   (xxml_synth UTT)
   (set! sable_vol_context (cons (list sable_vol_type sable_vol_factor)
				 sable_vol_context))
   (let ((level (sable_interpret_param
		(car (xxml_attval "LEVEL" ATTLIST))
		sable_volume_level_map
		sable_vol_factor
		1.0)))
     (cond
      ((string-matches (car (xxml_attval "LEVEL" ATTLIST)) ".*%")
       (set! sable_vol_type 'relative))
      ((string-matches (car (xxml_attval "LEVEL" ATTLIST))  SABLE_RXDOUBLE)
       (set! sable_vol_type 'absolute))
      (t
       (set! sable_vol_type 'relative)))
     (set! sable_vol_factor level))
   nil)
  (")VOLUME" (ATTLIST UTT)
   (xxml_synth UTT)
   (set! sable_vol_type (car (car sable_vol_context)))
   (set! sable_vol_factor (car (cdr (car sable_vol_context))))
   (set! sable_vol_context (cdr sable_vol_context))
   nil)
  ("(ENGINE" (ATTLIST UTT)
   ;; Status: probably complete
   (xxml_synth UTT)
   (if (string-matches (car (xxml_attval "ID" ATTLIST)) "festival.*")
       (let ((datastr ""))
	 (mapcar
	  (lambda (c) (set! datastr (string-append datastr " " c)))
	  (xxml_attval "DATA" ATTLIST))
	 (apply_hooks tts_hooks (eval (list 'Utterance 'Text datastr)))
	 (set! sable_omitted_mode t)) ;; ignore contents 
       ;; else 
       ;;  its not relevant to me
       )
   nil)
  (")ENGINE" (ATTLIST UTT)
   (xxml_synth UTT)
   (set! sable_omitted_mode nil)
   nil)
  ("MARKER" (ATTLIST UTT)
   ;; Status: does nothing
   ;; Can't support this without low-level control of audio spooler
   (format t "SABLE: marker \"%s\"\n" 
	   (car (xxml_attval "MARK" ATTLIST)))
   UTT)
  ("(PRON" (ATTLIST UTT)
   ;; Status: IPA currently ignored
   (sable_push_word_features)
   ;; can't deal with words per minute value
   (let ((ipa (xxml_attval "IPA" ATTLIST))
	 (sub (xxml_attval "SUB" ATTLIST)))
     (cond
      (ipa
       (format t "SABLE: ipa ignored\n")
       (set! xxml_word_features 
	     (cons (list "sable_ignore" "1") xxml_word_features)))
      (sub
       (set! xxml_word_features 
	     (cons (list "sable_sub" (format nil "%l" sub))
		   xxml_word_features))
       (set! xxml_word_features 
	     (cons (list "sable_ignore" "1") xxml_word_features))))
     UTT))
  (")PRON" (ATTLIST UTT)
   (set! xxml_word_features (sable_pop_word_features))
   UTT)
  ("(SAYAS" (ATTLIST UTT)
   ;; Status: only a few of the types are dealt with
   (sable_push_word_features)
    (set! sable_utt UTT)
   ;; can't deal with words per minute value
   (let ((mode (downcase (car (xxml_attval "MODE" ATTLIST))))
	 (modetype (car (xxml_attval "MODETYPE" ATTLIST))))
     (cond
      ((string-equal mode "literal")
       (set! xxml_word_features 
	     (cons (list "sable_literal" "1") xxml_word_features)))
      ((string-equal mode "phone")
       (set! xxml_word_features 
	     (cons (list "token_pos" "digits") xxml_word_features)))
      ((string-equal mode "ordinal")
       (set! xxml_word_features 
	     (cons (list "token_pos" "ordinal") xxml_word_features)))
      ((string-equal mode "cardinal")
       (set! xxml_word_features 
	     (cons (list "token_pos" "cardinal") xxml_word_features)))
      (t
       ;; blindly trust festival to get it right 
       t))
     UTT))
  (")SAYAS" (ATTLIST UTT)
   (set! xxml_word_features (sable_pop_word_features))
   UTT)

	     
))

(define (sable_init_func)
  "(sable_init_func)
Initialisation for SABLE mode"
  (sable_init_globals)
  (voice_kal_diphone)
  (set! sable_previous_elements xxml_elements)
  (set! xxml_elements sable_elements)
  (set! sable_previous_token_to_words english_token_to_words)
  (set! english_token_to_words sable_token_to_words)
  (set! token_to_words sable_token_to_words))

(define (sable_exit_func)
  "(sable_exit_func)
Exit function for SABLE mode"
  (set! xxml_elements sable_previous_elements)
  (set! token_to_words sable_previous_token_to_words)
  (set! english_token_to_words sable_previous_token_to_words))

(define (sable_push_word_features)
"(sable_push_word_features)
Save current word features on stack."
  (set! sable_word_features_stack 
	(cons xxml_word_features sable_word_features_stack)))

(define (sable_adjust_volume utt)
  "(sable_adjust_volume utt)
Amplify or attenutate signale based on value of sable_vol_factor
and sable_vol_type (absolute or relative)."
  (set! utts (cons utt utts))
  (cond
   ((equal? sable_vol_type 'no_change)
    utt)
   ((equal? sable_vol_type 'absolute)
    (utt.wave.rescale utt sable_vol_factor 'absolute))
   ((equal? sable_vol_type 'relative)
    (utt.wave.rescale utt sable_vol_factor))
   (t
    (format stderr "SABLE: volume unknown type \"%s\"\n" sable_vol_type)
    utt))
   utt)

(define (sable_pop_word_features)
"(sable_pop_word_features)
Pop word features from stack."
  (let ((r (car sable_word_features_stack)))
    (set! sable_word_features_stack (cdr sable_word_features_stack))
    r))

(define (sable_find_fval feat flist def)
  (cond
   ((null flist) def)
   ((string-equal feat (car (car flist)))
    (car (cdr (car flist))))
   (t
    (sable_find_fval feat (cdr flist) def))))

(define (sable_interpret_param ident map original current)
"(sable_interpret_param IDENT MAP ORIGINAL CURRENT)
If IDENT is in map return ORIGINAL times value in map, otherwise
treat IDENT of the form +/-N% and modify CURRENT accordingly."
  (let ((mm (assoc ident map)))
    (cond 
     (mm
      (* original (car (cdr mm))))
     ((string-matches ident SABLE_RXDOUBLE)
      (parse-number ident))
     ((string-matches ident ".*%")
      (+ current (* current (/ (parse-number (string-before ident "%")) 
			       100.0))))
;;     ((string-matches ident ".*%")
;;      (* current (/ (parse-number (string-before ident "%")) 100.0)))
     ((not ident) current)
     (t
      (format stderr "SABLE: modifier \"%s\" not of float, tag or +/-N\n"
	      ident)
      current))))

(define (sable_setup_voice_params)
"(sable_setup_voice_params)
Set up original values for various voice parameters."
 (set! sable_pitch_base_original (cadr (assoc 'target_f0_mean int_lr_params)))
 (set! sable_pitch_med_original (cadr (assoc 'target_f0_mean int_lr_params)))
 (set! sable_pitch_range_original (cadr (assoc 'target_f0_std int_lr_params)))
 (set! sable_rate_speed_original 1.0)
 (if (and after_synth_hooks (not (consp after_synth_hooks)))
     (set! after_synth_hooks 
	   (cons after_synth_hooks (list sable_adjust_volume)))
     (set! after_synth_hooks 
	   (append after_synth_hooks (list sable_adjust_volume))))
)

;;; Declare the new mode to Festival
(set! tts_text_modes
   (cons
    (list
      'sable   ;; mode name
      (list         
       (list 'init_func sable_init_func)
       (list 'exit_func sable_exit_func)
       '(analysis_type xml)
       ))
    tts_text_modes))

(provide 'sable-mode)
