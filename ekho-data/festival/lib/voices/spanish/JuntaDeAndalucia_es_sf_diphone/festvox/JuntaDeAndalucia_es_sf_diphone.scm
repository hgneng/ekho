;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                      JUNTA DE ANDALUCÍA                             ;;;
;;;                      Copyright (c) 2007                             ;;;
;;;                      All Rights Reserved.                           ;;;
;;;                                                                     ;;;
;;;  Distribution policy.                                               ;;;
;;;                                                                     ;;;
;;;  Free for any use.                                                  ;;;
;;;                                                                     ;;;
;;;  All the work is based on the Festvox Toolkit, provided by:         ;;;
;;;    - Carnegie Mellon University (http://www.festvox.org)            ;;;
;;;                                                                     ;;;
;;;  The copyright below belongs to the original Festvox project; it    ;;;
;;;  therefore applies to the present work.                             ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;  Standard Spanish female voice                                      ;;;
;;;                                                                     ;;;
;;;  Contractor: Consejería de Innovación, Ciencia y Empresa            ;;;
;;;              de la Junta de Andalucía                               ;;;
;;;                                                                     ;;;
;;;  Developed by: MP Sistemas and                                      ;;;
;;;                Intelligent Dialogue Systems S.L. (INDISYS)          ;;;
;;;                                                                     ;;;
;;;  Authors:   Del Solar, Carmen <c.delsolar@indisys.es>               ;;;
;;;             González, Jesús   <j.gonzalez@indisys.es>               ;;;
;;;             Manchón, Pilar    <p.manchon@indisys.es>                ;;;
;;;             Martín, Antonio   <amam@mpsistemas.es>                  ;;;
;;;             Martínez, Diego   <d.martinez@indisys.es>               ;;;
;;;             Pérez, Guillermo  <g.perez@indisys.es>                  ;;;
;;;             Varela, Víctor    <vmvr@mpsistemas.es>                  ;;;
;;;                                                                     ;;;
;;;  Voice Talent:  Silvia Fernández                                    ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;; Try to find out where we are
(if (assoc 'JuntaDeAndalucia_es_sf_diphone voice-locations)
    (defvar JuntaDeAndalucia_es_sf_dir 
      (cdr (assoc 'JuntaDeAndalucia_es_sf_diphone voice-locations)))
    ;;; Not installed in Festival yet so assume running in place
    (defvar JuntaDeAndalucia_es_sf_dir (pwd)))

(if (not (probe_file (path-append JuntaDeAndalucia_es_sf_dir "festvox/")))
    (begin
     (format stderr "JuntaDeAndalucia_es_sf: Can't find voice scm files they are not in\n")
     (format stderr "   %s\n" (path-append JuntaDeAndalucia_es_sf_dir "festvox/"))
     (format stderr "   Either the voice isn't linked into Festival\n")
     (format stderr "   or you are starting festival in the wrong directory\n")
     (error)))

;;;  Add the directory contains general voice stuff to load-path
(set! load-path (cons (path-append JuntaDeAndalucia_es_sf_dir "festvox/") load-path))

;;; Voice specific parameter are defined in each of the following
;;; files
(require 'JuntaDeAndalucia_es_sf_phoneset)
(require 'JuntaDeAndalucia_es_sf_tokenizer)
(require 'JuntaDeAndalucia_es_sf_tagger)
(require 'JuntaDeAndalucia_es_sf_lexicon)
(require 'JuntaDeAndalucia_es_sf_phrasing)
(require 'JuntaDeAndalucia_es_sf_intonation)
;;The modules below load the CART-tree duration model and the F0 contour model for JuntaDeAndalucia_es_sf
(require 'JuntaDeAndalucia_es_sf_duration)
(require 'JuntaDeAndalucia_es_sf_f0model)
(require 'JuntaDeAndalucia_es_sf_other)
;; ... and others as required

;;;  Ensure we have a festival with the right diphone support compiled in
(require_module 'UniSyn)

(set! JuntaDeAndalucia_es_sf_lpc_sep 
      (list
       '(name "JuntaDeAndalucia_es_sf_lpc_sep")
       (list 'index_file (path-append JuntaDeAndalucia_es_sf_dir "dic/sfdiph.est"))
       '(grouped "false")
       (list 'coef_dir (path-append JuntaDeAndalucia_es_sf_dir "lpc"))
       (list 'sig_dir  (path-append JuntaDeAndalucia_es_sf_dir "lpc"))
       '(coef_ext ".lpc")
       '(sig_ext ".res")
       (list 'default_diphone 
	     (string-append
	      (car (cadr (car (PhoneSet.description '(silences)))))
	      "-"
	      (car (cadr (car (PhoneSet.description '(silences)))))))))

(set! JuntaDeAndalucia_es_sf_lpc_group 
      (list
       '(name "sf_lpc_group")
       (list 'index_file 
	     (path-append JuntaDeAndalucia_es_sf_dir "group/sflpc16k.group"))
       '(grouped "true")
       (list 'default_diphone 
	     (string-append
	      (car (cadr (car (PhoneSet.description '(silences)))))
	      "-"
	      (car (cadr (car (PhoneSet.description '(silences)))))))))

;; Go ahead and set up the diphone db
;(set! JuntaDeAndalucia_es_sf_db_name (us_diphone_init JuntaDeAndalucia_es_sf_lpc_sep))
;; Once you've built the group file you can comment out the above and
;; uncomment the following.
(set! JuntaDeAndalucia_es_sf_db_name (us_diphone_init JuntaDeAndalucia_es_sf_lpc_group))

(define (JuntaDeAndalucia_es_sf_diphone_fix utt)
"(JuntaDeAndalucia_es_sf_diphone_fix UTT)
Map phones to phonological variants if the diphone database supports
them."
  (mapcar
   (lambda (s)
     (let ((name (item.name s)))
       (JuntaDeAndalucia_es_sf_diphone_fix_phone_name utt s)
       ))
   (utt.relation.items utt 'Segment))
  utt)

(define (JuntaDeAndalucia_es_sf_diphone_fix_phone_name utt seg)
"(JuntaDeAndalucia_es_sf_fix_phone_name UTT SEG)
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
	       (string-equal (item.feat seg "n.name") 'y))
	  (item.set_feat seg "us_diphone_left" (format nil "%s_" name)))
      (if (and (string-equal name 'y)
	       (string-equal (item.feat seg "p.name") 'hh))
	  (item.set_feat seg "us_diphone_right" (format nil "_%s" name)))
      (if (and (member_string name '(p t k b d g))
	       (member_string (item.feat seg "n.name") '(r w y l))
	       (item.relation.next seg "SylStructure"))
	  (item.set_feat seg "us_diphone_left" (format nil "%s_" name)))
      )
     ((string-equal "ah" (item.name seg))
      (item.set_feat seg "us_diphone" "aa"))

   )))

(define (JuntaDeAndalucia_es_sf_voice_reset)
  "(JuntaDeAndalucia_es_sf_voice_reset)
Reset global variables back to previous voice."
  (JuntaDeAndalucia_es_sf::reset_phoneset)
  (JuntaDeAndalucia_es_sf::reset_tokenizer)
  (JuntaDeAndalucia_es_sf::reset_tagger)
  (JuntaDeAndalucia_es_sf::reset_lexicon)
  (JuntaDeAndalucia_es_sf::reset_phrasing)
  (JuntaDeAndalucia_es_sf::reset_intonation)
  (JuntaDeAndalucia_es_sf::reset_duration)
  (JuntaDeAndalucia_es_sf::reset_f0model)
  (JuntaDeAndalucia_es_sf::reset_other)
)

;;;  Full voice definition 
(define (voice_JuntaDeAndalucia_es_sf_diphone)
"(voice_JuntaDeAndalucia_es_sf_diphone)
Set speaker to sf in us from JuntaDeAndalucia."

  ;; No pos prediction (get it from lexicon)
  (set! pos_lex_name nil)

  ;; Phrase break prediction by punctuation
  (set! pos_supported nil) ;; well not real pos anyhow

  ;; Accent and tone prediction
  (set! int_accent_cart_tree JuntaDeAndalucia_es_phrase_cart_tree)
  (Parameter.set 'Int_Target_Method 'Simple)

  (set! int_general_params (list (list 'targ_func JuntaDeAndalucia_es_sf_targ_func1)))
  (set! guess_pos spanish_guess_pos) 
  (Parameter.set 'Int_Method 'General)

  ;; Token to words
  (set! token_to_words JuntaDeAndalucia_es_sf::token_to_words)
  (Parameter.set 'Token_Method 'Token_Any)

  ;; Duration prediction

   ;;Uncomment this for loading the linear regression duration model 
   ;;This requires JuntaDeAndalucia_es_sf_durtreeZ.scm
  (set! duration_cart_tree JuntaDeAndalucia_es_sf_durtreeZ::zdur_tree)
  (set! duration_ph_info JuntaDeAndalucia_es_sf_durtreeZ::phone_durs)
  (Parameter.set 'Duration_Method 'Tree_ZScores)

   ;;Uncomment this for loading the rule-based duration model
   ;;This requires Indisys_MP_es_pa_durdata.scm
;  (set! duration_cart_tree JuntaDeAndalucia_es_sf::zdur_tree)
;  (set! duration_ph_info JuntaDeAndalucia_es_sf::phone_durs)
;  (Parameter.set 'Duration_Method 'Tree_ZScores)

  ;; Select appropriate phone set
  (JuntaDeAndalucia_es_sf::select_phoneset)

  ;; Select appropriate tokenization
  (JuntaDeAndalucia_es_sf::select_tokenizer)

  ;; For part of speech tagging
  (JuntaDeAndalucia_es_sf::select_tagger)

  (JuntaDeAndalucia_es_sf::select_lexicon)

  (JuntaDeAndalucia_es_sf::select_phrasing)

  (JuntaDeAndalucia_es_sf::select_intonation)

  (JuntaDeAndalucia_es_sf::select_duration)

  (JuntaDeAndalucia_es_sf::select_f0model)

  ;; Waveform synthesizer: UniSyn diphones
  (set! UniSyn_module_hooks (list JuntaDeAndalucia_es_sf_diphone_fix))
  (set! us_abs_offset 0.0)
  (set! window_factor 1.0)
  (set! us_rel_offset 0.0)
  (set! us_gain 0.9)

  (Parameter.set 'Synth_Method 'UniSyn)
  (Parameter.set 'POS_Method Classic_POS)
  (Parameter.set 'Phrasify_Method Classic_Phrasify)
  (Parameter.set 'Word_Method Classic_Word)
  (Parameter.set 'Pause_Method Classic_Pauses)
  (Parameter.set 'PostLex_Method Classic_PostLex)
  (Parameter.set 'us_sigpr 'lpc)
  (us_db_select JuntaDeAndalucia_es_sf_db_name)

  ;; This is where you can modify power (and sampling rate) if desired
  (set! after_synth_hooks nil)
;  (set! after_synth_hooks
;      (list
;        (lambda (utt)
;          (utt.wave.rescale utt 2.1))))

  ;; set callback to restore some original values changed by this voice
  (set! current_voice_reset JuntaDeAndalucia_es_sf_voice_reset)

  (set! current-voice 'JuntaDeAndalucia_es_sf_diphone)
)

(proclaim_voice
 'JuntaDeAndalucia_es_sf_diphone
 '((language spanish)
   (gender female)
   (dialect standard)
   (description
    "This is a standard female Spanish voice for the Festival UniSyn residual excited LPC diphone synthesizer developed by the Consejería de Innovación, Ciencia y Empresa of the Junta de Andalucía (Andalusian government). It includes an addenda for the pronunciation of special terms and abbreviations, hand-written letter-to-sound rules, a corpus-based F0 contour, a stochastic duration model, and punctuation-based prosodic phrasing. It was developed for the Guadalinex v4.0 distribution."
    )
   (builtwith festvox-2.0)))

(provide 'JuntaDeAndalucia_es_sf_diphone)
