;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
;;;                       Copyright (c) 2003, 2004                        ;;
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
;;; Multisyn top level scheme code  (Korin Richmond and Rob Clark)
;;;

; Requires
(require_module 'UniSyn)
(require_module 'MultiSyn)
(require 'multisyn_pauses)
(require 'target_cost)

;; use a global parameter to specify which UnitSelection voice
;; to use to synthesise a given utterance for now, because the
;; standard Festival synthesis mainline doesn't accept a voice
;; parameter.  (This should be set to the current voice object)
(defvar currentMultiSynVoice nil)
(defvar relp t)
(defvar flattenVoice nil)

; extract utt list from a .data file
(define (load_utt_list filename)
"(load_utt_list filename)
Loads a fextvox .data file and extracts an utterance list."
(let (l entries)
  (set! entries (load filename t))
  (mapcar
   (lambda (d)
     (set! l (cons (car d) l))
     t)
   entries)
l))

;; SynthType definition, main entry point.

(defSynthType MultiSyn
  ;(print "Multisyn unit selection synthesis")
  (defvar MultiSyn_module_hooks nil)
  (Param.def "unisyn.window_name" "hanning")
  (Param.def "unisyn.window_factor" 1.0)
  ;; Unisyn requires these to be set.
  (set! us_abs_offset 0.0)
  (set! us_rel_offset 0.0)

  (apply_hooks MultiSyn_module_hooks utt)  ;; 4processing of diphone names

  ;; find appropriate unit sequence and put sythesis
  ;; parameters in the Unit relation of the utterance structure
  (voice.getUnits currentMultiSynVoice utt)
  
  ;(print "doing concat")
  (us_unit_concat utt)

  ;(print "doing raw concat")

  (utt.relation.create utt 'SourceSegments)

  (set! do_prosmod (du_voice.prosodic_modification currentMultiSynVoice))

  (if do_prosmod
      (begin
	(if (not (member 'f0 (utt.relationnames utt)))
	    (targets_to_f0 utt))
	;; temporary fix
	(if (utt.relation.last utt 'Segment)
	    (set! pm_end (+ (item.feat (utt.relation.last utt 'Segment) "end") 0.02))
	    (set! pm_end 0.02))
	(us_f0_to_pitchmarks  utt 'f0 'TargetCoef pm_end)
	(us_mapping utt 'segment_single))
      (begin
	(utt.copy_relation utt 'SourceCoef 'TargetCoef)
	(us_mapping utt "linear")))


  ;(print "generating wave")
;; specify something else if you don't want lpc
  (us_generate_wave utt 'lpc)
)


; target cost scheme code 
(define (targetcost it1 it2)
  (Default_Target_Cost it1 it2))

; Evil function which writes the functions to actually load and switch new voices.
(define (make_voice_definition name srate config_function backoff_rules data_dir config)
  "(make_voice_definition NAME SRATE CONFIG_FUNCTION BACKOFF_RULES DATA_DIR CONFIG)
Create the fuction definitions to load and unload a voice."
  (let ((voice_name (string-append "voice_" name))
	(free_name (string-append "free_voice_" name))
	(pre_config_function (string-append config_function "_pre"))
	(voice_variable (upcase (string-append "voice_" name))))

    (eval (list 'defvar (intern voice_variable) nil))

    (eval (list 'define (list (intern voice_name))
		(list 'if (intern pre_config_function)
		      (list (intern pre_config_function) (intern voice_variable)))
		(list 'if (list 'null (intern voice_variable))
		      (list 'set! (intern voice_variable)
			    (list 'multisyn_load_voice_modules
				  (list 'quote name)
				  srate
				  (list 'quote backoff_rules)
				  data_dir
				  (list 'quote config))))
		(list (intern config_function) (intern voice_variable))
		(list 'set! 'current-voice (list 'quote name))
		(list 'define_current_voice_reset)
		(list 'set! 'currentMultiSynVoice (intern voice_variable))
		))

    (eval (list 'define
		(list (intern free_name))
		(list 'cond 
		      (list (list 'null (intern voice_variable))
			    (list 'error "Voice not currently loaded!"))
		      (list (list 'eq? 'currentMultiSynVoice (intern voice_variable))
			    (list 'error "Can't free current voice!"))
		      (list 't (list set! (intern voice_variable) 'nil))))))
  nil)
		      
(define (multisyn_load_voice_modules name srate backoff_rules base_dir module_list)
"(multisyn_add_module voice name srate backoff_rules base_dir module_list)
Add voice modules to a voice."
(let (voice)
  (mapcar
   (lambda (module_entry)
     (let ((dirs     (car module_entry))
	   (utt_list (load_utt_list (path-append base_dir 
						 (cadr module_entry)))))
       (if (null voice)
	   (set! voice (make_du_voice utt_list dirs srate))
	   (voice.addModule voice utt_list dirs srate))))
   module_list)
  (voice.setName voice name)
  (if flattenVoice 
      (du_voice.setTargetCost voice "flat")
      (du_voice.setTargetCost voice t))
  (du_voice.setJoinCost voice t)
  (format t "Please wait: Initialising multisyn voice.\n")
  (voice.init voice)
  (format t " Voice loaded successfully!\n")
  (du_voice.set_ob_pruning_beam voice 0.25)
  (du_voice.set_pruning_beam voice 0.25)
  (du_voice.setDiphoneBackoff voice backoff_rules)
voice))




(define (define_current_voice_reset)
"(define_current_voice_reset)
Re-define (current_voice_reset) correctly."
  (eval (list 'define
	      (list 'current_voice_reset)
	      (list 'multisyn_reset_globals))))

(define (multisyn_reset_globals)
"(multisyn_reset_globals)
Reset multisyn specific global variables."
(Param.set 'unisyn.window_symmetric 1))


(provide 'multisyn)
