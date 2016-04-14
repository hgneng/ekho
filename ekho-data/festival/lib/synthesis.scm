 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                                       ;;
 ;;                Centre for Speech Technology Research                  ;;
 ;;                     University of Edinburgh, UK                       ;;
 ;;                       Copyright (c) 1996,1997                         ;;
 ;;                        All Rights Reserved.                           ;;
 ;;                                                                       ;;
 ;;  Permission is hereby granted, free of charge, to use and distribute  ;;
 ;;  this software and its documentation without restriction, including   ;;
 ;;  without limitation the rights to use, copy, modify, merge, publish,  ;;
 ;;  distribute, sublicense, and/or sell copies of this work, and to      ;;
 ;;  permit persons to whom this work is furnished to do so, subject to   ;;
 ;;  the following conditions:                                            ;;
 ;;   1. The code must retain the above copyright notice, this list of    ;;
 ;;      conditions and the following disclaimer.                         ;;
 ;;   2. Any modifications must be clearly marked as such.                ;;
 ;;   3. Original authors' names are not deleted.                         ;;
 ;;   4. The authors' names are not used to endorse or promote products   ;;
 ;;      derived from this software without specific prior written        ;;
 ;;      permission.                                                      ;;
 ;;                                                                       ;;
 ;;  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        ;;
 ;;  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      ;;
 ;;  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   ;;
 ;;  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     ;;
 ;;  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    ;;
 ;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ;;
 ;;  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ;;
 ;;  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ;;
 ;;  THIS SOFTWARE.                                                       ;;
 ;;                                                                       ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                                       ;;
 ;;                 Author: Richard Caley (rjc@cstr.ed.ac.uk)             ;;
 ;;                   Date: Fri Aug 15 1997                               ;;
 ;; -------------------------------------------------------------------   ;;
 ;; New synthesis mainline.                                               ;;
 ;;                                                                       ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                                       ;;
 ;; Hooks to add to the synthesis process.                                ;;
 ;;                                                                       ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar default_before_synth_hooks nil
  "default_before_synth_hooks
   The default list of functions to be run on all synthesized utterances
   before synthesis starts.")

(defvar before_synth_hooks default_before_synth_hooks
  "before_synth_hooks
   List of functions to be run on synthesised utterances before synthesis
   starts.")

(defvar default_after_analysis_hooks nil
  "default_after_analysis_hooks
   The default list of functions to be run on all synthesized utterances
   after analysis but before synthesis.")

(defvar after_analysis_hooks default_after_analysis_hooks
  "after_analysis_hooks
   List of functions to be applied after analysis and before synthesis.")

(defvar default_after_synth_hooks nil
  "default_after_synth_hooks
   The default list of functions to be run on all synthesized utterances
   after Wave_Synth.  This will normally be nil but if for some reason you
   need to change the gain or rescale *all* waveforms you could set the
   function here, in your siteinit.scm.")

(defvar after_synth_hooks default_after_synth_hooks
  "after_synth_hooks
   List of functions to be applied after all synthesis modules have been
   applied.  This is primarily designed to allow waveform manipulation,
   particularly resampling and volume changes.")

(defvar default_access_strategy 'ondemand
  "default_access_strategy
   How to access units from databases.")

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                                       ;;
 ;; Macro to define utterance types.                                      ;;
 ;;                                                                       ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmac (defUttType form)
  (list 'defUttType_real 
	(list 'quote (cadr form))
	(list 'quote (cddr form))))

(defvar UttTypes nil
  "UttTypes
   List of types and functions used by the utt.synth function to call 
   appropriate methods.")

(define (defUttType_real type form)
  "(defUttType TYPE . BODY)
   Define a new utterance type.  TYPE is an atomic type that is specified
   as the first argument to the function Utterance.  BODY is evaluated
   with argument utt, when utt.synth is called with an utterance of type
   TYPE.  You almost always require the function Initialize first.
   [see Utterance types]"
  ;;; Yes I am cheating a bit with the macro/function name.
  ;;; should check about redefining and the syntax of the forms
  (set! UttTypes
	(cons 
	 (cons type form)
	 UttTypes))
  type)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                                       ;;
 ;; Macro to define synthesis types.                                      ;;
 ;;                                                                       ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmac (defSynthType form)
  (list 'defSynthType_real 
	(list 'quote (cadr form))
	(list 'quote (cddr form))))

(defvar SynthTypes nil
  "SynthTypes
   List of synthesis types and functions used by the utt.synth function to
   call appropriate methods for wave synthesis.")

(define (defSynthType_real type form)
  "(defSynthType TYPE . BODY)
   Define a new wave synthesis type.  TYPE is an atomic type that
   identifies the type of synthesis. BODY is evaluated with argument
   utt, when utt.synth is called with an utterance of type TYPE.
   [see Utterance types]"

  (set! SynthTypes
	(cons 
	 (cons type form)
	 SynthTypes))
  type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Some actual Utterance type definitions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defUttType Words
  (Initialize utt)
  (POS utt)
  (Phrasify utt)
  (Word utt)
  (Pauses utt)
  (Intonation utt)
  (PostLex utt)
  (Duration utt)
  (Int_Targets utt)
  (Wave_Synth utt)
  )

(defUttType Text
  (Initialize utt)
  (Text utt)
  (Token_POS utt)
  (Token utt)
  (POS utt)
  (Phrasify utt)
  (Word utt)
  (Pauses utt)
  (Intonation utt)
  (PostLex utt)
  (Duration utt)
  (Int_Targets utt)
  (Wave_Synth utt)
  )

(defUttType Tokens   ;; This is used in tts_file, Tokens will be preloaded
  (Token_POS utt)    ;; when utt.synth is called
  (Token utt)        
  (POS utt)
  (Phrasify utt)
  (Word utt)
  (Pauses utt)
  (Intonation utt)
  (PostLex utt)
  (Duration utt)
  (Int_Targets utt)
  (Wave_Synth utt)
  )

(defUttType Concept  ;; rather gradious name for when information has
  (POS utt)          ;; been preloaded (probably XML) to give a word
  (Phrasify utt)     ;; relation (SOLE uses this)
  (Word utt)
  (Pauses utt)
  (Intonation utt)
  (PostLex utt)
  (Duration utt)
  (Int_Targets utt)
  (Wave_Synth utt)
  )

(defUttType Phrase
  (Initialize utt)
  (Token_POS utt)
  (Token utt)        
  (POS utt)
  (Phrasify utt)
  (Word utt)
  (Pauses utt)
  (Intonation utt)
  (PostLex utt)
  (Duration utt)
  (Int_Targets utt)
  (Wave_Synth utt)
  )

(defUttType Segments
  (Initialize utt)
  (Wave_Synth utt)
  )

(defUttType Phones
  (Initialize utt)
  (Fixed_Prosody utt)
  (Wave_Synth utt)
  )

(defUttType SegF0
  (Wave_Synth utt)
  )

(defUttType Wave
  (Initialize utt))



 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                                       ;;
 ;; And some synthesis types.                                             ;;
 ;;                                                                       ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defSynthType Taylor
  (Taylor_Synthesize utt)
  )

(defSynthType UniSyn
  (defvar UniSyn_module_hooks nil)
  (Param.def "unisyn.window_name" "hanning")
  (Param.def "unisyn.window_factor" 1.0)
  (Parameter.def 'us_sigpr 'lpc)

  (apply_hooks UniSyn_module_hooks utt)  ;; for processing of diphone names
  (us_get_diphones utt)
  (us_unit_concat utt)
  
  (if (not (member 'f0 (utt.relationnames utt)))
      (targets_to_f0 utt))
;; temporary fix
  (if (utt.relation.last utt 'Segment)
      (set! pm_end (+ (item.feat (utt.relation.last utt 'Segment) "end") 0.02))
      (set! pm_end 0.02))

  (us_f0_to_pitchmarks  utt 'f0 'TargetCoef pm_end)
  (us_mapping utt 'segment_single)
  (cond
   ((string-equal "td_psola" (Parameter.get 'us_sigpr))
    ;; Not in standard distribution, so has to be separate function
    (us_tdpsola_synthesis utt 'analysis_period))
   (t
    ;; All the rest 
    (us_generate_wave utt (Parameter.get 'us_sigpr)
		      'analysis_period)))
)

(defSynthType None
  ;; do nothing
  utt
  )

(defSynthType Standard
  (print "synth method: Standard")
  
  (let ((select (Parameter.get 'SelectionMethod)))
    (if select
	(progn
	 (print "select")
	 (apply select (list utt))
	 )
	)
    )

  (let ((join (Parameter.get 'JoiningMethod)))
    (if join
	(progn
	 (print "join")
	 (apply join (list utt))
	 )
	)
    )

  (let ((impose (Parameter.get 'ImposeMethod)))
    (if impose
	(progn
	 (print "impose")
	 (apply impose (list utt))
	 )
	)
    )

  (let ((power (Parameter.get 'PowerSmoothMethod)))
    (if power
	(progn
	 (print "power")
	 (apply power (list utt))
	 )
	)
    )

  (let ((wavesynthesis (Parameter.get 'WaveSynthesisMethod)))
    (if wavesynthesis
	(progn
	 (print "synthesis")
	 (apply wavesynthesis (list utt))
	 )
	)
    )
  )

(defSynthType Minimal
  (print "synth method: Minimal")
  
  (let ((select (Parameter.get 'SelectionMethod)))
    (if select
	(progn
	 (print "select")
	 (apply select (list utt))
	 )
	)
    )

  (let ((wavesynthesis (Parameter.get 'WaveSynthesisMethod)))
    (if wavesynthesis
	(progn
	 (print "synthesis")
	 (apply wavesynthesis (list utt "Unit" "Join" "Wave"))
	 )
	)
    )
  )

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                                       ;;
 ;; Finally the actual driver function.                                   ;;
 ;;                                                                       ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (utt.synth utt)

  "(utt.synth UTT) 
    The main synthesis function.  Given UTT it will apply the
    functions specified for UTT's type, as defined with deffUttType
    and then those demanded by the voice.  After modules have been
    applied synth_hooks are applied to allow extra manipulation.
    [see Utterance types]"

  (apply_hooks before_synth_hooks utt)

  (let ((type (utt.type utt)))
    (let ((definition (assoc type UttTypes)))
      (if (null? definition)
	  (error "Unknown utterance type" type)
	  (let ((body (eval (cons 'lambda 
				  (cons '(utt) (cdr definition))))))
	    (body utt)))))

  (apply_hooks after_synth_hooks utt)
  utt)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                                       ;;
 ;; And a couple of utility expressions.                                  ;;
 ;;                                                                       ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (SayText text)
"(SayText TEXT)
TEXT, a string, is rendered as speech."
   (utt.play (utt.synth (eval (list 'Utterance 'Text text)))))

(define (SynthText text)
"(SynthText TEXT)
TEXT, a string, is rendered as speech."
   (utt.synth (eval (list 'Utterance 'Text text))))

(define (SayPhones phones)
"(SayPhones PHONES)
PHONES is a list of phonemes.  This uses the Phones type utterance
to synthesize and play the given phones.  Fixed duration specified in
FP_duration and fixed monotone duration (FP_F0) are used to generate
prosody."
   (utt.play (utt.synth (eval (list 'Utterance 'Phones phones)))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                                       ;;
 ;; This is the standard synthesis function.  The Wave Synthesis may be   ;;
 ;; more than a simple module                                             ;;
 ;;                                                                       ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (Wave_Synth utt)
"(Wave_Synth UTT)
  Generate waveform from information in UTT, at least a Segment stream
  must exist.  The actual form of synthesis used depends on the Parameter
  Synth_Method.   If it is a function that is applied.  If it is atom it
  should be a SynthType as defined by defSynthType
  [see Utterance types]"
  (apply_hooks after_analysis_hooks utt)
  (let ((method_val (Parameter.get 'Synth_Method)))
    (cond
     ((null method_val)
      (error "Undefined Synth_Method"))
     ((and (symbol? method_val) (symbol-bound? method_val))
      ;; Wish there was a function? 
      (apply (symbol-value method_val) (list utt)))
     ((member (typeof method_val) '(subr closure))
      (apply method_val (list utt)))
     (t  ;; its a defined synthesis type
      (let ((synthesis_modules (assoc_string method_val SynthTypes)))
	(if (null? synthesis_modules)
	    (error (format nil "Undefined SynthType %s\n" method_val))
	    (let ((body (eval (cons 'lambda 
				    (cons '(utt) (cdr synthesis_modules))))))
	      (body utt)))))))
  utt)

(provide 'synthesis)



