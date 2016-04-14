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
;;     A voice based on "HTS" HMM-Based Speech Synthesis System.      ;;
;;          Author :  Alan W Black                                    ;;
;;          Date   :  August 2002                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Try to find the directory where the voice is, this may be from
;;; .../festival/lib/voices/ or from the current directory
(if (assoc 'cmu_us_slt_arctic_hts voice-locations)
    (defvar cmu_us_slt_arctic_hts::hts_dir 
      (cdr (assoc 'cmu_us_slt_arctic_hts voice-locations)))
    (defvar cmu_us_slt_arctic_hts::hts_dir (string-append (pwd) "/")))

(defvar cmu_us_slt_arctic::clunits_dir cmu_us_slt_arctic_hts::hts_dir)
(defvar cmu_us_slt_arctic::clunits_loaded nil)

;;; Did we succeed in finding it
(if (not (probe_file (path-append cmu_us_slt_arctic_hts::hts_dir "festvox/")))
    (begin
     (format stderr "cmu_us_slt_arctic_hts::hts: Can't find voice scm files they are not in\n")
     (format stderr "   %s\n" (path-append  cmu_us_slt_arctic_hts::hts_dir "festvox/"))
     (format stderr "   Either the voice isn't linked in Festival library\n")
     (format stderr "   or you are starting festival in the wrong directory\n")
     (error)))

;;;  Add the directory contains general voice stuff to load-path
(set! load-path (cons (path-append cmu_us_slt_arctic_hts::hts_dir "festvox/") 
		      load-path))

(set! hts_data_dir (path-append cmu_us_slt_arctic_hts::hts_dir "hts/"))

(set! hts_feats_list
      (load (path-append hts_data_dir "feat.list") t))

(require 'hts)
(require_module 'hts_engine)

;;; Voice specific parameter are defined in each of the following
;;; files
(require 'cmu_us_slt_arctic_phoneset)
(require 'cmu_us_slt_arctic_tokenizer)
(require 'cmu_us_slt_arctic_tagger)
(require 'cmu_us_slt_arctic_lexicon)
(require 'cmu_us_slt_arctic_phrasing)
(require 'cmu_us_slt_arctic_intonation)
(require 'cmu_us_slt_arctic_duration)
(require 'cmu_us_slt_arctic_f0model)
(require 'cmu_us_slt_arctic_other)
;; ... and others as required


(define (cmu_us_slt_arctic_hts::voice_reset)
  "(cmu_us_slt_arctic_hts::voice_reset)
Reset global variables back to previous voice."
  (cmu_us_slt_arctic::reset_phoneset)
  (cmu_us_slt_arctic::reset_tokenizer)
  (cmu_us_slt_arctic::reset_tagger)
  (cmu_us_slt_arctic::reset_lexicon)
  (cmu_us_slt_arctic::reset_phrasing)
  (cmu_us_slt_arctic::reset_intonation)
  (cmu_us_slt_arctic::reset_duration)
  (cmu_us_slt_arctic::reset_f0model)
  (cmu_us_slt_arctic::reset_other)

  t
)

(set! cmu_us_slt_arctic_hts::hts_feats_list
      (load (path-append hts_data_dir "feat.list") t))

(set! cmu_us_slt_arctic_hts::hts_engine_params
      (list
       (list "-md" (path-append hts_data_dir "dur.pdf"))
       (list "-mm" (path-append hts_data_dir "mgc.pdf"))
       (list "-mf" (path-append hts_data_dir "lf0.pdf"))
       (list "-td" (path-append hts_data_dir "tree-dur.inf"))
       (list "-tm" (path-append hts_data_dir "tree-mgc.inf"))
       (list "-tf" (path-append hts_data_dir "tree-lf0.inf"))
       (list "-dm1" (path-append hts_data_dir "mgc.win1"))
       (list "-dm2" (path-append hts_data_dir "mgc.win2"))
       (list "-dm3" (path-append hts_data_dir "mgc.win3"))
       (list "-df1" (path-append hts_data_dir "lf0.win1"))
       (list "-df2" (path-append hts_data_dir "lf0.win2"))
       (list "-df3" (path-append hts_data_dir "lf0.win3"))
       (list "-cm" (path-append hts_data_dir "gv-mgc.pdf"))
       (list "-cf" (path-append hts_data_dir "gv-lf0.pdf"))
       (list "-em" (path-append hts_data_dir "tree-gv-mgc.inf"))
       (list "-ef" (path-append hts_data_dir "tree-gv-lf0.inf"))
       (list "-k" (path-append hts_data_dir "gv-switch.inf"))
       '("-s" 32000.0)
       '("-p" 160.0)
       '("-a" 0.45)
       '("-g" 0.0)
       '("-b" 0.0)
       '("-u" 0.5)
       ))

;; This function is called to setup a voice.  It will typically
;; simply call functions that are defined in other files in this directory
;; Sometime these simply set up standard Festival modules othertimes
;; these will be specific to this voice.
;; Feel free to add to this list if your language requires it

(define (voice_cmu_us_slt_arctic_hts)
  "(voice_cmu_us_slt_arctic_hts)
Define voice for limited domain: us."
  ;; *always* required
  (voice_reset)

  ;; Select appropriate phone set
  (cmu_us_slt_arctic::select_phoneset)

  ;; Select appropriate tokenization
  (cmu_us_slt_arctic::select_tokenizer)

  ;; For part of speech tagging
  (cmu_us_slt_arctic::select_tagger)

  (cmu_us_slt_arctic::select_lexicon)
  ;; For hts selection you probably don't want vowel reduction
  ;; the unit selection will do that
  (if (string-equal "americanenglish" (Param.get 'Language))
      (set! postlex_vowel_reduce_cart_tree nil))

  (cmu_us_slt_arctic::select_phrasing)

  (cmu_us_slt_arctic::select_intonation)

  (cmu_us_slt_arctic::select_duration)

  (cmu_us_slt_arctic::select_f0model)

  ;; Waveform synthesis model: hts
  (set! hts_engine_params cmu_us_slt_arctic_hts::hts_engine_params)
  (set! hts_feats_list cmu_us_slt_arctic_hts::hts_feats_list)
  (Parameter.set 'Synth_Method 'HTS)

  ;; This is where you can modify power (and sampling rate) if desired
  (set! after_synth_hooks nil)
;  (set! after_synth_hooks
;      (list
;        (lambda (utt)
;          (utt.wave.rescale utt 2.1))))

  (set! current_voice_reset cmu_us_slt_arctic_hts::voice_reset)

  (set! current-voice 'cmu_us_slt_arctic_hts)
)

(provide 'cmu_us_slt_arctic_hts)

