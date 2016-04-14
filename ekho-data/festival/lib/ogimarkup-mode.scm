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
;;;  An example tts text mode for reading OGI's CSLU toolkit mark up
;;;
;;;  Note not all tokens do something in festival but  all are removed 
;;;  from the actual text 
;;;

(defvar ogimarkup_eou_tree 
'((n.name matches "<.*")
  ((1))
((n.whitespace matches ".*\n.*\n\\(.\\|\n\\)*") ;; A significant break (2 nls)
  ((1))
  ((punc in ("?" ":" "!"))
   ((1))
   ((punc is ".")
    ;; This is to distinguish abbreviations vs periods
    ;; These are heuristics
    ((name matches "\\(.*\\..*\\|[A-Z][A-Za-z]?[A-Za-z]?\\|etc\\)")  ;; an abbreviation
     ((n.whitespace is " ")
      ((0))                  ;; if abbrev single space isn't enough for break
      ((n.name matches "[A-Z].*")
       ((1))
       ((0))))
     ((n.whitespace is " ")  ;; if it doesn't look like an abbreviation
      ((n.name matches "[A-Z].*")  ;; single space and non-cap is no break
       ((1))
       ((0)))
      ((1))))
    ((0)))))))

(define (ogimarkup_init_func)
 "Called on starting ogimarkup text mode."
 (set! ogimarkup_in_tag nil)
 (set! ogimarkup_tagtokens "")
 (set! ogimarkup_previous_t2w_func token_to_words)
 (set! english_token_to_words ogimarkup_token_to_words)
 (set! token_to_words ogimarkup_token_to_words)
 (set! ogimarkup_previous_eou_tree eou_tree)
 (set! eou_tree ogimarkup_eou_tree))

(define (ogimarkup_exit_func)
 "Called on exit ogimarkup text mode."
 (Parameter.set 'Duration_Stretch 1.0)
 (set! token_to_words ogimarkup_previous_t2w_func)
 (set! english_token_to_words ogimarkup_previous_t2w_func)
 (set! eou_tree ogimarkup_previous_eou_tree))

(define (ogimarkup_token_to_words token name)
  "(ogimarkup_token_to_words token name)
OGI markup specific token to word rules.  Tags may have optional
argument e.g. <slow> or <slow 0.6> which means the tag may be over
a number of tokens."
  (let (tag (arg nil) (rval nil))
  (cond
   ((string-matches name "<.*")
    (set! ogimarkup_tagtokens "")
    (set! tag (string-after name "<"))
    (if (string-matches tag ".*>$")
	(set! tag (string-before tag ">"))
	(if (string-matches (set! arg (item.feat token "n.name"))
			    ".*>$")
	    (set! arg (string-before arg ">"))))
    (set! ogimarkup_in_tag tag)
    (cond
     ((string-equal tag "slow")
      (Parameter.set 'Duration_Stretch 1.3))
     ((string-equal tag "SLOW")
      (Parameter.set 'Duration_Stretch 2.0))
     ((string-equal tag "normal")
      (Parameter.set 'Duration_Stretch 1.0))
     ((string-matches tag "FAST")
      (Parameter.set 'Duration_Stretch 0.5))
     ((string-matches tag "fast")
      (Parameter.set 'Duration_Stretch 0.8))
     ((string-matches tag"spell")
      ;; This ain't really right as we'll get an utterance break here
      (set! rval (symbolexplode arg)))
     ((string-matches tag "phone")
      ;; This ain't really right as we'll get an utterance break here
      (item.set_feat token "token_pos" "digits")  ;; canonical phone number
      (set! rval (ogimarkup_previous_t2w_func token arg)))
     ((string-matches tag "male")
      (if (and (member 'OGIresLPC *modules*)
	       (symbol-bound? 'voice_aec_diphone))
	  (voice_aec_diphone)
	  (voice_kal_diphone)))
     ((string-matches tag "Male")
      (if (and (member 'OGIresLPC *modules*)
	       (symbol-bound? 'voice_mwm_diphone))
	  (voice_mwm_diphone)
	  (voice_cmu_us_rms_cg)))
     ((string-matches tag "MALE")
      (if (and (member 'OGIresLPC *modules*)
	       (symbol-bound? 'voice_jph_diphone))
	  (voice_jph_diphone)
	  (voice_rab_diphone)))
     ((string-matches tag "FT")
      t)  ;; do nothing until the end of this tag
     ((string-matches (downcase tag) "female") 
      ;; only one female voice so map female Female FEMALE to it
      (if (and (member 'OGIresLPC *modules*)
	       (symbol-bound? 'voice_tll_diphone))
	  (voice_tll_diphone)
	  (voice_cmu_us_slt_arctic_hts))))
    (if (string-matches name ".*>$")
	(set! ogimarkup_in_tag nil))
    rval ;; mostly nil
    )
   ((string-matches name ".*>$")
    (set! ogimarkup_tagtokens 
	  (string-append
	   ogimarkup_tagtokens
	   (ogimarkup_get_token_string token t)))  ;; delete final >
    (if (string-equal ogimarkup_in_tag "FT")
	(ogimarkup_festival_eval ogimarkup_tagtokens))
    (set! ogimarkup_in_tag nil)   ;; end of tag
    nil)
   (ogimarkup_in_tag
    (set! ogimarkup_tagtokens
	  (string-append
	   ogimarkup_tagtokens
	   (ogimarkup_get_token_string token nil)))
    nil)                          ;; still in tag
   (t  ;; for all other cases
     (ogimarkup_previous_t2w_func token name)))))

(set! tts_text_modes
   (cons
    (list
      'ogimarkup   ;; mode name
      (list         ;; ogimarkup mode params
       (list 'init_func ogimarkup_init_func)
       (list 'exit_func ogimarkup_exit_func)))
    tts_text_modes))

(define (ogimarkup_get_token_string token delend)
  "(ogimarkup_get_token_string TOKEN DELEND)
return string for token including whitespace and punctuation.  If DELEND
is true remove > from the name."
  (string-append
   (item.feat token "whitespace")
   (item.feat token "prepunctuation")  
   (if delend
       (string-before 
	(item.feat token "name") ">")
       (item.feat token "name"))
   (if (string-equal "0" (item.feat token "punc"))
       ""
       (item.feat token "punc"))))

(define (ogimarkup_festival_eval tagtokens)
"(ogimarkup_festival_eval TAGTOKENS
Take a string of the tokens within the tag and read an s-expression from
it and then evaluate it."
  (let ((com "") (command nil))
    (set! command (read-from-string tagtokens))
    (eval command)))

(provide 'ogimarkup-mode)
