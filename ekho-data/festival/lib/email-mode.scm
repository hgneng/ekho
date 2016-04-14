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
;;;  An example tts text mode for reading email messages, this includes
;;;  support for extracting the interesting headers from the message
;;;  and for dealing with quoted text.  Its all very primitive and
;;;  will easily be confused but its here just as an example
;;;

(define (email_init_func)
 "(email_init_func)
Called on starting email text mode."
 (voice_rab_diphone)
 (set! email_previous_t2w_func token_to_words)
 (set! english_token_to_words email_token_to_words)
 (set! token_to_words english_token_to_words)
 (set! email_in_quote nil))

(define (email_exit_func)
 "(email_exit_func)
Called on exit email text mode."
 (set! english_token_to_words email_previous_t2w_func)
 (set! token_to_words english_token_to_words))

(define (email_token_to_words token name)
  "(email_token_to_words utt token name)
Email spcific token to word rules."
  (cond
   ((string-matches name "<.*@.*>")
     (append
      (email_previous_t2w_func token
       (string-after (string-before name "@") "<"))
      (cons 
       "at"
       (email_previous_t2w_func token
	(string-before (string-after name "@") ">")))))
   ((and (string-matches name ">")
         (string-matches (item.feat token "whitespace") 
			 "[ \t\n]*\n *"))
    (voice_cmu_us_awb_cg)
    nil ;; return nothing to say
   )
   (t  ;; for all other cases
     (if (string-matches (item.feat token "whitespace") 
			 ".*\n[ \n]*")
	 (voice_rab_diphone))
     (email_previous_t2w_func token name))))

(set! tts_text_modes
   (cons
    (list
      'email   ;; mode name
      (list         ;; email mode params
       (list 'init_func email_init_func)
       (list 'exit_func email_exit_func)
       '(filter "email_filter")))
    tts_text_modes))

(provide 'email-mode)
