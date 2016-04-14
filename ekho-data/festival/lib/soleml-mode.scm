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
;;;
;;;  Support for an SGML based mark-up language used in the SOLE
;;;  project.  This is all still experimental.
;;;
;;;  This currently treats one file as one utterance (to make dealing with
;;;  the SOLE museaum database easy

(set! soleml_word_features_stack nil)
(defvar sole_current_node nil)

(define (soleml_token_to_words utt token name)
  "(soleml_token_to_words utt token name)
SOLEML mode token specific analysis."
  (cond

   (t
    (soleml_previous_token_to_words utt token name))))

(define (voice_soleml)
"(soleml_voice)
Speaker specific initialisation for SOLE museum data."
  (voice_rab_diphone)
  ;; Utterances only come at end of file
  (set! eou_tree '((0)))
)

(defvar soleml_elements
'(
  ("(SOLEML" (ATTLIST UTT)
    ;; required to identify type 
    (voice_soleml)  ;; so we know what state we start in
    (set! soleml_utt (Utterance Tokens nil))
    (utt.stream.create soleml_utt 'Token)
    (utt.relation.create soleml_utt 'SOLEML)
    (set! sole_current_node 
	  (utt.relation_append soleml_utt 'SOLEML (cons "sole-ml" ATTLIST)))
    soleml_utt
  )
  (")SOLEML" (ATTLIST UTT)
    ;; required to identify end token
    ;; Don't really want to synthesize this
    ;; (xxml_synth UTT)  ;;  Synthesis the remaining tokens
    (set! soleml_utt UTT)	     
    UTT
  )
  ;; Utterance break elements
  ("(LANGUAGE" (ATTLIST UTT)
   ;; Select a new language
   (select_language (car (xxml_attval "NAME" ATTLIST)))
   UTT)
  ("(VOICE" (ATTLIST UTT)
   ;;(xxml_synth UTT)
   ;; Select a new voice
   (cond
    ((equal? (car (xxml_attval "NAME" ATTLIST)) 'male1)
     (voice_soleml_diphone))
    ((equal? (car (xxml_attval "NAME" ATTLIST)) 'male2)
     (voice_soleml_diphone))
    ((equal? (car (xxml_attval "NAME" ATTLIST)) 'male3)
     (voice_soleml_diphone))
    (t
     (print "SOLEML: selecting unknown voice")
     (voice_soleml_diphone)))
   UTT)
  ;; phrase-boundary  // mark on token (??)
  ;; punct-elem     // mark on token
  ;; sem-elem
  ;; text-elem      // ignore
  ;; rhet-elem  has nucleus and satellite
  ;; anaphora-elem
  ;; syn-elem
  ;; info-struct-elem
  ;; other-elem
  ("(PUNCT-ELEM" (ATTLIST UTT) 
   (soleml_push_word_features)
   (set! xxml_word_features
	  (cons (list "punct-elem" "1")
		(soleml_conv_attlist ATTLIST)))
   UTT)
  (")PUNCT-ELEM" (ATTLIST UTT) 
   (set! xxml_word_features (soleml_pop_word_features))
   UTT)
  ("(PHRASE-BOUNDARY" (ATTLIST UTT)
   (if (string-equal "4" (car (xxml_attval "STRENGTH" ATTLIST)))
       (begin
;;	 (xxml_synth UTT)
	 UTT)
       (let ((last_token (car (last (utt.stream UTT 'Token)))))
	 (if last_token
	     (item.set_feat last_token "pbreak" "B"))
	 UTT)))
  ;; For each recursive element simply build a new node
  ("(RHET-ELEM" (ATTLIST UTT)
   (let ((sdesc (list 'rhet-elem (soleml_conv_attlist ATTLIST))))
     (set! sole_current_node
	   (node.append_daughter sole_current_node sdesc))
     UTT))
  (")RHET-ELEM" (ATTLIST UTT)
    (set! sole_current_node (node.parent sole_current_node))
    UTT)
  ("(RHET-EMPH" (ATTLIST UTT)
   (let ((sdesc (list 'rhet-emph (soleml_conv_attlist ATTLIST))))
     (set! sole_current_node
	   (node.append_daughter sole_current_node sdesc))
     UTT))
  (")RHET-EMPH" (ATTLIST UTT)
    (set! sole_current_node (node.parent sole_current_node))
    UTT)
  ("(ANAPHORA-ELEM" (ATTLIST UTT)
   (let ((sdesc (list 'anaphora-elem (soleml_conv_attlist ATTLIST))))
     (set! sole_current_node
	   (node.append_daughter sole_current_node sdesc))
     UTT))
  (")ANAPHORA-ELEM" (ATTLIST UTT)
    (set! sole_current_node (node.parent sole_current_node))
    UTT)
  ("(SYN-ELEM" (ATTLIST UTT)
   (let ((sdesc (list 'syn-elem (soleml_conv_attlist ATTLIST))))
     (set! sole_current_node
	   (node.append_daughter sole_current_node sdesc))
     UTT))
  (")SYN-ELEM" (ATTLIST UTT)
    (set! sole_current_node (node.parent sole_current_node))
    UTT)
  ("(CONNECTIVE" (ATTLIST UTT)
   (let ((sdesc (list 'connective (soleml_conv_attlist ATTLIST))))
     (set! sole_current_node
	   (node.append_daughter sole_current_node sdesc))
     UTT))
  (")CONNECTIVE" (ATTLIST UTT)
    (set! sole_current_node (node.parent sole_current_node))
    UTT)
  ("(TEXT-ELEM" (ATTLIST UTT)
   (let ((sdesc (list 'text-elem (soleml_conv_attlist ATTLIST))))
     (set! sole_current_node
	   (node.append_daughter sole_current_node sdesc))
     UTT))
  (")TEXT-ELEM" (ATTLIST UTT)
    (set! sole_current_node (node.parent sole_current_node))
    UTT)
  ("(SEM-ELEM" (ATTLIST UTT)
   (let ((sdesc (list 'sem-elem (soleml_conv_attlist ATTLIST))))
     (set! sole_current_node
	   (node.append_daughter sole_current_node sdesc))
     UTT))
  (")SEM-ELEM" (ATTLIST UTT)
    (set! sole_current_node (node.parent sole_current_node))
    UTT)
  ("(INFO-STRUCT-ELEM" (ATTLIST UTT)
   (let ((sdesc (list 'info-struct-elem (soleml_conv_attlist ATTLIST))))
     (set! sole_current_node
	   (node.append_daughter sole_current_node sdesc))
     UTT))
  (")INFO-STRUCT-ELEM" (ATTLIST UTT)
    (set! sole_current_node (node.parent sole_current_node))
    UTT)
  ("(OTHER-ELEM" (ATTLIST UTT)
   (let ((sdesc (list 'other-elem (soleml_conv_attlist ATTLIST))))
     (set! sole_current_node
	   (node.append_daughter sole_current_node sdesc))
     UTT))
  (")OTHER-ELEM" (ATTLIST UTT)
    (set! sole_current_node (node.parent sole_current_node))
    UTT)
  ("(NUCLEUS" (ATTLIST UTT)
   (let ((sdesc (list 'nucleus (soleml_conv_attlist ATTLIST))))
     (set! sole_current_node
	   (node.append_daughter sole_current_node sdesc))
     UTT))
  (")NUCLEUS" (ATTLIST UTT)
    (set! sole_current_node (node.parent sole_current_node))
    UTT)
  ("(SATELLITE" (ATTLIST UTT)
   (let ((sdesc (list 'satellite (soleml_conv_attlist ATTLIST))))
     (set! sole_current_node
	   (node.append_daughter sole_current_node sdesc))
     UTT))
  (")SATELLITE" (ATTLIST UTT)
    (set! sole_current_node (node.parent sole_current_node))
    UTT)
  ;; Other control functions (probably not used in SOLE)  
  ("(CALL" (ATTLIST UTT)
;;   (xxml_synth UTT)
   (if (string-matches (car (xxml_attval "ENGID" ATTLIST)) "festival.*")
       (let ((comstr ""))
	 (mapcar
	  (lambda (c) (set! comstr (string-append comstr " " c)))
	  (xxml_attval "COMMAND" ATTLIST))
	 (eval (read-from-string comstr))))
   UTT)
  ("(DEFINE" (ATTLIST UTT)
;;    (xxml_synth UTT)
    (if (not (string-equal "NATIVE" (car (xxml_attval "SCHEME" ATTLIST))))
	(format t "DEFINE: unsupported SCHEME %s, definition ignored\n"
		(car (xxml_attval "SCHEME" ATTLIST)))
	(lex.add.entry
	 (list
	  (car (xxml_attval "WORDS" ATTLIST))   ;; head form
	  nil          ;; pos
	  (lex.syllabify.phstress (xxml_attval "PRONS" ATTLIST)))))
    UTT)
  ("(SOUND" (ATTLIST UTT)
;;   (xxml_synth UTT)
   (if (not soleml_omitted_mode)
       (apply_hooks tts_hooks
		    (eval (list 'Utterance 'Wave 
				(car (xxml_attval "SRC" ATTLIST))))))
   UTT)
  ("(EMPH" (ATTLIST UTT)
   ;; Festival is particularly bad at adding specific emphasis
   ;; that's what happens when you use statistical methods that
   ;; don't include any notion of emphasis
   ;; This is *not* recursive
   (soleml_push_word_features)
   (set! xxml_word_features 
	 (cons (list "EMPH" "1") xxml_word_features))
   UTT)
  (")EMPH" (ATTLIST UTT)
   (set! xxml_word_features (soleml_pop_word_features))
   UTT)
  ("(WORD" (ATTLIST UTT)
   ;; a word in-line
   (let ((name   (xxml_attval "NAME" ATTLIST))
	 (pos    (xxml_attval "POS" ATTLIST))
	 (accent (xxml_attval "ACCENT" ATTLIST))
	 (tone   (xxml_attval "TONE" ATTLIST))
	 (phonemes (xxml_attval "PHONEMES" ATTLIST))
	 token)
     (utt.item.insert UTT 'Token)  ;; add new Token
     (set! token (utt.stream.tail UTT 'Token))
     (item.set_name token (car name))
     (if pos (item.set_feat token "pos" (car pos)))
     (if accent (item.set_feat token "accent" (car accent)))
     (if tone (item.set_feat token "tone" (car tone)))
     (if phonemes (item.set_feat token "phonemes" 
				       (format nil "%l" phonemes)))
     UTT))
))

(define (soleml_init_func)
  "(soleml_init_func)
Initialisation for SOLEML mode"
  (voice_soleml)
  (set! soleml_previous_elements xxml_elements)
  (set! xxml_elements soleml_elements)
  (set! xxml_token_hooks soleml_token_function)
  (set! soleml_previous_token_to_words english_token_to_words)
  (set! english_token_to_words soleml_token_to_words)
  (set! token_to_words soleml_token_to_words))

(define (soleml_exit_func)
  "(soleml_exit_func)
Exit function for SOLEML mode"
  (set! xxml_elements soleml_previous_elements)
  (set! token_to_words soleml_previous_token_to_words)
  (set! english_token_to_words soleml_previous_token_to_words))

(define (soleml_token_function si)
"(soleml_token_function si)
This is called for each token found."
  (node.append_daughter sole_current_node si))

(define (soleml_push_word_features)
"(soleml_push_word_features)
Save current word features on stack."
  (set! soleml_word_features_stack 
	(cons xxml_word_features soleml_word_features_stack)))

(define (soleml_pop_word_features)
"(soleml_pop_word_features)
Pop word features from stack."
  (let ((r (car soleml_word_features_stack)))
    (set! soleml_word_features_stack (cdr soleml_word_features_stack))
    r))

(define (soleml_conv_attlist alist)
"(soleml_conv_attlist alist)
Flatten alist arguments."
  (cond
   ((null alist) nil)
   ((null (car (cdr (car alist))))
     (soleml_conv_attlist (cdr alist)))
   ((equal? (length (car (cdr (car alist)))) 1)
    (cons
     (list (car (car alist)) (car (car (cdr (car alist)))))
     (soleml_conv_attlist (cdr alist))))
   (t
    (cons
     (list (car (car alist)) (format nil "%l" (car (cdr (car alist)))))
     (soleml_conv_attlist (cdr alist))))))

(set! tts_text_modes
   (cons
    (list
      'soleml   ;; mode name
      (list         ;; email mode params
       (list 'init_func soleml_init_func)
       (list 'exit_func soleml_exit_func)
       '(analysis_type xxml)
       (list 'filter 
	     (format nil "%s -D %s " sgml_parse_progname libdir))))
    tts_text_modes))

(provide 'soleml-mode)
