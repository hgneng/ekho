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
;;;  Various tokenizing functions and customization 

(define (Token utt)
  "(Token UTT)
Build a Word stream from the Token stream, analyzing compound words
numbers etc as tokens into words. Respects the Parameter Language
to choose the appropriate token to word module."
  (let ((rval (apply_method 'Token_Method utt)) ;; might be defined
	(language (Parameter.get 'Language)))
    (cond
     (rval rval)  ;; newer style
     ((or (string-equal "britishenglish" language)
	  (string-equal "english" language)
	  (string-equal "americanenglish" language))
      (Token_English utt))
     ((string-equal "welsh" language)
      (Token_Welsh utt))
     (t
      (Token_Any utt)))))

(define (remove_leadtrail_underscores name)
  "(remove_leadtrail_underscores name)
Get rid of leading and trailing underscores that may be used for emphasis,
not this is called when there are underscores at the beginning and end but
there may not be an equal number of them."
  (let ((se (symbolexplode name)))
    (while (string-equal "_" (car se))
      (set! se (cdr se)))
    (set! se (reverse se))
    (while (string-equal "_" (car se))
      (set! se (cdr se)))
    (apply string-append (reverse se))))

(define (english_token_to_words token name)
"(english_token_to_words TOKEN NAME)
Returns a list of words for NAME from TOKEN.  This allows the
user to customize various non-local, multi-word, context dependent
translations of tokens into words.  If this function is unset only
the builtin translation rules are used, if this is set the builtin
rules are not used unless explicitly called. [see Token to word rules]"
 (cond
  ((string-matches name "[A-Z]*[\\$#\\\\Y£][0-9,]+\\(\\.[0-9]+\\)?")
   ;; Some form of money (pounds or type of dollars)
   (let (amount type currency)
     (cond
      ((string-matches name ".*\\$.*")
       (set! amount (string-after name "$"))
       (set! type (string-before name "$"))
       (set! currency "dollar"))
      ((string-matches name ".*£.*")
       (set! amount (string-after name "£"))
       (set! type (string-before name "£"))
       (set! currency "pound"))
      ((string-matches name ".*#.*")
       (set! amount (string-after name "#"))
       (set! type (string-before name "#"))
       (set! currency "pound"))
      ((string-matches name ".*Y[0-9].*")
       (set! amount (string-after name "Y"))
       (set! type (string-before name "Y"))
       (set! currency "yen"))
      ((string-matches name ".*\\\\.*")
       (set! amount (string-after name "\\"))
       (set! type (string-before name "\\"))
       (set! currency "yen"))
      (t
       ;; who knows
       (set! amount (string-after name "$"))
       (set! type (string-before name "$"))
       (set! currency "dollar")))
     (cond
      ((string-matches (item.feat token "n.name")
		       ".*illion.?")
       (append   ;; "billions and billions" - Sagan
	(builtin_english_token_to_words token amount)
	(list (item.feat token "n.name")) ;; illion
	(token_money_expand type)
	(list (string-append currency "s"))))
      ((string-matches amount ".*\\...$")
       (append   ;; exactly two places after point
	(builtin_english_token_to_words token (string-before amount "."))
	(token_money_expand type)
	(if (or (string-matches amount "1\\..*")
		(string-equal currency "yen"))
	    (list currency)
	    (list (string-append currency "s")))
	(if (not (string-matches name ".*\\.00$"))
	    (builtin_english_token_to_words 
	     token (remove_leading_zeros (string-after amount ".")))
	    nil)))
      (t
       (append   ;; nothing after point or lots after point
	(builtin_english_token_to_words token amount)
	(token_money_expand type)
	(if (or (string-matches amount "1")
		(string-equal currency "yen"))
	    (list currency)
	    (list (string-append currency "s"))))))))
  ((and (string-matches name ".*illion.?")
	(string-matches (item.feat token "p.name")
			"[A-Z]*[\\$#][0-9,]+\\(\\.[0-9]+\\)?"))
   nil ;; dealt with on the previous symbol
   )
  ((string-matches name "[1-9][0-9]*/[1-9][0-9]*")
   (let ((numerator (string-before name "/"))
	 (denominator (string-after name "/"))
	 )
     (cond
      ((string-matches name "1/2")
       (list "half"))
      ((string-matches denominator "4")
       (append
	(builtin_english_token_to_words token numerator)
	(list "quarter")
	(if (string-equal numerator "1")
	    (list '((name "'s")(pos nnp)))
	    nil)))
      (t
       (append
	(builtin_english_token_to_words token numerator)
	(begin
	  (item.set_feat token "token_pos" "ordinal")
	  (builtin_english_token_to_words token denominator))
	(if (string-equal numerator "1")
	    nil
	    (list '((name "'s")(pos nnp)))))))))
  ((and (string-matches name "No")
	(item.next token)
        (string-matches (item.feat token "n.name")
			"[0-9]+"))
   (list
    "number"))
  ((string-matches name ".*%$")
   (append
    (token_to_words token (string-before name "%"))
    (list "percent")))
  ((string-matches name "[0-9]+s")  ;; e.g. 1950s
   (item.set_feat token "token_pos" "year")  ;; reasonable guess
   (append
    (builtin_english_token_to_words token (string-before name "s"))
    (list '((name "'s")(pos nnp))) ;; will get assimilated by postlexical rules
   ))
  ((string-matches name "[0-9]+'s")  ;; e.g. 1950's
   (item.set_feat token "token_pos" "year")  ;; reasonable guess
   (append
    (builtin_english_token_to_words token (string-before name "'s"))
    (list '((name "'s")(pos nnp))) ;; will get assimilated by postlexical rules
   ))
  ((and (string-matches name ".*s$")
	(string-equal (item.feat token "punc") "'"))
   ;; potential possessive or may be end of a quote
   (if (token_no_starting_quote token)
       (item.set_feat token "punc" ""))
   (builtin_english_token_to_words token name))
  ((and (string-equal name "A")  ;; letter or determiner
	(or (string-matches (item.feat token "p.name") "[a-z].*")
	    (string-matches (item.feat token "n.name") "[A-Z].*")))
   (list (list '(name "a")(list 'pos token.letter_pos))))
  ((member_string name english_homographs)
   (list (list (list 'name name)
	       (list 'hg_pos (item.feat token "token_pos")))))
  ((string-matches name "__*[^_][^_]*_*_") ;; _emphasis_
   (english_token_to_words
    token
    (remove_leadtrail_underscores name)
   ))
  ((string-matches name "[0-9]?[0-9][:\\.][0-9][0-9][AaPp][Mm]")  ;; time
   ;;  must be am/pm present for . to be acceptable separator
   (let (hours mins half sep (ttime (downcase name)))
     (if (string-matches ttime ".*:.*")
	 (set! sep ":")
	 (set! sep "."))
     (set! hours (string-before ttime sep))
     (set! mins (string-after ttime sep))
     (if (string-matches ttime ".*am")
	 (set! sep "am")
	 (set! sep "pm"))
     (set! mins (string-before mins sep))
     (append
      (builtin_english_token_to_words token hours)
      (cond
       ((string-equal mins "00")
	nil)
       ((string-matches mins "0.")
	(cons
	 "oh"
	 (builtin_english_token_to_words token (string-after mins "0"))))
       (t
	(builtin_english_token_to_words token mins)))
      (if (string-equal sep "am")
	  (builtin_english_token_to_words token "A.M")
	  (builtin_english_token_to_words token "P.M")))))
  ((string-matches name "[0-9]?[0-9]:[0-9][0-9]")  ;; time
   (append
     (builtin_english_token_to_words 
      token (remove_leading_zeros (string-before name ":")))
     (cond
      ((string-equal "00" (string-after name ":"))
	 nil)
      ((string-matches (string-after name ":") "0.")
	(cons
	 "oh"
	 (builtin_english_token_to_words 
	  token 
	  (remove_leading_zeros (string-after name ":")))))
      (t
	 (builtin_english_token_to_words 
	  token 
	  (string-after name ":"))))))
  ((string-matches name "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]")  ;; exact time
   (append
    (builtin_english_token_to_words 
     token (remove_leading_zeros (string-before name ":")))
    (list "hours")
    (builtin_english_token_to_words 
      token (remove_leading_zeros 
		 (string-before (string-after name ":") ":")))
    (list "minutes" "and")
    (builtin_english_token_to_words 
      token (remove_leading_zeros
		 (string-after (string-after name ":") ":")))
    (list "seconds")))
  ((string-matches name "[0-9][0-9]?/[0-9][0-9]?/[0-9][0-9]\\([0-9][0-9]\\)?")
   ;; date, say it as numbers to avoid American/British problem
   (let ((num1 (string-before name "/"))
	 (num2 (string-before (string-after name "/") "/"))
	 (year (string-after (string-after name "/") "/"))
	 day month)
     (item.set_feat token "token_pos" "cardinal")
     (set! day (builtin_english_token_to_words token num1))
     (set! month (builtin_english_token_to_words token num2))
     (item.set_feat token "token_pos" "year")
     (append
      day
      month
      (list '((name ",")(pbreak_scale 0.9)))
      (builtin_english_token_to_words token year))))
  ((string-matches name "[0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]")
   (item.set_feat token "token_pos" "digits")  ;; canonical phone number
   (append
    (builtin_english_token_to_words token (string-before name "-"))
    (list '((name ",")(pbreak_scale 1.0)))
    (builtin_english_token_to_words token (string-after name "-"))))
  ((string-matches name "[0-9]+-[0-9]+-[-0-9]+")
   ;; long distance number 
   (let ((r '(dummy)) (remainder name))
     (item.set_feat token "token_pos" "digits")
     (while (> (length remainder) 0)
       (if (string-matches remainder "[0-9]+")
	   (set! r (append r 
		       (builtin_english_token_to_words 
			token remainder)))
	   (set! r (append r 
			   (builtin_english_token_to_words 
			    token (string-before remainder "-")))))
       (set! remainder (string-after remainder "-"))
       (if (> (length remainder) 0)
	   (set! r (append r (list '((name ",")(pbreak_scale 1.0)))))))
     (cdr r))
   )
  ((and (string-matches name "[0-9][0-9][0-9]")
	(string-matches (item.feat token "n.name")
			"[0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]"))
     (item.set_feat token "token_pos" "digits")
     (builtin_english_token_to_words token name))
  ((string-matches name "[0-9]+-[0-9]+")
   (let ((tokpos))
     (item.set_name token (string-before name "-"))
     (set! tokpos (wagon token 
			 (car (cdr (assoc "[0-9]+" token_pos_cart_trees)))))
     (item.set_feat token "token_pos" (car tokpos))
     (append
      (builtin_english_token_to_words token (string-before name "-"))
      (list "to")
      (builtin_english_token_to_words token (string-after name "-")))))
  ((string-matches name "\\(II?I?\\|IV\\|VI?I?I?\\|IX\\|X[VIX]*\\)")
   ;; Roman numerals
   (let ((tp (item.feat token "token_pos")))
     (cond
      ((string-matches tp "century");; always believe this
       (item.set_feat token "token_pos" "ordinal")
       (if (or (string-equal "1" (tok_rex token))
	       (item.feat token "p.lisp_tok_rex_names"))
	   (append
	    (list "the")
	    (builtin_english_token_to_words 
	     token (tok_roman_to_numstring name)))
	   (builtin_english_token_to_words 
	    token (tok_roman_to_numstring name))))
      ((string-matches name "[IVX]");; be *very* wary of this one
       (if (and (string-equal 
		 "1" (item.feat token "p.lisp_tok_section_name"))
		(string-matches tp "number"))
	   (builtin_english_token_to_words 
	    token (tok_roman_to_numstring name))
	   (tok_string_as_letters name)))
      ((string-matches tp "number")
       (item.set_feat token "token_pos" "cardinal")
       (builtin_english_token_to_words 
	token (tok_roman_to_numstring name)))
      (t;; else its a letter
       (tok_string_as_letters name)))))
  ((and (string-matches name "pp")
	(string-matches (item.feat token "n.name")
			"[0-9]+-[0-9]+"))
   (list "pages"))
  ((and (string-matches name "ss")
	(string-matches (item.feat token "n.name")
			"[0-9]+-[0-9]+"))
   (list "sections"))
  ((string-matches name "_____+")
   (list "line" "of" "underscores"))
  ((string-matches name "=====+")
   (list "line" "of" "equals"))
  ((string-matches name "-----+")
   (list "line" "of" "hyphens"))
  ((string-matches name "\\*\\*\\*\\*\\*+")
   (list "line" "of" "asterisks"))
  ((string-matches name "--+")
   (list '((name ",")(pbreak_scale 1.0))))
  ((string-matches name ".*--+.*")
   (append
    (builtin_english_token_to_words token (string-before name "--"))
    (list '((name ",")(pbreak_scale 1.0)))
    (builtin_english_token_to_words token (string-after name "--"))))
  ((string-matches name "[A-Z][A-Z]?&[A-Z][A-Z]?")
   (append
    (tok_string_as_letters (string-before name "&"))
    (list "and")
    (tok_string_as_letters (string-after name "&"))))
  ((and (string-equal name "Ms")
        (string-matches (item.feat token "n.name") "[A-Z][^A-Z]*"))
   (list "mizz"))
  ((or (string-matches name "[A-Z][A-Z]+s")
       (string-matches name "[BCDEFGHJKLMNOPQRSTVWXYZ]+s"))
   (append
    (builtin_english_token_to_words token (string-before name "s"))
    (list '((name "'s")(pos nnp))) ;; will get assimilated by postlexical rules
    ))
  ((string-matches name "<.*@.*>")  ;; quoted e-mail
   (append 
    (builtin_english_token_to_words
     token (string-after (string-before name "@") "<"))
    (list "at")
    (builtin_english_token_to_words
     token (string-before (string-after name "@") ">"))))
  ((string-matches name ".*@.*")  ;; e-mail
   (append 
    (builtin_english_token_to_words
     token (string-before name "@"))
    (list "at")
    (builtin_english_token_to_words
     token (string-after name "@") ">")))
  ((string-matches name "\\([dD][Rr]\\|[Ss][tT]\\)")
   (if (string-equal (item.feat token "token_pos") "street")
       (if (string-matches name "[dD][rR]")
	   (list "drive")
	   (list "street"))
       (if (string-matches name "[dD][rR]")  ;; default on title side
	   (list "doctor")
	   (list "saint"))))
  ((string-matches name "[Cc]alif")  ;; hopelessly specific ...
   (list 
    "california"))
  (t
   (builtin_english_token_to_words token name))))

;;; This is set as the default
(defvar token_to_words english_token_to_words)

(defvar token.punctuation "\"'`.,:;!?(){}[]"
  "token.punctuation
A string of characters which are to be treated as punctuation when
tokenizing text.  Punctuation symbols will be removed from the text
of the token and made available through the \"punctuation\" feature.
[see Tokenizing]")
(defvar token.prepunctuation "\"'`({["
  "token.prepunctuation
A string of characters which are to be treated as preceding punctuation
when tokenizing text.  Prepunctuation symbols will be removed from the text
of the token and made available through the \"prepunctuation\" feature.
[see Tokenizing]")
(defvar token.whitespace " \t\n\r"
  "token.whitespace
A string of characters which are to be treated as whitespace when
tokenizing text.  Whitespace is treated as a separator and removed
from the text of a token and made available through the \"whitespace\"
feature.  [see Tokenizing]")
(defvar token.singlecharsymbols ""
  "token.singlecharsymbols
Characters which have always to be split as tokens.  This would be
usual is standard text, but is useful in parsing some types of
file. [see Tokenizing]")

(defvar token.letter_pos 'nn
  "token.letter_pos
The part of speech tag (valid for your part of speech tagger) for
individual letters.  When the tokenizer decide to pronounce a token
as a list of letters this tag is added to each letter in the list.  
Note this should be from the part of speech set used in your tagger 
which may not be the same one that appears in the actual lexical 
entry (if you map them afterwards).  This specifically allows \"a\"
to come out as ae rather than @.")

(defvar token.unknown_word_name "unknown"
  "token.unknown_word_name
When all else fails and a pronunciation for a word or character can't
be found this word will be said instead.  If you make this \"\" them
the unknown word will simple be omitted.  This will only
really be called when there is a bug in the lexicon and characters
are missing from the lexicon.  Note this word should be in the lexicon.")

(def_feature_docstring
  'Token.punc
  "Token.punc
Succeeding punctuation symbol found after token in original 
string/file.")
(def_feature_docstring
  'Token.whitespace
  "Token.whitespace
Whitespace found before token in original string/file.")
(def_feature_docstring
  'Token.prepunctuation
  "Token.prepunctuation
Preceeding puctuation symbol found before token in original string/file.")

(require 'tokenpos)
;;;
;;;  Token pos are gross level part of speech tags which help decide
;;;  pronunciation of tokens (particular expansion of Tokens into words)
;;;  The most obvious example is identifying number types (ordinals,
;;;  years, digits or numbers).
;;;
(defvar english_token_pos_cart_trees
  '(
    ;;  Format is (Regex Tree)
    ("[0-9]+" 
     ((lisp_num_digits < 3.8)
      ((p.lisp_token_pos_guess is month)
       ((lisp_month_range is 0) ((year)) ((ordinal)))
       ((n.lisp_token_pos_guess is month)
	((lisp_month_range is 0) ((cardinal)) ((ordinal)))
	((n.lisp_token_pos_guess is numeric)
	 ((lisp_num_digits < 2)
	  ((p.lisp_token_pos_guess is numeric)
	   ((pp.lisp_token_pos_guess is sym) ((digits)) ((cardinal)))
	   ((cardinal)))
	  ((nn.lisp_token_pos_guess is sym) ((cardinal)) ((digits))))
	 ((lisp_num_digits < 2)
	  ((nn.lisp_token_pos_guess is numeric)
	   ((n.lisp_token_pos_guess is sym)
	    ((lisp_month_range is 0) ((digits)) ((cardinal)))
	    ((cardinal)))
	   ((cardinal)))
	  ((name < 302.3)
	   ((p.lisp_token_pos_guess is flight)
	    ((digits))
	    ((n.lisp_token_pos_guess is sym)
	     ((p.lisp_token_pos_guess is sym) ((digits)) ((cardinal)))
	     ((cardinal))))
	   ((p.lisp_token_pos_guess is a)
	    ((digits))
	    ((n.lisp_token_pos_guess is sym)
	     ((nn.lisp_token_pos_guess is sym)
	      ((name < 669.2) ((digits)) ((cardinal)))
	      ((cardinal)))
	     ((name < 373.2)
	      ((cardinal))
	      ((name < 436.2)
	       ((name < 392.6) ((digits)) ((cardinal)))
	       ((name < 716.5)
		((cardinal))
		((name < 773.6)
		 ((p.lisp_token_pos_guess is _other_) ((digits)) ((cardinal)))
		 ((cardinal)))))))))))))
      ((p.lisp_token_pos_guess is numeric)
       ((pp.lisp_token_pos_guess is month)
	((year))
	((nn.lisp_token_pos_guess is numeric) ((cardinal)) ((digits))))
       ((nn.lisp_token_pos_guess is numeric)
	((n.lisp_token_pos_guess is month)
	 ((cardinal))
	 ((n.lisp_token_pos_guess is numeric)
	  ((digits))
	  ((p.lisp_token_pos_guess is _other_) ((cardinal)) ((year)))))
	((p.lisp_token_pos_guess is _other_)
	 ((lisp_num_digits < 4.4)
	  ((name < 2959.6)
	   ((name < 1773.4) ((cardinal)) ((year)))
	   ((cardinal)))
	  ((pp.lisp_token_pos_guess is _other_) ((digits)) ((cardinal))))
	 ((n.lisp_token_pos_guess is to)
	  ((year))
	  ((p.lisp_token_pos_guess is sym)
	   ((pp.lisp_token_pos_guess is sym)
	    ((cardinal))
	    ((lisp_num_digits < 4.6) ((year)) ((digits))))
	   ((lisp_num_digits < 4.8)
	    ((name < 2880)
	     ((name < 1633.2)
	      ((name < 1306.4) ((cardinal)) ((year)))
	      ((year)))
	     ((cardinal)))
	    ((cardinal)))))))))
     )
    ("\\(II?I?\\|IV\\|VI?I?I?\\|IX\\|X[VIX]*\\)";; Roman numerals
     ((p.lisp_tok_rex_names is 0)
      ((lisp_num_digits is 5)
       ((number))
       ((lisp_num_digits is 4)
	((number))
	((nn.lisp_num_digits is 13)
	 ((number))
	 ((p.lisp_num_digits is 7)
	  ((number))
	  ((p.lisp_tok_section_name is 0)
	   ((lisp_tok_rex is 0)
	    ((lisp_num_digits is 3)
	     ((p.lisp_num_digits is 4)
	      ((number))
	      ((nn.lisp_num_digits is 4)
	       ((number))
	       ((n.lisp_num_digits is 4)
		((number))
		((pp.lisp_num_digits is 3)
		 ((number))
		 ((p.lisp_num_digits is 2)
		  ((letter))
		  ((nn.lisp_num_digits is 2)
		   ((letter))
		   ((n.cap is 0) ((letter)) ((number)))))))))
	     ((nn.lisp_num_digits is 11)
	      ((letter))
	      ((lisp_num_digits is 1)
	       ((pp.lisp_num_digits is 9)
		((letter))
		((p.lisp_num_digits is 9)
		 ((letter))
		 ((n.lisp_num_digits is 6)
		  ((letter))
		  ((pp.lisp_num_digits is 6)
		   ((letter))
		   ((pp.cap is 0)
		    ((n.cap is 0)
		     ((p.lisp_num_digits is 1)
		      ((letter))
		      ((n.lisp_num_digits is 4) ((letter)) ((letter))))
		     ((letter)))
		    ((letter)))))))
	       ((p.lisp_num_digits is 10)
		((number))
		((n.lisp_num_digits is 8)
		 ((number))
		 ((pp.lisp_num_digits is 9)
		  ((number))
		  ((nn.lisp_num_digits is 5)
		   ((number))
		   ((n.lisp_num_digits is 4) ((number)) ((letter))))))))))
	    ((letter)))
	   ((number)))))))
      ((century))))
    ("\\([dD][Rr]\\|[Ss][tT]\\)"
     ((n.name is 0)
      ((p.cap is 1)
       ((street))
       ((p.name matches "[0-9]*\\(1[sS][tT]\\|2[nN][dD]\\|3[rR][dD]\\|[0-9][tT][hH]\\)")
	((street))
	((title))))
      ((punc matches ".*,.*")
       ((street))
       ((p.punc matches ".*,.*")
	((title))
	((n.cap is 0)
	 ((street))
	 ((p.cap is 0)
	  ((p.name matches "[0-9]*\\(1[sS][tT]\\|2[nN][dD]\\|3[rR][dD]\\|[0-9][tT][hH]\\)")
	   ((street))
	   ((title)))
	  ((pp.name matches "[1-9][0-9]+")
	   ((street))
	   ((title)))))))))
    ("lead"
     ((p.name in (was were had been having has is are))
      ((led))
      ((liid))))
    ("read"
     ((p.name in (to))
      ((riid))
      ((red))))
    ))

(defvar english_homographs
  '("lead" "read")
  "english_homographs
A list of tokens that are dealt with by a homograph disambiguation tree
in english_token_pos_cart_trees.")

(defvar token_pos_cart_trees
  english_token_pos_cart_trees
  "token_pos_cart_trees
This is a list of pairs or regex plus CART tree.  Tokens that match
the regex will have the CART tree aplied, setting the result as
the token_pos feature on the token.  The list is checked in order
and only the first match will be applied.")

(provide 'token)
