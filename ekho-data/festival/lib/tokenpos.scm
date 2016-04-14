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
;;;  Functions used in identifying token types.
;;;

(defvar token_most_common
'(
sym numeric month to day in the of on and writes a years from
for jst at million by is was gmt page he that than more since as when
with but after about or his i has it date no died number bst who miles
university some people an only w year have ago were are pages up days
months hours minutes through out had which least hi last now ft this
all one its there between cents until over will before past they
nearly times tim message so lbs just if age we during she billion then
other be time new her first states not you members under would many
says degrees two next fax week while bush been around including back
campaign american within publisher flight points even early later
world countries every edt can president most could their what them
former began women killed another also received long americans pounds
do dear said km made into did dead war tel still old x took total men
like f am less c well late down weeks end chapter among place house
away him election death almost students state soviet where version
summer man s nation because washington top though m id est these spent
seats gnu estimated those lost ian high each copies children acres
tons son per my found won off seconds power nations federal born
presidential much city begin p name different whose three home hello)

"token_most_common
A list of (English) words which were found to be most common in 
an text database and are used as discriminators in token analysis.")

(define (token_pos_guess sc)
"(tok_pos sc)
Returns a general pos for sc's name. 
  numeric   All digits
  number    float or comma'd numeric
  sym       Contains at least one non alphanumeric
  month     has month name (or abbrev)
  day       has day name (or abbrev)
  rootname  else downcased alphabetic.
Note this can be used to find token_pos but isn't used directly as
its not disciminatory enough."
  (let ((name (downcase (item.name sc))))
    (cond
     ((string-matches name "[0-9]+")
      'numeric)
     ((or (string-matches name "[0-9]+\\.[0-9]+")
	  (string-matches name 
	     "[0-9][0-9]?[0-9]?,\\([0-9][0-9][0-9],\\)*[0-9][0-9][0-9]"))
      'number)
     ((string-matches name ".*[^A-Za-z0-9].*")
      'sym)
     ((member_string name '(jan january feb february mar march 
   			    apr april may jun june
			    jul july aug august sep sept september
			    oct october nov november dec december))
      'month)
     ((member_string name '(sun sunday mon monday tue tues tuesday 
			    wed wednesday thu thurs thursday 
			    fri friday sat saturday))
      'day)
     ((member_string name token_most_common)
      name)
     (t
      '_other_))))

(define (token_no_starting_quote token)
  "(token_no_starting_quote TOKEN)
Check to see if a single quote (or backquote) appears as prepunctuation
in this token or any previous one in this utterance.  This is used to
disambiguate ending single quote as possessive or end quote."
  (cond
   ((null token)
    t)
   ((string-matches (item.feat token "prepunctuation") "[`']")
    nil)
   (t
    (token_no_starting_quote (item.relation.prev token "Token")))))

(define (token_zerostart sc)
"(zerostart sc)
Returns, 1 if first char of sc's name is 0, 0 otherwise."
  (if (string-matches (item.name sc) "^0.*")
      "1"
      "0"))

(define (tok_roman_to_numstring roman)
  "(tok_roman_to_numstring ROMAN)
Takes a string of roman numerals and converts it to a number and
then returns the printed string of that.  Only deals with numbers up to 50."
  (let ((val 0) (chars (symbolexplode roman)))
    (while chars
     (cond
      ((equal? (car chars) 'X)
       (set! val (+ 10 val)))
      ((equal? (car chars) 'V)
       (set! val (+ 5 val)))
      ((equal? (car chars) 'I)
       (cond
	((equal? (car (cdr chars)) 'V)
	 (set! val (+ 4 val))
	 (set! chars (cdr chars)))
	((equal? (car (cdr chars)) 'X)
	 (set! val (+ 9 val))
	 (set! chars (cdr chars)))
	(t
	 (set! val (+ 1 val))))))
     (set! chars (cdr chars)))
    (format nil "%d" val)))

(define (num_digits sc)
"(num_digits SC)
Returns number of digits (actually chars) is SC's name."
  (string-length (format nil "%s" (item.name sc))))

(define (month_range sc)
"(month_range SC)
1 if SC's name is > 0 and < 32, 0 otherwise."
  (let ((val (parse-number (item.name sc))))
    (if (and (> val 0) (< val 32))
	"1"
	"0")))

(define (remove_leading_zeros name)
  "(remove_leading_zeros name)
Remove leading zeros from given string."
  (let ((nname name))
    (while (string-matches nname "^0..*")
	   (set! nname (string-after nname "0")))
    nname))

(define (token_money_expand type)
"(token_money_expand type)
Convert shortened form of money identifier to words if of a known type."
  (cond
   ((string-equal type "HK")
    (list "Hong" "Kong"))
   ((string-equal type "C")
    (list "Canadian"))
   ((string-equal type "A")
    (list "Australian"))
   ((< (length type) 4)
    (mapcar
     (lambda (letter)
       (list (list 'name letter)
	     (list 'pos token.letter_pos)))
     (symbolexplode type)))
   (t
    (list type))))

(define (find_month_from_number token string-number)
  "(find_month_from_number token string-number)
Find the textual representation of the month from the given string number"
  (let ((nnum (parse-number string-number)))
    (cond
     ((equal? 1 nnum) (list "January"))
     ((equal? 2 nnum) (list "February"))
     ((equal? 3 nnum) (list "March"))
     ((equal? 4 nnum) (list "April"))
     ((equal? 5 nnum) (list "May"))
     ((equal? 6 nnum) (list "June"))
     ((equal? 7 nnum) (list "July"))
     ((equal? 8 nnum) (list "August"))
     ((equal? 9 nnum) (list "September"))
     ((equal? 10 nnum) (list "October"))
     ((equal? 11 nnum) (list "November"))
     ((equal? 12 nnum) (list "December"))
     (t
      (cons "month"
	    (builtin_english_token_to_words token string-number))))))
      
(define (tok_allcaps sc)
  "(tok_allcaps sc)
Returns 1 if sc's name is all capitals, 0 otherwise"
  (if (string-matches (item.name sc) "[A-Z]+")
      "1"
      "0"))

(define (tok_section_name sc)
  "(tok_section_name sc)
Returns 1 if sc's name is in list of things that are section/chapter
like."
  (if (member_string
       (downcase (item.name sc))
       '(chapter section part article phrase verse scene act book 
		 volume chap sect art vol war fortran saturn
		 trek))
      "1"
      "0"))

(define (tok_string_as_letters name)
  "(tok_string_as_letters NAME)
Return list of letters marked as letter part of speech made
by exploding NAME."
  (mapcar
   (lambda (letter)
     (list (list 'name letter)
	   (list 'pos token.letter_pos)))
   (symbolexplode name)))

(define (tok_rex sc)
  "(tok_rex sc)
Returns 1 if King like title is within 3 tokens before or 2 after."
  (let ((kings '(king queen pope duke tsar emperor shah ceasar
		      duchess tsarina empress baron baroness
		      count countess)))
    (if (or (member_string 
	     (downcase (item.feat sc "R:Token.pp.name"))
	     kings)
	    (member_string 
	     (downcase (item.feat sc "R:Token.pp.p.name"))
	     kings)
	    (member_string 
	     (downcase (item.feat sc "R:Token.n.name"))
	     kings))
	"1"
	"0")))

(define (tok_rex_names sc)
  "(tok_rex sc)
Returns 1 if this is a King-like name."
  (if (and
       (member_string
        (downcase (item.name sc))
        '(louis henry charles philip george edward pius william richard
                ptolemy john paul peter nicholas
                alexander frederick james alfonso ivan napolean leo 
                gregory catherine alexandria pierre elizabeth mary))
       (or (string-equal "" (item.feat sc "punc"))
           (string-equal "0" (item.feat sc "punc"))))
      "1"
      "0"))

(provide 'tokenpos)
