;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                      JUNTA DE ANDALUCÕA                             ;;;
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
;;;  Standard Spanish male voice                                        ;;;
;;;                                                                     ;;;
;;;  Contractor: ConsejerÌa de InnovaciÛn, Ciencia y Empresa            ;;;
;;;              de la Junta de AndalucÌa                               ;;;
;;;                                                                     ;;;
;;;  Developed by: MP Sistemas and                                      ;;;
;;;                Intelligent Dialogue Systems S.L. (INDISYS)          ;;;
;;;                                                                     ;;;
;;;  Authors:   Del Solar, Carmen <c.delsolar@indisys.es>               ;;;
;;;             Gonz·lez, Jes˙s   <j.gonzalez@indisys.es>               ;;;
;;;             ManchÛn, Pilar    <p.manchon@indisys.es>                ;;;
;;;             MartÌn, Antonio   <amam@mpsistemas.es>                  ;;;
;;;             MartÌnez, Diego   <d.martinez@indisys.es>               ;;;
;;;             PÈrez, Guillermo  <g.perez@indisys.es>                  ;;;
;;;             Varela, VÌctor    <vmvr@mpsistemas.es>                  ;;;
;;;                                                                     ;;;
;;;  Voice Talent:  Pedro Alonso                                        ;;;
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
;;;
;;; Tokenizer for Spanish
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Load any other required files

;;; Voice/es token_to_word rules 

(define (spanish_number name)
"(spanish_number name)
Convert a string of digits into a list of words saying the number."
  (if (string-matches name "0")
      (list "cero")
      (spanish_number_from_digits (symbolexplode name))))

(define (just_zeros digits)
"(just_zeros digits)
If this only contains 0s then we just do something different."
 (cond
  ((not digits) t)
  ((string-equal "0" (car digits))
   (just_zeros (cdr digits)))
  (t nil)))

(define (spanish_number_from_digits digits)
  "(spanish_number_from_digits digits)
Takes a list of digits and converts it to a list of words
saying the number."
  (let ((l (length digits)))
    (cond
     ((equal? l 0)
      nil)
     ((string-equal (car digits) "0")
      (spanish_number_from_digits (cdr digits)))
     ((equal? l 1);; single digit
      (cond 
       ((string-equal (car digits) "0") (list "cero"))
       ((string-equal (car digits) "1") (list "un"))
       ((string-equal (car digits) "2") (list "dos"))
       ((string-equal (car digits) "3") (list "tres"))
       ((string-equal (car digits) "4") (list "cuatro"))
       ((string-equal (car digits) "5") (list "cinco"))
       ((string-equal (car digits) "6") (list "seis"))
       ((string-equal (car digits) "7") (list "siete"))
       ((string-equal (car digits) "8") (list "ocho"))
       ((string-equal (car digits) "9") (list "nueve"))
       ;; fill in the rest
       (t (list "equis"))));; $$$ what should say?
     ((equal? l 2);; less than 100
      (cond
       ((string-equal (car digits) "0");; 0x
	(spanish_number_from_digits (cdr digits)))
     
       ((string-equal (car digits) "1");; 1x
	(cond
	 ((string-equal (car (cdr digits)) "0") (list "diez"))
	 ((string-equal (car (cdr digits)) "1") (list "once"))
	 ((string-equal (car (cdr digits)) "2") (list "doce"))
	 ((string-equal (car (cdr digits)) "3") (list "trece"))
	 ((string-equal (car (cdr digits)) "4") (list "catorce"))
	 ((string-equal (car (cdr digits)) "5") (list "quince"))
	 (t 
	  (cons "dieci" (spanish_number_from_digits (cdr digits))))))
     
       ((string-equal (car digits) "2");; 2x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "veinte")
	    (cons "venti" (spanish_number_from_digits (cdr digits)))))

       ((string-equal (car digits) "3");; 3x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "treinta")
	    (cons "trentai" (spanish_number_from_digits (cdr digits)))))

       ((string-equal (car digits) "4");; 4x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "cuarenta")
	    (cons "cuarentai" (spanish_number_from_digits (cdr digits)))))

       ((string-equal (car digits) "5");; 5x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "cincuenta")
	    (cons "cincuentai" (spanish_number_from_digits (cdr digits)))))

       ((string-equal (car digits) "6");; 6x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "sesenta")
	    (cons "sesentai" (spanish_number_from_digits (cdr digits)))))

       ((string-equal (car digits) "7");; 7x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "setenta")
	    (cons "setentai" (spanish_number_from_digits (cdr digits)))))

       ((string-equal (car digits) "8");; 8x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "ochenta")
	    (cons "ochentai" (spanish_number_from_digits (cdr digits)))))

       ((string-equal (car digits) "9");; 9x
	(if (string-equal (car (cdr digits)) "0") 
	    (list "noventa")
	    (cons "noventai" (spanish_number_from_digits (cdr digits)))))

       ))

     ((equal? l 3);; in the hundreds
      (cond 
     
       ((string-equal (car digits) "1");; 1xx
	(if (just_zeros (cdr digits)) (list "cien")
	    (cons "ciento" (spanish_number_from_digits (cdr digits)))))

       ((string-equal (car digits) "5");; 5xx
	(cons "quinientos" (spanish_number_from_digits (cdr digits))))

       ((string-equal (car digits) "7");; 7xx
	(cons "setecientos" (spanish_number_from_digits (cdr digits))))

       ((string-equal (car digits) "9");; 9xx
	(cons "novecientos" (spanish_number_from_digits (cdr digits))))

       (t;; ?xx
	(append (spanish_number_from_digits (list (car digits))) 
		(list "cientos") 
		(spanish_number_from_digits (cdr digits))))
       ))

     ((< l 7)
      (let ((sub_thousands 
	     (list 
	      (car (cdr (cdr (reverse digits))))
	      (car (cdr (reverse digits)))
	      (car (reverse digits))))
	    (thousands (reverse (cdr (cdr (cdr (reverse digits)))))))
	(set! x (spanish_number_from_digits thousands))
	(append
	 (if (string-equal (car x) "un") nil x)
	 (list "mil")
	 (spanish_number_from_digits sub_thousands))))

     ((< l 13)
      (let ((sub_million 
	     (list 
	      (car (cdr (cdr (cdr (cdr (cdr(reverse digits)))))))
	      (car (cdr (cdr (cdr (cdr (reverse digits))))))
	      (car (cdr (cdr (cdr (reverse digits)))))
	      (car (cdr (cdr (reverse digits))))
	      (car (cdr (reverse digits)))
	      (car (reverse digits))
	      ))
	    (millions (reverse (cdr (cdr (cdr (cdr (cdr (cdr (reverse digits))))))))))
	(set! x (spanish_number_from_digits millions))
	(append
	 (if (string-equal (car x) "un") 
	     (list "un" "millon") 
	     (append x (list "millones")))
	 (spanish_number_from_digits sub_million))))

     (t
      (list "tropecientos")))))

(define (get_symbol_of_word name say_symbols not_say_symbols)
"(get_symbol_of_word name say_symbols not_say_symbols)
 Creates a list containing all the say_symbols and not_say_symbols that appear in name
 Example: name-> (diego-carmen.prueba@hotmail.com)
	  symbols_to_say -> (\"_.@\")
 	  symbols_not_to_say -> (-)
  	  OUT-> ( (position symbol say_or_not_say) (position symbol say_or_not_say)...)"

  (let (list_out)
    (set! list_out nil)
    (mapcar
      (lambda (symbol_character)
        (set! symbol_position 0)
          (mapcar
	    (lambda (name_character)
              (let (symbol_match_string)
	     ;;get the symbol_match_string to search in name the say_symbols elements
              (cond 
                (t (set! symbol_match_string symbol_character)))
  	        (if (string-equal name_character symbol_match_string)
	  	   (set! list_out (append list_out (list(list symbol_position symbol_character "say"))))
                   (list nil))
	      (set! symbol_position (+ 1 symbol_position))
	     ))
	  (symbolexplode name)))
    (symbolexplode say_symbols))

    (mapcar
      (lambda (symbol_character)
        (set! symbol_position 0)
          (mapcar
	    (lambda (name_character)
              (let (symbol_match_string)
	     ;;get the symbol_match_string to search inname the say_symbols elements 
              (cond 
                (t (set! symbol_match_string symbol_character)))
  	        (if (string-equal name_character symbol_match_string)
	  	   (set! list_out (append list_out (list(list symbol_position symbol_character "not_say"))))
                   (list nil))
	      (set! symbol_position (+ 1 symbol_position))
	     ))
	  (symbolexplode name)))
    (symbolexplode not_say_symbols))
    list_out)
)

(define (insert_element_in position input_list element_to_insert)
"(insert_element_in position input_list element_to_insert)
 Inserts element_to_insert, with its position, in parameter position, from input_list
 Note: element_to_insert must be as follow: (position symbol say_or_not_say)
       first element is 0."
  (let (return_list)
    (if (equal? 0 position)
       (set! return_list (append (list element_to_insert) input_list))
       (let (actual_position)
	 (set! actual_position 0)
         (mapcar
           (lambda (l)
	     (if (equal? actual_position position)
	       (set! return_list (append return_list (list element_to_insert))))
	     (set! return_list (append return_list (list l)))
	     (set! actual_position (+ 1 actual_position))
           )
         input_list)))
     
   return_list)
)	
     
(define (sort_symbol_of_word symbols_list)
 "(sort_symbol_of_word symbols_list)
  Sorts symbols_list using position number of data list. 
  Ascending order."
  (let (temp_list orig_position sorted_position sorted_list index_orig_list flag_final_insertion previous_element_orig_list
flag_insertion_done)

    (set! orig_list (cdr symbols_list))
    (set! sorted_list (list(car symbols_list)))

    (mapcar
      (lambda (element_orig_list)
	(set! index_orig_list 0)
 	(if flag_final_insertion 
	  (set! sorted_list (append sorted_list (list previous_element_orig_list))))
	(set! flag_final_insertion t)
	(set! flag_insertion_done "no")
        (mapcar
 	  (lambda (element_sorted_list)
  	    (set! previous_element_orig_list element_orig_list)
	    (set! orig_position (car element_orig_list))
	    (set! sorted_position (car element_sorted_list))		
	    (if (and (< orig_position sorted_position)
		     (string-equal flag_insertion_done "no"))
	      (let ()			
	        (set! sorted_list (insert_element_in index_orig_list sorted_list element_orig_list))
	        (set! flag_insertion_done "yes")
	        (set! flag_final_insertion nil)))

	    (set! index_orig_list (+ 1 index_orig_list))

	  )
	 sorted_list))
    orig_list)
    ;;add the last element
    (if flag_final_insertion 
	  (set! sorted_list (append sorted_list (list previous_element_orig_list))))

    sorted_list)
) 

(define (get_number_and_string name)
"
  Takes a mixed entry (string+number), spells the string part and converts digits into number
  Example: IN-> name = \"carmen54\"
	   OUT-> (c a r m e n \"cincuentai\" \"cuatro\")
"
  (let (subwords previous_word previous_number )
    (set! previous_number nil)
    (set! previous_word nil)
    (set! flag_previous_state nil)
    (mapcar
       (lambda (letter)
	 (cond
	   ((string-matches letter "[0-9]")
             (if (string-equal flag_previous_state "word_found")
	        (set! subwords (append subwords previous_word)))
	     (set! previous_word nil)
	     (if previous_number
       	       (set! previous_number (string-append previous_number letter))
	       (set! previous_number letter))
             (set! flag_previous_state "number_found"))
	   ((string-matches letter "['a-zA-Z¡…Õ”⁄‹—·ÈÌÛ˙¸Ò~]")
             (if (string-equal flag_previous_state "number_found")
	         (set! subwords (append subwords (spanish_number previous_number))))
	     (set! previous_number nil)
	     (set! previous_word (append previous_word (list letter)))

             (set! flag_previous_state "word_found"))
	   (t 
		(set! subwords (append subwords (list "ups")))))
	)
    (symbolexplode name))

     (if previous_word
      (set! subwords (append subwords previous_word)))

     (if previous_number
      (set! subwords (append subwords (spanish_number previous_number)))) 
  
  subwords)
)

(define (expand_as_words_list_with_symbol name symbols_to_say symbols_not_to_say)
  "(expand_as_words_list_with_symbol name symbols_to_say symbols_not_to_say)
   Returns a list of words falling to the right and left of symbols_to_say and symbols_not_to_say, plus symbols in symbols_to_say
   Example: input: name        -> diego_carmen.prueba
	    symbols_to_say     -> (_)
            symbols_not_to_say -> (.)
	    output_list	       -> (\"diego\" \"_\" \"carmen\" \"prueba\" " 
  (set! symbols_into_name_not_sorted (get_symbol_of_word name symbols_to_say symbols_not_to_say))
  (set! symbols_into_name_sorted (sort_symbol_of_word symbols_into_name_not_sorted))

  (set! out_name_listed nil)
  
  (if (not (string-equal name ""))
    (let (out_name_listed)
      (if (car symbols_into_name_sorted)
        (let ()
          (mapcar
             (lambda (element_symbol)
	       (set! symbol (car (cdr element_symbol)))
	       (set! flag_say (car (reverse element_symbol)))
	       (if (not (string-equal (string-before name symbol) ""))
		 (cond
 		    ((string-matches (string-before name symbol) "[0-9]+")   
    		       (set! out_name_listed (append out_name_listed (spanish_number (string-before name symbol)))))
		    ((string-matches (string-before name symbol) "['a-zA-Z¡…Õ”⁄‹—·ÈÌÛ˙¸Ò~]+")
	              (set! out_name_listed (append out_name_listed (list (string-before name symbol)))))
		    (t
    		       (set! out_name_listed (append out_name_listed (get_number_and_string (string-before name symbol)))))))
	       (if (string-equal flag_say "say")
	       (set! out_name_listed (append out_name_listed (list (string-append "" symbol)))))
	      (set! name (string-after name symbol))
	     )	       
          symbols_into_name_sorted)
       (if (not (string-equal name ""))
 	(cond
 	  ((string-matches name "[0-9]+")
 	     (set! out_name_listed (append out_name_listed (spanish_number name))))
          ((string-matches name "['a-zA-Z¡…Õ”⁄‹—·ÈÌÛ˙¸Ò~]+")
	     (set! out_name_listed (append out_name_listed (list name))))
	  (t
             (set! out_name_listed (append out_name_listed (get_number_and_string name)))))))

	(cond
 	  ((string-matches name "[0-9]+")
 	     (set! out_name_listed (append out_name_listed (spanish_number name))))
          ((string-matches name "['a-zA-Z¡…Õ”⁄‹—·ÈÌÛ˙¸Ò~]+")
	     (set! out_name_listed (append out_name_listed (list name))))
	  (t
             (set! out_name_listed (append out_name_listed (get_number_and_string name)))))
    )
   out_name_listed)
  ())
)

(define (find_spanish_month_from_number string-number)
  "(find_spanish_month_from_number string-number)
  Find the textual representation of the month from the given string number"
  (let ((nnum (parse-number string-number)))
    (cond
     ((equal? 1 nnum) (list "Enero"))
     ((equal? 2 nnum) (list "Febrero"))
     ((equal? 3 nnum) (list "Marzo"))
     ((equal? 4 nnum) (list "Abril"))
     ((equal? 5 nnum) (list "Mayo"))
     ((equal? 6 nnum) (list "Junio"))
     ((equal? 7 nnum) (list "Julio"))
     ((equal? 8 nnum) (list "Agosto"))
     ((equal? 9 nnum) (list "Septiembre"))
     ((equal? 10 nnum) (list "Octubre"))
     ((equal? 11 nnum) (list "Noviembre"))
     ((equal? 12 nnum) (list "Diciembre"))
     (t
      (cons "mes"
	    (spanish_number string-number))))))


(define (JuntaDeAndalucia_es_pa::token_to_words token name)
  "(JuntaDeAndalucia_es_pa::token_to_words token name)
Specific token to word rules for the voice JuntaDeAndalucia_es_pa.  Returns a list
of words that expand given token with name."
  (cond
  ;; Indisys_MP: time must be am/pm present for . to be acceptable separator
  ((string-matches name "[0-9]?[0-9][:\\.][0-9][0-9][AaPp][Mm]")  
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
       (spanish_number hours)
      (cond
       ((string-equal mins "00")
	nil)
       ((string-matches mins "0.")
	(cons
	 "cero"
	 (spanish_number (string-after mins "0"))))
       (t
	(spanish_number mins)))
      (if (string-equal sep "am")
	(list(list(list 'name 'A) (list 'pos 'nn)) (list(list 'name 'M) (list 'pos 'nn)))
	(list(list(list 'name 'P) (list 'pos 'nn)) (list(list 'name 'M) (list 'pos 'nn)))))))

  ;; Indisys_MP: time without am or pm
  ((string-matches name "[0-9]?[0-9]:[0-9][0-9]")  
   (append
     (spanish_number (string-before name ":"))
     (cond
      ((string-equal "00" (string-after name ":"))
	 nil)
      ((string-matches (string-after name ":") "0.")
	(cons
	 "cero"
	 (spanish_number (string-after name ":"))))
      (t
	 (spanish_number (string-after name ":"))))))

  ;; Indisys_MP: exact time
  ((string-matches name "[0-9]?[0-9]:[0-9][0-9]:[0-9][0-9]")  
   (append
    (spanish_number (string-before name ":"))
    (list "horas")
    (spanish_number (string-before (string-after name ":") ":"))
    (list "minutos" "y")
    (spanish_number (string-after (string-after name ":") ":"))
    (list "segundos")))

  ;; Indisys_MP: date, 
  ((string-matches name "[0-9][0-9]?/[0-9][0-9]?/[0-9][0-9]\\([0-9][0-9]\\)?")
   ;; date, say it as numbers to avoid American/British problem
   (let ((num1 (string-before name "/"))
	 (num2 (string-before (string-after name "/") "/"))
	 (year (string-after (string-after name "/") "/"))
	 day month)
     (set! day (spanish_number num1))
     (set! month (find_spanish_month_from_number num2))
     (append
      day
      (list "de")
      month
      (list "del")
;      (list '((name ",")(pbreak_scale 0.9)))
      (spanish_number year))))

  ;; Indisys_MP: quoted e-mail
  ((string-matches name "<.*@.*>")
   (set! symbols_not_to_say "")
   (set! symbols_to_say "*%&$§@#+~=/\\_|><[]{}\"'`.,;:°!ø?-™∫()")
   (append 
    (expand_as_words_list_with_symbol (string-after (string-before name "@") "<") symbols_to_say symbols_not_to_say)
    (list "@")
    (expand_as_words_list_with_symbol (string-before (string-after name "@") ">") symbols_to_say symbols_not_to_say)))

  ;; Indisys_MP: e-mail without quoted
  ((string-matches name ".*@.*")
   (set! symbols_not_to_say "")
   (set! symbols_to_say "*%&$§@#+~=/\\_|><[]{}\"'`.,;:°!ø?-™∫()")
   (append 
    (expand_as_words_list_with_symbol (string-before name "@") symbols_to_say symbols_not_to_say)
    (list "@")
    (expand_as_words_list_with_symbol (string-after name "@") symbols_to_say symbols_not_to_say)))

  ;;Indisys_MP: line definition
  ((string-matches name "_____+")
   (list "lÌnea" "de" "guiones" "bajos"))
  ((string-matches name "=====+")
   (list "lÌnea" "de" "iguales"))
  ((string-matches name "-----+")
   (list "lÌnea" "de" "guiones" "medios"))
  ((string-matches name "\\*\\*\\*\\*\\*+")
   (list "lÌnea" "de" "asteriscos"))

   ;;Indisys_MP: URL's
   ((string-matches name "http://.*")
    (set! symbols_not_to_say "")
    (set! symbols_to_say "*%&$§@#+~=/\\_|><[]{}\"'`.,;:°!ø?-™∫()")
    (append
     (list "http" ":" "barra" "barra")
     (expand_as_words_list_with_symbol (string-after name "http://") symbols_to_say symbols_not_to_say)))
   ((string-matches name "www..*")
    (set! symbols_not_to_say "")
    (set! symbols_to_say "*%&$§@#+~=/\\_|><[]{}\"'`.,;:°!ø?-™∫()")
    (expand_as_words_list_with_symbol name symbols_to_say symbols_not_to_say))    

   ;;Indisys_MP: numbers
   ((string-matches name "[1-9][0-9]+")
    (spanish_number name))

   ;;Indisys_MP: say "y" when found &. 
;   ((string-matches name "[A-Z][A-Z]?&[A-Z][A-Z]?")
;   (set! symbols_not_to_say "\"'`.,;:°!ø?-™∫[]{}()")
;   (set! symbols_to_say "*%&$§@#+^~=/\\_|><")
;   (append
;    (expand_as_words_list_with_symbol (string-before name "&") symbols_to_say symbols_not_to_say)
;    (list "y")
;    (expand_as_words_list_with_symbol (string-after name "&") symbols_to_say symbols_not_to_say)))

   ;;Indisys_MP: only one symbol or more without numbers or characters.
   ((string-matches name "[-*%&§@#+\^~=/\\_|<>(){}\"'`.,;:!°ø?∫™]+")
    (let ()
      (set! symbols_not_to_say "")
      (set! symbols_to_say "*%&$§@#+~=/\\_|><[]{}\"'`.,;:°!ø?-™∫()")
      (expand_as_words_list_with_symbol name symbols_to_say symbols_not_to_say)))
   
   ;;Indisys_MP: strings (character or number) with symbols
   ((string-matches name ".*[-*%&§@#+\^~=/\\_|<>(){}\"'`.,;:!°ø?∫™]+.*")
    (let ()
      (set! symbols_not_to_say "\"`.,;:°!ø?-™∫[]{}()")
      (set! symbols_to_say "*%&$§@#+^=/\\_|><")
      (expand_as_words_list_with_symbol name symbols_to_say symbols_not_to_say)))	

   ((not (lts.in.alphabet name 'downcase_allcharacters))
   ;; it contains some other than the lts can deal with
    (let ((subwords))
      (item.set_feat token "pos" "nn")
      (mapcar
       (lambda (letter)
	 ;; might be symbols or digits
	 (set! subwords
	       (append
		subwords
		(cond
		 ((string-matches letter "[0-9]")
		  (spanish_number letter))
		 ((string-matches letter "[A-Z¡…Õ”⁄‹—]")
		    (lts.apply letter 'downcase_allcharacters))
		 (t
		  (list letter))))))
       (symbolexplode name))
      subwords))

   ;;Indisys_MP: from el_diphone
   (t
    (list name)))
)

(define (JuntaDeAndalucia_es::number token name)
  "(JuntaDeAndalucia_es::number token name)
Return list of words that pronounce this number in Spanish."

  (error "JuntaDeAndalucia_es::number to be written\n")

)

(define (JuntaDeAndalucia_es_pa::select_tokenizer)
  "(JuntaDeAndalucia_es_pa::select_tokenizer)
Set up tokenizer for Spanish."
  (Parameter.set 'Language 'JuntaDeAndalucia_es)

  (set! token_to_words JuntaDeAndalucia_es_pa::token_to_words)
  (set! token.whitespace " \t\n\r")
  (set! token.punctuation "\"'`.,;:°!ø?(){}[]")
  (set! token.prepunctuation "\"'`°ø({[-")
  (set! token.singlecharsymbols "")

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
)

(define (JuntaDeAndalucia_es_pa::reset_tokenizer)
  "(JuntaDeAndalucia_es_pa::reset_tokenizer)
Reset any globals modified for this voice.  Called by 
(JuntaDeAndalucia_es_pa::voice_reset)."
  ;; None

  t
)

(provide 'JuntaDeAndalucia_es_pa_tokenizer)
