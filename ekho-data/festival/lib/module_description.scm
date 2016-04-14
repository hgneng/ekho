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
;;;  Handle module descriptions.
;;;  

(defvar *module-descriptions* nil
  "*module-descriptions*
   An association list recording the description objects for proclaimed
   modules.")

(define (set_module_description mod desc)
  "(set_module_description MOD DESC)
   Set the description for the module named MOD."
  (let ((entry (assoc mod *module-descriptions*)))
    (if entry
	(set-cdr! entry (cons desc nil))
	(set! *module-descriptions* (cons (cons mod (cons desc nil)) 
					  *module-descriptions*))
	)
    )
  )

(define (module_description mod)
  "(module_description MOD)
   Returns the description record of the module named by symbol MOD"
  (let ((entry (assoc mod *module-descriptions*)))
    (if entry
	(car (cdr entry))
	nil
	)
    )
  )

(defmac (proclaim form)
  "(proclaim NAME &opt DESCRIPTION...)
   Anounce the availability of a module NAME. DESCRIPTION
   is a description in a fixed format."
  (let ((name (car (cdr form)))
	(description (cdr form))
	)
    (list 'proclaim-real (list 'quote name) (list 'quote description))
    )
  )

(define (proclaim-real name description)
  (set! *modules* (cons name *modules*))
;  (if description
;      (set_module_description name (create_module_description description))
;      )
  )

(define (describe_module mod)
  "(describe_module MOD)
   Describe the module named by the symbol MOD."

  (let ((entry (module_description mod)))
    (format t "---------------------\n")
    (if entry
	(print_module_description entry)
	(format t "No description for %l\n" mod)
	)
    (format t "---------------------\n")
    )
  )

(define (describe_all_modules)
  "(describe_all_modules)
   Print descriptions of all proclaimed modules"
  (format t "---------------------\n")
  (let ((p *module-descriptions*))
    (while p
	   (print_module_description (car (cdr (car p))))
	   (format t "---------------------\n")
	   (set! p (cdr p))
	   )
    )
  )

(proclaim 
 module_description 1.1 
 "CSTR"  "Richard Caley <rjc@cstr.ed.ac.uk>"
  ( "Handle module descriptions from C++ and from Scheme."
   )
  )

(provide 'module_description)
