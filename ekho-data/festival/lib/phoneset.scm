;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
;;;                         Copyright (c) 1999                            ;;
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
;;;                         Author: Alan W Black
;;;                         Date:   April 1999
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  
;;;  Support code for phone set definitions
;;;

(defmac (defPhoneSet form)
  (list 'defPhoneSet_real 
	(list 'quote (cadr form))
	(list 'quote (car (cddr form)))
	(list 'quote (cadr (cddr form)))))

(define (defPhoneSet_real name featdefs phones)
  "(defPhoneSet NAME FEATTYPES PHONES)
Define a phone set with given name, feature types and
list of phones.  This also selects name as the current phoneset."
  (let (info)
    (if (not (eq? 'Features (car featdefs)))
	(begin
	  ;; Old format that has the same number of phone features for
	  ;; all phones
	  (set! info
		(mapcar 
		 (lambda (ph)
		   (let ((fvs
			  (mapcar
			   list
			   (mapcar car featdefs)
			   (cdr ph))))
		   (ps_check_fvals
		    (cons (car ph) (cons (list 'type t) fvs))
		    (cons t fvs))
		   (list (car ph) fvs)))
		 phones)))
	;; else
	;;   New format where types are specified so phones may have
        ;;   different features
	(set! info
	 (mapcar 
	  (lambda (ph)
	    (let ((fvs 
		   (cons 
		    (list 'type (cadr ph))
		    (mapcar 
		     list 
		     (mapcar car (cdr (assoc (cadr ph) (cdr featdefs))))
		     (cddr ph)))))
	      (ps_check_fvals 
	       (cons (car ph) fvs)
	       (assoc (cadr ph) (cdr featdefs)))
	      (list (car ph) fvs)))
	  (cdr phones))))
    (Param.set 
     (string-append "phonesets." name)
     info)
    (PhoneSet.select name)
    (list name info)))

(define (ps_check_fvals fvs featdefs)
  "(ps_check_fvals fvs featdefs)
Check that feature values in a phone definition are in the defined
set of possibles."
  (mapcar 
   (lambda (fp)
     (let ((def (cdr (assoc (car fp) (cdr featdefs)))))
       (cond
	((not def)
	 (error "Phoneset definition: phone has no defined type" fvs))
	((not (member_string (car (cdr fp)) def))
	 (error 
	  (format nil "Phoneset definition: phone feature %l is undefined" fp) fvs)))))
   (cdr (cdr fvs))))

(define (PhoneSet.select name)
  "(PhoneSet.select name)
Select named phonset as current."
  (if (feats.present Param (string-append "phonesets." name))
      (Param.set "phoneset" (Param.get (string-append "phonesets." name)))
      (error "no phoneset defined: " name)))

(define (PhoneSet.description name)
  "(PhoneSet.description)
Return (lisp) representation of current phoneset."
  (feats.tolisp (Param.get "phoneset")))

(define (PhoneSet.list)
  "(PhoneSet.list)
List of the names of the currently defined phonesets."
  ;; This isn't a particularly efficient way to get the answer
  (mapcar car (feats.tolisp (Param.get "phonesets"))))

(define (PhoneSet.silences sils)
  "(PhoneSet.silences SILLIST)
Define the silence phones for the currently selected phoneset."
  (Param.set "phoneset.silences" sils))

(provide 'phoneset)




