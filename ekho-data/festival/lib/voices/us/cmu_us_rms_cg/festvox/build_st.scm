;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                     Carnegie Mellon University                      ;;;
;;;                  and Alan W Black and Kevin Lenzo                   ;;;
;;;                      Copyright (c) 1998-2002                        ;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;; Code for converting festvox setup files to SphinxTrain files        ;;;
;;; for auto-labelling                                                  ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(cond
; ((probe_file "festvox/build_clunits.scm")
;  (load "festvox/build_clunits.scm"))
; ((probe_file "festvox/build_ldom.scm")
;  (load "festvox/build_ldom.scm")))

(define (st_setup datafile vname)

  (st_phonedictrans datafile vname)
)

(define (st_phones vname)
  (let ((ofd (fopen (format nil "st/etc/%s.phone" vname) "w")))
    (mapcar
     (lambda (p) (format ofd "%s\n" (car p)))
     (cadr (assoc 'phones (PhoneSet.description))))
    (fclose ofd)))

(define (add_to_list i list)
  (cond
   ((member_string i list)
    list)
   (t
    (cons i list))))

(define (ennicen_word w)
  (let ((ws (format nil "%s" w))
	(lets nil) (p 0))
    (while (< p (length ws))
      (if (string-matches (substring ws p 1) "[A-Za-z0-9]")
	  (set! lets (cons (substring ws p 1) lets))
	  (set! lets (cons "Q" lets)))
      (set! p (+ 1 p)))
    (if (> (length lets) 20)  ;; long word names can confuse sphinx
        (set! lets (list "x" (length lets) "Q")))
    (apply
     string-append
     (reverse lets))))

(defvar wordlist nil)

(define (add_word_to_list name d)
  (let ((w wordlist))
    (if (string-matches name "[A-Z0-9][A-Z0-9]*")
	(set! nicename name)
	(set! nicename (ennicen_word (upcase name))))
    (set! entry (assoc_string nicename wordlist))
    (if entry
	(begin
	  (set! subentry (assoc_string d (cdr entry)))
	  (if subentry
	      (if (string-equal "1" (cadr subentry))
		  (car entry)
		  (format nil "%s%d" (car entry) (cadr subentry)))
	      (begin
		(set-cdr!   ;; new homograph
		 entry
		 (cons
		  (list d (+ 1 (cadr (cadr entry))))
		  (cdr entry)))
		(format nil "%s%d" (car entry) (cadr (cadr entry))))))
	(begin ;; new word
	  (set! wordlist ;; new word
		(cons
		 (list nicename (list d 1)) wordlist))
	  nicename))))

;     (while (and 
; 	    w (not (string-equal d (cadr (car w)))))
;       (set! w (cdr w)))
;     (if w
; 	(caar w)
; 	(begin
; 	  (set! wordname (format nil "%s%d" nicename wordn))
; 	  (set! wordlist
; 		(cons (list wordname d) wordlist))
; 	  (set! wordn (+ 1 wordn))
; 	  wordname))))

(define (make_nicephone s)
  "(make_nicephone s)
Sphinx can't deal with phone names distinguished only by case, so
if the phones have any upper case they are prepend CAP."
  (let ((n))
    (if (string-matches (item.name s) ".*[A-Z].*")
	(set! n (string-append "CAP" (item.name s)))
	(set! n (item.name s)))
    n))
  

(define (st_phonedictrans datafile vname)
  (let ((words nil) (phones nil) (wordn 0)
	(silence (car (cadr (car (PhoneSet.description '(silences))))))
	(pfd (fopen (format nil "st/etc/%s.phone" vname) "w"))
	(dfd (fopen (format nil "st/etc/%s.dic" vname) "w"))
	(ffd (fopen (format nil "st/etc/%s.filler" vname) "w"))
	(ifd (fopen (format nil "st/etc/%s.fileids" vname) "w" ))
	(tfd (fopen (format nil "st/etc/%s.transcription" vname) "w"))
	)
    (mapcar
     (lambda (p)
       (format t "%s\n" (car p))
       (let ((u (Utterance Text "")))
	 (set! pron "")
	 (set! u (utt.load nil (format nil "prompt-utt/%s.utt" (car p))))
	 (format ifd "%s\n" (car p))
	 (format tfd "<s>")
	 (mapcar
	  (lambda (s) 
;	    (format t "ph %s - %s\n" (item.name s) pron)
;	    (format t "qqq %l %l %l %l\n" 
;		    (item.relation s 'SylStructure)
;		    (null (item.prev (item.relation.parent s "SylStructure")))
;		    (null (item.relation.prev s "SylStructure"))
;		    (not (string-equal "" pron)))
	    (if (and (null (item.prev (item.relation.parent s "SylStructure")))
		     (null (item.relation.prev s "SylStructure"))
		     (not (string-equal "" pron)))
		(begin
		  (format tfd " %s" (add_word_to_list
				     maybewordname
				     pron))
		  (set! pron ""))
		)
	    (cond
;	     ((and (string-equal (item.name s) silence)
;		   (not (string-equal (item.feat s "p.name") silence)))
;	      (format tfd " <sil>"))
	     ((not (string-equal (item.name s) silence))
	      (set! maybewordname 
		    (item.feat s "R:SylStructure.parent.parent.name"))
	      (set! nicephone (make_nicephone s))
	      (set! phones (add_to_list nicephone phones))
              (set! pron (format nil "%s %s" pron nicephone))
;	      (format t "pron is %s\n" pron)
	      )
	     (t
	      nil)))
	  (utt.relation.items u 'Segment))
	 (if (not (string-equal "" pron))
	     (format tfd " %s" (add_word_to_list maybewordname pron)))
	 (format tfd " </s> (%s)\n" (car p)))
       )
     (load datafile t))
    (fclose tfd)
    
    (mapcar
     (lambda (l)
       (mapcar
	(lambda (ss)
	  (if (> (cadr ss) 1)
	      (format dfd "%s%d %s\n" (car l) (cadr ss) (car ss))
	      (format dfd "%s %s\n" (car l) (car ss))))
	(reverse (cdr l))))
     wordlist)
    (fclose dfd)

    (set! sfd (fopen "etc/mysilence" "w"))
    (format sfd "%s\n" silence)
    (fclose sfd)
    (mapcar
     (lambda (p)
       (if (not (string-equal silence p))
	   (format pfd "%s\n" p)))
     phones)
    (format pfd "SIL\n")  ;; SphinxTrain requires silence as "SIL"
    (fclose pfd)

    (format ffd "<s> SIL\n")
    (format ffd "</s> SIL\n")
    (format ffd "<sil> SIL\n")
    (fclose ffd)
))
