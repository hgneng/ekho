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
;;;  Functions specific to supporting a trained LTS rules
;;;

(define (lts_rules_predict word feats)
  (let ((dcword (downcase word))
	(syls) (phones))
    (if (string-matches dcword "[a-z]*")
	(begin
	  (set! phones 
		(cdr (reverse (cdr (reverse (lts_predict dcword))))))
	  (set! phones (add_lex_stress word feats phones))
	  (set! syls (lex.syllabify.phstress phones))
;;	  (set! syls (add_lex_stress word syls))
	  )
	(set! syls nil))
    (format t "word %l phones %l\n" word syls)
    (list word nil syls)))

;(define (add_lex_stress word syls)
;  (cond
;   ((> (length syls) 1)
;      (set-car! (cdr (nth (- (length syls) 2) syls)) 1))
;   ((word-is-content word english_guess_pos)
;      (set-car! (cdr (car syls)) 1)))
;  syls)

(define (word-is-content word guess_pos)
  (cond
   ((null guess_pos)
    t)
   ((member_string word (cdr (car guess_pos)))
    nil)
   (t
    (word-is-content word (cdr guess_pos)))))

(defvar lts_pos nil)

(define (lts_predict word rules)
  "(lts_predict word rules)
Return list of phones related to word using CART trees."
  (let ((utt (make_let_utt (enworden (wordexplode word)))))
    (predict_phones utt rules)
    (cdr (reverse (cdr (reverse ;; remove #'s
      (mapcar 
       (lambda (p) (intern (item.name p)))
       (utt.relation.items utt 'PHONE))))))
    )
)

(define (wordexplode lets)
  (if (consp lets)
      lets
      (symbolexplode lets)))

(define (make_let_utt letters)
"(make_let_utt letters)
Build an utterances from th4ese letters."
  (let ((utt (Utterance Text "")))
    (utt.relation.create utt 'LTS)
    (utt.relation.create utt 'LETTER)
    (utt.relation.create utt 'PHONE)
    ;; Create letter stream
    (mapcar
     (lambda (l)
       (let ((lsi (utt.relation.append utt 'LETTER)))
	 (item.set_feat lsi "pos" lts_pos)
	 (item.set_name lsi l)))
     letters)
    utt))

(define (predict_phones utt rules)
  "(predict_phones utt)
Predict phones using CART."
  (add_new_phone utt (utt.relation.first utt 'LETTER) '#)
  (mapcar
   (lambda (lsi)
     (let ((tree (car (cdr (assoc_string (item.name lsi) rules)))))
       (if (not tree)
	   (format t "failed to find tree for %s\n" (item.name lsi))
	   (let ((p (wagon_predict lsi tree)))
;	     (format t "predict %s %s\n" (item.name lsi) p)
	     (cond
	      ((string-matches p ".*-.*-.*-.*") ; a quad one
	       (add_new_phone utt lsi (string-before p "-"))
	       (add_new_phone utt lsi (string-before (string-after p "-") "-"))
	       (add_new_phone utt lsi (string-before (string-after (string-after p "-") "-") "-"))
	       (add_new_phone utt lsi (string-after (string-after (string-after p "-") "-") "-")))
	      ((string-matches p ".*-.*-.*") ; a triple one
	       (add_new_phone utt lsi (string-before p "-"))
	       (add_new_phone utt lsi (string-before (string-after p "-") "-"))
	       (add_new_phone utt lsi (string-after (string-after p "-") "-")))
	      ((string-matches p ".*-.*");; a double one
	       (add_new_phone utt lsi (string-before p "-"))
	       (add_new_phone utt lsi (string-after p "-")))
	      (t
	       (add_new_phone utt lsi p)))))))
   (reverse (cdr (reverse (cdr (utt.relation.items utt 'LETTER))))))
  (add_new_phone utt (utt.relation.last utt 'LETTER) '#)
  utt)

(define (add_new_phone utt lsi p)
  "(add_new_phone utt lsi p)
Add new phone linking to letter, ignoreing it if its _epsilon_."
  (if (not (equal? p '_epsilon_))
      (let ((psi (utt.relation.append utt 'PHONE)))
	(item.set_name psi p)
	(item.relation.append_daughter
	 (utt.relation.append utt 'LTS lsi)
	 'LTS psi)
	)))

(define (enworden lets)
  (cons '# (reverse (cons '# (reverse lets)))))

;;; Lexical stress assignment
;;;

(define (add_lex_stress word pos phones tree)
  "(add_lex_stress word syls)
Predict lexical stress by decision tree."
  (let ((utt (Utterance Text ""))
	(si)
	(nphones))
    (utt.relation.create utt 'Letter)
    (set! si (utt.relation.append utt 'Letter))
    (item.set_feat si 'pos pos)
    (item.set_feat si 'numsyls (count_syls phones))
    (item.set_feat si 'sylpos 1)
    (set! nphones (add_lex_stress_syl phones si tree))
;    (format t "%l\n" phones)
;    (format t "%l\n" nphones)
    nphones))

(define (count_syls phones)
  (cond
   ((null phones) 0)
   ((string-matches (car phones) "[aeiou@].*")
    (+ 1 (count_syls (cdr phones))))
   (t (count_syls (cdr phones)))))

(define (add_lex_stress_syl phones si tree)
  "(add_lex_stress_syl phones si tree)
Add lexical stressing."
  (cond
   ((null phones) nil)
   ((string-matches (car phones) "[aeiou@].*")
    (item.set_feat si 'phone (car phones))
    (item.set_feat si 'name (car phones))
    (item.set_feat si 'num2end 
			 (- (+ 1 (item.feat si 'numsyls))
			    (item.feat si 'sylpos)))
    (set! stress (wagon_predict si tree))
    (item.set_feat si 'sylpos
			 (+ 1 (item.feat si 'sylpos)))
    (cons
     (if (not (string-equal stress "0"))
	 (string-append (car phones) stress)
	 (car phones))
     (add_lex_stress_syl (cdr phones) si tree)))
   (t 
    (cons
     (car phones)
     (add_lex_stress_syl (cdr phones) si tree)))))

;;; Morphological analysis


;(define (wfst_stemmer)
;  (wfst.load 'stemmer "/home/awb/projects/morpho/engstemmer.wfst")
;  (wfst.load 'stemmerL "/home/awb/projects/morpho/engstemmerL.wfst")
;  t)

;(define (stem word)
;  (wfst.transduce 'stemmer (enworden (symbolexplode word))))

;(define (stemL word)
;  (wfst.transduce 'stemmerL (enworden (symbolexplode word))))

(provide 'lts)
