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
;;;  Functions for building LTS rules sets from lexicons
;;;
;;;

(defvar pl-table nil)

(define (allaligns phones letters)
  "(cummulate phones lets)
Aligns all possible ways for these strings."
  (cond
   ((null letters)
    ;; (wrongly) assume there are never less letters than phones
    (if phones
	(format t "wrong end: %s\n" word))
    nil)
   ((null phones)
    nil)
   (t
    (if (< (length phones) (length letters))
	(begin
	  (cummulate '_epsilon_ (car letters))
	  (allaligns phones (cdr letters))))
    (cummulate (car phones) (car letters))
    (allaligns (cdr phones) (cdr letters)))))

(define (valid-pair phone letter)
  "(valid-pair phone letter)
If predefined to be valid."
  (let ((entry1 (assoc_string letter pl-table)))
    (if entry1
	(assoc_string phone (cdr entry1))
	nil)))

(define (valid-pair-e phone nphone letter)
  "(valid-pair-e phone letter)
Special cases for when epsilon may be inserted before letter."
  (let ((ll (assoc_string letter pl-table))
	(pp (intern (string-append phone "-" nphone))))
    (assoc_string pp (cdr ll))))

(define (find-aligns phones letters)
  "(find-aligns phones letters)
Find all feasible alignments."
  (let ((r nil))
    (cond
     ((and (null (cdr phones)) (null (cdr letters))
	   (equal? (car phones) (car letters))
	   (equal? '# (car phones)))
      (list (list (cons '# '#)))) ;; valid end match
     (t
      (if (valid-pair '_epsilon_ (car letters))
	  (set! r (mapcar
		   (lambda (p)
		     (cons (cons '_epsilon_ (car letters)) p))
		   (find-aligns phones (cdr letters)))))
      (if (valid-pair (car phones) (car letters))
	  (set! r 
		(append r
			(mapcar
			 (lambda (p)
			   (cons (cons (car phones) (car letters)) p))
			 (find-aligns (cdr phones) (cdr letters))))))
      ;; Hmm, change this to always check doubles
      (if (valid-pair-e (car phones) (car (cdr phones)) (car letters))
	  (set! r
		(append r
			(mapcar
			 (lambda (p)
			   (cons (cons (intern (format nil "%s-%s"
						       (car phones)
						       (car (cdr phones))))
				       (car letters)) p))
			 (find-aligns (cdr (cdr phones)) 
				      (cdr letters))))))
      r))))

(define (findallaligns phones letters)
  (let ((a (find-aligns phones letters)))
    (if (null a)
	(begin
	  (set! failedaligns (+ 1 failedaligns))
	  (format t "failed: %l %l\n" letters phones)))
    a))

(define (cummulate phone letter)
  "(cummulate phone letter)
record the alignment of this phone and letter."
  (if (or (equal? phone letter)
	  (and (not (equal? phone '#))
	       (not (equal? letter '#))))
  (let ((entry1 (assoc_string letter pl-table))
	score)
    (if (equal? phone '_epsilon_)
	(set! score 0.1)
	(set! score 1))
    (if entry1
	(let ((entry2 (assoc_string phone (cdr entry1))))
	  (if entry2
	      (set-cdr! entry2 (+ score (cdr entry2)))
	      (set-cdr! entry1 (cons (cons phone 1) (cdr entry1)))))
	(set! pl-table
	      (cons 
	       (cons letter
		     (list (cons phone score)))
	       pl-table)))
    t)))

(define (score-pair phone letter)
"(score-pair phone letter)
Give score for this particular phone letter pair."
  (let ((entry1 (assoc_string letter pl-table)))
    (if entry1
	(let ((entry2 (assoc_string phone (cdr entry1))))
	  (if entry2
	      (cdr entry2)
	      0))
	0)))

(define (cummulate-aligns aligns)
  (mapcar
   (lambda (a)
     (mapcar 
      (lambda (p)
	(cummulate (car p) (cdr p)))
      a))
   aligns)
  t)

(define (cummulate-pairs trainfile)
  "(cummulate-pairs trainfile)
Build cummulatation table from allowable alignments in trainfile."
  (set! failedaligns 0)
  (set! allaligns 0)
  (if (not pl-table)
      (set! pl-table
	    (mapcar
	     (lambda (l)
	       (cons (car l) (mapcar (lambda (c) (cons c 0)) (cdr l))))
	     allowables)))
  (let ((fd (fopen trainfile "r"))
	(c 0) (d 0)
	(entry))
    (while (not (equal? (set! entry (readfp fd)) (eof-val)))
	   (if (equal? c 1000)
	       (begin
		 (format t "ENTRY: %d %l\n" (set! d (+ 1000 d)) entry)
		 (set! c 0)))
	   (set! word (car entry))
	   (cummulate-aligns
	    (findallaligns 
	     (enworden (car (cdr (cdr entry))))
	     (enworden (wordexplode (car entry)))))
	   (set! allaligns (+ 1 allaligns))
           (format t "aligned %d\n" allaligns)
	   (set! c (+ 1 c)))
    (fclose fd)
    (format t "failedaligns %d/%d\n" failedaligns allaligns)
    ))

(define (find_best_alignment phones letters)
  "(find_best_alignment phones letters)
Find the alignement containg the most frequent alignment pairs."
  ;; hackily do this as a global
  (set! fba_best_score 0)
  (set! fba_best nil)
  (find-best-align phones letters nil 0)
  fba_best
)


(define (find-best-align phones letters path score)
  "(find-best-align phones letters)
Find all feasible alignments."
  (cond
   ((null letters)
    (if (> score fba_best_score)
	(begin
	  (set! fba_best_score score)
	  (set! fba_best (reverse path))))
    nil)
   (t
    (if (valid-pair '_epsilon_ (car letters))
	(find-best-align phones (cdr letters)
			 (cons (cons '_epsilon_ (car letters)) path)
			 (+ score (score-pair '_epsilon_ (car letters)))))
    (if (valid-pair (car phones) (car letters))
	(find-best-align (cdr phones) (cdr letters)
			 (cons (cons (car phones) (car letters))path)
			 (+ score (score-pair (car phones) (car letters)))))
    (if (valid-pair-e (car phones) (car (cdr phones)) (car letters))
	(find-best-align (cdr (cdr phones)) (cdr letters)
			 (cons (cons (intern (format nil "%s-%s"
						     (car phones)
						     (car (cdr phones))))
				     (car letters))
			       path)
			 (+ score (score-pair 
				   (intern (format nil "%s-%s"
						   (car phones)
						   (car (cdr phones))))
				   (car letters))))))))

(define (align_and_score phones letters path score)
  "(align_and_score phones lets)
Aligns all possible ways for these strings."
  (cond
   ((null letters)
    (if (> score fba_best_score)
	(begin
	  (set! fba_best_score score)
	  (set! fba_best (reverse path))))
    nil)
   (t
    (if (< (length phones) (length letters))
	(align_and_score
	 phones
	 (cdr letters)
	 (cons '_epsilon_ path)
	 (+ score
	    (score-pair '_epsilon_ (car letters)))))
    (align_and_score
     (cdr phones)
     (cdr letters)
     (cons (car phones) path)
     (+ score
	(score-pair (car phones) (car letters)))))))

(define (aligndata file ofile)
  (let ((fd (fopen file "r"))
	(ofd (fopen ofile "w"))
	(c 1)
	(entry))
    (while (not (equal? (set! entry (readfp fd)) (eof-val)))
	   (set! lets (enworden (wordexplode (car entry))))
	   (set! bp (find_best_alignment
		     (enworden (car (cdr (cdr entry))))
		     lets))
	   (if (not bp)
	       (format t "align failed: %l\n" entry)
	       (save_info (car (cdr entry)) bp ofd))
	   (set! c (+ 1 c)))
    (fclose fd)
    (fclose ofd)))

(define (enworden lets)
  (cons '# (reverse (cons '# (reverse lets)))))

(define (wordexplode lets)
  (if (consp lets)
      lets
      (symbolexplode lets)))

(define (save_info pos bp ofd)
  "(save_info pos bp ofd)
Cut out one expensive step and 50M of diskspace and just save it
in a simpler format."
  (format ofd "( ( ")
  (mapcar
   (lambda (l) 
     (if (not (string-equal "#" (cdr l)))
	 (format ofd "%l " (cdr l))))
   bp)
  (format ofd ") %l" pos)
  (mapcar
   (lambda (l)
     (if (not (string-equal "#" (car l)))
	 (format ofd " %s" (car l))))
   bp)
  (format ofd " )\n"))

(define (normalise-table pl-table)
  "(normalise-table pl-table)
Change scores into probabilities."
  (mapcar
   (lambda (s)
     (let ((sum (apply + (mapcar cdr (cdr s)))))
       (mapcar
	(lambda (p)
          (if (equal? sum 0)
              (set-cdr! p 0)
              (set-cdr! p (/ (cdr p) sum))))
	(cdr s))))
   pl-table)
  t)

(define (save-table pre)
  (normalise-table pl-table)
  (set! fd (fopen (string-append pre "pl-tablesp.scm") "w"))
  (format fd "(set! pl-table '\n")
  (pprintf pl-table fd)
  (format fd ")\n")
  (fclose fd)
  t)

(define (build-feat-file alignfile featfile)
"(build-feat-file alignfile featfile)
Build a feature file from the given align file.  The feature
file contain predicted phone, and letter with 3 preceding and
3 succeeding letters."
  (let ((fd (fopen alignfile "r"))
	(ofd (fopen featfile "w"))
	(entry)
	(pn)
	(sylpos 1))
    (while (not (equal? (set! entry (readfp fd)) (eof-val)))
;;           (format t "read: %l\n" entry)
	   (set! lets (append '(0 0 0 0 #) (wordexplode (car entry))
			      '(# 0 0 0 0)))
	   (set! phones (cdr (cdr entry)))
	   (set! pn 5)
	   (mapcar
	    (lambda (p)
	      (format ofd
		      "%s  %s %s %s %s  %s  %s %s %s %s  %s\n"
		      p
		      (nth (- pn 4) lets)
		      (nth (- pn 3) lets)
		      (nth (- pn 2) lets)
		      (nth (- pn 1) lets)
		      (nth pn lets)
		      (nth (+ pn 1) lets)
		      (nth (+ pn 2) lets)
		      (nth (+ pn 3) lets)
		      (nth (+ pn 4) lets)
                      (cond
                       ((not (consp (car (cdr entry))))
                        (car (cdr entry)))
                       ((not (consp (caar (cdr entry))))
                        (caar (cdr entry)))
                       (t nil))
		      ;; sylpos
		      ;; numsyls
		      ;; num2end
		      )
	      (set! pn (+ 1 pn)))
	    phones))
    (fclose fd)
    (fclose ofd))
)

(define (merge_models name filename allowables)
"(merge_models name filename)
Merge the models into a single list of cart trees as a variable
named by name, in filename."
  (require 'cart_aux)
  (let (trees fd)
    (set! trees nil)
    (set! lets (mapcar car allowables))
    (while lets
      (if (probe_file (format nil "lts.%s.tree" (car lets)))
          (begin
            (format t "%s\n" (car lets))
            (set! tree (car (load (format nil "lts.%s.tree" (car lets)) t)))
            (set! tree (cart_simplify_tree2 tree nil))
            (set! trees
                  (cons (list (car lets) tree) trees))))
      (set! lets (cdr lets)))
    (set! trees (reverse trees))
    (set! fd (fopen filename "w"))
    (format fd ";; LTS rules \n")
    (format fd "(set! %s '(\n" name)
    (mapcar
     (lambda (tree) (pprintf tree fd))
     trees)
    (format fd "))\n")
    (fclose fd))
)

(define (lts_testset file cartmodels)
  "(lts_testset file cartmodels)
Test an aligned lexicon file against a set of cart trees.  Prints out
The number of letters correct (for each letter), total number of 
letters correct and the total number of words correct.  cartmodels is
the structure as saved by merge_models."
  (let ((fd (fopen file "r"))
	(entry)
	(wordcount 0)
	(correctwords 0)
	(phonecount 0)
	(correctphones 0))
    (while (not (equal? (set! entry (readfp fd)) (eof-val)))
	   (let ((letters (enworden (wordexplode (car entry))))
		 (phones (enworden (cdr (cdr entry))))
		 (pphones))
	     (set! wordcount (+ 1 wordcount))
	     (set! pphones (gen_cartlts letters (car (cdr entry)) cartmodels))
;	     (set! pphones 
;                   (or ; unwind-protect
;                    (gen_vilts letters (car (cdr entry))
;                               cartmodels wfstname)
;                    nil))
	     (if (equal? (ph-normalize pphones) (ph-normalize phones))
		 (set! correctwords (+ 1 correctwords))
		 (or nil
		     (format t "failed %l %l %l %l\n" (car entry) (car (cdr entry)) phones pphones)))
	     (count_correct_letters   ;; exclude #, cause they're always right
	      (cdr letters)
	      (cdr phones)
	      (cdr pphones))
	     (set! phonecount (+ (length (cdr (cdr letters))) phonecount))
	     ))
    (fclose fd)
    (mapcar
     (lambda (linfo)
       (format t "%s %d correct %d (%2.2f)\n"
	       (car linfo) (car (cdr linfo))
	       (car (cdr (cdr linfo)))
	       (/ (* (car (cdr (cdr linfo))) 100) (car (cdr linfo))))
       (set! correctphones (+ correctphones (car (cdr (cdr linfo))))))
     correct_letter_table)
    (format t "phones %d correct %d (%2.2f)\n"
	    phonecount correctphones (/ (* correctphones 100) phonecount))
    (format t "words %d correct %d (%2.2f)\n"
	    wordcount correctwords (/ (* correctwords 100) wordcount))
    (format t "tree model has %d nodes\n"
	    (apply + (mapcar (lambda (a) (cart_tree_node_count (car (cdr a))))
			     cartmodels)))
    ))

(define (cart_tree_node_count tree)
  "(tree_node_count tree)
Count the number nodes (questions and leafs) in the given CART tree."
  (cond
   ((cdr tree)
    (+ 1
       (cart_tree_node_count (car (cdr tree)))
       (cart_tree_node_count (car (cdr (cdr tree))))))
   (t
    1)))

(defvar correct_letter_table
  (mapcar
   (lambda (l) (list l 0 0))
   '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
  "correct_letter_table
List used to cummulate the number of correct (and incorrect) letter to
phone predictions.  This list will be extended if there are more letters
in your alphabet, though it doesn't take a fairly western european
view of the alphabet,  but you can change this yourself is necessary.")

(define (count_correct_letters lets phs pphs)
 "(count_correct_letters lets phs pphs)
Count which letters have the correct phone prediction.  Cummulate this
is a per letter table."
 (cond
  ((or (null phs) (null pphs) (null lets))
   (format t "misaligned entry\n") 
   nil)
  ((and (null (cdr lets)) (null (cdr phs)) (null (cdr pphs)))
   nil)  ;; omit final #
  (t
   (let ((letinfo (assoc_string (car lets) correct_letter_table)))
     (if (not letinfo)
	 (set! correct_letter_table
	       (append correct_letter_table
		       (list (set! letinfo (list (car lets) 0 0))))))
     (set-car! (cdr letinfo) (+ 1 (car (cdr letinfo)))) ;; total
     (if (equal? (car phs) (car pphs))                  ;; correct 
	 (set-car! (cdr (cdr letinfo)) (+ 1 (car (cdr (cdr letinfo))))))
     (count_correct_letters (cdr lets) (cdr phs) (cdr pphs))))))

(define (ph-normalize ph)
  (cond
   ((null ph) nil)
   ((string-equal "_epsilon_" (car ph))
    (ph-normalize (cdr ph)))
   ((string-matches (car ph) ".*-.*")
    (cons
     (string-before (car ph) "-")
     (cons
      (string-after (car ph) "-")
      (ph-normalize (cdr ph)))))
   (t
    (cons (car ph) (ph-normalize (cdr ph))))))

(define (make_let_utt_p letters pos)
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
	 (item.set_name lsi l)
	 (item.set_feat lsi "pos" pos)))
     letters)
    utt))

(define (gen_vilts letters pos cartmodels ngram)
  "(get_vilts letters pos cartmodels ngram)
Use cart plus ngrams in viterbi search."
  (require 'lts)
  (let ((utt (make_let_utt_p letters pos)))
    (set! gen_vit_params
	  (list
	   (list 'Relation "LETTER")
	   (list 'return_feat "phone")
	   (list 'p_word "#")
	   (list 'pp_word "0")
           (list 'ngramname ngram)
;	   (list 'wfstname ngram)
	   (list 'cand_function 'lts_cand_function)))
    (Gen_Viterbi utt)
    (mapcar 
      (lambda (lsi)
	(intern (item.feat lsi "phone")))
      (utt.relation.items utt 'LETTER))))

(define (gen_cartlts letters pos cartmodels)
  "(get_cartlts letters cartmodels)
Generate the full list of predicted phones, including
epsilon and unexpanded multi-phones."
  (require 'lts)
  (let ((utt (make_let_utt_p letters pos)))
    (enworden
     (mapcar
      (lambda (lsi)
	(let ((tree (car (cdr (assoc_string (item.name lsi) cartmodels))))
	      (p))
	  (if (not tree)
	      (begin
		(format t "failed to find tree for %s\n" (item.name lsi))
		nil)
	      (begin
		(set! p (wagon_predict lsi tree))
		(item.set_feat lsi "val" p)
		p))))
      (reverse (cdr (reverse (cdr (utt.relation.items utt 'LETTER)))))))))

(define (reduce_lexicon entryfile exceptionfile lts_function)
  "(reduce_lexicon entryfile exceptionfile lts_function)
Look up each word in entryfile using the current lexicon, if the entry
doesn't match save it in the exception file.  This is a way of reducing
the lexicon based on a letter to sound model (and lexical stress 
model, if appropriate)."
  (let ((fd (fopen entryfile "r"))
	(ofd (fopen exceptionfile "w"))
	(entry)
	(wordcount 0)
	(correctwords 0))
    (while (not (equal? (set! entry (readfp fd)) (eof-val)))
	   (if (and (consp entry) 
		    (> (length entry) 1))
	       (let ((lts (lts_function (car entry) (car (cdr entry))))
		     (encount (lex.entrycount (car entry))))
		 (set! wordcount (+ 1 wordcount))
		 (if (and (equal? (nth 2 entry) (nth 2 lts))
			  (< encount 2))
		     (set! correctwords (+ 1 correctwords))
		     (format ofd "%l\n" entry))
		 )))
    (fclose fd)
    (fclose ofd)
    (format t "words %d correct %d (%2.2f)\n"
	    wordcount correctwords (/ (* correctwords 100) wordcount))
    ))

(define (dump-flat-entries infile outfile ltype)
  (let ((ifd (fopen infile "r"))
        (ofd (fopen outfile "w"))
        clength
        entry)
;    (set! entry (readfp ifd))
;    (if (or (consp entry) (not (string-equal entry "MNCL")))
;        (begin 
;          (format t "Expected MNCL at start of file: not a compiled lexicon\n")
;          (exit)))
    (while (not (equal? (set! entry (readfp ifd)) (eof-val)))
       (cond
        ((not (consp entry))
         t) ;; not an entry
        ((string-equal ltype "utf8")
         (set! clength (length (utf8explode (car entry)))))
        (t
         (set! clength (length (car entry)))))
       (cond
        ((not (consp entry))
         t) ;; not an entry
        ((and ;(string-matches (car entry) "...*")
              ;(< clength 14)
              (not (string-matches (car entry) ".*'.*")) ;; no quotes
              (car (cddr entry))) ;; non-nil pronounciation
         (begin
           (cond
            ((string-equal ltype "utf8")
             (format ofd
                       "( %l %l ("
                       (utf8explode (car entry))
                       (cadr entry)))
            ((string-equal ltype "asis")
             (format ofd
                     "( \"%s\" %l ("
                     (car entry)
                     (cadr entry)))
            (t
             (format ofd
                     "( \"%s\" %l ("
                     (downcase (car entry))
                     (cadr entry))))
           (if (consp (car (car (cddr entry))))
               (begin ;; it is syllabified)
                 (mapcar
                  (lambda (syl)
                    (mapcar
                     (lambda (seg)
                       (cond
                        ((string-matches seg "[aeiouAEIOU@].*")
                         (format ofd "%s " (string-append seg (cadr syl))))
                        (t
                         (format ofd "%s " seg))))
                     (car syl)))
                  (car (cddr entry))))
               (begin ;; it is already flat
                 (mapcar
                  (lambda (p)
                    (format ofd "%s " p))
                  (car (cddr entry)))
                 ))
           (format ofd "))\n")))
        (t nil)))
    (fclose ifd)
    (fclose ofd)))

(define (dump-lets-phones infile)
  "(dump-lets-phones infile)
Dump all the letters to alllets.out and phones to allphones.out for processing.
This expects an external script to sort and uniquify them.  This is done
in scheme so we can get utf8/non-utf8 to be easy."
  (let ((ifd (fopen infile "r"))
        (lfd (fopen "alllets.out" "w"))
        (apfd (fopen "allphones.out" "w"))
        (pfd (fopen "let2phones.out" "w"))
        entry)
    (while (not (equal? (set! entry (readfp ifd)) (eof-val)))
       (mapcar
        (lambda (l) 
          (format lfd "%s\n" l)
          (format pfd "%s " l)
          (mapcar 
           (lambda (p) (format pfd "%s " p))
           (car (cddr entry)))
          (format pfd "\n"))
        (wordexplode (car entry)))
       (mapcar
        (lambda (p) (format apfd "%s " p))
        (car (cddr entry)))
       (format apfd "\n")
       )
    (fclose ifd)
    (fclose lfd)
    (fclose pfd)
    (fclose apfd)
    t))

(define (dump-flat-entries-all infile outfile)
  "(dump-flat-entries-all infile outfile)
Do this for *all* entries not just ones with more than three chars."
  (let ((ifd (fopen infile "r"))
        (ofd (fopen outfile "w"))
        entry)
    (readfp ifd) ;; skip "MNCL"
    (while (not (equal? (set! entry (readfp ifd)) (eof-val)))
     (if (consp entry)
         (begin
           (format ofd
                   "( \"%s\" %s ("
                   (downcase (car entry))
                   (cadr entry))
           (mapcar
            (lambda (syl)
              (mapcar
               (lambda (seg)
                 (cond
;                 ((string-equal seg "ax")
;                    (format ofd "%s " seg))
                  ((string-matches seg "[aeiouAEIOU@].*")
                     (format ofd "%s " (string-append seg (cadr syl))))
                  (t
                   (format ofd "%s " seg))))
               (car syl)))
            (car (cddr entry)))
           (format ofd "))\n"))))
    (fclose ifd)
    (fclose ofd)))

(provide 'lts_build)

