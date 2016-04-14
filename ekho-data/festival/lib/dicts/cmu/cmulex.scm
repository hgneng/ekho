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
;;;  Definition of Festival lexicon, derived from CMUDICT-0.4
;;;

(defvar cmulexdir (path-append lexdir "cmu"))

(require 'pos)

(set! cmulex_version "2.0 (from 0.4) July 2008")

(define (cmu_lts_function word feats)
  "(cmu_lts_function word feats)
Function called for CMULEX when word is not found in lexicon.  Uses
LTS rules trained from the original lexicon, and lexical stress
prediction rules."
  (require 'lts)
  (if (not (boundp 'cmu_lts_rules))
      (load (path-append cmulexdir "cmu_lts_rules.scm")))
  (let ((dcword (downcase word))
	(syls) (phones))
    (if (string-matches dcword "[a-z]*")
	(begin
	  (set! phones (lts_predict dcword cmu_lts_rules))
	  (set! syls (cmulex_mosyl_phstress phones))
	  )
	(set! syls nil))
    (list word nil syls)))

(define (cmulex_addenda)
  "(cmulex_addenda)
Add entries to the current lexicon (radio/darpa).  These are basically
words that are not in the CMU lexicon."
  (lex.add.entry '("worf" n (((w ao r f) 1))))
  (lex.add.entry '("t" n (((t iy) 1))))
  (lex.add.entry '("I'll" v (((ay l) 1))))
  (lex.add.entry '("it's" v (((ih t s) 1))))
  (lex.add.entry '("don't" v (((d ow n t) 1))))
  (lex.add.entry '("didn't" v (((d ih d n t) 1))))
  (lex.add.entry '("isn't" v (((ih z n t) 1))))
  (lex.add.entry '("doesn't" v (((d ah z n t) 1))))
  (lex.add.entry '("that's" v (((dh ae t s) 1))))
  (lex.add.entry '("won't" v (((w ow n t) 1))))
  (lex.add.entry '("aren't" v (((ae r n t) 1))))
  (lex.add.entry '("there's" v (((dh er z) 1))))
  (lex.add.entry '("we're" v (((w iy r) 1))))
  (lex.add.entry '("wouldn't" v (((w uh d n t) 1))))
  (lex.add.entry '("wasn't" v (((w aa z n t) 1))))
  (lex.add.entry '("they're" v (((dh er) 1))))
  (lex.add.entry '("weren't" v (((w er n t) 1))))
  (lex.add.entry '("i'm" v (((ay m) 1))))
  (lex.add.entry '("he's" v (((hh iy z) 1))))
  (lex.add.entry '("you're" v (((y uw r) 1))))
  (lex.add.entry '("haven't" v (((hh ae v n t) 1))))
  (lex.add.entry '("we've" v (((w iy v) 1))))
  (lex.add.entry '("i've" v (((ay v) 1))))
  (lex.add.entry '("hadn't" v (((hh ae d n t) 1))))
  (lex.add.entry '("they've" v (((dh ey v) 1))))
  (lex.add.entry '("shouldn't" v (((sh uh d n t) 1))))
  (lex.add.entry '("I'd" v (((ay d) 1))))
  (lex.add.entry '("they'll" v (((dh ey l) 1))))
  (lex.add.entry '("you've" v (((y uw v) 1))))
  (lex.add.entry '("you'll" v (((y uw l) 1))))
  (lex.add.entry '("I'll" v (((ay l) 1))))
  (lex.add.entry '("we'd" v (((w iy d) 1))))
  (lex.add.entry '("he'd" v (((hh iy d) 1))))
  (lex.add.entry '("he'll" v (((hh iy l) 1))))
  (lex.add.entry '("they'd" v (((dh ey d) 1))))
  (lex.add.entry '("you'd" v (((y uw d) 1))))
  (lex.add.entry '("it'll" v (((ih t) 1) ((ah l) 0))))
  (lex.add.entry '("who've" v (((hh uw v) 1))))
  (lex.add.entry '("ain't" v (((ey n t) 1))))
  (lex.add.entry '("needn't" v (((n iy d n t) 1))))
  (lex.add.entry '("she'd" v (((sh iy d) 1))))
  (lex.add.entry '("who'd" v (((hh uw d) 1))))
  (lex.add.entry '("she'll" v (((sh iy l) 1))))
  (lex.add.entry '("there'll" v (((dh er l) 1))))
  (lex.add.entry '("there'd" v (((dh er d) 1))))
  (lex.add.entry '("it'd" v (((ih t) 1) ((ah d) 0))))
  (lex.add.entry '("who'll" v (((hh uw l) 1))))
  (lex.add.entry '("that'll" v (((dh ae t l) 1))))
  (lex.add.entry '("mightn't" v (((m ay t n t) 1))))
  (lex.add.entry '("would've" v (((w uh d) 1) ((ah v) 0))))
  (lex.add.entry '("mustn't" v (((m ah s n t) 1))))
  (lex.add.entry '("how'd" v (((hh ow d) 1))))
  (lex.add.entry '("could've" v (((k uh d) 1) ((ah v) 0))))

  (lex.add.entry '("hasn't" v (((hh ae z n t) 1))))
  (lex.add.entry '("couldn't" v (((k uh d n t) 1))))
  (lex.add.entry '("can't" v (((k ae n t) 1))))
  (lex.add.entry '("we'll" v (((w iy l) 1))))

  (lex.add.entry '("uk" n (((y uw) 1) ((k ey) 1))))
  (lex.add.entry '("w" n (((d ah b) 1) ((ah l) 0) ((y uw) 1))))
  (lex.add.entry '("'s" pos (((ax z) 0))))
  (lex.add.entry '("bought" v (((b ao t) 1))))
  (lex.add.entry '("edinburgh" n (((eh d) 1) ((ah n) 0) ((b ax ) 0) ((r ow) 0))))
  (lex.add.entry '("non-intoxicating" () (((n aa n) 1) ((ih n t) 0) ((aa k) 1) ((s ih k) 0) ((ey t) 1) ((ih ng) 0))))
  (lex.add.entry '("AT&T" n (((ey) 1) ((t iy) 1) ((ah n d) 0) ((t iy) 1))))
  (lex.add.entry 
   '("cepstra" n (((k eh p) 1) ((s t r aa) 0))))
  (lex.add.entry 
   '("cepstral" n (((k eh p) 1) ((s t r ah l) 0))))
  (lex.add.entry 
   '("cepstrum" n (((k eh p) 1) ((s t r ah m) 0))))
  (lex.add.entry
   '("diphone" n (((d ay) 1) ((f ow n) 0))))
  (lex.add.entry
   '("diphones" n (((d ay) 1) ((f ow n s) 0))))
  (lex.add.entry
   '("Dr" n (((d aa k) 1) ((t er) 0))))
  (lex.add.entry
   '("emacs" n (((iy) 1) ((m ae k s) 0))))
  (lex.add.entry 
   '("hz" n (((hh eh r t z) 1))))
  (lex.add.entry 
   '("khz" n (((k ih) 1) ((l ax) 0) ((hh eh r t z) 1))))
  (lex.add.entry
   '("lived" v (((l ih v d) 1))))
  (lex.add.entry
   '("Ltd" n (((l ih m) 1) ((ah t) 0) ((ah d) 0))))
  (lex.add.entry
   '("Mrs" n (((m ih s) 1) ((ah s) 0))))
  (lex.add.entry 
   '("mhz" n (((m eh) 1) ((g ax) 0) ((hh eh r t z) 1))))
  (lex.add.entry
   '("NASA" n (((n ae) 1) ((s ae) 1))))
  (lex.add.entry 
   '("lead" led (((l eh d) 1))))
  (lex.add.entry 
   '("lead" liid (((l iy d) 1))))
  (lex.add.entry
   '("pasadena" n (((p ae s) 1) ((ae d) 0) ((iy n) 1) ((ax) 0))))
  (lex.add.entry 
   '("read" red (((r eh d) 1))))
  (lex.add.entry 
   '("read" riid (((r iy d) 1))))
  (lex.add.entry
   '("reuters" n (((r oy t) 1) ((er z) 0))))
  (lex.add.entry
   '("to" to (((t ax) 0))))
;  (lex.add.entry
;   '("usually" r (((y uw zh) 1) ((ax l) 0) ((iy) 0))))
;  (lex.add.entry
;   '("usual" r (((y uw zh) 1) ((ax l) 0))))
  (lex.add.entry
   '("Jan" n (((jh ae n y) 1) ((uw) 0) ((eh r) 1) ((iy) 0))))
  (lex.add.entry
   '("Feb" n (((f eh b) 1) ((r ax) 0) ((er) 0) ((iy) 0))))
  (lex.add.entry
   '("February" n (((f eh b) 1) ((r ax) 0) ((er) 0) ((iy) 0))))
  (lex.add.entry
   '("Mar" n (((m aa r ch) 0))))
  (lex.add.entry
   '("Apr" n (((ey p) 1) ((r ah l) 0))))
  (lex.add.entry
   '("Jun" n  (((jh uw n) 1))))
  (lex.add.entry
   '("Jul" n  (((jh uw l) 1) ((ay) 1))))
  (lex.add.entry
   '("Aug" n  (((aa g) 1) ((ah s t) 0))))
  (lex.add.entry
   '("Sep" n (((s eh p) 0) ((t eh m b) 1) ((er) 0))))
  (lex.add.entry
   '("Sept" n (((s eh p) 0) ((t eh m b) 1) ((er) 0))))
  (lex.add.entry
   '("Oct" n (((aa k) 0) ((t ow b) 1) ((er) 0))))
  (lex.add.entry
   '("Nov" n (((n ow v) 0) ((eh m b) 1) ((er) 0))))
  (lex.add.entry
   '("Dec" n (((d ih s) 0) ((eh m b) 1) ((er) 0))))
  (lex.add.entry
   '("'s" pos (((ax z) 0))))
  (lex.add.entry 
   '("*" n (((ae s) 1) ((t er) 0) ((ih s k) 0)) ((pos "K6%"))))
  (lex.add.entry 
   '("%" n (((p er) 1) ((s eh n t) 1)) ((pos "K9%"))))
  (lex.add.entry 
   '("&" n (((ae m p) 1) ((er s) 0) ((ae n d) 0))))
  (lex.add.entry 
   '("$" n (((d aa l) 1) ((er) 0))))
  (lex.add.entry 
   '("#" n (((hh ae sh) 1))))
  (lex.add.entry 
   '("@" n (((ae t) 1))))
  (lex.add.entry 
   '("+" n (((p l ah s) 0)) ((pos "K7%" "OA%" "T-%"))))
  (lex.add.entry 
   '("^" n (((k eh r) 1) ((eh t) 0)) ((pos "K6$"))))
  (lex.add.entry 
   '("~" n (((t ih l d) 1) ((ah) 0)) ((pos "K6$"))))
  (lex.add.entry 
   '("=" n (((iy k) 1) ((w ah l) 0))))
  (lex.add.entry 
   '("/" n (((s l ae sh) 1))))
  (lex.add.entry 
   '("\\" n (((b ae k) 1) ((s l ae sh) 1))))
  (lex.add.entry 
   '("_" n (((ah n d) 1) ((er s) 0) ((k ao r) 1))))
  (lex.add.entry 
   '("|" n (((v er t) 1) ((ih k) 0) ((ah l) 0) ((b aa r) 1))))
  (lex.add.entry 
   '(">" n ((( g r ey ) 1) ((t er) 0) ((dh ae n) 1))))
  (lex.add.entry 
   '("<" n ((( l eh s) 1) ((dh ae n) 1))))
  (lex.add.entry 
   '("[" n ((( l eh f t) 1) ((b r ae k) 1) ((ih t) 0))))
  (lex.add.entry 
   '("]" n ((( r ay t) 1) ((b r ae k) 1) ((ih t) 0))))
  (lex.add.entry 
   '(" " n (((s p ey s) 1))))
  (lex.add.entry 
   '("\t" n (((t ae b) 1))))
  (lex.add.entry 
   '("\n" n (((n uw) 1) ((l ay n) 1))))
  (lex.add.entry '("." punc nil))
  (lex.add.entry '("'" punc nil))
  (lex.add.entry '(":" punc nil))
  (lex.add.entry '(";" punc nil))
  (lex.add.entry '("," punc nil))
  (lex.add.entry '("-" punc nil))
  (lex.add.entry '("\"" punc nil))
  (lex.add.entry '("`" punc nil))
  (lex.add.entry '("?" punc nil))
  (lex.add.entry '("!" punc nil))
  (lex.add.entry
   '(before ()
	    (((b iy) 0) ((f ao r) 1))))
  )

(define  (cmulex_map_sylstructure syls)
  (unwind-protect
   (begin
     (mapcar
      (lambda (s1 s2)
        (list s1 (cadr s2)))
      (cmulex_syllabify_maxonset
       (apply
        append
        (mapcar car syls)))
      syls))
   (begin
     (format t "Failed to resyllabify %l\n" syls)
     syls))
)

(define (cmulex_mosyl_phstress phones)
  (set! xxx  (mapcar
   (lambda (syl)
     (set! stress 0)
     (list 
      (mapcar
       (lambda (p)
         (cond
          ((string-matches p "[aeiou@].*1")
           (set! stress 1)
           (intern (substring p 0 (- (length p) 1))))
          ((string-matches p "[aeiou@].*0")
           (set! stress 0)
           (intern (substring p 0 (- (length p) 1))))
          (t
           (intern p))))
       (mapcar string-append syl))
      stress))
   (cmulex_syllabify_maxonset phones)))
;  (format t "%l\n%l\n" phones xxx)
  xxx

)

(define (cmulex_syllabify_maxonset phones) 
  (cmulex_syllabify_maxonset2 
   (mapcar intern (reverse phones))
   nil nil)
)

(define (cmulex_syllabify_maxonset2 phones syl syls)
  "(cmulex_syllabify_maxonset phones)
Syllabify by maximum onset. phones is given in reverse order"
;  (format t "csm2 phones %l syl %l syls %l\n" 
;          phones syl syls)
  (cond
   ((null phones)
    (if syl
        (cons (reverse syl) syls)
        syls))
   ((null (cmulex_has_vowel phones))  ;; safety case
    ;; could some weird onset we've never seen before
    (cons
     (append (reverse phones) syl)
     syls))
   ((null (string-matches (car phones) "[aeiou@].*"))  ;; a vowel
    (cmulex_syllabify_maxonset2
     (cdr phones)
     (cons (car phones) syl)
     syls))
   (t  ;; is a vowel
    (let ((onset (cmulex_maxonset (cdr phones))))
      (cmulex_syllabify_maxonset2
       (nth_cdr (+ 1 (length onset)) phones)
       nil
       (cons (append onset (list (car phones)) syl) syls))))))

(define (cmulex_has_vowel p)
  (cond
   ((null p) nil)
   ((string-matches (car p) "[aeiou@].*")  ;; a vowel
    t)
   (t
    (cmulex_has_vowel (cdr p)))))

(define (cmulex_maxonset phones)
  (cond
   ((string-matches (car phones) "[aeiou@].*")
    nil)
   ((string-equal (car phones) "ng") ;; only non-syl-initial phone
    nil)
   ((null phones) nil)
   ((and (> (length phones) 2)
         (member (list (car (cddr phones))
                       (cadr phones)
                       (car phones)
                       )
                 cmulex_tri_onsets))
    (list (car (cddr phones))
                       (cadr phones)
                       (car phones)
                       ))
   ((and (> (length phones) 1)
         (member (list (cadr phones)
                       (car phones))
                 cmulex_di_onsets))
    (list (cadr phones)
          (car phones)))
   (t
    (list (car phones)))))

(set! cmulex_tri_onsets
      '(
        (s t r)
        (s p y)
        (s p r)
        (s p l)
        (s k y)
        (s k w)
        (s k r)
        (s k l)
        ))

(set! cmulex_di_onsets
'(
        (z w)
        (z l)
        (v y)
        (v r)
        (v l)
        (th w)
        (th r)
        (t y)
        (t w)
;        (t s)
        (t r)
        (sh w)
        (sh r)
        (sh n)
        (sh m)
        (sh l)
        (s w)
        (s v)
        (s t)
        (s r)
        (s p)
        (s n)
        (s m)
        (s l)
        (s k)
        (s f)
        (p y)
        (p w)
        (p r)
        (p l)
        (n y)
        (m y)
        (m r)
        (l y)
        (k y)
        (k w)
        (k r)
        (k l)
        (hh y)
        (hh w)
        (hh r)
        (hh l)
        (g y)
        (g w)
        (g r)
        (g l)
        (f y)
        (f r)
        (f l)
        (d y)
        (d w)
        (d r)
        (b y)
        (b w)
        (b r)
        (b l)
))

(set! lex_syllabification (list cmulex_mosyl_phstress))

(lex.create "cmu")
(lex.set.compile.file (path-append cmulexdir "cmudict-0.4.out"))
(lex.set.phoneset "radio")
(lex.set.lts.method 'cmu_lts_function)
(lex.set.pos.map english_pos_map_wp39_to_wp20)
(cmulex_addenda)

(provide 'cmulex)

