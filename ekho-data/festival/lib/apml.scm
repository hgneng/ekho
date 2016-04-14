;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
;;;                         Copyright (c) 2002                            ;;
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
;;;                         Author: Rob Clark
;;;                         Date:   July 2002
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sets up the current voice to synthesise from APML.
;;
;;

(require 'apml_f2bf0lr)
(require 'apml_kaldurtreeZ)

;; Default pitch settings (if unspecified in current voice.)

(defvar apml_default_pitch_mean 170 )
(defvar apml_default_pitch_standard_deviation 34 )

;; apml sythesis wrappers.

(define (apml_client_synth apml)
  "(apml_client_synth apml)
Synthesise apml and return waveform(s) to client."
  (utt.send.wave.client (apml_synth apml)))

(define (apml_synth apml)
"(apml_synth xml)
Synthesis an apml string."
(let ((tmpfile (make_tmp_filename))
      utt)
  (string_to_file tmpfile apml)
  (set! utt (apml_file_synth tmpfile))
  (delete-file tmpfile)
  utt))

(define (apml_file_synth filename)
  "(apml_file_synth filename)
Synthesis an apml file."
  (let ((utt (Utterance Tokens nil)))
    (utt.load utt filename)
    (utt.synth utt)))

(define (string_to_file file s)
"(string_to_file file string)
 Write string to file."
(let ((fd))
  (set! fd (fopen file "wb"))
  (format fd "%s" s)
  (fclose fd)))


;;;
;;; Phrasing.
;;;

;; phrasing CART.
;
; It has been decided that by default, only punctuation should affect
; phrasing (and subsequently pauses)
;
(set! apml_phrase_tree
      '
      ((lisp_apml_punc in ("?" "." ":"))         ; big punctuation
       ((BB))
       ((lisp_apml_punc in ("'" "\"" "," ";"))   ; else little punctuation
	((B))
	((lisp_apml_last_word is 1)
	 ((BB))                                  ; need a BB at the end!
	 ((NB))))))                              ; else nothing

;; feature functions for phrasing
(define (apml_punc word)
  (item.feat (item.relation.parent word 'Token) 'punc))

(define (apml_last_word word)
  (if (item.next word)
      "0" "1"))


;;;
;;; Pauses
;;;

;; feature functions for pauses
(define (apml_is_pause word)
  (if (item.relation (item.relation.parent word 'Token) 'Pause)
      t
      nil))

(define (apml_pause word)
  (if (item.relation word 'Pause)
	(item.feat (item.relation.parent (item.relation.parent word 'Token) 'Pause) "sec")
      0))

(define (Apml_Pauses utt)
  "(Pauses UTT)
Predict pause insertion for apml."
  (let ((words (utt.relation.items utt 'Word)) lastword tpname)
    (if words
        (begin
          (insert_initial_pause utt)   ;; always have a start pause
          (set! lastword (car (last words)))
          (mapcar
           (lambda (w)
             (let ((pbreak (item.feat w "pbreak"))
                   (emph (item.feat w "R:Token.parent.EMPH")))
               (cond
		((apml_is_pause w)
		 (insert_pause utt w))
                ((or (string-equal "B" pbreak)
                     (string-equal "BB" pbreak))
                 (insert_pause utt w))
                ((equal? w lastword)
                 (insert_pause utt w)))))
           words)
          ;; The embarassing bit.  Remove any words labelled as punc or fpunc
          (mapcar
           (lambda (w)
             (let ((pos (item.feat w "pos")))
               (if (or (string-equal "punc" pos)
                       (string-equal "fpunc" pos))
                   (let ((pbreak (item.feat w "pbreak"))
                         (wp (item.relation w 'Phrase)))
                     (if (and (string-matches pbreak "BB?")
                              (item.relation.prev w 'Word))
                         (item.set_feat 
                          (item.relation.prev w 'Word) "pbreak" pbreak))
                     (item.relation.remove w 'Word)
                     ;; can't refer to w as we've just deleted it
                     (item.relation.remove wp 'Phrase)))))
           words)))
  utt))



;;;
;;; Intonation.
;;;

;; Accent prediction (well transfer really).
;; 
;; We treat L+H* L-H% on a single syllable as a special case.

(set! apml_accent_cart
      '
      ((lisp_apml_accent is "Hstar")
       ((H*))
       ((lisp_apml_accent is "Lstar")
	((L*))
	((lisp_apml_LHLH is "LHLH")
	 ((L+H*L-H%))
	 ((lisp_apml_accent is "LplusHstar")
	  ((L+H*))
	  ((lisp_apml_accent is "LstarplusH")
	   ((L*+H))
	   ((NONE))))))))

(set! apml_boundary_cart
      '
      ((lisp_apml_boundary is "LL")
       ((L-L%))
       ((lisp_apml_LHLH is "LHLH")
	((NONE))                      ; this is dealt with by the accent feature
	((lisp_apml_boundary is "LH")
	 ((L-H%))
	 ((lisp_apml_boundary is "HH")
	  ((H-H%))
	  ((lisp_apml_boundary is "HL")
	   ((H-L%))
	   ((NONE))))))))

;; feature functions.
(define (apml_accent syl)
  (let ((token (item.relation.parent (item.relation.parent syl 'SylStructure) 'Token)))
    (if (and (eq (item.feat syl 'stress) 1)
	     (item.relation.parent token 'Emphasis))
	(item.feat (item.relation.parent token 'Emphasis) 'x-pitchaccent)
	0)))
	     
(define (apml_boundary syl)
  (let ((token (item.relation.parent (item.relation.parent syl 'SylStructure) 'Token)))
    (if (and (> (item.feat syl 'syl_break) 0)
	     (item.relation.parent token 'Boundary))
	(item.feat (item.relation.parent token 'Boundary) 'type)
	0)))

(define (apml_LHLH syl)
  (let ((accent (apml_accent syl))
	(boundary (apml_boundary syl)))
    (if (and (string-equal accent "LplusHstar")
	     (string-equal boundary "LH"))
	"LHLH"
	0)))


(define (apml_seg_is_LHLH_vowel seg)
  (if (and (string-equal (apml_LHLH (item.relation.parent seg 'SylStructure))
			 "LHLH")
	   (string-equal (item.feat seg 'ph_vc) "+"))
      "LHLH"
      0))


;;;; feature functions:

(define (apml_tgtype syl)
  (let ((l (apml_boundl (item.relation.parent syl 'SylStructure)))
	(r (apml_boundr (item.relation.parent syl 'SylStructure))))
    (if (eq (item.feat syl 'accented) 0)
	0   ; this is a quirk related to the way the models were trained
	(cond
	 ((eq l 0)
	  1)
	 ((eq r 1)
	  3)
	 (t 2)))))
  

(define (apml_iecount syl)
  (if (eq (item.feat syl 'accented) 0)
      0   ; this is a quirk related to the way the models were trained
      (+ (item.feat syl 'asyl_in) 1)))

;; suport functions.
(define (apml_boundl word)
"(apml_boundl word)
Number of boundaries in this performative to the left of this word."
  (let ((w (item.prev word))
	(c 0))
    (while (and w (apml_same_p w word))
	   (if (item.relation.parent (item.relation.parent w 'Token) 'Boundary)
	       (set! c (+ c 1)))
	   (set! w (item.prev w)))
    c))

(define (apml_boundr word)
"(apml_boundr word)
Number of boundaries in this performative to the right of this word."
  (let ((w word)
	(c 0))
    (while (and w (apml_same_p w word))
	   (if (item.relation.parent (item.relation.parent w 'Token) 'Boundary)
	       (set! c (+ c 1)))
	   (set! w (item.next w)))
    c))

(define (apml_same_p w1 w2)
"(apml_same_p w1 w2)
 Are these two words in the same performative?"
(let ((p1 (item.relation.parent (item.relation.parent w1 'Token) 'SemStructure))
      (p2 (item.relation.parent (item.relation.parent w1 'Token) 'SemStructure)))
  (if (and (item.parent p1) (item.parent p2))  ; not true if theme/rheme omitted.
      (equal? (item.parent p1) (item.parent p2))
      (equal? p1 p2))))

;;;
;;; segment timings
;;;

(define (apml_seg_times utt)
  "(apml_seg_times utt)
Output the segment timings for an apml utterance."
  (let ((segs (utt.relation.items utt 'Segment)))
    (mapcar
     (lambda (x)
       (format t "%s %s\n" (item.name x) (item.feat x 'end)))
     segs)
    t))

;;;
;;; Additional functions for f0model.
;;;


(define (find_hstar_left syl)
"(find_hstar_left syl)
If the closest accent or boundary to the left is H* return how many syllables away it is. Returns 0 if nearest accent is not H*"
(let ((count 0))
  ;; if this syllable has a pitch event
  (if (or (not (string-equal (item.feat syl 'tobi_accent) "NONE"))
	  (not (string-equal (item.feat syl 'tobi_endtone) "NONE")))
      0)
  (while (and syl
	      (string-equal (item.feat syl 'tobi_accent) "NONE")
	      (string-equal (item.feat syl 'tobi_endtone) "NONE"))
	 (set! count (+ count 1))
	 (set! syl (item.prev syl)))
  (cond
   ;; run out of syllables before finding accent
   ((null syl)
    0)
   ((string-equal (item.feat syl 'tobi_accent) "H*")
    count)
   (t 0))))

(define (find_ll_right syl)
"(find_ll_right syl)
If the closest accent or boundary to the right is L-L% return how many syllables away it is. Returns 0 if nearest is not L-L%."
(let ((count 0))
  ;; if this syllable has a pitch event
  (if (or (not (string-equal (item.feat syl 'tobi_accent) "NONE"))
	  (not (string-equal (item.feat syl 'tobi_endtone) "NONE")))
      0)
  (while (and syl
	      (string-equal (item.feat syl 'tobi_accent) "NONE")
	      (string-equal (item.feat syl 'tobi_endtone) "NONE"))
	 (set! count (+ count 1))
	 (set! syl (item.next syl)))
  (cond
   ;; run out of syllables before finding boundary
   ((null syl)
    0)
   ((string-equal (item.feat syl 'tobi_endtone) "L-L%")
    count)
   (t 0))))

(define (l_spread syl)
"(l_spread syl)
Proportion of pitch lowering required due to L- spreading backwards."
(let ((l (find_hstar_left syl))
      (r (find_ll_right syl)))
  (cond
   ((or (eq l 0)
	(eq r 0))
    0)
   (t
    (/ r (- (+ l r) 1))))))


;;;
;;; Debuging and other useful stuff.
;;;



(define (apml_print_semstruct utt)
"(apml_print_semstruct utt)
Pretty print APML semantic structure."
  (let ((i (utt.relation.first utt 'SemStructure)))
    (while (not (null i))
	   (apml_pss_item 0 i)
	   (apml_pss_daughters 1 (item.daughters i))
	   (set! i (item.next i)))))
			  
(define (apml_pss_daughters depth list)
  (mapcar
   (lambda (x)
     (apml_pss_item depth x)
     (apml_pss_daughters (+ depth 1) (item.daughters x))
     )
   list))


(define (apml_pss_item depth item)
  (let ((c 0))
    (while (< c depth)
	   (format t " ")
	   (set! c (+ c 1)))
    (format t "%s\n" (item.name item))))


(define (apml_print_words utt)
"(apml_print_words utt)
 Pretty print APML words with associated accents."
  (mapcar
   (lambda (x)
     (format t "%s (" (item.name x))
     (apml_pww_accent x)
     (apml_pww_boundary x)
     (apml_pww_pause x)
     (format t ")\n"))
   (utt.relation.items utt 'Word))
  t)

(define (apml_pww_accent item)
  (let ((p (item.relation.parent (item.relation.parent item 'Token) 'Emphasis)))
    (if p (apml_ppw_list (item.features p)))))

(define (apml_pww_boundary item)
  (let ((p (item.relation.parent (item.relation.parent item 'Token) 'Boundary)))
    (if p (apml_ppw_list (item.features p)))))

(define (apml_pww_pause item)
  (let ((p (item.relation.parent (item.relation.parent item 'Token) 'Pause)))
    (if p (apml_ppw_list (item.features p)))))

(define (apml_ppw_list l)
  (mapcar
   (lambda (x)
     (format t " %s" x))
   (flatten l)))


(define (apml_print_sylstructure utt)
"(apml_print_sylstructure utt)
Pretty print APML syllable structure."
  (mapcar
   (lambda (x)
     (format t "%s\n" (item.name x))
     (apml_psyl x))
   (utt.relation.items utt 'Word))
  t)

(define (apml_psyl word)
  (mapcar
   (lambda (x)
     (apml_psegs x)
     (if (eq (item.feat x 'stress) 1)
	 (format t " (1)"))
     (if (item.relation.daughter1 x 'Intonation)
	 (begin
	   (let ((ie (item.relation.daughter1 x 'Intonation)))
	     (format t " [")
	     (while ie
		    (format t "%s" (item.name ie))
		    (set! ie (item.next ie))
		    (if ie (format t " ")))
	     (format t "]"))))
     (format t "\n"))
   (item.daughters (item.relation word 'SylStructure))))

(define (apml_psegs syl)
  (let ((segs (item.daughters syl)))
    (format t " ")
    (while segs
	   (format t "%s" (item.name (car segs)))
	   (if (cdr segs)
	       (format t "."))
	   (set! segs (cdr segs)))))


(define (apml_get_lr_params)
  (let ((m 0)
	(s 0))
    (if (or (equal? (Parameter.get 'Int_Target_Method) Int_Targets_LR)
	    (equal? (Parameter.get 'Int_Target_Method) Int_Targets_5_LR))
	(begin
	  (set! m (car (cdr (car int_lr_params))))
	  (set! s (car (cdr (car (cdr int_lr_params))))))
	(begin
        (set! m apml_default_pitch_mean)
        (set! s apml_default_pitch_standard_deviation)))
    (list m s)))




(define (apml_initialise)
  "(apml_initialise)
Set up the current voice for apml use."
  (if (not (string-matches current-voice ".*multisyn.*")) ; nothing if multisyn
      (cond
       ((or (string-equal (Parameter.get 'Language) "americanenglish")
	    (string-equal (Parameter.get 'Language) "britishenglish"))
	(begin
	  (format t "Initialising APML for English.\n")
	  ;; Phrasing.
	  (Parameter.set 'Phrase_Method 'cart_tree)
	  (set! phrase_cart_tree apml_phrase_tree)
	  ;; Pauses.
	  ;;(set! duration_cart_tree apml_kal_duration_cart_tree)
	  ;;(set! duration_ph_info apml_kal_durs)
	  ;;(Parameter.set 'Pause_Method Apml_Pauses)
	  ;; Lexicon.
      ;;;; We now assume the lexicon you have already set is suitable,
      ;;;; You probably want to ensure this is "apmlcmu" or "unilex"
	  ;;(if (not (member_string "apmlcmu" (lex.list)))
	  ;;  (load (path-append lexdir "apmlcmu/apmlcmulex.scm")))
	  ;;(lex.select "apmlcmu")
	  ;; Add other lex entries here:
	  ;;(lex.add.entry '("minerals" nil (((m ih n) 1) ((er) 0) ((ax l z) 0))))
	  ;;(lex.add.entry '("fibre" nil (((f ay b) 1) ((er) 0))))
	  ;;(lex.add.entry '("dont" v (((d ow n t) 1))))
	  ;;(lex.add.entry '("pectoris" nil (((p eh k) 2) ((t ao r) 1) ((ih s) 0))))
	  ;;(lex.add.entry '("sideeffects" nil (((s ay d) 1) ((ax f) 0) ((eh k t s) 2))))
	  
	  ;; Intonation events.
	  (set! int_accent_cart_tree apml_accent_cart)
	  (set! int_tone_cart_tree   apml_boundary_cart)
	  (Parameter.set 'Int_Method Intonation_Tree)
	  ;; Intonation f0 contour.
	  (set! f0_lr_start apml_f2b_f0_lr_start)
	  (set! f0_lr_left apml_f2b_f0_lr_left)
	  (set! f0_lr_mid apml_f2b_f0_lr_mid)
	  (set! f0_lr_right apml_f2b_f0_lr_right)
	  (set! f0_lr_end apml_f2b_f0_lr_end)
	  (set! int_lr_params
		(list (list 'target_f0_mean (car (apml_get_lr_params)))
		      (list 'target_f0_std (car (cdr (apml_get_lr_params))))
		      (list 'model_f0_mean 170)
		      (list 'model_f0_std 40)))
	  (Parameter.set 'Int_Target_Method Int_Targets_5_LR)
	  nil))
       ((string-equal (Parameter.get 'Language) "italian")
	(begin
	  (format t "Initialising APML for Italian.\n")
	  ;; Phrasing.
	  (Parameter.set 'Phrase_Method 'cart_tree)
	  (set! phrase_cart_tree apml_phrase_tree)
	  ;; Intonation events.
	  (set! int_accent_cart_tree apml_accent_cart)
	  (set! int_tone_cart_tree   apml_boundary_cart)
	  (Parameter.set 'Int_Method Intonation_Tree)
	  ;; Intonation f0 contour.
	  (set! f0_lr_start apml_f2b_f0_lr_start)
	  (set! f0_lr_mid apml_f2b_f0_lr_mid)
	  (set! f0_lr_end apml_f2b_f0_lr_end)
	  (set! int_lr_params
		(list (list 'target_f0_mean (car (apml_get_lr_params)))
		      (list 'target_f0_std (car (cdr (apml_get_lr_params))))
		      (list 'model_f0_mean 170)
		      (list 'model_f0_std 34)))
	  (Parameter.set 'Int_Target_Method Int_Targets_LR)
	  nil))
   (t nil))))

(provide 'apml)
