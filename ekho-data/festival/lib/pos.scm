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
;;;   A part of speech tagger
;;;

(set! english_guess_pos
      '((in of for in on that with by at from as if that against about 
	    before because if under after over into while without
	    through new between among until per up down)
	(to to)
	(det the a an no some this that each another those every all any 
	     these both neither no many)
	(md will may would can could should must ought might)
	(cc and but or plus yet nor)
	(wp who what where how when)
	(pps her his their its our their its mine)
	(aux is am are was were has have had be)
	(punc "." "," ":" ";" "\"" "'" "(" "?" ")" "!")
	))

(defvar guess_pos english_guess_pos
  "guess_pos
  An assoc-list of simple part of speech tag to list of words in that
  class.  This basically only contains closed class words all other 
  words may be assumed to be content words.  This was built from information
  in the f2b database and is used by the ffeature gpos.")

;;;  A more elaborate part of speech tagger using ngrams works but
;;;  at present requires a large list of a priori probabilities
;;;  to work.  If that file exists on your system we'll use it otherwise
;;;  POS is guessed by the lexicon

;;;  These models were build from the Penn TreeBank, WSJ corpus

(defvar pos_model_dir lexdir
  "pos_model_dir
  The directory contains the various models for the POS module.  By
  default this is the same directory as lexdir.  The directory should
  contain two models: a part of speech lexicon with reverse log probabilities
  and an ngram model for the same part of speech tag set.")

(defvar pos_p_start_tag "punc"
  "pos_p_start_tag
  This variable's value is the tag most likely to appear before
  the start of a sentence.  It is used when looking for pos context
  before an utterance.  Typically it should be some type of punctuation
  tag.")

(defvar pos_pp_start_tag "n"
  "pos_pp_start_tag
  This variable's value is the tag most likely to appear before
  pos_p_start_tag and any position preceding that.  It is typically
  some type of noun tag.  This is used to provide pos context for
  early words in an utterance.")

(defvar pos_supported nil
  "pos_supported
  If set to non-nil use part of speech prediction, if nil just get
  pos information from the lexicon.")

(defvar pos_ngram_name nil
  "pos_ngram_name
  The name of a loaded ngram containing the a posteriori ngram model for 
  predicting part of speech.  The a priori model is held as a 
  lexicon call poslex.")

(defvar pos_map nil
  "pos_map
  If set this should be a reverse assoc-list mapping on part of speech
  tag set to another.  It is used after using the defined POS models to
  map the pos feature on each word to a new tagset.")

;;;
;;;  All the names here don't really allow multiple versions
;;;  they should be prefixed with english_
;;;

(if (probe_file (path-append pos_model_dir "wsj.wp39.poslexR"))
    (begin
      (lex.create "english_poslex")
      (lex.set.compile.file 
       (path-append pos_model_dir "wsj.wp39.poslexR"))
      (lex.set.phoneset "mrpa")
      (lex.set.lts.method nil)
      (set! pos_lex_name "english_poslex")
      (set! pos_p_start_tag "punc")
      (set! pos_pp_start_tag "nn")
      ;; wp39
      (lex.add.entry '("_OOV_" ((nnp -2.9144) (jj -2.7357) (nn -3.5787)
				(nns -3.4933) (vbn -3.2486) (vbg -2.9419)
				(vb  -3.5471) (vbd -3.7896) (vbz -3.7820)
				(rb  -4.1940) (vbp -3.2755) (nnps -2.1605))
			       ()))
      (lex.add.entry '("_number_" 
		       ((cd -0.35202) (jj -4.1083) (nns -6.4488) (nnp -7.3595))
		       () ))
      (lex.add.entry '("," ((punc -0.88488)) () ))
      (lex.add.entry '("." ((punc -1.1104)) () ))
      (lex.add.entry '(":" ((punc -4.4236)) () ))
      (lex.add.entry '("``" ((punc -2.7867)) () ))
      (lex.add.entry '("`" ((punc -2.7867)) () ))
      (lex.add.entry '("'" ((punc -2.7867)) () ))
      (lex.add.entry '("\"" ((punc -2.7867)) () ))
      ;; wp17
;;      (lex.add.entry '("_OOV_" ((n -3.4109) (j -2.7892) (v -3.7426)) ()))
;      (lex.add.entry '("_OOV_" ((n -1.968) (j -2.351) (v -2.287)) ()))
;      (lex.add.entry '("_number_" ((j -0.35202)) ()))
;      (lex.add.entry '("," ((punc -0.88359)) () ))
;      (lex.add.entry '("." ((punc -1.1101)) () ))
;      (lex.add.entry '(":" ((punc -4.4236)) () ))
;      (lex.add.entry '("``" ((punc -2.7867)) () ))
;      (lex.add.entry '("`" ((punc -2.7867)) () ))
;      (lex.add.entry '("'" ((punc -2.7867)) () ))
;      (lex.add.entry '("\"" ((punc -2.7867)) () ))
      ;; wp22
;      (lex.add.entry '("_OOV_" ((n -3.4109) (j -2.7892) (v -3.7426)) ()))
;      (lex.add.entry '("_number_" ((cd -0.35202) (j -4.1908) (n -7.3890)) ()))
;      (lex.add.entry '("," ((punc -0.88359)) () ))
;      (lex.add.entry '("." ((punc -1.1101)) () ))
;      (lex.add.entry '(":" ((punc -4.4236)) () ))
;      (lex.add.entry '("``" ((punc -2.7867)) () ))
      ;; wp18 
;      (lex.add.entry '("_OOV_" ((n -3.4109) (j -2.7892) (v -3.7426)) ()))
;      (lex.add.entry '("_number_" ((j -0.35202)) ()))
;      (lex.add.entry '("`" ((punc -6.539) ) () ))
;      (lex.add.entry '("``" ((punc -2.399) ) () ))
;      (lex.add.entry '("," ((punc -0.480) ) () ))
;      (lex.add.entry '("." ((fpunc -0.012) ) () ))
;      (lex.add.entry '(":" ((punc -4.100) ) () ))

     (ngram.load 'english_pos_ngram
	    (path-append pos_model_dir  "wsj.wp39.tri.ngrambin"))
;      (ngram.load 'english_pos_ngram
;	    (path-append pos_model_dir  "wsj.wp45.tri.ngram"))
      (set! pos_supported t)
      )
    (set! pos_supported nil))

(setq english_pos_map_wp39_to_wp20
      '(
	(( vbd vb vbn vbz vbp vbg ) v)
	(( nn nnp nns nnps fw sym ls ) n)
	(( dt ) dt)
	(( punc fpunc ) punc)
	(( in ) in)
	(( jj jjr jjs 1 2 ) j)
	(( prp ) prp)
	(( rb rp rbr rbs ) r)
	(( cc ) cc)
	(( of ) of)
	(( to ) to)
	(( cd ) cd)
	(( md ) md)
	(( pos ) pos)
	(( wdt ) wdt)
	(( wp ) wp)
	(( wrb ) wrb)
	(( ex ) ex)
	(( uh ) uh)
	(( pdt ) pdt)
	))

(defvar pos_map nil
  "pos_map
A reverse assoc list of predicted pos tags to some other tag set.  Note
using this changes the pos tag loosing the actual predicted value.  Rather
than map here you may find it more appropriate to map tags sets locally
in the modules that use them (e.g. phrasing and lexicons).")

;;(setq pos_map_remap
;;      '(
;;	(( fpunc ) punc)
;;	(( of ) in)))

(def_feature_docstring 'Word.pos
  "Word.pos
  Part of speech tag value returned by the POS tagger module.")

(def_feature_docstring 'Word.pos_score
  "Word.pos_score
  Part of speech tag log likelihood from Viterbi search.")

(define (POS utt)
"(POS utt)                                
Apply part of speech tagging (and possible parsing too) to Word
relation."
  (let ((rval (apply_method 'POS_Method utt)))
    (cond
     (rval rval) ;; new style
     (t
      (Classic_POS utt)))))


(provide 'pos)
