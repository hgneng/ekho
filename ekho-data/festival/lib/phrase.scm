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
;;;   Phrase boundary prediction.
;;;   
;;;   Two methods supported, if POS is enabled we use ngrams for that
;;;   otherwise we use a CART tree
;;;
;;;   Models trained from the IBM/Lancaster Spoken English Corpus and 
;;;   Boston University's FM Radio Corpus.

;;;
;;;  Here's a very simple CART tree for predicting phrase breaks
;;;  based on punctuation only
;;;
(set! simple_phrase_cart_tree
'
((lisp_token_end_punc in ("?" "." ":"))
  ((BB))
  ((lisp_token_end_punc in ("'" "\"" "," ";"))
   ((B))
   ((n.name is 0)  ;; end of utterance
    ((BB))
    ((NB))))))

(define (token_end_punc word)
  "(token_end_punc UTT WORD)
If punctuation at end of related Token and if WORD is last word
in Token return punc, otherwise 0."
  (if (item.relation.next word "Token")
      "0"
      (item.feat word "R:Token.parent.punc")))

;;;  This is a simple CART tree used after boundaries are predicted
;;;  by the probabilistic method to get two levels of break
(set! english_phrase_type_tree
'((pbreak is NB)
  ((num_break is 1)
   ((mB))
   ((R:Token.parent.EMPH is 1)
    ((NB))
    ((n.R:Token.parent.EMPH is 1)
     ((NB))
     ((NB)))))
  ((pbreak is BB)
   ((BB))
   ((pbreak is mB)
    ((mB))
    ((name in ("." "!" "?"));; only (potentially) change Bs to BBs
     ((BB))
     ((B)))))))

(set! f2b_phrase_cart_tree
'
((gpos is punc)
 (((1 0.00238095) (3 0) (4 0.997619) B))
 (((4 0.00238095) (3 0) (1 0.997619) NB))))

;;;  For more detailed prediction of phrase breaks we use POS and
;;;  probability distribution of breaks
;;;  These models were trained using data from the Lancaster/IBM
;;;  Spoken English Corpus

(require 'pos)   ;; for part of speech map

(defvar pbreak_ngram_dir libdir
  "pbreak_ngram_dir
  The directory containing the ngram models for predicting phrase
  breaks.  By default this is the standard library directory.")

(defvar english_phr_break_params
  (list
   ;; The name and filename off the ngram with the a priori ngram model
   ;; for predicting phrase breaks in the Phrasify module.  This model should 
   ;; predict probability distributions for B and NB given some context of 
   ;; part of  speech tags.
   (list 'pos_ngram_name 'english_break_pos_ngram)
   (list 'pos_ngram_filename
	 (path-append pbreak_ngram_dir "sec.ts20.quad.ngrambin"))
   ;; The name and filename of the ngram  containing the a posteriori ngram
   ;; for predicting phrase breaks in the Phrasify module.  This module should
   ;; predict probability distributions for B and NB given previous B and
   ;; NBs.
   (list 'break_ngram_name 'english_break_ngram)
   (list 'break_ngram_filename
	 (path-append pbreak_ngram_dir "sec.B.hept.ngrambin"))
   ;; A weighting factor for breaks in the break/non-break ngram.
   (list 'gram_scale_s 0.59)
   ;; When Phrase_Method is prob_models, this tree, if set is used to 
   ;; potentially predict phrase type.  At least some prob_models only
   ;; predict B or NB, this tree may be used to change some Bs into
   ;; BBs.  If it is nil, the pbreak value predicted by prob_models
   ;; remains the same.
   (list 'phrase_type_tree english_phrase_type_tree)
   ;; A list of tags used in identifying breaks.  Typically B and NB (and
   ;; BB).  This should be the alphabet of the ngram identified in
   ;; break_ngram_name
   (list 'break_tags '(B NB))
   (list 'pos_map english_pos_map_wp39_to_wp20)
   )
  "english_phr_break_params
Parameters for English phrase break statistical model.")

(defvar phr_break_params nil
  "phr_break_params
Parameters for phrase break statistical model.  This is typcal set by
a voice selection function to the parameters for a particular model.")

;;;
;;; Declaration of some features 
;;; 

(def_feature_docstring 
  'Word.pbreak
  "Word.pbreak
  Result from statistical phrasing module, may be B or NB denoting
  phrase break or non-phrase break after the word.")

(def_feature_docstring 
  'Word.pbreak_score
  "Word.pbreak_score
  Log likelihood score from statistical phrasing module, for pbreak
  value.")

(def_feature_docstring 
  'Word.blevel
  "Word.blevel
  A crude translation of phrase break into ToBI like phrase level.
  Values may be 0,1,2,3,4.")

(define (Phrasify utt)
"(Phrasify utt)                                
Construct phrasify over Words module."
  (let ((rval (apply_method 'Phrasify_Method utt)))
    (cond
     (rval rval) ;; new style
     (t
      (Classic_Phrasify utt)))))


(provide 'phrase)
