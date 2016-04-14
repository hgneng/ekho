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
;;;  Some (experimental) data for investigating metrical trees
;;;

;;; Set up generation of metrical tree, this includes getting
;;; a syntactic parse
;;;
;;; Use as 
;;;   (set! utt1 (metsynth (Utterance Text "For afternoon tea")))
;;;   (utt.relation_tree utt1 'MetricalTree)

(require 'scfg)
(set! scfg_grammar (load (path-append libdir "scfg_wsj_wp20.gram") t))

(define (mettext utt)
  (Initialize utt)
  (Text utt)
  (Token_POS utt)
  (Token utt)
  (POS utt)
  (print "here1")
  (Phrasify utt)
  (print "here2")
  (ProbParse utt)
  (print "here3")
  (auto_metrical_tree utt)
)

(define (metsynth utt)
  (mettext utt)
  (Wave_Synth utt)
)

;;; Assumed everything is using Roger diphones

;;(lex.create "cmu_mettree")
;;;(lex.set.phoneset "radio_phones")
;;(lex.set.phoneset "radio_phones")

(define (setup_cmu_mettree_lex)
  "(setup_cmu_mettreelex)
Lexicon derived from the CMU lexicon (cmudict-0.1), around 100,000 entries,
in the radio phoneset (sort of darpa-like)."
  (if (not (member_string "cmu_mettree" (lex.list)))
      (begin
	(print "making cmu lexicon")
	(lex.create "cmu_mettree")
	(lex.set.compile.file (path-append lexdir "cmu_mettree_lex.out"))
	(lex.set.phoneset "radio")
	(require 'lts__us)    ;; US English letter to sound rules
	(lex.set.lts.method 'lts_rules)
	(lex.set.lts.ruleset 'nrl_us))))

(provide 'mettree)


