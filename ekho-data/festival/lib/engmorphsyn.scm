;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
;;;                         Copyright (c) 1997                            ;;
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
;;;                         Date:   December 1997
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  
;;;   THIS IS EXPERIMENTAL AND DOES *NOT* WORK
;;;
;;;  
;;;  An English morpho-syntax finite-state grammar
;;;  This is used for morphological decomposition of unknown words
;;;  specifically (only) words that are not found in the lexicon.
;;;  This idea is that when an unknown word is found an attempt is made
;;;  to see if it contains any well known morphological inflections or
;;;  derivations, if so a better use of LTS can be made on the root, of
;;;  none are found this
;;;
;;;
;;;  Based on "Analysis of Unknown Words through Morphological 
;;;  Decomposition", Black, van de Plassche, Willians, European ACL 91.
;;;  with the anyword matcher from a question by Lauri Karttunen after
;;;  the talk.
;;;
;;;  The suffixes and finite-state morph-syntax grammar is based 
;;;  (very roughly) on the rules in "Computational Morphology"
;;;  Ritchie et al. MIT Press 1992.
;;;
;;;  Can be compiled with
;;;   wfst_build -type rg -o engmorphsyn.wfst -detmin engmorphsyn.scm
;;;
;;;  The result can be combined with the morphographemic rules 
;;;  with
;;;   	wfst_build -type compose engmorph.wfst engmorphsyn.wfst -detmin -o engstemmer.wfst
;;;
;;;  echo "# b o x e/+ s #" | wfst_run -wfst engstemmer.wfst -recog
;;;  state 0 #/# -> 1
;;;  state 1 b/b -> 3
;;;  state 3 o/o -> 17
;;;  state 17 x/x -> 14
;;;  state 14 e/+ -> 36
;;;  state 36 s/s -> 34
;;;  state 34 #/# -> 16
;;;  OK.
;;;  echo "# b o x e s #" | wfst_run -wfst engstemmer.wfst -recog
;;;  state 0 #/# -> 1
;;;  state 1 b/b -> 3
;;;  state 3 o/o -> 17
;;;  state 17 x/x -> 14
;;;  state 14 e/e -> 22
;;;  state 22 s/s -> -1

(RegularGrammar
 engsuffixmorphosyntax
 ;; Sets 
 (
 (V a e i o u y)
 (C b c d f g h j k l m n p q r s t v w x y z)
 )
 ;; Rules

 (
 ;; A word *must* have a suffix to be recognized
 (Word -> # Syls Suffix )
 (Word -> # Syls End )

 ;; This matches any string of characters that contains at least one vowel
 (Syls -> Syl Syls )
 (Syls -> Syl )
 (Syl -> Cs V Cs )
 (Cs -> C Cs )
 (Cs -> )

 (Suffix -> VerbSuffix )
 (Suffix -> NounSuffix )
 (Suffix -> AdjSuffix )
 (VerbSuffix -> VerbFinal End )
 (VerbSuffix -> VerbtoNoun NounSuffix )
 (VerbSuffix -> VerbtoNoun End )
 (VerbSuffix -> VerbtoAdj  AdjSuffix )
 (VerbSuffix -> VerbtoAdj End )
 (NounSuffix -> NounFinal End )
 (NounSuffix -> NountoNoun NounSuffix )
 (NounSuffix -> NountoNoun End )
 (NounSuffix -> NountoAdj AdjSuffix )
 (NounSuffix -> NountoAdj End )
 (NounSuffix -> NountoVerb VerbSuffix )
 (NounSuffix -> NountoVerb End )
 (AdjSuffix -> AdjFinal End )
 (AdjSuffix -> AdjtoAdj AdjSuffix)
 (AdjSuffix -> AdjtoAdj End)
 (AdjSuffix -> AdjtoAdv End)  ;; isn't any Adv to anything

 (End -> # )  ;; word boundary symbol *always* present

 (VerbFinal -> + e d)
 (VerbFinal -> + i n g)
 (VerbFinal -> + s)

 (VerbtoNoun -> + e r)
 (VerbtoNoun -> + e s s)
 (VerbtoNoun -> + a t i o n)
 (VerbtoNoun -> + i n g)
 (VerbtoNoun -> + m e n t)

 (VerbtoAdj -> + a b l e)

 (NounFinal -> + s)

 (NountoNoun -> + i s m)
 (NountoNoun -> + i s t)
 (NountoNoun -> + s h i p)

 (NountoAdj -> + l i k e)
 (NountoAdj -> + l e s s)
 (NountoAdj -> + i s h)
 (NountoAdj -> + o u s)

 (NountoVerb -> + i f y)
 (NountoVerb -> + i s e)
 (NountoVerb -> + i z e)

 (AdjFinal -> + e r)
 (AdjFinal -> + e s t)

 (AdjtoAdj -> + i s h)
 (AdjtoAdv -> + l y)
 (AdjtoNoun -> + n e s s)
 (AdjtoVerb -> + i s e)
 (AdjtoVerb -> + i z e)

)
)








