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
;;;  Koskenniemi-style context rewrite rules for English Morphographemics
;;;  Basically splits words into their (potential) morphemes.
;;;
;;;  Based (roughly) on the rules in "Computational Morphology"
;;;  Ritchie et al. MIT Press 1992.
;;;
;;;  This is not a Scheme file and can't be loaded and evaluated
;;;  It is designed for use with the wfst tools in the speech tools
;;;  e.g. wfst_build -type kk -o engmorph.wfst -detmin engmorph.scm
;;;

(KKrules
 engmorph
 (Alphabets
  ;; Input Alphabet
  (a b c d e f g h i j k l m n o p q r s t u v w x y z #) 
  ;; Output Alphabet
  (a b c d e f g h i j k l m n o p q r s t u v w x y z + #) 
 )
 (Sets
  (LET a b c d e f g h i j k l m n o p q r s t u v w x y z)
 )
 (Rules
 ;; The basic rules
 ( a => nil --- nil) 
 ( b => nil --- nil) 
 ( c => nil --- nil) 
 ( d => nil --- nil) 
 ( e => nil --- nil) 
 ( f => nil --- nil) 
 ( g => nil --- nil) 
 ( h => nil --- nil) 
 ( i => nil --- nil) 
 ( j => nil --- nil) 
 ( k => nil --- nil) 
 ( l => nil --- nil) 
 ( m => nil --- nil) 
 ( n => nil --- nil) 
 ( o => nil --- nil) 
 ( p => nil --- nil) 
 ( q => nil --- nil) 
 ( r => nil --- nil) 
 ( s => nil --- nil) 
 ( t => nil --- nil) 
 ( u => nil --- nil) 
 ( v => nil --- nil) 
 ( w => nil --- nil) 
 ( x => nil --- nil) 
 ( y => nil --- nil) 
 ( z => nil --- nil) 
 ( # => nil --- nil)
; ( _epsilon_/+ => (or LET _epsilon_/e ) --- (LET))
 ( _epsilon_/+ => (or LET _epsilon_/e) --- nil)

 ;; The rules that do interesting things
 
 ;; Epenthesis
 ;;   churches -> church+s
 ;;   boxes -> box+s
 (e/+ <=> (or (s h) (or s x z) (i/y) (c h))
	    ---
	    (s))
 ;; Gemination
 (b/+ <=> ( (or b c d f g h j k l m n p q r s t v w z) (or a e i o u y) b )
	   ---
	   ((or a e i o u)))
 (d/+ <=> ((or b c d f g h j k l m n p q r s t v w z) (or a e i o u y) d )
          ---
	  ((or a e i o u)))
 (f/+ <=> ((or b c d f g h j k l m n p q r s t v w z) (or a e i o u y) f )
          ---
	  ((or a e i o u)))
 (g/+ <=> ((or b c d f g h j k l m n p q r s t v w z) (or a e i o u y) g )
          ---
	  ((or a e i o u)))
 (m/+ <=> ((or b c d f g h j k l m n p q r s t v w z) (or a e i o u y) m )
          ---
	  ((or a e i o u)))
 (p/+ <=> ((or b c d f g h j k l m n p q r s t v w z) (or a e i o u y) p )
          ---
	  ((or a e i o u)))
 (s/+ <=> ((or b c d f g h j k l m n p q r s t v w z) (or a e i o u y) s )
          ---
	  ((or a e i o u)))
 (t/+ <=> ((or b c d f g h j k l m n p q r s t v w z) (or a e i o u y) t )
          ---
	  ((or a e i o u)))
 (z/+ <=> ((or b c d f g h j k l m n p q r s t v w z) (or a e i o u y) z )
          ---
	  ((or a e i o u)))
 (n/+ <=> ((or b c d f g h j k l m n p q r s t v w z) (or a e i o u y) n )
          ---
	  ((or a e i o u)))
 (l/+ <=> ((or b c d f g h j k l m n p q r s t v w z) (or a e i o u y) l )
          ---
	  ((or a e i o u)))
 (r/+ <=> ((or b c d f g h j k l m n p q r s t v w z) (or a e i o u y) r )
          ---
	  ((or a e i o u)))
 ;; tries->try+s
 ( i/y <=>  ((or b c d f g h j k l m n p q r s t v w x z))
            ---
	    ((or ( e/+ s )
		 ( _epsilon_/+ (or a d e f h i l m n o p s w y)))))
 ;; Elision
 ;;   moved -> move+ed
 (_epsilon_/e <=> 
	      ((or a e i o u ) (or b c d f g j k l m n p q r s t v x z))
	      ---
	      ( _epsilon_/+ (or a e i o u )))

 )
)
