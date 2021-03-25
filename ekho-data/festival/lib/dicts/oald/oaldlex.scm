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
;;;  Definition of Festival lexicon, derived from Oxford Advanced
;;;  Learners' Dictionary of Contemporary English
;;;
;;;  NOTE: OALD is free for non-commercial use only
;;;
;;;  Using the trained LTS model we have managed to remove 39451 (out
;;;  of 70646 (55.84%)) that can be properly predicted by the 
;;;  the model.

(defvar oaldlexdir (path-append lexdir "oald"))

(require 'pos)

(define (oald_lts_function word feats)
  "(oald_lts_function word feats)
Function called for OALD when word is not found in lexicon.  Uses
LTS rules trained from the original lexicon, and lexical stress
prediction rules."
  (require 'lts)
  (if (not (boundp 'oald_lts_rules))
      (load (path-append oaldlexdir "oald_lts_rules.scm")))
  (let ((dcword (downcase word))
	(syls) (phones))
    (if (string-matches dcword "[a-z]*")
	(begin
	  (set! lts_pos feats)
	  (set! phones (lts_predict dcword oald_lts_rules))
	  (set! syls (lex.syllabify.phstress phones))
	  )
	(set! syls nil))
    (list word nil syls)))

(define (mrpa_addenda)
"(mrpa_addenda)
Add a whole host of various entries to the current lexicon with
mrpa phones."
  (lex.add.entry 
   '("a" dt (((@) 0))))
  (lex.add.entry 
   '("a" n (((ei) 1))))
  (lex.add.entry 
   '("ac" n (((ei) 1) ((s ii) 1))))
  (lex.add.entry
   '("us" prp (((uh s) 0)) ((pos "Qx*"))))
  (lex.add.entry
   '("'s" pos (((@ z) 0))))
  (lex.add.entry
   '("'s" n (((@ z) 0))))
  (lex.add.entry 
   '("the" dt (((dh @) 0))))
  (lex.add.entry
   '("taylor" n (((t ei) 1) ((l @) 0))))
  (lex.add.entry
   '("who" prp ((( h uu ) 0))))
  (lex.add.entry 
   '("algorithm" n (((a l g) 1) ((o) 0) ((r i th m) 0))))
  (lex.add.entry 
   '("algorithms" n (((a l g) 1) ((o) 0) ((r i th m z) 0))))
  (lex.add.entry 
   '("algorithmic" n (((a l g) 1) ((o) 0) ((r i th) 1) ((m i k) 0))))
  (lex.add.entry 
   '("alices" n (((a l) 1) ((i s) 0) ((i z) 0))))
  (lex.add.entry 
   '( "Angeles" n (((a n) 1) ((jh i) 0) ((l ii z) 0))))
  (lex.add.entry 
   '( "atr" n ((( ei ) 1) (( t ii ) 1) (( a a ) 1))))
  (lex.add.entry 
   '( "att" n ((( ei ) 1) (( t ii ) 1) (( a n d ) 0) (( t ii ) 1))))
  (lex.add.entry 
   '( "awb" n ((( ei ) 1) ((d uh) 1) ((b @ l) 0) ((y uu) 0) ((b ii) 1))))
  (lex.add.entry
   '("color" n (((k uh l) 1) ((@ r) 0))))
  (lex.add.entry
   '("colors" n (((k uh l) 1) ((@ r z) 0))))
  (lex.add.entry
   '("colored" j (((k uh l) 1) ((@ r d) 0))))
  (lex.add.entry
   '("coloring" j (((k uh l) 1) ((@ r) 0) (( i ng ) 0))))
  (lex.add.entry
   '("cdrom" n (((s ii) 1) ((d ii) 1) ((r o m) 1))))
  (lex.add.entry
   '("cdroms" n (((s ii) 1) ((d ii) 1) ((r o m z) 1))))
  (lex.add.entry 
   '("cepstra" n (((k e p) 1) ((s t r @) 0))))
  (lex.add.entry 
   '("cepstral" n (((k e p) 1) ((s t r @ l) 0))))
  (lex.add.entry 
   '("cepstrum" n (((k e p) 1) ((s t r @ m) 0))))
  (lex.add.entry 
   '("co" nil (((k ou) 1))))
  (lex.add.entry 
   '( "cstr" n ((( s ii ) 1) (( e s ) 1) (( t ii ) 1) (( aa ) 1)) ))
  (lex.add.entry 
   '( "cogsci" n ((( k o g ) 1) (( s ai) 1))))
  (lex.add.entry
   '("database" n (((d ei) 1) ((t @) 0) ((b ei s) 1))))
  (lex.add.entry
   '("databases" n (((d ei) 1) ((t @) 0) ((b ei s) 1) ((i z) 0))))
  (lex.add.entry
   '("diphone" n (((d ai) 1) ((f ou n) 0))))
  (lex.add.entry
   '("diphones" n (((d ai) 1) ((f ou n s) 0))))
  (lex.add.entry 
   '( "edinburgh" n ((( e d ) 1) (( i n ) 0) ((b r @) 0))))
  (lex.add.entry 
   '( "email" n ((( ii ) 1) (( m ei l) 0))))
  (lex.add.entry 
   '( "emailed" n ((( ii ) 1) (( m ei l d) 0))))
  (lex.add.entry 
   '( "emacs" n ((( ii ) 1) (( m a k s) 0))))
  (lex.add.entry 
   '( "favorite" j (((f ei v) 1) ((@ r) 0) ((i t) 0))))
  (lex.add.entry 
   '( "favor" n (((f ei v) 1) ((@ r) 0))))
  (lex.add.entry 
   '( "favors" n (((f ei v) 1) ((@ r z) 0))))
  (lex.add.entry 
   '( "favoring" n (((f ei v) 1) ((@ r) 0) ((i ng) 0))))
  (lex.add.entry 
   '( "favored" n (((f ei v) 1) ((@ r d) 0))))
  (lex.add.entry 
   '("globally" a (((g l ou b) 1) ((@ l) 0) ((ii) 0))))
  (lex.add.entry 
   '("gorbachev" m (((g oo b) 1) ((@) 0) ((ch e v) 0))))
  (lex.add.entry
   '("grave" n (((g r ei v) 1)) ((pos "Kj%"))))
  (lex.add.entry
   '("graves" j (((g r ei v z) 1)) ((pos "Kj%"))))
  (lex.add.entry
   '("greece" n (((g r ii s) 1)) ((pos "Nm%"))))
  (lex.add.entry 
   '("hong" j (((h o ng) 1))))
  (lex.add.entry 
   '("hz" n (((h @@ t z) 1))))
  (lex.add.entry 
   '("lead" led (((l e d) 1))))
  (lex.add.entry 
   '("lead" liid (((l ii d) 1))))
  (lex.add.entry 
   '("innovative" j (((i n) 1) ((@) 0) ((v ei t) 1) ((i v) 0))))
  (lex.add.entry 
   '("job" n (((jh o b) 1))))
  (lex.add.entry
   '("jobs" n (((jh o b z) 1))))
  (lex.add.entry 
   '( "Jr" n (((jh uu n) 1) ((i@) 0)) ((pos "K6%" "OA%"))))
  (lex.add.entry 
   '("kong" n (((k o ng) 1))))
  (lex.add.entry 
   '("khz" n (((k i) 1) ((l ou) 0) ((h @@ t z) 1))))
  (lex.add.entry
   '("labor" n (((l ei) 1) ((b @) 0))))
  (lex.add.entry 
   '( "Los" n ((( l o s) 1))))
  (lex.add.entry 
   '("lower" v (((l ou) 1) ((@) 0))))
  (lex.add.entry 
   '("lowered" v (((l ou) 1) ((@ d) 0))))
  (lex.add.entry 
   '("lowering" v (((l ou) 1) ((@ r) 0) (( i ng ) 0))))
  (lex.add.entry 
   '( "mbrola" n (((e m) 0) ((b r ou l ) 1) (( @ ) 0))))
  (lex.add.entry 
   '("mhz" n (((m e) 1) ((g @) 0) ((h @@ t z) 1))))
  (lex.add.entry 
   '("minute" n (((m i n) 1) ((i t) 0))))
  (lex.add.entry 
   '("ms" n (((e m) 1) ((e s) 1))))
  (lex.add.entry 
   '("no" dt (((n ou) 1))))
  (lex.add.entry 
   '("pault" n ((( p oo l) 1) ((t ii) 1))))
  (lex.add.entry 
   '("put" v (((p u t) 1))))
  (lex.add.entry 
   '("putting" v (((p u t) 1) (( i ng) 0))))
  (lex.add.entry 
   '( "psola" n ((( p i ) 0) (( s ou  ) 1) (( l @ ) 0 ))))
  (lex.add.entry 
   '("read" red (((r e d) 1))))
  (lex.add.entry 
   '("read" riid (((r ii d) 1))))
  (lex.add.entry 
   '("reuter" n (((r oi) 1) ((t @@) 0))))
  (lex.add.entry 
   '("reuters" n (((r oi) 1) ((t @@ s) 0))))
  (lex.add.entry
   '("row" v (((r ou) 1))))
  (lex.add.entry
   '("row" n (((r ou) 1))))
  (lex.add.entry 
   '( "San" n ((( s a n) 1))))
  (lex.add.entry
   '("second" n (((s e k) 1) ((@ n d) 0))))
  (lex.add.entry
   '("seconds" n (((s e k) 1) ((@ n d z) 0))))
  (lex.add.entry
   '("sixteenth" n (((s i k) 1) ((s t ii n th) 1)) ((pos "K6%" "OA%"))))
  (lex.add.entry
   '("sony" n (((s ou) 1) ((n ii) 1))))
  (lex.add.entry
   '("SSML" v (((e s) 1) ((e s) 1) ((e m) 1) ((e l) 0))))
  (lex.add.entry 
   '("sun" n (((s uh n) 1))))
  (lex.add.entry
   '("synthesise" v (((s i n th) 1) ((@ s) 0) ((ai z) 0))))
  (lex.add.entry
   '("synthesised" v (((s i n th) 1) ((@ s) 0) ((ai z d) 0))))
  (lex.add.entry
   '("synthesizer" n (((s i n th) 1) ((@ s) 0) ((ai z) 0) ((@) 0))))
  (lex.add.entry
   '("synthesiser" n (((s i n th) 1) ((@ s) 0) ((ai z) 0) ((@) 0))))
  (lex.add.entry
   '("synthesizers" n (((s i n th) 1) ((@ s) 0) ((ai z) 0) ((@ s) 0))))
  (lex.add.entry
   '("synthesisers" n (((s i n th) 1) ((@ s) 0) ((ai z) 0) ((@ s) 0))))
  (lex.add.entry 
   '( "tobi" n ((( t ou ) 1) (( b ii ) 0))))
  (lex.add.entry 
   '("todays" n (((t @ d) 0) ((d ei s) 1))))
  (lex.add.entry 
   '( "tts" n ((( t ii ) 1) (( t ii ) 1) (( e s ) 1))))
  (lex.add.entry 
   '( "ulaw" n ((( m y uu ) 1) (( l oo ) 0))))
  (lex.add.entry 
   '( "uk" n ((( y uu ) 1) (( k ei ) 1))))
  (lex.add.entry
   '("waveform" n  (((w ei v) 1) ((f oo m) 0))))
  (lex.add.entry
   '("winds" n (((w i n d z) 1)) ))
  (lex.add.entry
   '("wind" v (((w ai n d) 1)) ))
  (lex.add.entry 
   '("within" a (((w i dh) 0) ((i n) 1)) ((pos "Pu*" "T-*"))))
  (lex.add.entry 
   '("*" n (((a s) 1) ((t @ r) 0) ((i s k) 0)) ((pos "K6%"))))
  (lex.add.entry 
   '("%" n (((p @ s) 1) ((e n t) 1)) ((pos "K9%"))))
  (lex.add.entry 
   '("&" n (((a m p) 1) ((@@ s) 0) ((a n d) 0))))
  (lex.add.entry 
   '("$" n (((d o l) 1) ((@) 0))))
  (lex.add.entry 
   '("#" n (((h a sh) 1))))
  (lex.add.entry 
   '("@" n (((a t) 1))))
  (lex.add.entry 
   '("+" n (((p l uh s) 0)) ((pos "K7%" "OA%" "T-%"))))
  (lex.add.entry 
   '("^" n (((k a r) 1) ((e t) 0)) ((pos "K6$"))))
  (lex.add.entry 
   '("~" n (((t i l d) 1) ((@) 0)) ((pos "K6$"))))
  (lex.add.entry 
   '("=" n (((ii k) 1) ((w @ l) 0))))
  (lex.add.entry 
   '("/" n (((s l a sh) 1))))
  (lex.add.entry 
   '("\\" n (((b a k) 1) ((s l a sh) 1))))
  (lex.add.entry 
   '("_" n (((uh n d) 1) ((@ s) 0) ((k oo) 1))))
  (lex.add.entry 
   '("|" n (((v @@ t) 1) ((i k l) 0) ((b aa) 1))))
  (lex.add.entry 
   '(">" n ((( g r ei ) 1) ((t @@) 0) ((dh a n) 1))))
  (lex.add.entry 
   '("<" n ((( l e s) 1) ((dh a n) 1))))
  (lex.add.entry 
   '("[" n ((( l e f t) 1) ((b r a k) 1) ((i t) 0))))
  (lex.add.entry 
   '("]" n ((( r ai t) 1) ((b r a k) 1) ((i t) 0))))
  (lex.add.entry 
   '(" " n (((s p ei s) 1))))
  (lex.add.entry 
   '("\t" n (((t a b) 1))))
  (lex.add.entry 
   '("\n" n (((n y uu) 1) ((l ai n) 1))))
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
  )

(lex.create "oald")
;(lex.set.compile.file (path-append oaldlexdir "cuvoald710-0.3.out"))
(lex.set.compile.file (path-append oaldlexdir "oald-0.4.out"))
;(lex.set.compile.file (path-append oaldlexdir "cuvoald710-0.2.out"))
(lex.set.phoneset "mrpa")
(lex.set.lts.method 'oald_lts_function)
(lex.set.pos.map english_pos_map_wp39_to_wp20)
(mrpa_addenda)


(provide 'oaldlex)

