;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;  Various tts functions and hooks

;;;  Once the utterance is built these functions synth and play it
(defvar tts_hooks (list utt.synth utt.play)
  "tts_hooks
Function or list of functions to be called during text to speech.
The function tts_file, chunks data into Utterances of type Token and
applies this hook to the utterance.  This typically contains the utt.synth
function and utt.play. [see TTS]")

;;;  This is used to define utterance breaks in tts on files
(defvar eou_tree 
  '((lisp_max_num_tokens > 200)
    ((1))
    ((n.whitespace matches ".*\n.*\n\\(.\\|\n\\)*");; significant break (2 nls)
     ((1))
     ((name matches "--+")
     ((1))
     ((punc matches ".*[\\?:!;].*")
      ((1))
      ((punc matches ".*\\..*")
       ((punc matches "..+");; longer punctuation string
	((punc matches "\\..*,")  ;; for U.S.S.R., like tokens
	 ((0))
	 ((1)))
	;; This is to distinguish abbreviations vs periods
	;; These are heuristics
	((name matches "\\(.*\\..*\\|[A-Z][A-Za-z]?[A-Za-z]?\\|etc\\)");; an abbreviation
	 ((n.whitespace is " ")
	  ((0));; if abbrev single space isn't enough for break
	  ((n.name matches "[A-Z].*")
	   ((1))
	   ((0))))
	 ((n.whitespace is " ");; if it doesn't look like an abbreviation
	  ((n.name matches "[A-Z].*");; single space and non-cap is no break
	   ((1))
	   ((0)))
	  ((1)))))
       ((0)))))))
  "eou_tree
End of utterance tree.  A decision tree used to determine if the given
token marks the end of an utterance.  It may look one token ahead to
do this. [see Utterance chunking]")

(define (max_num_tokens x)
  "(num_tokens x)
This is probably controversial, but its good to have a maximum number
of tokens in an utterance.  You really dont want to wait on very long
utterances, some utts can be thousands of words long, these maybe
shouldn't be spoken, but we do have to deal with them."
  (let ((c 1) (y x))
    (while y
     (set! c (+ 1 c))
     (set! y (item.prev y)))
    c))

;;;  The program used to parse stml files
;;;  Needs version 1.0 to allow -D option to work
(defvar sgml_parse_progname "nsgmls-1.0"
  "sgml_parse_progname
The name of the program to use to parse SGML files.  Typically this is
nsgml-1.0 from the sp SGML package. [see XML/SGML requirements]")

;;;  When PHRASE elements are specified in an utterance in STML
;;;  no other method for phrase prediction is to be used, so we
;;;  use the following tree
(set! stml_phrase_cart_tree
'((R:Token.parent.pbreak is B)
  ((B))
  ((n.name is 0)
   ((B))
   ((NB)))))

(define (xxml_synth utt)
"(xxml_synth UTT)
This applies the xxml_hooks (mode specific) and tts_hooks to the
given utterance.  This function should be called from xxml element
definitions that signal an utterance boundary."
   (cond
    ((or (not utt)
	 (not (utt.relation utt 'Token)))  ;; no tokens
     nil)
    (t
     (apply_hooks xxml_hooks utt)
     (apply_hooks tts_hooks utt)
     (set! utt nil) ;; not enough ... 
     (gc)
     utt))
)

(define (xxml_attval ATTNAME ATTLIST)
"(xxml_attval ATTNAME ATTLIST)
Returns attribute value of ATTNAME in ATTLIST or nil if it doesn't
exists."
  (cond
   ((not ATTLIST)
    nil)
   ((string-equal ATTNAME (car (car ATTLIST)))
    (car (cdr (car ATTLIST))))
   (t
    (xxml_attval ATTNAME (cdr ATTLIST)))))

(defvar xxml_word_features nil
  "xxml_word_features
An assoc list of features to be added to the current word when
in xxml parse mode.")

(defvar xxml_token_hooks nil
  "xxml_token_hooks
Functions to apply to each token.")

(defvar xxml_hooks nil
  "xxml_hooks
  Function or list of functions to be applied to an utterance when
  parsed with xxML, before tts_hooks.")

(defvar xxml_elements nil
  "xxml_elements
List of Scheme actions to perform on finding xxML tags.")

(defvar xml_dtd_dir libdir
  "xml_dtd_dir
The directory holding standard DTD form the xml parser.")

(set! tts_fnum 1)
(define (save_tts_output utt)
  (let ((fname (string-append "tts_file_" tts_fnum ".wav")))
    (format stderr "festival: saving waveform in %s\n" fname)
    (utt.save.wave utt fname)
    (set! tts_fnum (+ 1 tts_fnum))
    utt))

(define (save_waves_during_tts)
  "(save_waves_during_tts)
Save each waveform in the current directory in files \"tts_file_XXX.wav\".
use (save_waves_during_tts_STOP) to stop saving waveforms"
  (if (not (member save_tts_output tts_hooks))
      (set! tts_hooks (append tts_hooks (list save_tts_output))))
  t)

(define (save_waves_during_tts_STOP)
  "(save_waves_during_tts_STOP)
Stop saving waveforms when doing tts."
  (if (member save_tts_output tts_hooks)
      (set! tts_hooks (delq save_tts_output tts_hooks)))
  t)

(define (tts file mode)
  "(tts FILE MODE)
  Convert FILE to speech.  MODE identifies any special treatment
  necessary for FILE.  This is simply a front end to tts_file but 
  puts the system in async audio mode first. [see TTS]"
  (audio_mode 'async)
  (if mode
      (tts_file file mode)
      (tts_file file (tts_find_text_mode file auto-text-mode-alist)))
;;  (audio_mode 'sync) ;; Hmm this is probably bad
)

(define (tts_text string mode)
  "(tts_text STRING mode)
Apply tts on given string.  That is, segment it into utterances and
apply tts_hooks to each utterance.  This is naively done by saving the
string to a file and calling tts_file on that file.  This differs from
SayText which constructs a single utterance for the whole given text."
  (let ((tmpfile (make_tmp_filename))
	(fd))
    (set! fd (fopen tmpfile "wb"))
    (format fd "%s" string)
    (fclose fd)
    (audio_mode 'async)
    (tts_file tmpfile mode)
    (delete-file tmpfile)))

(define (save_record_wave utt)
"Saves the waveform and records its so it can be joined into a 
a single waveform at the end."
  (let ((fn (make_tmp_filename)))
    (utt.save.wave utt fn)
    (set! wavefiles (cons fn wavefiles))
    utt))

(define (combine_waves)
  "(combine_waves)
Join all the waves together into the desired output file
and delete the intermediate ones."
  (let ((wholeutt (Utterance Text "")))
    (mapcar
     (lambda (d) 
       (utt.import.wave wholeutt d t)
       (delete-file d))
     (reverse wavefiles))
    wholeutt))

(define (tts_textall string mode)
  "(tts_textall STRING MODE)
Apply tts to STRING.  This function is specifically designed for
use in server mode so a single function call may synthesize the string.
This function name maybe added to the server safe functions."
  (if (not (string-equal mode "nil"))
      (begin
 	  ;; a mode has been specified so do something different
	  (let ((tmpfile (make_tmp_filename))
		(fd))
	    (set! fd (fopen tmpfile "wb"))
	    (format fd "%s" string)
	    (fclose fd)
	    (set! tts_hooks (list utt.synth save_record_wave))
	    (set! wavefiles nil)
	    (tts_file tmpfile mode)
	    (delete-file tmpfile)
	    (utt.send.wave.client (combine_waves))
	))
      ;; Simple fundamental mode
      (utt.send.wave.client 
       (utt.synth
	(eval (list 'Utterance 'Text string))))))

;; Function to interface with app_festival for asterisk
;; See http://www.asterisk.org
(define (tts_textasterisk string mode)
  "(tts_textasterisk STRING MODE)
Apply tts to STRING.  This function is specifically designed for
use in server mode so a single function call may synthesize the string.
This function name may be added to the server safe functions."
  (utt.send.wave.asterisk
   (utt.synth
    (eval (list 'Utterance 'Text string)))))



(define (tts_return_to_client)
  "(tts_return_to_client)
This function is called by clients who wish to return waveforms of
their text samples asynchronously.  This replaces utt.play in tts_hooks
with utt.send.wave.client."
  (if (not (member utt.send.wave.client tts_hooks))
      (set! tts_hooks
	    (append (delq utt.play tts_hooks)
		    (list utt.send.wave.client)))))

(defvar tts_text_modes nil
"tts_text_modes
An a-list of text modes data for file type specific tts functions.
See the manual for an example.  [see Text modes]")

(define (tts_find_text_mode file alist)
  "(find_text_mode FILE ALIST)
Search through ALIST for one that matches FILE.  Returns nil if
nothing macthes."
  (cond
   ((null alist) nil)  ;; can't find a match
   ((string-matches file (string-append ".*" (car (car alist)) ".*"))
    (cdr (car alist)))
   (t
    (tts_find_text_mode file (cdr alist)))))

(defvar auto-text-mode-alist
  (list
   (cons "\\.sable$" 'sable)
   (cons "\\.ogi" 'ogimarkup)
   (cons "\\.email" 'email)
   (cons "" 'fundamental)
   )
  "auto-text-mode-alist
Following Emacs' auto-mode-alist thios provides a mechanism for auto
selecting a TTS text mode based on the filename being analyzed.  Its
format is exactly the same as Emacs in that it consists of an alist of
dotted pairs of regular expression and text mode name.")

(provide 'tts)
