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
;;;  General Festival Scheme specific functions
;;;  Including definitions of various standard variables.

;; will be set automatically on start-up
(defvar festival_version "unknown"
  "festival_version
 A string containing the current version number of the system.")

;; will be set automatically on start-up
(defvar festival_version_number '(x x x)
  "festival_version_number
 A list of major, minor and subminor version numbers of the current
 system.  e.g. (1 0 12).")

(define (apply_method method utt)
"(apply_method METHOD UTT)
Apply the appropriate function to utt defined in parameter."
  (let ((method_val (Parameter.get method)))
    (cond
     ((null method_val)
      nil)   ;; should be an error, but I'll let you off at present
     ((and (symbol? method_val) (symbol-bound? method_val))
      (apply (symbol-value method_val) (list utt)))
     ((member (typeof method_val) '(subr closure))
      (apply method_val (list utt)))
     (t      ;; again is probably an error
      nil))))

(define (require_module l)
  "(require_module l)
Check that certain compile-time modules are included in this installation.
l may be a single atom or list of atoms.  Each item in l must appear in
*modules* otherwise an error is throw."
  (if (consp l)
      (mapcar require_module l)
      (if (not (member_string l *modules*))
	  (error (format nil "module %s required, but not compiled in this installation\n" l))))
  t)

;;;  Feature Function Functions
(define (utt.features utt relname func_list)
"(utt.features UTT RELATIONNAME FUNCLIST)
  Get vectors of feature values for each item in RELATIONNAME in UTT.
  [see Features]"
  (mapcar 
   (lambda (s) 
     (mapcar (lambda (f) (item.feat s f)) func_list))
   (utt.relation.items utt relname)))

(define (utt.type utt)
"(utt.type UTT)
  Returns the type of UTT."
  (intern (utt.feat utt 'type)))

(define (utt.save.segs utt filename)
"(utt.save.segs UTT FILE)
  Save segments of UTT in a FILE in xlabel format."
  (let ((fd (fopen filename "w")))
    (format fd "#\n")
    (mapcar
     (lambda (info)
       (format fd "%2.4f 100 %s\n" (car info) (car (cdr info))))
     (utt.features utt 'Segment '(segment_end name)))
    (fclose fd)
    utt))

(define (utt.save.words utt filename)
"(utt.save.words UTT FILE)
  Save words of UTT in a FILE in xlabel format."
  (let ((fd (fopen filename "w")))
    (format fd "#\n")
    (mapcar
     (lambda (info)
       (format fd "%2.4f 100 %s\n" (car info) (car (cdr info))))
     (utt.features utt 'Word '(word_end name)))
    (fclose fd)
    utt))

(define (utt.resynth labfile f0file)
"(utt.resynth LABFILE F0FILE)
Resynthesize an utterance from a label file and F0 file (in any format
supported by the Speech Tool Library).   This loads, synthesizes and
plays the utterance."
  (let (u f0 f0_item)
    (set! u (Utterance SegF0)) ; need some u to start with
    (utt.relation.load u 'Segment labfile)
    (utt.relation.create u 'f0)
    (set! f0 (track.load f0file))
    (set! f0_item (utt.relation.append u 'f0))
    (item.set_feat f0_item "name" "f0")
    (item.set_feat f0_item "f0" f0)

    ;; emulabel may have flipped pau to H#
    (mapcar
     (lambda (s)
       (cond
	((string-matches (item.name s) "[hH]#")
	 (item.set_feat s "name" "pau"))
	((string-matches (item.name s) "#.*")
	 (item.set_feat s "name" (string-after (item.name s) "#")))))
     (utt.relation.items u 'Segment))

    (Wave_Synth u)
    (utt.play u)
    u))

(define (utt.relation.present utt relation)
"(utt.relation.present UTT RELATIONNAME)
Returns t if UTT caontains a relation called RELATIONNAME, nil otherwise."
  (if (member_string relation (utt.relationnames utt))
      t
      nil))

(define (utt.relation.leafs utt relation)
"(utt.relation.leafs UTT RELATIONNAME)
Returns a list of all the leafs in this relation."
  (let ((leafs nil))
    (mapcar
     (lambda (i)
       (if (not (item.down (item.relation i relation)))
	   (set! leafs (cons i leafs))))
     (utt.relation.items utt relation))
    (reverse leafs)))

(define (utt.relation.first utt relation)
"(utt.relation.first UTT RELATIONNAME)
Returns a the first item in this relation."
  (utt.relation utt relation))

(define (utt.relation.last utt relation)
"(utt.relation.last UTT RELATIONNAME)
Returns a the last item in this relation."
  (let ((i (utt.relation.first utt relation)))
    (while (item.next i)
	   (set! i (item.next i)))
    i))

(define (item.feat.present item feat)
  "(item.feat.present item feat)
nil if feat doesn't existing in this item, non-nil otherwise."
  (and item (assoc_string feat (item.features item))))

(define (item.relation.append_daughter parent relname daughter)
"(item.relation.append_daughter parent relname daughter)
Make add daughter to parent as a new daughter in relname."
   (item.append_daughter (item.relation parent relname) daughter))

(define (item.relation.insert si relname newsi direction)
"(item.relation.insert si relname newsi direction)
Insert newsi in relation relname with respect to direction.  If
direction is ommited after is assumed, valid directions are after
before, above and below.  Note you should use 
item.relation.append_daughter for tree adjoining.  newsi maybe
a item itself of a LISP description of one."
   (item.insert 
    (item.relation si relname)
    newsi
    direction))

(define (item.relation.daughters parent relname)
  "(item.relation.daughters parent relname)
Return a list of all daughters of parent by relname."
  (let ((d1 (item.daughter1 (item.relation parent relname)))
	(daughters))
    (while d1
	   (set! daughters (cons d1 daughters))
	   (set! d1 (item.next d1)))
    (reverse daughters)))

(define (item.daughters p)
  "(item.daughters parent)
Return a list of all daughters of parent."
  (item.relation.daughters p (item.relation.name p)))

(define (item.relation.parent si relname)
  "(item.relation.parent item relname)
Return the parent of this item in this relation."
  (item.parent (item.relation si relname)))

(define (item.relation.daughter1 si relname)
  "(item.relation.daughter1 item relname)
Return the first daughter of this item in this relation."
  (item.daughter1 (item.relation si relname)))

(define (item.relation.daughter2 si relname)
  "(item.relation.daughter2 item relname)
Return the second daughter of this item in this relation."
  (item.daughter2 (item.relation si relname)))

(define (item.relation.daughtern si relname)
  "(item.relation.daughtern item relname)
Return the final daughter of this item in this relation."
  (item.daughtern (item.relation si relname)))

(define (item.relation.next si relname)
  "(item.relation.next item relname)
Return the next item in this relation."
  (item.next (item.relation si relname)))

(define (item.relation.prev si relname)
  "(item.relation.prev item relname)
Return the previous item in this relation."
  (item.prev (item.relation si relname)))

(define (item.relation.first si relname)
  "(item.relation.first item relname)
Return the most previous item from this item in this relation."
  (let ((n (item.relation si relname)))
    (while (item.prev n)
     (set! n (item.prev n)))
    n))

(define (item.leafs si)
  "(item.relation.leafs item relname)
Return a list of the leafs of this item in this relation."
  (let ((ls nil)
	(pl (item.first_leaf si))
	(ll (item.next_leaf (item.last_leaf si))))
    (while (and pl (not (equal? pl ll)))
	   (set! ls (cons pl ls))
	   (set! pl (item.next_leaf pl)))
    (reverse ls)))

(define (item.relation.leafs si relname)
  "(item.relation.leafs item relname)
Return a list of the leafs of this item in this relation."
  (item.leafs (item.relation si relname)))

(define (item.root s)
  "(item.root s)
Follow parent link until s has no parent."
  (cond
   ((item.parent s) 
    (item.root (item.parent s)))
   (t s)))

(define (item.parent_to s relname)
  "(item.parent_to s relname)
Find the first ancestor of s in its current relation that is also in
relname.  s is treated as an ancestor of itself so if s is in relname
it is returned.  The returned value is in will be in relation relname
or nil if there isn't one."
  (cond
   ((null s) s)
   ((member_string relname (item.relations s)) 
    (item.relation s relname))
   (t (item.parent_to (item.parent s) relname))))

(define (item.daughter1_to s relname)
  "(item.daughter1_to s relname)
Follow daughter1 links of s in its current relation until an item
is found that is also in relname, is s is in relname it is returned.
The return item is returned in relation relname, or nil if there is
nothing in relname."
  (cond
   ((null s) s)
   ((member_string relname (item.relations s)) (item.relation s relname))
   (t (item.daughter1_to (item.daughter1 s) relname))))

(define (item.daughtern_to s relname)
  "(item.daughter1_to s relname)
Follow daughtern links of s in its current relation until an item
is found that is also in relname, is s is in relname it is returned.
The return item is returned in relation relname, or nil if there is
nothing in relname."
  (cond
   ((null s) s)
   ((member_string relname (item.relations s)) (item.relation s relname))
   (t (item.daughtern_to (item.daughtern s) relname))))

(define (item.name s)
"(item.name ITEM)
  Returns the name of ITEM. [see Accessing an utterance]"
  (item.feat s "name"))

(define (utt.wave utt)
  "(utt.wave UTT)
Get waveform from wave (R:Wave.first.wave)."
  (item.feat (utt.relation.first utt "Wave") "wave"))

(define (utt.wave.rescale . args)
 "(utt.wave.rescale UTT FACTOR NORMALIZE)
Modify the gain of the waveform in UTT by GAIN.  If NORMALIZE is
specified and non-nil the waveform is maximized first."
  (wave.rescale (utt.wave (nth 0 args)) (nth 1 args) (nth 2 args))
  (nth 0 args))

(define (utt.wave.resample utt rate)
  "(utt.wave.resample UTT RATE)\
Resample waveform in UTT to RATE (if it is already at that rate it remains
unchanged)."
  (wave.resample (utt.wave utt) rate)
  utt)

(define (utt.import.wave . args)
  "(utt.import.wave UTT FILENAME APPEND)
Load waveform in FILENAME into UTT in R:Wave.first.wave.  If APPEND
is specified and non-nil append this to the current waveform."
  (let ((utt (nth 0 args))
	(filename (nth 1 args))
	(append (nth 2 args)))
    (if (and append (member 'Wave (utt.relationnames utt)))
	(wave.append (utt.wave utt) (wave.load filename))
	(begin
	  (utt.relation.create utt 'Wave)
	  (item.set_feat
	   (utt.relation.append utt 'Wave)
	   "wave"
	   (wave.load filename))))
    utt))

(define (utt.save.wave . args)
  "(utt.save.wave UTT FILENAME FILETYPE)
Save waveform in UTT in FILENAME with FILETYPE (if specified) or
using global parameter Wavefiletype."
  (wave.save 
   (utt.wave (nth 0 args))
   (nth 1 args)
   (nth 2 args))
  (nth 0 args))

(define (utt.play utt)
  "(utt.play UTT)
Play waveform in utt by current audio method."
  (wave.play (utt.wave utt))
  utt)

(define (utt.save.track utt filename relation feature)
  "(utt.save.track utt filename relation feature)
DEPRICATED use trace.save instead."
  (format stderr "utt.save.track: DEPRICATED use track.save instead\n")
  (track.save 
   (item.feat
    (utt.relation.first utt relation)
    feature)
   filename)
  utt)

(define (utt.import.track utt filename relation fname)
  "(utt.import.track UTT FILENAME RELATION FEATURE_NAME)
Load track in FILENAME into UTT in R:RELATION.first.FEATURE_NAME.
Deletes RELATION if it already exists. (you maybe want to use track.load
directly rather than this legacy function."
  (utt.relation.create utt relation)
  (item.set_feat
   (utt.relation.append utt relation)
   fname
   (track.load filename))
  utt)

(define (wagon_predict item tree)
"(wagon_predict ITEM TREE)
Predict with given ITEM and CART tree and return the prediction
(the last item) rather than whole probability distribution."
 (car (last (wagon item tree))))

(define (phone_is_silence phone)
  (member_string 
   phone
   (car (cdr (car (PhoneSet.description '(silences)))))))

(define (phone_feature phone feat)
"(phone_feature phone feat)
Return the feature for given phone in current phone set, or 0
if it doesn't exist."
  (let ((ph (intern phone)))
    (let ((fnames (cadr (assoc 'features (PhoneSet.description))))
	  (fvals (cdr (assoc ph (cadr (assoc 'phones (PhoneSet.description)))))))
      (while (and fnames (not (string-equal feat (car (car fnames)))))
	     (set! fvals (cdr fvals))
	     (set! fnames (cdr fnames)))
      (if fnames
	  (car fvals)
	  0))))

(defvar server_max_clients 10
  "server_max_clients
In server mode, the maximum number of clients supported at any one
time.  When more that this number of clients attach simulaneous
the last ones are denied access.  Default value is 10.
[see Server/client API]")

(defvar server_port 1314
  "server_port
In server mode the inet port number the server will wait for connects
on.  The default value is 1314. [see Server/client API]")

(defvar server_log_file t
  "server_log_file
If set to t server log information is printed to standard output
of the server process.  If set to nil no output is given.  If set
to anything else the value is used as the name of file to which
server log information is appended.  Note this value is checked at
server start time, there is no way a client may change this.
[see Server/client API]")

(defvar server_passwd nil
  "server_passwd
If non-nil clients must send this passwd to the server followed by
a newline before they can get a connection.  It would be normal
to set this for the particular server task.
[see Server/client API]")

(defvar server_access_list '(localhost)
  "server_access_list
If non-nil this is the exhaustive list of machines and domains
from which clients may access the server.  This is a list of REGEXs
that client host must match.  Remember to add the backslashes before
the dots. [see Server/client API]")

(defvar server_deny_list nil
  "server_deny_list
If non-nil this is a list of machines which are to be denied access
to the server absolutely, irrespective of any other control features.
The list is a list of REGEXs that are used to matched the client hostname.
This list is checked first, then server_access_list, then passwd.
[see Server/client API]")

(define (def_feature_docstring fname fdoc)
"(def_feature_docstring FEATURENAME FEATUREDOC)
As some feature are used directly of stream items with no
accompanying feature function, the features are just values on the feature
list.  This function also those features to have an accompanying
documentation string."
  (let ((fff (assoc fname ff_docstrings)))
    (cond
     (fff  ;; replace what's already there
      (set-cdr! fff fdoc))
     (t
      (set! ff_docstrings (cons (cons fname fdoc) ff_docstrings))))
    t))

(define (linear_regression item model)
  "(linear_regression ITEM MODEL)
Use linear regression MODEL on ITEM.  MODEL consists of a list
of features, weights and optional map list.  E.g. ((Intercept 100)
(tobi_accent 10 (H* !H*)))."
  (let ((intercept (if (equal? 'Intercept (car (car model))) 
                       (car (cdr (car model))) 0))
        (mm (if (equal? 'Intercept (car (car model))) 
                (cdr model) model)))
  (apply + 
   (cons intercept
   (mapcar
    (lambda (f)
     (let ((ff (item.feat item (car f))))
      (if (car (cdr (cdr f)))
         (if (member_string ff (car (cdr (cdr f))))
           (car (cdr f))
           0)
         (* (parse-number ff) (car (cdr f))))))
    mm)))))

(defvar help
 "The Festival Speech Synthesizer System: Help

Getting Help
  (doc '<SYMBOL>)   displays help on <SYMBOL>
  (manual nil)      displays manual in local netscape
  C-c               return to top level
  C-d or (quit)     Exit Festival
(If compiled with editline)
  M-h               displays help on current symbol  
  M-s               speaks help on current symbol  
  M-m               displays relevant manula page in local netscape
  TAB               Command, symbol and filename completion
  C-p or up-arrow   Previous command
  C-b or left-arrow Move back one character
  C-f or right-arrow 
                    Move forward one character
  Normal Emacs commands work for editing command line

Doing stuff
  (SayText TEXT)      Synthesize text, text should be surrounded by
                      double quotes
  (tts FILENAME nil)  Say contexts of file, FILENAME should be 
                      surrounded by double quotes
  (voice_rab_diphone) Select voice (Britsh Male)
  (voice_ked_diphone) Select voice (American Male)
")

(define (festival_warranty)
"(festival_warranty)
  Display Festival's copyright and warranty. [see Copying]"
 (format t
   (string-append
    "    The Festival Speech Synthesis System: "
    festival_version
"
                Centre for Speech Technology Research                  
                     University of Edinburgh, UK                       
                       Copyright (c) 1996-2010
                        All Rights Reserved.                           
                                                                       
  Permission is hereby granted, free of charge, to use and distribute  
  this software and its documentation without restriction, including   
  without limitation the rights to use, copy, modify, merge, publish,  
  distribute, sublicense, and/or sell copies of this work, and to      
  permit persons to whom this work is furnished to do so, subject to   
  the following conditions:                                            
   1. The code must retain the above copyright notice, this list of    
      conditions and the following disclaimer.                         
   2. Any modifications must be clearly marked as such.                
   3. Original authors' names are not deleted.                         
   4. The authors' names are not used to endorse or promote products   
      derived from this software without specific prior written        
      permission.                                                      
                                  
  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        
  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      
  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   
  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     
  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   
  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          
  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       
  THIS SOFTWARE.                                                       
")))

(define (intro)
"(intro)
 Synthesize an introduction to the Festival Speech Synthesis System."
  (tts (path-append libdir "../examples/intro.text") nil))

(define (intro-spanish)
"(intro-spanish)
 Synthesize an introduction to the Festival Speech Synthesis System
 in spanish.  Spanish voice must already be selected for this."
  (tts (path-append libdir "../examples/spintro.text") nil))

(define (na_play FILENAME)
"(play_wave FILENAME)
Play given wavefile"
 (utt.play (utt.synth (eval (list 'Utterance 'Wave FILENAME)))))

;;; Some autoload commands
(autoload manual-sym "festdoc" "Show appropriate manual section for symbol.")
(autoload manual "festdoc" "Show manual section.")

(autoload display "display" "Graphically display utterance.")

(autoload festtest "festtest" "Run tests of Festival.")

(defvar diphone_module_hooks nil
  "diphone_module_hooks
  A function or list of functions that will be applied to the utterance
  at the start of the diphone module.  It can be used to map segment 
  names to those that will be used by the diphone database itself.
  Typical use specifies _ and $ for consonant clusters and syllable 
  boundaries, mapping to dark ll's etc.  Reduction and tap type 
  phenomena should probabaly be done by post lexical rules though the 
  distinction is not a clear one.")

(def_feature_docstring
  'Segment.diphone_phone_name
  "Segment.diphone_phone_name
  This is produced by the diphone module to contain the desired phone
  name for the desired diphone.  This adds things like _ if part of 
  a consonant or $ to denote syllable boundaries.  These are generated
  on a per voice basis by function(s) specified by diphone_module_hooks.
  Identification of dark ll's etc. may also be included.  Note this is not
  necessarily the name of the diphone selected as if it is not found
  some of these characters will be removed and fall back values will be
  used.")

(def_feature_docstring
  'Syllable.stress
  "Syllable.stress
  The lexical stress of the syllable as specified from the lexicon entry
  corresponding to the word related to this syllable.")

;;;
;;;  I tried some tests on the resulting speed both runtime and loadtime
;;;  but compiled files don't seem to make any significant difference
;;;
(define (compile_library)
  "(compile_library)
Compile all the scheme files in the library directory."
  (mapcar
   (lambda (file)
     (format t "compile ... %s\n" file)
     (compile-file (string-before file ".scm")))
   (list
     "synthesis.scm" "siod.scm" "init.scm" "lexicons.scm"
     "festival.scm" "gsw_diphone.scm" "intonation.scm" "duration.scm"
     "pos.scm" "phrase.scm" "don_diphone.scm" "rab_diphone.scm"
     "voices.scm" "tts.scm" "festdoc.scm" "languages.scm" "token.scm"
     "mbrola.scm" "display.scm" "postlex.scm" "tokenpos.scm"
     "festtest.scm" "cslush.scm" "ducs_cluster.scm" "sucs.scm"
     "web.scm" "cart_aux.scm"
     "lts_nrl.scm" "lts_nrl_us.scm" "email-mode.scm"
     "mrpa_phones.scm" "radio_phones.scm" "holmes_phones.scm"
     "mrpa_durs.scm" "klatt_durs.scm" "gswdurtreeZ.scm"
     "tobi.scm" "f2bf0lr.scm"))
  t)

;;; For mlsa resynthesizer
(defvar mlsa_alpha_param 0.42)
(defvar mlsa_beta_param 0.0)

(provide 'festival)
