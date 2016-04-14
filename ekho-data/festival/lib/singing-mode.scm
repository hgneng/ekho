;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Festival Singing Mode
;;;
;;; Written by Dominic Mazzoni
;;; Carnegie Mellon University
;;; 11-752 - "Speech: Phonetics, Prosody, Perception and Synthesis"
;;; Spring 2001
;;;
;;; Extended by Milan Zamazal <pdm@brailcom.org>, 2006:
;;; - Slur support.
;;; - Czech support.
;;; - Some cleanup.
;;; - Print debugging information only when singing-debug is true.
;;;
;;; This code is public domain; anyone may use it freely.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require_module 'rxp)

(xml_register_id "-//SINGING//DTD SINGING mark up//EN"
		(path-append xml_dtd_dir "Singing.v0_1.dtd")
		)

(xml_register_id "-//SINGING//ENTITIES Added Latin 1 for SINGING//EN"
		 (path-append xml_dtd_dir  "sable-latin.ent")
		 )

;; Set this to t to enable debugging messages:
(defvar singing-debug nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; XML parsing functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; singing_xml_targets
;;
;; This variable defines the actions that are to be taken when
;; parsing each of our XML tags: SINGING, PITCH, DURATION, and REST.
;;
;; When we get the pitch and duration of each token, we store them
;; in features of the token.  Later our intonation and duration
;; functions access these features.
;;

(defvar singing_xml_elements
  '(
    ("(SINGING" (ATTLIST UTT)
     (set! singing_pitch_att_list nil)
     (set! singing_dur_att_list nil)
     (set! singing_global_time 0.0)
     (set! singing_bpm (get-bpm ATTLIST))
     (set! singing_bps (/ singing_bpm 60.0))
     nil)

    (")SINGING" (ATTLIST UTT)
     (xxml_synth UTT)  ;;  Synthesize the remaining tokens
     nil)

    ("(PITCH" (ATTLIST UTT)
     (set! singing_pitch_att_list ATTLIST)
     UTT)

    (")PITCH" (ATTLIST UTT)
     (let ((freq (get-freqs singing_pitch_att_list)))
       (if singing-debug
           (begin
             (print "freqs")
             (print freq)))
       (singing-append-feature! UTT 'freq freq))
     UTT)
    
    ("(DURATION" (ATTLIST UTT)
     (set! singing_dur_att_list ATTLIST)
     UTT)

    (")DURATION" (ATTLIST UTT)
     (let ((dur (get-durs singing_dur_att_list)))
       (if singing-debug
           (begin
             (print "durs")
             (print dur)))
       (singing-append-feature! UTT 'dur dur))
     UTT)
    
    ("(REST" (ATTLIST UTT)
     (let ((dur (get-durs ATTLIST)))
       (if singing-debug
           (begin
             (print "rest durs")
             (print dur)))
       (singing-append-feature! UTT 'rest (caar dur)))
     UTT)
    ))

;;
;; get-bpm
;;
;; Given the attribute list of a SINGING tag, returns the beats
;; per minute of the song from the BPM parameter.
;;

(define (get-bpm atts)
  (parse-number (car (car (cdr (assoc 'BPM atts))))))

;;
;; get-durs
;;
;; Given the attribute list of a DURATION tag, returns a list of
;; durations in seconds for the syllables of the word enclosed by
;; this tag.
;;
;; It first looks for a BEATS parameter, and converts these to
;; seconds using BPM, which was set in the SINGING tag.  If this
;; is not present, it looks for the SECONDS parameter.
;;

(define (get-durs atts)
  (let ((seconds (car (car (cdr (assoc 'SECONDS atts)))))
        (beats (car (car (cdr (assoc 'BEATS atts))))))
    (if (equal? beats 'X)
        (mapcar (lambda (lst) (mapcar parse-number lst))
                (string->list seconds))
        (mapcar (lambda (lst)
                  (mapcar (lambda (x) (/ (parse-number x) singing_bps)) lst))
                (string->list beats)))))

;;
;; get-freqs
;;
;; Given the attribute list of a PITCH tag, returns a list of
;; frequencies in Hertz for the syllables of the word enclosed by
;; this tag.
;;
;; It first looks for a NOTE parameter, which can contain a MIDI
;; note of the form "C4", "D#3", or "Ab6", and if this is not
;; present it looks for the FREQ parameter.
;;

(define (get-freqs atts)
  (let ((freqs (car (car (cdr (assoc 'FREQ atts)))))
        (notes (car (car (cdr (assoc 'NOTE atts))))))
    (if (equal? notes 'X)
        (mapcar (lambda (lst) (mapcar parse-number lst))
                (string->list freqs))
        (mapcar (lambda (lst) (mapcar note->freq lst))
                (string->list notes)))))

;;
;; note->freq
;;
;; Converts a string representing a MIDI note such as "C4" and
;; turns it into a frequency.  We use the convention that
;; A5=440 (some call this note A3).
;;

(define (note->freq note)
  (if singing-debug
      (format t "note is %l\n" note))
  (set! note (format nil "%s" note))
  (if singing-debug
      (print_string note))
  (let (l octave notename midinote thefreq)
    (set! l (string-length note))
    (set! octave (substring note (- l 1) 1))
    (set! notename (substring note 0 (- l 1)))
    (set! midinote (+ (* 12 (parse-number octave))
                      (notename->midioffset notename)))
    (set! thefreq (midinote->freq midinote))
    (if singing-debug
        (format t "note %s freq %f\n" note thefreq))
    thefreq))

;;
;; midinote->freq
;;
;; Converts a MIDI note number (1 - 127) into a frequency.  We use
;; the convention that 69 = "A5" =440 Hz.
;;

(define (midinote->freq midinote)
  (* 440.0 (pow 2.0 (/ (- midinote 69) 12))))

;;
;; notename->midioffset
;;
;; Utility function that looks up the name of a note like "F#" and
;; returns its offset from C.
;;

(define (notename->midioffset notename)
  (parse-number (car (cdr (assoc_string notename note_names)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pitch modification functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; singing_f0_targets
;;
;; This function replaces the normal intonation function used in
;; festival.  For each syllable, it extracts the frequency that
;; was calculated from the XML tags and stored in the token this
;; syllable comes from, and sets this frequency as both the start
;; and end f0 target.  Really straightforward!
;;

(defvar singing-last-f0 nil)
(define (singing_f0_targets utt syl)
  "(singing_f0_targets utt syl)"
  (let ((start (item.feat syl 'syllable_start))
        (end (item.feat syl 'syllable_end))
        (freqs (mapcar parse-number (syl->freq syl)))
        (durs (syl->durations syl)))
    (let ((total-durs (apply + durs))
          (total-time (- end start))
          (time start)
          (prev-segment (item.prev (item.relation (item.daughter1 (item.relation syl 'SylStructure)) 'Segment)))
          (last-f0 singing-last-f0))
      (if freqs
          (begin
            (set! singing-last-f0 (car (last freqs)))
            (append (if (and last-f0
                             prev-segment
                             (item.prev prev-segment)
                             (string-equal (item.feat prev-segment 'name)
                                           (car (car (cdr (car (PhoneSet.description '(silences))))))))
                        (let ((s (item.feat prev-segment "p.end"))
                              (e (item.feat prev-segment "end")))
                          (list (list (+ s (* (- e s) 0.8)) last-f0)
                                (list (+ s (* (- e s) 0.9)) (car freqs)))))
                    (apply append
                           (mapcar (lambda (d f)
                                     (let ((range (* (/ d total-durs) total-time))
                                           (old-time time))
                                       (set! time (+ time range))
                                       (let ((range-fraction (* 0.1 range)))
                                         (list (list (+ old-time range-fraction) f)
                                               (list (- time range-fraction) f)))))
                                   durs freqs))))))))

;;
;; syl->freq
;;
;; Given a syllable, looks up the frequency in its token.  The token
;; stores a list of all of the frequencies associated with its
;; syllables, so this syllable grabs the frequency out of the list
;; corresponding to its index within the word.  (This assumes that
;; a frequency was given for each syllable, and that a token
;; corresponds directly to a word.  Singing-mode is not guaranteed
;; to work at all if either of these things are not true.)
;;

(define (syl->freq syl)
  (let ((index (item.feat syl "R:Syllable.pos_in_word"))
        (freqs (singing-feat syl "R:SylStructure.parent.R:Token.parent.freq")))
    (nth index freqs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Duration modification functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; singing_duration_method 
;;
;; Calculates the duration of each phone in the utterance, in three
;; passes.  Consult the three functions it calls, below, to see what
;; each one does.
;;

(define (singing_duration_method utt)
  (mapcar singing_adjcons_syllable (utt.relation.items utt 'Syllable))
  (singing_do_initial utt (car (utt.relation.items utt 'Token)))
  (mapcar singing_do_syllable (utt.relation.items utt 'Syllable))
  (mapcar singing_fix_segment (utt.relation.items utt 'Segment))
  utt)

;;
;; singing_adjcons_syllable
;;
;; First pass.  Looks at the first phone of each syllable and
;; adjusts the starting time of this syllable such that the
;; perceived start time of the first phone is at the beginning
;; of the originally intended start time of the syllable.
;;
;; If this is not done, telling it to say the word "ta" at time
;; 2.0 actually doesn't "sound" like it says the "t" sound until
;; about 2.1 seconds.
;;
;; This function has a little bit of duplicated code from
;; singing_do_syllable, below - it could be modularized a little
;; better.
;;

(define (singing_adjcons_syllable syl)
  (let ((totlen (apply + (mapcar (lambda (s)
                                   (get_avg_duration (item.feat s "name")))
                                 (item.leafs
                                  (item.relation syl 'SylStructure)))))
        (syldur (apply + (syl->durations syl)))
        ;; figure out the offset of the first phone
        (phone1 (item.daughter1 (item.relation syl 'SylStructure)))
        (prevsyl (item.prev (item.relation syl 'Syllable))))
    (let ((offset (get_duration_offset (item.feat phone1 "name"))))
      (if singing-debug
          (format t "offset: %f\n" offset) )
      (if (< syldur totlen)
          (set! offset (* offset (/ syldur totlen))))
      (if singing-debug
          (format t "Want to adjust syl by %f\n" offset))
      (if prevsyl
          (begin
            (item.set_feat prevsyl 'subtractoffset offset)
            (item.set_feat syl 'addoffset offset))))))

;;
;; singing_do_syllable
;;
;; Second pass.  For each syllable, adds up the amount of time
;; that would normally be spent in consonants and vowels, based
;; on the average durations of these phones.  Then, if the
;; intended length of this syllable is longer than this total,
;; stretch only the vowels; otherwise shrink all phones
;; proportionally.  This function actually sets the "end" time
;; of each phone using a global "singing_global_time" variable.
;;
;; We also handle rests at this point, which are tagged onto the
;; end of the previous token.
;;

(defvar singing-max-short-vowel-length 0.11)

(define (singing_do_initial utt token)
  (if (equal? (item.name token) "")
      (let ((restlen (car (item.feat token 'rest))))
        (if singing-debug
            (format t "restlen %l\n" restlen))
        (if (> restlen 0)
            (let ((silence (car (car (cdr (assoc 'silences (PhoneSet.description)))))))
              (set! singing_global_time restlen)
              (item.relation.insert (utt.relation.first utt 'Segment) 'Segment
                                    (list silence (list (list "end" singing_global_time)))
                                    'before))))))

(define (singing_do_syllable syl)
  (let ((conslen 0.0)
        (vowlen 0.0)
        (segments (item.leafs (item.relation syl 'SylStructure))))
    ;; if there are no vowels, turn a middle consonant into a vowel;
    ;; hopefully this works well for languages where syllables may be
    ;; created by some consonants too
    (let ((segments* segments)
          (vowel-found nil))
      (while (and segments* (not vowel-found))
        (if (equal? "+" (item.feat (car segments*) "ph_vc"))
            (set! vowel-found t)
            (set! segments* (cdr segments*))))
      (if (not vowel-found)
          (item.set_feat (nth (nint (/ (- (length segments) 1) 2))
                              segments)
                         "singing-vc" "+")))
    ;; sum up the length of all of the vowels and consonants in
    ;; this syllable
    (mapcar (lambda (s)
              (let ((slen (get_avg_duration (item.feat s "name"))))
                (if (or (equal? "+" (item.feat s "ph_vc"))
                        (equal? "+" (item.feat s "singing-vc")))
                    (set! vowlen (+ vowlen slen))
                    (set! conslen (+ conslen slen)))))
            segments)
    (let ((totlen (+ conslen vowlen))
          (syldur (apply + (syl->durations syl)))
          (addoffset (item.feat syl 'addoffset))
          (subtractoffset (item.feat syl 'subtractoffset))
          offset)
      (set! offset (- subtractoffset addoffset))
      (if singing-debug
          (format t "Vowlen: %f conslen: %f totlen: %f\n" vowlen conslen totlen))
      (if (< offset (/ syldur 2.0))
	  (begin
            (set! syldur (- syldur offset))
            (if singing-debug
                (format t "Offset: %f\n" offset))))
      (if singing-debug
          (format t "Syldur: %f\n" syldur))
      (if (> totlen syldur)
	  ;; if the total length of the average durations in the syllable is
	  ;; greater than the total desired duration of the syllable, stretch
	  ;; the time proportionally for each phone
	  (let ((stretch (/ syldur totlen)))
            (mapcar (lambda (s)
                      (let ((slen (* stretch (get_avg_duration (item.feat s "name")))))
                        (set! singing_global_time (+ slen singing_global_time))
                        (item.set_feat s 'end singing_global_time)))
                    (item.leafs (item.relation syl 'SylStructure))))
	  ;; otherwise, stretch the vowels and not the consonants
	  (let ((voweltime (- syldur conslen)))
            (let ((vowelstretch (/ voweltime vowlen))
                  (phones (mapcar car (car (cdar (PhoneSet.description '(phones)))))))
              (mapcar (lambda (s)
                        (let ((slen (get_avg_duration (item.feat s "name"))))
                          (if (or (equal? "+" (item.feat s "ph_vc"))
                                  (equal? "+" (item.feat s "singing-vc")))
                              (begin
                                (set! slen (* vowelstretch slen))
                                ;; If the sound is long enough, better results
                                ;; may be achieved by using longer versions of
                                ;; the vowels.
                                (if (> slen singing-max-short-vowel-length)
                                    (let ((sname (string-append (item.feat s "name") ":")))
                                      (if (member_string sname phones)
                                          (item.set_feat s "name" sname))))))
                          (set! singing_global_time (+ slen singing_global_time))
                          (item.set_feat s 'end singing_global_time)))
                      segments))))))
  (let ((restlen (car (syl->rest syl))))
    (if singing-debug
        (format t "restlen %l\n" restlen))
    (if (> restlen 0)
        (let ((lastseg (item.daughtern (item.relation syl 'SylStructure)))
              (silence (car (car (cdr (assoc 'silences (PhoneSet.description))))))
              (singing_global_time* singing_global_time))
          (let ((seg (item.relation lastseg 'Segment))
                (extra-pause-length 0.00001))
            (set! singing_global_time (+ restlen singing_global_time))
            (item.insert seg (list silence (list (list "end" singing_global_time))) 'after)
            ;; insert a very short extra pause to avoid after-effects, especially
            ;; after vowels
            (if (and seg
                     (equal? (item.feat seg "ph_vc") "+")
                     (< extra-pause-length restlen))
                (item.insert seg (list silence (list (list "end" (+ singing_global_time*
                                                                extra-pause-length))))
                             'after)))))))

;;
;; singing_fix_segment
;;
;; Third pass.  Finds any segments (phones) that we didn't catch earlier
;; (say if they didn't belong to a syllable, like silence) and sets them
;; to zero duration
;;

(define (singing_fix_segment seg)
  (if (equal? 0.0 (item.feat seg 'end))
      (if (equal? nil (item.prev seg))
          (item.set_feat seg 'end 0.0)
          (item.set_feat seg 'end (item.feat (item.prev seg) 'end)))
      (if singing-debug
          (format t "segment: %s end: %f\n" (item.name seg) (item.feat seg 'end)))))
  
;; returns the duration of a syllable (stored in its token)
(define (syl->durations syl)
  (let ((index (item.feat syl "R:Syllable.pos_in_word"))
        (durs (singing-feat syl "R:SylStructure.parent.R:Token.parent.dur")))
    (mapcar parse-number (nth index durs))))

;; returns the duration of the rest following a syllable
(define (syl->rest syl)
  (let ((index (item.feat syl "R:Syllable.pos_in_word"))
        (durs (singing-feat syl "R:SylStructure.parent.R:Token.parent.dur"))
        (pauselen (singing-feat syl "R:SylStructure.parent.R:Token.parent.rest")))
    (if (equal? index (- (length durs) 1))
        (list (or pauselen 0.0))
        (list 0.0))))

;; get the average duration of a phone
(define (get_avg_duration phone)
  (let ((pd (assoc_string phone phoneme_durations)))
    (if pd
	(car (cdr pd))
	0.08)))

;; get the duration offset of a phone (see the description above)
(define (get_duration_offset phone)
  (parse-number (car (cdr (assoc_string phone phoneme_offsets*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Other utility functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (char-quote string)
  (if (member string '("*" "+" "?" "[" "]" "."))
      (string-append "[" string "]")
      string))

(define (split-string string separator)
  (if (string-matches string (string-append ".+" (char-quote separator) ".+"))
      (cons (string-before string separator)
            (split-string (string-after string separator) separator))
      ;; We have to convert the weird XML attribute value type to string
      (list (string-append string ""))))

(define (string->list string)
  (mapcar (lambda (s) (split-string s "+")) (split-string string ",")))

(define (singing-append-feature! utt feature value)
  (let ((tokens (utt.relation.items utt 'Token)))
    (if tokens
        ;; we have to wrap value into a list to work around a Festival bug
        (item.set_feat (car (last tokens)) feature (list value))
        (begin
          (utt.relation.append utt 'Token '("" ((name "") (whitespace "")
                                                (prepunctuation "") (punc ""))))
          (item.set_feat (car (last (utt.relation.items utt 'Token))) feature (list value))))))

(define (singing-feat item feature)
  (let ((value (item.feat item feature)))
    (if (equal? value 0)
        nil
        (car value))))

(define (current-language)
  (cadr (car (assoc 'language (voice.description current-voice)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Initializing and exiting singing mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; singing_init_func
;;

(defvar singing_previous_eou_tree nil)

(define (singing_init_func)
  "(singing_init_func) - Initialization for Singing mode"
  (if (not (symbol-bound? 'phoneme_durations))
      (set! phoneme_durations '()))
  ;; use our intonation function
  (Parameter.set 'Int_Method 'General)
  (Parameter.set 'Int_Target_Method Int_Targets_General)
  (set! int_general_params `((targ_func ,singing_f0_targets)))
  (set! singing-last-f0 nil)
  ;; use our duration function
  (Parameter.set 'Duration_Method singing_duration_method)
  ;; set phoneme corrections for the current language
  (let ((language (cadr (assoc 'language
                               (cadr (voice.description current-voice))))))
    (set! phoneme_offsets* (cdr (assoc language phoneme_offsets))))
  ;; avoid splitting to multiple utterances with insertion of unwanted pauses
  (set! singing_previous_eou_tree eou_tree)
  (set! eou_tree nil)
  ;; use our xml parsing function
  (set! singing_previous_elements xxml_elements)
  (set! xxml_elements singing_xml_elements))

;;
;; singing_exit_func
;;

(define (singing_exit_func)
  "(singing_exit_func) - Exit function for Singing mode"
  (set! eou_tree singing_previous_eou_tree)
  (set! xxml_elements singing_previous_elements))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Data tables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar note_names
  '((C 0)
    (C# 1)
    (Db 1)
    (D 2)
    (D# 3)
    (Eb 3)
    (E 4)
    (E# 5)
    (Fb 4)
    (F 5)
    (F# 6)
    (Gb 6)
    (G 7)
    (G# 8)
    (Ab 8)
    (A 9)
    (A# 10)
    (Bb 10)
    (B 11)
    (B# 12)
    (Cb 11)))

;;
;; The following list contains the offset into each phone that best
;; represents the perceptual onset of the phone.  This is important
;; to know to get durations right in singing.  For example, if the
;; offset for "t" is .060, and you want to start a "t" sound at
;; time 2.0 seconds, you should actually start the phone playing
;; at time 1.940 seconds in order for it to sound like the onset of
;; the "t" is really right at 2.0.
;;
;; These were derived empically by looking at and listening to the
;; waveforms of each phone for mwm's voice.
;;

(defvar phoneme_offsets
  `((english (t 0.050)
             (T 0.050)
             (d 0.090)
             (D 0.090)
             (p 0.080)
             (b 0.080)
             (k 0.090)
             (g 0.100)
             (9r 0.050) ;; r
             (l 0.030)
             (f 0.050)
             (v 0.050)
             (s 0.040)
             (S 0.040)
             (z 0.040)
             (Z 0.040)
             (n 0.040)
             (N 0.040)
             (m 0.040)
             (j 0.090)
             (E 0.0)
             (> 0.0)
             (>i 0.0)
             (aI 0.0)
             (& 0.0)
             (3r 0.0)
             (tS 0.0)
             (oU 0.0)
             (aU 0.0)
             (A 0.0)
             (ei 0.0)
             (iU 0.0)
             (U 0.0)
             (@ 0.0)
             (h 0.0)
             (u 0.0)
             (^ 0.0)
             (I 0.0)
             (dZ 0.0)
             (i: 0.0)
             (w 0.0)
             (pau 0.0)
             (brth 0.0)
             (h# 0.0)
             )))

(defvar phoneme_offsets* nil)

;;
;; Declare the new mode to Festival
;;

(set! tts_text_modes
      (cons `(singing   ;; mode name
              ((init_func ,singing_init_func)
               (exit_func ,singing_exit_func)
               (analysis_type xml)))
            tts_text_modes))

(provide 'singing-mode)
