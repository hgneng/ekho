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
;;;  Basic Intonation modules.  These call appropriate sub-modules
;;;  depending on the chosen intonation methods
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  These modules should predict intonation events/labels
;;;  based on information in the phrase and word streams

; to detect prespecified accents (feature "accent" in 'Word relation)
; AS 5/29/00

(define (tobi_accent_prespecified utt)
  (let ((tobi_found nil)
	(words (utt.relation.items utt 'Word)))
    
    (while (and words (not tobi_found))
; feature "accent" might be prespecified on words or tokens, AS 05/29/00
	   (if (item.feat.present (car words) 'accent)
	       (set! tobi_found t)
; if Token relation exists, check tokens as well
               (if (not (null (item.parent (item.relation (car words) 'Token))))
                   (if (item.feat.present (item.parent (item.relation (car words) 'Token)) 'accent)
                       (set! tobi_found t)
                       (set! words (cdr words)))
                   (set! words (cdr words)))))
    tobi_found))

(set! int_accent_cart_tree_no_accent
'((NONE)))

(define (Intonation utt)
"(Intonation utt)                                
Select between different intonation modules depending on the Parameter
Int_Method.  Currently offers three types: Simple, hats on each content
word; ToBI, a tree method for predicting ToBI accents; and Default a
really bad method with a simple downward sloping F0.  This is the first
of a two-stage intonation prediction process.  This adds accent-like
features to syllables, the second, Int_Targets generates the F0 contour
itself. [see Intonation]"

; AS 5/29/00: Hack to avoid prediction of further accent labels
; on utterance chunks that have already been annotated with
; accent labels
; use CART that doesn't assign any labels when using Intonation_Tree

(if (tobi_accent_prespecified utt)
    (progn
     (set! int_accent_cart_tree_save int_accent_cart_tree)  
     (set! int_accent_cart_tree int_accent_cart_tree_no_accent)
     (Intonation_Tree utt)
     (set! int_accent_cart_tree int_accent_cart_tree_save))

  (let ((rval (apply_method 'Int_Method utt)))
    (Parameter.get 'Int_Method)
    (cond   
     (rval rval) ;; new style
     ((eq 'Simple (Parameter.get 'Int_Method))
      (Intonation_Simple utt))
     ((eq 'ToBI (Parameter.get 'Int_Method))
      (format t "Using Intonation_Tree")
      (Intonation_Tree utt))
     ((eq 'General (Parameter.get 'Int_Method))
      (Intonation_Simple utt))  ;; yes this is a duplication
     (t
      (Intonation_Default utt))))))


;;;  These modules should create an actual F0 contour based on the
;;;  the existing intonational events/labels etc
;;;  Specifically this is called after durations have been predicted
 
(define (Int_Targets utt)
"(Int_Targets utt)
The second stage in F0 prediction.  This generates F0 targets 
related to segments using one of three methods, a simple hat, 
linear regression based on ToBI markings, and a simple declining
slope.  This second part deals with actual F0 values and durations,
while the previous section only deals with accent (and boundary tone)
assignment. [see Intonation]"
  (let ((rval (apply_method 'Int_Target_Method utt)))
    (cond
     (rval rval) ;; new style
     ((eq 'Simple (Parameter.get 'Int_Method))
      (Int_Targets_Simple utt))
     ((eq 'ToBI (Parameter.get 'Int_Method))
      (Int_Targets_LR utt))
     ((eq 'General (Parameter.get 'Int_Method))
      (Int_Targets_General utt))
     (t
      (Int_Targets_Default utt)))))

;;;
;;;  A tree that adds accents (H) to all content words
;;;  simple but better than nothing at all
;;;
(set! simple_accent_cart_tree
 '
  ((R:SylStructure.parent.gpos is content)
   ((stress is 1)
    ((Accented))
    ((position_type is single)
     ((Accented))
     ((NONE))))
   ((NONE))))

(defvar duffint_params '((start 130) (end 110))
  "duffint_params
Default parameters for Default (duff) intonation target generation.
This is an assoc list of parameters.  Two parameters are supported
start specifies the start F0 in Hertz for an utterance, and end specifies
the end.")

;;;
;;;  For simple testing, this function adds fixed duration and 
;;;  monotone intonation to a set of phones
;;;
(defvar FP_F0 120
"FP_F0
In using Fixed_Prosody as used in Phones type utterances and hence
SayPhones, this is the value in Hertz for the monotone F0.")
(defvar FP_duration 100
"FP_duration
In using Fixed_Prosody as used in Phones type utterances and hence
SayPhones, this is the fix value in ms for phone durations.")

(define (Fixed_Prosody utt)
"(Fixed_Prosody UTT)
Add fixed duration and fixed monotone F0 to the sgements in UTT.
Uses values of FP_duration and FP_F0 as fixed values."
  (let (utt1
	(dur_stretch (Parameter.get 'Duration_Stretch))
	(orig_duffint_params duffint_params))
    (Parameter.set 'Duration_Stretch (/ FP_duration 100.0))
    (set! duffint_params (list (list 'start FP_F0) (list 'end FP_F0)))

    (set! utt1 (Duration_Default utt))
    (set! utt1 (Int_Targets_Default utt1))

    ;; Reset Parameter values back
    (Parameter.set 'Duration_Stretch dur_stretch)
    (set! duffint_params orig_duffint_params)

    utt1
    )
)

(define (segment_dpitch seg)
"(segment_dpitch UTT SEG)
Returns delta pitch, this pitch minus previous pitch."
  (- 
   (parse-number (item.feat utt seg 'seg_pitch))
   (parse-number (item.feat utt seg 'R:Segment.p.seg_pitch))))

(provide 'intonation)
