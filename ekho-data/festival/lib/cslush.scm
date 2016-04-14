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
;;;  Functions specific to using Festival in cslush part of the OGI toolkit
;;;  The OGI toolkit is a complete dialog building system with speech
;;;  recognition and synthesis (Festival) it is available for free for
;;;  research purposes from
;;;     http://www.cse.ogi.edu/CSLU/toolkit/toolkit.html
;;;
;;;  Note this cslush interface requires you to compile festival
;;;  with tcl (7.6)
;;;
;;;  The functions replace the C++ level functions Jacques H. de Villiers
;;;  <jacques@cse.ogi.edu> from CSLU wrote for the previous version
;;;

(if (not (member 'tcl *modules*))
      (error "cslush: can't load cslush, TCL not supported in this installation of Festival."))

(define (cslush.getwave utt)
"(cslush.getwave UTT)
Extract wave memory info, pass this to wave import in CSLUsh."
 (format nil "%s %s %s"
	 (utt.wave.info utt 'data_addr)
	 (utt.wave.info utt 'num_samples)
	 (utt.wave.info utt 'sample_rate)))

(define (cslush.getphone utt)
"(cslush.getphone UTT)
Return segment names a single string of phones, for use to pass to
TCL."
  (let ((phones ""))
    (mapcar
     (lambda (s)
       (if (string-equal phones "")
	   (set! phones (format nil "%s" (utt.streamitem.feat utt s 'name)))
	   (set! phones (format nil "%s %s" 
				phones (utt.streamitem.feat utt s 'name)))))
     (utt.stream utt 'Segment))
    phones))

(define (cslush TCLCOMMAND)
"(cslush TCLCOMMAND)
Pass TCLCOMMAND to TCL interpreter, returns what TCL returns as a
string."
  (tcl_eval TCLCOMMAND))


(provide 'cslush)
