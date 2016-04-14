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
;;;                         Author: Alan W Black
;;;                         Date:   December 1996
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  An xwaves display function for utterances
;;;
;;;  Requires Xwaves to be running, saves labels etc and sends 
;;;  messages to Xwaves to display the utterance.
;;;
;;;  This can be a model for other display functions.
;;;

(define (display utt)
"(display utt)
Display an utterance's waveform, F0 and segment labels in Xwaves.
Xwaves must be running on the current machine, with a labeller for
this to work."
  (let ((tmpname (make_tmp_filename)))
    (utt.save.wave utt (string-append tmpname ".wav"))
    (utt.save.segs utt (string-append tmpname ".lab"))
    (utt.save.f0 utt (string-append tmpname ".f0"))
    (system (format nil "send_xwaves make file %s name %s height 150"
		    (string-append tmpname ".f0") tmpname))
    (system (format nil "send_xwaves make name %s file %s height 200"
		    tmpname (string-append tmpname ".wav")))
    (system (format nil "send_xwaves send make file %s name %s color 125"
		    (string-append tmpname ".lab") tmpname))
    (system (format nil "send_xwaves send activate name %s fields 1"
		    tmpname))
    (system (format nil "send_xwaves %s align file %s"
		    tmpname (string-append tmpname ".wav"))))
 )

(provide 'display)




