;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
;;;                       Copyright (c) 2003, 2004                        ;;
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
;;; xwaves interface for festival for multisyn (Rob Clark)
;;;
;;; This is never loaded by defualt.
;;; You'd need to change the paths here for this to currently work outside of CSTR.
;;; If anyone else ends up using it let me know and I'll make it more robust.
;;;

;; Send commands to xwaves

(defvar send_xwaves_command "/cstr/linux/entropic/esps531.linux/bin/send_xwaves")
(defvar spectrogram_command "/cstr/linux/entropic/esps531.linux/bin/sgram")
(defvar data_path "/projects/cougar/data/cstr/nina")

(set! xw_object_count 0)
(set! xw_active_list nil)

;;
;; Display a synthesised utterance
;;
(define (xwaves_display_utterance utt)
"(xwaves_display_utterance utt)
Display join and target information for an utterance."
  (let ((units (utt.relation.items utt 'Unit))
	(object (xw_name_object))
	wavfile specfile segfile diphfile joinfile targfile sourcefile timefile)

    (set! wavfile (xw_make_tmp_filename object))
    (set! specfile (xw_make_tmp_filename object))
    (set! segfile (xw_make_tmp_filename object))
    (set! diphfile (xw_make_tmp_filename object))
    (set! joinfile (xw_make_tmp_filename object))
    (set! targfile (xw_make_tmp_filename object))
    (set! sourcefile (xw_make_tmp_filename object))
    (set! timefile (xw_make_tmp_filename object))
    
    ; display resulting waveform
    (utt.save.wave utt wavfile 'riff)
    (xwaves_show_general object wavfile 1500 200 10 10)
    ; display resulting spectrogram
    (xw_genspec wavfile specfile)
    (xwaves_show_general object specfile 1500 400 10 260)
    ; segments
    (utt.save.unit_selection_segs utt segfile)
    (xwaves_show_labels object segfile specfile)
    ; Unit information
    (utt.save.unit_selection_info utt diphfile joinfile targfile sourcefile timefile)
    (xwaves_show_labels object timefile specfile)
    (xwaves_show_labels object sourcefile specfile)
    (xwaves_show_labels object targfile specfile)
    (xwaves_show_labels object joinfile specfile)
    (xwaves_show_labels object diphfile specfile)
    ; mark files
    (xw_register_active object (list wavfile specfile segfile diphfile joinfile sourcefile timefile))
))

;;
;; Edit a diphone source
;;

(define (xwaves_edit_diphone utt id)
  "(xwaves_edit_diphone utt id)
Access the source diphone for label correction."
(let ((diphone nil)
      segfilename
      wavefilename
      (utt (Utterance Text nil))
      segs
      (seg nil)
      (start 0) 
      end)
  
  ;; find unit.
  (mapcar
   (lambda (unit)
     (if (string-equal (format nil "_%s" id) (item.feat unit id))
	 (set! diphone unit)))
   (utt.relation.items utt 'Unit))
  (if (null diphone)
      (error (format nil "Diphone with id _%s not found in utterance.")))
  (set! uttname (item.feat diphone "source_utt"))
  (set! end (item.feat diphone "source_end"))

  (set! segfilename  (format nil "%s/lab/%s.lab" data_path uttname))
  (set! wavefilename (format nil "%s/wav/%s.wav" data_path uttname))
  (utt.relation.load utt 'Segment segfilename)

  (set! segs (utt.relation.items utt 'Segment))
  (while (and segs 
	      (not (equal? (item.feat (car segs) "end") end)))
	 (set! segs (cdr segs)))
  
  if null seg ...

    (if (item.prev diphone)
      (set! start (item.feat seg "start"))
      (set! start 0))


))
  
  



;;
;; Interface with xwaves.
;;


(define (xwaves_show_general object file width height xpos ypos)
"(xwaves_show_general object file width height xpos ypos)
Display an wave or track file."
  (xw_send (format nil "make name %s file %s width %d height %d loc_x %d loc_y %d" object file width height xpos ypos)))

(define (xwaves_show_wave object file)
"(xwaves_show_wave object file)
Display a waveform."
  (xwaves_show_general object file 1500 200 10 10))

(define (xwaves_show_labels object file attachto)
"(xwaves_show_labels object file attachto)
Display a label file."
  (xw_send (format nil "send make signal %s name %s file %s color 125" attachto object file))
  (xw_send "send activate fields 1 2 3 4 5"))


(define (xwaves_attach_xlabel)
"(xwaves_attach_xlabel)
Attach xlabel to xwaves."
  (xw_send "attach function xlabel"))

(define (xwaves_set_markers object left right)
"(xwaves_set_markers object left right)
Set the markers."
  (xw_send (format nil "%s set l_marker_time %f" object left))
  (xw_send (format nil "%s set r_marker_time %f" object right)))

(define (xwaves_bracket_markers object file)
"(xwaves_bracket_markers object file)
Bracket markers."
  (xw_send (format nil "%s bracket file %s " object file)))

(define (xwaves_close_windows object)
"(xwaves_close_windows object)
Close currently open windows related to object or all if nil.."
(cond
 ((null object)
  (xw_send "kill"))
 (t 
  (xw_send (format nil "kill name %s" object))))
(xw_clear_active_list object))


(define (xwaves_wait)
"(xwaves_wait)
Wait for xwaves continue signal."
  (xw_send "pause"))


;;
;; Object naming
;;
(define (xw_name_object)
"(xw_name_object)
Generate a name for this object."
(let (name)
  (set! name (string-append "obj" xw_object_count))
  (set! xw_object_count (+ xw_object_count 1))
  name))

;;
;; Temp file lists
;;

(define (xw_clear_active_list object)
"(xw_clear_active_list)
Clear active list of specified object, or all if nil."
(let (new_active_list)
(mapcar
 (lambda (objectlist)
   (cond
    ((or (null object)
	 (string-equal object (car objectlist)))
     (mapcar
      (lambda (file)
	(delete-file file))
      (cadr objectlist)))
    (t
     (set! new_active_list (cons objectlist new_active_list)))))
 xw_active_list)
(set! xw_active_list new_active_list))
nil)


(define (xw_register_active object flist)
  "(xw_register_active object flist)
Adds  an object and its filenames to the active list."
  (set! xw_active_list (cons (cons object (list flist)) xw_active_list))
  nil)

(define (xw_make_tmp_filename object)
  "(xw_make_tmp_filename)
make tmp file name which incorporates object name."
(format nil "%s_%s" (make_tmp_filename) object))


;;
;; Low level xwaves stuff.
;;

(define (xw_genspec wavfile specfile)
"(xw_genspec wavfile specfile)
Generate a spectrogram file."
  (system (format nil "%s -dHAMMING -o8 -E0.94 -S2 -w8 %s %s\n" spectrogram_command wavfile specfile)))

(define (xw_send command)
"(xw_send command)
Send a command to xwaves."
  (system (format nil "%s %s\n" send_xwaves_command command)))



;;
;; General Festival stuff.
;;


(define (utt.save.unit_selection_segs utt filename)
"(utt.save.unit_selection_segs utt filename)
  Save unit selection segments of UTT in a FILE in xlabel format."
  (let ((fd (fopen filename "w")))
    (format fd "#\n")
    (mapcar
     (lambda (info)
       (format fd "%2.4f 100 %s\n" (car info) (car (cdr info))))
     (utt.features utt 'Segment '(source_end name)))
    (fclose fd)
    utt))

(define (utt.save.unit_selection_info utt diphfile joinfile targfile sourcefile timefile)
"(utt.save.unit_selection_info utt diphfile joinfile targfile sourcefile timefile)
  Save stuff in xlabel format."
  (let ((fdd (fopen diphfile "w"))
	(fdj (fopen joinfile "w"))
	(fdt (fopen targfile "w"))
	(fds (fopen sourcefile "w"))
	(fdx (fopen timefile "w"))
	real_join)
    (format fdd "#\n")
    (format fdj "#\n")
    (format fdt "#\n")
    (format fds "#\n")
    (format fdx "#\n")
    (mapcar
     (lambda (unit)
       (set! real_join "")
       (if (item.next unit)
	   (if (not (string-equal (item.feat unit 'source_utt)
				  (item.feat (item.next unit) 'source_utt)))
	       (set! real_join "*")))
       (format fdd "%2.4f 100 %s %s\n" 
	       (item.feat unit 'end)
	       (item.feat unit 'name)
	       real_join)
       (format fdj "%2.4f 100 %s\n" 
	       (item.feat unit 'end)
	       (if (item.next unit)
		   (item.feat (item.next unit) 'join_cost)
		   0))
       (format fdt "%2.4f 100 %s\n" 
	       (item.feat unit 'end)
	       (item.feat unit 'target_cost))
       (format fds "%2.4f 100 %s\n" 
	       (item.feat unit 'end)
	       (item.feat unit 'source_utt))
       (format fdx "%2.4f 100 %s\n" 
	       (item.feat unit 'end)
	       (item.feat unit 'source_end)))
     (utt.relation.items utt 'Unit))
    (fclose fdd)
    (fclose fdj)
    (fclose fdt)
    (fclose fds)
    (fclose fdx)
    utt))
