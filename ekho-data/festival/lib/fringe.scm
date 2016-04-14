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
;;; Talking to fringe.

(defvar fringe_verbose nil
  "fringe_verbose
  If this is set true, all subsequent fringe connections will
  print a trace of what they are doing.")

;;; Aliases which are better suited to command line use.

(defvar fringe_name "fringe"
  "fringe_name
  The name of the last name passed to \[fringe_setup\].")

(defvar fringe_connection nil
  "fringe_connection
  A connection to fringe, used by the command line fringe functions.")

(define (fringe_setup &opt name)
  "(fringe_setup &opt name)
  Connect to fringe."

  (fringe_read_server_table)
  (if (not name) (set! name fringe_name))
  (set! fringe_connection (fringe_server "fringe"))
  (set! fringe_name name)
  )

(define (fringe command)
  "(fringe COMMAND)
  Send COMMAND to the fringe server \[fringe_connection\]
  For command line use, use (fringe_comand_string...) in scripts. "
  (if (not fringe_connection) (fringe_setup))
  (let ((val (fringe_command_string fringe_connection command)))
    (if (or (null val) (consp val))
	nil
	val)
    )
  )
  
(define (fringel package operation args)
  "(fringel PACKAGE OPERATION ARGS)
  Send a command to the fringe server \[fringe_connection\].
  For command line use, use (fringe_comand...) in scripts. "

  (if (not fringe_connection) (fringe_setup))
  (let ((val (fringe_command fringe_connection package operation args)))
    (if (or (null val) (consp val))
	nil
	val)
    )
  )

(provide 'fringe)
