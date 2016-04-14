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
;;;  Some things for dealing with the web.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get_url url filename)
  "(get_url URL OUTFILE)
  Get URL and put contents in OUTFILE. Currently only http, and file
  type URLs are supported."

  (let ((infile (fopen (parse_url url) "rb")))
    (if infile
	(let ((outfile (fopen filename "wb")))
	  (if outfile
	      (let ((buffer "                                                                                                                               ") n)
		(while (set! n (fread buffer infile))
		       (if ( < n (length buffer))
			   (setq buffer (substring buffer 0 n)))
		       (fwrite buffer outfile))
		(fclose infile)
		(fclose outfile)
		)
	      "can't open out"
	      )
	  )
	"can't open in"
	)
    )
  )

(define (socket_open host port how)
  "(socket_open HOST PORT HOW)
   Open a file descriptor to the BSD socket on HOST at PORT.  HOW may
   be \"r\" or \"w\" for a read only or write only filedescriptor.  If
   HOW is unspecified or NIL, \"w\" is assumed.  If HOW is \"rw\" then
   a list of two file descriptors is returned, the first for reading
   the second for writing.  Take care when using the bidiectional socket
   that deadlock doesn't occur."

  (let ((file (fopen (list "tcp" host port "") how)))
    (if (or (equal? how "rw") (equal how "r+"))
	(list file file)
	file)
    )
  )
  
    
(provide 'web)
	
