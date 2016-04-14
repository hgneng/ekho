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
;;;                         Date:   August 1996
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Save documentation strings as texinfo files 
;;;
;;;  Finds all functions with documentation, and all variables with
;;;  documentation, sorts and dumps the information in doc/festfunc.texi
;;;  and doc/festvars.texi
;;;
;;;  The makefile in the doc directory runs the compiled festival binary and
;;;  causes these files to be created form the currently defined functions
;;;  and variables
;;;
;;;  Also provides function to extract manual section for documentation
;;;  string and send a url to Netscape to display it 
;;;

(define (make-doc)
"(make-doc)
Find function and variable document strings and save them in texinfo
format to respective files."
 (format t "Making function, feature and variable lists\n")

 ;; Need to ensure all library files are actually loaded if they contain
 ;; funcstions/variables which have to be put in the manual
 (require 'display)
 (require 'mbrola)
 (require 'tilt)

 (make-a-doc "festfunc.texi" 'function)
 (make-a-doc "festfeat.texi" 'features)
 (make-a-doc "festvars.texi" 'vars))

(define (make-a-doc outfile doclist)
"(make-a-doc FILENAME DOCLIST)
Make a texinfo document in FILENAME as a texinfo table, items are
from DOCLIST.  DOCLIST names which doclist to use, it may be
one of 'function, 'features or 'vars."
  (let ((outfp (fopen outfile "wb")))
   (format outfp "@table @code\n")
   ;; Yes I am so lazy I'm not willing to write a sort function in Scheme
   (sort-and-dump-docstrings doclist outfp)
   (format outfp "@end table\n")
   (fclose outfp)))

;;;
;;;  Documentation string may refer to a section in the manual
;;;  If it does then we can automatically go to that section in the
;;;  menu using Netscape.
;;;

(defvar manual-browser "netscape"
"manual-browser
The Unix program name of your Netscape Navigator browser.
[see Getting some help]")

(defvar manual-url 
  (format nil "http://www.cstr.ed.ac.uk/projects/festival/manual-%s.%s.%s/"
	  (car festival_version_number)
	  (car (cdr festival_version_number))
	  (car (cdr (cdr festival_version_number))))
"manual-url
The default URL for the Festival Manual in html format.  You may
reset this to a file://.../... type URL on you're local machine.
[see Getting some help]")

;;;  Paul got this idea from VM, the email system for emacs and
;;;  I found out how to do this from their code, thanks Kyle

(define (send-url-to-netscape url)
"(send-url-to-netscape URL)
Send given URL to netscape for display.  This is primarily used to 
display parts of the manual referenced in documentation strings."
  (system
   (string-append
    manual-browser
    " -remote \"openURL( "
    url
    " )\" ")))

(define (lastline string)
"(lastline STRING)
Returns the part of the string which between the last newline and the 
end of string."
  (let ((ns (string-after string "\n")))
    (if (string-equal ns "")
	string
	(lastline ns))))

(define (manual-sym symbol)
"(manual-sym SYMBOL)
Display the section in the manual that SYMBOL's docstring has
identified as the most relevant.  The section is named on the
last line of a documentation string with no newlines within it
prefixed by \"[see \" with a \"]\" just immediately before the end
of the documentation string.  The manual section name is translated to
the section in the HTML version of the manual and a URL is
and sent to Netscape for display.  [see Getting some help]"
(let ((section (string-before (string-after 
			       (lastline (eval (list 'doc symbol)))
			       "[see ")
			      "]")))
  (cond
   ((string-equal section "")
    (eval (list 'doc symbol)))  ;; nothing there
   (t
    (manual section)))))

(define (manual section)
"(manual SECTION)
Display SECTION in the manual.  SECTION is a string identifying
a manual section (it could be an initial substring.  If SECTION
is nil or unspecifed then the Manual table of contents is displayed.
This uses netscape to display the manual page so you must have that
(use variable manual-browser to identify it) and the variable 
manual-url pointing to a copy of the manual. [see Getting some help]"
(let ((tmpfile (make_tmp_filename))
      (manual-section))
  (cond
   ((string-matches section "\"")
    (string-append "Invalid section reference containing quote: "
		   section "\n"))
   ((not section)
    (send-url-to-netscape (string-append manual-url "festival_toc.html")))
   (t                            ;; find section in manual
    (get_url (string-append manual-url "festival_toc.html") tmpfile)
    (system
     (string-append
      "grep -i \"^<LI><A NAME.*" section "\" \"" tmpfile 
      "\" | sed 's/^.*HREF=.//' | sed 's/.>.*$//' > \""
      tmpfile ".out\""))
    (set! manual-section (load (string-append tmpfile ".out") t))
    (cond
     ((not manual-section)
      (string-append "No section called: " section))
     (t
      (send-url-to-netscape (string-append manual-url (car manual-section)))
      (delete-file tmpfile)
      (delete-file (string-append tmpfile ".out"))
      "Sent manual reference url to netscape."))))))

(provide 'festdoc)




