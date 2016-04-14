;; SIOD: Scheme In One Defun                                  -*-mode:scheme-*-
;;
;; *                        COPYRIGHT (c) 1989-1992 BY                       *
;; *        PARADIGM ASSOCIATES INCORPORATED, CAMBRIDGE, MASSACHUSETTS.      *
;; *        See the source file SLIB.C for more information.                 *
;;
;; A fair amount of modifications and tidy up was done by AWB, particularly
;;   * adding documentation strings to all functions/variable
;;   * removing some example functions not relevant to Festival (or siod)
;;   * addition of new functions (require provide etc)

;(puts  ";; Optional Runtime Library for Release 2.8
;")

(define list 
  (lambda n 
"(list A0 A1 ...)
Return list containing A0 A1 ..."
  n))

(define (caar x) 
"(caar X)
Return the (car (car X))."
  (car (car x)))
(define (cadr x) 
"(cadr X)
Return the (car (cdr X))."
  (car (cdr x)))
(define (cdar x)
"(cdar X)
Return the (cdr (car X))."
 (cdr (car x)))
(define (cddr x) 
"(cddr X)
Return the (cdr (cdr X))."
 (cdr (cdr x)))

(define (caddr x) 
"(caddr X)
Return the (car (cdr (cdr X)))."
  (car (cdr (cdr x))))
(define (cdddr x) 
"(cdddr X)
Return the (cdr (cdr (cdr X)))."
  (cdr (cdr (cdr x))))

(define consp pair?)

(define (replace before after)
"(replace BEFORE AFTER)
Destructively replace contents of cons cell BEFORE with those of
AFTER."
  (set-car! before (car after))
  (set-cdr! before (cdr after))
  after)

(define (prognify forms)
  (if (null? (cdr forms))
      (car forms)
    (cons 'begin forms)))

(define (defmac-macro form)
"(defmac-macro MACRONAME FORM)
Define a macro.  Macro expand FORM in-line."
  (let ((sname (car (cadr form)))
	(argl (cdr (cadr form)))
	(fname nil)
	(body (prognify (cddr form))))
    (set! fname (symbolconc sname '-macro))
    (list 'begin
	  (list 'define (cons fname argl)
		(list 'replace (car argl) body))
	  (list 'define sname (list 'quote fname)))))

(define defmac 'defmac-macro)

(defmac (push form)
  (list 'set! (caddr form)
	(list 'cons (cadr form) (caddr form))))

(defmac (pop form)
  (list 'let (list (list 'tmp (cadr form)))
	(list 'set! (cadr form) '(cdr tmp))
	'(car tmp)))

;;;  Have to set var-docstrings to nil first as defvar requires it to be set
(set! var-docstrings nil)
(define (add-doc-var varname docstring)
"(add-doc-var VARNAME DOCSTRING)
  Add document string DOCSTRING to VARNAME.  If DOCSTRING is nil
  this has no effect.  If VARNAME already has a document string replace
  it with DOCSTRING."
 (if (null? docstring)
     t
     (let ((lpair (assq varname var-docstrings)))
       (if lpair
	   (set-cdr! lpair docstring)
	   (set! var-docstrings (cons (cons varname docstring) 
				      var-docstrings))))))

(set! boundp symbol-bound?)

(defmac (defvar form)
  (begin  ;; always add the documentation string
    (add-doc-var (cadr form) (car (cdddr form)))
    (list 'or
	(list 'symbol-bound? (list 'quote (cadr form)))
	(list 'define (cadr form) (caddr form)))))

(defvar var-docstrings nil
  "var-docstrings
  An assoc-list of variable names and their documentation strings.")

(defmac (defun form)
  (cons 'define
	(cons (cons (cadr form) (caddr form))
	      (cdddr form))))

(defmac (setq form)
  (let ((l (cdr form))
	(result nil))
    (define (loop)
      (if l
	  (begin (push (list 'set! (car l) (cadr l)) result)
		 (set! l (cddr l))
		 (loop))))
    (loop)
    (prognify (reverse result))))
  
(define progn begin)

(defun atom (x)
"(atom X)
True if X is not a cons cells, nil otherwise."
  (not (consp x)))

(define eq eq?)

(defmac (cond form)
  (cond-convert (cdr form)))

(define null null?)

(defun cond-convert (l)
  (if (null l)
      ()
    (if (null (cdar l))
	(if (null (cdr l))
	    (caar l)
	  (let ((rest (cond-convert (cdr l))))
	    (if (and (consp rest) (eq (car rest) 'or))
		(cons 'or (cons (caar l) (cdr rest)))
	      (list 'or (caar l) rest))))
      (if (or (eq (caar l) 't)
	      (and (consp (caar l)) (eq (car (caar l)) 'quote)))
	  (prognify (cdar l))
	(list 'if
	      (caar l)
	      (prognify (cdar l))
	      (cond-convert (cdr l)))))))

(defmac (+internal-comma form)
  (error 'comma-not-inside-backquote))

(define +internal-comma-atsign +internal-comma)
(define +internal-comma-dot +internal-comma)

(defmac (+internal-backquote form)
  (backquotify (cdr form)))

(defun backquotify (x)
"(backquote FORM)
Backquote function for expanding forms in macros."
  (let (a d aa ad dqp)
    (cond ((atom x) (list 'quote x))
	  ((eq (car x) '+internal-comma) (cdr x))
	  ((or (atom (car x))
	       (not (or (eq (caar x) '+internal-comma-atsign)
			(eq (caar x) '+internal-comma-dot))))
	   (setq a (backquotify (car x)) d (backquotify (cdr x))
		 ad (atom d) aa (atom a)
		 dqp (and (not ad) (eq (car d) 'quote)))
	   (cond ((and dqp (not (atom a)) (eq (car a) 'quote))
		  (list 'quote (cons (cadr a) (cadr d))))
		 ((and dqp (null (cadr d)))
		  (list 'list a))
		 ((and (not ad) (eq (car d) 'list))
		  (cons 'list (cons a (cdr d))))
		 (t (list 'cons a d))))
	  ((eq (caar x) '+internal-comma-atsign)
	   (list 'append (cdar x) (backquotify (cdr x))))
	  ((eq (caar x) '+internal-comma-dot)
	   (list 'nconc (cdar x)(backquotify (cdr x)))))))


(defun append n
"(append L0 L1 ...)
Append each list to the first list in turn."
  (appendl n))

(defun appendl (l)
  (cond ((null l) nil)
	((null (cdr l)) (car l))
	((null (cddr l))
	 (append2 (car l) (cadr l)))
	('else
	 (append2 (car l) (appendl (cdr l))))))

(defun append2 (a b)
  (if (null a)
      b
      (begin
        (let ((x (reverse a))
              (y b))
          (while x
            (set! y (cons (car x) y))
            (set! x (cdr x)))
          y))))

(defun rplacd (a b)
"(replacd A B)
Destructively replace the cdr of A with B."
  (set-cdr! a b)
  a)

(defun nconc (a b)
"(nconc A B)
Destructively append B to A, if A is nil return B."
  (if (null a)
      b
    (rplacd (last a) b)))

(defun last (a)
"(last A)
Last (cdr) element in list A."
  (cond ((null a) (error'null-arg-to-last))
	((null (cdr a)) a)
	((last (cdr a)))))

(define (remove i l)
"(remove ITEM LIST)
(Non-destructively) remove ITEM from LIST."
  (cond    
   ((null l) nil)
   ((eq? i (car l))
    (cdr l))
   (t
    (cons (car l) (remove i (cdr l))))))

(define (remove-duplicates l)
"(remove-duplicates LIST)
Remove duplicate items in LIST."
 (cond
  ((null l) l)
  ((member_string (car l) (cdr l))
   (remove-duplicates (cdr l)))
  (t
   (cons (car l) (remove-duplicates (cdr l))))))

(define (nth n l)
  "(nth N LIST)
Returns nth car of LIST, 0 is car."
  (if (< n 1)
      (car l)
      (nth (- n 1) (cdr l))))

(define (position thing l)
  "(position thing l)
What position is thing in l, -1 if it doesn't exist."
  (let ((p 0) (m l))
    (while (and m (not (equal? thing (car m))))
      (set! p (+ 1 p))
      (set! m (cdr m)))
    (if m p nil)))

(define (nth_cdr n l)
  "(nth_cdr N LIST)
Returns nth cdr of LIST, 0 is LIST."
  (if (< n 1)
      l
      (nth_cdr (- n 1) (cdr l))))

(define (<= a b)
"(<= NUM1 NUM2)
  Returns t if NUM1 is less than or equal to NUM2, nil otherwise.  An error is
  given is either argument is not a number."
  (or (< a b)
      (equal? a b)))

(define (>= a b)
"(>= NUM1 NUM2)
  Returns t if NUM1 is greater than or equal to NUM2, nil otherwise.  
  An error is given is either argument is not a number."
  (or (> a b)
      (equal? a b)))

(define (approx-equal? a b diff)
  "(approx-equal? a b diff)
True is the difference between a b is less than diff.  This allows equality
between floats which may have been written out and read in and hence have
slightly different precision."
  (< (if (> a b) (- a b) (- b a)) diff))

(define (assoc_string key alist)
  "(assoc_string key alist)
Look up key in alist using string-equal.  This allow indexing by
string rather than just symbols."
  (cond
   ((null alist) nil)
   ((string-equal key (car (car alist))) (car alist))
   (t (assoc_string key (cdr alist))))
)

(defvar *fasdump-hash* t)

(defun fasl-open (filename mode)
"(fasl-open FILENAME MODE)
Open fasl FILENAME as MODE. Returns a fasl-table."
  (list (fopen filename mode)
	(if (or (equal? mode "rb") *fasdump-hash*)
	    (cons-array 100))
	;; If this is set NIL, then already hashed symbols will be
	;; optimized, and additional ones will not.
	0))

(defun fasl-close (table)
"(fasl-close TABLE)
Close fasl table."
  (fclose (car table)))

(defun fasload args
"(fasload FILENAME ARGS)
Fast load FILENAME."
  (let ((filename (car args))
	(head (and (cadr args) (cons nil nil))))
    (let ((table (fasl-open filename "rb"))
	  (exp)
	  (tail head))
      (while (not (eq table (setq exp (fast-read table))))
	(cond (head
	       (setq exp (cons exp nil))
	       (set-cdr! tail exp)
	       (setq tail exp))
	      ('else
	       (eval exp))))
      (fasl-close table)
      (and head (cdr head)))))

(defun fasdump (filename forms)
"(fasdump FILENAME FORMS)
Fast dump FORMS into FILENAME."
  (let ((table (fasl-open filename "wb"))
	(l forms))
    (while l
      (fast-print (car l) table)
      (set! l (cdr l)))
    (fasl-close table)))

(defun compile-file (filename)
"(compile-file FILENAME)
Compile lisp forms in FILENAME.scm to FILENAME.bin."
  (let ((forms (load (string-append filename ".scm") t)))
    (puts "Saving forms
")
    (fasdump (string-append filename ".bin")
	     forms)))

(defvar *properties* (cons-array 100)
  "*properties*
Array for holding symbol property lists.")

(defun get (sym key)
"(get SYM KEY)
Get property named KEY for SYM."
  (cdr (assq key (href *properties* sym))))

(defun putprop (sym val key)
"(putprop SYM VAL KEY)
Put property VAL named KEY for SYM."
  (let ((alist (href *properties* sym)))
    (let ((cell (assq key alist)))
      (cond (cell
	     (set-cdr! cell val))
	    ('else
	     (hset *properties* sym (cons (cons key val) alist))
	     val)))))

;;(define (mapcar1 f l1)
;;  (and l1 (cons (f (car l1)) (mapcar1 f (cdr l1)))))

;; An iterative version of the above
(define (mapcar1 f l1)
  (let ((l2 l1) (r nil))
    (while l2
      (set! r (cons (f (car l2)) r))
      (set! l2 (cdr l2)))
    (reverse r)))

;;(define (mapcar2 f l1 l2)
;;  (and l1 l2 (cons (f (car l1) (car l2)) (mapcar2 f (cdr l1) (cdr l2)))))

;; An iterative version
(define (mapcar2 f l1 l2)
  (let ((a1 l1) (a2 l2) (r nil))
    (while a1
      (set! r (cons (f (car a1) (car a2)) r))
      (set! a1 (cdr a1))
      (set! a2 (cdr a2)))
    (reverse r)))

(define (mapcar . args)
"(mapcar FUNCTION ARGS [ARGS2])
Apply FUNCTION to each member of ARGS (and [ARGS2]), returning list of
return values."
  (cond ((null args)
	 (error "too few arguments"))
	((null (cdr args))
	 (error "too few arguments"))
	((null (cdr (cdr args)))
	 (mapcar1 (car args) (car (cdr args))))
	((null (cdr (cdr (cdr args))))
	 (mapcar2 (car args) (car (cdr args)) (car (cdr (cdr args)))))
	('else
	 (error "two many arguments"))))

;; will be set automatically on start-up
(defvar libdir '<automatically_set>
  "libdir
  The pathname of the run-time libary directory.  Note reseting is 
  almost definitely not what you want to do.   This value is automatically
  set at start up from the value specifed at compile-time or the value
  specifed with --libdir on the command line.  A number of other variables
  depend on this value.")

(defvar load-path (list libdir)
  "load-path
  A list of directories containing .scm files.  Used for various functions
  such as load_library and require.  Follows the same use as EMACS.  By
  default it is set up to the compile-time library directory but may be 
  changed by the user at run time, by adding a user's own library directory
  or even replacing all of the standard library. [see Site initialization]")

;; will be set automatically on start-up
(defvar *ostype* 'unknown
  "*ostype*
  Contains the name of the operating system type that Festival is running
  on, e.g. SunOS5, FreeBSD, linux etc.  The value is taken from the Makefile
  variable OSTYPE at compile time.")

(defvar etc-path (path-append libdir "etc/" *ostype*)
  "etc-path
  A list of directories where binaries specific to Festival may be located.
  This variable is automatically set to LIBDIR/etc/OSTYPE/
  and that path is added to the end of the UNIX PATH environment variable.")

(define (library_expand_filename fname)
"(library_expand_filename FILENAME)
  Search for filename by appending FILENAME to each member of load-path.
  Full expanded pathname is returned.  If not found in load-path FILENAME
  is returned."
  (let ((p load-path)
	(found nil))
    (while (and p (null? found))
      (let ((pot-file (path-append (car p) fname)))
	(if (probe_file pot-file)
	    (setq found pot-file))
	(setq p (cdr p))))
    (if (null? found)
	fname
      found)))

(define (load_library fname)
"(load_library FILENAME)
  Load file from library, appends FILENAME to each path in load-path
  until a valid file is found. If none found loads name itself"
  (load (library_expand_filename fname)))

(define (fasload_library fname)
"(fasload_library FILENAME)
  Load binary file from library"
  (fasload (library_expand_filename fname)))

(define (member item list)
"(member ITEM LIST)
  Returns subset of LIST whose car is ITEM if it exists, nil otherwise."
  (if (consp list)
      (if (equal? item (car list))
	  list
	(member item (cdr list)))
    nil))

(define (member_string item list)
"(member_string STRING LIST)
  Returns subset of LIST whose car is STRING if it exists, nil otherwise."
  (if (consp list)
      (if (string-equal item (car list))
	  list
	(member_string item (cdr list)))
    nil))

(defvar provided nil
  "provided
  List of file names (omitting .scm) that have been provided.  This list
  is checked by the require function to find out if a file needs to be 
  loaded.  If that file is already in this list it is not loaded.  Typically
  a file will have (provide 'MYNAME) at its end so that a call to 
  (require 'MYNAME) will only load MYNAME.scm once.")

(define (require fname)
"(require FILENAME)
  Checks if FNAME is already provided (member of variable provided) if not 
  loads it, appending \".scm\" to FILENAME.  Uses load_library to find 
  the file."
 (let ((bname (intern (basename fname))))
  (if (null? (member bname provided))
      (progn 
        ;;; Compiled files aren't faster, so we don't do this
	; (fasload_library (string-append fname ".bin"))
       (load_library (string-append fname ".scm"))
	't)
    nil)))

(define (request fname)
"(request FILENAME)
  Checks if FNAME is already provided (member of variable provided) if not 
  tries to loads it, appending \".scm\" to FILENAME.  Uses load_library 
  to find the file. Unlike require, fname isn't found no error occurs"
 (unwind-protect (require fname)))

(define (provide fname)
"(provide FILENAME)
  Adds FNAME to the variable provided (if not already there).  This means
  that future calls to (require FILENAME) will not cause FILENAME to
  be re-loaded."
  (if (null? (member fname provided))
      (set! provided (cons fname provided))))

(define (apply_hooks hooks obj)
"(apply_hooks HOOK OBJ)
  Apply HOOK(s) to OBJ.  HOOK is a function or list of functions that
  take one argument."
(cond
 ((null? hooks) obj)
 ((consp hooks) 
  (apply_hooks (cdr hooks) ((car hooks) obj)))
 (t (hooks obj))))

(define (apply func args)
"(apply FUNC ARGS)
Call FUNC with ARGS as arguments."
  (eval
   (cons func
	 (mapcar (lambda (a) (list 'quote a)) args))))

(defmac (autoload form)
"(autoload FUNCTION FILENAME DOCSTRING)
Define FUNCTION that when called automatically loads FILENAME
and calls FUNCTION (assumed to be defined in FILENAME)."
  (list 'define
	(cadr form)
	(list 
	 'lambda
	 'n
	 (list 'let (list (list 'me  (cadr form)))
	       (list 'require (car (cdr (cdr form))))
	       (list 'if (list 'eq 'me (cadr form))
		     (list 'error
			   (list 'string-append 
				 "autoload: \""
				 (car (cdr (cdr form)))
				 ".scm\" does not define "
				 (list 'quote (cadr form)))))
	      
	       (list 'apply (cadr form) 'n)))))

(define (:backtrace frame)
"(:backtrace [FRAME])
This function called *immediately* after an error will display a backtrace
of the functions evaluated before the error.  With no arguments it
lists all stack frames, with the (possibly shortened) forms that were
evaluated at that level.  With a numeric argument it displays
the form at that level in full.  This function only works at
top level in the read-eval-print loop (command interpreter).  Note
that any valid command will leave the backtrace stack empty. Also
note that backtrace itself does not reset the backtrace, unless you
make an error in calling it."

"The function is interpreted specially by the read-eval-interpreter
and hence has no body, its actual body is defined in 
src/arch/siod-3.0/slib.cc."
)

(defvar hush_startup nil
  "hush_startup
  If set to non-nil, the copyright banner is not displayed at start up.")

(defvar editline_histsize 256
  "editline_histsize
  The number of lines to be saved in the users history file when a 
  Festival session ends.  The histfile is \".festival_history\" in the
  users home directory.  Note this value is only checked when the 
  command interpreter is started, hence this should be set in a user's
  \".festivalrc\" or system init file.  Reseting it at the command
  interpreter will have no effect.")

(defvar editline_no_echo (getenv "EMACS")
  "editline_no_echo
  When running under Emacs as an inferior process, we don't want to 
  echo the content of the line, only the prompt.")

(defvar ! nil
  "!
  In interactive mode, this variable's value is the return value of the
  previously evaluated expression.")

(provide 'siod)
