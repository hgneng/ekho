;;;
;;;  File: festival.el
;;;  Emacs Lisp
;;;
;;;  Alan W Black  CSTR (awb@cstr.ed.ac.uk) June 1996
;;;
;;;  Provide an emacs mode for interfacing to the festival speech
;;;  synthesizer system
;;;
;;;  I've looked at many examples from the emacs Lisp directory
;;;  copying relevant bits from here and there, so this can only
;;;  reasonably inherit the GNU licence (GPL)
;;;
;;;  Setup:
;;;  In your .emacs add the following 2 lines to get a Say menu:
;;;
;;;  (autoload 'say-minor-mode "festival" "Menu for using Festival." t)
;;;  (say-minor-mode t)
;;;  (setq auto-mode-alist 
;;;     (append '(("\\.festivalrc$" . scheme-mode)) auto-mode-alist))
;;;
;;;  The following gives you pretty colors in emacs-19 if you are into
;;;  such things 
;;;  ;;;  Some colors for scheme mode
;;;  (hilit-set-mode-patterns
;;;   '(scheme-mode)
;;;   '(
;;;     (";.*" nil comment)
;;;     (hilit-string-find ?\\ string)
;;;     ("^\\s *(def\\s +" "\\()\\|nil\\)" defun)
;;;     ("^\\s *(defvar\\s +\\S +" nil decl)
;;;     ("^\\s *(set\\s +\\S +" nil decl)
;;;     ("^\\s *(defconst\\s +\\S +" nil define)
;;;     ("^\\s *(\\(provide\\|require\\).*$" nil include)
;;;     ("(\\(let\\*?\\|cond\\|if\\|or\\|and\\|map\\(car\\|concat\\)\\|prog[n1*]?\\|while\\|lambda\\|function\\|Parameter\\|set\\([qf]\\|car\\|cdr\\)?\\|nconc\\|eval-when-compile\\|condition-case\\|unwind-protect\\|catch\\|throw\\|error\\)[ \t\n]" 1 keyword)))
;;;  
;;;
;;;--------------------------------------------------------------------
;;;               Copyright (C) Alan W Black 1996
;;; This code is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY. No author or distributor accepts
;;; responsibility to anyone for the consequences of using this code
;;; or for whether it serves any particular purpose or works at all,
;;; unless explicitly stated in a written agreement.
;;;
;;; Everyone is granted permission to copy, modify and redistribute
;;; this code, but only under the conditions described in the GNU
;;; Emacs General Public License.  A copy of this license is
;;; distrubuted with GNU Emacs so you can know your rights and
;;; responsibilities.  It should be in a file named COPYING.  Among
;;; other things, the copyright notice and this notice must be
;;; preserved on all copies.
;;;--------------------------------------------------------------------
;;;  

(defvar festival-program-name "festival")

(defvar festival-process nil)

(defvar festival-tmp-file
  (format "/tmp/festival-emacs-tmp-%s" (user-real-login-name))
 "Filename to save input for Festivial.")

(defun festival-fast () 
  (interactive)
  (festival-send-command '(Parameter.set 'Duration.Stretch 0.8)))
(defun festival-slow () 
  (interactive)
  (festival-send-command '(Parameter.set 'Duration.Stretch 1.2)))
(defun festival-ndur () 
  (interactive)
  (festival-send-command '(Parameter.set 'Duration.Stretch 1.0)))
(defun festival-intro () 
  (interactive)
  (festival-send-command '(intro)))

(defun festival-gsw () 
  (interactive)
  (festival-send-command '(voice_gsw_diphone)))
(defun festival-rab () 
  (interactive)
  (festival-send-command '(voice_rab_diphone)))
(defun festival-ked () 
  (interactive)
  (festival-send-command '(voice_ked_diphone)))
(defun festival-kal () 
  (interactive)
  (festival-send-command '(voice_kal_diphone)))
(defun festival-don () 
  (interactive)
  (festival-send-command '(voice_don_diphone)))
(defun festival-welsh () 
  (interactive)
  (festival-send-command '(voice_welsh_hl)))
(defun festival-spanish () 
  (interactive)
  (festival-send-command '(voice_spanish_el)))

(defun festival-say-string (string)
   "Send string to festival and have it said"
   (interactive "sSay: ")
   (festival-start-process)
   (process-send-string festival-process 
			(concat "(SayText " (format "%S" string) ")
")))

(defun festival-send-command (cmd)
   "Send command to festival"
   (interactive "px")
   (festival-start-process)
   (process-send-string festival-process (format "%S
" cmd)))

(defun festival-process-status ()
  (interactive)
  (if festival-process
      (message (format "Festival process status: %s" 
		       (process-status festival-process)))
    (message (format "Festival process status: NONE"))))

(defun festival-start-process ()
  "Check status of process and start it if necessary"
  (interactive )
  (let ((process-connection-type t))
    (if (and festival-process
	     (eq (process-status festival-process) 'run))
	't
      ;;(festival-kill-festival t)
      (message "Starting new synthesizer process...")
      (sit-for 0)
      (setq festival-process
	    (start-process "festival" (get-buffer-create "*festival*")
			   festival-program-name)))
    ))

(defun festival-kill-process ()
  "Kill festival sub-process"
  (interactive)
  (if festival-process
      (kill-process festival-process))
  (setq festival-process nil)
  (message "Festival process killed"))

(defun festival-send-string (string)
  "Send given string to fesitval process."
  (interactive)
  (festival-start-process)
  (process-send-string festival-process string))

(defun festival-say-region (reg-start reg-end)
  "Send given region to festival for saying.  This saves the region
as a file in /tmp and then tells festival to say that file.  The
major mode is *not* passed as text mode name to Festival."
  (interactive "r")
  (write-region reg-start reg-end festival-tmp-file)
  (festival-send-command (list 'tts festival-tmp-file nil)))

(defun festival-say-buffer ()
  "Send given region to festival for saying.  This saves the region
as a file in /tmp and then tells festival to say that file.  The
major-mode is passed as a text mode to Festival."
  (interactive)
  (write-region (point-min) (point-max) festival-tmp-file)
  ;; Because there may by sgml-like sub-files mentioned 
  ;; ensure festival tracks the buffer's default-directory
  (festival-send-command (list 'cd (expand-file-name default-directory)))
  (if (equal "-mode" (substring (format "%S" major-mode) -5 nil))
      (if (equal "sgml" (substring (format "%S" major-mode) 0 -5))
	  (festival-send-command 
	   (list 'tts festival-tmp-file "sable"))
	(festival-send-command 
	 (list 'tts festival-tmp-file 
	       (substring (format "%S" major-mode) 0 -5))))
    (festival-send-command (list 'tts festival-tmp-file nil))))

;;
;; say-minor-mode provides a menu offering various speech synthesis commands
;;
(defvar say-minor-mode nil)

(defun say-minor-mode (arg)
  "Toggle say minor mode.
With arg, turn say-minor-mode on iff arg is positive."
  (interactive "P")
  (setq say-minor-mode
	(if (if (null arg) (not say-minor-mode)
	      (> (prefix-numeric-value arg) 0))
	    t))
  (force-mode-line-update))

(setq say-params-menu (make-sparse-keymap "Pitch/Duration"))
(fset 'say-params-menu (symbol-value 'say-params-menu))
(define-key say-params-menu [say-fast] '("Fast" . festival-fast))
(define-key say-params-menu [say-slow] '("Slow" . festival-slow))
(define-key say-params-menu [say-ndur] '("Normal Dur" . festival-ndur))

(setq say-lang-menu (make-sparse-keymap "Select language"))
(fset 'say-lang-menu (symbol-value 'say-lang-menu))
(define-key say-lang-menu [say-lang-spain1] '("Spanish el" . festival-spanish))
(define-key say-lang-menu [say-lang-welsh1] '("Welsh hl" . festival-welsh))
(define-key say-lang-menu [say-lang-eng5] '("English gsw" . festival-gsw))
(define-key say-lang-menu [say-lang-eng4] '("English don" . festival-don))
(define-key say-lang-menu [say-lang-eng3] '("English rab" . festival-rab))
(define-key say-lang-menu [say-lang-eng2] '("English ked" . festival-ked))
(define-key say-lang-menu [say-lang-eng1] '("English kal" . festival-kal))
;(define-key say-params-menu [say-set-dur-stretch] 
;  '("Set Duration Stretch" . festival-set-dur-stretch))
;(define-key say-params-menu [say-high] '("High" . festival-high))
;(define-key say-params-menu [say-low] '("Low" . festival-low))
;(define-key say-params-menu [say-npit] '("Normal Pitch" . festival-npit))
;(define-key say-params-menu [say-set-pitch-stretch] 
;  '("Set Pitch Stretch" . festival-set-pitch-stretch))

(setq say-minor-mode-map (make-sparse-keymap))
(setq say-menu (make-sparse-keymap "SAY"))
(define-key say-minor-mode-map [menu-bar SAY] (cons "Say" say-menu))
(define-key say-minor-mode-map [menu-bar SAY festival-intro] '("Festival Intro" . festival-intro))
(define-key say-minor-mode-map [menu-bar SAY festival-process-status] '("Festival status" . festival-process-status))
(define-key say-minor-mode-map [menu-bar SAY festival-kill-process] '("Kill Festival" . festival-kill-process))
(define-key say-minor-mode-map [menu-bar SAY festival-start-process] '("(Re)start Festival" . festival-start-process))
;;(define-key say-menu [separator-process] '("--"))
;;(define-key say-menu [params] '("Pitch/Durations" . say-params-menu))
(define-key say-menu [separator-buffers] '("--"))
(define-key say-menu [festival-send-command] '("Festival eval command" . festival-send-command))
(define-key say-menu [say-lang-menu] '("Select language" . say-lang-menu))
(define-key say-menu [festival-say-buffer] '("Say buffer" . festival-say-buffer))
(define-key say-menu [festival-say-region] '("Say region" . festival-say-region))


(setq minor-mode-map-alist
      (cons
       (cons 'say-minor-mode say-minor-mode-map)
       minor-mode-map-alist))

(or (assq 'say-minor-mode minor-mode-alist)
              (setq minor-mode-alist
                    (cons '(say-minor-mode "") minor-mode-alist)))

;;;
;;;  A FESTIVAL inferior mode  (copied from prolog.el)
;;;
(defvar inferior-festival-mode-map nil)

(defun inferior-festival-mode ()
  "Major mode for interacting with an inferior FESTIVAL process.

The following commands are available:
\\{inferior-festival-mode-map}

Entry to this mode calls the value of `festival-mode-hook' with no arguments,
if that value is non-nil.  Likewise with the value of `comint-mode-hook'.
`festival-mode-hook' is called after `comint-mode-hook'.

You can send text to the inferior FESTIVAL from other buffers
using the commands `send-region', `send-string'

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
\\[comint-kill-input] and \\[backward-kill-word] are kill commands, imitating normal Unix input editing.
\\[comint-interrupt-subjob] interrupts the shell or its current subjob if any.
\\[comint-stop-subjob] stops. \\[comint-quit-subjob] sends quit signal."
  (interactive)
  (require 'comint)
  (comint-mode)
  (setq major-mode 'inferior-festival-mode
	mode-name "Inferior FESTIVAL"
	comint-prompt-regexp "^festival> ")
  (if inferior-festival-mode-map nil
    (setq inferior-festival-mode-map (copy-keymap comint-mode-map))
    (festival-mode-commands inferior-festival-mode-map))
  (use-local-map inferior-festivalr-mode-map)
  (run-hooks 'festival-mode-hook))

;;;###autoload
(defun run-festival ()
  "Run an inferior FESTIVAL process, input and output via buffer *festival*."
  (interactive)
  (require 'comint)
  (switch-to-buffer (make-comint "festival" festival-program-name))
  (inferior-festival-mode))

(provide 'festival)
