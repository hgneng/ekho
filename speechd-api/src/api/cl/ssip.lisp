;;; Speech Synthesis Interface Protocol (SSIP) interface

;; Author: Milan Zamazal <pdm@brailcom.org>

;; Copyright (C) 2004 Brailcom, o.p.s.

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 2.1 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License
;; for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Note: This library was ported from the Elisp library, so don't wonder much
;;; about elispisms found here...


(in-package :ssip)


;;; Exported variables


(defvar *application-name* "lisp"
  "String defining current application name.")
  
(defvar *client-name* "default"
  "String defining current client name.
This variable's value defines which connection is used when communicating via
SSIP, each connection has its own client name.  Usually, you select the proper
client (connection) by assigning a value to this variable locally through
`let'.")

(defvar *language* nil
  "If non-nil, it is an RFC 1766 language code, as a string.
If text is read and this variable is non-nil, the text is read in the given
language.")

(defvar *spell* nil
  "If non-nil, any spoken text is spelled.")


;;; Internal constants and configuration variables


(defparameter +version+ "$Id: ssip.lisp,v 1.3 2006-02-17 13:18:55 pdm Exp $"
  "Version stamp of the source file.
Useful only for diagnosing problems.")

(defvar *language-codes*
  '(("czech" . "cs")
    ("english" . "en")
    ("american english" . "en-US")
    ("french" . "fr")
    ("german" . "de"))
  "Mapping of LANG values to language ISO codes.")

(defvar *default-voice* "male1")
(defvar *default-language* (or (cdr (assoc (getenv "LANG") *language-codes*
                                           :test #'string=))
                               "en"))

(defparameter +parameter-names+
  '((client-name . "CLIENT_NAME")
    (language . "LANGUAGE")
    (message-priority . "PRIORITY")
    (punctuation-mode . "PUNCTUATION")
    (pause-context . "PAUSE_CONTEXT")
    (capital-character-mode . "CAP_LET_RECOGN")
    (voice . "VOICE")
    (rate . "RATE")
    (pitch . "PITCH")
    (pitch_range . "PITCH_RANGE")
    (spelling-mode . "SPELLING")
    (output-module . "OUTPUT_MODULE")
    ))

(defparameter +list-parameter-names+
  '((voices . "VOICES")))

(defparameter +parameter-value-mappings+
  '((message-priority
     (:important .    "IMPORTANT")
     (:message .      "MESSAGE")
     (:text .         "TEXT")
     (:notification . "NOTIFICATION")
     (:progress .     "PROGRESS")
     )
    (punctuation-mode
     (:none . "none")
     (:some . "some")
     (:most . "most")
     (:all .  "all"))
    (capital-character-mode
     (:none . "none")
     (:spell . "spell")
     (:icon . "icon"))
    (spelling-mode
     (t . "on")
     (nil . "off"))))

(defparameter +volatile-parameters+ '(output-module))

(defparameter +punctuation-modes+ '(("none" . none)
                                    ("some" . some)
                                    ("most" . most)
                                    ("all" .  all)))

(defparameter +capital-character-modes+ '(("none" .  none)
                                          ("spell" . spell)
                                          ("icon" .  icon)))


;;; Internal variables


(defstruct connection
  name
  host
  port
  (failure-p nil)
  stream
  (paused-p nil)
  (in-block nil)
  (transaction-state nil)
  (parameters ())
  (forced-priority nil)
  (last-command nil))

(defstruct request
  string
  (transaction-state '(nil nil)))

(defvar *connections* (make-hash-table :test #'equal)
  "Hash table mapping client names to `connection' instances.")

(defvar *connection* nil
  "Current connection.")



;;; Utilities


(defmacro iterate-clients (&rest body)
  `(maphash #'(lambda (*client-name* _) (declare (ignore _)) ,@body)
    *connections*))

(defmacro iterate-connections (&rest body)
  `(maphash #'(lambda (_ *connection*) (declare (ignore _)) ,@body)
    *connections*))

(defun connection-names ()
  "Return the list of all present connection names."
  (let ((names '()))
    (iterate-clients
      (push *client-name* names))
    names))

(defmacro with-current-connection (&rest body)
  `(let ((*connection* (get-connection)))
     ,@body))

(defmacro with-connection-setting (var value &rest body)
  (let ((accessor (intern (concat "CONNECTION-" (symbol-name var))))
	(orig-value (gensym)))
    `(let ((,orig-value (,accessor *connection*)))
       (setf (,accessor *connection*) ,value)
       (unwind-protect
	   (progn
	     ,@body)
	 (setf (,accessor *connection*) ,orig-value)))))

(defmacro with-connection-parameters (parameters &rest body)
  (let (($parameters (gensym))
        ($orig-parameters (gensym))
        ($cparameters (gensym))
        ($p (gensym))
        ($v (gensym))
        ($orig-v (gensym))
        ($pv (gensym)))
    `(let* ((,$parameters ,parameters)
            (,$orig-parameters ()))
       (unwind-protect
           (progn
             (while ,$parameters
               (let* ((,$p (first ,$parameters))
                      (,$v (second ,$parameters))
                      (,$cparameters
                       (connection-parameters *connection*))
                      (,$orig-v (plist-get ,$cparameters ,$p)))
                 (when (and (not (equal ,$v ,$orig-v))
                            (or ,$v
                                (not (member ,$p '(language)))))
                   (when (plist-member ,$cparameters ,$p)
                     (push (cons ,$p ,$orig-v) ,$orig-parameters))
                   (set-parameter ,$p ,$v)))
               (setq ,$parameters (nthcdr 2 ,$parameters)))
             ,@body)
         (dolist (,$pv ,$orig-parameters)
           (set-parameter (car ,$pv) (cdr ,$pv)))))))


;;; Process management functions


(defun get-connection (&optional (name *client-name*) (create-if-needed t))
  (or (gethash name *connections*)
      (and create-if-needed
	   (let ((*client-name* name))
	     (open-connection)))))

(defun close-connection-stream (connection)
  (let ((stream (connection-stream connection)))
    (when stream
      (ignore-errors (close-network-stream stream)))
    (setf (connection-stream connection) nil)))

(defun open-connection (&optional host port &key quiet force-reopen)
  "Open SSIP connection to given HOST and PORT.
If the connection corresponding to the current `*client-name*' value
already exists, close it and reopen again, with the same connection parameters.

The optional arguments HOST and PORT identify the speechd server location
differing from the values of `speechd-host' and `speechd-port'.

If the key argument QUIET is non-nil, don't report failures and quit silently.
If the key argument FORCE-REOPEN is non-nil, try to reopen an existent
connection even if it previously failed.

Return the opened connection on success, nil otherwise."
  (let ((connection (gethash *client-name* *connections*)))
    (let ((host (or host *host*))
	  (port (or port *port*)))
      (when connection
	(close-connection connection)
	(setq host (connection-host connection)
	      port (connection-port connection)))
      (let* ((name *client-name*)
             (default-parameters (append
                                  (cdr (assoc *client-name*
                                              *connection-parameters*
                                              :test #'string=))
                                  (cdr (assoc t *connection-parameters*))))
	     (parameters (if connection
                             (append
                              (connection-parameters connection)
                              default-parameters)
                             default-parameters))
	     (stream (when (or (not connection)
                               (not (connection-failure-p connection))
                               force-reopen)
                       (ignore-errors
                         (open-network-stream host port)))))
	(when (and (not stream) (not quiet))
          (error "Connection to SSIP failed"))
	(setq connection (make-connection
			  :name name :host host :port port
			  :stream stream :failure-p (not stream)))
	(setf (gethash name *connections*) connection)
	(when stream
	  (set-connection-name name)
          (setq parameters (append parameters
                                   (list 'language *default-language*
                                         'voice *default-voice*)))
          (let ((already-set '(client-name)))
            (while parameters
              (destructuring-bind (parameter value . next) parameters
                (unless (member parameter already-set)
                  (push parameter already-set)
                  (set-parameter parameter value))
                (setq parameters next)))))
        (let ((priority (and
                         connection
                         (plist-get default-parameters 'message-priority))))
          (when priority
            (set-parameter 'message-priority priority)
            (setf (connection-forced-priority connection) t)))))
    connection))

(defun close-connection (&optional (name *client-name*))
  "Close speechd connection named NAME."
  (let ((connection (get-connection name nil)))
    (when connection
      (close-connection-stream connection)
      (remhash name *connections*))))

(defun reopen-connection ()
  "Close and open again all the connections to speechd."
  (iterate-clients (open-connection :quiet t :force-reopen t)))

(defun running-p ()
  "Return non-nil, if the current speechd client name process is running."
  (let ((connection (get-connection)))
    (and connection (connection-stream connection))))


;;; Process communication functions


(defun permanent-connection-failure (connection)
  (close-connection-stream connection)
  (setf (connection-failure-p connection) t
	(connection-paused-p connection) nil
	(connection-transaction-state connection) nil
	(connection-parameters connection) ()))

(defun send-string (string)
  (with-current-connection
    (let ((stream (connection-stream *connection*)))
      (when stream
	(unwind-protect
             (format stream "~A" string)
	  (when (not (running-p))
            (permanent-connection-failure *connection*)))))))

(defun process-request (request)
  (with-current-connection
    ;; Ensure proper transaction state
    (let* ((state-spec (request-transaction-state request))
           (required-state (first state-spec))
           (new-state (second state-spec)))
      (labels ((check-state (reopen-if-needed)
                 (let ((current-state (connection-transaction-state
                                       *connection*)))
                   (when (and (not (eq current-state required-state))
                              (not (eq current-state new-state)))
                     (cond
                       ((and (eq required-state 'in-data)
                             (not (eq new-state nil)))
                        (send-data-begin))
                       ((eq required-state nil)
                        (send-data-end))))
                   (setq current-state (connection-transaction-state
                                        *connection*))
                   (if (and reopen-if-needed
                            (not (eq current-state required-state))
                            (not (eq current-state new-state))
                            (not (connection-failure-p *connection*)))
                       (progn
                         (open-connection)
                         (setq *connection* (get-connection))
                         (check-state nil))
                       (eq current-state required-state)))))
        ;; Continue only if the state can be set properly after reopen,
        ;; otherwise give up and ignore the request completely.
        ;; This also works for the "." command when in non-data state.
        (when (check-state t)
          (send-string (request-string request))
          ;; Read command answer
          (unless (equal state-spec '(in-data in-data))
            (destructuring-bind (answer-line . data-lines)
                (loop with stream = (connection-stream *connection*)
                      for line = (read-line stream)
                      for lines = (list line) then (cons line lines)
                      while (and (> (length line) 3)
                                 (char= (char line 3) #\-))
                      finally (return lines))
              (let* ((code (subseq answer-line 0 3))
                     (answer (subseq answer-line 4))
                     (success (member (char code 0) '(#\1 #\2)))
                     (data (and success
                                (mapcar #'(lambda (line) (subseq line 4))
                                        data-lines))))
                (when success
                  (setf (connection-transaction-state *connection*) new-state))
                (list success data code answer)))))))))

(defun send-request (request)
  (with-current-connection
    (process-request request)))

(defparameter +block-commands+
  '(("speak")
    ("sound_icon")
    ("char")
    ("key")
    ("quit")
    ("block" ("end"))
    ("set" ("self" ("rate" "pitch" "pitch_range" "voice" "language")))))

(defun block-command-p (command &optional allowed)
  (unless allowed
    (setq allowed +block-commands+))
  (let* ((match (assoc (first command) allowed :test #'string-equal))
         (rest-allowed (cdr match)))
    (and match
         (or (not rest-allowed)
             (block-command-p (rest command) rest-allowed)))))

(defun send-command (command &optional (transaction-state '(nil nil)))
  (unless (listp command)
    (setq command (list command)))
  (with-current-connection
    (setf (connection-last-command *connection*) command)
    (when (or (not (connection-in-block *connection*))
              (block-command-p command))
      (send-request
        (make-request
         :string (format nil "~{~A~^ ~}~A~A" command #\Return #\Linefeed)
         :transaction-state transaction-state)))))

(defun send-data-begin ()
  (send-command "SPEAK" '(nil in-data)))

(defun send-data (text)
  (let ((text* text))
    (flet ((send (string)
             (unless (string= string "")
               (send-request (make-request
                              :string string
                              :transaction-state '(in-data in-data))))))
      (loop with eol = (format nil "~A~A" #\Return #\Linefeed)
            for length = (length text*)
            for nlpos = (or (position #\Linefeed text*) length)
            for dotted = (and (> (length text*) 0)
                              (char= (char text* 0) #\.))
            until (string= text* "")
            do (progn
                 (when dotted
                   (send "."))
                 (send (subseq text* 0 nlpos))
                 (send eol)
                 (setq text* (subseq text* (min (1+ nlpos) length))))))))

(defun send-data-end ()
  (send-command "." '(in-data nil)))


;;; Value retrieval functions


(defun list-values (parameter)
  (second (send-command
	   (list "LIST" (cdr (assoc parameter +list-parameter-names+))))))


;;; Parameter setting functions


(defun convert-numeric (number)
  (cond ((< number -100) -100)
	((> number 100) 100)
	(t number)))

(defun transform-parameter-value (parameter value)
  (cond
   ((stringp value)
    value)
   ((integerp value)
    (format nil "~D" (convert-numeric value)))
   ((symbolp value)
    (cdr (assoc value
		(cdr (assoc parameter +parameter-value-mappings+)))))))

(defun set-parameter (parameter value)
  (with-current-connection
    (let* ((plist (connection-parameters *connection*))
	   (orig-value (if (plist-member plist parameter)
			   (plist-get plist parameter)
			 'unknown)))
      (when (or (member parameter +volatile-parameters+)
                (and (not (equal orig-value value))
                     (or (not (eq parameter 'message-priority))
                         (not (connection-forced-priority *connection*)))))
	(let ((answer
	       (send-command
                (let ((p (cdr (assoc parameter +parameter-names+)))
                      (v (transform-parameter-value parameter value)))
                  (unless p
                    (error "Invalid parameter name: `~A'" parameter))
                  (unless v
                    (error "Invalid parameter value: ~A=~A" parameter value))
                  (list "SET" "self" p v)))))
	  (setq *connection* (get-connection))
          (when (first answer)
            (setf (connection-parameters *connection*)
                  (plist-put (connection-parameters *connection*)
                             parameter value))))))))

(defun set-connection-name (name)
  (set-parameter
   'client-name
   (format nil "~A:~A:~A" (user-login-name) *application-name* name)))

(defun set-language (language)
  "Set language of the current client connection to LANGUAGE.
Language must be an RFC 1766 language code, as a string."
  (set-parameter 'language language)
  (setq *language* language))


;;; Blocks


(defmacro with-block (parameters &rest body)
  "Set PARAMETERS and enclose BODY by an SSIP block.
Before invoking BODY, the BLOCK BEGIN command is sent, and the BLOCK END
command is sent afterwards.
PARAMETERS is a property list defining parameters to be set before sending the
BLOCK BEGIN command.  The property-value pairs correspond to the arguments of
the `set-parameter' function."
  `(with-current-connection
     (with-connection-parameters ,parameters
       (if (and *connection* (connection-in-block *connection*))
           (progn ,@body)
         (let ((block-connection *connection*))
           (send-command '("BLOCK BEGIN"))
           (unwind-protect
               (progn
                 (with-current-connection
                  (when *connection*
                    (setf (connection-in-block *connection*) t)))
                 ,@body)
             (let ((*connection* block-connection))
               (when *connection*
                 (setf (connection-in-block *connection*) nil)
                 (let ((*client-name*
                        (connection-name *connection*)))
                   (send-command '("BLOCK END")))))))))))


;;; Speaking functions


(defun say-text (text &key (priority *default-text-priority*))
  "Speak the given TEXT, represented by a string.
The key argument `priority' defines the priority of the message and must be one
of the symbols `important', `message', `text', `notification' or
`progress'."
  (set-parameter 'message-priority priority)
  (unless (string= text "")
    (send-data-begin)
    (send-data text)
    (send-data-end)))

(defun say-sound (name &key (priority *default-sound-priority*))
  "Play an auditory icon.
NAME is the name of the icon, any string acceptable by speechd.
The key argument `priority' defines the priority of the message and must be one
of the symbols `important', `message', `text', `notification' or
`progress'."
  (set-parameter 'message-priority priority)
  (send-command (list "SOUND_ICON" name)))

(defun say-char (char &key (priority *default-char-priority*))
  "Speak the given CHAR, any UTF-8 character.
The key argument `priority' defines the priority of the message and must be one
of the symbols `important', `message', `text', `notification' or
`progress'."
  (set-parameter 'message-priority priority)
  (with-current-connection
    (with-connection-parameters `(language ,*language*)
      (send-command
       (list "CHAR" (format nil "~A" (case char
                                       (?  "space")
                                       (?\n "linefeed")
                                       (t (format nil "~A" char)))))))))


;;; Control functions


(defun control-command (command all &optional repeatable)
  (cond
   ((not all)
    (when (or repeatable
              (not (equal (first (connection-last-command (get-connection)))
                          command)))
      (send-command (list command "self"))))
   ((numberp all)
    (iterate-clients (control-command command nil)))
   (t
    (send-command (list command "all")))))

(defun cancel (&optional all)
  "Stop speaking all the messages sent through the current client so far.
If the universal argument is given, stop speaking messages of all clients.
If a numeric argument is given, stop speaking messages of all current Emacs
session clients."
  (control-command "CANCEL" all))

(defun stop (&optional all)
  "Stop speaking the currently spoken message (if any) of this client.
If the optional argument ALL is non-nil, stop speaking the currently spoken
messages of all clients."
  (control-command "STOP" all t))

(defun pause (&optional all)
  "Pause speaking in the current client.
If the optional argument ALL is non-nil, pause speaking in all clients."
  (if all
      (iterate-connections
        (setf (connection-paused-p *connection*) t))
    (setf (connection-paused-p (get-connection)) t))
  (control-command "PAUSE" (not (not all))))

(defun resume (&optional all)
  "Resume previously stopped speaking in the current client.
If the optional argument ALL is non-nil, resume speaking messages of all
clients."
  (when (or all (connection-paused-p (get-connection)))
    (control-command "RESUME" (not (not all)))
    (if all
        (setf (connection-paused-p (get-connection)) nil)
      (iterate-connections
        (setf (connection-paused-p *connection*) nil)))))
