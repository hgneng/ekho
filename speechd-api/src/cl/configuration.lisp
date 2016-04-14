;;; Configuration variables

;; Author: Milan Zamazal <pdm@brailcom.org>

;; Copyright (C) 2004 Brailcom, o.p.s.

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA.


(in-package :ssip)


(defvar *host* "localhost"
  "Name of the default host running speechd to connect to.")

(defvar *port* (or (ignore-errors
                     (car (read-from-string (getenv "SPEECHD_PORT"))))
                   6560)
  "Default port of speechd.")


(defvar *default-text-priority* :text
  "Default Speech Dispatcher priority of sent texts.")

(defvar *default-sound-priority* :message
  "Default Speech Dispatcher priority of sent sound icons.")

(defvar *default-char-priority* :notification
  "Default Speech Dispatcher priority of sent single letters.")


(defvar *connection-parameters* '()
  "Alist of connection names and their parameters.

Each element of the list is of the form (CONNECTION-NAME . PARAMETERS), where
CONNECTION-NAME is a connection name as expected to be in `speechd-client-name'
and PARAMETERS is a property list with the pairs of parameter identifiers and
parameter values.  Valid parameter names are the following symbols:
language, message-priority, punctuation-mode, capital-character-mode, voice,
rate, pitch, output-module.  See the corresponding speechd-set-* functions for
valid parameter values.

If the symbol t is specified as the connection name, the element defines
default connection parameters if no connection specification applies.  Only one
such an element is allowed in the whole alist.

The message-priority parameter has a special meaning: It overrides priority of
all messages sent through the connection.

You must reopen the connections to apply the changes to this variable.")
