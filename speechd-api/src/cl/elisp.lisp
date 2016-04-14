;;; Elisp compatibility functions

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


(defmacro while (condition &body body)
  `(loop while ,condition
         do (progn ,@body)))


(defun plist-get-internal (plist prop)
  (cond
    ((null plist)
     nil)
    ((eq (car plist) prop)
     (cdr plist))
    (t
     (plist-get-internal (nthcdr 2 plist) prop))))
  
(defun plist-get (plist prop)
  (first (plist-get-internal plist prop)))

(defun plist-member (plist prop)
  (not (null (plist-get-internal plist prop))))

(defun plist-put (plist prop val)
  (let ((value (plist-get-internal plist prop)))
    (if value
        (progn
          (rplaca value val)
          plist)
        (list* prop val plist))))


(defun user-login-name ()
  (or (getenv "LOGNAME") (getenv "USER")))


(defun concat (&rest args)
  (apply #'concatenate 'string args))
