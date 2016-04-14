;;; System dependent functions

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


(defun getenv (var)
  #+SBCL
  (sb-ext:posix-getenv var)
  #+CLISP
  (ext:getenv var))


#+CLISP
(defparameter +encoding+
  (ext:make-encoding :charset 'charset:utf-8 :line-terminator :unix))

(defun open-network-stream (host port)
  #+CLISP
  (socket:socket-connect port host :external-format +encoding+)
  #+SBCL
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream
                          :protocol :tcp)))
    (sb-bsd-sockets:socket-connect
     s
     (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host))
     port)
    (sb-bsd-sockets:socket-make-stream s :input t :output t :buffering :none))
  )

(defun close-network-stream (stream)
  #+(or SBCL CLISP)
  (close stream))

