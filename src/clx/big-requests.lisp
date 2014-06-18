;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB; -*-
;;;
;;; (c) copyright 2006 Richard Kreuter
;;; (c) copyright 2007 by Christophe Rhodes
;;;
;;; Permission is granted to any individual or institution to use,
;;; copy, modify, and distribute this software, provided that this
;;; complete copyright and permission notice is maintained, intact, in
;;; all copies and supporting documentation.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(in-package "XLIB")

;;; No new events or errors are defined by this extension.  (Big
;;; Requests Extension, section 3)
;;;
;;; The name of this extension is "BIG-REQUESTS" (Big Requests
;;; Extension, section 4)
(define-extension "BIG-REQUESTS")

(defun enable-big-requests (display)
  (declare (type display display))
  (let ((opcode (extension-opcode display "BIG-REQUESTS")))
    (with-buffer-request-and-reply (display opcode nil)
	((data 0))
      (let ((maximum-request-length (card32-get 8)))
	(setf (display-extended-max-request-length display) 
	      maximum-request-length)))))
