;;; -*- Package: XLIB -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /loaclhost/usr/local/src/cvs/clx/cmudep.lisp,v 1.1 2000/07/02 19:19:46 dan Exp $")
;;;
;;; **********************************************************************
;;;
(in-package "XLIB")

(alien:def-alien-routine ("connect_to_server" xlib::connect-to-server)
			 c-call:int
  (host c-call:c-string)
  (port c-call:int))
