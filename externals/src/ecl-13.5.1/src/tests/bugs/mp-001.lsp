;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Multithreading API regression tests

(in-package :cl-test)

;;; Date: 04/09/2009
;;; From: Matthew Mondor
;;; Fixed: 05/09/2009 (Juanjo)
;;; Description:
;;;
;;;	When a WITH-LOCK is interrupted, it is not able to release
;;;	the resulting lock and an error is signaled.
;;;

(def-mp-test mp-0001-with-lock
    (let ((flag t)
	  (lock (mp:make-lock :name "mp-0001-with-lock" :recursive nil)))
      (mp:with-lock (lock)
        (let ((background-process
               (mp:process-run-function
                "mp-0001-with-lock"
                #'(lambda ()
		    (handler-case
			(progn
			  (setf flag 1)
			  (mp:with-lock (lock)
			    (setf flag 2)))
		      (error (c)
			(princ c)(terpri)
			(setf flag c)))
		    (setf flag 2)))))
          ;; The background process should not be able to get
          ;; the lock, and will simply wait. Now we interrupt it
          ;; and the process should gracefully quit, without
          ;; signalling any serious condition
          (and (progn (sleep 1)
		      (mp:process-kill background-process))
               (progn (sleep 1)
                      (not (mp:process-active-p background-process)))
               (eq flag 1)
	       t))))
  t)