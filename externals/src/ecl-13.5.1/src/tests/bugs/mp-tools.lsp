;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 CEST 2012
;;;; Contains: Supporting routines for multithreaded tests

(in-package :cl-test)

(defun kill-and-wait (process-list &optional original wait)
  "Kills a list of processes, which may be the difference between two lists,
waiting for all processes to finish. Currently it has no timeout, meaning
it may block hard the lisp image."
  (let ((process-list (set-difference process-list original)))
    (when (member mp:*current-process* process-list)
      (error "Found myself in the kill list"))
    (mapc #'mp:process-kill process-list)
    (when wait
      (loop for i in process-list
	 do (mp:process-join i)))
    process-list))

(defun mp-test-run (closure)
  (let* ((all-processes (mp:all-processes))
	 (output (multiple-value-list (funcall closure))))
    (sleep 0.2) ; time to exit some processes
    (let ((leftovers (kill-and-wait (mp:all-processes) all-processes)))
      (cond (leftovers
	     (format t "~%;;; Stray processes: ~A" leftovers))
	    (t
	     (values-list output))))))

(defmacro def-mp-test (name body expected-value)
  "Runs some test code and only returns the output when the code exited without
creating stray processes."
  (let ((all-processes (gensym))
	(output (gensym))
	(leftover (gensym)))
    `(deftest ,name
	 (mp-test-run #'(lambda () ,body))
       ,expected-value)))
