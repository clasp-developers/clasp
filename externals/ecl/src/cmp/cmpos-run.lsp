;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPOS-RUN  Executing auxiliary programs

(in-package "COMPILER")

#+(and cygwin (not ecl-min))
(ffi:clines "#include <stdlib.h>")

(defun safe-system (string)
  (cmpnote "Invoking external command:~%  ~A~%" string)
  (let ((result (ext:system string)))
    (unless (zerop result)
      (cerror "Continues anyway."
	      "(SYSTEM ~S) returned non-zero value ~D"
	      string result))
    result))

(defun save-directory (forms)
  (let ((directory
         (probe-file (make-pathname :name nil :type nil
                                    :defaults *default-pathname-defaults*))))
    (if directory
        (let ((*default-pathname-defaults* directory)
              (old-directory (ext:chdir directory)))
          (unwind-protect
               (funcall forms)
            (ext:chdir old-directory)))
        (funcall forms))))    

(defmacro with-current-directory (&body forms)
  `(save-directory #'(lambda () ,@forms)))

#+(and cygwin (not ecl-min))
(defun old-crappy-system (program args)
  (let* ((command (format nil "~S~{ ~S~}" program args))
         (base-string-command (si:copy-to-simple-base-string command))
	 (code (ffi:c-inline (base-string-command) (:object) :int
                  "system((const char*)(#0->base_string.self))":one-liner t)))
    (values nil code nil)))

(defun safe-run-program (program args)
  (cmpnote "Invoking external command:~%  ~A ~{~A ~}" program args)
  (multiple-value-bind (stream result process)
      (let* ((*standard-output* ext:+process-standard-output+)
             (*error-output* ext:+process-error-output+))
        (with-current-directory
	    #-(and cygwin (not ecl-min))
            (ext:run-program program args :input nil :output t :error t :wait t)
	    #+(and cygwin (not ecl-min))
            (old-crappy-system program args)
            ))
    (cond ((null result)
           (cerror "Continues anyway."
                   "Unable to execute:~%(RUN-PROGRAM ~S ~S)"
                   program args result))
          ((not (zerop result))
           (cerror "Continues anyway."
                   "Error code ~D when executing~%(RUN-PROGRAM ~S ~S)"
                   result program args)))
    result))

(defun split-program-options (string)
  (labels ((maybe-push (options current)
             (if current
                 (cons current options)
                 options))
           (new-string ()
             (make-array 32 :element-type 'base-char :adjustable t
                         :fill-pointer 0))
           (push-char (c s)
             (unless s (setf s (new-string)))
             (vector-push-extend c s)
             s))
    (loop with output = '()
       with option = nil
       with status = nil
       for i from 0 below (length string)
       for c = (char string i)
       for now = (first status)
       do (cond ((eq now #\')
                 (if (eq c #\')
                     (setf status (rest status))
                     (setf option (push-char c option))))
                ((eq now #\\)
                 (setf option (push-char c option)
                       status (rest status)))
                ((eq now #\")
                 (if (eq c #\")
                     (setf status (rest status))
                     (setf option (push-char c option))))
                ((member c '(#\\ #\' #\"))
                 (push c status))
                ((member c '(#\Space #\Tab #\Return #\Newline))
                 (setf output (maybe-push output option)
                       option nil))
                (t
                 (setf option (push-char c option))))
       finally (cond (status
                      (error "In split-program-options, unterminated option list:~%~S"
                             string))
                     (t
                      (return (nreverse (maybe-push output option))))))))
