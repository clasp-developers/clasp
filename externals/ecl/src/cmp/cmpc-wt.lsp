;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;; CMPC-WT -- Routines for writing code to C files.
;;;;

(in-package #-new-cmp "COMPILER" #+new-cmp "C-BACKEND")

(defun wt1 (form)
  (typecase form
    ((or STRING INTEGER CHARACTER)
     (princ form *compiler-output1*))
    (SINGLE-FLOAT
     (format *compiler-output1* "(float)~10,,,,,,'eG" form))
    (DOUBLE-FLOAT
     (format *compiler-output1* "~10,,,,,,'eG" form))
    (LONG-FLOAT
     (format *compiler-output1* "~,,,,,,'eEl" form))
    (VAR (wt-var form))
    (t (wt-loc form)))
  nil)

(defun wt-h1 (form)
  (let ((*compiler-output1* *compiler-output2*))
    (wt1 form)))

(defun wt (&rest forms)
  (mapc #'wt1 forms))

(defun wt-h (&rest forms)
  (mapc #'wt-h1 forms))

(defun wt-nl-h (&rest forms)
  (terpri *compiler-output2*)
  (mapc #'wt-h1 forms))

(defun princ-h (form)
  (princ form *compiler-output2*))

(defun wt-nl (&rest forms)
  (wt-nl-indent)
  (mapc #'wt1 forms))

(defun wt-nl1 (&rest forms)
  (wt1 #\Newline)
  (mapc #'wt1 forms))

;;; Blocks beyond this value will not be indented
(defvar +max-depth+ 10)
(defvar +c-newline-indent-strings+
  #.(coerce (let ((basis (make-array (1+ +max-depth+)
				     :initial-element #\Space
				     :element-type 'base-char)))
	      (setf (aref basis 0) #\Newline)
	      (loop for i from 0 to +max-depth+
		 collect (subseq basis 0 (1+ i))))
	    'vector))

(defun wt-nl-indent ()
  (wt1 (aref +c-newline-indent-strings+ (min *opened-c-braces* +max-depth+))))

(defun wt-open-brace ()
  (wt1 #\{)
  (incf *opened-c-braces*))

(defun wt-nl-open-brace ()
  (wt-nl-indent)
  (wt-open-brace))

(defun wt-nl-close-many-braces (final-value)
  (let ((diff (- *opened-c-braces* final-value)))
    (when (minusp diff)
      (baboon :format-control "Mismatch in C blocks"))
    (loop for i from 0 below diff
       do (wt-nl-close-brace))))

(defun wt-nl-close-brace ()
  (if (plusp *opened-c-braces*)
      (progn
	(decf *opened-c-braces*)
	(wt-nl-indent)
	(wt1 #\}))
      (baboon :format-control "Mismatch in C blocks")))

(defmacro with-indentation (&body body)
  `(let ((*opened-c-braces* (1+ *opened-c-braces*)))
     ,@body))

;;;
;;; LABELS AND JUMPS
;;;

(defun wt-go (label)
  #-new-cmp
  (setf (cdr label) t
        label (car label))
  (wt "goto L" label ";"))

(defun wt-label (label)
  #-new-cmp
  (when (cdr label) (wt-nl1 "L" (car label) ":;"))
  #+new-cmp
  (wt-nl1 "L" label ":;"))

;;;
;;; C/C++ COMMENTS
;;;

(defun wt-filtered-comment (text stream single-line)
  (declare (string text))
  (if single-line
      (progn
	(fresh-line stream)
	(princ "/*	" stream))
      (format stream "~50T/*  "))
  (let* ((l (1- (length text))))
    (declare (fixnum l))
    (dotimes (n l)
      (let* ((c (schar text n))
             (code (char-code c)))
        (cond
          ((or (eq c #\Newline) (eq c #\Tab))
           (princ c stream))
	  ((or (< code 32) (> code 127))
           (format stream "\ux" code))
          ((and (char= c #\*) (char= (schar text (1+ n)) #\/))
           (princ #\\ stream))
          (t
           (princ c stream)))))
    (princ (schar text l) stream))
  (format stream "~70T*/")
  )

(defun do-wt-comment (message-or-format args single-line-p)
  (unless (and (symbolp message-or-format) (not (symbol-package message-or-format)))
    (wt-filtered-comment (if (stringp message-or-format)
                             (if args
                                 (apply #'format nil message-or-format args)
                                 message-or-format)
                             (princ-to-string message-or-format))
                         *compiler-output1*
                         single-line-p)))

(defun wt-comment (message &rest extra)
  (do-wt-comment message extra nil))

(defun wt-comment-nl (message &rest extra)
  (do-wt-comment message extra t))

;;;
;;; STRINGS
;;;
;;; This routine converts lisp data into C-strings. We have to take
;;; care of escaping special characteres with backslashes. We also have
;;; to split long lines using  the fact that multiple strings are joined
;;; together by the compiler.
;;;

(defvar *wt-string-size* 0)

#+unicode
(defun encode-string (string format)
  (let* ((output (make-array (round (* 1.2 (length string)))
			     :element-type 'base-char
			     :adjustable t
			     :fill-pointer 0))
	 (stream (make-sequence-output-stream output :external-format format)))
    (write-string string stream)
    output))

(defun wt-filtered-data (string stream &key one-liner
			 (external-format #-unicode :default #+unicode :utf-8))
  #+unicode
  (setf string (encode-string string external-format))
  (let ((N (length string))
	(wt-data-column 80))
    (incf *wt-string-size* N) ; 1+ accounts for a blank space
    (format stream (if one-liner "\"" "~%\""))
    (dotimes (i N)
      (decf wt-data-column)
      (when (< wt-data-column 0)
	(format stream "\"~% \"")
	(setq wt-data-column 79))
      (let ((x (aref string i)))
	(cond
	  ((or (< (char-code x) 32)
	       (> (char-code x) 127))
	   (case x
	     ; We avoid a trailing backslash+newline because some preprocessors
	     ; remove them.
	     (#\Newline (princ "\\n" stream))
	     (#\Tab (princ "\\t" stream))
	     (t (format stream "\\~3,'0o" (char-code x)))))
	  ((char= x #\\)
	   (princ "\\\\" stream))
	  ((char= x #\")
	   (princ "\\\"" stream))
	  (t (princ x stream)))))
    (princ "\"" stream)
    string))

(defun c-filtered-string (string &rest args)
  (with-output-to-string (aux-stream)
    (apply #'wt-filtered-data string aux-stream :one-liner t args)))
