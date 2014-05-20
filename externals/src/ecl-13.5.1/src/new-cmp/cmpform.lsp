;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;; CMPFORM -- Internal representation of Lisp forms
;;;;

(in-package "C-DATA")

(defmacro make-c1form* (&rest args)
  `(list (make-c1form-alone ,@args)))

#+(or)
(defmacro make-c1form-alone (name &rest args)
  (let ((info-args '())
	(form-args '()))
    (do ((l args (cdr l)))
	((endp l))
      (let ((key (first l)))
	(cond ((not (keywordp key))
	       (baboon))
	      ((eq key ':args)
	       (setf form-args (rest l))
	       (return))
	      (t
	       (setf info-args (list* key (second l) info-args)
		     l (cdr l))))))
    `(do-make-c1form :name ,name :args (list ,@form-args)
                     :form *current-form*
                     :file *compile-file-truename*
                     :file-position *compile-file-position*
                     ,@info-args)))

(defun make-c1form-alone (name &rest args)
  (let ((info-args '())
	(form-args '()))
    (do ((l args (cdr l)))
	((endp l))
      (let ((key (first l)))
	(cond ((not (keywordp key))
	       (baboon))
	      ((eq key ':args)
	       (setf form-args (rest l))
	       (return))
	      (t
	       (setf info-args (list* key (second l) info-args)
		     l (cdr l))))))
    (apply #'do-make-c1form :name name :args form-args
           :form *current-form*
           :file *compile-file-truename*
           :file-position *compile-file-position*
           info-args)))

(defun copy-c1form (form)
  (copy-structure form))

(defmacro c1form-arg (nth form)
  (case nth
    (0 `(first (c1form-args ,form)))
    (1 `(second (c1form-args ,form)))
    (otherwise `(nth ,nth (c1form-args ,form)))))

(defun c1form-volatile* (form)
  (if (c1form-volatile form) "volatile " ""))

(defun c1form-set-volatile (flag forms)
  (loop for i in forms
     do (setf (c1form-volatile i) flag))
  forms)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OUTPUT C1FORMS
;;;

(defun pprint-c1form (f &optional (stream t))
  (cond ((c1form-p f)
         (format stream "~&~4T~16A~4T~{~A ~}" (c1form-name f) (c1form-args f)))
        ((tag-p f)
         (format stream "~&~A / ~A:" (tag-name f) (tag-label f)))
        (t
         (format stream "~&;;; Unknown form ~A" f)))
  (force-output stream)
  f)

(defun pprint-c1forms (forms &optional (stream t))
  (loop for f in forms do (pprint-c1form f stream)))

(defun print-c1form (form stream)
  (format stream "#<form ~A ~X>" (c1form-name form) (ext::pointer form)))
