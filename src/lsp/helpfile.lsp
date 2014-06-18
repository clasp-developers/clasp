;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2001, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;

(in-package "SYSTEM")

;;;;----------------------------------------------------------------------
;;;; Documentation system
;;;;

#+ecl-min
(progn
  (*make-special '*documentation-pool*)
  (setq *documentation-pool* nil)
  (*make-special '*keep-documentation*)
  (setq *keep-documentation* t))
#-ecl-min
(progn
  (setq *documentation-pool* (list (make-hash-table :test #'equal :size 128)
                                   "SYS:help.doc"))
  (defparameter *keep-documentation* t))

(defun new-documentation-pool (&optional (size 1024))
  "Args: (&optional hash-size)
Sets up a new hash table for storing documentation strings."
  (push (make-hash-table :test #'eql :size size)
	*documentation-pool*))

(defun record-cons (record key sub-key)
  (let ((cons (cons key sub-key)))
    (dolist (i record i)
      (when (equalp (car i) cons)
        (return i)))))

(defun record-field (record key sub-key)
  (cdr (record-cons record key sub-key)))

(defun set-record-field (record key sub-key value)
  (let ((field (record-cons record key sub-key)))
    (if field
        (rplacd field value)
        (setq record (list* (cons (cons key sub-key) value) record)))
    record))

(defun rem-record-field (record key sub-key)
  (let ((x (record-cons record key sub-key)))
    (if x
        (let ((output '()))
          (dolist (i record output)
            (when (not (eq i x))
              (setq output (cons i output)))))
        record)))

(defun annotate (object key sub-key value)
  (let ((dict (first *documentation-pool*)))
    (when (hash-table-p dict)
      (let ((record (set-record-field (gethash object dict)
                                      key sub-key value)))
        (si::hash-set object dict record)))))

(defun remove-annotation (object key sub-key)
  (let ((dict (first *documentation-pool*)))
    (when (hash-table-p dict)
      (let ((record (rem-record-field (gethash object dict)
                                      key sub-key)))
	(if record
            (si::hash-set object dict record)
            (remhash object dict))))))

(defun get-annotation (object key &optional (sub-key :all))
  (let ((output '()))
    (dolist (dict *documentation-pool* output)
      (let ((record (if (hash-table-p dict)
                        (gethash object dict)
                        (if (stringp dict)
                            (search-help-file object dict)
                            nil))))
        (when record
          (if (eq sub-key :all)
              (dolist (i record)
                (let ((key-sub-key (car i)))
                  (when (equal (car key-sub-key) key)
                    (push (cons (cdr key-sub-key) (cdr i)) output))))
              (if (setq output (record-field record key sub-key))
                  (return output))))))))

(defun dump-documentation (file &optional (merge nil))
  "Args: (filespec &optional (merge nil))
Saves the current hash table for documentation strings to the specificed file.
If MERGE is true, merges the contents of this table with the original values in
the help file."
  (let ((dict (first *documentation-pool*)))
    (when (hash-table-p dict)
      (dump-help-file dict file merge)
      (rplaca *documentation-pool* file))))

(defun get-documentation (object doc-type)
  (when (functionp object)
    (when (null (setq object (compiled-function-name object)))
      (return-from get-documentation nil)))
  (if (and object (listp object) (si::valid-function-name-p object))
      (get-annotation (second object) 'setf-documentation doc-type)
      (get-annotation object 'documentation doc-type)))

(defun set-documentation (object doc-type string)
  (when (not (or (stringp string) (null string)))
    (error "~S is not a valid documentation string" string))
  (let ((key 'documentation))
    (when (and object (listp object) (si::valid-function-name-p object))
      (setq object (second object) key 'setf-documentation))
    (if string
        (annotate object key doc-type string)
        (remove-annotation object key doc-type)))
  string)

(defun expand-set-documentation (symbol doc-type string)
  (when (and *keep-documentation* string)
    (when (not (stringp string))
      (error "~S is not a valid documentation string" string))
    `((set-documentation ',symbol ',doc-type ,string))))

#-clos
(defun documentation (object type)
  "Args: (symbol doc-type)
Returns the DOC-TYPE doc-string of SYMBOL; NIL if none exists.  Possible doc-
types are:
	FUNCTION  (special forms, macros, and functions)
	VARIABLE  (global variables)
	TYPE      (type specifiers)
	STRUCTURE (structures)
	SETF      (SETF methods)
All built-in special forms, macros, functions, and variables have their doc-
strings."
  (cond ((member type '(function type variable setf structure))
	 (when (not (symbolp object))
	   (error "~S is not a symbol." object))
	 (si::get-documentation object type))
	(t
	 (error "~S is an unknown documentation type" type))))

(defun make-dspec (definition)
  (when (consp definition)
    (let* ((kind (first definition))
           (name (second definition))
           (extra '()))
      (when (eq kind 'defmethod)
        (let ((list (third definition)))
          (setq extra (if (symbolp list)
                          (cons list (fourth definition))
                          list))))
      (list* kind name extra))))

;; (EXT:OPTIONAL-ANNOTATION arguments for EXT:ANNOTATE)
(si::fset 'ext:optional-annotation
          #'(ext:lambda-block ext:optional-annotation (whole env)
               (declare (ignore env #-ecl-min whole))
               #+ecl-min
               `(ext:annotate ,@(rest whole)))
          t)

(defun default-annotation-logic (source-location definition output-form
                                 &optional (dspec (make-dspec definition)))
  (let* ((kind (first definition))
         (name (second definition)))
    `(progn
       (ext:optional-annotation ',name 'location ',dspec ',source-location)
       ,(when (member kind '(defun defmacro defgeneric))
          `(ext:optional-annotation ',name :lambda-list nil ',(third definition)))
       ,output-form)))

#+ecl-min
(when (null *documentation-pool*) (new-documentation-pool 1024))
#+ecl-min
(setq ext::*register-with-pde-hook* 'default-annotation-logic)


