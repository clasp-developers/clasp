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
;;;;  Help files
;;;;

#+(or)
(defun read-help-file (path)
  (let* ((*package* (find-package "CL"))
         (file (open path :direction :input)))
    (do ((end nil)
         (h (make-hash-table :size 1024 :test #'eql)))
        (end h)
      (do ((c (read-char file nil)))
          ((or (not c) (eq c #\^_))
           (when (not c) (setq end t)))
        )
      (when (not end)
        (let* ((key (read file))
               (value (read file)))
          #+ecl(si::hash-set core::hash-table-setf-gethash key h value)
          #+clasp(core::hash-table-setf-gethash h key value)
	  )))))

#+(or)(defun dump-help-file (hash-table path &optional (merge nil))
         (let ((entries nil))
           (when merge
             (let ((old-hash (read-help-file path)))
               (push old-hash *documentation-pool*)
               (maphash #'(lambda (key doc)
                            (when doc
                              (do* ((list doc)
                                    (doc-type (first list))
                                    (string (second list)))
                                   (list)
                                (set-documentation key doc-type string))))
                        hash-table)
               (setq hash-table (pop *documentation-pool*))))
           (maphash #'(lambda (key doc)
                        (when (and (symbolp key) doc)
                          (push (cons key doc) entries)))
                    hash-table)
           (setq entries (sort entries #'string-lessp :key #'car))
           (let* ((*package* (find-package "CL"))
                  (file (open path :direction :output)))
             (dolist (l entries)
               (format file "~A~S~%~S~%" #\^_ (car l) (rest l)))
             (close file)
             path)))

#+clasp (defun search-help-file (key path) nil) ;; we don't have help-files in clasp yet
#+(or)
(defun search-help-file (key path &aux (pos 0))
  (labels ((bin-search (file start end &aux (delta 0) (middle 0) sym)
             (declare (fixnum start end delta middle))
             (when (< start end)
               (setq middle (round (+ start end) 2))
               (file-position file middle)
               (if (and (plusp (setq delta (scan-for #\^_ file)))
                        (<= delta (- end middle)))
                   (if (equal key (setq sym (read file)))
                       t
                       (if (string< key sym)
                           (bin-search file start (1- middle))
                           (bin-search file (+ middle delta) end)))
                   (bin-search file start (1- middle)))))
           (scan-for (char file)
             (do ((v #\space (read-char file nil nil))
                  (n 0 (1+ n)))
                 ((or (eql v #\^_) (not v)) (if v n -1))
               (declare (fixnum n)))))
    (when (not (probe-file path))
      (return-from search-help-file nil))
    (let* ((*package* (find-package "CL"))
           (file (open path :direction :input))
           output)
      (when (bin-search file 0 (file-length file))
        (setq output (read file)))
      (close file)
      output)))




;;;;----------------------------------------------------------------------
;;;; Documentation system
;;;;

;; *documentation-pool* was defined in documentation.cc

(defparameter *keep-documentation* t)

#|
;; In Clasp I implemented the following functions in documentation.cc
;; in C++ so that they would be available during startup before
;; the CL code is loaded.
;; The following functions are defined in documentation.cc

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

|#



(defun remove-annotation (object key sub-key)
  (let ((dict (first *documentation-pool*)))
    (when (hash-table-p dict)
      (let ((record (rem-record-field (gethash object dict)
                                      key sub-key)))
	(if record
            #+ecl(si::hash-set object dict record)
            #+clasp(core::hash-table-setf-gethash dict object record)
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
(export 'get-annotation)

;;  "Args: (filespec &optional (merge nil))
;;Saves the current hash table for documentation strings to the specificed file.
;;If MERGE is true, merges the contents of this table with the original values in
;;the help file."

(defun dump-documentation (file &optional (merge nil))
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
        (ext:annotate object key doc-type string)
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
(export (list (intern "OPTIONAL-ANNOTATION" "EXT")
              (intern "ANNOTATE" "EXT"))
              "EXT")
(si::fset 'ext:optional-annotation
          (function 
           (lambda (whole env)
	    (declare (ignore env #-clasp-min whole)
		     (core:lambda-name ext:optional-annotation))
            #+clasp-min `(ext:annotate ,@(rest whole))))
	  t)

(defun default-annotation-logic (source-loc definition output-form
                                 &optional (dspec (make-dspec definition)))
  (let* ((kind (first definition))
         (name (second definition)))
    `(progn
       (ext:optional-annotation ',name 'location ',dspec ',source-loc)
       ,(when (member kind '(defun defmacro defgeneric))
          `(ext:optional-annotation ',name :lambda-list nil ',(third definition)))
       ,output-form)))

