;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPOPT-SEQUENCE  Optimization of SEQUENCE functions
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defun constant-function-expression (form)
  (and (consp form)
       (member (first form) '(quote function lambda))))

(defun seq-opt-test-function (test-flag test)
  (cond ((null test-flag)
         (values (seq-opt-test-function :test '#'eql) nil))
        ((eq test-flag :test-not)
         (multiple-value-bind (function init)
             (seq-opt-test-function :test test)
           (values #'(lambda (v1 v2) `(not ,(funcall function v1 v2)))
                   init)))
        ((constant-function-expression test)
         (values #'(lambda (v1 v2) `(funcall ,test ,v1 ,v2))
                 nil))
        (t
         (ext:with-unique-names (test-function)
           (values #'(lambda (v1 v2) `(funcall ,test-function ,v1 ,v2))
                   (list (list test-function test)))))))

(defun seq-opt-key-function (key)
  (cond ((null key)
         (values #'identity nil))
        ((constant-function-expression key)
         (values #'(lambda (elt) `(funcall ,key ,elt))
                 nil))
        (t
         (ext:with-unique-names (key-function)
           (values #'(lambda (elt) `(funcall ,key-function ,elt))
                   (list (list key-function
                               `(or ,key #'identity))))))))

(defun seq-opt-parse-args (function args &key (start-end t))
  (loop with key-flag = nil
     with key = nil
     with init = nil
     with test = ''eql
     with test-flag = nil
     with start = 0
     with end = nil
     with keyword
     while args
     do (cond ((or (atom args)
                   (null (rest args))
                   (eq keyword :allow-other-keys)
                   (not (keywordp (setf keyword (pop args)))))
               (return nil))
              ((eq keyword :key)
               (unless key-flag
                 (setf key (pop args)
                       key-flag t)))
              ((or (eq keyword :test)
                   (eq keyword :test-not))
               (cond ((null test-flag)
                      (setf test (pop args)
                            test-flag keyword))
                     ((not (eq test-flag keyword))
                      (cmpwarn "Cannot specify :TEST and :TEST-NOT arguments to ~A"
                               function)
                      (return nil))))
              ((eq keyword :start)
               (unless start-end
                 (cmpwarn "Unexpected keyword argument ~A in a call to function ~A"
                          keyword function)
                 (return nil))
               (setf start (pop args)))
              ((eq keyword :end)
               (unless start-end
                 (cmpwarn "Unexpected keyword argument ~A in a call to function ~A"
                          keyword function)
                 (return nil))
               (setf end (pop args)))
              ((eq keyword :from-end)
               (unless (null (pop args))
                 (return nil)))
              (t (return nil)))
     finally
       (multiple-value-bind (key-function key-init)
           (seq-opt-key-function key)
         (multiple-value-bind (test-function test-init)
             (seq-opt-test-function test-flag test)
         (return (values key-function
                         test-function
                         (nconc key-init test-init)
                         key-flag
                         test-flag
                         test))))))

#+(or)
(define-compiler-macro si::make-seq-iterator (seq &optional (start 0))
  (with-clean-symbols (%seq %start)
    `(let ((%seq (optional-type-check ,seq sequence))
           (%start ,start))
       (cond ((consp %seq)
              (nthcdr %start %seq))
             ((< %start (length %seq))
              %start)
             (t
              nil)))))

#+(or)
(define-compiler-macro si::seq-iterator-ref (seq iterator)
  (with-clean-symbols (%seq %iterator)
    `(let* ((%seq ,seq)
            (%iterator ,iterator))
       (declare (optimize (safety 0)))
       (if (si::fixnump %iterator)
           ;; Fixnum iterators are always fine
           (aref %seq %iterator)
           ;; Error check in case we may have been passed an improper list
           (cons-car (checked-value cons %iterator))))))

#+(or)
(define-compiler-macro si::seq-iterator-next (seq iterator)
  (with-clean-symbols (%seq %iterator)
    `(let* ((%seq ,seq)
            (%iterator ,iterator))
       (declare (optimize (safety 0)))
       (if (si::fixnump %iterator)
           (let ((%iterator (1+ (truly-the fixnum %iterator))))
             (declare (fixnum %iterator))
             (and (< %iterator (length (truly-the vector %seq)))
                  %iterator))
           (cons-cdr %iterator)))))

(defmacro do-in-seq ((%elt sequence &key (start 0) end output) &body body)
  (ext:with-unique-names (%start %iterator %counter %sequence)
    (let* ((counter (if end
                        `(- (or ,end most-positive-fixnum) ,%start)
                        0))
           (test (if end
                     `(and ,%iterator (plusp ,%counter))
                     %iterator)))
      `(let* ((,%sequence ,sequence)
              (,%start ,start)
              (,%iterator (si::make-seq-iterator ,%sequence ,%start))
              (,%counter ,counter))
         (declare (:read-only ,%sequence ,%start ,%counter)
                  (ignorable ,%counter)
                  (fixnum ,%counter))
         (loop
            (unless ,test (return ,output))
            (let ((,%elt (si::seq-iterator-ref ,%sequence ,%iterator)))
              ,@body)
            (setf ,%iterator (si::seq-iterator-next ,%sequence ,%iterator)))))))

;;;
;;; MEMBER
;;;

(defmacro do-in-list ((%elt %sublist list &rest output) &body body)
  `(do* ((,%sublist ,list (cons-cdr ,%sublist)))
        ((null ,%sublist) ,@output)
     (let* ((,%sublist (optional-type-check ,%sublist cons))
            (,%elt (cons-car ,%sublist)))
       ,@body)))

(defun expand-member (value list &rest sequence-args)
  (multiple-value-bind (key-function test-function init
                        key-flag test-flag test)
      (seq-opt-parse-args 'member sequence-args :start-end nil)
    ;; When having complex arguments (:allow-other-keys, etc)
    ;; we just give up.
    (when (null key-function)
      (return-from expand-member nil))
    (unless key-flag
      (when (and (or (null test) (constant-function-expression test))
                 (constant-expression-p list))
        (when (<= (length (setf list (cmp-eval list))) 4)
          (return-from expand-member
            (ext:with-unique-names (%value)
              `(let ((,%value ,value))
                 (or ,@(loop for l on list
                          for elt = (first l)
                          collect `(and ,(funcall test-function %value `',elt)
                                        ',l)))))))
        (when (or (consp list) (symbol list))
          (setf list `',list)))
      (when (or (null test-flag) (eq test-flag :test))
        (when (member test '('EQ #'EQ) :test #'equal)
          (return-from expand-member
            `(ffi:c-inline (,value ,list) (:object :object) :object
                           "si_memq(#0,#1)" :one-liner t :side-effects nil)))
        (when (member test '('EQL #'EQL) :test #'equal)
          (return-from expand-member
            `(ffi:c-inline (,value ,list) (:object :object) :object
                           "ecl_memql(#0,#1)" :one-liner t :side-effects nil)))
        (when (member test '('EQUAL #'EQUAL) :test #'equal)
          (return-from expand-member
            `(ffi:c-inline (,value ,list) (:object :object) :object
                           "ecl_member(#0,#1)" :one-liner t :side-effects nil)))))
    (ext:with-unique-names (%value %sublist %elt)
      `(let ((,%value ,value)
             ,@init)
         (do-in-list (,%elt ,%sublist ,list)
           (when ,(funcall test-function %value
                           (funcall key-function %elt))
             (return ,%sublist)))))))

(define-compiler-macro member (&whole whole value list &rest sequence-args)
  (if (policy-inline-sequence-functions)
      (or (apply #'expand-member (rest whole))
          whole)
      whole))

;;;
;;; ASSOC
;;;

(defun expand-assoc (value list &rest sequence-args)
  (multiple-value-bind (key-function test-function init
                        key-flag test-flag test)
      (seq-opt-parse-args 'assoc sequence-args :start-end nil)
    (unless key-flag
      (when (or (null test-flag) (eq test-flag :test))
        (when (member test '('EQ #'EQ) :test #'equal)
          (return-from expand-assoc
            `(ffi:c-inline (,value ,list) (:object :object) :object
                           "ecl_assq(#0,#1)" :one-liner t :side-effects nil)))
        (when (member test '('EQL #'EQL) :test #'equal)
          (return-from expand-assoc
            `(ffi:c-inline (,value ,list) (:object :object) :object
                           "ecl_assql(#0,#1)" :one-liner t :side-effects nil)))
        (when (member test '('EQUAL #'EQUAL) :test #'equal)
          (return-from expand-assoc
            `(ffi:c-inline (,value ,list) (:object :object) :object
                           "ecl_assoc(#0,#1)" :one-liner t :side-effects nil)))
        (when (member test '('EQUALP #'EQUALP) :test #'equal)
          (return-from expand-assoc
            `(ffi:c-inline (,value ,list) (:object :object) :object
                           "ecl_assqlp(#0,#1)" :one-liner t :side-effects nil)))))
    (when test-function
      (ext:with-unique-names (%value %sublist %elt %car)
        `(let ((,%value ,value)
               ,@init)
           (do-in-list (,%elt ,%sublist ,list)
             (when ,%elt
               (let ((,%car (cons-car (optional-type-check ,%elt cons))))
                 (when ,(funcall test-function %value
                                 (funcall key-function %car))
                   (return ,%elt))))))))))

(define-compiler-macro assoc (&whole whole value list &rest sequence-args)
  (if (policy-inline-sequence-functions)
      (or (apply #'expand-assoc (rest whole))
          whole)
      whole))

;;;
;;; FIND
;;;

(defun expand-find (value sequence &rest sequence-args)
  (multiple-value-bind (key-function test-function init
                        key-flag test-flag test start end)
      (seq-opt-parse-args 'find sequence-args)
    (when test-function
      (ext:with-unique-names (%value %elt)
        `(let ((,%value ,value)
               ,@init)
           (do-in-seq (,%elt ,sequence)
             (when ,(funcall test-function %value
                             (funcall key-function %elt))
               (return ,%elt))))))))

(define-compiler-macro find (&whole whole value sequence &rest sequence-args)
  (if (policy-inline-sequence-functions)
      (or (apply #'expand-find (rest whole))
          whole)
      whole))
