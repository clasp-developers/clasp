;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

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

;; If a form refers to a function we can use as the head of a form, return something suitable
;; as head of form. Else NIL.
(defun constant-function-expression (form env)
  (declare (ignore env))
  (if (consp form)
      (cond ((eq (car form) 'function)
             (if (and (consp (cdr form)) (null (cddr form)))
                 (second form)
                 ;; invalid function form; could warn, but compiler should get it
                 nil))
            ;; FIXME: commented out because there could be a local binding; need to be smarter w/environment
            #+(or)
            ((eq (car form) 'quote)
             (if (and (consp (cdr form)) (null (cddr form)))
                 (second form)
                 nil))
            ((eq (car form) 'lambda) form)
            (t nil))
      nil))

;; Return a function of two forms that returns a condition form for testing them, as well as required bindings.
;; E.g. (seq-opt-test-function :test #'foo nil) => #'(lambda (v1 v2) `(foo ,v1 ,v2)), NIL
;; (seq-opt-test-function :test (generate-test) nil) =>
;;   #'(lambda (v1 v2) `([gensym] ,v1 ,v2)), (([gensym] (generate-test)))
(defun seq-opt-test-function (test-flag test env)
  (cond ((null test-flag)
         (values (seq-opt-test-function :test '#'eql env) nil))
        ((eq test-flag :test-not)
         (multiple-value-bind (function init)
             (seq-opt-test-function :test test env)
           (values #'(lambda (v1 v2) `(not ,(funcall function v1 v2)))
                   init)))
        (t (let ((maybe-head (constant-function-expression test env)))
             (if maybe-head
                 (values #'(lambda (v1 v2) `(,maybe-head ,v1 ,v2)) nil)
                 (si::with-unique-names (test-function)
                   (values #'(lambda (v1 v2) `(funcall ,test-function ,v1 ,v2))
                           (list (list test-function test)))))))))

;; Like the above, but with a key.
(defun seq-opt-key-function (key env)
  (cond ((null key)
         (values #'identity nil))
        (t (let ((maybe-head (constant-function-expression key env)))
             (if maybe-head
                 (values #'(lambda (elt) `(,maybe-head ,elt)) nil)
                 (si::with-unique-names (key-function)
                   (values #'(lambda (elt) `(funcall ,key-function ,elt))
                           (list (list key-function
                                       `(or ,key #'identity))))))))))

;; Returns (values keyf testf inits keyk testk test start end)
;; keyf and testf are the functions returned by seq-opt-etc above. inits are from them as well.
;; keyk and testk are the keywords for them, i.e. keyk is :key if :key was specified or else NIL,
;;  and testk is :test or :test-not or nil.
;; test is the form passed as a :test or :test-not, or #'eql if none was passed.
;; test is used for dropping to memq and friends even if a full inline isn't done. (FIXME: will be used for.)
;; start and end are the same parameters. the start-end argument controls whether they're valid.
;; If the call is invalid - e.g. has both :test and :test-not - all values will be nil, including the
;;  primary value keyf, which is otherwise a function.
(defun seq-opt-parse-args (function args &key (start-end t) environment)
  (loop with key-flag = nil
        with key = nil
        with init = nil
        with test = '#'eql
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
                          key-flag keyword)))
                 ((or (eq keyword :test)
                      (eq keyword :test-not))
                  (cond ((null test-flag)
                         (setf test (pop args)
                               test-flag keyword))
                        ((not (eq test-flag keyword))
                         (warn "Cannot specify :TEST and :TEST-NOT arguments to ~A"
                               function)
                         (return nil))))
                 ((eq keyword :start)
                  (unless start-end
                    (warn "Unexpected keyword argument ~A in a call to function ~A"
                          keyword function)
                    (return nil))
                  (setf start (pop args)))
                 ((eq keyword :end)
                  (unless start-end
                    (warn "Unexpected keyword argument ~A in a call to function ~A"
                          keyword function)
                    (return nil))
                  (setf end (pop args)))
                 ((eq keyword :from-end)
                  (unless (null (pop args))
                    (return nil)))
                 (t (return nil)))
        finally
           (multiple-value-bind (key-function key-init)
               (seq-opt-key-function key environment)
             (multiple-value-bind (test-function test-init)
                 (seq-opt-test-function test-flag test environment)
               (return (values key-function
                               test-function
                               (nconc key-init test-init)
                               key-flag
                               test-flag
                               test
                               start
                               end))))))

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
  (si::with-unique-names (%start %iterator %counter %sequence)
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
         (declare (ignorable ,%counter)
                  (type fixnum ,%counter))
         (loop
            (unless ,test (return ,output))
            (let ((,%elt (si::seq-iterator-ref ,%sequence ,%iterator)))
              ,@body)
            (setf ,%iterator (si::seq-iterator-next ,%sequence ,%iterator)))))))

;;;
;;; MEMBER
;;;

(defmacro do-in-list ((%elt %sublist list &rest output) &body body)
  `(do* ((,%sublist ,list (cdr ,%sublist)))
        ((null ,%sublist) ,@output)
     (let ((,%elt (car (the cons ,%sublist))))
       ,@body)))

(defun expand-member (env value list &rest sequence-args)
  (multiple-value-bind (key-function test-function init
                        key-flag test-flag test)
      (seq-opt-parse-args 'member sequence-args :start-end nil :environment env)
    ;; When having complex arguments (:allow-other-keys, etc)
    ;; we just give up.
    (when (null key-function)
      (return-from expand-member nil))
    (si::with-unique-names (%value %sublist %elt)
      `(let ((,%value ,value)
             ,@init)
         (do-in-list (,%elt ,%sublist ,list)
           (when ,(funcall test-function %value
                           (funcall key-function %elt))
             (return ,%sublist)))))))

(define-compiler-macro member (&whole whole value list &rest sequence-args &environment env)
  ;; FIXME: pay attention to policy, e.g. don't inline for high SPACE.
  (or (apply #'expand-member env (rest whole))
      whole))

;;;
;;; ASSOC
;;;

(defun expand-assoc (env value list &rest sequence-args)
  (multiple-value-bind (key-function test-function init
                        key-flag test-flag test)
      (seq-opt-parse-args 'assoc sequence-args :start-end nil :environment env)
    (when test-function
      (si::with-unique-names (%value %sublist %elt %car)
        `(let ((,%value ,value)
               ,@init)
           (do-in-list (,%elt ,%sublist ,list)
             (when ,%elt
               (let ((,%car (car (the cons ,%elt))))
                 (when ,(funcall test-function %value
                                 (funcall key-function %car))
                   (return ,%elt))))))))))

(define-compiler-macro assoc (&whole whole value list &rest sequence-args &environment env)
  (or (apply #'expand-assoc env (rest whole))
      whole))

;;;
;;; FIND
;;;

(defun expand-find (env value sequence &rest sequence-args)
  (multiple-value-bind (key-function test-function init
                        key-flag test-flag test start end)
      (seq-opt-parse-args 'find sequence-args :environment env)
    (when test-function
      (si::with-unique-names (%value %elt)
        `(let ((,%value ,value)
               ,@init)
           (do-in-seq (,%elt ,sequence :start ,start :end ,end)
             (when ,(funcall test-function %value
                             (funcall key-function %elt))
               (return ,%elt))))))))

(define-compiler-macro find (&whole whole value sequence &rest sequence-args &environment env)
  (or (apply #'expand-find env (rest whole))
      whole))
