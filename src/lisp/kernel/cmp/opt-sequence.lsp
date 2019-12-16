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

;;; TODO: Avoid iteration for constant sequence (but watch out for growth)

;;;
;;; FIND
;;;

#+(or)
(define-compiler-macro find (&whole whole value sequence &rest sequence-args &environment env)
  (multiple-value-bind (key-function test-function init
                        key-flag test-flag test start end)
      (two-arg-test-parse-args 'find sequence-args :environment env)
    (if key-function
        (si::with-unique-names (%value %elt)
          `(let ((,%value ,value)
                 ,@init)
             (core::do-subsequence (,%elt ,sequence ,start ,end)
               (when ,(funcall test-function %value
                               (funcall key-function %elt))
                 (return ,%elt)))))
        whole)))

;;;
;;; MAKE-SEQUENCE
;;;

(define-compiler-macro make-sequence
    (&whole form type size &key (initial-element nil iesp) &environment env)
  (unless (constantp type env)
    (return-from make-sequence form))
  (let ((type (ext:constant-form-value type env)))
    (multiple-value-bind (kind length exactp success)
        (si::sequence-type-maker-info type env)
      (cond ((not success)
             ;; give up for runtime error or unknown, and warn
             (cmp:warn-undefined-type nil type)
             form)
            ((eq kind 'nil)
             ;; (make-sequence nil...) is weird shit that we leave to runtime.
             form)
            ((eq kind 'list)
             (let ((ss (gensym "SIZE")) (r (gensym "RESULT")))
               `(let* ((,ss ,size)
                       (,r (make-list ,ss :initial-element ,initial-element)))
                  ,@(when length
                      `((unless (,(if exactp 'eql '>=) ,ss ',length)
                          (core::error-sequence-length ,r ',type ,ss))))
                  ,r)))
            ((consp kind) ; (VECTOR uaet)
             (let ((uaet (second kind)) (r (gensym "RESULT")) (ss (gensym "SIZE")))
               `(let* ((,ss ,size)
                       ;; negative size will crash sys:make-vector
                       (,r
                         (if (< ,ss 0)
                             (error 'type-error :datum ,ss :expected-type '(integer 0 *))
                             (sys:make-vector ',uaet ,ss))))
                  ,@(when iesp
                      `((si::fill-array-with-elt ,r ,initial-element 0 nil)))
                  ,@(unless (null length)
                      `((unless (eql ,ss ',length)
                          (si::error-sequence-length ,r ',type ,ss))))
                  ,r)))
            (t ; user sequence type
             ;; Lengths cannot be specified for these
             (if iesp
                 `(sequence:make-sequence ',kind ,size
                                          :initial-element ,initial-element)
                 `(sequence:make-sequence ',kind ,size)))))))

;;;
;;; CONCATENATE
;;;

(define-compiler-macro concatenate
    (&whole form result-type &rest sequences &environment env)
  (unless (constantp result-type env) (return-from concatenate form))
  (let ((type (ext:constant-form-value result-type env)))
    (multiple-value-bind (kind length exactp success)
        (si::sequence-type-maker-info type env)
      (cond ((not success)
             (cmp:warn-undefined-type nil type)
             form)
            ((eq kind 'nil) form)
            ((eq kind 'list)
             (if length ; have to check
                 (let ((r (gensym "RESULT")) (l (gensym "LENGTH")))
                   `(let* ((,r (si::concatenate-to-list ,@sequences))
                           (,l (length ,r)))
                      (unless (,(if exactp 'eq '>=) ,l ',length)
                        (core::error-sequence-length ,r ',result-type ,l))))
                 `(si::concatenate-to-list ,@sequences)))
            (t
             (let* ((symlist (gensym-list sequences "SEQUENCE"))
                    (binds (mapcar #'list symlist sequences))
                    (sum `(+ ,@(loop for s in symlist
                                     collect `(length ,s)))))
               `(let (,@binds)
                  (si::concatenate-into-sequence
                   ;; this means the make-sequence expander will check
                   ;; sequence-type-maker-info again, which is kind of
                   ;; redundant, but it means we can let it handle any
                   ;; length check and stuff like that.
                   (make-sequence ',type ,sum)
                   ,@symlist))))))))

;;;
;;; MAP
;;;

(define-compiler-macro core::map-for-effect
    (&whole form function sequence &rest more-sequences)
  (if (null more-sequences)
      `(core::map-for-effect/1 ,function ,sequence)
      form))

(define-compiler-macro core::map-to-list
    (&whole form function sequence &rest more-sequences)
  (if (null more-sequences)
      `(core::map-to-list/1 ,function ,sequence)
      form))

(define-compiler-macro map
    (&whole form result-type function sequence &rest more-sequences &environment env)
  (if (constantp result-type env)
      (let ((type (ext:constant-form-value result-type env))
            (function `(core:coerce-fdesignator ,function))
            (sequences (cons sequence more-sequences)))
        (if (null type)
            `(core::map-for-effect ,function ,@sequences)
            (multiple-value-bind (kind length exactp success)
                (si::sequence-type-maker-info type env)
              (cond ((not success)
                     (cmp:warn-undefined-type nil type)
                     form)
                    ((eq kind 'list)
                     (if length ; have to check
                         (let ((r (gensym "RESULT")) (l (gensym "LENGTH")))
                           `(let* ((,r (core::map-to-list ,function ,@sequences))
                                   (,l (length ,r)))
                              (unless (,(if exactp 'eq '>=) ,l ',length)
                                (core::error-sequence-length
                                 ,r ',result-type ,l))
                              ,r))
                         `(core::map-to-list ,function ,@sequences)))
                    (t
                     ;; vector type or user sequence type or can't determine.
                     ;; note that map-into-sequence will be sorta inefficient (but valid)
                     ;; if the type ends up being a list, but in the case of weird result
                     ;; types we kind of just give up.
                     ;; MAKE-SEQUENCE handles any length check.
                     ;; TODO: Call SEQUENCE:MAP for user sequence types, maybe.
                     (let ((ssyms (gensym-list sequences "SEQUENCE")))
                       `(let (,@(mapcar #'list ssyms sequences))
                          (core::map-into-sequence
                           (make-sequence ',type
                                          (min ,@(loop for ssym in ssyms
                                                       collect `(length ,ssym))))
                           ,function ,@ssyms))))))))
      form))

;;;
;;; MAP-INTO
;;;

(define-compiler-macro core::map-into-sequence
    (&whole form result function &rest sequences)
  (if (and (consp sequences) (null (rest sequences)))
      `(core::map-into-sequence/1 ,result ,function ,@sequences)
      form))

;;;
;;; EVERY, SOME, ANY, NOTANY
;;; These are actually part of "Data and Control Flow" but they're basically sequence functions.
;;;

(define-compiler-macro every (&whole form predicate sequence &rest more-sequences)
  (if (null more-sequences)
      `(core::every/1 (core:coerce-fdesignator ,predicate) ,sequence)
      form))

(define-compiler-macro some (&whole form predicate sequence &rest more-sequences)
  (if (null more-sequences)
      `(core::some/1 (core:coerce-fdesignator ,predicate) ,sequence)
      form))

(define-compiler-macro notany (predicate sequence &rest more-sequences)
  `(not (some ,predicate ,sequence ,@more-sequences)))

(define-compiler-macro notevery (predicate sequence &rest more-sequences)
  `(not (every ,predicate ,sequence ,@more-sequences)))

(define-compiler-macro si::every* (predicate &rest sequences)
  (let ((seqs (gensym-list sequences "SEQUENCE")))
    `(and (= ,@(mapcar (lambda (s) `(length ,s)) seqs))
          (every ,predicate ,@seqs))))
