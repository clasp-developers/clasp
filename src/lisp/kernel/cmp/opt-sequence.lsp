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

(defmacro do-in-seq ((%elt sequence &key (start 0) end output) &body body)
  (si::with-unique-names (%start %iterator %counter %sequence)
    (flet ((optional-end (&rest forms)
             (when end
               `(,@forms))))
      `(let* ((,%sequence ,sequence)
              (,%start ,start)
              (,%iterator (si::make-seq-iterator ,%sequence ,%start))
              ,@(optional-end `(,%counter (- (or ,end most-positive-fixnum) ,%start))))
         ,@(optional-end `(declare (type fixnum ,%counter)))
         (loop
           (unless (and ,%iterator
                        ,@(optional-end `(plusp ,%counter)))
             (return ,output))
           (let ((,%elt (si::seq-iterator-ref ,%sequence ,%iterator)))
             ,@body)
           (setf ,%iterator (si::seq-iterator-next ,%sequence ,%iterator))
           ,@(optional-end `(decf ,%counter)))))))

(defun gensym-list (list &optional x)
  (loop
    :for _ :in list
    :collect (if x (gensym x) (gensym))))

;;; Somewhat harder to understand version of do-sequences
;;; where the list of sequences is known at compile time.
;;; CALLER is a symbol. It is bound to a function of one argument,
;;; a function. CALLER calls this function with elements of the
;;; sequences as arguments.
;;; So, (do-static-sequences (c (list 1 2 3) (list 4 5)) (c (lambda (a b) (print (+ a b)))))
;;; will print 5, then 7, then stop.
(defmacro do-static-sequences ((caller &rest sequences) &body body)
  (let ((seqs (gensym-list sequences "SEQUENCE"))
        (iters (gensym-list sequences "ITERATOR")))
    `(block nil
       (let (,@(mapcar #'list seqs sequences))
         (do (,@(mapcar (lambda (s i)
                          `(,i (si::make-seq-iterator ,s) (si::seq-iterator-next ,s ,i)))
                        seqs iters))
             ((or ,@(mapcar (lambda (s i) `(si::seq-iterator-endp ,s ,i)) seqs iters)))
           ;; We should just have a local function, but as of July 2017 we do very
           ;; badly at eliminating unneeded closures.
           (macrolet ((,caller (fun)
                        (list 'funcall fun ,@(mapcar (lambda (s i) `'(si::seq-iterator-ref ,s ,i))
                                                     seqs iters))))
             (tagbody ,@body))
           #+(or)
           (flet ((,caller (fun)
                    (funcall fun ,@(mapcar (lambda (s i) `(seq-iterator-ref ,s ,i))
                                           seqs iters))))
             (declare (inline ,caller))
             (tagbody ,@body)))))))

;;; TODO: Avoid iteration for constant sequence (but watch out for growth)

;;;
;;; FIND
;;;

(define-compiler-macro find (&whole whole value sequence &rest sequence-args &environment env)
  (multiple-value-bind (key-function test-function init
                        key-flag test-flag test start end)
      (two-arg-test-parse-args 'find sequence-args :environment env)
    (if key-function
        (si::with-unique-names (%value %elt)
          `(let ((,%value ,value)
                 ,@init)
             (do-in-seq (,%elt ,sequence :start ,start :end ,end)
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
    (multiple-value-bind (element-type length success)
        (si::closest-sequence-type type)
      (cond ((not success) ; give up for runtime error or unknown, and warn
             (cmp:warn-undefined-type nil type)
             form)
            ((eq element-type 'list)
             (if (eq length '*)
                     (let ((ss (gensym "SIZE")))
                        `(let* ((,ss ,size))
                           (if (and (subtypep ',type 'CONS) (zerop ,ss))
                               (si::error-sequence-length nil ',type ,ss)
                               (make-list ,size :initial-element ,initial-element))))
                     (let ((ss (gensym "SIZE"))
                           (r (gensym "RESULT")))
                   `(let* ((,ss ,size)
                           (,r (make-list ,ss :initial-element :initial-element)))
                      (if (eql ,length ,ss)
                          ,r
                          (si::error-sequence-length ,r ',type ,ss))))))
            (t (let ((r (gensym "RESULT")) (ss (gensym "SIZE")))
                 `(let* ((,ss ,size)
                         ;; negative size will crash sys:make-vector
                         (,r (if (< ,ss 0)
                                 (error 'type-error :datum ,ss :expected-type '(integer 0 *))
                                 (sys:make-vector ',(if (eq element-type '*) t element-type)
                                                  ,ss))))
                    ,@(when iesp
                        `((si::fill-array-with-elt ,r ,initial-element 0 nil)))
                    ,@(unless (eql length '*)
                        `((unless (eql ,length ,ss)
                            (si::error-sequence-length ,r ',type ,ss))))
                    ,r)))))))

;;;
;;; CONCATENATE
;;;

(define-compiler-macro concatenate
    (&whole form result-type &rest sequences &environment env)
  (unless (constantp result-type env) (return-from concatenate form))
  (let ((type (ext:constant-form-value result-type env)))
    (multiple-value-bind (element-type length success)
        (si::closest-sequence-type type)
      (cond ((not success)
             (cmp:warn-undefined-type nil type)
             form)
            ((eq element-type 'list)
             `(si::concatenate-to-list ,@sequences))
            (t
             (let ((symlist (gensym-list sequences "SEQUENCE")))
               `(let (,@(loop for s in symlist for ss in sequences
                              collect `(,s ,ss)))
                  (si::concatenate-into-vector
                   (sys:make-vector ',(if (eq element-type '*) t element-type)
                                    (+ ,@(loop for s in symlist
                                               collect `(length ,s))))
                   ,@sequences))))))))

;;;
;;; MAP
;;;

(define-compiler-macro si::map-for-effect
    (function sequence &rest more-sequences)
  (let* ((fun (gensym "FUNCTION")))
    `(let ((,fun (si::coerce-fdesignator ,function)))
       (do-static-sequences (call ,sequence ,@more-sequences)
         (call ,fun))
       nil)))

(define-compiler-macro map
    (&whole form result-type function sequence &rest more-sequences &environment env)
  (if (constantp result-type env)
      (let ((result-type (ext:constant-form-value result-type env))) ; constant-form-value
        (if result-type
            (let* ((fun (gensym "FUNCTION"))
                   (output (gensym "OUTPUT"))
                   (output-iter (gensym "OUTPUT-ITER"))
                   (sequences (cons sequence more-sequences))
                   (seqs (gensym-list sequences "SEQUENCE")))
              ;; We might turn this into an assertion in later stages.
              `(the (values ,result-type &rest nil)
                    ;; this is basically MAP-INTO, except we don't bother checking
                    ;; for the end of output iteration.
                    (let* ((,fun (si::coerce-fdesignator ,function))
                           ;; Have to (redundantly) once-only these because
                           ;; we need the lengths.
                           ,@(mapcar #'list seqs sequences)
                           (,output (make-sequence
                                     ',result-type
                                     (min ,@(mapcar (lambda (s) `(length ,s)) seqs))))
                           (,output-iter (si::make-seq-iterator ,output)))
                      (do-static-sequences (call ,@seqs)
                        (si::seq-iterator-set ,output ,output-iter (call ,fun))
                        (setq ,output-iter (si::seq-iterator-next ,output ,output-iter)))
                      ,output)))
            `(si::map-for-effect ,function ,sequence ,@more-sequences)))
      form))

;;;
;;; MAP-INTO
;;;


;;; MAP-INTO has special behavior on vectors with fill pointers, so we specialize.
(defmacro map-into-usual (output fun &rest seqs)
  (let ((output-iter (gensym "OUTPUT-ITERATOR")))
    `(let ((,output-iter (si::make-seq-iterator ,output)))
       (do-static-sequences (call ,@seqs)
         (when (si::seq-iterator-endp ,output ,output-iter) (return))
         (si::seq-iterator-set ,output ,output-iter (call ,fun))
         (setq ,output-iter (si::seq-iterator-next ,output ,output-iter))))))

(defmacro map-into-fp (output fun &rest seqs)
  (let ((output-index (gensym "OUTPUT-INDEX"))
        (output-size (gensym "OUTPUT-SIZE")))
    `(let ((,output-index 0)
           (,output-size (array-dimension ,output 0)))
       (do-static-sequences (call ,@seqs)
         (when (= ,output-index ,output-size) (return))
         (setf (aref ,output ,output-index) (call ,fun))
         (incf ,output-index))
       (setf (fill-pointer ,output) ,output-index))))

(define-compiler-macro map-into (result function &rest sequences)
  ;; handle multiple evaluation up here
  (let ((output (gensym "OUTPUT"))
        (fun (gensym "FUNCTION"))
        (seqs (gensym-list sequences "SEQUENCE")))
    `(let ((,output ,result)
           (,fun (si::coerce-fdesignator ,function))
           ,@(mapcar #'list seqs sequences))
       (if (and (vectorp ,output) (array-has-fill-pointer-p ,output))
           (map-into-fp ,output ,fun ,@seqs)
           (map-into-usual ,output ,fun ,@seqs))
       ,output)))

;;;
;;; EVERY, SOME, ANY, NOTANY
;;; These are actually part of "Data and Control Flow" but they're basically sequence functions.
;;;

(flet ((body (predicate sequences whenless found unfound)
         (let ((p (gensym "PREDICATE"))
               (b (gensym)))
         `(block ,b
            (let ((,p ,predicate))
              (do-static-sequences (call ,@sequences)
                (let ((it (call ,p)))
                  (,whenless it (return-from ,b ,found))))
              ,unfound)))))
  (macrolet ((def (name whenless found unfound)
               `(define-compiler-macro ,name (predicate sequence &rest more-sequences)
                  (body predicate (cons sequence more-sequences) ',whenless ',found ',unfound))))
    (def some when it nil)
    (def every unless nil t)
    (def notany when nil t)
    (def notevery unless t nil)))

(define-compiler-macro si::every* (predicate &rest sequences)
  (let ((seqs (gensym-list sequences "SEQUENCE")))
    `(and (= ,@(mapcar (lambda (s) `(length ,s)) seqs))
          (every ,predicate ,@seqs))))
