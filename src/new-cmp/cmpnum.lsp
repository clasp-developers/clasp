;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;; CMPNUM -- Optimizer for numerical expressions.

;;;;  Copyright (c) 2005, Juan Jose Garcia Ripoll
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defun simplify-arithmetic (operator args whole)
  (if (every #'numberp args)
      (apply operator args)
      (let ((l (length args)))
        (cond ((> l 2)
               (simplify-arithmetic
                operator
                (list* (simplify-arithmetic operator
                                            (list (first args) (second args))
                                            nil)
                       (cddr args))
                nil))
              ((= l 2)
               (or whole (list* operator args)))
              ((= l 1)
               (if (or (eq operator '*) (eq operator '+))
                   (first args)
                   (or whole (list* operator args))))
              ((eq operator '*)
               1)
              ((eq operator '+)
               0)
              (t
               (error 'simple-program-error
                      :format-error "Wrong number of arguments for operator ~a in ~a"
                      :format-arguments (list operators (or whole
                                                            (list* operator args)))))))))

(define-compiler-macro * (&whole all &rest args)
  (simplify-arithmetic '* args all))

(define-compiler-macro + (&whole all &rest args)
  (simplify-arithmetic '+ args all))

(define-compiler-macro / (&whole all &rest args)
  (simplify-arithmetic '/ args all))

(define-compiler-macro - (&whole all &rest args)
  (simplify-arithmetic '- args all))

;;;
;;; The following are type propagators for arithmetic operations. Note
;;; that some of they have become binary operators.
;;;

(defun maximum-number-type (t1 t2 &optional only-real)
  ;; Computes the output type of an operation between number types T1
  ;; and T2 using the rules of floating point contagion. It returns
  ;; the type of the result, and the types of T1 and T2, if they
  ;; represent known types, or NUMBER, in other cases.
  (let ((t1-eq nil)
        (t2-eq nil)
        (output nil)
        (default (if only-real 'REAL 'NUMBER))
        (types-list (if only-real
                        '(FIXNUM INTEGER RATIONAL
                          #+short-float SHORT-FLOAT SINGLE-FLOAT
                          DOUBLE-FLOAT #+long-float LONG-FLOAT FLOAT REAL
                          NUMBER)
                        '(FIXNUM INTEGER RATIONAL
                          #+short-float SHORT-FLOAT SINGLE-FLOAT
                          DOUBLE-FLOAT #+long-float LONG-FLOAT FLOAT REAL))))
    (dolist (i types-list
             (values (if (and t1-eq t2-eq output) output default)
                     (if t1-eq t1 default)
                     (if t2-eq t2 default)))
      (when (and (null t1-eq) (type>= i t1))
        (if (equalp t1 t2)
            (setf t2-eq i))
        (setf t1-eq i output i))
      (when (and (null t2-eq) (type>= i t2))
        (setf t2-eq i output i)))))

(defun ensure-number-type (general-type)
  (maximum-number-type general-type general-type))

(defun ensure-nonrational-type (general-type)
  (maximum-number-type general-type 'single-float))

(defun ensure-real-type (general-type)
  (maximum-number-type general-type 'integer :only-real))

(defun arithmetic-propagator (op1-type others integer-result)
  ;; Propagates types for an associative operator (we do not care which one).
  ;; We collect either the types of the arguments or 'NUMBER, as a generic
  ;; expected type. The output type is computed using the rules of floating
  ;; point contagion, with the exception that an operation between two
  ;; integers has type INTEGER-RESULT (integer for *,-,+ and rational else)
  (multiple-value-bind (result-type op1-type)
      (ensure-number-type op1-type)
    (loop with arg-types = (list op1-type)
       for x in others
       for op2-type = x
       do (progn
            (multiple-value-setq (result-type op1-type op2-type)
              (maximum-number-type result-type op2-type))
            (when (or (eq result-type 'FIXNUM) (eq result-type 'INTEGER))
              (setf result-type integer-result))
            (setf arg-types (cons op2-type arg-types)))
       finally (return (values (nreverse arg-types) result-type)))))

(def-type-propagator * (fname op1 &rest others)
  (arithmetic-propagator op1 others 'integer))

(copy-type-propagator '* '(+ -))

(def-type-propagator / (fname op1 &rest others)
  (arithmetic-propagator op1 others 'rational))

;;;
;;; SPECIAL FUNCTIONS
;;;

(def-type-propagator cos (fname op1-type)
  (multiple-value-bind (output-type op1-type)
      (ensure-nonrational-type op1-type)
    (values (list op1-type) output-type)))

(copy-type-propagator 'cos '(sin tan cosh sinh tanh exp))

(def-type-propagator acos (fname op1-type)
  (multiple-value-bind (output-type op1-type)
      (ensure-nonrational-type op1-type)
    (values (list op1-type) 'NUMBER)))

(def-type-propagator atan (fname op1-type &optional (op2-type t op2-p))
  (multiple-value-bind (float-t1 t1)
      (ensure-nonrational-type op1-type)
    (if op2-p
        (multiple-value-bind (result t1 t2)
            (maximum-number-type t1 op2-type :only-real)
          (values (list t1 t2) result))
        (values (list t1) t1))))

(def-type-propagator expt (fname base exponent)
  ;; Rules:
  ;;	(expt number-type integer) -> number-type
  ;;	(expt number-type1 number-type2) -> (max-float number-type1 number-type2)
  ;;
  (multiple-value-bind (simplified-exponent exponent)
      (ensure-real-type exponent)
    (unless (eql simplified-exponent 'integer)
      (setf simplified-exponent (ensure-nonrational-type simplified-exponent)))
    (multiple-value-bind (result-type base aux)
        (maximum-number-type base simplified-exponent)
      (values (list base exponent) result-type))))

(def-type-propagator abs (fname arg)
  (multiple-value-bind (output arg)
      (ensure-number-type arg)
    (values (list arg)
            (or (cdr (assoc output
                            '((FIXNUM . (INTEGER 0 #.MOST-POSITIVE-FIXNUM))
                              (INTEGER . (INTEGER 0 *))
                              (RATIONAL . (RATIONAL 0 *))
                              (SHORT-FLOAT . (SHORT-FLOAT 0 *))
                              (SINGLE-FLOAT . (SINGLE-FLOAT 0 *))
                              (DOUBLE-FLOAT . (DOUBLE-FLOAT 0 *))
                              (LONG-FLOAT . (LONG-FLOAT 0 *))
                              (REAL . (REAL 0 *))
                              (NUMBER . (REAL 0 *)))))
                output))))

(def-type-propagator sqrt (fname arg)
  (multiple-value-bind (output arg)
      (ensure-nonrational-type arg)
    (values (list arg)
            (if (type>= '(REAL 0 *) arg) output 'NUMBER))))

(def-type-propagator isqrt (fname arg)
  (if (type>= '(integer 0 #.MOST-POSITIVE-FIXNUM) arg)
      (values '((integer 0 #.MOST-POSITIVE-FIXNUM))
              '(integer 0 #.MOST-POSITIVE-FIXNUM))
      (values '((integer 0 *)) '(integer 0 *))))

