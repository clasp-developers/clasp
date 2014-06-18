;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Compiler regression tests

(in-package :cl-test)

;;; Date: 10/08/2008
;;; From: Juanjo
;;; Fixed: 10/08/2008
;;; Description:
;;;
;;;	COS, SIN and TAN were expanded using a wrong C expression.
;;;

(deftest num-0001-inline-cos
    (loop with *compile-verbose* = nil
       with *compile-print* = nil
       for type in '(short-float single-float double-float long-float)
       for sample = (coerce 1.0 type)
       for epsilon in '(#.short-float-epsilon #.single-float-epsilon #.double-float-epsilon #.long-float-epsilon)
       unless (loop for op in '(sin cos tan sinh cosh tanh)
                 for f = (compile 'nil `(lambda (x)
                                          (declare (,type x)
                                                   (optimize (safety 0)
                                                             (speed 3)))
                                          (+ ,sample (,op x))))
                 always (loop for x from (- pi) below pi by 0.05
                           for xf = (float x sample)
                           for error =  (- (funcall f xf) (+ 1 (funcall op xf)))o
                           always (< (abs error) epsilon)))
       collect type)
  nil)
