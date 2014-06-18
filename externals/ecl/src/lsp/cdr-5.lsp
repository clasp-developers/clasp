;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2011, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "EXT")

;;;
;;; List of symbols for symbols_list.h
;;;
#+(or)
(progn
  (loop with *print-case* = :downcase
     for i in '(fixnum integer rational ratio
                   real float short-float single-float
                   double-float long-float)
     do (loop for j in '("negative-~a-p" "non-negative-~a-p"
                         "non-positive-~a-p" "positive-~a-p")
           do (format t "ext:~? " j (list i)))
     do (terpri))
  (loop for i in '(fixnum integer rational ratio
                   real float short-float single-float
                   double-float long-float)
     for name = (substitute #\_ #\- (string-downcase i))
     do (loop for j in '("NEGATIVE-~A" "NON-NEGATIVE-~A" "NON-POSITIVE-~A" "POSITIVE-~A")
           do (format t
                      "~%{EXT_ \"~?\", EXT_ORDINARY, NULL, -1, OBJNULL},"
                      j (list i)))
     do (loop for j in
             '("~%{EXT_ \"NEGATIVE-~A-P\", EXT_ORDINARY, ECL_NAME(si_negative_~A_p), 1, OBJNULL},"
               "~%{EXT_ \"POSITIVE-~A-P\", EXT_ORDINARY, ECL_NAME(si_positive_~A_p), 1, OBJNULL},"
               "~%{EXT_ \"NON-NEGATIVE-~A-P\", EXT_ORDINARY, ECL_NAME(si_non_negative_~A_p), 1, OBJNULL},"
               "~%{EXT_ \"NON-POSITIVE-~A-P\", EXT_ORDINARY, ECL_NAME(si_non_positive_~A_p), 1, OBJNULL},")
           do (format t j i name))
     do (terpri))
  (loop for i in '(fixnum integer rational ratio
                real float short-float single-float
                double-float long-float)
     for name = (substitute #\_ #\- (string-downcase i))
     do (loop for j in '("negative" "non_negative" "non_positive" "positive")
           do (format t "~%extern ECL_API cl_object si_~a_~a_p(cl_object);" j name))
     do (terpri))
  (loop with *print-case* = :downcase
     for i in '(fixnum integer rational ratio
                   real float short-float single-float
                   double-float long-float)
     do (loop for j in '("NEGATIVE-~A-P" "NON-NEGATIVE-~A-P"
                         "NON-POSITIVE-~A-P" "POSITIVE-~A-P")
             for name = (format nil j (symbol-name i))
             for s = (intern name (find-package "EXT"))
             do (print `(proclamation ,s (t) gen-bool :pure)))
     do (terpri)))

;;;
;;; Small integers
;;;

(deftype negative-fixnum ()
  `(integer ,most-negative-fixnum -1))

(deftype non-positive-fixnum ()
  `(integer ,most-negative-fixnum 0))

(deftype non-negative-fixnum ()
  `(integer 0 , most-positive-fixnum))

(deftype positive-fixnum ()
  `(integer 1 ,most-positive-fixnum))

(defun negative-fixnum-p (p)
  (and (si::fixnump p) (minusp (truly-the fixnum p))))

(defun positive-fixnum-p (p)
  (and (si::fixnump p) (plusp (truly-the fixnum p))))

(defun non-negative-fixnum-p (p)
  (and (si::fixnump p) (not (minusp (truly-the fixnum p)))))

(defun non-positive-fixnum-p (p)
  (and (si::fixnump p) (not (plusp (truly-the fixnum p)))))

(defun array-index-p (p)
  (and (si::fixnump p)
       (<= 0 (truly-the fixnum p) array-dimension-limit)))

;;;
;;; Integers
;;;

(deftype negative-integer ()
  '(integer * -1))

(deftype non-positive-integer ()
  '(integer * 0))

(deftype non-negative-integer ()
  '(integer 0 *))

(deftype positive-integer ()
  '(integer 1 *))

(defun negative-integer-p (p)
  (and (integerp p) (minusp (truly-the integer p))))

(defun positive-integer-p (p)
  (and (integerp p) (plusp (truly-the integer p))))

(defun non-negative-integer-p (p)
  (and (integerp p) (not (minusp (truly-the integer p)))))

(defun non-positive-integer-p (p)
  (and (integerp p) (not (plusp (truly-the integer p)))))

;;;
;;; Rationals
;;;

(deftype negative-rational ()
  '(rational * (0)))

(deftype non-positive-rational ()
  '(rational * 0))

(deftype non-negative-rational ()
  '(rational 0 *))

(deftype positive-rational ()
  '(rational (0) *))

(defun negative-rational-p (p)
  (and (rationalp p) (minusp (truly-the rational p))))

(defun positive-rational-p (p)
  (and (rationalp p) (plusp (truly-the rational p))))

(defun non-negative-rational-p (p)
  (and (rationalp p) (not (minusp (truly-the rational p)))))

(defun non-positive-rational-p (p)
  (and (rationalp p) (not (plusp (truly-the rational p)))))

;;;
;;; Ratios
;;;

(defun ratiop (x)
  (eq (type-of x) 'RATIO))

(defun positive-ratio-p (x)
  (and (ratiop x) (plusp x)))

(defun negative-ratio-p (x)
  (and (ratiop x) (minusp x)))

(deftype negative-ratio ()
  '(satisfies negative-ratio-p))

(deftype non-positive-ratio ()
  'negative-ratio)

(deftype non-negative-ratio ()
  'positive-ratio)

(deftype positive-ratio ()
  '(satisfies positive-ratio-p))

(defun non-negative-ratio-p (p)
  (positive-ratio-p p))

(defun non-positive-ratio-p (p)
  (negative-ratio-p p))

;;;
;;; Reals
;;;

(deftype negative-real ()
  '(real * (0)))

(deftype non-positive-real ()
  '(real * 0))

(deftype non-negative-real ()
  '(real 0 *))

(deftype positive-real ()
  '(real (0) *))

(defun negative-real-p (p)
  (and (realp p) (minusp (truly-the real p))))

(defun positive-real-p (p)
  (and (realp p) (plusp (truly-the real p))))

(defun non-negative-real-p (p)
  (and (realp p) (not (minusp (truly-the real p)))))

(defun non-positive-real-p (p)
  (and (realp p) (not (plusp (truly-the real p)))))

;;;
;;; Floats
;;;

(deftype negative-float ()
  '(float * (0)))

(deftype non-positive-float ()
  '(float * 0))

(deftype non-negative-float ()
  '(float 0 *))

(deftype positive-float ()
  '(float (0) *))

(defun negative-float-p (p)
  (and (floatp p) (minusp (truly-the float p))))

(defun positive-float-p (p)
  (and (floatp p) (plusp (truly-the float p))))

(defun non-negative-float-p (p)
  (and (floatp p) (not (minusp (truly-the float p)))))

(defun non-positive-float-p (p)
  (and (floatp p) (not (plusp (truly-the float p)))))

;;;
;;; SHORT-FLOAT
;;;

(deftype negative-short-float ()
  '(short-float * (0S0)))

(deftype non-positive-short-float ()
  '(short-float * 0S0))

(deftype non-negative-short-float ()
  '(short-float 0S0 *))

(deftype positive-short-float ()
  '(short-float (0S0) *))

(defun negative-short-float-p (p)
  (and (short-floatp p) (minusp (truly-the short-float p))))

(defun positive-short-float-p (p)
  (and (short-floatp p) (plusp (truly-the short-float p))))

(defun non-negative-short-float-p (p)
  (and (short-floatp p) (not (minusp (truly-the short-float p)))))

(defun non-positive-short-float-p (p)
  (and (short-floatp p) (not (plusp (truly-the short-float p)))))

;;;
;;; SINGLE-FLOAT
;;;

(deftype negative-single-float ()
  '(single-float * (0F0)))

(deftype non-positive-single-float ()
  '(single-float * 0F0))

(deftype non-negative-single-float ()
  '(single-float 0F0 *))

(deftype positive-single-float ()
  '(single-float (0F0) *))

(defun negative-single-float-p (p)
  (and (single-floatp p) (minusp (truly-the single-float p))))

(defun positive-single-float-p (p)
  (and (single-floatp p) (plusp (truly-the single-float p))))

(defun non-negative-single-float-p (p)
  (and (single-floatp p) (not (minusp (truly-the single-float p)))))

(defun non-positive-single-float-p (p)
  (and (single-floatp p) (not (plusp (truly-the single-float p)))))

;;;
;;; DOUBLE-FLOAT
;;;

(deftype negative-double-float ()
  '(double-float * (0D0)))

(deftype non-positive-double-float ()
  '(double-float * 0D0))

(deftype non-negative-double-float ()
  '(double-float 0D0 *))

(deftype positive-double-float ()
  '(double-float (0D0) *))

(defun negative-double-float-p (p)
  (and (double-floatp p) (minusp (truly-the double-float p))))

(defun positive-double-float-p (p)
  (and (double-floatp p) (plusp (truly-the double-float p))))

(defun non-negative-double-float-p (p)
  (and (double-floatp p) (not (minusp (truly-the double-float p)))))

(defun non-positive-double-float-p (p)
  (and (double-floatp p) (not (plusp (truly-the double-float p)))))

;;;
;;; LONG-FLOAT
;;;

(deftype negative-long-float ()
  '(long-float * (0L0)))

(deftype non-positive-long-float ()
  '(long-float * 0L0))

(deftype non-negative-long-float ()
  '(long-float 0L0 *))

(deftype positive-long-float ()
  '(long-float (0L0) *))

(defun negative-long-float-p (p)
  (and (long-floatp p) (minusp (truly-the long-float p))))

(defun positive-long-float-p (p)
  (and (long-floatp p) (plusp (truly-the long-float p))))

(defun non-negative-long-float-p (p)
  (and (long-floatp p) (not (minusp (truly-the long-float p)))))

(defun non-positive-long-float-p (p)
  (and (long-floatp p) (not (plusp (truly-the long-float p)))))
