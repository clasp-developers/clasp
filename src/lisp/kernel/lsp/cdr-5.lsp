;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2011 Juan Jose Garcia-Ripoll
;;;;
;;;;  See file 'LICENSE' for the copyright details.
;;;;

;;;; CDR 5: Sub-interval Numerical Types for Common Lisp
;;;;
;;;;   https://common-lisp.net/project/cdr/document/5/index.html

(pushnew :cdr-5 *features*)

(in-package "EXT")

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
  (and (si::fixnump p) (minusp (the fixnum p))))

(defun positive-fixnum-p (p)
  (and (si::fixnump p) (plusp (the fixnum p))))

(defun non-negative-fixnum-p (p)
  (and (si::fixnump p) (not (minusp (the fixnum p)))))

(defun non-positive-fixnum-p (p)
  (and (si::fixnump p) (not (plusp (the fixnum p)))))

(defun array-index-p (p)
  (and (si::fixnump p)
       (<= 0 (the fixnum p) array-dimension-limit)))

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
  (and (integerp p) (minusp (the integer p))))

(defun positive-integer-p (p)
  (and (integerp p) (plusp (the integer p))))

(defun non-negative-integer-p (p)
  (and (integerp p) (not (minusp (the integer p)))))

(defun non-positive-integer-p (p)
  (and (integerp p) (not (plusp (the integer p)))))

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
  (and (rationalp p) (minusp (the rational p))))

(defun positive-rational-p (p)
  (and (rationalp p) (plusp (the rational p))))

(defun non-negative-rational-p (p)
  (and (rationalp p) (not (minusp (the rational p)))))

(defun non-positive-rational-p (p)
  (and (rationalp p) (not (plusp (the rational p)))))

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
  (and (realp p) (minusp (the real p))))

(defun positive-real-p (p)
  (and (realp p) (plusp (the real p))))

(defun non-negative-real-p (p)
  (and (realp p) (not (minusp (the real p)))))

(defun non-positive-real-p (p)
  (and (realp p) (not (plusp (the real p)))))

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
  (and (floatp p) (minusp (the float p))))

(defun positive-float-p (p)
  (and (floatp p) (plusp (the float p))))

(defun non-negative-float-p (p)
  (and (floatp p) (not (minusp (the float p)))))

(defun non-positive-float-p (p)
  (and (floatp p) (not (plusp (the float p)))))

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
  (and (core:short-float-p p) (minusp (the short-float p))))

(defun positive-short-float-p (p)
  (and (core:short-float-p p) (plusp (the short-float p))))

(defun non-negative-short-float-p (p)
  (and (core:short-float-p p) (not (minusp (the short-float p)))))

(defun non-positive-short-float-p (p)
  (and (core:short-float-p p) (not (plusp (the short-float p)))))

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
  (and (core:single-float-p p) (minusp (the single-float p))))

(defun positive-single-float-p (p)
  (and (core:single-float-p p) (plusp (the single-float p))))

(defun non-negative-single-float-p (p)
  (and (core:single-float-p p) (not (minusp (the single-float p)))))

(defun non-positive-single-float-p (p)
  (and (core:single-float-p p) (not (plusp (the single-float p)))))

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
  (and (core:double-float-p p) (minusp (the double-float p))))

(defun positive-double-float-p (p)
  (and (core:double-float-p p) (plusp (the double-float p))))

(defun non-negative-double-float-p (p)
  (and (core:double-float-p p) (not (minusp (the double-float p)))))

(defun non-positive-double-float-p (p)
  (and (core:double-float-p p) (not (plusp (the double-float p)))))

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
  (and (core:long-float-p p) (minusp (the long-float p))))

(defun positive-long-float-p (p)
  (and (core:long-float-p p) (plusp (the long-float p))))

(defun non-negative-long-float-p (p)
  (and (core:long-float-p p) (not (minusp (the long-float p)))))

(defun non-positive-long-float-p (p)
  (and (core:long-float-p p) (not (plusp (the long-float p)))))
