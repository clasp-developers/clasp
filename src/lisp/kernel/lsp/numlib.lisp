;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                           number routines

(in-package "SYSTEM")

(defmacro ext::with-float-traps-masked (traps &body body)
  (let ((previous (gensym "PREVIOUS"))
        (mask (reduce (lambda (bits trap)
                        (logior bits
                                (ecase trap
                                  (:underflow core:+fe-underflow+)
                                  (:overflow core:+fe-overflow+)
                                  (:invalid core:+fe-invalid+)
                                  (:inexact core:+fe-inexact+)
                                  (:divide-by-zero core:+fe-divbyzero+)
                                  (:denormalized-operand 0))))
                      traps
                      :initial-value 0)))
    `(let ((,previous (core:fe-disable-except ,mask)))
       (unwind-protect
            (progn ,@body)
         (core:fe-restore-except ,previous)))))

(defun 1- (num) (- num 1))
(defun 1+ (num) (+ num 1))

;;; KLUDGE: (setf compiler-macro-function) is defined later in compiler-macro.lisp
;;; but we want these compiler macros as early as possible during build, so that they
;;; can be applied for e.g. DO-SUBSEQUENCE. So do it at compile time.
(eval-when (:compile-toplevel))

(defun isqrt (i)
  "Args: (integer)
Returns the integer square root of INTEGER."
  (unless (and (integerp i) (>= i 0))
    (error 'type-error :datum i :expected-type 'unsigned-byte))
  (if (zerop i)
      0
      (let ((n (integer-length i)))
        (do ((x (ash 1 (ceiling n 2))))
            (nil)
          (let ((y (floor i x)))
            (when (<= x y)
              (return x))
            (setq x (floor (+ x y) 2)))))))

(defun phase (x)
  "Args: (number)
Returns the angle part (in radians) of the polar representation of NUMBER.
Returns zero for non-complex numbers."
  (if (zerop x)
    (if (eq x 0) 0.0 (float 0 (realpart x)))
    (atan (imagpart x) (realpart x))))

(defun cis (theta)
  "Args: (theta)
Returns a complex number whose realpart and imagpart are the values of (COS
THETA) and (SIN THETA) respectively."
  (complex (cos theta) (sin theta)))

;; Ported from CMUCL
(defun complex-asin (z)
  (declare (number z))
  (let ((sqrt-1-z (sqrt (- 1 z)))
	(sqrt-1+z (sqrt (+ 1 z))))
    (complex (atan (realpart z) (realpart (* sqrt-1-z sqrt-1+z)))
	     (asinh (imagpart (* (conjugate sqrt-1-z)
				 sqrt-1+z))))))

;; Ported from CMUCL
(defun complex-acos (z)
  (declare (number z))
  (let ((sqrt-1+z (sqrt (+ 1 z)))
	(sqrt-1-z (sqrt (- 1 z))))
    (complex (* 2 (atan (realpart sqrt-1-z) (realpart sqrt-1+z)))
	     (asinh (imagpart (* (conjugate sqrt-1+z)
				 sqrt-1-z))))))

;; Ported from CMUCL
(defun asinh (x)
  "Args: (number)
Returns the hyperbolic arc sine of NUMBER."
  ;(log (+ x (sqrt (+ 1.0 (* x x)))))
  (if (complexp x)
      (let* ((iz (complex (- (imagpart x)) (realpart x)))
	     (result (complex-asin iz)))
	(complex (imagpart result)
		 (- (realpart result))))
      (float (core:num-op-asinh (float x 1l0)) (float x))))

;; Ported from CMUCL
(defun acosh (x)
  "Args: (number)
Returns the hyperbolic arc cosine of NUMBER."
  ;(log (+ x (sqrt (* (1- x) (1+ x)))))
  (if (complexp x)
      (complex-acosh x)
      (let* ((x (float x))
	     (xr (float x 1d0)))
	(declare (double-float xr))
	(if (<= 1l0 xr)
	    (float (core:num-op-acosh xr) x)
	    (complex-acosh x)))))

(defun complex-acosh (z)
  (declare (number z))
  (let ((sqrt-z-1 (sqrt (- z 1)))
	(sqrt-z+1 (sqrt (+ z 1))))
    (complex (asinh (realpart (* (conjugate sqrt-z-1)
				 sqrt-z+1)))
	     (* 2 (atan (imagpart sqrt-z-1) (realpart sqrt-z+1))))))

(defun atanh (x)
  "Args: (number)
Returns the hyperbolic arc tangent of NUMBER."
  ;(/ (- (log (1+ x)) (log (- 1 x))) 2)
  (if (complexp x)
      (complex-atanh x)
      (let* ((x (float x))
	     (xr (float x 1l0)))
	(declare (long-float xr))
	(if (and (<= -1l0 xr) (<= xr 1l0))
	    (float (core:num-op-atanh xr) x)
	    (complex-atanh x)))))

(defun complex-atanh (z)
  (/ (- (log (1+ z)) (log (- 1 z))) 2))

(defun ffloor (x &optional (y 1))
  "Args: (number &optional (divisor 1))
Same as FLOOR, but returns a float as the first value."
  (multiple-value-bind (i r) (floor x y)
    (values (if (floatp r) (float i r) (float i)) r)))

(defun fceiling (x &optional (y 1.0f0))
  "Args: (number &optional (divisor 1))
Same as CEILING, but returns a float as the first value."
  (multiple-value-bind (i r) (ceiling x y)
    (values (if (floatp r) (float i r) (float i)) r)))

(defun ftruncate (x &optional (y 1.0f0))
  "Args: (number &optional (divisor 1))
Same as TRUNCATE, but returns a float as the first value."
  (multiple-value-bind (i r) (truncate x y)
    (values (if (floatp r) (float i r) (float i)) r)))

(defun fround (x &optional (y 1.0f0))
  "Args: (number &optional (divisor 1))
Same as ROUND, but returns a float as the first value."
  (multiple-value-bind (i r) (round x y)
    (values (if (floatp r) (float i r) (float i)) r)))

(defun logtest (x y)
  "Args: (integer1 integer2)
Equivalent to (NOT (ZEROP (LOGAND INTEGER1 INTEGER2)))."
  (not (zerop (logand x y))))

;;; Some compiler macros we want to be available as early as possible.
;;; (setf compiler-macro-function) is not yet defined during load, but the build
;;; can use it no problem.
;;; Load-time definitions are in cmp/opt/opt-number.lisp.
(eval-when (:compile-toplevel)
(define-compiler-macro min (&whole form &rest args)
  (if (null args) ; invalid
      form
      (let ((arg0 (first args)) (args (rest args)))
        (if (null args)
            ;; preserve nontoplevelness and eliminate extra values
            `(core::the-single real ,arg0)
            (let ((s (gensym)))
              `(let ((,s ,arg0)
                     (minrest (min ,@args)))
                 (if (<= ,s minrest) ,s minrest)))))))
(define-compiler-macro max (&whole form &rest args)
  (if (null args) ; invalid
      form
      (let ((arg0 (first args)) (args (rest args)))
        (if (null args)
            `(core::the-single real ,arg0) ; preserve nontoplevelness
            (let ((s (gensym)))
              `(let ((,s ,arg0)
                     (maxrest (max ,@args)))
                 (if (>= ,s maxrest) ,s maxrest)))))))

(define-compiler-macro + (&rest numbers)
  (core:expand-associative '+ 'core:two-arg-+ numbers 0))

(define-compiler-macro * (&rest numbers)
  (core:expand-associative '* 'core:two-arg-* numbers 1))

(define-compiler-macro - (minuend &rest subtrahends)
  (if (proper-list-p subtrahends)
      (if subtrahends
          `(core:two-arg-- ,minuend (+ ,@subtrahends))
          `(core:negate ,minuend))
      (error "The - operator can not be part of a form that is a dotted list.")))

(define-compiler-macro / (dividend &rest divisors)
  (if (proper-list-p divisors)
      (if divisors
          `(core:two-arg-/ ,dividend (* ,@divisors))
          `(core:reciprocal ,dividend))
      (error "The / operator can not be part of a form that is a dotted list.")))

(define-compiler-macro < (&whole form &rest numbers)
  (core:expand-compare form 'core:two-arg-< numbers 'real))

(define-compiler-macro <= (&whole form &rest numbers)
  (core:expand-compare form 'core:two-arg-<= numbers 'real))

(define-compiler-macro > (&whole form &rest numbers)
  (core:expand-compare form 'core:two-arg-> numbers 'real))

(define-compiler-macro >= (&whole form &rest numbers)
  (core:expand-compare form 'core:two-arg->= numbers 'real))

(define-compiler-macro = (&whole form &rest numbers)
  (core:expand-compare form 'core:two-arg-= numbers 'number))

(define-compiler-macro /= (&whole form &rest numbers)
  (core:expand-uncompare form 'core:two-arg-= numbers 'number))

(define-compiler-macro plusp (number) `(> ,number 0))
(define-compiler-macro minusp (number) `(< ,number 0))

(define-compiler-macro 1+ (x)
  `(core:two-arg-+ ,x 1))

(define-compiler-macro 1- (x)
  `(core:two-arg-- ,x 1))

;;; TODO: With integers it might be easier to do log base 2; rewriting as such
;;; would need a more sophisticated transformation.
(define-compiler-macro log (&whole form number &optional (base nil base-p))
  (if base-p
      `(/ (log ,number) (log ,base))
      form))

;;; log* operations
(define-compiler-macro logand (&rest numbers)
  (core:expand-associative 'logand 'core:logand-2op numbers -1))

(define-compiler-macro logxor (&rest numbers)
  (core:expand-associative 'logxor 'core:logxor-2op numbers 0))

(define-compiler-macro logior (&rest numbers)
  (core:expand-associative 'logior 'core:logior-2op numbers 0))

(define-compiler-macro logeqv (&rest numbers)
  (core:expand-associative 'logeqv 'core:logeqv-2op numbers -1))
) ; eval-when

(defun byte (size position)
  "Args: (size position)
Returns a byte specifier of integers.  The value specifies the SIZE-bits byte
starting the least-significant-bit but POSITION bits of integers.  In ECL, a
byte specifier is represented by a dotted pair (SIZE . POSITION)."
  (cons size position))

(defun byte-size (bytespec)
  "Args: (byte)
Returns the size part (in Clasp, the car part) of the byte specifier BYTE."
  (car bytespec))

(defun byte-position (bytespec)
  "Args: (byte)
Returns the position part (in Clasp, the cdr part) of the byte specifier BYTE."
  (cdr bytespec))

(declaim (inline %ldb))
(defun %ldb (size position integer)
  (logand (ash integer (- position))
          (lognot (ash -1 size))))

(defun ldb (bytespec integer)
  "Args: (bytespec integer)
Extracts a byte from INTEGER at the specified byte position, right-justifies
the byte, and returns the result as an integer."
  (%ldb (byte-size bytespec) (byte-position bytespec) integer))

(declaim (inline %ldb-test))
(defun %ldb-test (size position integer)
  (not (zerop (%mask-field size position integer))))

(defun ldb-test (bytespec integer)
  "Args: (bytespec integer)
Returns T if at least one bit of the specified byte is 1; NIL otherwise."
  (%ldb-test (byte-size bytespec) (byte-position bytespec) integer))

(declaim (inline %mask-field))
(defun %mask-field (size position integer)
  (logand (ash (lognot (ash -1 size))
	       position)
	  integer))

(defun mask-field (bytespec integer)
  "Args: (bytespec integer)
Extracts the specified byte from INTEGER and returns the result as an integer."
  (%mask-field (byte-size bytespec) (byte-position bytespec) integer))

(declaim (inline %dpb))
(defun %dpb (newbyte size position integer)
  (let ((mask (ash (lognot (ash -1 size)) position)))
    (logior (logandc2 integer mask)
	    (logand (ash newbyte position) mask))))

(defun dpb (newbyte bytespec integer)
  "Args: (newbyte bytespec integer)
Replaces the specified byte of INTEGER with NEWBYTE (an integer) and returns
the result."
  (%dpb newbyte (byte-size bytespec) (byte-position bytespec) integer))

(declaim (inline %deposit-field))
(defun %deposit-field (newbyte size position integer)
  (let ((mask (ash (lognot (ash -1 size)) position)))
    (logior (logandc2 integer mask)
	    (logand newbyte mask))))

(defun deposit-field (newbyte bytespec integer)
  "Args: (integer1 bytespec integer2)
Returns an integer represented by the bit sequence obtained by replacing the
specified bits of INTEGER2 with the specified bits of INTEGER1."
  (%deposit-field newbyte (byte-size bytespec) (byte-position bytespec) integer))

;;; Look for an explicit (byte ...) form as is usually used in ldb etc.
;;; return NIL if it's not a (byte ...) form.
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun parse-bytespec (bytespec)
  (when (and (consp bytespec)
             (eql (car bytespec) 'byte)
             (consp (cdr bytespec))
             (consp (cddr bytespec))
             (null (cdddr bytespec)))
    (values (cadr bytespec) (caddr bytespec)))))

(eval-when (:compile-toplevel)
(define-compiler-macro ldb-test (&whole whole bytespec integer)
  (multiple-value-bind (size position) (parse-bytespec bytespec)
    (if size
        `(%ldb-test ,size ,position ,integer)
        whole)))

(define-compiler-macro mask-field (&whole whole bytespec integer)
  (multiple-value-bind (size position) (parse-bytespec bytespec)
    (if size
        `(%mask-field ,size ,position ,integer)
        whole)))

(define-compiler-macro dpb (&whole whole newbyte bytespec integer)
  (multiple-value-bind (size position) (parse-bytespec bytespec)
    (if size
        `(%dpb ,newbyte ,size ,position ,integer)
        whole)))

(define-compiler-macro deposit-field (&whole whole newbyte bytespec integer)
  (multiple-value-bind (size position) (parse-bytespec bytespec)
    (if size
        `(%deposit-field ,newbyte ,size ,position ,integer)
        whole))))
