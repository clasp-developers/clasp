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

#.
(flet ((binary-search (f min max)
	 (do ((new (/ (+ min max) 2) (/ (+ min max) 2)))
	     ((>= min max)
	      max)
	   (if (funcall f new)
	       (if (= new max)
		   (return max)
		   (setq max new))
	       (if (= new min)
		   (return max)
		   (setq min new)))))
       (epsilon+ (x)
	 (/= (float 1 x) (+ (float 1 x) x)))
       (epsilon- (x)
	 (/= (float 1 x) (- (float 1 x) x))))
  `(eval-when (compile load eval)
    (defconstant short-float-epsilon
      ,(binary-search #'epsilon+ (coerce 0 'short-float) (coerce 1 'short-float))
      "The smallest postive short-float E that satisfies
	(not (= (float 1 E) (+ (float 1 E) E)))")
    (defconstant single-float-epsilon
      ,(binary-search #'epsilon+ (coerce 0 'single-float) (coerce 1 'single-float))
      "The smallest postive single-float E that satisfies
	(not (= (float 1 E) (+ (float 1 E) E)))")
    (defconstant double-float-epsilon
      ,(binary-search #'epsilon+ (coerce 0 'double-float) (coerce 1 'double-float))
      "The smallest postive double-float E that satisfies
	(not (= (float 1 E) (+ (float 1 E) E)))")
    (defconstant long-float-epsilon
      ,(binary-search #'epsilon+ (coerce 0 'long-float) (coerce 1 'long-float))
      "The smallest postive long-float E that satisfies
	(not (= (float 1 E) (+ (float 1 E) E)))")
    (defconstant short-float-negative-epsilon
      ,(binary-search #'epsilon- (coerce 0 'short-float) (coerce 1 'short-float))
      "The smallest positive short-float E that satisfies
	(not (= (float 1 E) (- (float 1 E) E)))")
    (defconstant single-float-negative-epsilon
      ,(binary-search #'epsilon- (coerce 0 'single-float) (coerce 1 'single-float))
      "The smallest positive single-float E that satisfies
	(not (= (float 1 E) (- (float 1 E) E)))")
    (defconstant double-float-negative-epsilon
      ,(binary-search #'epsilon- (coerce 0 'double-float) (coerce 1 'double-float))
      "The smallest positive double-float E that satisfies
	(not (= (float 1 E) (- (float 1 E) E)))")
    (defconstant long-float-negative-epsilon
      ,(binary-search #'epsilon- (coerce 0 'long-float) (coerce 1 'long-float))
      "The smallest positive long-float E that satisfies
	(not (= (float 1 E) (- (float 1 E) E)))")
    ))

#+ieee-floating-point
(locally (declare (notinline -))
  (let ((bits (si::trap-fpe 'last nil)))
    (unwind-protect
         (progn
           (let ((a (/ (coerce 1 'short-float) (coerce 0.0 'short-float))))
             (defconstant short-float-positive-infinity a)
             (defconstant short-float-negative-infinity (- a)))
           (let ((a (/ (coerce 1 'single-float) (coerce 0.0 'single-float))))
             (defconstant single-float-positive-infinity a)
             (defconstant single-float-negative-infinity (- a)))
           (let ((a (/ (coerce 1 'double-float) (coerce 0.0 'double-float))))
             (defconstant double-float-positive-infinity a)
             (defconstant double-float-negative-infinity (- a)))
           (let ((a (/ (coerce 1 'long-float) (coerce 0.0 'long-float))))
             (defconstant long-float-positive-infinity a)
             (defconstant long-float-negative-infinity (- a))))
      (si::trap-fpe bits t))))

(defconstant imag-one #C(0.0 1.0))

(defun isqrt (i)
  "Args: (integer)
Returns the integer square root of INTEGER."
       (unless (and (integerp i) (>= i 0))
               (error 'type-error :datum i :expected-type 'unsigned-byte))
       (if (zerop i)
           0
           (let ((n (integer-length i)))
                (do ((x (ash 1 (ceiling n 2)))
                     (y))
                    (nil)
                    (setq y (floor i x))
                    (when (<= x y)
                          (return x))
                    (setq x (floor (+ x y) 2))))))

(defun phase (x)
  "Args: (number)
Returns the angle part (in radians) of the polar representation of NUMBER.
Returns zero for non-complex numbers."
  (if (zerop x)
    (if (eq x 0) 0.0 (float 0 (realpart x)))
    (atan (imagpart x) (realpart x))))

(defun signum (x)
  "Args: (number)
Returns a number that represents the sign of NUMBER.  Returns NUMBER If it is
zero.  Otherwise, returns the value of (/ NUMBER (ABS NUMBER))"
  (if (zerop x) x (/ x (abs x))))

(defun cis (x)
  "Args: (radians)
Returns a complex number whose realpart and imagpart are the values of (COS
RADIANS) and (SIN RADIANS) respectively."
  (exp (* imag-one x)))

(defun asin (x)
  "Args: (number)
Returns the arc sine of NUMBER."
  (if #+clasp-min t #-clasp-min (complexp x)
      (complex-asin x)
      #-clasp-min
      (let* ((x (float x))
	     (xr (float x 1l0)))
	(declare (long-float xr))
	(if (and (<= -1.0 xr) (<= xr 1.0))
	    (float (core:num-op-asin xr) x)
	    (complex-asin x)))))

;; Ported from CMUCL
(defun complex-asin (z)
  (declare (number z))
  (let ((sqrt-1-z (sqrt (- 1 z)))
	(sqrt-1+z (sqrt (+ 1 z))))
    (complex (atan (realpart z) (realpart (* sqrt-1-z sqrt-1+z)))
	     (asinh (imagpart (* (conjugate sqrt-1-z)
				 sqrt-1+z))))))

(defun acos (x)
  "Args: (number)
Returns the arc cosine of NUMBER."
  (if #+clasp-min t #-clasp-min (complexp x)
      (complex-acos x)
      #-clasp-min
      (let* ((x (float x))
	     (xr (float x 1l0)))
	(declare (long-float xr))
	(if (and (<= -1.0 xr) (<= xr 1.0))
	    (float (core:num-op-acos xr) (float x))
	    (complex-acos x)))))

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
  (if #+clasp-min t #-clasp-min (complexp x)
      (let* ((iz (complex (- (imagpart x)) (realpart x)))
	     (result (complex-asin iz)))
	(complex (imagpart result)
		 (- (realpart result))))
      #-clasp-min
      (float (core:num-op-asinh x) (float x))))

;; Ported from CMUCL
(defun acosh (x)
  "Args: (number)
Returns the hyperbolic arc cosine of NUMBER."
  ;(log (+ x (sqrt (* (1- x) (1+ x)))))
  (if #+clasp-min t #-clasp-min (complexp x)
      (complex-acosh x)
      #-clasp-min
      (let* ((x (float x))
	     (xr (float x 1d0)))
	(declare (double-float xr))
	(if (<= 1.0 xr)
	    (float (core:num-op-acosh xr) (float x))
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
  (if #+clasp-min t #-clasp-min (complexp x)
      (complex-atanh x)
      #-clasp-min
      (let* ((x (float x))
	     (xr (float x 1d0)))
	(declare (double-float xr))
	(if (and (<= -1.0 xr) (<= xr 1.0))
	    (float (core:num-op-atanh xr) (float x))
	    (complex-atanh x)))))

(defun complex-atanh (z)
  (declare (number z))
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


(defun byte (size position)
  "Args: (size position)
Returns a byte specifier of integers.  The value specifies the SIZE-bits byte
starting the least-significant-bit but POSITION bits of integers.  In ECL, a
byte specifier is represented by a dotted pair (SIZE . POSITION)."
  (cons size position))

(defun byte-size (bytespec)
  "Args: (byte)
Returns the size part (in ECL, the car part) of the byte specifier BYTE."
  (car bytespec))

(defun byte-position (bytespec)
  "Args: (byte)
Returns the position part (in ECL, the cdr part) of the byte specifier BYTE."
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

(defun parse-bytespec (bytespec)
  (when (and (consp bytespec)
             (eql (car bytespec) 'byte)
             (consp (cdr bytespec))
             (consp (cddr bytespec))
             (null (cdddr bytespec)))
    (values (cadr bytespec) (caddr bytespec))))

(core:bclasp-define-compiler-macro ldb (&whole whole bytespec integer)
  (multiple-value-bind (size position) (parse-bytespec bytespec)
    (if size
        `(%ldb ,size ,position ,integer)
        whole)))

(declaim (inline %ldb-test))
(defun %ldb-test (size position integer)
  (not (zerop (%mask-field size position integer))))

(defun ldb-test (bytespec integer)
  "Args: (bytespec integer)
Returns T if at least one bit of the specified byte is 1; NIL otherwise."
  (%ldb-test (byte-size bytespec) (byte-position bytespec) integer))

(core:bclasp-define-compiler-macro ldb-test (&whole whole bytespec integer)
  (multiple-value-bind (size position) (parse-bytespec bytespec)
    (if size
        `(%ldb-test ,size ,position ,integer)
        whole)))

(declaim (inline %mask-field))
(defun %mask-field (size position integer)
  (logand (ash (lognot (ash -1 size))
	       position)
	  integer))

(defun mask-field (bytespec integer)
  "Args: (bytespec integer)
Extracts the specified byte from INTEGER and returns the result as an integer."
  (%mask-field (byte-size bytespec) (byte-position bytespec) integer))

(core:bclasp-define-compiler-macro mask-field (&whole whole bytespec integer)
  (multiple-value-bind (size position) (parse-bytespec bytespec)
    (if size
        `(%mask-field ,size ,position ,integer)
        whole)))

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

(core:bclasp-define-compiler-macro dpb (&whole whole newbyte bytespec integer)
  (multiple-value-bind (size position) (parse-bytespec bytespec)
    (if size
        `(%dpb ,newbyte ,size ,position ,integer)
        whole)))

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

(core:bclasp-define-compiler-macro deposit-field (&whole whole newbyte bytespec integer)
  (multiple-value-bind (size position) (parse-bytespec bytespec)
    (if size
        `(%deposit-field ,newbyte ,size ,position ,integer)
        whole)))
