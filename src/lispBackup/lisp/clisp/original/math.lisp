#|#########################################################################
# Copyright (C) 1997-2011 Ufasoft                                         #
# http://ufasoft.com   mailto:support@ufasoft.com                         #
#                                                                         #
# This program is free software; you can redistribute it and/or modify it #
# under the terms of the GNU General Public License as published by the   #
# Free Software Foundation; either version 3, or (at your option) any     #
# later version.                                                          #
#                                                                         #
# This program is distributed in the hope that it will be useful,         #
# but WITHOUT ANY WARRANTY; without even the implied warranty of          #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           #
# GNU General Public License for more details.                            #
#                                                                         #
# You should have received a copy of the GNU General Public License       #
# along with this program; If not, see <http://www.gnu.org/licenses/>     #
########################################################################=|#

(in-package "SYS")

(defconstant SHORT-FLOAT-EPSILON  SINGLE-FLOAT-EPSILON)
(defconstant DOUBLE-FLOAT-EPSILON SINGLE-FLOAT-EPSILON)
(defconstant LONG-FLOAT-EPSILON   SINGLE-FLOAT-EPSILON)
(defconstant SHORT-FLOAT-NEGATIVE-EPSILON SINGLE-FLOAT-NEGATIVE-EPSILON)
(defconstant DOUBLE-FLOAT-NEGATIVE-EPSILON SINGLE-FLOAT-NEGATIVE-EPSILON)
(defconstant LONG-FLOAT-NEGATIVE-EPSILON SINGLE-FLOAT-NEGATIVE-EPSILON)
(defconstant MOST-POSITIVE-SHORT-FLOAT MOST-POSITIVE-SINGLE-FLOAT)
(defconstant MOST-POSITIVE-DOUBLE-FLOAT MOST-POSITIVE-SINGLE-FLOAT)
(defconstant MOST-POSITIVE-LONG-FLOAT MOST-POSITIVE-SINGLE-FLOAT)
(defconstant LEAST-POSITIVE-SHORT-FLOAT LEAST-POSITIVE-SINGLE-FLOAT)
(defconstant LEAST-POSITIVE-DOUBLE-FLOAT LEAST-POSITIVE-SINGLE-FLOAT)
(defconstant LEAST-POSITIVE-LONG-FLOAT LEAST-POSITIVE-SINGLE-FLOAT)
(defconstant LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT)
(defconstant LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT)
(defconstant LEAST-POSITIVE-NORMALIZED-LONG-FLOAT LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT)
(defconstant MOST-NEGATIVE-SHORT-FLOAT MOST-NEGATIVE-SINGLE-FLOAT)
(defconstant MOST-NEGATIVE-DOUBLE-FLOAT MOST-NEGATIVE-SINGLE-FLOAT)
(defconstant MOST-NEGATIVE-LONG-FLOAT MOST-NEGATIVE-SINGLE-FLOAT)
(defconstant LEAST-NEGATIVE-SHORT-FLOAT LEAST-NEGATIVE-SINGLE-FLOAT)
(defconstant LEAST-NEGATIVE-DOUBLE-FLOAT LEAST-NEGATIVE-SINGLE-FLOAT)
(defconstant LEAST-NEGATIVE-LONG-FLOAT LEAST-NEGATIVE-SINGLE-FLOAT)
(defconstant LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT)
(defconstant LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT)
(defconstant LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT)   

(defconstant pi 3.141592653589793)

(defun float-radix (x)
	(declare (ignore x))
	2)

(defun float-precision (x)
  (multiple-value-bind (m e s)
      (integer-decode-float x)
    (integer-length m)))
	
(defun _check-floors (n d)
  (if (or (not (realp n))
          (not (realp d))
          (zerop d))
    (err)))

#|!!!R    
(defun _truncate (n d)
  (_check-floors n d)
  (cond ((and (rationalp n) (rationalp d))
           (let ((q (truncate (* (numerator n) (denominator d))
                              (* (numerator d) (denominator n)))))
             (values q (- n (* q d)))))
        (t (truncate (float n) (float d)))))
|#
#|!!!   
(defun _floor (n d)
  (_check-floors n d)
  (let ((i (_floor1 (/ n d))))
     (values i (- n (* i d)))))
|#
    
(defun floor (n &optional (d 1))
  (multiple-value-bind (q r) (truncate n d)
    (if (or (zerop r)
            (if (minusp d) (minusp n) (plusp n)))
      (values q r)
      (values (1- q) (+ r d)))))      

(defun ceiling (n &optional (d 1))
  (multiple-value-bind (q r) (truncate n d)
    (if (or (zerop r)
            (if (minusp d) (plusp n) (minusp n)))
      (values q r)
      (values (1+ q) (- r d)))))

  								
(macrolet ((frob (name op)
	           `(defun ,name (n &optional (d 1))
								(multiple-value-bind (q r) (,op n d)
									(values (float q) r)))))
  (frob ffloor floor)
  (frob fceiling ceiling)
  (frob ftruncate truncate)
  (frob fround round))
  								
     
(defun mod (n d)
  (nth-value 1 (floor n d)))

(defun rem (n d)
  (nth-value 1 (truncate n d)))

(defun oddp (a)
  (not (evenp a)))


(defun _mul-signum (x s)
  (if (minusp s) (- x) x))
      

(defun round (n &optional (d 1))
  (multiple-value-bind (q r) (truncate n d)
    (let ((k (abs (/ d 2))))
			(cond ((or (< (abs r) k) (and (= (abs r) k) (evenp q)))
			        (values q r))
			      (t (values (+ q (_mul-signum (_mul-signum 1 n) d))
			                 (if (plusp r) (- r (abs d))
			                               (+ r (abs d)))))))))
  
(defun expt (b p)
  (cond ((zerop p) (coerce 1 (type-of b)))
        ((= p 1) b)
        ((integerp p)
          (if (minusp p)
            (/ (expt b (- p)))
            (let ((r (expt (* b b) (ash p -1))))
              (if (evenp p) r (* r b)))))
        ((zerop b) (cond ((plusp (realpart p)) 0)
                         ((error "Return" "Invalid argument"))))
        ((exp (* p (log b))))))


(defun sqrt (x)
  (cond ((complexp x) (exp (/ (log x) 2)))
        ((minusp x) (complex 0 (sqrt (- x))))
        ((_sqrt (float x)))))

;;; Newton's method
(defun isqrt (n)
  (check-type n (integer 0 *))
  (do* ((x (ash 1 (ash (integer-length n) -1)))
         (d (truncate n x) (truncate n x)))
        ((prog1 (< -2 (- x d) 2)
                (setq x (ash (+ x d) -1)))
         x)))


(defun sin (z)
  (if (complexp z) (/ (- (exp (* z #C(0 1))) (exp (* z #C(0 -1)))) #C(0 2))
                   (_sin (float z))))
(defun cos (z)
  (if (complexp z) (/ (+ (exp (* z #C(0 1))) (exp (* z #C(0 -1)))) 2)
                   (_cos (float z))))

(defun tan (z)
  (/ (sin z) (cos z)))
  
(defun asin (z)
  (if (and (realp z) (<= (abs z) 1))
    (_asin (float z))
  	(* #C(0 -1) (log (* #C(0 1) (+ z (sqrt (- (* z z) 1))))))))

(defun acos (z)
  (if (and (realp z) (<= (abs z) 1))
    (_acos (float z))
  (* #C(0 -1) (log (- z (sqrt (- (* z z) 1)))))))

(defun atan (x &optional y)
  (cond (y (_atan2 (float x) (float y)))
        ((realp x) (_atan (float x)))
        (* #C(0 -1/2) (log (/ (- #C(0 1) x) (+ #C(0 1) x))))))

(defun sinh (z)
  (/ (- (exp z) (exp (- z))) 2))

(defun cosh (z)
  (/ (+ (exp z) (exp (- z))) 2))

(defun tanh (z)
  (/ (sinh z) (cosh z)))

(defun asinh (z)
  (log (+ z (sqrt (1+ (* z z))))))

(defun acosh (z)
  (* 2 (log (+ (sqrt (/ (1+ z) 2))
               (sqrt (/ (1- z) 2))))))

(defun atanh (z)
  (/ (log (/ (1+ z) (- 1 z))) 2))

(defun cis (radians)
  (check-type radians real)
  (complex (cos radians) (sin radians)))

(defun exp (z)
  (cond ((complexp z) (* (exp (realpart z)) (cis (imagpart z))))
        ((_exp (float z)))))

(defun log (z &optional (b nil b-p))
  (if b-p
     (/ (log z) (log b))
	 (cond
       ((or (complexp z) (minusp z))
			 (complex (log (abs z))
                      (phase z)))
        ((_log (float z))))))



(defun lcm (&rest r)
  (if r (let ((a (abs (car r)))
              (d (cdr r)))
          (if d (let ((b (abs (car d))))
                  (if (or (zerop a) (zerop b))
                    0
                    (apply #'lcm (/ (* a b) (gcd a b)) (cdr d))))
                a))
        1))
        

(defun _rational-rationalize (x f)
  (if (rationalp x)
    x
    (multiple-value-bind (m e s) (integer-decode-float x)
      (_mul-signum (if (minusp e)
                     (funcall f m (ash 1 (- e)))
     				 (ash m e))
                   s))))
        

(defun rational (x)
  (_rational-rationalize x #'(lambda (m ee) (/ m ee))))

(defun _fraction (a b)              ;  find ratio with smallest denominator between a & b
  (let ((c (ceiling a)))
    (if (< c b)
      c
      (let ((k (1- c)))
        (+ k (/ 1 (_fraction (/ 1 (- b k))
                             (/ 1 (- a k)))))))))


(defun rationalize (x)
  (_rational-rationalize x #'(lambda (m ee)
                               (_fraction (/ (- m 1/2) ee)
                                          (/ (+ m 1/2) ee)))))

#|!!!Hardcoded
(defun logcount (x)
	(if (not (integerp x)) (err))
	(do* ((a (if (minusp x) (lognot x) x) (ash a -1))
	      (sum (logand a 1) (+ sum (logand a 1))))	
	    ((zerop a) sum)
	  ))
|#
	  	  

(defconstant boole-clr   0)
(defconstant boole-nor   1)
(defconstant boole-andc2 2)
(defconstant boole-c2    3)
(defconstant boole-andc1 4)
(defconstant boole-c1    5)
(defconstant boole-xor   6)
(defconstant boole-nand  7)
(defconstant boole-and   8)
(defconstant boole-eqv   9)
(defconstant boole-1     10)
(defconstant boole-orc2  11)
(defconstant boole-2     12)
(defconstant boole-orc1  13)
(defconstant boole-ior   14)
(defconstant boole-set   15)

(defvar _*boole-funs*
  #(#'(lambda (x y) 0)
    #'lognor
    #'logandc2
	#'(lambda (x y) (lognot y))
	#'logandc1
	#'(lambda (x y) (lognot x))
	#'logxor
	#'lognand
	#'logand
	#'logeqv
	#'(lambda (x y) x)
	#'logorc2
	#'(lambda (x y) y)
	#'logorc1
	#'logior
	#'(lambda (x y) -1)
  ))

(defun boole (op x y)
  (check-type op (integer 0 15))
  (funcall (svref _*boole-funs* op) x y))
