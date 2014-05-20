
(defconstant +max-diff+ 1024)

(defun cast-int (x)
  (cond
    ((floatp x) (sys::cast-to-integer x))
    ((integerp x) x)
    (t (error "Handle cast-int for ~a" x))))

(defun value-compare (val cmp)
  (cond
    ((and (complexp val) (complexp cmp))
     (let* ((itr (cast-int (realpart val)))
	    (iti (cast-int (imagpart val)))
	    (ivr (cast-int (realpart cmp)))
	    (ivi (cast-int (imagpart cmp)))
	    (diff-r (abs (- itr ivr)))
	    (diff-i (abs (- iti ivi))))
       (list 'cmp-complex-complex diff-r diff-i)))
    ((and (sys:ratio-p val) (sys:ratio-p cmp))
     (list 'cmp-ratio-ratio (- (numerator val) (numerator cmp)) (- (denomenator val) (denomenator cmp))))
    (t (list 'cmp-vals (abs (- (cast-int val) (cast-int cmp)))))))

#||
(defun value-compare (v c)
  (/ (- v c) v))
||#

(defun values-compare (vals cmps)
  (list* 'cmp-values (mapcar #'(lambda (v c) (value-compare v c)) vals cmps)))


(defmacro nt (exp cmp)
  (let ((eexp (gensym))
	(ecmp (gensym)))
    `(let ((,eexp (multiple-value-list ,exp))
	   (,ecmp (multiple-value-list ,cmp)))
       (format t "    ~A  -> ~a~%" ',exp (values-compare ,eexp ,ecmp)))))

(nt (log 10.0) 2.3025850929940460)


(nt (sys::one-plus most-positive-fixnum) (+ 1 most-positive-fixnum))

(nt (sys::one-plus 1) 2)

(nt (sys::one-minus most-negative-fixnum) (- most-negative-fixnum 1))
(nt (sys::one-minus 10) 9)

(nt (sys::one-plus #c(2.0 10.0)) #c(3.0 10.0))
(nt (sys::one-minus #c(7.0 10.0)) #c(6.0 10.0))
(nt (sys::one-plus 8/5) 13/5)
(nt (sys::one-minus 8/5) 3/5)


;; (floor x y) with x Fixnum and y Bignum

(nt (floor 1232 238423489234829384923842938492384928349234) (values 0 1232))
(nt (floor 3 2) (values 1 1))
(nt (ceiling 3 2) (values 2 -1))
(nt (round 5 6) (values 1 -1))
(nt (round 348234823482348234 3) (values 116078274494116078 0 ))


