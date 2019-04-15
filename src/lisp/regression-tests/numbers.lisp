(in-package #:clasp-tests)

(test fixnum-double-high
      (loop for i from -500 to 500
            for n = (+ most-positive-fixnum i)
            for rn = (truncate (float n 1d0))
            always (if (<= rn most-positive-fixnum)
                       (typep rn 'fixnum)
                       (typep rn 'bignum))))
(test fixnum-double-low
      (loop for i from -500 to 500
            for n = (+ most-negative-fixnum i)
            for rn = (truncate (float n 1d0))
            always (if (>= rn most-negative-fixnum)
                       (typep rn 'fixnum)
                       (typep rn 'bignum))))
(test fixnum-single-high
      (loop for i from -500 to 500
            for n = (+ most-positive-fixnum i)
            for rn = (truncate (float n 1f0))
            always (if (<= rn most-positive-fixnum)
                       (typep rn 'fixnum)
                       (typep rn 'bignum))))
(test fixnum-single-low
      (loop for i from -500 to 500
            for n = (+ most-negative-fixnum i)
            for rn = (truncate (float n 1f0))
            always (if (<= rn most-negative-fixnum)
                       (typep rn 'fixnum)
                       (typep rn 'bignum))))

(test oddp-1 (oddp -1))
(test oddp-2 (oddp most-positive-fixnum))
(test oddp-3 (oddp (1+ (1+ most-positive-fixnum))))
(test oddp-4 (not (oddp most-negative-fixnum)))
(test oddp-5 (oddp (1- most-negative-fixnum)))

(test random-0 (random 23))
(test-expect-error
 random-1
 (random -1)
 :type type-error)

(test random-2 (random (1+ most-positive-fixnum)))

(test-expect-error
 random-3
 (random (1- most-negative-fixnum ))
 :type type-error)

(test random-4 (random 1.23d0))
(test random-5 (random 1.23s0))
(test random-6 (random 1.23f0))
(test-expect-error random-4a (random -1.23d0) :type type-error)
(test-expect-error random-5a (random -1.23s0) :type type-error)
(test-expect-error random-6a (random -1.23f0) :type type-error)

(test-expect-error random-7 (random 0) :type type-error)
(test-expect-error random-7a (random 0.0s0) :type type-error)
(test-expect-error random-7b (random 0.0d0) :type type-error)
(test-expect-error random-7c (random 0.0f0) :type type-error)

;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_eq_sle.htm
;;; (= 3 3) is true.              (/= 3 3) is false.             
;;; (= 3 5) is false.             (/= 3 5) is true.              
;;; (= 3 3 3 3) is true.          (/= 3 3 3 3) is false.         
;;; (= 3 3 5 3) is false.         (/= 3 3 5 3) is false.         
;;; (= 3 6 5 2) is false.         (/= 3 6 5 2) is true.          
;;; (= 3 2 3) is false.           (/= 3 2 3) is false. 

(test eq-1 (= 3 3))
(test eq-2 (not (= 3 5)))
(test eq-3 (= 3 3 3 3))
(test eq-4 (not (= 3 3 5 3)))
(test eq-5 (not (= 3 6 5 2)))
(test eq-6 (not (= 3 2 3)))

(test neq-1
      (not (/= 3 4 4 5)))
(test neq-2
      (not (/= 3 3)))
(test neq-3
      (/= 3 5))
(test neq-4
      (not (/= 3 3 3 3)))
(test neq-5
      (not (/= 3 3 5 3)))
(test neq-6
      (/= 3 6 5 2))
(test neq-7
      (not (/= 3 2 3)))

(test neq-8
      (let ()
        (/= (complex 1 2)(complex 1 3))))

(test eq-7
      (let ()
        (= (complex 1 2)(complex 1 2))))

(test /=.ORDER.3
      (equal
       '(t 6 1 2 3 4 5 6)
       (multiple-value-list
        (LET ((I 0) U V W X Y Z)
          (VALUES
           (/= (PROGN (SETF U (INCF I)) 1)
               (PROGN (SETF V (INCF I)) 2)
               (PROGN (SETF W (INCF I)) 3)
               (PROGN (SETF X (INCF I)) 4)
               (PROGN (SETF Y (INCF I)) 5)
               (PROGN (SETF Z (INCF I)) 6))
           I
           U
           V
           W
           X
           Y
           Z)))))

(test ratios-1 (equal 2 (+ 1/2 1/2 1/2 1/2)))

(test ratios-2 (equalp (LOOP FOR X FROM 0 TO 5 BY 1/2 COLLECT X)
                       '(0 1/2 1 3/2 2 5/2 3 7/2 4 9/2 5)))

(test ratios-3 (equal 8 8/1))

(test ratios-4 (equal 2  (/ 1/2)))

(test ratios-5
      (eql 0 (Let ((X 1/3))
               (* 0 X))))

;;;; taken from ansi-tests https://gitlab.common-lisp.net/ansi-test/ansi-test.git
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep  7 10:10:10 2003
;;;; Contains: Tests for INTEGER-LENGTH

(test integer-length.1
  (loop for len from 0 to 100
        for i = (1- (ash 1 len))
        for vals = (multiple-value-list (integer-length i))
        for len2 = (car vals)
        always (and (= (length vals) 1)
                    (eql len len2))))

(test integer-length.2
  (loop for len from 0 to 100
        for i = (ash 1 len)
        for vals = (multiple-value-list (integer-length i))
        for len2 = (car vals)
        always (and (= (length vals) 1)
                    (eql (1+ len) len2))))

(test integer-length.3
  (loop for len from 0 to 100
        for i = (- (ash 1 len))
        for vals = (multiple-value-list (integer-length i))
        for len2 = (car vals)
        always (and (= (length vals) 1)
                    (eql len len2))))

(test integer-length.4
  (loop for len from 0 to 100
        for i = (- -1 (ash 1 len))
        for vals = (multiple-value-list (integer-length i))
        for len2 = (car vals)
        always (and (= (length vals) 1)
                    (eql (1+ len) len2))))

;;; boole

(test-expect-error boole-1 (boole nil 1 2) :type type-error)
(test-expect-error boole-2 (boole 1 nil 2) :type type-error)
(test-expect-error boole-3 (boole 1 2 nil) :type type-error)
(test-expect-error boole-4 (boole 1.1 1 2) :type type-error)
(test-expect-error boole-5 (boole -1 1 2) :type type-error)
(test-expect-error boole-6 (boole 42 1 2) :type type-error)
(test-expect-error boole-7 (boole 1 1.1 1) :type type-error)
(test-expect-error boole-8 (boole 1 1 1.1) :type type-error)

;;; floor
;; ratio / same ratio
(test floor-ratio-1
      (multiple-value-bind
            (div rem)
          (floor 1/3 1/3)
        (and (= 1 div)(zerop rem))))

(test floor-ratio-2
      (multiple-value-bind
            (div rem)
          (floor 1/3 1/5)
        (and (= 1 div)(equal rem 2/15))))

(test floor-ratio-3
      (multiple-value-bind
            (div rem)
          (floor 1/3 2)
        (and (zerop div)(equal rem 1/3))))


(test floor-ratio-4
      (multiple-value-bind
            (div rem)
          (floor 2 1/3)
        (and (zerop rem)(= div 6))))

(test floor-ratio-5
      (multiple-value-bind
            (div rem)
          (floor (expt 2 64) 1/3)
        (and (zerop rem)(= div 55340232221128654848))))

(test floor-ratio-6
      (multiple-value-bind
            (div rem)
          (floor 1/3 (expt 2 64))
        (and (zerop div)(= rem 1/3))))

(test floor-ratio-float-1
      (multiple-value-bind
            (div rem)
          (floor 1/2 1.0d0)
        (and (zerop div)(floatp rem))))

;;; ceiling
(test ceiling-ratio-1  ;;;error rem == 0/1
      (multiple-value-bind
            (div rem)
          (ceiling 1/3 1/3)
        (and (typep rem 'fixnum)(zerop rem)(typep div 'fixnum)(= div 1))))

;;; (FLOOR (- -2305843009213693952) -2305843009213693952) should be -1 0, is 1 0
;;; same for ceiling and truncate

(test floor-7
      (multiple-value-bind
            (div rem)
          (FLOOR 2305843009213693952 -2305843009213693952)
        (and (zerop rem)(= div -1))))

(test floor-ceiling-2
      (multiple-value-bind
            (div rem)
          (ceiling 2305843009213693952 -2305843009213693952)
        (and (zerop rem)(= div -1))))

(test floor-truncate-1
      (multiple-value-bind
            (div rem)
          (truncate 2305843009213693952 -2305843009213693952)
        (and (zerop rem)(= div -1))))

(test floor-truncate-2
      (multiple-value-bind
            (div rem)
          (round 2305843009213693952 -2305843009213693952)
        (and (zerop rem)(= div -1))))

;;; the above 4 fail, since (- -2305843009213693952) fails, put 2305843009213693952 instead

(test negate-most-negative-fixnum-1
      (plusp (- -2305843009213693952)))

(test reciprocal-1
      (typep (/ -1) 'fixnum))


(test-expect-error reciprocal-0 (/ 0) :type DIVISION-BY-ZERO)
(test-expect-error reciprocal-0-1  (/ 1 0) :type DIVISION-BY-ZERO)
(test-expect-error reciprocal-2 (/ 17 10 0 11) :type DIVISION-BY-ZERO)

(test abs-1 (plusp (ABS -2305843009213693952)))

(test complex-1 (equal (complex 0 2) (1- (complex 1 2))))

(test make-complex-0
      (and (= 2 (complex 2 0))
           (typep (complex 2 0) 'fixnum)))

(test expt-complex-1
      (and (= -4 (EXPT (complex 0 2) 2))
           (typep  (expt (complex 0 2) 2) 'fixnum)))


(test times-complex-2
      (and (= -4 (* (complex 0 2) (complex 0 2)))
           (typep  (* (complex 0 2) (complex 0 2)) 'fixnum)))

(test complex-float-1 (typep (complex pi) 'complex))
(test complex-rational-1 (typep (complex 1/3 0) 'ratio))
;;; (complex a-float 0) -> imnagpart has floattype of a-float
(test complex-float-3 (eql (complex 3.0 0.0)(complex 3.0)))
(test complex-float-4 (eql (complex 3.0d0 0.0d0)(complex 3.0d0)))
;;; if on of real or imag is float, both will be float
(test complex-float-5 (eql (complex 3.0 4.0)(complex 3 4.0)))
(test complex-float-6 (eql (complex 3.0 4.0)(complex 3.0 4)))
(test complex-float-7 (eql (complex 3.0d0 4.0d0)(complex 3 4.0d0)))
(test complex-float-8 (eql (complex 3.0d0 4.0d0)(complex 3.0d0 4)))
;;; real and imagpart must be of the same float type, if both are floats
(test complex-float-9
      (let ((c (complex 3.0 3.0d0)))
        (eql (type-of (imagpart c)) (type-of (realpart c)))))
(test complex-float-10
      (let ((c (complex 3.0 3.0l0)))
        (eql (type-of (imagpart c)) (type-of (realpart c)))))
(test complex-float-11
      (let ((c (complex 3.0 3.0)))
        (eql (type-of (imagpart c)) (type-of (realpart c)))))
(test complex-float-12
      (let ((c (complex 3.0d0  3.0 )))
        (eql (type-of (imagpart c)) (type-of (realpart c)))))
(test complex-float-13
      (let ((c (complex 3.0l0 3.0 )))
        (eql (type-of (imagpart c)) (type-of (realpart c)))))

(test ash-1 (= -1 (ash -4294967296 -4294967296)))

(test logbitp-1 (LOGBITP MOST-POSITIVE-FIXNUM -1))
(test logbitp-2 (null (LOGBITP (1+ MOST-POSITIVE-FIXNUM) 0)))
(test-expect-error logbitp-3
 (LOGBITP -1 0)
 :type type-error)
(test-expect-error logbitp-4
 (LOGBITP (1- most-negative-fixnum) 0)
 :type type-error)
(test logbitp-5 (null (LOGBITP 37 0)))
(test logbitp-6 (null (LOGBITP 37 (1+ MOST-POSITIVE-FIXNUM))))


;;GCD
(test gcd-1 (typep (gcd 0 1) 'fixnum))
(test gcd-2 (plusp (gcd -1 -2147483648)))
(test gcd-3 (plusp (gcd most-positive-fixnum most-positive-fixnum)))
(test gcd-4 (plusp (gcd most-positive-fixnum most-negative-fixnum)))
(test gcd-5 (plusp (gcd (1+ most-positive-fixnum) most-positive-fixnum)))
(test gcd-6 (plusp (gcd most-positive-fixnum (1- most-negative-fixnum))))


;;; suprinsingy same tests fail when compile-filed when using #c(x y),
;;; but no when loaded
;;; this is (mis)behaving of #C(x Y)
;;; this does not work, when compile-filed since a is #C(0.0d0 2.0d0)
;;; bug is in ltvc_make_complex, which only works for doubles
(test ltv-complex-0
      (let ((a #C(0 2))
            (b (complex 0 2)))
        (print `(should ,b))
        (print `(is ,a))
        (equal a b)))

(test ltv-complex-1
      (let ((a #C(0.0 2.0))
            (b (complex 0.0 2.0)))
        (print `(should ,b))
        (print `(is ,a))
        (equal a b)))

(test ltv-complex-2
      (let ((a #C(0.0d0 2.0d0))
            (b (complex 0.0d0 2.0d0)))
        (print `(should ,b))
        (print `(is ,a))
        (equal a b)))

(test ltv-complex-3
      (let ((a #C(1/2 1/3))
            (b (complex 1/2 1/3)))
        (print `(should ,b))
        (print `(is ,a))
        (equal a b)))

(test ltv-complex-4
      (let ((a #C(23058430092136939510 230584300921369395100))
            (b (complex 23058430092136939510 230584300921369395100)))
        (print `(should ,b))
        (print `(is ,a))
        (equal a b)))

;;; infinities
(test infinity-1 (ext:float-infinity-p ext:short-float-positive-infinity))
(test infinity-2 (ext:float-infinity-p ext:short-float-negative-infinity))
(test infinity-3 (ext:float-infinity-p ext:single-float-positive-infinity))
(test infinity-4 (ext:float-infinity-p ext:single-float-negative-infinity))
(test infinity-5 (ext:float-infinity-p ext:double-float-positive-infinity))
(test infinity-6 (ext:float-infinity-p ext:double-float-negative-infinity))
(test infinity-7 (ext:float-infinity-p ext:long-float-positive-infinity))
(test infinity-8 (ext:float-infinity-p ext:long-float-negative-infinity))
(test infinity-9 (ext:float-infinity-p (+ most-positive-long-float most-positive-long-float)))

;;; nan
(test nan-1 (ext:float-nan-p (/ 0s0 0s0)))
(test nan-2 (ext:float-nan-p (/ 0.0 0.0)))
(test nan-3 (ext:float-nan-p (/ 0d0 0d0)))
(test nan-4 (ext:float-nan-p (/ 0l0 0l0)))


(test signum-1 (= -1 (signum most-negative-fixnum)))
(test signum-1a (= -1 (signum (1- most-negative-fixnum))))
(test signum-2 (= 1 (signum most-positive-fixnum)))
(test signum-2a (= 1 (signum (1+ most-positive-fixnum))))

(test sqrt-big-ratio-1 
      (let ((result 
             (SQRT 28022395738783732117648967388274923619871355234097921/122167958641777737216225939000892255646232346624)))
        (and (typep result 'float)(not (ext:float-nan-p result)))))

(test sqrt-bignum-should-fit-in-double-1
      (let ((result 
             (sqrt 28022395738783732117648967388274923619871355234097921)))
        (and (typep result 'float)(not (ext:float-nan-p result))(not (ext:float-infinity-p result)))))

(test sqrt-bignum-should-fit-in-double-2
      (let ((result 
             (sqrt 2802239573878373211764896738827492361987135523409792123423423468273647283642783467283643456837465347653487563847658346587346523847687324678236487234687234627834687234678236478237687623423426862843627834623846782346234239479283472934798237498273423467823642342342837468723467283462348762378462342347862344998)))
        (and (typep result 'float)(not (ext:float-nan-p result))(not (ext:float-infinity-p result)))))

(test sqrt-bignum-should-fit-in-double-overflow
      (let ((result 
             (sqrt 280223957387837321176489673882749236198713552340979212342342346827364728364278346728364345683746534765348756384765834658734652384768732467823648723468723462783468723467823647823768762342342686284362783462384678234623423947928347293479823749827342346782364234234283746872346728346234876237846234234786234499889)))
        (and (typep result 'float)(not (ext:float-nan-p result))(ext:float-infinity-p result))))

;;; the following all have &rest numbers+ in the definition, so need at least 1 argument
(test-expect-error number-compare-1 (=) :type program-error)
(test-expect-error number-compare-2 (/=) :type program-error)
(test-expect-error number-compare-3 (<) :type program-error)
(test-expect-error number-compare-4 (>) :type program-error)
(test-expect-error number-compare-5 (<=) :type program-error)
(test-expect-error number-compare-6 (>=) :type program-error)

(test-expect-error max-1 (max) :type program-error)
(test-expect-error max-2 (locally (declare (notinline max))(max #c(1 2))) :type type-error)
(test-expect-error max-2a (max #c(1 2)) :type type-error)
(test max-3 (max 36 (1+ (integer-length most-positive-fixnum))))

(test-expect-error min-1 (min) :type program-error)
(test-expect-error min-2 (locally (declare (notinline min))(min #c(1 2))) :type type-error)
(test-expect-error min-2a (min #c(1 2)) :type type-error)
(test min-3 (min 36 (1+ (integer-length most-positive-fixnum))))

;;; <, >, <=, >=: take reals, =, /= numbers
(test-expect-error number-comparison-real-1 (locally (declare (notinline <)) (< #c(1 2))) :type type-error)
(test-expect-error number-comparison-real-2 (locally (declare (notinline >))(> #c(1 2))) :type type-error)
(test-expect-error number-comparison-real-3 (locally (declare (notinline <=))(<= #c(1 2))) :type type-error)
(test-expect-error number-comparison-real-4 (locally (declare (notinline >=))(>= #c(1 2))) :type type-error)
(test-expect-error number-comparison-real-7 (locally (declare (notinline =))(= (make-hash-table))) :type type-error)
(test-expect-error number-comparison-real-8 (locally (declare (notinline /=))(/= (make-hash-table))) :type type-error)
(test-expect-error number-comparison-real-9 (locally (declare (notinline =))(= "jd")) :type type-error)
(test-expect-error number-comparison-real-10 (locally (declare (notinline /=))(/= "jd")) :type type-error)
(test-expect-error number-comparison-number-11 (locally (declare (notinline <))(< "jd")) :type type-error)
(test-expect-error number-comparison-number-12 (locally (declare (notinline >))(> "jd")) :type type-error)
(test-expect-error number-comparison-number-13 (locally (declare (notinline <=))(<= "jd")) :type type-error)
(test-expect-error number-comparison-number-14 (locally (declare (notinline >=))(>= "jd")) :type type-error)

;;; and with compiler macros
(test-expect-error number-comparison-real-1a (let ()(< #c(1 2))) :type type-error)
(test-expect-error number-comparison-real-2a (let ()(> #c(1 2))) :type type-error)
(test-expect-error number-comparison-real-3a (let ()(<= #c(1 2))) :type type-error)
(test-expect-error number-comparison-real-4a (let ()(>= #c(1 2))) :type type-error)
(test-expect-error number-comparison-real-7a (let ()(= (make-hash-table))) :type type-error)
(test-expect-error number-comparison-real-8a (let ()(/= (make-hash-table))) :type type-error)
(test-expect-error number-comparison-real-9a (let ()(= "jd")) :type type-error)
(test-expect-error number-comparison-real-10a (let ()(/= "jd")) :type type-error)
(test-expect-error number-comparison-number-11a (let ()(< "jd")) :type type-error)
(test-expect-error number-comparison-number-12a (let ()(> "jd")) :type type-error)
(test-expect-error number-comparison-number-13a (let ()(<= "jd")) :type type-error)
(test-expect-error number-comparison-number-14a (let ()(>= "jd")) :type type-error)

