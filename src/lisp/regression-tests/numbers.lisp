(in-package #:clasp-tests)

(test-true fixnum-double-high
      (loop for i from -500 to 500
            for n = (+ most-positive-fixnum i)
            for rn = (truncate (float n 1d0))
            always (if (<= rn most-positive-fixnum)
                       (typep rn 'fixnum)
                       (typep rn 'bignum))))
(test-true fixnum-double-low
      (loop for i from -500 to 500
            for n = (+ most-negative-fixnum i)
            for rn = (truncate (float n 1d0))
            always (if (>= rn most-negative-fixnum)
                       (typep rn 'fixnum)
                       (typep rn 'bignum))))
(test-true fixnum-single-high
      (loop for i from -500 to 500
            for n = (+ most-positive-fixnum i)
            for rn = (truncate (float n 1f0))
            always (if (<= rn most-positive-fixnum)
                       (typep rn 'fixnum)
                       (typep rn 'bignum))))
(test-true fixnum-single-low
      (loop for i from -500 to 500
            for n = (+ most-negative-fixnum i)
            for rn = (truncate (float n 1f0))
            always (if (<= rn most-negative-fixnum)
                       (typep rn 'fixnum)
                       (typep rn 'bignum))))

(test-true oddp-1 (oddp -1))
(test-true oddp-2 (oddp most-positive-fixnum))
(test-true oddp-3 (oddp (1+ (1+ most-positive-fixnum))))
(test-true oddp-4 (not (oddp most-negative-fixnum)))
(test-true oddp-5 (oddp (1- most-negative-fixnum)))

(test-true random-0 (random 23))
(test-expect-error
 random-1
 (random -1)
 :type type-error)

(test-true random-2 (random (1+ most-positive-fixnum)))

(test-expect-error
 random-3
 (random (1- most-negative-fixnum ))
 :type type-error)

(test-true random-4 (random 1.23d0))
(test-true random-5 (random 1.23s0))
(test-true random-6 (random 1.23f0))
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

(test-true eq-1 (= 3 3))
(test-true eq-2 (not (= 3 5)))
(test-true eq-3 (= 3 3 3 3))
(test-true eq-4 (not (= 3 3 5 3)))
(test-true eq-5 (not (= 3 6 5 2)))
(test-true eq-6 (not (= 3 2 3)))

(test-true neq-1 (not (/= 3 4 4 5)))
(test-true neq-2 (not (/= 3 3)))
(test-true neq-3 (/= 3 5))
(test-true neq-4 (not (/= 3 3 3 3)))
(test-true neq-5 (not (/= 3 3 5 3)))
(test-true neq-6 (/= 3 6 5 2))
(test-true neq-7 (not (/= 3 2 3)))

(test-true neq-8 (let () (/= (complex 1 2) (complex 1 3))))
(test-true eq-7 (let () (= (complex 1 2) (complex 1 2))))

(test /=.ORDER.3
      (LET ((I 0) U V W X Y Z)
        (VALUES (/= (PROGN (SETF U (INCF I)) 1)
                    (PROGN (SETF V (INCF I)) 2)
                    (PROGN (SETF W (INCF I)) 3)
                    (PROGN (SETF X (INCF I)) 4)
                    (PROGN (SETF Y (INCF I)) 5)
                    (PROGN (SETF Z (INCF I)) 6))
                I U V W X Y Z))
      (t 6 1 2 3 4 5 6))

(test ratios-1 (+ 1/2 1/2 1/2 1/2) (2))

(test ratios-2 (LOOP FOR X FROM 0 TO 5 BY 1/2 COLLECT X)
      ((0 1/2 1 3/2 2 5/2 3 7/2 4 9/2 5)))

(test ratios-3 8/1 (8))

(test ratios-4 (/ 1/2) (2))

(test ratios-5 (let ((x 1/3)) (* 0 x)) (0))

;;;; taken from ansi-tests https://gitlab.common-lisp.net/ansi-test/ansi-test.git
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep  7 10:10:10 2003
;;;; Contains: Tests for INTEGER-LENGTH

(test-true integer-length.1
  (loop for len from 0 to 100
        for i = (1- (ash 1 len))
        for vals = (multiple-value-list (integer-length i))
        for len2 = (car vals)
        always (and (= (length vals) 1)
                    (eql len len2))))

(test-true integer-length.2
  (loop for len from 0 to 100
        for i = (ash 1 len)
        for vals = (multiple-value-list (integer-length i))
        for len2 = (car vals)
        always (and (= (length vals) 1)
                    (eql (1+ len) len2))))

(test-true integer-length.3
  (loop for len from 0 to 100
        for i = (- (ash 1 len))
        for vals = (multiple-value-list (integer-length i))
        for len2 = (car vals)
        always (and (= (length vals) 1)
                    (eql len len2))))

(test-true integer-length.4
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
(test floor-ratio-1 (floor 1/3 1/3) (1 0))
(test floor-ratio-2 (floor 1/3 1/5) (1 2/15))
(test floor-ratio-3 (floor 1/3 2) (0 1/3))
(test floor-ratio-4 (floor 2 1/3) (6 0))
(test floor-ratio-5 (floor (expt 2 64) 1/3) (55340232221128654848 0))
(test floor-ratio-6 (floor 1/3 (expt 2 64)) (0 1/3))

(test-true floor-ratio-float-1
           (multiple-value-bind
                 (div rem)
               (floor 1/2 1.0d0)
             (and (zerop div)(floatp rem))))

;;; ceiling
(test-true ceiling-ratio-1  ;;;error rem == 0/1
      (multiple-value-bind
            (div rem)
          (ceiling 1/3 1/3)
        (and (typep rem 'fixnum)(zerop rem)(typep div 'fixnum)(= div 1))))

;;; (FLOOR (- -2305843009213693952) -2305843009213693952) should be -1 0, is 1 0
;;; same for ceiling and truncate

(test floor-7 (FLOOR 2305843009213693952 -2305843009213693952) (-1 0))

(test floor-ceiling-2 (ceiling 2305843009213693952 -2305843009213693952) (-1 0))

(test floor-truncate-1 (truncate 2305843009213693952 -2305843009213693952)
      (-1 0))
(test floor-truncate-2 (round 2305843009213693952 -2305843009213693952) (-1 0))

;;; the above 4 fail, since (- -2305843009213693952) fails, put 2305843009213693952 instead

;;; See bugs #399, #1205
(test floor-mpf-1 (floor 2.305843e18) (2305843009213693952 0e0))

;;; The standard doesn't specify the types here beyond "float", but we want
;;; to be using the usual contagion rules.
(test floor-remainder-contagion
      (loop for fun in '(floor ceiling truncate round
                         ffloor fceiling ftruncate fround)
            nconc
            (loop for args in '((3f0 2) (3 2f0) (3f0 4/7) (31/30 2f0)
                                (3f0 2d0) (3d0 2f0) (3d0 2d0))
                  for rtype in '(single-float single-float single-float single-float
                                 double-float double-float double-float)
                  unless (typep (nth-value 1 (apply fun args)) rtype)
                    collect (cons fun args)))
      (nil))
;; The standard's description here is just weird, but again, we go with contagion.
;; And in this case also that given two rationals we should get a single.
(test ffloor-quotient-contagion
      (loop for fun in '(ffloor fceiling ftruncate fround)
            nconc
            (loop for args in '((3 2) (3 4/7) (31/30 2)
                                (3f0 2) (3 2f0) (3f0 4/7) (31/30 2f0)
                                (3f0 2d0) (3d0 2f0) (3d0 2d0))
                  for rtype in '(single-float single-float single-float
                                 single-float single-float single-float single-float
                                 double-float double-float double-float)
                  unless (typep (apply fun args) rtype)
                    collect (cons fun args)))
      (nil))

(test-true negate-most-negative-fixnum-1 (plusp (- -2305843009213693952)))

(test-type reciprocal-1 (/ -1) fixnum)

(test-expect-error reciprocal-0 (/ 0) :type DIVISION-BY-ZERO)
(test-expect-error reciprocal-0-1  (/ 1 0) :type DIVISION-BY-ZERO)
(test-expect-error reciprocal-2 (/ 17 10 0 11) :type DIVISION-BY-ZERO)

(test-true abs-1 (plusp (ABS -2305843009213693952)))

(test complex-1 (1- (complex 1 2)) (#c(0 2)))

(test-true make-complex-0
      (and (= 2 (complex 2 0))
           (typep (complex 2 0) 'fixnum)))

(test-true expt-complex-1
      (and (= -4 (EXPT (complex 0 2) 2))
           (typep  (expt (complex 0 2) 2) 'fixnum)))


(test-true times-complex-2
      (and (= -4 (* (complex 0 2) (complex 0 2)))
           (typep  (* (complex 0 2) (complex 0 2)) 'fixnum)))

(test-type complex-float-1 (complex pi) complex)
(test-type complex-rational-1 (complex 1/3 0) ratio)
;;; (complex a-float 0) -> imnagpart has floattype of a-float
(test-true complex-float-3 (eql (complex 3.0 0.0)(complex 3.0)))
(test-true complex-float-4 (eql (complex 3.0d0 0.0d0)(complex 3.0d0)))
;;; if on of real or imag is float, both will be float
(test-true complex-float-5 (eql (complex 3.0 4.0)(complex 3 4.0)))
(test-true complex-float-6 (eql (complex 3.0 4.0)(complex 3.0 4)))
(test-true complex-float-7 (eql (complex 3.0d0 4.0d0)(complex 3 4.0d0)))
(test-true complex-float-8 (eql (complex 3.0d0 4.0d0)(complex 3.0d0 4)))
;;; real and imagpart must be of the same float type, if both are floats
(test-true complex-float-9
           (let ((c (complex 3.0 3.0d0)))
             (eql (type-of (imagpart c)) (type-of (realpart c)))))
(test-true complex-float-10
           (let ((c (complex 3.0 3.0l0)))
             (eql (type-of (imagpart c)) (type-of (realpart c)))))
(test-true complex-float-11
           (let ((c (complex 3.0 3.0)))
             (eql (type-of (imagpart c)) (type-of (realpart c)))))
(test-true complex-float-12
           (let ((c (complex 3.0d0  3.0 )))
             (eql (type-of (imagpart c)) (type-of (realpart c)))))
(test-true complex-float-13
           (let ((c (complex 3.0l0 3.0 )))
             (eql (type-of (imagpart c)) (type-of (realpart c)))))

(test ash-1 (ash -4294967296 -4294967296) (-1))

(test-true logbitp-1 (LOGBITP MOST-POSITIVE-FIXNUM -1))
(test logbitp-2 (LOGBITP (1+ MOST-POSITIVE-FIXNUM) 0) (nil))
(test-expect-error logbitp-3
 (LOGBITP -1 0)
 :type type-error)
(test-expect-error logbitp-4
 (LOGBITP (1- most-negative-fixnum) 0)
 :type type-error)
(test logbitp-5 (LOGBITP 37 0) (nil))
(test logbitp-6 (LOGBITP 37 (1+ MOST-POSITIVE-FIXNUM)) (nil))


;;GCD
(test-type gcd-1 (gcd 0 1) fixnum)
(test-true gcd-2 (plusp (gcd -1 -2147483648)))
(test-true gcd-3 (plusp (gcd most-positive-fixnum most-positive-fixnum)))
(test-true gcd-4 (plusp (gcd most-positive-fixnum most-negative-fixnum)))
(test-true gcd-5 (plusp (gcd (1+ most-positive-fixnum) most-positive-fixnum)))
(test-true gcd-6 (plusp (gcd most-positive-fixnum (1- most-negative-fixnum))))


;;; suprinsingy someme tests have failed when compile-filed when using #c(x y),
;;; but no when loaded
;;; this is (mis)behaving of #C(x Y)
(test-true ltv-complex-0
      (let ((a #C(0 2))
            (b (complex 0 2)))
        (equal a b)))

(test-true ltv-complex-1
      (let ((a #C(0.0 2.0))
            (b (complex 0.0 2.0)))
        (equal a b)))

(test-true ltv-complex-2
      (let ((a #C(0.0d0 2.0d0))
            (b (complex 0.0d0 2.0d0)))
        (equal a b)))

(test-true ltv-complex-3
      (let ((a #C(1/2 1/3))
            (b (complex 1/2 1/3)))
        (equal a b)))

(test-true ltv-complex-4
      (let ((a #C(23058430092136939510 230584300921369395100))
            (b (complex 23058430092136939510 230584300921369395100)))
        (equal a b)))

;;; infinities
(test-true infinity-1 (ext:float-infinity-p ext:short-float-positive-infinity))
(test-true infinity-2 (ext:float-infinity-p ext:short-float-negative-infinity))
(test-true infinity-3 (ext:float-infinity-p ext:single-float-positive-infinity))
(test-true infinity-4 (ext:float-infinity-p ext:single-float-negative-infinity))
(test-true infinity-5 (ext:float-infinity-p ext:double-float-positive-infinity))
(test-true infinity-6 (ext:float-infinity-p ext:double-float-negative-infinity))
(test-true infinity-7 (ext:float-infinity-p ext:long-float-positive-infinity))
(test-true infinity-8 (ext:float-infinity-p ext:long-float-negative-infinity))
(test-true infinity-9 (ext:with-float-traps-masked (:overflow)
                        (ext:float-infinity-p (+ most-positive-long-float
                                                 most-positive-long-float))))

;;; nan
(test-true nan-1 (ext:with-float-traps-masked (:invalid)
                   (ext:float-nan-p (/ 0s0 0s0))))
(test-true nan-2 (ext:with-float-traps-masked (:invalid)
                   (ext:float-nan-p (/ 0.0 0.0))))
(test-true nan-3 (ext:with-float-traps-masked (:invalid)
                   (ext:float-nan-p (/ 0d0 0d0))))
(test-true nan-4 (ext:with-float-traps-masked (:invalid)
                   (ext:float-nan-p (/ 0l0 0l0))))


(test signum-1 (signum most-negative-fixnum) (-1))
(test signum-1a (signum (1- most-negative-fixnum)) (-1))
(test signum-2 (signum most-positive-fixnum) (1))
(test signum-2a (signum (1+ most-positive-fixnum)) (1))
(test signum-3 (SIGNUM (COMPLEX 3/5 4/5)) (#c(0.6 0.8)))

(test-true sqrt-big-ratio-1 
  (ext:with-float-traps-masked (:invalid :overflow :underflow :divide-by-zero)
    (let ((result
            (SQRT 28022395738783732117648967388274923619871355234097921/122167958641777737216225939000892255646232346624)))
      (and (typep result 'float)(not (ext:float-nan-p result))))))

(test-true sqrt-bignum-should-fit-in-single-float-1
  (ext:with-float-traps-masked (:invalid :overflow :underflow :divide-by-zero)
    (let ((result
            (sqrt 28022395738783732117648967388274923619871355234097921)))
      (and (typep result 'float)
           (not (ext:float-nan-p result))
           (not (ext:float-infinity-p result))))))

(test-true sqrt-bignum-does-not-fit-in-single-float-overflow-1
  (ext:with-float-traps-masked (:invalid :overflow :underflow :divide-by-zero)
    (let ((result
            (sqrt 2802239573878373211764896738827492361987135523409792123423423468273647283642783467283643456837465347653487563847658346587346523847687324678236487234687234627834687234678236478237687623423426862843627834623846782346234239479283472934798237498273423467823642342342837468723467283462348762378462342347862344998)))
      (and (typep result 'float)
           (ext:float-infinity-p result)))))

(test-true sqrt-bignum-should-fit-in-single-float-overflow-2
  (ext:with-float-traps-masked (:invalid :overflow :underflow :divide-by-zero)
    (let ((result
            (sqrt 280223957387837321176489673882749236198713552340979212342342346827364728364278346728364345683746534765348756384765834658734652384768732467823648723468723462783468723467823647823768762342342686284362783462384678234623423947928347293479823749827342346782364234234283746872346728346234876237846234234786234499889)))
      (and (typep result 'float)
           (ext:float-infinity-p result)))))

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
(test-true max-3 (max 36 (1+ (integer-length most-positive-fixnum))))

(test-expect-error min-1 (min) :type program-error)
(test-expect-error min-2 (locally (declare (notinline min))(min #c(1 2))) :type type-error)
(test-expect-error min-2a (min #c(1 2)) :type type-error)
(test-true min-3 (min 36 (1+ (integer-length most-positive-fixnum))))

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

(test complex-reciprocal (/ #c(1 1)) (#c(1/2 -1/2)))
(test-expect-error number-unary-operations-1-inline (let ()(+ "jd")) :type type-error)
(test-expect-error number-unary-operations-1-notinline (locally
                                                           (declare (notinline +))
                                                         (+ "jd")) :type type-error)

;;; 12.2.62 logand, logandc1, logandc2, logeqv, logior, lognand, lognor, lognot, logorc1, logorc2, logxor
(test log-functions-1 (logior 1 2 4 8) (15))
(test log-functions-2 (logxor 1 3 7 15) (10))
(test log-functions-3 (logeqv) (-1))
(test log-functions-4 (logand 16 31) (16))
(test log-functions-5 (lognot 0) (-1))
(test log-functions-5a (lognot 1) (-2))
(test log-functions-5b (lognot (1+ (lognot 1000))) (999))

;;; borrowed from ansi-tests
(test logand.1 (logand) (-1))
(test logand.2 (logand 1231) (1231))
(test logand.3 (logand -198) (-198))
(test logand.order.2.simple (logand #b11011 #b10110 #b110101) (#b10000))
(test-true logand.3a
      (= (logand (1+ most-positive-fixnum)) (1+ most-positive-fixnum)))
(test-true logand.3b
           (= (logand (1+ most-negative-fixnum)) (1+ most-negative-fixnum)))
(test-true logand.3c
           (= (logand (1+ most-positive-fixnum)(1+ (1+ most-positive-fixnum))) (1+ most-positive-fixnum)))
(test-true logand.3d
           (= (logand (1- most-negative-fixnum)(1- (1- most-negative-fixnum))) (- most-negative-fixnum 2)))
(test logand.5.simple
      (let ((x 33300140732146818380750772381422989832214186835186851059977249))
        (logand x (lognot x)))
      (0))

(test logandc1.1 (logandc1 0 0) (0))
(test logandc1.2 (logandc1 0 -1) (-1))
(test logandc1.3 (logandc1 0 123) (123))
(test-true logandc1.4.simple
      (let ((x 33300140732146818380750772381422989832214186835186851059977249))
        (and (eql x (logandc1 0 x))
             (eql 0 (logandc1 x x))
             (eql x (logandc1 (lognot x) x))
             (eql (lognot x) (logandc1 x (lognot x))))))

(test logandc2.1 (logandc2 0 0) (0))
(test logandc2.2 (logandc2 -1 0) (-1))
(test-true logandc2.3 (= (1+ most-positive-fixnum) (logandc2 (1+ most-positive-fixnum) 0)))
(test-true logandc2.4.simple
      (let ((x 33300140732146818380750772381422989832214186835186851059977249))
        (and (eql x (logandc2 x 0))
                    (eql 0 (logandc2 x x))
                    (eql x (logandc2 x (lognot x)))
                    (eql (lognot x) (logandc2 (lognot x) x)))))

(test logeqv.1 (logeqv) (-1))
(test logeqv.2 (logeqv 1231) (1231))
(test logeqv.3 (logeqv -198) (-198))
(test logeqv.3a (logeqv  #b11011  #b10110 #b110101) (#b111000))
(test LOGEQV.5.simple (logeqv 2305843009213693953 (LOGNOT 2305843009213693953))
      (0))
(test LOGEQV.5.simple.inline (let () (logeqv 2305843009213693953 (LOGNOT 2305843009213693953))) (0))

(test logior.1 (logior) (0))
(test logior.2 (logior 1231) (1231))
(test logior.3 (logior -198) (-198))
(test logior.3a (logior #b10011 #b10110  #b110101) (#b110111))
(test logior.5.simple
      (let ((x 33300140732146818380750772381422989832214186835186851059977249))
        (logior x (lognot x)))
      (-1))

(test lognand.1 (lognand 0 0) (-1))
(test lognand.2 (lognand 0 -1) (-1))
(test lognand.3 (lognand -1 123) (-124))
(test lognand.3a (lognand -2 -3) (3))
(test-true lognand.4.simple
      (let ((x 33300140732146818380750772381422989832214186835186851059977249))
        (and (eql -1 (lognand 0 x))
                    (eql (lognot x) (lognand x x))
                    (eql -1 (lognand (lognot x) x))
                    (eql -1 (lognand x (lognot x))))))

(test lognor.1 (lognor 0 0) (-1))
(test lognor.2 (lognor 0 -1) (0))
(test lognor.3 (lognor -1 123) (0))
(test lognor.3a  (lognor -2 -3) (0))
(test-true lognor.4.simple
      (let ((x 33300140732146818380750772381422989832214186835186851059977249))
        (and (eql (lognot x) (lognor 0 x))
                    (eql (lognot x) (lognor x x))
                    (eql 0 (lognor (lognot x) x))
                    (eql 0 (lognor x (lognot x))))))

(test logorc1.1 (logorc1 0 0) (-1))
(test logorc1.2  (logorc1 0 -1) (-1))
(test logorc1.3 (logorc1 -1 0) (0))
(test logorc1.4 (logorc1 123 0) (-124))
(test logorc1.4a (logorc1 -3 19) (19))
(test-true logorc1.4.simple
      (let ((x 33300140732146818380750772381422989832214186835186851059977249))
        (and (eql -1 (logorc1 0 x))
                    (eql x (logorc1 -1 x))
                    (eql -1 (logorc1 x x))
                    (eql x (logorc1 (lognot x) x))
                    (eql (lognot x) (logorc1 x (lognot x))))))

(test logorc2.1 (logorc2 0 0) (-1))
(test logorc2.2 (logorc2 -1 0) (-1))
(test logorc2.3 (logorc2 0 -1) (0))
(test logorc2.4 (logorc2 0 123) (-124))
(test logorc2.4a (logorc2 27 -1) (27))
(test-true logorc2.4.simple
      (let ((x 33300140732146818380750772381422989832214186835186851059977249))
        (and (eql -1 (logorc2 x 0))
                    (eql x (logorc2 x -1))
                    (eql -1 (logorc2 x x))
                    (eql x (logorc2 x (lognot x)))
                    (eql (lognot x) (logorc2 (lognot x) x)))))

(test logxor.1 (logxor) (0))
(test logxor.2 (logxor 1231) (1231))
(test logxor.3 (logxor -198) (-198))
(test logxor.3a (logxor #b11011 #b10110 #b110101) (#b111000))
(test-true logxor.4.simple
      (let ((x 33300140732146818380750772381422989832214186835186851059977249))
        (eql x (logxor x))))
(test-true logxor.5.simple
      (let ((x 33300140732146818380750772381422989832214186835186851059977249))
        (and (eql -1 (logxor x (lognot x)))
                    (eql 0 (logxor x x))
                    (eql x (logxor x x x)))))

(test-true log-bignum
      (labels ((factorial (n) (if (<= n 1) 1 (* n (factorial (1- n))))))
        (let ((result (log (factorial 200)))) 
          (and (floatp result)
               (not (ext:float-nan-p result))))))

(test-true load-log-bignum
           (labels ((factorial (n) (if (<= n 1) 1 (* n (factorial (1- n))))))
             (loop for x from 1 to 500
                   for res = (log (factorial x))
                   always (and (floatp res) (not (ext:float-nan-p res))))))

(test-true big-ratio-coerce
      (let ((result
              (COERCE 6795704571147613088400718678381656244393117734249898937417419069310191575883868986428455446312916068568665979830007649804543533991572608932250835738151489076845330821569858726908084574701882988127547528934585469826755385222874837210418773111903651670205662000421193529634355863606092768847694362480173879256904596515653328461457501880112334566400744016842783001773321772818609367618076742443023275354232092047563787771643659430278130974461062642384241926963624924919867161137264969829092223075246982819340445538562498073940878705520674737984170082956322173496241162764552348099573721983300906273607793253095492671040351/2500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 'SINGLE-FLOAT)))
        (and (floatp result)
             (not (ext:float-nan-p result)))))
             
;;; cannot be run yet, because of https://github.com/clasp-developers/clasp/issues/961        
#+(or)
(progn
  (test-expect-error
   integer_decode_float_infinity_short_positive
   (integer-decode-float #.ext:short-float-positive-infinity)
   :type arithmetic-error)

  (test-expect-error
   integer_decode_float_infinity_single_positive
   (integer-decode-float #.ext:single-float-positive-infinity)
   :type arithmetic-error)

  (test-expect-error
   integer_decode_float_infinity_double_positive
   (integer-decode-float #.ext:double-float-positive-infinity)
   :type arithmetic-error)

  (test-expect-error
   integer_decode_float_infinity_long_positive
   (integer-decode-float #.ext:long-float-positive-infinity)
   :type arithmetic-error)

  (test-expect-error
   integer_decode_float_infinity_short_negative
   (integer-decode-float #.ext:short-float-negative-infinity)
   :type arithmetic-error)

  (test-expect-error
   integer_decode_float_infinity_single_negative
   (integer-decode-float #.ext:single-float-negative-infinity)
   :type arithmetic-error)

  (test-expect-error
   integer_decode_float_infinity_double_negative
   (integer-decode-float #.ext:double-float-negative-infinity)
   :type arithmetic-error)

  (test-expect-error
   integer_decode_float_infinity_long_negative
   (integer-decode-float #.ext:long-float-negative-infinity)
   :type arithmetic-error)
  )

(test-type ratio-error-from-#clasp (/ 1/3) fixnum)

;;; seen in mcclim gadget test
(test-true all-div-mult-plus-minus-combinations
      (let ((operands
              (list 23 4611686018427387904 (/ 3 2) 1.23d0 1.23 #c(2 3)))
            (results nil)
            (print nil)
            (compare nil)
            (results-from-sbcl
              (list 0 #C(4 6) #C(-5 12) 1 #C(0.77 3.0) #C(3.23 3.0) #C(2.46 3.69)
                 #C(1.6260163 2.4390244) #C(0.77d0 3.0d0) #C(3.23d0 3.0d0) #C(2.46d0 3.69d0)
                 #C(1.6260162601626016d0 2.4390243902439024d0) #C(1/2 3) #C(7/2 3) #C(3 9/2)
                 #C(4/3 2) #C(-4611686018427387902 3) #C(4611686018427387906 3)
                 #C(9223372036854775808 13835058055282163712)
                 #C(1/2305843009213693952 3/4611686018427387904) #C(-21 3) #C(25 3) #C(46 69)
                 #C(2/23 3/23) #C(-0.77 -3.0) #C(3.23 3.0) #C(2.46 3.69)
                 #C(0.18923077 -0.28384614) 0.0 2.46 1.5129 1.0 1.907348634588857d-8
                 2.4600000190734863d0 1.5129000234603882d0 1.0000000155068995d0 -0.26999998
                 2.73 1.845 0.82 -4.611686e18 4.611686e18 5.672374e18 2.6671374e-19 -21.77
                 24.23 28.29 0.053478263 #C(-0.77d0 -3.0d0) #C(3.23d0 3.0d0) #C(2.46d0 3.69d0)
                 #C(0.18923076923076923d0 -0.28384615384615386d0) -1.907348634588857d-8
                 2.4600000190734863d0 1.5129000234603882d0 0.9999999844931008d0 0.0d0 2.46d0
                 1.5129d0 1.0d0 -0.27d0 2.73d0 1.845d0 0.82d0 -4.611686018427388d18
                 4.611686018427388d18 5.672373802665687d18 2.667137344314341d-19 -21.77d0
                 24.23d0 28.29d0 0.05347826086956522d0 #C(-1/2 -3) #C(7/2 3) #C(3 9/2)
                 #C(3/13 -9/26) 0.26999998 2.73 1.845 1.2195122 0.27d0 2.73d0 1.845d0
                 1.2195121951219512d0 0 3 9/4 1 -9223372036854775805/2 9223372036854775811/2
                 6917529027641081856 3/9223372036854775808 -43/2 49/2 69/2 3/46
                 #C(4611686018427387902 -3) #C(4611686018427387906 3)
                 #C(9223372036854775808 13835058055282163712)
                 #C(9223372036854775808/13 -13835058055282163712/13) 4.611686e18 4.611686e18
                 5.672374e18 3.7493382e18 4.611686018427388d18 4.611686018427388d18
                 5.672373802665687d18 3.74933822636373d18 9223372036854775805/2
                 9223372036854775811/2 6917529027641081856 9223372036854775808/3 0
                 9223372036854775808 21267647932558653966460912964485513216 1
                 4611686018427387881 4611686018427387927 106068778423829921792
                 4611686018427387904/23 #C(21 -3) #C(25 3) #C(46 69) #C(46/13 -69/13) 21.77
                 24.23 28.29 18.699186 21.77d0 24.23d0 28.29d0 18.69918699186992d0 43/2 49/2
                 69/2 46/3 -4611686018427387881 4611686018427387927 106068778423829921792
                 23/4611686018427387904 0 46 529 1))
            (no-error-p t))
        (dolist (o1 operands)
          (dolist (o2 operands)
            (handler-case
                (progn
                  (when print (format t "Div result ~a with ~a and ~a~2%" (/ o1 o2) o1 o2))
                  (push (/ o1 o2) results))
              (error (e)
                (setq no-error-p nil)
                (format t "Div error ~a with ~a and ~a~2%" e o1 o2)))
            (handler-case
                (progn
                  (when print (format t "Mult result ~a with ~a and ~a~2%" (* o1 o2) o1 o2))
                  (push (* o1 o2) results))
              (error (e)
                (setq no-error-p nil)
                (format t "Mult error ~a with ~a and ~a~2%" e o1 o2)))
            (handler-case
                (progn
                  (when print (format t "Plus result ~a with ~a and ~a~2%" (+ o1 o2) o1 o2))
                  (push (+ o1 o2) results))
              (error (e)
                (setq no-error-p nil)
                (format t "Plus error ~a with ~a and ~a~2%" e o1 o2)))
            (handler-case
                (progn
                  (when print (format t "Minus result ~a with ~a and ~a~2%" (- o1 o2) o1 o2))
                  (push (- o1 o2)  results))
              (error (e)
                (setq no-error-p nil)
                (format t "Minus error ~a with ~a and ~a~2%" e o1 o2)))))
        (when compare
          (loop for x in results for y in results-from-sbcl
              unless (equal x y)
              do (format t "Diff ~a with ~a~2%" x y)))
        (values no-error-p)))

(test-true float-constants-1
      (<=  least-positive-double-float least-positive-normalized-double-float
           least-positive-single-float least-positive-normalized-single-float))

(test-true float-constants-2
      (<=  least-positive-long-float least-positive-normalized-long-float
           least-positive-short-float least-positive-normalized-short-float))

(test-true float-constants-3
      (<= least-positive-long-float least-positive-double-float
          least-positive-single-float least-positive-short-float))

(test-true float-constants-4
      (<= least-positive-normalized-long-float least-positive-normalized-double-float
          least-positive-normalized-single-float least-positive-normalized-short-float))

(test-true float-constants-1-negative
      (>=  least-negative-double-float least-negative-normalized-double-float
           least-negative-single-float least-negative-normalized-single-float))

(test-true float-constants-2-negative
      (>=  least-negative-long-float least-negative-normalized-long-float
           least-negative-short-float least-negative-normalized-short-float))

(test-true float-constants-3-negative
      (>= least-negative-long-float  least-negative-double-float
          least-negative-single-float least-negative-short-float))

(test-true float-constants-4-negative
      (>= least-negative-normalized-long-float  least-negative-normalized-double-float
          least-negative-normalized-single-float least-negative-normalized-short-float))

(test-true float-constants-5
      (every #'plusp
             (list least-positive-short-float least-positive-normalized-short-float
                   least-positive-single-float least-positive-normalized-single-float
                   least-positive-double-float least-positive-normalized-double-float
                   least-positive-long-float least-positive-normalized-double-float)))

(test-true float-constants-5-negative
      (every #'minusp
             (list least-negative-short-float least-negative-normalized-short-float
                   least-negative-single-float least-negative-normalized-single-float
                   least-negative-double-float least-negative-normalized-double-float
                   least-negative-long-float least-negative-normalized-long-float)))

(test-true random-short
      (zerop (random least-positive-short-float)))

(test-true random-single
      (zerop (random least-positive-single-float)))

(test-true random-double
      (zerop (random least-positive-double-float)))

(test-true random-long
           (zerop (random least-positive-long-float)))

(defun abs-fixnum (obj)
  (declare  (fixnum obj))
  (abs obj))

(test-true issue-1257
           (typep (abs-fixnum 23) 'fixnum))

(test-true issue-1258
           (numberp
            (funcall 
             (lambda (end)
               (declare (fixnum end))
               (let* ((integer-factor (floor 1.05d0)))
                 (declare (fixnum integer-factor))
                 (the fixnum (* integer-factor end))))
             13)))
