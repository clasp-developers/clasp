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

