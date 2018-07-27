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
