(in-package #:clasp-tests)

(test-expect-error subseq-oob1 (subseq #(1 2 3) 1 5))
(test-expect-error subseq-oob2 (subseq '(1 2 3) 0 5))
(test subseq0 (subseq '(1 2 3 4 5) 0 3) ((1 2 3)))

(test last-1 (last (list 1 2 3 4) most-positive-fixnum) ((1 2 3 4)))
(test last-2 (last (list 1 2 3 4) (1+ most-positive-fixnum)) ((1 2 3 4)))
(test last-3 (last (list 1 2 3 4) 0) (nil))
(TEST-EXPECT-ERROR last-4 (last (list 1 2 3 4) -1) :type type-error)
(TEST-EXPECT-ERROR last-5 (last (list 1 2 3 4) most-negative-fixnum) :type type-error)
(TEST-EXPECT-ERROR last-6 (last (list 1 2 3 4) (1- most-negative-fixnum)) :type type-error)

(test butlast-1 (butlast (list 1 2 3 4 5 6 7 8 9 10) (1+ most-positive-fixnum))
      (nil))
(test nbutlast-1 (nbutlast (list 1 2 3 4 5 6 7 8 9 10) (1+ most-positive-fixnum))
      (nil))

(TEST-EXPECT-ERROR butlast-2 (butlast (list 1 2 3 4 5 6 7 8 9 10) -5) :type type-error)
(TEST-EXPECT-ERROR nbutlast-2 (nbutlast (list 1 2 3 4 5 6 7 8 9 10) -5) :type type-error)

(test-expect-error nth-1
		   (nth -1 (list 1 2 3))
		   :type type-error)

(test nth-2 (nth most-positive-fixnum (list 1 2 3)) (nil))
(test nth-3 (nth (1+  most-positive-fixnum) (list 1 2 3)) (nil))

(test-expect-error nthcdr-1
		   (nthcdr -1 (list 1 2 3))
		   :type type-error)

(test nthcdr-2 (nthcdr most-positive-fixnum (list 1 2 3)) (nil))
(test nthcdr-3 (nthcdr (1+  most-positive-fixnum) (list 1 2 3)) (nil))

(test write-sequence-1a
      (with-output-to-string (blah)
        (write-sequence "1223" blah :start 0 :end 2))
      ("12"))

(test write-sequence-1b
      (with-output-to-string (blah)
        (write-sequence (list #\1 #\2 #\3 #\4) blah :start 0 :end 2))
      ("12"))

(test write-sequence-1c
      (with-output-to-string (blah)
        (write-sequence (vector #\1 #\2 #\3 #\4) blah :start 0 :end 2))
      ("12"))

(test write-sequence-2
      (with-output-to-string (blah)
        (gray:stream-write-sequence blah "1223"))
      ("1223"))

(test-expect-error write-sequence-3
                   (WRITE-SEQUENCE "ABCDEFGH" *STANDARD-OUTPUT* :END -1)
                    :type type-error)

(test-expect-error write-sequence-4
                   (WRITE-SEQUENCE "ABCDEFGH" *STANDARD-OUTPUT* :END 1 :start 2)
                   :type type-error)

(test write-sequence-5
      (with-output-to-string (*STANDARD-OUTPUT*)
        (WRITE-SEQUENCE "ABCDEFGH" *STANDARD-OUTPUT* :END 1 :start 1))
      (""))

(test read-sequence-1
      (let ((seq (make-string 10 :initial-element #\space)))
	(with-input-from-string (blah "127182187261762")
	  (read-sequence seq blah :end 2))
	seq)
      ("12        "))

(test read-sequence-2
      (let ((seq (make-string 10 :initial-element #\space)))
	(with-input-from-string (blah "127182187261762")
	  (gray:stream-read-sequence blah seq))
	seq)
      ("1271821872"))

(test-expect-error read-sequence-3
                   (let ((seq (make-string 10 :initial-element #\space)))
                     (with-input-from-string (blah "127182187261762")
                       (read-sequence seq blah :end -1))))

(test-expect-error read-sequence-4
                   (let ((seq (make-string 10 :initial-element #\space)))
                     (with-input-from-string (blah "127182187261762")
                       (read-sequence seq blah :start 3 :end 2))))
(test nsubst-1
      (nsubst 1 2 '(1 2 3(1 2 3(1 2 3)))) ((1 1 3 (1 1 3 (1 1 3)))))

(test nsubst-2
      (nsubst 1 2 '(1 2 3(1 2 3(1 2 3))) :test #'eql)
      ((1 1 3 (1 1 3 (1 1 3)))))

(test nsubst-3
      (nsubst 1 2 '(1 2 3(1 2 3(1 2 3)))
              :test-not #'(lambda(a b)(not (eql a b))))
      ((1 1 3 (1 1 3 (1 1 3)))))

(test-expect-error nsubst-4 
		   (nsubst 1 2 '(1 2 3(1 2 3(1 2 3))) :test #'eql :test-not #'(lambda(a b)(not (eql a b))))
		   :type program-error)

(test-expect-error elt-1 
		   (ELT "ABCDEFGH" -1)
		   :type type-error)

(test-expect-error elt-2 
		   (ELT "ABCDEFGH" 15)
		   :type type-error)

(test-expect-error elt-3 
		   (ELT (make-array 10 :initial-element 42) 15)
		   :type type-error)

(test-expect-error elt-4 
		   (ELT (make-array 10 :initial-element 42) -1)
		   :type type-error)

(test-expect-error elt-5 
		   (ELT (list 1 2 3) -1)
		   :type type-error)

;;; 
(test-expect-error elt-5a 
		   (ELT (list 1 2 3) 4)
		   :type type-error)

(test-expect-error elt-5b 
		   (ELT (cons 1 2) 1)
		   :type type-error)

(test-expect-error elt-6 
		   (ELT nil 2)
		   :type type-error)

(test-expect-error elt-7 
		   (ELT (make-array 0) 0)
		   :type type-error)

;;; fails, should report an error, since 5 is after the fill-pointer
;;; for aref should not fail, since elt is convert to aref, we have a deviation
;;; fixed now
(test-expect-error elt-8 
		   (elt (make-array 10 :initial-contents '(0 1 2 3 4 5 6 7 8 9) :fill-pointer 3) 5)
		   :type type-error)

(test-expect-error elt-9 
		   (setf (ELT (list 1 2 3) -1) 23)
		   :type type-error)

(test-expect-error elt-10 
		   (setf (ELT (list 1 2 3) 4) 23)
		   :type type-error)

(test-expect-error elt-11 
		   (setf (ELT nil 2) 23)
		   :type type-error)

(test-expect-error elt-12 
		   (setf (ELT (make-array 0) 0) 23)
		   :type type-error)

(test-expect-error elt-13 
		   (setf (elt (make-array 10 :initial-contents '(0 1 2 3 4 5 6 7 8 9) :fill-pointer 3) 5) 23)
		   :type type-error)
;;;
(test-expect-error fill-1 
                   (FILL (make-array 6 :initial-element 3) -5 :start -1)
                   :type  type-error)

(test-expect-error fill-1a 
                   (FILL (make-array 6 :initial-element 3) -5 :start 23)
                   :type  type-error)

(test-expect-error fill-2
                   (FILL (make-array 6 :initial-element 3) -5 :end -1)
                   :type  type-error)

(test-expect-error fill-3
                   (FILL (make-array 6 :initial-element 3) -5 :end 27)
                   :type  type-error)

(test-expect-error fill-4
                   (FILL (make-array 6 :initial-element 3) -5 :start 4 :end 3)
                   :type  program-error)

(test fill-5
      (FILL (make-array 6 :initial-element 3) -5 :start 3 :end 3)
      (#(3 3 3 3 3 3)))

(test fill-6
      (fill (make-array  32 :element-type 'bit
                            :initial-contents (list 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1))
            1 :start 5 :end 16)
      (#*01010111111111110101010101010101))

(test-expect-error fill-7
                   (fill #*01010111111111110101010101010101 2)
                   :type type-error)

(test fill-8 (fill #*0000000000 1) (#*1111111111))
(test fill-9 (fill #*0000000000 1 :start 6) (#*0000001111))
(test fill-10 (fill #*0000000000 1 :end 6) (#*1111110000))
(test fill-11 (fill #*0000000000 1 :end 10) (#*1111111111))
(test fill-12 (fill #*1111111111 0 :end 10) (#*0000000000))

(test fill-13
      (let ((vector (vector 0 1 2 3))
            (results nil))
        (dotimes (x (length vector))
          (push (let () (find x vector)) results))
        (dotimes (x (length vector))
          (dotimes (y (length vector))
            (push (let () (find x vector :end y)) results)))
        (dotimes (x (length vector))
          (dotimes (y (length vector))
            (push (let ()(find x vector :start y)) results)))
        (dotimes (x (length vector))
          (dotimes (y (length vector))
            (dotimes (z (length vector))
              (when (>= z y)
                (push (let () (find x vector :start y :end z)) results)))))
        results)
      ((NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 2 NIL 2 NIL NIL 2 NIL NIL
            NIL NIL NIL NIL 1 1 NIL 1 1 NIL NIL NIL NIL NIL NIL NIL NIL 0 0 0
            NIL 3 3 3 3 NIL 2 2 2 NIL NIL 1 1 NIL NIL NIL 0 NIL NIL NIL NIL 2
            NIL NIL NIL 1 1 NIL NIL 0 0 0 NIL 3 2 1 0)))

(test fill-13b
      (let ((vector (vector 0 1 2 3))
            (results nil))
        (dotimes (x (length vector))
          (push (find x vector) results))
        (dotimes (x (length vector))
          (dotimes (y (length vector))
            (push (find x vector :end y) results)))
        (dotimes (x (length vector))
          (dotimes (y (length vector))
            (push (find x vector :start y) results)))
        (dotimes (x (length vector))
          (dotimes (y (length vector))
            (dotimes (z (length vector))
              (when (>= z y)
                (push (find x vector :start y :end z) results)))))
        results)
      ((NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL 2 NIL 2 NIL NIL 2 NIL NIL
            NIL NIL NIL NIL 1 1 NIL 1 1 NIL NIL NIL NIL NIL NIL NIL NIL 0 0 0
            NIL 3 3 3 3 NIL 2 2 2 NIL NIL 1 1 NIL NIL NIL 0 NIL NIL NIL NIL 2
            NIL NIL NIL 1 1 NIL NIL 0 0 0 NIL 3 2 1 0)))

(test fill-14 (let ()(FIND 1 #*0010010 :END 1)) (nil))
(test fill-15 (let ()(FIND 1 (vector 0 1 2 3 4 5) :END 1)) (nil))

(test fill-16 (let ()(FIND 1 (vector 0 1 2 3 4 5) :END 2)) (1))

(test search-sequence-1 (search "5" "0123456789" :start2 2) (5))
(test search-sequence-2
      (search (vector 4 5) (vector 0 1 2 3 4 5 6 7 8 9) :start2 2)
      (4))
(test search-sequence-3
      (search (list 4 5) (list 0 1 2 3 4 5 6 7 8 9) :start2 2)
      (4))

(test-true equalp-1
           (equalp "1234567890"
                   (make-array 15 :element-type 'character :initial-contents "123456789012345" :fill-pointer 10)))

;;; fails no longer
(test-true equalp-2
           (equalp
            (make-array 12 :element-type 'character :initial-contents "123456789012" :fill-pointer 10)
            (make-array 15 :element-type 'character :initial-contents "123456789012345" :fill-pointer 10)))

(test-true equalp-2a
           (equalp
            (make-array 15 :element-type 'base-char :initial-contents "123456789012345" :fill-pointer 10)
            (make-array 12 :element-type 'base-char :initial-contents "123456789012" :fill-pointer 10)))

(test-true equalp-2b
           (equalp
            (make-array 3 :element-type 'base-char :initial-contents "abc" :fill-pointer 2)
            (vector #\a #\B)))

(test-true equalp-2c
           (equalp
            (make-array 3 :element-type 'character :initial-contents "abc" :fill-pointer 2)
            (vector #\a #\B)))

(test-true equalp-2d
           (not (equalp
                 (make-array 3 :element-type 'character :initial-contents "abc" :fill-pointer 2)
                 23)))


;;; from clhs Fill-pointer in the second array seem to be respected
(test-true equalp-clhs-1
           (Let ((array1 (make-array 6 :element-type 'integer
                                       :initial-contents '(1 1 1 3 5 7)))
                 (array2 (make-array 8 :element-type 'integer
                                       :initial-contents '(1 1 1 3 5 7 2 6)
                                       :fill-pointer 6)))
             (equalp array1 array2)))

;;; from clhs 5.3.36 equalp
;;; If two arrays have the same number of dimensions, the dimensions match, and the corresponding active elements are equalp.
(test-true equalp-clhs-2a
           (Let ((array1 (make-array 6 :element-type 'integer
                                       :initial-contents '(1 1 1 3 5 7)))
                 (array2 (make-array 8 :element-type 'integer
                                       :initial-contents '(1 1 1 3 5 7 2 6)
                                       :fill-pointer 6)))
             (equalp array2 array1)))

(test-true equalp-clhs-2b
           (Let ((array2 (make-array 6 :element-type 'integer
                                       :initial-contents '(1 1 1 3 5 7)))
                 (array1 (make-array 8 :element-type 'integer
                                       :initial-contents '(1 1 1 3 5 7 2 6)
                                       :fill-pointer 6)))
             (equalp array2 array1)))

(test-true equalp-3 (equalp #*0010 #(0 0 1 0)))

(test-true equalp-4
           (equalp
            (MAKE-ARRAY '(4) :INITIAL-CONTENTS '(0 0 1 0) :ELEMENT-TYPE 'BIT)
            #(0 0 1 0)))

(test-true equalp-5
           (equalp
            (MAKE-ARRAY '(4) :INITIAL-CONTENTS '(1 2 3 4) :ELEMENT-TYPE 'Integer)
            #(1 2 3 4)))

(test-true equalp-6 (equalp "ab" #(#\a #\b)))

(test-true equalp-7
           (let ((vector (make-array 3 :fill-pointer 2 :initial-contents (list 1 2 3))))
             (equalp vector (copy-seq vector))))

;;; COERCE.6
(test-true equalp-8 (equalp #* #()))

;;; COERCE.7 + 8
(test-true equalp-9 (equalp #*10 #(1 0)))

(test-true nreverse-1
           (let ((array (MAKE-ARRAY 10 :INITIAL-CONTENTS '(1 2 3 4 5 6 7 8 9 10) :FILL-POINTER 5)))
             (let ((new (nreverse array)))
               (array-has-fill-pointer-p new))))

(test reverse-1
      (reverse
       (make-array 3 :displaced-to
                   (make-array 5 :initial-contents (list 0 1 2 3 4))
                     :displaced-index-offset 1))
      (#(3 2 1)))

(test nreverse-2
      (nreverse
       (make-array 3 :displaced-to
                   (make-array 5 :initial-contents (list 0 1 2 3 4))
                     :displaced-index-offset 1))
      (#(3 2 1)))

(test assoc-1
      (locally (declare (notinline assoc))
        (ASSOC NIL '((A . B) NIL (C . D) (NIL . E) (NIL . F) NIL (G . H))))
      ((nil . e)))

(test assoc-compiler-macro
      (Let () (ASSOC NIL '((A . B) NIL (C . D) (NIL . E) (NIL . F) NIL (G . H))))
      ((nil . e)))

(test-expect-error assoc-2-error
                   (assoc 3 (list (list 1 3) 2))
                   :type type-error)

(test-expect-error assoc-2-error-no-compiler-macro
                    (locally (declare (notinline assoc))
                      (assoc 3 (list (list 1 3) 2)))
                      :type type-error)

;;; That used to crash clasp, test with care
;;; Use (declare (notinline ..)) to prevent use of the compiler-macro
(test-expect-error make-list-1 (MAKE-LIST -1) :type type-error)
(test-expect-error make-sequence-1
                   (locally (declare (notinline make-sequence))
                     (make-sequence 'list -1)) :type type-error)
(test-expect-error make-sequence-1a
                   (let ()
                     (MAKE-sequence 'list -1)) :type type-error)
(test-expect-error make-sequence-2
                   (locally (declare (notinline make-sequence))
                     (MAKE-sequence 'vector -1)) :type type-error)
(test-expect-error make-sequence-2a (let () (MAKE-sequence 'vector -1)) :type type-error)
;;; fixed in the function and  in the compiler-macro
(test-expect-error make-sequence-3
                   (locally (declare (notinline make-sequence))
                     (MAKE-sequence 'cons 0)) :type type-error)
(test-expect-error make-sequence-3a (let ()
                                      (MAKE-sequence 'cons 0)) :type type-error)
;;; find
(test-expect-error find-1 (find 5 '(1 2 3 . 4)) :type type-error)
(test-expect-error find-1a
                   (locally (declare (notinline find))
                     (find 5 '(1 2 3 . 4))) :type type-error)
(test-expect-error find-2 (find 5 '(1 2 3 . 4)) :type type-error)
(test-expect-error find-2a
                   (locally (declare (notinline find))
                     (find 5 '(1 2 3 . 4))) :type type-error)

(test remove-if-1
      (let ((s (make-array 10 :element-type 'base-char
                              :displaced-to (make-array 15
                                                        :initial-contents "XXab1c23def4YYY"
                                                        :element-type 'base-char)
                              :displaced-index-offset 2)))
        (remove-if #'ALPHA-CHAR-P s :count 3))
      ("123def4"))

(test position-1
      (LET* ((S1 (COPY-SEQ "xxxabcdyyyyy"))
             (S2
               (MAKE-ARRAY '(4)
                           :DISPLACED-TO S1
                           :DISPLACED-INDEX-OFFSET 3
                           :ELEMENT-TYPE (ARRAY-ELEMENT-TYPE S1))))
        (POSITION #\c S2))
      (2))

(test position-2
      (LET* ((S1 (COPY-SEQ "xxxabcdabcdyyyyyyyy"))
             (S2
               (MAKE-ARRAY '(8)
                           :DISPLACED-TO S1
                           :DISPLACED-INDEX-OFFSET 3
                           :ELEMENT-TYPE (ARRAY-ELEMENT-TYPE S1))))
        (POSITION #\c S2 :FROM-END T))
      (6))


(test-expect-error stable-sort.error.11
                   (funcall #'(lambda (x) (stable-sort x #'<)) #'position)
                   :type type-error)

(test nreverse-bit-vector.2-a (nreverse (copy-seq #*0011)) (#*1100))

(test nreverse-bit-vector.2-b (NREVERSE (COPY-SEQ #*0001101101)) (#*1011011000))

(test nreverse-bit-vector.2-c (NREVERSE (COPY-SEQ #*000110110110))
      (#*011011011000))

(test nreverse-bit-vector.3
      (nreverse (make-array 10 :initial-contents '(0 0 0 1 1 0 1 0 1 0)
                               :fill-pointer 5
                               :element-type 'bit))
      (#*11000))

(test nreverse-vector.8
      (nreverse (MAKE-ARRAY 8 :ELEMENT-TYPE '(INTEGER -1 0)
                              :INITIAL-CONTENTS #(0 0 -1 0 -1 -1 0 -1)))
      (#(-1 0 -1 -1 0 -1 0 0)))

(test-expect-error
 every.error.14
 (every #'identity '(1 2 3 . 4))
 :type type-error)

(test-expect-error
 some.error.14
 (some #'null '(a b . c))
 :type type-error)

(test-true sequence-clx-compile-notinline
           (let ()
             (declare (notinline make-sequence))
             (make-sequence '(array char (*)) 0)))

(test-true sequence-clx-compile-inline
           (let ()
             (declare (inline make-sequence))
             (make-sequence '(array char (*)) 0)))
             
(test-type can-map-to-specialized-vectors-0
           (map (class-of (make-array 0)) 'identity (list 1 2 3))
           simple-vector)

(test-type can-map-to-specialized-vectors-1
           (map (class-of (make-array 0 :adjustable t)) 'identity (list 1 2 3))
           (vector t))

(test-type can-map-to-specialized-vectors-2
           (map (class-name (class-of (make-array 0 :adjustable t))) 'identity (list 1 2 3))
           (vector t))

(test-type can-map-to-specialized-vectors-3
           (map (class-of (make-array 0 :fill-pointer t)) 'identity (list 1 2 3))
           (vector t))

(test-type can-map-to-specialized-vectors-4
           (map (class-of (make-array 0 :displaced-to (make-array 3))) 'identity (list 1 2 3))
           (vector t))
