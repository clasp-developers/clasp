(in-package #:clasp-tests)

(test-expect-error subseq-oob1 (subseq #(1 2 3) 1 5))
(test-expect-error subseq-oob2 (subseq '(1 2 3) 0 5))
(test subseq0 (equal (subseq '(1 2 3 4 5) 0 3) (list 1 2 3)))

(test last-1 (equal (list 1 2 3 4)
             (last (list 1 2 3 4) most-positive-fixnum)))
(test last-2  (equal (list 1 2 3 4)
             (last (list 1 2 3 4) (1+ most-positive-fixnum))))
(test last-3  (null (last (list 1 2 3 4) 0)))
(TEST-EXPECT-ERROR last-4 (last (list 1 2 3 4) -1) :type type-error)
(TEST-EXPECT-ERROR last-5 (last (list 1 2 3 4) most-negative-fixnum) :type type-error)
(TEST-EXPECT-ERROR last-6 (last (list 1 2 3 4) (1- most-negative-fixnum)) :type type-error)

(test butlast-1  (null (butlast (list 1 2 3 4 5 6 7 8 9 10) (1+ most-positive-fixnum))))
(test nbutlast-1  (null (nbutlast (list 1 2 3 4 5 6 7 8 9 10) (1+ most-positive-fixnum))))

(TEST-EXPECT-ERROR butlast-2 (butlast (list 1 2 3 4 5 6 7 8 9 10) -5) :type type-error)
(TEST-EXPECT-ERROR nbutlast-2 (nbutlast (list 1 2 3 4 5 6 7 8 9 10) -5) :type type-error)

(test-expect-error nth-1
		   (nth -1 (list 1 2 3))
		   :type type-error)

(test nth-2 (null (nth most-positive-fixnum (list 1 2 3))))
(test nth-3 (null (nth (1+  most-positive-fixnum) (list 1 2 3))))

(test-expect-error nthcdr-1
		   (nthcdr -1 (list 1 2 3))
		   :type type-error)

(test nthcdr-2 (null (nthcdr most-positive-fixnum (list 1 2 3))))
(test nthcdr-3 (null (nthcdr (1+  most-positive-fixnum) (list 1 2 3))))

(test write-sequence-1
      (string= "12"
               (with-output-to-string (blah)
                 (core:do-write-sequence "1223" blah 0 2))))

(test write-sequence-1a
      (string= "12"
               (with-output-to-string (blah)
                 (core:do-write-sequence (list #\1 #\2 #\3 #\4) blah 0 2))))

(test write-sequence-1b
      (string= "12"
               (with-output-to-string (blah)
                 (core:do-write-sequence (vector #\1 #\2 #\3 #\4) blah 0 2))))

(test write-sequence-1c
      (string= "12"
               (with-output-to-string (blah)
                 (write-sequence "1223" blah :start 0 :end 2))))

(test write-sequence-1d
      (string= "12"
               (with-output-to-string (blah)
                 (write-sequence (list #\1 #\2 #\3 #\4) blah :start 0 :end 2))))

(test write-sequence-1e
      (string= "12"
               (with-output-to-string (blah)
                 (write-sequence (vector #\1 #\2 #\3 #\4) blah :start 0 :end 2))))

(test write-sequence-2
       (string= "1223"
                (with-output-to-string (blah)
                  (gray:stream-write-sequence blah "1223"))))

(test read-sequence-1
      (string= "12        "
	       (let ((seq (make-string 10 :initial-element #\space)))
		 (with-input-from-string (blah "127182187261762")
		   (core:do-read-sequence seq blah 0 2))
		 seq)))

(test read-sequence-2
      (string= "1271821872"
	       (let ((seq (make-string 10 :initial-element #\space)))
		 (with-input-from-string (blah "127182187261762")
		   (gray:stream-read-sequence blah seq))
		 seq)))
(test nsubst-1
      (equalp '(1 1 3 (1 1 3 (1 1 3)))
	      (nsubst 1 2 '(1 2 3(1 2 3(1 2 3))))))

(test nsubst-2
      (equalp '(1 1 3 (1 1 3 (1 1 3)))
	      (nsubst 1 2 '(1 2 3(1 2 3(1 2 3))) :test #'eql)))

(test nsubst-3
      (equalp '(1 1 3 (1 1 3 (1 1 3)))
	      (nsubst 1 2 '(1 2 3(1 2 3(1 2 3))) :test-not #'(lambda(a b)(not (eql a b))))))

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

;;; Fails
(test-expect-error elt-5a 
		   (ELT (list 1 2 3) 4)
		   :type type-error)

(test-expect-error elt-6 
		   (ELT nil 2)
		   :type type-error)

(test-expect-error elt-7 
		   (ELT (make-array 0) 0)
		   :type type-error)

;;; fails
(test-expect-error elt-8 
		   (elt (make-array 10 :initial-contents '(0 1 2 3 4 5 6 7 8 9) :fill-pointer 3) 5)
		   :type type-error)



  
