(in-package #:clasp-tests)

(test hash-table-1 (let ((ht1 (make-hash-table))
                         (ht2 (make-hash-table)))
                     (setf (gethash 23 ht1) 42)
                     (setf (gethash 23 ht2) 42)
                     (equalp ht1 ht2)))

(test hash-table-2 (let ((ht1 (make-hash-table))
                         (ht2 (make-hash-table)))
                     (setf (gethash 23 ht1) 42)
                     (setf (gethash 24 ht2) 41)
                     (not (equalp ht1 ht2))))

(test hash-table-3 (let ((ht1 (make-hash-table))
                         (ht2 (make-hash-table)))
                     (setf (gethash #\a ht1) 42)
                     (setf (gethash #\A ht2) 41)
                     (not (equalp ht1 ht2))))

;;; Should not be equal, since the keys are not equalp
(test hash-table-4 (let ((ht1 (make-hash-table))
                         (ht2 (make-hash-table)))
                     (setf (gethash '#:a ht1) 42)
                     (setf (gethash '#:a ht2) 41)
                     (not (equalp ht1 ht2))))

;;; sxhash must be a positive fixnum
(test sxhash-1
      (let ((hash (sxhash (MAKE-SYMBOL "FOO"))))
        (and (plusp hash)
             (typep hash 'fixnum))))

(test sxhash-2
      (let ((hash (sxhash 5)))
        (and (plusp hash)
             (typep hash 'fixnum))))

(test sxhash-3
      (let ((hash (sxhash #*11)))
        (and (plusp hash)
             (typep hash 'fixnum))))

(test hash-table-count (hash-table-count (make-hash-table)))
(test hash-table-size (hash-table-size (make-hash-table)))
(test hash-table-rehash-size (HASH-TABLE-REHASH-SIZE (make-hash-table)))
(test hash-table-rehash-THRESHOLD (HASH-TABLE-REHASH-THRESHOLD (make-hash-table)))
(test hash-table-test (HASH-TABLE-TEST (make-hash-table)))

;;; issue 620
(test-expect-error hash-table-count-nil (hash-table-count nil) :type type-error)
(test-expect-error hash-table-size-nil (hash-table-size nil) :type type-error)
(test-expect-error hash-table-rehash-size-nil (HASH-TABLE-REHASH-SIZE nil) :type type-error)
(test-expect-error hash-table-rehash-THRESHOLD-nil (HASH-TABLE-REHASH-THRESHOLD nil) :type type-error)
(test-expect-error hash-table-test-nil (HASH-TABLE-TEST nil) :type type-error)


