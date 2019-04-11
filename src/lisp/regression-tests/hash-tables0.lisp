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

(test hash-table-count (zerop (hash-table-count (make-hash-table))))
(test hash-table-count-2
      (= 2
         (let ((table (make-hash-table)))
           (setf (gethash :key table) 23)
           (setf (gethash :key1 table) 22)
           (setf (gethash :key table) 25)
           (hash-table-count table))))
(test hash-table-size (= 128 (hash-table-size (make-hash-table :size 128))))
(test hash-table-rehash-size (HASH-TABLE-REHASH-SIZE (make-hash-table)))
(test hash-table-rehash-THRESHOLD (HASH-TABLE-REHASH-THRESHOLD (make-hash-table)))
(test hash-table-test (HASH-TABLE-TEST (make-hash-table)))
(test setf-gethash (= 23
                      (let ((table (make-hash-table)))
                        (setf (gethash :key table) 23)
                        (gethash :key table))))
(test clrhash (clrhash (make-hash-table)))
(test maphash (progn
                (maphash #'(lambda(a b))
                         (make-hash-table))
                t))

(test remhash (progn
                (remhash :key (make-hash-table))
                t))


;;; issue 620
(test-expect-error hash-table-count-nil (hash-table-count nil) :type type-error)
(test-expect-error hash-table-size-nil (hash-table-size nil) :type type-error)
(test-expect-error hash-table-rehash-size-nil (HASH-TABLE-REHASH-SIZE nil) :type type-error)
(test-expect-error hash-table-rehash-THRESHOLD-nil (HASH-TABLE-REHASH-THRESHOLD nil) :type type-error)
(test-expect-error hash-table-test-nil (HASH-TABLE-TEST nil) :type type-error)

;;weak tables: weakness :key
(test hash-table-count-weak-key (zerop (hash-table-count (make-hash-table :weakness :key))))
(test hash-table-count-2-weak-key
      (= 2
         (let ((table (make-hash-table :weakness :key)))
           (setf (gethash :key table) 23)
           (setf (gethash :key1 table) 22)
           (setf (gethash :key table) 25)
           (hash-table-count table))))

(test hash-table-size-weak-key (= 128 (hash-table-size (make-hash-table :size 128 :weakness :key))))
(test hash-table-rehash-size-weak-key (HASH-TABLE-REHASH-SIZE (make-hash-table :weakness :key)))
(test hash-table-rehash-THRESHOLD-weak-key (HASH-TABLE-REHASH-THRESHOLD (make-hash-table :weakness :key)))
(test hash-table-test-weak-key (HASH-TABLE-TEST (make-hash-table :weakness :key)))
(test gethash-weak-key (let ((table (make-hash-table :weakness :key)))
                         (gethash :key table)
                         t))
(test setf-gethash-weak-key (= 23
                               (let ((table (make-hash-table :weakness :key)))
                                 (setf (gethash :key table) 23)
                                 (gethash :key table))))
(test clrhash-weak-key (clrhash (make-hash-table :weakness :key)))
(test maphash-weak-key (progn
                         (maphash #'(lambda(a b))
                                  (make-hash-table :weakness :key))
                         t))
(test remhash-weak-key (progn
                         (remhash :key (make-hash-table  :weakness :key))
                         t))

(test hash-table-classes
      (let ((sub (clos:class-direct-subclasses (first (clos:class-direct-superclasses (find-class 'hash-table))))))
        (and (= 2 (length sub))
             (find (find-class 'hash-table) sub)
             (find (find-class 'CORE:WEAK-KEY-HASH-TABLE) sub)
             t)))


