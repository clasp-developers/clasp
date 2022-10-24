(in-package #:clasp-tests)

(test-true hash-table-1 (let ((ht1 (make-hash-table))
                              (ht2 (make-hash-table)))
                          (setf (gethash 23 ht1) 42)
                          (setf (gethash 23 ht2) 42)
                          (equalp ht1 ht2)))

(test-true hash-table-2 (let ((ht1 (make-hash-table))
                              (ht2 (make-hash-table)))
                          (setf (gethash 23 ht1) 42)
                          (setf (gethash 24 ht2) 41)
                          (not (equalp ht1 ht2))))

(test-true hash-table-3 (let ((ht1 (make-hash-table))
                              (ht2 (make-hash-table)))
                          (setf (gethash #\a ht1) 42)
                          (setf (gethash #\A ht2) 41)
                          (not (equalp ht1 ht2))))

;;; Should not be equal, since the keys are not equalp
(test-true hash-table-4 (let ((ht1 (make-hash-table))
                              (ht2 (make-hash-table)))
                          (setf (gethash '#:a ht1) 42)
                          (setf (gethash '#:a ht2) 41)
                          (not (equalp ht1 ht2))))

;;; sxhash must be a positive fixnum
(test-true sxhash-1
           (let ((hash (sxhash (MAKE-SYMBOL "FOO"))))
             (and (plusp hash)
                  (typep hash 'fixnum))))

(test-true sxhash-2
           (let ((hash (sxhash 5)))
             (and (plusp hash)
                  (typep hash 'fixnum))))

(test-true sxhash-3
           (let ((hash (sxhash #*11)))
             (and (plusp hash)
                  (typep hash 'fixnum))))

(test hash-table-count (hash-table-count (make-hash-table)) (0))
(test hash-table-count-2
      (let ((table (make-hash-table)))
        (setf (gethash :key table) 23)
        (setf (gethash :key1 table) 22)
        (setf (gethash :key table) 25)
        (hash-table-count table))
      (2))
(test hash-table-size (hash-table-size (make-hash-table :size 128)) (128))
(test-true hash-table-rehash-size (HASH-TABLE-REHASH-SIZE (make-hash-table)))
(test-true hash-table-rehash-THRESHOLD
           (HASH-TABLE-REHASH-THRESHOLD (make-hash-table)))
(test-true hash-table-test (HASH-TABLE-TEST (make-hash-table)))
(test setf-gethash (let ((table (make-hash-table)))
                     (setf (gethash :key table) 23)
                     (values (gethash :key table)))
      (23))
(test-true clrhash (clrhash (make-hash-table)))
(test-true maphash (progn
                     (maphash #'(lambda(a b)
                                  (declare (ignore a b)))
                              (make-hash-table))
                     t))

(test-true remhash (progn
                     (remhash :key (make-hash-table))
                     t))


;;; issue 620
(test-expect-error hash-table-count-nil (hash-table-count nil) :type type-error)
(test-expect-error hash-table-size-nil (hash-table-size nil) :type type-error)
(test-expect-error hash-table-rehash-size-nil (HASH-TABLE-REHASH-SIZE nil) :type type-error)
(test-expect-error hash-table-rehash-THRESHOLD-nil (HASH-TABLE-REHASH-THRESHOLD nil) :type type-error)
(test-expect-error hash-table-test-nil (HASH-TABLE-TEST nil) :type type-error)

;;weak tables: weakness :key
(test hash-table-count-weak-key
      (hash-table-count (make-hash-table :test #'eq :weakness :key))
      (0))
(test hash-table-count-2-weak-key
      (let ((table (make-hash-table :test #'eq :weakness :key)))
        (setf (gethash :key table) 23)
        (setf (gethash :key1 table) 22)
        (setf (gethash :key table) 25)
        (hash-table-count table))
      (2))

(test hash-table-size-weak-key
      (hash-table-size (make-hash-table :size 128 :test #'eq :weakness :key))
      (128))
(test-true hash-table-rehash-size-weak-key (HASH-TABLE-REHASH-SIZE (make-hash-table :test #'eq :weakness :key)))
(test-true hash-table-rehash-THRESHOLD-weak-key (HASH-TABLE-REHASH-THRESHOLD (make-hash-table :test #'eq :weakness :key)))
(test-true hash-table-test-weak-key (HASH-TABLE-TEST (make-hash-table :test #'eq :weakness :key)))
(test-true gethash-weak-key (let ((table (make-hash-table :test #'eq :weakness :key)))
                         (gethash :key table)
                         t))
(test setf-gethash-weak-key
      (let ((table (make-hash-table :test #'eq :weakness :key)))
        (setf (gethash :key table) 23)
        (values (gethash :key table)))
      (23))
(test-true clrhash-weak-key (clrhash (make-hash-table :test #'eq :weakness :key)))
(test-true maphash-weak-key (progn
                              (maphash #'(lambda(a b)
                                           (declare (ignore a b)))
                                       (make-hash-table :test #'eq :weakness :key))
                              t))
(test-true remhash-weak-key (progn
                              (remhash :key (make-hash-table :test #'eq  :weakness :key))
                              t))

(test equalp-hash-table-1
      (let ((key-1 #\a)
            (key-2 #\A)
            (ht (make-hash-table :test #'equalp)))
          (setf (gethash key-1 ht) t)
          (gethash key-2 ht))
      (t t))

(test equalp-hash-table-2
      (let ((key-1 #*0010)
            (key-2 #(0 0 1 0))
            (ht (make-hash-table :test #'equalp)))
          (setf (gethash key-1 ht) t)
          (gethash key-2 ht))
      (t t))

(test equalp-hash-table-3
      (let ((key-2 #*0010)
            (key-1 #(0 0 1 0))
            (ht (make-hash-table :test #'equalp)))
        (setf (gethash key-1 ht) t)
        (gethash key-2 ht))
      (t t))

;;; Custom tables (extension)

(defun car-equal (x y) (equal (car x) (car y)))
(defun car-sxhash (x) (sxhash (car x)))

(test custom-hash-table-1
      (let ((key1 (list 'a 'b))
            (key2 (list 'a 'c))
            (table (make-hash-table :test #'car-equal
                                    :hash-function #'car-sxhash)))
        (setf (gethash key1 table) t)
        (gethash key2 table))
      (t t))

;;;

(test test-issue-946
      (values-list
       (mapcar #'hash-table-p
               (list (make-hash-table)
                     (make-hash-table :test #'eq :weakness :key)
                     1 1.0 "weruz" #\a #'car (find-class 'number))))
      (t t nil nil nil nil nil nil))

(test test-issue-950
      (let ((hash-table (make-hash-table :test #'eq :weakness :key))
            (result nil)
            (store-keys nil))
        (let ((key (cons 1 2)))
          (setf (gethash key hash-table) 23)
          (push key store-keys))
        (let ((key (cons 3 4)))
          (setf (gethash key hash-table) 24)
          (push key store-keys))
        (let ((key (cons 4 5)))
          (setf (gethash key hash-table) 25)
          (push key store-keys))
        (with-hash-table-iterator (it hash-table)
          (loop
            (multiple-value-bind (more-p key value)
                (it)
              (if more-p
                  (push (list value key) result)
                  (return)))))
        (sort result #'< :key #'first))
      (((23 (1 . 2)) (24 (3 . 4)) (25 (4 . 5)))))

(test-true test-issue-1042
           (let ()
             (make-hash-table :size 128 :test #'eq :weakness :key)
             (gctools:garbage-collect)
             t))
