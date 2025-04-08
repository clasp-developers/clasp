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

;; weak tables
(defun hash-table-basics (table)
  (values (hash-table-p table)
          (hash-table-size table)
          (hash-table-count table)
          (typep (hash-table-rehash-size table) '(real 1))
          (typep (hash-table-rehash-threshold table) '(real 0 1))
          (hash-table-test table)
          (ext:hash-table-weakness table)
          (multiple-value-list (gethash :key table))))

(test hash-table-weak-key.basic
      (hash-table-basics (make-hash-table :size 128 :weakness :key))
      (t 128 0 t t eql :key (nil nil)))
(test hash-table-weak-value.basic
      (hash-table-basics (make-hash-table :size 128 :weakness :value))
      (t 128 0 t t eql :value (nil nil)))
(test hash-table-weak-key-and-value.basic
      (hash-table-basics (make-hash-table :size 128 :weakness :key-and-value))
      (t 128 0 t t eql :key-and-value (nil nil)))
(test hash-table-weak-key-or-value.basic
      (hash-table-basics (make-hash-table :size 128 :weakness :key-or-value))
      (t 128 0 t t eql :key-or-value (nil nil)))

(test hash-table-count-2-weak-key
      (let ((table (make-hash-table :weakness :key)))
        (setf (gethash :key table) 23)
        (setf (gethash :key1 table) 22)
        (setf (gethash :key table) 25)
        (hash-table-count table))
      (2))

(test setf-gethash-weak-key
      (let ((table (make-hash-table :weakness :key)))
        (setf (gethash :key table) 23)
        (values (gethash :key table)))
      (23))

(test clrhash-weak-key
      (let ((table (make-hash-table :weakness :key)))
        (values (hash-table-p (clrhash table))
                (hash-table-count table)))
      (t 0))
(test-nil maphash-weak-key (maphash (lambda (a b)
                                      (declare (ignore a b)))
                                    (make-hash-table :weakness :key)))
(test-nil remhash-weak-key (remhash :key (make-hash-table :weakness :key)))

;;; These tests are pretty strict - they want the garbage to actually be
;;; collected by that garbage-collect call, which may not be the case if we
;;; ever get a more relaxed collector (generational or something)
(test weak-key-weakness
      (let ((table (make-hash-table :weakness :key)))
        (setf (gethash (list 37) table) :value
              (gethash :key table) (list nil)
              (gethash :key2 table) (list nil))
        (gctools:garbage-collect)
        (hash-table-count table))
      (2))
(test weak-value-weakness
      (let ((table (make-hash-table :weakness :value)))
        (setf (gethash :key table) (list 37)
              (gethash (list nil) table) :value
              (gethash (list 18) table) :value)
        (gctools:garbage-collect)
        (hash-table-count table))
      (2))
(test weak-key-and-value-weakness
      (let ((table (make-hash-table :weakness :key-and-value)))
        (setf (gethash (list nil) table) :value
              (gethash :key table) (list nil)
              (gethash (list nil) table) (list nil)
              (gethash :key table) :value)
        (gctools:garbage-collect)
        (hash-table-count table))
      (1))
#-use-boehm ; on boehm key-or-value tables are effectively strong.
(test weak-key-or-value-weakness
      (let ((table (make-hash-table :weakness :key-or)))
        (setf (gethash (list nil) table) :value
              (gethash :key table) (list nil)
              (gethash (list nil) table) (list nil)
              (gethash :key table) :value)
        (gctools:garbage-collect)
        (hash-table-count table))
      (3))

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
