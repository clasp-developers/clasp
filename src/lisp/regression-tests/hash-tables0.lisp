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
