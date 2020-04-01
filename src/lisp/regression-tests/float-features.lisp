(in-package #:clasp-tests)

(test float-features-1
      (ext:float-infinity-p
       (flet ((foo () (if (> 10 (random 20)) 0.0 0.0))
              (bar () (if (> 10 (random 20)) 23 24)))
         (ext:with-float-traps-masked (:divide-by-zero)
           (/ (bar) (foo))))))
(test float-features-1
      (handler-case
          (flet ((foo () (if (> 10 (random 20)) 0.0 0.0))
                 (bar () (if (> 10 (random 20)) 23 24)))
            (ext:with-float-traps-masked ()
              (/ (bar) (foo))
              ))
        (division-by-zero (error)
          (values t error))))

(defun foo-ext-1 (n)
  (if (> n (random 20)) most-positive-long-float most-positive-long-float))

(defun bar-ext-1 (n)
  (if (> n (random 20)) most-positive-long-float most-positive-long-float))

(test float-features-3
      (ext:float-infinity-p
       (ext:with-float-traps-masked (:overflow :inexact)
         (let ((n (random 100)))
           (+ (foo-ext-1 n) (bar-ext-1 n))))))

(test float-features-4
      (handler-case
          (let ((n (random 100)))
            (ext:with-float-traps-masked ()
              (+ (foo-ext-1 n) (bar-ext-1 n))))
        (FLOATING-POINT-OVERFLOW (error)
          (values t error))))

(defun foo-ext-2 (n)
  (if (> n (random 20)) (- most-positive-long-float 3) (- most-positive-long-float 3)))

(defun bar-ext-2 (n)
  (if (> n (random 20)) (- most-positive-long-float 3) (- most-positive-long-float 3)))

(test float-features-5
      (ext:float-infinity-p
       (ext:with-float-traps-masked (:overflow :inexact)
         (let ((n (random 100)))
           (+ (foo-ext-2 n) (bar-ext-2 n))))))

(test float-features-6
      (handler-case
          (let ((n (random 100)))
            (ext:with-float-traps-masked ()
              (+ (foo-ext-2 n) (bar-ext-2 n))))
        (FLOATING-POINT-INEXACT (error)
          (values t error))
        (FLOATING-POINT-OVERFLOW (error)
          (values t error))))

(defun foo-ext-3 (n)
  (if (> n (random 20)) 0.0 0.0))

(defun bar-ext-3 (n)
  (if (> n (random 20)) 0.0 0.0))

(test float-features-7
      (ext:float-nan-p
       (ext:with-float-traps-masked (:invalid)
         (let ((n (random 100)))
           (/ (foo-ext-3 n) (bar-ext-3 n))))))

(test float-features-8
      (handler-case
          (ext:with-float-traps-masked ()
            (let ((n (random 100)))
              (/ (foo-ext-3 n) (bar-ext-3 n))))
        (FLOATING-POINT-INEXACT (error)
          (values t error))
        (FLOATING-POINT-INVALID-OPERATION (error)
          (values t error))))
