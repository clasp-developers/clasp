(in-package #:clasp-tests)

(test-true float-features-1a
      (ext:float-infinity-p
       (flet ((foo () (if (> 10 (random 20)) 0.0 0.0))
              (bar () (if (> 10 (random 20)) 23 24)))
         (ext:with-float-traps-masked (:divide-by-zero)
           (/ (bar) (foo))))))

(test-expect-error float-features-1b
                   (flet ((foo () (if (> 10 (random 20)) 0.0 0.0))
                          (bar () (if (> 10 (random 20)) 23 24)))
                     (/ (bar) (foo)))
                   :type division-by-zero)

(defun foo-ext-1 (n)
  (if (> n (random 20)) most-positive-long-float most-positive-long-float))

(defun bar-ext-1 (n)
  (if (> n (random 20)) most-positive-long-float most-positive-long-float))

(test-true float-features-3
      (ext:float-infinity-p
       (ext:with-float-traps-masked (:overflow :inexact)
         (let ((n (random 100)))
           (+ (foo-ext-1 n) (bar-ext-1 n))))))

(test-expect-error float-features-4
                   (let ((n (random 100)))
                     (+ (foo-ext-1 n) (bar-ext-1 n)))
                   :type floating-point-overflow)

(defun foo-ext-2 (n)
  (if (> n (random 20)) (- most-positive-long-float 3) (- most-positive-long-float 3)))

(defun bar-ext-2 (n)
  (if (> n (random 20)) (- most-positive-long-float 3) (- most-positive-long-float 3)))

(test-true float-features-5
      (ext:float-infinity-p
       (ext:with-float-traps-masked (:overflow :inexact)
         (let ((n (random 100)))
           (+ (foo-ext-2 n) (bar-ext-2 n))))))

(test-expect-error float-features-6
                   (let ((n (random 100)))
                     (+ (foo-ext-2 n) (bar-ext-2 n)))
                   :type (or floating-point-inexact floating-point-overflow))

(defun foo-ext-3 (n)
  (if (> n (random 20)) 0.0 0.0))

(defun bar-ext-3 (n)
  (if (> n (random 20)) 0.0 0.0))

(test-true float-features-7
      (ext:float-nan-p
       (ext:with-float-traps-masked (:invalid)
         (let ((n (random 100)))
           (/ (foo-ext-3 n) (bar-ext-3 n))))))

(test-expect-error float-features-8
                   (let ((n (random 100)))
                       (/ (foo-ext-3 n) (bar-ext-3 n)))
                   :type (or floating-point-inexact
                             floating-point-invalid-operation))
