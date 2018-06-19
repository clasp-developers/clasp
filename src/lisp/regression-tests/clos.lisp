(in-package #:clasp-tests)

(defun foo (x) :nothing)
(test generic-function-p-negative (not (core:generic-function-p (fdefinition 'foo))))

(defgeneric gf123bar (x))
(test generic-function-p-positive (core:generic-function-p (fdefinition 'gf123bar)))

(defclass test ()((foo :initform :bar :accessor test-foo)))
(test-expect-error accessor-too-many-args-1
                   (test-foo (make-instance 'test) 23 24 25))
