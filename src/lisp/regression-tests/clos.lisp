(in-package #:clasp-tests)

(defun foo (x) :nothing)
(test generic-function-p-negative (not (core:generic-function-p (fdefinition 'foo))))

(defgeneric gf123bar (x))
(test generic-function-p-positive (core:generic-function-p (fdefinition 'gf123bar)))

(defclass test ()((foo :initform :bar :accessor test-foo)))
(test-expect-error accessor-too-many-args-1
                   (test-foo (make-instance 'test) 23 24 25))

(test-expect-error defclass-1 (eval '(defclass foo ()(bar bar))) :type program-error)

(test 'JIT-FUNCTION-NAME-1
      (equal '(NIL (3 (3 . 2)))
             (multiple-value-list
              (let* ((sym (gensym))
                     (method
                      (eval `(defmethod (setf ,sym) ((x t) (y cons)) (setf (car y) x)))))
                (values
                 (fboundp sym)
                 (let ((x (cons 1 2))) (list (funcall (fdefinition `(setf ,sym)) 3 x) x)))))))
