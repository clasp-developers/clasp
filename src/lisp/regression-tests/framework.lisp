(defpackage #:clasp-tests
    (:use :cl :core)
  (:export #:test))

(in-package #:clasp-tests)

(defparameter *passes* 0)
(defparameter *fails* 0)

(defmacro test (name form &key description )
  `(if (ignore-errors ,form)
       (progn
         (format t "Passed ~s~%" ',name)
         (incf *passes*))
       (progn
         (incf *fails*)
         (format t "Failed ~s~%" ',name)
         (when ,description (format t "~s~%" ,description)))))

(defmacro test-type= (t1 t2)
  `(test (and (subtypep ,t1 ,t2) (subtypep ,t2 ,t1))))

(defun expand-test-expect-error (fn)
  (handler-case
      (progn
        (funcall fn)
        nil)
    (error (err) t)))

(defmacro test-expect-error (name form &key (type 'error) description)
  `(test ,name (handler-case (progn ,form nil)
                 (,type (err) t)) :description ,description))
