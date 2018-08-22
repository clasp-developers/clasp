(defpackage #:clasp-tests
    (:use :cl :core)
  (:export #:test #:test-expect-error))

(in-package #:clasp-tests)

(defparameter *passes* 0)
(defparameter *fails* 0)
(defparameter *failed-tests* nil)
(defparameter *expected-failures* nil)

(defun reset-clasp-test ()
  (setq *passes* 0
        *fails* 0
        *failed-tests* nil))

(defun note-test-finished ()
  (setq *failed-tests* (nreverse *failed-tests*)))
  
(defun show-failed-tests ()
  (cond (*failed-tests*
         (format t "~%Failed tests ~a~%" *failed-tests*)
         (show-unexpected-failures))
        (t (format t "~%No tests failed~%"))))

(defun show-unexpected-failures ()
  (let ((unexpected-failures nil))
    (dolist (fail *failed-tests*)
      (unless (member fail *expected-failures*)
        (push fail unexpected-failures)))
    (when unexpected-failures
      (format t "~%unexpected failures ~a~%" (nreverse unexpected-failures)))
    (let ((unexpected-successes nil))
      (dolist (fail *expected-failures*)
        (unless (member fail *failed-tests*)
          (push fail unexpected-successes)))
      (when unexpected-successes
        (format t "~%unexpected success ~a~%" (nreverse unexpected-successes))))))

(defmacro test (name form &key description )
  `(if (ignore-errors ,form)
       (progn
         (format t "Passed ~s~%" ',name)
         (incf *passes*))
       (progn
         (incf *fails*)
         (pushnew ',name *failed-tests*)
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
