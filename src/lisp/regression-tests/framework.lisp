(defpackage #:clasp-tests
    (:use :cl :core)
  (:export #:test #:test-expect-error))

(in-package #:clasp-tests)

(defparameter *passes* 0)
(defparameter *fails* 0)
(defparameter *failed-tests* nil)
(defparameter *expected-failures* nil)
(defparameter *files-failed-to-compile* nil)
(defparameter *test-marker-table* (make-hash-table))

(defun reset-clasp-tests ()
  (setq *passes* 0
        *fails* 0
        *failed-tests* nil
        *files-failed-to-compile* nil
        *test-marker-table* (make-hash-table)
        *duplicate-tests* nil))

(defun note-test (name)
  (cond ((gethash name *test-marker-table*)
         (push name *duplicate-tests*)
         (warn "~%Duplicate test ~a~%" name))
        (t (setf (gethash name *test-marker-table*) t)))) 

(defun note-test-finished ()
  (setq *failed-tests* (nreverse *failed-tests*)))

(defun note-compile-error (file&error)
  (push file&error *files-failed-to-compile*))
  
(defun show-failed-tests ()
  (cond (*failed-tests*
         (format t "~%Failed tests ~a~%" *failed-tests*)
         (show-unexpected-failures))
        (t (format t "~%No tests failed~%")))
  (when *files-failed-to-compile*
    (dolist (file&error *files-failed-to-compile*)
      (format t "Compilation error for file ~a with error  ~a~%" (first file&error)(second file&error))))
  (when *duplicate-tests*
    (dolist (test *duplicate-tests*)
      (format t "Duplicate test ~a~%" test))))

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
  `(if (progn
         (note-test ',name)
         (ignore-errors ,form))
       (progn
         (format t "Passed ~s~%" ',name)
         (incf *passes*))
       (progn
         (incf *fails*)
         (push ',name *failed-tests*)
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

(defun load-if-compiled-correctly (file)
  (handler-case
      (multiple-value-bind
            (fasl warnings-p failure-p)
          (let ((cmp::*compile-file-parallel* nil))
            (compile-file file))
        (declare (ignore warnings-p failure-p))
        (when fasl
          (load fasl)))
    (error (e)
      (note-compile-error (list file e))
      (format t "Regression: compile-file of ~a failed with ~a~%" file e))))
