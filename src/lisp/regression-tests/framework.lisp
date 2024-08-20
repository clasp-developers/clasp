(defpackage #:clasp-tests
    (:use :cl)
  (:export #:test #:test-expect-error))

(in-package #:clasp-tests)

(defparameter *expected-failed-tests* nil)
(defparameter *unexpected-failed-tests* nil)
(defparameter *expected-passed-tests* nil)
(defparameter *unexpected-passed-tests* nil)
(defparameter *expected-failures* nil)
(defparameter *files-failed-to-compile* nil)
(defparameter *test-marker-table* (make-hash-table))
(defparameter *duplicate-tests* nil)

(defun reset-clasp-tests ()
  (setq *expected-failed-tests* nil
        *unexpected-failed-tests* nil
        *expected-passed-tests* nil
        *unexpected-passed-tests* nil
        *files-failed-to-compile* nil
        *test-marker-table* (make-hash-table)
        *duplicate-tests* nil))

(defun note-test (name)
  (cond ((gethash name *test-marker-table*)
         (push name *duplicate-tests*)
         (warn "~%Duplicate test ~a~%" name))
        (t (setf (gethash name *test-marker-table*) t)))) 

(defun note-compile-error (file&error)
  (push file&error *files-failed-to-compile*))
  
(defun show-test-summary ()
  (format t "~@[~%Failures:~%  ~/pprint-fill/~%~]~
~@[~%Unexpected Successes:~%  ~/pprint-fill/~%~]~
~@[~%Expected Failures:~%  ~/pprint-fill/~%~]
Successes: ~d~%"
          (reverse *unexpected-failed-tests*) (reverse *unexpected-passed-tests*)
          (reverse *expected-failed-tests*) (length *expected-passed-tests*))
  (when *files-failed-to-compile*
    (dolist (file&error *files-failed-to-compile*)
      (format t "Compilation error for file ~a with error  ~a~%" (first file&error)(second file&error))))
  (when *duplicate-tests*
    (dolist (test *duplicate-tests*)
      (format t "Duplicate test ~a~%" test)))
  (not *unexpected-failed-tests*))

(defvar *all-runtime-errors* nil)

(defun %fail-test-with-error (name form expected error description)
  (declare (ignore expected)) ; maybe display later?
  (push (list name error) *all-runtime-errors*)
  (if (member name *expected-failures*)
      (push name *expected-failed-tests*)
      (push name *unexpected-failed-tests*))
  (format t "~&Failed ~s~%Unexpected error~%~t~a~%while evaluating~%~t~a~%"
          name error form)
  (when description (format t "~s~%" description)))

(defun %fail-test (name form expected actual description test)
  (if (member name *expected-failures*)
      (push name *expected-failed-tests*)
      (push name *unexpected-failed-tests*))
  (format t "~&Failed ~s~%Wanted values ~s to~%~{~t~a~%~}but got~%~{~t~a~%~}"
          name test expected actual)
  (format t "while evaluating~%~t~a~%" form)
  (when description (format t "~s~%" description)))

(defun %succeed-test (name)
  (if (member name *expected-failures*)
      (push name *unexpected-passed-tests*)
      (push name *expected-passed-tests*))
  (format t "~&Passed ~s~%" name))

(defun %test (name form thunk expected &key description (test 'equalp))
  (note-test name)
  (multiple-value-bind (results error)
      (ignore-errors (values (multiple-value-list (funcall thunk)) nil))
    (cond (error (%fail-test-with-error name form expected error description))
          ((and (= (length expected) (length results))
                (every test results expected))
           (%succeed-test name))
          (t (%fail-test name form expected results description test)))))

(defmacro test (name form expected &key description (test ''equalp))
  `(%test ',name ',form (lambda () ,form) ',expected
          :description ,description :test ,test))

(defmacro test-expect-error (name form &key (type 'error) description)
  `(test ,name
         (ignore-errors (values (multiple-value-list ,form) nil))
         (null ,type)
         :test 'typep
         :description ,description))

(defmacro test-true (name form &key description)
  `(test ,name (not (not ,form)) (t) :description ,description))

(defmacro test-nil (name form &key description)
  `(test ,name ,form (nil) :description ,description))

(defmacro test-type (name form type &key description)
  `(test ,name (values ,form) (,type) :test 'typep :description ,description))

(defmacro test-finishes (name form &key description)
  `(test ,name (handler-case (progn ,form nil)
                 (serious-condition (e) e))
         (nil)
         :description ,description))

(defun load-if-compiled-correctly (file)
  (handler-case
      (multiple-value-bind
            (fasl warnings-p failure-p)
          (let (#+(or) (cmp::*compile-file-parallel* t))
            (compile-file file))
        (declare (ignore warnings-p failure-p))
        (when fasl
          (load fasl)))
    (error (e)
      (note-compile-error (list file e))
      (format t "Regression: compile-file of ~a failed with ~a~%" file e))))

(defun no-handler-case-load-if-compiled-correctly (file)
  (multiple-value-bind
        (fasl warnings-p failure-p)
      (let (#+(or) (cmp::*compile-file-parallel* t))
        (compile-file file))
    (declare (ignore warnings-p failure-p))
    (when fasl
      (load fasl))))
