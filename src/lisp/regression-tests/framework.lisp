(defpackage #:clasp-tests
    (:use :cl)
  (:export #:test #:test-expect-error))

(in-package #:clasp-tests)

(defparameter *passes* 0)
(defparameter *fails* 0)
(defparameter *failed-tests* nil)
(defparameter *expected-failures* nil)
(defparameter *files-failed-to-compile* nil)
(defparameter *test-marker-table* (make-hash-table))
(defparameter *duplicate-tests* nil)

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

(defvar *all-runtime-errors* nil)

(defun %fail-test-with-error (name form expected error description)
  (declare (ignore expected)) ; maybe display later?
  (push (list name error) *all-runtime-errors*)
  (incf *fails*)
  (push name *failed-tests*)
  (format t "~&Failed ~s~%Unexpected error~%~t~a~%while evaluating~%~t~a~%"
          name error form)
  (when description (format t "~s~%" description)))

(defun %fail-test (name form expected actual description test)
  (incf *fails*)
  (push name *failed-tests*)
  (format t "~&Failed ~s~%Wanted values ~s to~%~{~t~a~%~}but got~%~{~t~a~%~}"
          name test expected actual)
  (format t "while evaluating~%~t~a~%" form)
  (when description (format t "~s~%" description)))

(defun %succeed-test (name)
  (format t "~&Passed ~s~%" name)
  (incf *passes*))

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

(defmacro test-type (name form type &key description)
  `(test ,name (values ,form) (,type) :test 'typep :description ,description))

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
