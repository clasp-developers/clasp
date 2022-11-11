;;;; This code is run by the run-program tests (run-program.lisp).
;;;; It is based on ECL's external-process-programs.lisp.

(in-package #:cl-user)

;;; FIXME: Export some kind of interface for getting program arguments.

(defmacro define-function (name &body body)
  `(defun ,name (&aux
                   (argc (si:argc))
                   (argv core:*command-line-arguments*))
     (declare (ignorable argc argv))
     ,@body))

(define-function argcount-test
    ;; *args-number* will be defined by run-program.
  (if (= argc *args-number*)
      (ext:quit 0)
      (ext:quit argc)))

(define-function print-test
  (terpri *standard-output*)
  (princ "Hello stdout" *standard-output*)
  (finish-output *standard-output*)
  (terpri *error-output*)
  (princ "Hello stderr" *error-output*)
  (finish-output *error-output*))

(define-function io/err
  (princ "Welcome to ITP(NR) - Intelligent Test Program (not really)!")
  (print argc *error-output*)
  (princ "Type your SEXP: ")
  (let ((result (read *standard-input* nil :eof)))
    (princ result *error-output*)
    (cond ((eq result :eof)
           (princ "No? Shame...")
           (ext:quit 1))
          (t
           (princ "Thank you. Your wish has been heard loud and clear.")
           (ext:quit 0)))))

(define-function terminate
  ;; This process should be killed from the outside.
  (sleep 10)
  (ext:quit 0))
