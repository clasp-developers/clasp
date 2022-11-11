(in-package #:clasp-tests)

;;;; This file is based on ECL's run-program.lsp test file.
;;;; We especially copy ECL's idea of calling Clasp itself,
;;;; which after all definitely exists on the system, and
;;;; which we control the interface to.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *binary* (si:argv 0))
  (defparameter *program-filename* "sys:src;lisp;regression-tests;external-process-programs.lisp"))

(defmacro with-run-program ((name args &rest params) &body body)
  "Run Clasp with the given args, with the params passed to run-program.
Then run the body while the process is running.
Return the result of the body and the results of external-process-wait."
  `(multiple-value-bind (,name code process)
       (ext:run-program *binary*
                        '("--norc"
                          "--feature" "ignore-extensions"
                          "--eval" ,(format nil "(defparameter *args-number* ~d)" (+ 14 (length args)))
                          "--eval" "(setf *load-verbose* nil)"
                          "--load" ,*program-filename*
                          "--eval" ,(format nil "(~a)" name)
                          "--quit"
                          "--" ,@args)
                        ,@params
                        :wait nil)
     (declare (ignorable ,name code))
     (multiple-value-call #'values (progn ,@body) (ext:external-process-wait process t))))

(defun slurp (stream)
  (loop for last = nil then line
        for line = (read-line stream nil :eof)
        until (eql line :eof)
        finally (return last)))

(test run-program-argcount
      (with-run-program (argcount-test ("a" "b c" "d \\" "e\ 4\\
")))
      (nil :exited 0))

(test output-streams.1
      (with-run-program (print-test () :output :stream :error :stream)
        ;; FIXME export
        (let ((print-test-err (ext::external-process-error-stream process)))
          (values (slurp print-test) (slurp print-test-err))))
      ("Hello stdout" "Hello stderr" :exited 0))

(test output-streams.2
      (with-run-program (print-test () :output :stream :error :output)
        (let ((print-test-err (ext::external-process-error-stream process)))
          ;; stdout and stderr are redirected to the same stream.
          ;; So the first slurp will read both lines, leaving nothing for
          ;; the second slurp.
          (values (slurp print-test) (slurp print-test-err))))
      ("Hello stderr" nil :exited 0))

(test interactive-input.1
      (with-run-program (io/err nil)
        (format io/err "42~%"))
      (nil :exited 0))

(test interactive-input.2
  ;; process will have :eof on input and should quit with "1"
      (with-run-program (io/err nil :input nil))
      (nil :exited 1))

(test non-fd-streams
      ;; Run a program with string streams for output, and check that something
      ;; was output to them.
      (block nil
        (with-output-to-string (output-stream)
          (with-output-to-string (error-stream)
            ;; note the space: otherwise reader waits for next character
            (with-input-from-string (input-stream "42 ")
              (with-run-program (io/err nil
                                        :input input-stream
                                        :output output-stream
                                        :error error-stream)))
            (return (values (zerop (length (get-output-stream-string output-stream)))
                            (zerop (length (get-output-stream-string error-stream))))))))
  (nil nil))

(test empty-string-input-stream
      ;; Run a program with an empty string input stream and check that
      ;; something was output.
      (block nil
        (with-output-to-string (output-stream)
          (with-output-to-string (error-stream)
            (with-input-from-string (input-stream "")
              (with-run-program (io/err nil
                                        :input input-stream
                                        :output output-stream
                                        :error error-stream
                                        :wait t)
                (return (values (zerop (length (get-output-stream-string output-stream)))
                                (zerop (length (get-output-stream-string error-stream))))))))))
  (nil nil))
