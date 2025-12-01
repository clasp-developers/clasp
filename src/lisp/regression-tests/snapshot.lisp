(in-package #:clasp-tests)

#+use-precise-gc
(test slad-snapshot
      (let ((binary (ext:argv 0))
            (snap-fname
              (namestring
               (translate-logical-pathname "sys:src;lisp;regression-tests;testsnap")))
            (ostream (make-string-output-stream)))
        (multiple-value-bind (stream dump-code)
            (ext:run-program binary
                             (list "--norc" "--base" "--feature" "ignore-extensions"
                                   "--non-interactive"
                                   "--eval" "(defparameter *foo* 89)"
                                   "--eval"
                                   (format nil "(ext:save-lisp-and-die \"~a\")" snap-fname))
                             :output ostream)
          (declare (ignore stream))
          (if (eql dump-code 0)
              (multiple-value-bind (stream code)
                  (ext:run-program binary
                                   (list "--snapshot" (format nil "~a" snap-fname)
                                         "--eval" "(ext:quit *foo*)")
                                   :output ostream)
                (declare (ignore stream))
                (unless (eql code 89)
                  (write-string (get-output-stream-string ostream) *error-output*))
                (delete-file snap-fname)
                (values dump-code code))
              (progn
                (write-string (get-output-stream-string ostream) *error-output*)
                (values dump-code nil)))))
      (0 89))

#+use-precise-gc
(test slad-executable
      (let ((binary (ext:argv 0))
            (snap-fname
              (namestring
               (translate-logical-pathname "sys:src;lisp;regression-tests;testexec")))
            (ostream (make-string-output-stream)))
        (multiple-value-bind (stream dump-code)
            (ext:run-program binary
                             (list "--norc" "--base" "--feature" "ignore-extensions"
                                   "--non-interactive"
                                   "--eval" "(defparameter *foo* 90)"
                                   "--eval"
                                   (format nil "(ext:save-lisp-and-die \"~a\" :executable t)"
                                           snap-fname))
                             :output ostream)
          (declare (ignore stream))
          (if (eql dump-code 0)
              (multiple-value-bind (stream code)
                  (ext:run-program snap-fname '("--eval" "(ext:quit *foo*)")
                                   :output ostream)
                (declare (ignore stream))
                (unless (eql code 90)
                  (write-string (get-output-stream-string ostream) *error-output*))
                (delete-file snap-fname)
                (values dump-code code))
              (progn
                (write-string (get-output-stream-string ostream) *error-output*)
                (values dump-code nil)))))
      (0 90))
