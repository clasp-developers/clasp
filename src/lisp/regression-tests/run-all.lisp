(in-package :cl-user)

(declaim (optimize (safety 3)))

(let ((compiled-file
        (compile-file "sys:src;lisp;regression-tests;framework.lisp")))
  (if compiled-file
       (load compiled-file)
       (error "Could not compile ~s~%" "sys:src;lisp;regression-tests;framework.lisp")))

(load "sys:src;lisp;regression-tests;set-unexpected-failures.lisp")

(in-package #:clasp-tests)

;;; ------------------------------------------------------------
;;; Run tests
(reset-clasp-tests)

(defvar *suites*
  '("defcallback-native"
    "lowlevel"
    "fastgf"
    "array0"
    "tests01"
    "finalizers"
    "strings01"
    "cons01"
    "sequences01"
    "clos"
    "mop"
    "update-instance-abort"
    "numbers"
    "ehkiller"
    "package"
    "structures"
    "symbol0"
    "string-comparison0"
    "bit-array0"
    "bit-array1"
    "character0"
    #+unicode "unicode"
    "hash-tables0"
    "misc"
    "read01"
    "printer01"
    "streams01"
    "environment01"
    "types01"
    "control01"
    "iteration"
    "loop"
    "numbers-core"
    "unwind"
    #+unicode "encodings"
    "environment"
    "conditions"
    "float-features"
    "debug"
    "mp"
    "interrupt"
    "posix"
    "btb"
;;; When we have system construction before debug.lisp, debug.lisp will fail
    "system-construction"
    "extensions"
    "run-program"
    "snapshot"))

(loop with requested-suites = (core:split (or (ext:getenv "TEST_SUITES") "") ",")
      for suite in *suites*
      finally (sys:quit (if (show-test-summary) 0 1))
      when (or (null requested-suites)
               (member suite requested-suites :test #'equal))
        do (message :emph "~%Running ~a suite..." suite)
           (load-if-compiled-correctly (merge-pathnames #P"sys:src;lisp;regression-tests;"
                                                        (make-pathname :name suite
                                                                       :type "lisp"))))
