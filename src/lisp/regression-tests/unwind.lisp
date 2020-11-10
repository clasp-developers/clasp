(in-package #:clasp-tests)

;; Assert that this compilation does not invoke any unwinds.
(test compile-file-no-unwind
      (let ((unwinds (gctools:thread-local-unwinds)))
        (cmp::compile-file-serial "sys:kernel;lsp;predlib.lsp"
                                  :output-file (make-pathname
                                                :type (pathname-type (compile-file-pathname "foo.lisp"))
                                                :defaults (core:mkstemp "TMP:predlib")))
        (= unwinds (gctools:thread-local-unwinds))))
