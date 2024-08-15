(in-package #:clasp-tests)

;; Assert that this compilation does not invoke any unwinds.
(test compile-file-no-unwind
      (let ((unwinds (gctools:thread-local-unwinds)))
        (ext:with-unlocked-packages ("CL" "CORE")
          (compile-file "sys:src;lisp;kernel;lsp;predlib.lisp"
                        :execution :serial
                        :output-file (make-pathname
                                      :type (pathname-type (compile-file-pathname "foo.lisp"))
                                      :defaults (core:mkstemp "/tmp/predlib")))
          (- (gctools:thread-local-unwinds) unwinds)))
      (0))
