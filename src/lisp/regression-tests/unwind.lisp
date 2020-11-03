(in-package #:clasp-tests)

;; Assert that this compilation does not invoke any unwinds.
(test compile-file-no-unwind
      (let ((unwinds (gctools:thread-local-unwinds)))
        (cmp::compile-file-serial "sys:kernel;lsp;predlib.lsp")
        (= unwinds (gctools:thread-local-unwinds))))
