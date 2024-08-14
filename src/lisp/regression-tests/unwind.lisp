(in-package #:clasp-tests)

(let ((lockedcl (si:package-locked-p "COMMON-LISP")) (lockedcore (si:package-locked-p "CORE")))
  (core:package-unlock "CORE")
  (core:package-unlock "CL")
  ;; Assert that this compilation does not invoke any unwinds.
  (test compile-file-no-unwind
        (let ((unwinds (gctools:thread-local-unwinds)))
          (compile-file "sys:src;lisp;kernel;lsp;predlib.lisp"
                        :execution :serial
                        :output-file (make-pathname
                                      :type (pathname-type (compile-file-pathname "foo.lisp"))
                                      :defaults (core:mkstemp "/tmp/predlib")))
          (- (gctools:thread-local-unwinds) unwinds)
          )
        (0)
  )

  (if lockedcl (si::package-lock "COMMON-LISP"))
  (if lockedcore (si::package-lock "CORE"))
)
