(in-package #:clasp-tests)

(let ((lockedcl (si::package-is-locked "COMMON-LISP")) (lockedcore (si::package-is-locked "CORE")))
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
