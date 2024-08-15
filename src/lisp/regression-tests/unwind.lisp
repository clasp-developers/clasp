(in-package #:clasp-tests)

(let ((lockedcl (ext:package-locked-p "COMMON-LISP"))
      (lockedcore (ext:package-locked-p "CORE")))
  (ext:unlock-package "CORE")
  (ext:unlock-package "CL")
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

  (when lockedcl (ext:lock-package "COMMON-LISP"))
  (when lockedcore (ext:lock-package "CORE"))
)
