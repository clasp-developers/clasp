(cl:in-package #:common-lisp-user)

(asdf:defsystem :clasp-analyzer
    :components
  ((:file "clang-tool")
   (:file "mps-interface")))
