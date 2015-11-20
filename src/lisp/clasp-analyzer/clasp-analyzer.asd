(cl:in-package #:asdf-user)

(asdf:defsystem :clasp-analyzer
    :components
  ((:file "packages")
   (:file "clang-tool")
   (:file "mps-interface")))
