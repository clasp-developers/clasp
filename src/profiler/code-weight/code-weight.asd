(in-package :asdf-user)

(defsystem "code-weight"
  :description "Measure inlining weight"
  :version "0.0.1"
  :author "Christian Schafmeister <chris.schaf@verizon.net>"
  :licence "LGPL-3.0"
  :depends-on ( :cl-ppcre)
  :serial t
  :components
  ((:file "packages")
   (:file "code-weight")))
