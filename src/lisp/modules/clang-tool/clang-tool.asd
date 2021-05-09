(in-package :asdf-user)

(defsystem "clang-tool"
  :description "Clang ASTMatcher library stuff"
  :version "0.0.1"
  :author "Christian Schafmeister <chris.schaf@verizon.net>"
  :licence "LGPL-3.0"
  :depends-on ( )
  :serial t
  :components
  ((:file "packages")
   (:file "clang-tool")
   ))
