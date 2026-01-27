(in-package :asdf-user)

(defsystem "framework-tests"
  :description "Run tests"
  :version "0.0.1"
  :author "Karsten"
  :licence "LGPL-3.0"
  :serial t
  :components ((:file "framework")))
