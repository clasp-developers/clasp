(cl:in-package #:common-lisp-user)

(asdf:defsystem :sockets
    :components
  ((:file "package")
   (:file "sockets")))
