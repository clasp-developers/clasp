(defpackage #:koga
  (:use #:common-lisp)
  (:nicknames #:k)
  (:documentation "A lisp based metabuilder for Clasp.")
  (:export #:archive
           #:configure
           #:*extensions*
           #:framework
           #:help
           #:includes
           #:defines
           #:library
           #:make-source
           #:make-source-output
           #:recurse
           #:source-path
           #:sources
           #:systems))

