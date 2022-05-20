(defpackage #:koga
  (:use #:common-lisp)
  (:nicknames #:k)
  (:documentation "A lisp based metabuilder for Clasp.")
  (:export #:configure
           #:help
           #:includes
           #:library
           #:make-source
           #:make-source-output
           #:recurse
           #:source-path
           #:sources
           #:systems))

