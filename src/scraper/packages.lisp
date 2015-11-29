(defpackage "CSCRAPE"
    (:use :cl)
    (:export
     #:*begin-tag*
     #:*end-tag*
     #:buffer-stream
     #:buffer
     #:search-for-tag
     #:skip-char
     #:search-for-character
     #:read-string-to-character
     #:next-tag-name
     #:read-string-to-tag
     #:skip-tag
     ))

(defpackage "TAGS"
  (:use :cl)
  (:export
   #:unknown-tag
   #:handler-code
   #:lambda-tag
   #:docstring-tag
   #:declare-tag
   #:expose-function-tag
   #:namespace-tag
   #:make-handler-hash-table))
           
