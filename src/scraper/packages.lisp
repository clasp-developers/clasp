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
   #:lambda-list
   #:docstring-tag
   #:docstring
   #:declare-tag
   #:source-tag-mixin
   #:expose-code-tag
   #:file
   #:line
   #:character-offset
   #:namespace
   #:expose-function-tag
   #:function-name
   #:signature-text
   #:namespace-tag
   #:symbol-internal-tag
   #:symbol-external-tag
   #:intern-tag
   #:namespace-package-association-tag
   #:make-handler-hash-table))
           
