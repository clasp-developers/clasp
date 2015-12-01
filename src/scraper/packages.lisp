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
   #:lisp-base-tag
   #:lisp-class-tag
   #:c++-base%
   #:namespace%
   #:package%
   #:c++-class%
   #:class-symbol%
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
   #:symbol-tag
   #:name%
   #:c++-name%
   #:class-symbol%
   #:exported%
   #:namespace-package-association-tag
   #:package-var
   #:make-handler-hash-table))
           
