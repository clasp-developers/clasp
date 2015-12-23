(defpackage "CSCRAPE"
    (:use :cl)
    (:export
     #:*begin-tag*
     #:*end-tag*
     #:buffer-stream
     #:buffer
     #:search-for-element
     #:skip-char
     #:search-for-character
     #:search-for-whitespace
     #:read-string-to-character
     #:read-string-to-white-space
     #:next-tag-name
     #:read-string-to-tag
     #:gather-source-files
     ))

(defpackage "TAGS"
  (:use :cl)
  (:export
   #:*tag-handlers*
   #:tag-code
   #:tag-identifier
   #:tag-source-pos
   #:unknown-tag
   #:handler-code
   #:lisp-base-tag
   #:lisp-class-tag
   #:c++-base%
   #:namespace%
   #:package%
   #:c++-class%
   #:class-symbol%
   #:name-tag
   #:name%
   #:lambda-tag
   #:lambda-list
   #:docstring-tag
   #:docstring%
   #:declare-tag
   #:declare-form%
   #:source-tag
   #:extern-defun-tag
   #:extern-defmethod-tag
   #:pointer%
   #:type%
   #:expose-code-tag
   #:expose-method-tag
   #:class-name%
   #:method-name%
   #:file%
   #:line%
   #:character-offset%
   #:expose-function-tag
   #:function-name%
   #:signature-text%
   #:symbol-tag
   #:c++-name%
   #:class-symbol%
   #:exported%
   #:namespace-package-association-tag
   #:package-var))
           
