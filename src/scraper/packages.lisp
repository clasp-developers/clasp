(defpackage "CSCRAPE"
  (:use :cl)
  (:import-from #:alexandria
                #:define-constant
                #:starts-with-subseq)
  (:export
   #:+begin-tag+
   #:+end-tag+
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
   #:generate-sif
   #:generate-headers
   #:extract-function-name-from-signature
   #:maybe-remove-one-prefix-from-start
   #:extract-class-method-name-from-signature
   #:extract-method-name-from-signature
   #:extract-method-name-from-pointer
   #:packaged-class-name
   #:extract-function-name-from-pointer
   #:magic-name
   #:lispify-symbol-name
   ))

(defpackage "TAGS"
  (:use :cl)
  (:export
   #:tag
   #:*tag-handlers*
   #:tag-code
   #:identifier
   #:source-pos
   #:unknown-tag
   #:handler-code
   #:forwards-tag
   #:forwards%
   #:cl-initializer-tag
   #:cl-expose-tag
   #:cl-terminator-tag
   #:cl-pregcstartup-tag
   #:lisp-base-tag
   #:lisp-class-tag
   #:lisp-internal-class-tag
   #:lisp-external-class-tag
   #:cl-begin-enum-tag
   #:cl-value-enum-tag
   #:cl-end-enum-tag
   #:symbol%
   #:description%
   #:value%
   #:base%
   #:namespace%
   #:namespace
   #:class%
   #:package%
   #:c++-class%
   #:class-symbol%
   #:name-base-tag
   #:cl-name-tag
   #:cl-lispify-name-tag
   #:packaged-name
   #:internal-code-tag
   #:external-code-tag
   #:package-use-tag
   #:gc-managed-type-tag
   #:package-shadow-tag
   #:package-nickname-tag
   #:cl-pkg-name-tag
   #:name%
   #:c++type%
   #:full-name
   #:meta-class-tag
   #:meta-class%
   #:maybe-meta-class
   #:cl-lambda-tag
   #:lambda-list%
   #:cl-docstring-tag
   #:cl-docstring-long-tag
   #:docstring%
   #:docstring-long%
   #:cl-unwind-coop-tag
   #:coop%
   #:cl-priority-tag
   #:priority%
   #:maybe-priority
   #:cl-declare-tag
   #:declare-form%
   #:source-tag
   #:external-code-tag
   #:internal-code-tag
   #:pointer%
   #:type%
   #:class-name%
   #:method-name%
   #:file%
   #:line%
   #:character-offset%
   #:cl-defun-tag
   #:cl-defun-setf-tag
   #:cl-defmethod-tag
   #:clasp-defmethod-tag
   #:cl-def-class-method-tag
   #:cl-extern-defun-tag
   #:cl-extern-defmethod-tag
   #:function-name%
   #:signature-text%
   #:symbol-tag
   #:symbol-internal-tag
   #:symbol-external-tag
   #:symbol-shadow-external-tag
   #:detailed-symbol-external-tag
   #:symbol-intern-tag
   #:class-symbol%
   #:exported%
   #:shadow%
   #:c++-name%
   #:lisp-name%
   #:namespace-tag
   #:class-tag
   #:namespace-package-association-tag
   #:package%
   #:package-str%
   #:maybe-lambda-list
   #:maybe-docstring
   #:maybe-docstring-long
   #:maybe-unwind-coop
   #:maybe-declare)
  ;; Static analyzer tags
  (:export
   #:variable-bit-array0 #:integral-value #:offset-base-ctype #:field-names
   #:end-field-names #:length-field-names
   #:variable-array0 #:variable-capacity #:ctype
   #:offset-type-cxx-identifier #+(or)#:ctype-key
   #:fixup-ctype-offset-type-key #:fixup-ctype-key
   #:layout-offset-field-names #:variable-field-only #:fixup-type
   #:variable-field #:fixed-field #:offset-ctype #:offset-base-ctype
   #:class-kind #:stamp-name #:stamp-key
   #:parent-class #:lisp-class-base #:root-class
   #:stamp-wtag #:definition-data
   #:container-kind
   #:bitunit-container-kind #:bitwidth
   #:templated-kind))
