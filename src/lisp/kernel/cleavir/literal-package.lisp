(defpackage #:literal
  (:use #:cl #:core)
  (:export
   #:next-value-table-holder-name
   #:make-general-entry-placeholder
   #:make-literal-node-call
   #:make-literal-node-creator
   #:literal-node-runtime-p
   #:literal-node-runtime-object
   #:lookup-literal-index
   #:reference-literal
   #:compile-reference-to-literal
   #:constants-table-reference
   #:constants-table-value))
