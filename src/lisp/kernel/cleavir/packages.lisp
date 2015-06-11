(cl:in-package #:common-lisp-user)

(defpackage #:clasp-cleavir
  (:use #:common-lisp #:core)
  (:nicknames #:cc)
  (:export
   #:cleavir-compile-eval
   #:clasp
   #:invoke-instruction
   #:invoke-multiple-value-call-instruction
   #:*debug-log*
   #:instruction-gid
   #:datum-gid
   #:create-landing-pad
   #:translate-datum
   #:convert-funcalls
   #:finalize-unwind-and-landing-pad-instructions
   #:cleavir-compile
   #:cleavir-compile-file
   #:cclasp-compile-in-env
))

(defpackage #:clasp-cleavir-generate-ast
  (:nicknames #:cc-generate-ast))


(defpackage #:clasp-cleavir-ast
  (:nicknames #:cc-ast)
  (:use #:common-lisp)
  (:export 
   #:hoist-load-time-value
   #:precalculated-value-ast
   #:make-precalc-vector-function-ast
   #:named-function-ast
   #:debug-message-ast
   #:make-throw-ast
   #:cleanup-ast
   #:make-setf-fdefinition-ast
   #:lambda-name
   #:debug-message
   #:precalc-symbol-reference-ast
   #:precalc-symbol-reference-index
   #:precalc-symbol-reference-ast-original-object
   #:precalc-value-reference-ast
   #:precalc-value-reference-index
   #:precalc-value-reference-ast-original-object
   #:setf-fdefinition-ast
   #:throw-ast
   #:result-ast
   #:tag-ast
   #:datum-id
   ))

(defpackage #:clasp-cleavir-hir
  (:use #:common-lisp)
  (:export 
   #:precalc-reference-instruction
   #:named-enter-instruction
   #:landing-pad-named-enter-instruction
   #:frame-holder
   #:indexed-unwind-instruction
   #:landing-pad-instruction
   #:landing-pad-return-instruction
   #:landing-pad
   #:jump-id
   #:make-named-enter-instruction
   #:debug-message-instruction
   #:make-precalc-symbol-instruction
   #:make-precalc-value-instruction
   #:invoke-instruction
   #:make-setf-fdefinition-instruction
   #:make-throw-instruction
   #:lambda-name
   #:precalc-symbol-instruction
   #:precalc-value-instruction
   #:debug-message
   #:setf-fdefinition-instruction
   #:throw-instruction
   ))

(defpackage #:clasp-cleavir-ast-to-hir
  (:use #:common-lisp)
  (:export
   #:*landing-pad*)
)

(defpackage #:cc-generate-ast
  (:use #:common-lisp)
  )

(defpackage #:cc-hir-to-mir
  (:use #:common-lisp)
)

(defpackage #:cc-mir
  (:use #:common-lisp)
  (:export 
   #:enter-instruction
   #:closure-pointer-dynamic-lexical-location
   #:describe-mir
   #:assign-mir-instruction-datum-ids
   ))

(defpackage #:cleavir-ir-gml
  (:use #:common-lisp #:cleavir-ir)
  (:export
   #:draw-flowchart))

(defpackage #:lisp-executable.creation
  (:use #:common-lisp))

