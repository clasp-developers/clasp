(cl:in-package #:common-lisp-user)

(defpackage #:cclasp-build
  (:use #:common-lisp #:core)
  (:export
   #:compile-full-cclasp
   #:link))

(defpackage #:clasp-cleavir
  (:use #:common-lisp #:core)
  (:nicknames #:cc)
  (:export
   #:*use-cst*
   #:literal
   #:literal-label
   #:immediate-literal
   #:arrayed-literal
   #:%literal-index
   #:*clasp-ordinary-lambda-list-grammar*
   #:*use-type-inference*
   #:cleavir-compile-eval
   #:compile-cst-or-form
   #:clasp
   #:invoke-instruction
   #:invoke-multiple-value-call-instruction
   #:*debug-log*
   #:*debug-final-gml*
   #:*debug-cleavir*
   #:*debug-cleavir-literals*
   #+stealth-gids :instruction-gid
   #:unsafe-multiple-value-foreign-call
   #:unsafe-foreign-call
   #:unsafe-foreign-call-pointer
   #:datum-gid
   #:create-landing-pad
   #:translate-datum
   #:convert-funcalls
   #:finalize-unwind-and-landing-pad-instructions
   #:optimize-stack-enclose
   #:cleavir-compile
   #:cleavir-compile-file
   #:cclasp-compile-in-env
   #:*function-inline-asts*
   #:*clasp-env*
   #:*clasp-system*
   #:*code-walker*
   #:alloca-i8
   #:inline-ast
))

(defpackage #:cc-generate-ast)


(defpackage #:clasp-cleavir-ast
  (:nicknames #:cc-ast)
  (:use #:common-lisp)
  (:export 
   #:hoist-load-time-value
   #:precalculated-value-ast
   #:make-precalc-vector-function-ast
   #:named-function-ast
   #:original-lambda-list
   #:docstring
   #:debug-message-ast
   #:debug-break-ast
   #:multiple-value-foreign-call-ast
   #:foreign-call-ast
   #:foreign-call-pointer-ast
   #:argument-asts
   #:function-name
   #:foreign-types
   #:make-throw-ast
   #:cleanup-ast
   #:make-setf-fdefinition-ast
   #:lambda-name
   #:debug-message
   #:debug-break
   #:precalc-value-reference-ast
   #:precalc-value-reference-ast-index
   #:precalc-value-reference-ast-original-object
   #:setf-fdefinition-ast
   #:throw-ast
   #:result-ast
   #:tag-ast
   #:datum-id
   #:vector-length-ast #:vl-ast-vector
   #:displacement-ast #:displacement-ast-mdarray
   #:displaced-index-offset-ast #:displaced-index-offset-ast-mdarray
   #:array-total-size-ast #:array-total-size-ast-mdarray
   #:array-rank-ast #:array-rank-ast-mdarray
   #:array-dimension-ast #:array-dimension-ast-mdarray #:array-dimension-ast-axis
   #:bind-va-list-ast #:make-bind-va-list-ast #:va-list-ast
   ))

(defpackage #:clasp-cleavir-hir
  (:use #:common-lisp)
  (:export 
   #:precalc-reference-instruction
   #:named-enter-instruction
   #:frame-holder
   #:landing-pad
   #:jump-id
   #:lambda-list
   #:original-lambda-list
   #:docstring
   #:make-named-enter-instruction
   #:debug-message-instruction
   #:debug-break-instruction
   #:multiple-value-foreign-call-instruction
   #:foreign-call-instruction
   #:foreign-call-pointer-instruction
   #:function-name
   #:foreign-types
   #:make-multiple-value-foreign-call-instruction
   #:make-foreign-call-instruction
   #:make-foreign-call-pointer-instruction
   #:make-precalc-value-instruction
   #:invoke-instruction
   #:make-setf-fdefinition-instruction
   #:make-throw-instruction
   #:lambda-name
   #:precalc-value-instruction
   #:debug-message
   #:debug-break
   #:setf-fdefinition-instruction
   #:throw-instruction
   #:precalc-value-instruction-original-object
   #:instruction-id
   #:push-special-binding-instruction
   #:make-push-special-binding-instruction
   #:pop-special-binding-instruction
   #:make-pop-special-binding-instruction
   #:make-vector-length-instruction
   #:make-displacement-instruction
   #:make-displaced-index-offset-instruction
   #:make-array-total-size-instruction
   #:make-array-rank-instruction
   #:make-array-dimension-instruction
   #:bind-va-list-instruction #:make-bind-va-list-instruction
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
  (:export
   #:reduce-typeqs)
)

(defpackage #:cc-mir
  (:use #:common-lisp)
  (:export
   #:characterp-instruction
   #:make-characterp-instruction
   #:single-float-p-instruction
   #:make-single-float-p-instruction
   #:headerq-instruction
   #:header-value-min-max
   #:make-headerq-instruction
   #:save-frame-instruction
   #:make-save-frame-instruction
   #:assign-catch-instruction #:go-index
   #:enter-instruction
   #:typed-lexical-location #:lexical-location-type
   #:closure-pointer-dynamic-lexical-location
   #:describe-mir
   #+stealth-gids #:assign-mir-instruction-datum-ids
   ))

(defpackage #:cleavir-ir-gml
  (:use #:common-lisp #:cleavir-ir)
  (:export
   #:draw-flowchart))

(defpackage #:lisp-executable.creation
  (:use #:common-lisp))

