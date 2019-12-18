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
   #:rest-alloc
   #:debug-message-ast
   #:debug-break-ast
   #:multiple-value-foreign-call-ast
   #:foreign-call-ast
   #:foreign-call-pointer-ast
   #:argument-asts
   #:function-name
   #:foreign-types
   #:defcallback-ast #:defcallback-args
   #:header-stamp-case-ast #:make-header-stamp-case-ast
   #:stamp-ast #:derivable-ast #:rack-ast #:wrapped-ast #:header-ast
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
   #:vector-length-ast
   #:displacement-ast
   #:displaced-index-offset-ast
   #:array-total-size-ast
   #:array-rank-ast
   #:array-dimension-ast
   #:vaslist-pop-ast
   #:vaslist-length-ast
   #:header-stamp-ast #:rack-stamp-ast
   #:wrapped-stamp-ast #:derivable-stamp-ast
   #:bind-va-list-ast #:make-bind-va-list-ast #:va-list-ast
   #:cmp-ast
   #:cas-car-ast #:cas-cdr-ast #:slot-cas-ast
   #:invoke-ast #:multiple-value-invoke-ast #:destinations
   #:introduce-invoke
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
   #:rest-alloc
   #:make-named-enter-instruction
   #:debug-message-instruction
   #:debug-break-instruction
   #:save-values-instruction #:make-save-values-instruction
   #:load-values-instruction #:make-load-values-instruction
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
   #:precalc-value-instruction #:precalc-value-instruction-p
   #:debug-message
   #:debug-break
   #:setf-fdefinition-instruction
   #:throw-instruction
   #:precalc-value-instruction-index
   #:precalc-value-instruction-original-object
   #:instruction-id
   #:vector-length-instruction
   #:displacement-instruction
   #:displaced-index-offset-instruction
   #:array-total-size-instruction
   #:array-rank-instruction
   #:array-dimension-instruction
   #:header-stamp-instruction #:rack-stamp-instruction
   #:wrapped-stamp-instruction #:derivable-stamp-instruction
   #:vaslist-pop-instruction #:make-vaslist-pop-instruction
   #:vaslist-length-instruction #:make-vaslist-length-instruction
   #:bind-va-list-instruction #:make-bind-va-list-instruction
   #:defcallback-instruction #:defcallback-args
   #:header-stamp-case-instruction #:make-header-stamp-case-instruction
   #:cas-car-instruction #:cas-cdr-instruction #:slot-cas-instruction
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
   #:generalp-instruction
   #:make-generalp-instruction
   #:headerq-instruction
   #:header-value-min-max
   #:make-headerq-instruction
   #:save-frame-instruction
   #:make-save-frame-instruction
   #:memcas2-instruction
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

