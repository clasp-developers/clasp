(cl:in-package #:common-lisp-user)

(defpackage #:cclasp-build
  (:use #:common-lisp #:core)
  (:export
   #:compile-full-cclasp
   #:link))

(defpackage #:clasp-cleavir
  (:use #:common-lisp #:core)
  (:nicknames #:cc #:clasp-cleavir-translate-bir)
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
   #:precalculated-value-ast
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
   #:instance-rack-ast #:instance-rack-set-ast
   #:rack-read-ast #:rack-write-ast
   #:make-throw-ast
   #:cleanup-ast
   #:make-setf-fdefinition-ast
   #:debug-message
   #:debug-break
   #:precalc-value-reference-ast
   #:precalc-value-reference-ast-index
   #:precalc-value-reference-ast-form
   #:precalc-constant-reference-ast
   #:precalc-constant-reference-ast-value
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
   #:bind-va-list-ast #:rest-alloc #:make-bind-va-list-ast #:va-list-ast
   #:cmp-ast
   #:cas-car-ast #:cas-cdr-ast #:slot-cas-ast #:acas-ast
   #:bind-ast
   #:unwind-protect-ast #:cleanup-ast
   #:invoke-ast #:multiple-value-invoke-ast #:destinations
   #:introduce-invoke
   ))

(defpackage #:clasp-cleavir-hir
  (:use #:common-lisp)
  (:export 
   #:precalc-reference-instruction
   #:frame-holder
   #:landing-pad
   #:jump-id
   #:lambda-list
   #:rest-alloc
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
   #:precalc-value-instruction #:precalc-value-instruction-p
   #:debug-message
   #:debug-break
   #:setf-fdefinition-instruction
   #:throw-instruction
   #:precalc-value-instruction-index
   #:precalc-value-instruction-form
   #:instruction-id
   #:vector-length-instruction
   #:displacement-instruction
   #:displaced-index-offset-instruction
   #:array-total-size-instruction
   #:array-rank-instruction
   #:array-dimension-instruction
   #:header-stamp-instruction #:rack-stamp-instruction
   #:wrapped-stamp-instruction #:derivable-stamp-instruction
   #:instance-rack-instruction #:instance-rack-set-instruction
   #:rack-read-instruction #:rack-write-instruction
   #:vaslist-pop-instruction #:make-vaslist-pop-instruction
   #:vaslist-length-instruction #:make-vaslist-length-instruction
   #:bind-va-list-instruction #:make-bind-va-list-instruction
   #:defcallback-instruction #:defcallback-args
   #:header-stamp-case-instruction #:make-header-stamp-case-instruction
   #:cas-car-instruction #:cas-cdr-instruction #:slot-cas-instruction
   #:acas-instruction
   #:bind-instruction #:unwind-protect-instruction
   ))

(defpackage #:clasp-cleavir-bir
  (:use #:cl)
  (:nicknames #:cc-bir)
  (:shadow #:unwind-protect)
  (:export #:precalc-value #:precalc-value-index #:precalc-constant
           #:unwind-protect #:bind #:header-stamp-case
           #:foreign-call-pointer #:foreign-types
           #:defcallback #:defcallback-args
           #:mv-foreign-call #:function-name
           #:acas #:element-type #:simple-p #:boxed-p))

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

(defpackage #:cc-bir-to-bmir
  (:use #:cl)
  (:export #:reduce-module-typeqs)
  (:export #:reduce-module-primops))

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
   #:clasp-save-values-instruction
   #:clasp-load-values-instruction
   #:memcas2-instruction
   #:enter-instruction
   #:typed-lexical-location #:lexical-location-type
   #:closure-pointer-dynamic-lexical-location
   #:describe-mir
   #+stealth-gids #:assign-mir-instruction-datum-ids
   ))

(defpackage #:clasp-cleavir-bmir
  (:nicknames #:cc-bmir)
  (:shadow #:characterp #:consp #:load)
  (:export #:fixnump #:characterp #:consp #:single-float-p #:generalp
           #:headerq #:info)
  (:export #:memref2 #:offset #:load #:store))

(defpackage #:cleavir-ir-gml
  (:use #:common-lisp #:cleavir-ir)
  (:export
   #:draw-flowchart))

(defpackage #:lisp-executable.creation
  (:use #:common-lisp))

