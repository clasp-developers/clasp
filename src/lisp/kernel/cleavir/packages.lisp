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

(defpackage #:clasp-cleavir-translate-bir
  (:use #:cl))

(defpackage #:cc-generate-ast)

(defpackage #:clasp-cleavir-ast
  (:nicknames #:cc-ast)
  (:use #:common-lisp)
  (:export
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

(defpackage #:clasp-cleavir-bir
  (:use #:cl)
  (:nicknames #:cc-bir)
  (:shadow #:unwind-protect)
  (:export #:unwind-protect #:bind #:header-stamp-case
           #:foreign-call-pointer #:foreign-types
           #:defcallback #:defcallback-args
           #:mv-foreign-call #:function-name
           #:acas #:element-type #:simple-p #:boxed-p))

(defpackage #:cc-generate-ast
  (:use #:common-lisp)
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

(defpackage #:lisp-executable.creation
  (:use #:common-lisp))

