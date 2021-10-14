(cl:in-package #:common-lisp-user)

(defpackage #:clasp-cleavir
  (:use #:common-lisp #:core)
  (:nicknames #:cc #:clasp-cleavir-translate-bir)
  (:local-nicknames (#:bir #:cleavir-bir)
                    (#:bir-transformations #:cleavir-bir-transformations)
                    (#:ast #:cleavir-ast)
                    (#:cst-to-ast #:cleavir-cst-to-ast)
                    (#:env #:cleavir-env))
  (:export
   #:*use-cst*
   #:literal
   #:%literal-index
   #:*clasp-ordinary-lambda-list-grammar*
   #:cleavir-compile-eval
   #:compile-cst-or-form
   #:clasp
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
   #:convert-funcalls
   #:finalize-unwind-and-landing-pad-instructions
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

(defpackage #:clasp-cleavir-ast
  (:nicknames #:cc-ast)
  (:local-nicknames (#:ast #:cleavir-ast))
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
   #:vaslist-pop-ast
   #:vaslist-length-ast
   #:header-stamp-ast #:rack-stamp-ast
   #:wrapped-stamp-ast #:derivable-stamp-ast
   #:bind-va-list-ast #:rest-alloc #:make-bind-va-list-ast #:va-list-ast
   #:atomic-car-ast #:atomic-cdr-ast #:atomic-rplaca-ast #:atomic-rplacd-ast
   #:fence-ast #:cmp-ast #:order
   #:cas-car-ast #:cas-cdr-ast #:slot-cas-ast
   #:atomic-vref-ast #:atomic-vset-ast #:vcas-ast
   #:atomic-rack-read-ast #:atomic-rack-write-ast #:cas-rack-ast #:rack-ast
   #:bind-ast
   #:unwind-protect-ast #:cleanup-ast
   #:invoke-ast #:multiple-value-invoke-ast #:destinations
   #:introduce-invoke
   ))

(defpackage #:clasp-cleavir-bir
  (:use #:cl)
  (:nicknames #:cc-bir)
  (:local-nicknames (#:bir #:cleavir-bir)
                    (#:ast-to-bir #:cleavir-ast-to-bir))
  (:shadow #:unwind-protect)
  (:export #:unwind-protect #:bind #:header-stamp-case
           #:foreign-call-pointer #:foreign-types
           #:defcallback #:defcallback-args
           #:mv-foreign-call #:function-name
           #:atomic #:order #:fence
           #:atomic-rack-read #:atomic-rack-write #:cas-rack
           #:vref #:vset #:vcas #:element-type #:simple-p #:boxed-p)
  (:export #:primop-rtype-info))

(defpackage #:cc-bir-to-bmir
  (:use #:cl)
  (:local-nicknames (#:bir #:cleavir-bir))
  (:export #:reduce-module-typeqs)
  (:export #:reduce-module-primops)
  (:export #:assign-module-rtypes #:insert-casts-into-module))

(defpackage #:clasp-cleavir-bmir
  (:nicknames #:cc-bmir)
  (:shadow #:characterp #:consp #:load)
  (:local-nicknames (#:bir #:cleavir-bir))
  (:export #:fixnump #:characterp #:consp #:single-float-p #:generalp
           #:headerq #:info)
  (:export #:memref2 #:offset #:load #:store #:cas)
  (:export #:cast)
  (:export #:datum #:output #:phi #:variable)
  (:export #:rtype))
