(cl:in-package #:common-lisp-user)

(defpackage #:clasp-cleavir
  (:use #:common-lisp #:core)
  (:nicknames #:clasp-cleavir-translate-bir)
  (:local-nicknames (#:bir #:cleavir-bir)
                    (#:bir-transformations #:cleavir-bir-transformations)
                    (#:ast #:cleavir-ast)
                    (#:ctype #:cleavir-ctype)
                    (#:cst-to-ast #:cleavir-cst-to-ast)
                    (#:build #:cleavir-bir-builder)
                    (#:env #:cleavir-env)
                    (#:policy #:cleavir-compilation-policy))
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
   #:unsafe-foreign-call
   #:unsafe-foreign-call-pointer
   #:datum-gid
   #:create-landing-pad
   #:convert-funcalls
   #:finalize-unwind-and-landing-pad-instructions
   #:cleavir-compile
   #:cleavir-compile-file
   #:*function-inline-asts*
   #:*clasp-env*
   #:*clasp-system*
   #:alloca-i8
   #:inline-ast
   )
  (:export #:primop-rtype-info)
  ;; for ext:describe-compiler-policy, CL compiler macros
  (:implement #:ext #:cl))

(defpackage #:clasp-cleavir-ast
  (:nicknames #:cc-ast)
  (:local-nicknames (#:ast #:cleavir-ast))
  (:use #:common-lisp)
  (:export
   #:foreign-call-ast
   #:foreign-call-pointer-ast
   #:argument-asts
   #:foreign-types
   #:make-throw-ast
   #:throw-ast
   #:result-ast
   #:tag-ast
   #:datum-id
   ))

(defpackage #:clasp-cleavir-bir
  (:use #:cl)
  (:nicknames #:cc-bir)
  (:local-nicknames (#:bir #:cleavir-bir)
                    (#:ast-to-bir #:cleavir-ast-to-bir)
                    (#:build #:cleavir-bir-builder))
  (:export #:header-stamp-case
           #:foreign-call-pointer #:foreign-types
           #:atomic #:order))

(defpackage #:cc-bir-to-bmir
  (:use #:cl)
  (:local-nicknames (#:bir #:cleavir-bir))
  (:export #:reduce-module-instructions)
  (:export #:assign-module-rtypes #:insert-casts-into-module))

(defpackage #:cc-bmir-to-blir
  (:use #:cl)
  (:local-nicknames (#:bir #:cleavir-bir))
  (:export #:reduce-module-instructions))

(defpackage #:clasp-cleavir-bmir
  (:nicknames #:cc-bmir)
  (:shadow #:characterp #:consp #:load #:variable)
  (:local-nicknames (#:bir #:cleavir-bir))
  (:export #:fixnump #:characterp #:consp #:single-float-p #:generalp
           #:headerq #:info)
  (:export #:cast #:unboxed-constant-reference
           #:mtf #:append-values #:fixed-mv-call #:fixed-mv-local-call)
  (:export #:datum)
  (:export #:rtype)
  (:export #:cast-one))

(defpackage #:clasp-cleavir-blir
  (:nicknames #:cc-blir)
  (:local-nicknames (#:bir #:cleavir-bir))
  ;; Shadowing cl:load isn't strictly necessary, but will keep it from
  ;; showing up in M-. or the like.
  (:shadow #:load)
  (:export #:memref2 #:offset #:load #:store #:cas))

(defpackage #:clasp-cleavir-vaslist
  (:nicknames #:cc-vaslist)
  (:local-nicknames (#:bir #:cleavir-bir)
                    (#:ctype #:cleavir-ctype)
                    (#:set #:cleavir-set)
                    (#:attributes #:cleavir-attributes)
                    (#:policy #:cleavir-compilation-policy))
  (:shadow #:values-list #:nth #:nthcdr #:last #:butlast #:length)
  (:export #:values-list #:nth #:nthcdr #:last #:butlast #:nendp #:length)
  (:export #:maybe-transform-module)
  (:export #:vaslistablep))
