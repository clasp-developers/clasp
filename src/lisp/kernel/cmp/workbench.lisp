(progn
  (setf cmp::*debug-compiler* t)
  (setf cmp::*use-human-readable-bitcode* t)
  (trace COMPILER:SETUP-CALLING-CONVENTION
         COMPILER::IRC-local-FUNCTION-CREATE
         cmp::irc-xep-functions-create COMPILER::CODEGEN-FILL-FUNCTION-FRAME
         COMPILER::CODEGEN-FUNCTION COMPILER::COMPILE-TO-MODULE COMPILER::CODEGEN-CLOSURE
         COMPILER:COMPILE-LAMBDA-FUNCTION COMPILER::GENERATE-LLVM-FUNCTION-FROM-CODE
         COMPILER::TRANSFORM-LAMBDA-PARTS COMPILE-FILE
         cmp::do-new-function
         cmp::do-dbg-function
         cmp::compile-file-serial
         cmp::compile-file-to-module
         cmp::loop-read-and-compile-file-forms
         cmp::bclasp-loop-read-and-compile-file-forms
         cmp::t1expr
         cmp::t1eval-when
         cmp::t1progn
         cmp::compile-top-level
         literal:arrange-thunk-as-top-level
         cmp::compile-thunk
         cmp::codegen-symbol-value
         cmp::codegen
         cmp::codegen-progn
         cmp::codegen-let/let*
         cmp::codegen-let
         cmp::codegen-let*
         cmp::codegen-special-operator
         cmp::codegen-cons
         cmp::codegen-var-lookup
         cmp::codegen-lexical-var-lookup
         cmp::codegen-alloca-var-lookup
         cmp::variable-info
         cmp::classify-variable
         cmp::compile-lambda-list-code
         cmp::compile-general-lambda-list-code
         cmp::compile-only-req-and-opt-arguments
         cmp::compile-required-arguments
         cmp::compile-optional-arguments
         cmp::compile-key-arguments
         cmp::add-global-ctor-function
         cmp::irc-create-invoke-default-unwind
         cmp::codegen-lexical-var-reference
         cmp::irc-intrinsic-call-or-invoke
         cmp::irc-arity-info
         cmp::do-make-new-run-all
         cmp::irc-ret
         cmp::irc-ret-void
         cmp::irc-ret-null-t*
         cmp::irc-funcall-results-in-registers
         cmp::irc-function-create
         cmp::irc-create-local-entry-point-reference
         cmp::irc-create-global-entry-point-reference
         cmp::make-xep-group
         cmp::irc-calculate-entry
         cmp::make-xep-arity
         cmp::irc-calculate-real-args
         cmp::irc-calculate-call-info
         cmp::initialize-calling-convention
         cmp::compile-wrong-number-arguments-block
         cmp::compile-error-if-too-many-arguments
         cmp::compile-error-if-not-enough-arguments
         cmp::irc-icmp-ugt
         cmp::bclasp-llvm-function-info-xep-function
         cmp::maybe-spill-to-register-save-area
         cmp::make-calling-convention
         cmp::layout-xep-function
         cmp::layout-xep-function*
         cmp::irc-create-call-wft
         cmp::irc-typed-gep
         cmp::irc-bit-cast
         cmp::dbg-parameter-var
         cmp::%dbg-variable-value
         cmp::%dbg-variable-addr
         cmp::alloca-temp-values
         cmp::alloca-arguments
         cmp::alloca-register-save-area
         cmp::lambda-list-arguments
         cmp::jit-add-module-return-function
         cmp::c++-field-ptr
         cmp::calling-convention-vaslist.va-arg
         cmp::calling-convention-vaslist*
         cmp::irc-typed-load
         cmp::irc-add
         cmp::irc-sub
         cmp::irc-store
         literal::do-literal-table
         literal::do-rtv
         literal::constants-table-value
         literal::constants-table-reference
         cmp::irc-phi-add-incoming
         cmp::cleavir-lambda-list-analysis-lambda-list-arguments
         cmp::process-cleavir-lambda-list-analysis
         cmp::ensure-cleavir-lambda-list
         cmp::ensure-cleavir-lambda-list-analysis
         cmp::process-bir-lambda-list
         cmp::generate-function-for-arity-p
         
         cmp:xep-group-p
         cmp:xep-group-arities
         cmp:xep-arity-function-or-placeholder
         literal::register-function->function-datum-impl
         cmp::make-entry-point-reference
         literal::reference-literal
         literal::register-local-function-index
         literal::register-xep-function-indices
         literal::register-local-function->function-datum
         literal::register-xep-function->function-datums
         sys:make-global-simple-fun-generator
         sys:make-local-simple-fun-generator
         )
  (compile 'foo '(lambda (x) x))
  #+(or)(compile-file "/tmp/xxx.lisp")
  )

