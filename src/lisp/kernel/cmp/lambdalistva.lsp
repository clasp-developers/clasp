

(in-package :compiler)




;;--------------------------------------------------
;;--------------------------------------------------
;;
;; Calling functions
;;
;;--------------------------------------------------
;;--------------------------------------------------


(defun cmp-lookup-function (result fn-designator evaluate-env)
  (if (atom fn-designator)
      (let ((classified (classify-function-lookup evaluate-env fn-designator)))
	(if (eq (car classified) 'core::global-function)
	    (irc-intrinsic "va_symbolFunction" result (irc-global-symbol fn-designator evaluate-env))
	    (irc-intrinsic "va_lexicalFunction" result
			   (jit-constant-i32 (caddr classified))
			   (jit-constant-i32 (cadddr classified))
			   (irc-renv evaluate-env))))
      (if (eq (car fn-designator) 'cl:lambda)
	  (error "Handle lambda expressions at head of form")
	  (error "Illegal head of form: ~a" fn-designator))))



(defun codegen-call (result form evaluate-env)
  "Make and fill a value frame with the evaluated arguments and invoke the function with the value frame"
  ;; setup the ActivationFrame for passing arguments to this function in the setup arena
  (assert-result-isa-llvm-value result)
  (let* ((sym (car form))
	 (nargs (length (cdr form)))
	 (arg-array (irc-alloca-tsp-array evaluate-env :num nargs )))
    (dbg-set-invocation-history-stack-top-source-pos form)
    ;; evaluate the arguments into the array
    ;;  used to be done by --->    (codegen-evaluate-arguments (cdr form) evaluate-env)
    (do* ((cur-exp (cdr form) (cdr cur-exp))
	  (exp (car cur-exp) (car cur-exp))
	  (i 0 (+ 1 i)))
	 ((endp cur-exp) nil)
      (let ((target (irc-gep arg-array (list (jit-constant-i32 i)) (bformat nil "arg-%d"  i) )))
	(codegen target exp evaluate-env)))
    (let* ((fn (irc-alloca-Function_sp evaluate-env :label "func")))
      (cmp-lookup-function fn sym evaluate-env)
      (irc-intrinsic "FUNCALL" result fn (jit-constant-i32 nargs) arg-array))))





;;--------------------------------------------------
;;--------------------------------------------------
;;
;; Handling arguments that are passed into functions
;;
;;--------------------------------------------------
;;--------------------------------------------------


(defun compile-copy-only-required-arguments (new-env ; The environment that will be enriched by the copied arguments
					     lambda-list-handler ; Names of the copied arguments
					     dest-activation-frame ; where the arguments will be copied to
					     argument-holder) ; (contains narg and va-list )
  (let ((number-of-required-arguments (number-of-required-arguments lambda-list-handler))
	(nargs (first argument-holder))
	(va-list (second argument-holder))
	)
    (compile-error-if-wrong-number-of-arguments nargs
						number-of-required-arguments )
    ;; enrich the new-env with the local variables
    (dolist (classified-local (classified-symbols lambda-list-handler))
      (cond
	((eq (car classified-local) 'ext:lexical-var)
	 (let ((local-sym (cadr classified-local))
	       (local-idx (cddr classified-local)))
	   (value-environment-define-lexical-binding new-env local-sym local-idx)))
	((eq (car classified-local) 'ext:special-var)
	 (value-environment-define-special-binding new-env (cdr classified-local)))
	(t (error "Illegal variable classification: ~a" classified-local))))
    (irc-intrinsic "va_fillActivationFrameWithRequiredVarargs" dest-activation-frame nargs va-list)
    ))



(defun compile-error-if-not-enough-arguments (nargs lv-required-number-of-arguments)
  "If nargs < lv-required-number-of-arguments then throw an exception - no cleanup needed because no new environment was created yet"
  (let* ((error-block (irc-basic-block-create "error"))
	 (cont-block (irc-basic-block-create "continue")))
    (let ((cmp (irc-icmp-slt nargs lv-required-number-of-arguments "enough-args")))
      (irc-cond-br cmp error-block cont-block)
      (irc-begin-block error-block)
      (irc-intrinsic "va_throwNotEnoughArgumentsException" *gv-current-function-name* nargs lv-required-number-of-arguments )
      (irc-unreachable)
      (irc-begin-block cont-block)
      )))


(defun compile-error-if-wrong-number-of-arguments (nargs required-number-of-arguments)
  "If nargs /= lv-required-number-of-arguments then throw an exception - no cleanup needed/nothing was created"
  (let* ((error-block (irc-basic-block-create "error"))
	 (cont-block (irc-basic-block-create "continue"))
	 (given-number-of-arguments nargs )
	 (required-number-of-arguments (jit-constant-i32 required-number-of-arguments))
	 (cmp (irc-icmp-ne given-number-of-arguments required-number-of-arguments "correct-num-args")))
    (irc-cond-br cmp error-block cont-block)
    (irc-begin-block error-block)
    (compile-error-if-not-enough-arguments given-number-of-arguments required-number-of-arguments)
    (irc-intrinsic "va_throwTooManyArgumentsException" *gv-current-function-name* given-number-of-arguments required-number-of-arguments)
    (irc-unreachable)
    (irc-begin-block cont-block)
    ))




#||
(defun compile-save-if-special (env target &key make-unbound)
  (when (eq (car target) 'ext:special-var)
    (cmp-log "compile-save-if-special - the target: %s is special - so I'm saving it\n" target)
    (let* ((target-symbol (cdr target))
	   (saved-special-val (irc-alloca-tsp env :label (bformat nil "saved->%s" (symbol-name target-symbol)))))
      (irc-intrinsic "symbolValueReadOrUnbound" saved-special-val (irc-global-symbol target-symbol env))
      (when make-unbound
	(irc-intrinsic "makeUnboundTsp" (irc-intrinsic "symbolValueReference" (irc-global-symbol target-symbol env))))
;;;      (irc-intrinsic "copyTsp" (codegen-special-var-reference target-symbol env) val)
      (irc-push-unwind env `(symbolValueRestore ,saved-special-val ,target-symbol))
      ;; Make the variable locally special
      (value-environment-define-special-binding env target-symbol))))
||#


(defun compile-save-if-special (env target &key make-unbound)
  (when (eq (car target) 'ext:special-var)
    (cmp-log "compile-save-if-special - the target: %s is special - so I'm saving it\n" target)
    (let* ((target-symbol (cdr target))
	   (irc-target (irc-global-symbol target-symbol env)))
      (irc-intrinsic "pushDynamicBinding" irc-target)
      (when make-unbound
	(irc-intrinsic "makeUnboundTsp" (irc-intrinsic "symbolValueReference" irc-target)))
      (irc-push-unwind env `(symbolValueRestore ,target-symbol))
      ;; Make the variable locally special
      (value-environment-define-special-binding env target-symbol))))




(defun compile-target-reference* (env target)
  "This function determines if target is special or lexical and generates
code to get the reference to the target.
It then returns (values target-ref target-type target-symbol target-lexical-index).
If target-type=='special-var then target-lexical-index will be nil.
You don't want to only write into the target-reference because
you need to also bind the target in the compile-time environment "
  (cmp-log "compile-target-reference target[%s]\n" target)
  (cond
    ((eq (car target) 'ext:special-var)
     (cmp-log "compiling as a special-var\n")
     (values (irc-intrinsic "symbolValueReference" (irc-global-symbol (cdr target) env))
	     (car target)		; target-type --> 'special-var
	     (cdr target)		; target-symbol
	     ))
    ((eq (car target) 'ext:lexical-var)
     (cmp-log "compiling as a ext:lexical-var\n")
     (values (irc-intrinsic "lexicalValueReference"
		       (jit-constant-i32 0)
		       (jit-constant-i32 (cddr target))
		       (irc-renv env))
	     (car target)		; target-type --> 'ext:lexical-var
	     (cadr target)		; target-symbol
	     (cddr target)		; target-lexical-index
	     ))
    (t (error "Illegal target type[~a] for argument" target))))




(defun define-binding-in-value-environment* (env target)
  "Define the target within the ValueEnvironment in env.
If the target is special then define-special-binding.
If the target is lexical then define-lexical-binding."
  (cmp-log "define-binding-in-value-environment for target: %s\n" target)
  (cond
    ((eq (car target) 'ext:special-var)
     (let ((target-symbol (cdr target)))
       (value-environment-define-special-binding env target-symbol)))
    ((eq (car target) 'ext:lexical-var)
     (let ((target-symbol (cadr target))
	   (target-lexical-index (cddr target)))
       (value-environment-define-lexical-binding env target-symbol target-lexical-index)))
    (t (error "Illegal target-type ~a" (car target))))
)




(defmacro with-target-reference-do ((target-ref target env) &rest body)
  "This function generates code to write val into target
\(special-->Symbol value slot or lexical-->ActivationFrame) at run-time.
Use cases:
- generate code to copy a value into the target-ref
\(with-target-reference-do (target-ref target env)
  (irc-intrinsic \"copyTsp\" target-ref val))
- compile arbitrary code that writes result into the target-ref
\(with-target-reference-do (target-ref target env)
  (codegen target-ref form env))"
  `(progn
     (let ((,target-ref (compile-target-reference* ,env ,target)))
       ,@body)
     ;; Add the target to the ValueEnvironment AFTER storing it in the target reference
     ;; otherwise the target may shadow a variable in the lexical environment
     (define-binding-in-value-environment* ,env ,target)
     ))



(defmacro with-target-reference-if-runtime-unbound-do ((target-ref target env) &rest body)
  "Generate code that does everything with-target-reference-do does
but tests if the value in target-ref is unbound and if it is only-then evaluate body which
will put a value into target-ref."
  (let ((i1-target-is-bound-gs (gensym))
	(unbound-do-block-gs (gensym))
	(unbound-cont-block-gs (gensym)))
    `(progn
       (with-target-reference-do (,target-ref ,target ,env)
	 (let ((,i1-target-is-bound-gs (irc-trunc (irc-intrinsic "isBoundTsp" ,target-ref) +i1+))
	       (,unbound-do-block-gs (irc-basic-block-create "unbound-do"))
	       (,unbound-cont-block-gs (irc-basic-block-create "unbound-cont"))
	       )
	   (irc-cond-br ,i1-target-is-bound-gs ,unbound-cont-block-gs ,unbound-do-block-gs)
	   (irc-begin-block ,unbound-do-block-gs)
	   ,@body
	   (irc-br ,unbound-cont-block-gs)
	   (irc-begin-block ,unbound-cont-block-gs)
	   ))
       ;; Add the target to the ValueEnvironment AFTER storing it in the target reference
       ;; otherwise the target may shadow a variable in the lexical environment
       (define-binding-in-value-environment* ,env ,target)
       )))
	 




(defun compile-required-arguments (reqargs env
				   nargs
				   va-list
				   new-env
				   entry-arg-idx ;; this is now in a register
				   )
  (irc-branch-to-and-begin-block (irc-basic-block-create "process-required-arguments"))
  (compile-error-if-not-enough-arguments nargs
					 (jit-constant-i32 (car reqargs)))
  (dbg-set-current-debug-location-here)
  ;; First save any special values
  (do* ((cur-req (cdr reqargs) (cdr cur-req))
	(target (car cur-req) (car cur-req)))
       ((endp cur-req) ())
    (compile-save-if-special new-env target))
  ;; Now copy the required arguments into their targets
  (do* ((cur-req (cdr reqargs) (cdr cur-req))
	(target (car cur-req) (car cur-req))
	(arg-idx entry-arg-idx (irc-add arg-idx (jit-constant-i32 1) "arg-idx")))
       ((endp cur-req) arg-idx)
    (let* ((target-idx (cdr target))
	   (val-ref (irc-gep va-list (list arg-idx)
			     (bformat nil "arg-%d" target-idx ))))
      (with-target-reference-do (tref target new-env) ; run-time binding
	(irc-intrinsic "copyTsp" tref val-ref)))))



(defun compile-optional-arguments (optargs old-env
				   nargs
				   va-list
				   new-env
				   entry-arg-idx )
  (irc-branch-to-and-begin-block (irc-basic-block-create "process-optional-arguments"))
  (cmp-log "About to compile-optional-arguments: %s\n" optargs)
  ;; First save any special values
  (do* ((cur-opt (cdr optargs) (cdddr cur-opt))
	(target (car cur-opt) (car cur-opt))
	(init-form (cadr cur-opt) (cadr cur-opt))
	(flag (caddr cur-opt) (caddr cur-opt))
	)
       ((endp cur-opt) ())
    (compile-save-if-special new-env target)
    (when flag
      (compile-save-if-special new-env flag)))
  ;; Copy argument values or evaluate init forms
  (do* ((cur-opt (cdr optargs) (cdddr cur-opt))
	(target (car cur-opt) (car cur-opt))
	(init-form (cadr cur-opt) (cadr cur-opt))
	(flag (caddr cur-opt) (caddr cur-opt))
	(temp-result (irc-alloca-tsp new-env :label "temp-result"))
	(arg-idx entry-arg-idx (irc-add arg-idx (jit-constant-i32 1) "arg-idx")))
       ((endp cur-opt) arg-idx)
    (let ((arg-block (irc-basic-block-create "opt-arg"))
	  (init-block (irc-basic-block-create "opt-init"))
	  (cont-block (irc-basic-block-create "opt-cont"))
	  (cmp (irc-icmp-slt arg-idx nargs "enough-given-args")))
      (irc-cond-br cmp arg-block init-block)
      (irc-begin-block arg-block)
      (let ((val-ref (irc-gep va-list (list arg-idx))))
	(with-target-reference-do (target-ref target new-env) ; run-time af binding
	  (irc-intrinsic "copyTsp" target-ref val-ref))
	(when flag
	  (with-target-reference-do (flag-ref flag new-env) ; run-time AF binding
	    (irc-intrinsic "copyTsp" flag-ref (compile-reference-to-literal t new-env))))
	(irc-br cont-block)
	(irc-begin-block init-block)
	(with-target-reference-do (target-ref target new-env) ; codegen init-form into target-ref
	  ;; Use new-env so that symbols already defined in this lambda can be accessed
	  (codegen target-ref init-form new-env))
	(when flag
	  (with-target-reference-do (flag-ref flag new-env) ; copy nil into flag-ref
	    (irc-intrinsic "copyTsp" flag-ref (compile-reference-to-literal nil new-env))))
	(irc-br cont-block) 
	(irc-begin-block cont-block)))))


(defun compile-rest-arguments (rest-var old-env
			       nargs
			       va-list
			       new-env entry-arg-idx)
  (irc-branch-to-and-begin-block (irc-basic-block-create "process-rest-arguments"))
  (with-target-reference-do (rest-ref rest-var new-env)
    (irc-intrinsic "va_fillRestTarget" rest-ref nargs va-list
	      entry-arg-idx *gv-current-function-name* )))



(defun compile-key-arguments-parse-arguments (keyargs
					      lambda-list-allow-other-keys
					      old-env
					      nargs
					      va-list
					      new-env
					      entry-arg-idx
					      sawkeys )
  "saw-aok keeps track if &allow-other-keys was defined or if :allow-other-keys t/nil was seen.
   saw-aok can have the values (2[&a-o-k or :a-o-k t], 1[:a-o-k nil] or 0 [no &a-o-k or :a-o-k]) "
  (let ((process-kw-args-block (irc-basic-block-create "process-kw-args")))
    (irc-branch-to-and-begin-block process-kw-args-block)
    (let* ((entry-saw-aok (jit-constant-i32 (if lambda-list-allow-other-keys 2 0)))
	   (entry-bad-kw-idx (jit-constant-i32 -1))
	   (aok-ref (compile-reference-to-literal :allow-other-keys old-env))
	   (loop-kw-args-block (irc-basic-block-create "loop-kw-args"))
	   (kw-exit-block (irc-basic-block-create "kw-exit-block"))
	   (loop-cont-block (irc-basic-block-create "loop-cont"))
	   (kw-start-block (irc-basic-block-create "kw-begin-block")))
      (irc-branch-to-and-begin-block kw-start-block)
      (let ((entry-arg-idx_lt_nargs (irc-icmp-slt entry-arg-idx nargs)) )
	(irc-cond-br entry-arg-idx_lt_nargs loop-kw-args-block kw-exit-block))
      (irc-begin-block loop-kw-args-block)
      (let* ((phi-saw-aok (irc-phi +i32+ 2 "phi-saw-aok"))
	     (phi-arg-idx (irc-phi +i32+ 2 "phi-reg-arg-idx"))
	     (phi-bad-kw-idx (irc-phi +i32+ 2 "phi-bad-kw-idx")) )
	(irc-phi-add-incoming phi-saw-aok entry-saw-aok kw-start-block)
	(irc-phi-add-incoming phi-arg-idx entry-arg-idx kw-start-block)
	(irc-phi-add-incoming phi-bad-kw-idx entry-bad-kw-idx kw-start-block)
	(irc-low-level-trace)
	(let* ((arg-ref (irc-gep va-list (list phi-arg-idx))))
	  (irc-intrinsic "kw_throwIfNotKeyword" arg-ref)
	  (let* ((eq-aok-ref-and-arg-ref (irc-trunc (irc-intrinsic "compareTsp" aok-ref arg-ref) +i1+)) ; compare arg-ref to a-o-k
		 (aok-block (irc-basic-block-create "aok-block"))
		 (possible-kw-block (irc-basic-block-create "possible-kw-block"))
		 (advance-arg-idx-block (irc-basic-block-create "advance-arg-idx-block"))
		 (bad-kw-block (irc-basic-block-create "bad-kw-block"))
		 (good-kw-block (irc-basic-block-create "good-kw-block"))
		 )
	    (irc-cond-br eq-aok-ref-and-arg-ref aok-block possible-kw-block)
	    (irc-begin-block aok-block)
	    (let* ((loop-saw-aok (irc-intrinsic "va_allowOtherKeywords"
					   phi-saw-aok
					   nargs
					   va-list
					   phi-arg-idx)) )
	      (irc-br advance-arg-idx-block)
	      (irc-begin-block possible-kw-block)
	      ;; Generate a test for each keyword
	      (do* ((cur-key-arg (cdr keyargs) (cddddr cur-key-arg))
		    (key (car cur-key-arg) (car cur-key-arg))
		    (target (cadr cur-key-arg) (cadr cur-key-arg))
		    (init-form (caddr cur-key-arg) (caddr cur-key-arg))
		    (flag (cadddr cur-key-arg) (cadddr cur-key-arg))
		    (idx 0 (1+ idx))
		    (next-kw-test-block (irc-basic-block-create "next-kw-block")
					(irc-basic-block-create "next-kw-block")) )
		   ((endp cur-key-arg))
		(irc-branch-to-and-begin-block (irc-basic-block-create (bformat nil "kw-%s-test" key)))
		(irc-low-level-trace)
		(let* ((kw-ref (compile-reference-to-literal key old-env))
		       (eq-kw-and-arg (irc-trunc (irc-intrinsic "matchKeywordOnce" kw-ref arg-ref (elt sawkeys idx)) +i1+))
		       (match-kw-block (irc-basic-block-create "match-kw-block")))
		  (irc-cond-br eq-kw-and-arg match-kw-block next-kw-test-block)
		  (irc-begin-block match-kw-block)
		  (let* ((arg-idx+1 (irc-add phi-arg-idx (jit-constant-i32 1)))
			 (kw-arg-ref (irc-gep va-list (list arg-idx+1))))
		    (with-target-reference-do (target-ref target new-env) ; run-time binding
		      (irc-intrinsic "copyTsp" target-ref kw-arg-ref))
		    ;; Set the boolean flag to indicate that we saw this key
		    (irc-store (jit-constant-i8 1) (elt sawkeys idx))
		    (when flag
		      (with-target-reference-do (flag-ref flag new-env)
			(irc-intrinsic "copyTsp" flag-ref (compile-reference-to-literal t new-env))))
		    #|| ;; The old way was to write a value only if the target was UNBOUND - that was wrong
		    (with-target-reference-if-runtime-unbound-do (flag-ref flag new-env) ; run-time binding ;
		    (irc-intrinsic "copyTsp" flag-ref (compile-reference-to-literal t new-env))))
		  ||#
		  (irc-br good-kw-block)
		  (irc-begin-block next-kw-test-block)
		  )))
	      ;; We fell through all the keyword tests - this might be a unparameterized keyword
	    (irc-branch-to-and-begin-block bad-kw-block) ; fall through to here if no kw recognized
	    (let ((loop-bad-kw-idx (irc-intrinsic "kw_trackFirstUnexpectedKeyword"
					     phi-bad-kw-idx phi-arg-idx)))
	      (irc-low-level-trace)
	      (irc-br advance-arg-idx-block)
	      (irc-begin-block good-kw-block) ; jump to here if kw was recognized
	      (irc-low-level-trace)
	      (irc-br advance-arg-idx-block)
	      ;; Now advance the arg-idx, finish up the phi-nodes
	      ;; and if we ran out of arguments branch out of the loop else branch to the top of the loop
	      (irc-begin-block advance-arg-idx-block)
	      (let* ((phi-arg-bad-good-aok (irc-phi +i32+ 3 "phi-this-was-aok"))
		     (phi.aok-bad-good.bad-kw-idx (irc-phi +i32+ 3 "phi.aok-bad-good.bad-kw-idx")))
		(irc-phi-add-incoming phi-arg-bad-good-aok loop-saw-aok aok-block)
		(irc-phi-add-incoming phi-arg-bad-good-aok phi-saw-aok bad-kw-block)
		(irc-phi-add-incoming phi-arg-bad-good-aok phi-saw-aok good-kw-block)
		(irc-phi-add-incoming phi.aok-bad-good.bad-kw-idx phi-bad-kw-idx aok-block)
		(irc-phi-add-incoming phi.aok-bad-good.bad-kw-idx loop-bad-kw-idx bad-kw-block)
		(irc-phi-add-incoming phi.aok-bad-good.bad-kw-idx phi-bad-kw-idx good-kw-block)
		(irc-low-level-trace)
		(let* ((loop-arg-idx (irc-add phi-arg-idx (jit-constant-i32 2)))
		       (loop-arg-idx_lt_nargs (irc-icmp-slt loop-arg-idx nargs)))
		  (irc-phi-add-incoming phi-saw-aok phi-arg-bad-good-aok advance-arg-idx-block)
		  (irc-phi-add-incoming phi-bad-kw-idx phi.aok-bad-good.bad-kw-idx advance-arg-idx-block)
		  (irc-phi-add-incoming phi-arg-idx loop-arg-idx advance-arg-idx-block)
		  (irc-cond-br loop-arg-idx_lt_nargs loop-kw-args-block loop-cont-block)
		  (irc-begin-block loop-cont-block)
		  (irc-intrinsic "va_throwIfBadKeywordArgument" phi-arg-bad-good-aok phi.aok-bad-good.bad-kw-idx nargs va-list)
		  (let ((kw-done-block (irc-basic-block-create "kw-done-block")))
		    (irc-branch-to-and-begin-block kw-done-block)
		    (irc-branch-to-and-begin-block kw-exit-block)
		    (let ((phi-arg-idx-final (irc-phi +i32+ 2 "phi-arg-idx-final")))
		      (irc-phi-add-incoming phi-arg-idx-final entry-arg-idx kw-start-block)
		      (irc-phi-add-incoming phi-arg-idx-final loop-arg-idx kw-done-block)
		      (irc-low-level-trace)
		      phi-arg-idx-final))))))))))))


(defun compile-key-arguments-fill-in-init-forms (keyargs new-env sawkeys)
  (do* ((cur-key-arg (cdr keyargs) (cddddr cur-key-arg))
	(key (car cur-key-arg) (car cur-key-arg))
	(target (cadr cur-key-arg) (cadr cur-key-arg))
	(init-form (caddr cur-key-arg) (caddr cur-key-arg))
	(flag (cadddr cur-key-arg) (cadddr cur-key-arg))
	(idx 0 (1+ idx))
	)
       ((endp cur-key-arg) ())
    (let ((init-default-kw-block (irc-basic-block-create "init-default-kw-block"))
	  (next-kw-block (irc-basic-block-create "next-kw-block"))
	  (i1-missing-key (irc-icmp-eq (irc-load (elt sawkeys idx)) (jit-constant-i8 0))))
      (irc-low-level-trace)
      (irc-cond-br i1-missing-key init-default-kw-block next-kw-block)
      (irc-begin-block init-default-kw-block)
      (irc-low-level-trace)
      (with-target-reference-do (target-ref target new-env) ; run-time binding
	(irc-low-level-trace)
	(codegen target-ref init-form new-env))
      (irc-low-level-trace)
      (when flag
	(irc-low-level-trace)
	(with-target-reference-do (flag-ref flag new-env) ; run-time binding
	  (irc-intrinsic "copyTsp" flag-ref (compile-reference-to-literal nil new-env))))
      (irc-low-level-trace)
      (irc-branch-to-and-begin-block next-kw-block))))
    


(defun compile-key-arguments (keyargs
			      lambda-list-allow-other-keys
			      old-env
			      nargs
			      va-list
			      new-env
			      entry-arg-idx)
  ;; First save and makunbound each of the targets
  (irc-low-level-trace)
  (let ((sawkeys (make-array (car keyargs))))
    (do* ((cur-key-arg (cdr keyargs) (cddddr cur-key-arg))
	  (key (car cur-key-arg) (car cur-key-arg))
	  (target (cadr cur-key-arg) (cadr cur-key-arg))
	  (init-form (caddr cur-key-arg) (caddr cur-key-arg))
	  (flag (cadddr cur-key-arg) (cadddr cur-key-arg))
	  (idx 0 (1+ idx)))
	 ((endp cur-key-arg) ())
      (irc-low-level-trace)
      (setf (elt sawkeys idx) (irc-alloca-i8 old-env 0))
      (compile-save-if-special new-env target :make-unbound nil)
      (irc-low-level-trace)
      (when flag
	(compile-save-if-special new-env flag :make-unbound nil)))
    (irc-low-level-trace)
    ;; Now parse the arguments in argument-activation-frame and copy the
    ;; passed values into the new-env and dynamic variables according to keyargs
    (let ((arg-idx (compile-key-arguments-parse-arguments keyargs
							  lambda-list-allow-other-keys
							  old-env
							  nargs
							  va-list
							  new-env
							  entry-arg-idx
							  sawkeys)))
      (irc-low-level-trace)
      (compile-key-arguments-fill-in-init-forms keyargs new-env sawkeys )
      (irc-low-level-trace)
      arg-idx)))
  


		

(defun compile-throw-if-excess-keyword-arguments (env nargs va-list arg-idx)
  (irc-intrinsic "va_throwIfExcessKeywordArguments" *gv-current-function-name* nargs va-list arg-idx))





(defun compile-aux-arguments (auxargs old-env new-env)
  (let ((aux-arguments-block (irc-basic-block-create "process-aux-arguments")))
    (irc-br aux-arguments-block)
    (irc-begin-block aux-arguments-block))
  ;; save special variables
  (do* ((cur-aux (cdr auxargs) (cddr cur-aux))
	(target (car cur-aux) (car cur-aux))
	(init-form (cadr cur-aux) (cadr cur-aux))
	(temp-result (irc-alloca-tsp new-env :label "temp-result")))
       ;; TODO: setup one temp-result and use it for all types of args
       ((endp cur-aux) ())
    (compile-save-if-special new-env target))
  ;; Now process the arguments
  (do* ((cur-aux (cdr auxargs) (cddr cur-aux))
	(target (car cur-aux) (car cur-aux))
	(init-form (cadr cur-aux) (cadr cur-aux))
	(temp-result (irc-alloca-tsp new-env :label "temp-result")))
       ((endp cur-aux) ())
    ;; Copy the argument into _temp-result_
    (cmp-log "Compiling aux init-form %s\n" init-form)
    (cmp-log "    in environment: %s\n" new-env)
    (with-target-reference-do (target-ref target new-env) ; run-time binding
      (codegen target-ref init-form new-env))))




(defun compile-lambda-list-code (lambda-list-handler
				 old-env
				 argument-holder
				 new-env
				 &aux (nargs (first argument-holder)) (va-list (second argument-holder)))
  "Fill the dest-activation-frame with values using the
lambda-list-handler/env/argument-activation-frame"
  ;; Declare the arg-idx i32 that stores the current index in the argument-activation-frame
  (dbg-set-current-debug-location-here)
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys auxargs)
      (process-lambda-list-handler lambda-list-handler)
    (let* ((arg-idx (jit-constant-i32 0))
	   (opt-arg-idx (compile-required-arguments reqargs
						    old-env
						    nargs
						    va-list
						    new-env arg-idx))
	   (rest/key-arg-idx (if (/= 0 (car optargs))
				 (compile-optional-arguments optargs
							     old-env
							     nargs
							     va-list
							     new-env opt-arg-idx)
				 opt-arg-idx))
	   (dummy (when rest-var
		    (compile-rest-arguments rest-var
					    old-env
					    nargs
					    va-list
					    new-env
					    rest/key-arg-idx)))
	   (last-arg-idx (if key-flag
			     (compile-key-arguments keyargs
						    allow-other-keys
						    old-env
						    nargs
						    va-list
						    new-env
						    rest/key-arg-idx)
			     rest/key-arg-idx)))
      (unless rest-var
	(compile-throw-if-excess-keyword-arguments old-env nargs va-list last-arg-idx))
      (when (/= 0 (car auxargs))
	(compile-aux-arguments auxargs old-env new-env))
      )
    ))



