 
;;
(in-package :cmp)




(defun parse-macro (name vl body &optional env)
  (multiple-value-bind (lambda-block ppn doc)
      (si::expand-defmacro name vl body)
    lambda-block))

(defun augment-environment-with-declares (env declares)
  ;; Do nothing for now
  nil
  )


;;
;; codegen for atoms
;;






(defun codegen-global-function-call (result sym env)
  "Generate code to invoke a global function symbol"
  (irc-call env "invokePossibleMultipleValueSymbolFunction" result (irc-global-symbol sym) (irc-renv env)))


(defun codegen-lexical-function-call (result depth index env)
  (irc-call env "invokePossibleMultipleValueLexicalFunction" result (jit-constant-i32 depth) (jit-constant-i32 index) (irc-renv env)))



(defun compile-only-required-arguments (lambda-list-handler old-env activation-frame)
  "Create a new environment that expands env with the required arguments in lambda-list-handler.
Also allocate a new runtime-env and copy the activation-frame into it because it only contains required arguments.
Return the new environment."
  (irc-new-value-environment
   old-env
   :lambda-list-handler lambda-list-handler
   :label "copy-req-args"
   :fill-runtime-form (lambda (new-env) (irc-call new-env "copyAFsp" activation-frame (irc-renv new-env)))))


(defun compile-copy-activation-frame (old-env new-env lambda-list-handler src-activation-frame dest-activation-frame)
  (let ((number-of-required-arguments (number-of-required-arguments lambda-list-handler)))
    (compile-error-if-wrong-number-of-arguments old-env src-activation-frame number-of-required-arguments )
    ;; enrich the new-env with the local variables
    (dolist (classified-local (classified-symbols lambda-list-handler))
      (let ((local-sym (cadr classified-local))
	    (local-idx (cddr classified-local)))
      (value-environment-define-lexical-binding new-env local-sym local-idx)))
    (irc-call new-env "copyAFsp" src-activation-frame dest-activation-frame)
    ))



(defun compile-arguments (fn-name lambda-list-handler old-env src-activation-frame new-env)
  (if (lambda-list-handler-required-lexical-arguments-only-p lambda-list-handler)
      (compile-copy-activation-frame old-env new-env lambda-list-handler src-activation-frame (irc-renv new-env))
      (let ((closed-renv (irc-call new-env "activationFrameParentRef" src-activation-frame))
	    (lexical-names (gather-lexical-variable-names (classified-symbols lambda-list-handler))))
	(cmp-log "lambda-list-handler for fn %s --> %s\n" fn-name lambda-list-handler)
	(cmp-log "Gathered lexical variables for fn %s --> %s\n" fn-name lexical-names)
	(irc-call new-env "makeValueFrame" (irc-renv new-env)
		  (jit-constant-i32 (number-of-lexical-variables lambda-list-handler))
		  closed-renv)
	(compile-lambda-list-code lambda-list-handler old-env src-activation-frame new-env)
	(irc-attach-debugging-info-to-value-frame (irc-renv new-env) lexical-names new-env))))




(defparameter *lambda-args-num* 0)

(defun compile-lambda/lambda-block (name lambda-list-handler declares docstring code old-env &key wrap-block block-name)
  (setq *lambda-args-num* (1+ *lambda-args-num*))
  (cmp-log "About to compile-lambda/lambda-block for lambda-list-handler: %s\n" lambda-list-handler)
  (let ((fn (with-new-function (fn fn-env :function-name name :parent-env old-env :function-form code)
	      (let* ((result (car (llvm-sys:get-argument-list fn)))
		     (activation-frame (cadr (llvm-sys:get-argument-list fn)))
		     traceid
		     (new-env (irc-new-value-environment
			       fn-env
			       :lambda-list-handler lambda-list-handler
			       :label (bformat nil "lambda-args-%s-" *lambda-args-num*)
			       :fill-runtime-form (lambda (the-new-env)
						    (compile-arguments name lambda-list-handler fn-env activation-frame the-new-env)))))
		(if wrap-block
		    (codegen-block result block-name code new-env)
		    (codegen-progn result code new-env))
		)
	      )))
;;    (log-dump fn)
    (irc-verify-function fn)
    fn
    ))





(defun compile-lambda-function (lambda env &key (function-name "lambda"))
  (dbg-set-current-debug-location-here)
  (let* (block-name lambda-list body)
    (if (eq (car lambda) 'lambda-block)
	(setq function-name (cadr lambda)
	      block-name (function-block-name function-name)
	      lambda-list (caddr lambda)
	      body (cdddr lambda))
	(setq lambda-list (cadr lambda)
	      body (cddr lambda)))
    (multiple-value-bind (declares code docstring specials )
	(process-declarations body)
      (cmp-log "About to create lambda-list-handler\n")
      (dbg-set-current-debug-location-here)
      (let ((lambda-list-handler (make-lambda-list-handler lambda-list declares 'core::function)))
	(let ((savedbb (irc-get-insert-block))
	      (lambda-function (compile-lambda/lambda-block function-name
							    lambda-list-handler
							    declares
							    docstring
							    code
							    env
							    :wrap-block (eq (car lambda) 'lambda-block)
							    :block-name block-name
							    )))
	  (irc-set-insert-point savedbb)
	  ;; Now I have the lambda-function which is a llvm::Function* and the environment (env)
	  ;; I want to generate code that takes these and returns a Closure object
	  ;;
	  lambda-function)))))



(defun codegen-closure (result lambda env &key (function-name "lambda"))
  (let* ((compiled-fn (compile-lambda-function lambda env :function-name function-name)))
    (let ((ccode (irc-call env "makeClosure" result compiled-fn (irc-renv env))))
      ccode)))




(defun codegen-global-function-lookup (result sym env)
  (irc-call env "symbolFunctionRead" result (irc-global-symbol sym)))


(defun codegen-global-setf-function-lookup (result setf-function-name env)
  (let ((setf-symbol (cadr setf-function-name)))
    (irc-call env "setfSymbolFunctionRead" result (irc-global-setf-symbol setf-symbol))
  ))


(defun codegen-lexical-function-lookup (result depth index env)
  (irc-call env "lexicalFunctionRead" result (jit-constant-i32 depth) (jit-constant-i32 index) (irc-renv env)))

(defun codegen-function-symbol-lookup (result func env)
  "Classify the function and look it up and put it in result"
  (let* ((classified (classify-function-lookup env func)))
    (if (eq (car classified) 'core::global-function)
	(codegen-global-function-lookup result func env)
	(codegen-lexical-function-lookup result (caddr classified) (cadddr classified) env)))
)


(defun codegen-function-setf-symbol-lookup (result setf-func env)
  "Classify the (setf XXXX) function and put it in the result"
  (let* ((classified (classify-function-lookup env setf-func)))
    (if (eq (car classified) 'core::global-function)
	(codegen-global-setf-function-lookup result setf-func env)
	(codegen-lexical-function-lookup result (caddr classified) (cadddr classified) env)))
)



(defun codegen-function (result name-or-lambda env)
  "Return IR code for a function or closure"
  (dbg-set-current-debug-location-here)
  (cmp-log "About to codegen-function for: %s\n" name-or-lambda)
  (cond
    ((and name-or-lambda (symbolp name-or-lambda)) (codegen-function-symbol-lookup result name-or-lambda env))
    ((and (consp name-or-lambda)
	  (or (eq (car name-or-lambda) 'lambda)
	      (eq (car name-or-lambda) 'lambda-block)))
     (dbg-set-current-debug-location-here)
     (codegen-closure result name-or-lambda env))
    ((and (consp name-or-lambda)
	  (eq (car name-or-lambda) 'setf))
     (if (fboundp name-or-lambda)
	 (let ((setf-code (fdefinition name-or-lambda)))
;;	   (bformat t "Got function definition for[%s] --> %s\n" name-or-lambda setf-code)
;;	   (break "Just got setf-code from sysprop - make sure we got the right thing")
	   (codegen-function-setf-symbol-lookup result name-or-lambda env))
	 (error "Could not find setf-method for ~a" name-or-lambda)))
    (t (error "Add support for other arguments for FUNCTION special operator in codegen-function: %s" name-or-lambda))))



(defun codegen-fill-value-frame ( result-af exps parent-env evaluate-env)
  "Evaluate each of the exps in the evaluate-env environment
and put the values into the activation frame in result-af.
env is the parent environment of the result-af value frame"
  (dbg-set-current-debug-location-here)
  (irc-call parent-env "makeValueFrame" result-af (jit-constant-i32 (length exps)) (irc-renv parent-env))
  (cmp-log "About to generate code for exps: %s\n" exps)
  (do* ((cur-exp exps (cdr cur-exp))
	(exp (car cur-exp) (car cur-exp))
	(i 0 (+ 1 i)))
       ((endp cur-exp) nil)
    (let ((target (irc-call parent-env "valueFrameReference" result-af
			    (jit-constant-i32 i)
			    (bformat nil "%s-ref-%d" (llvm-sys:get-name result-af) i) )))
      (unless target (error "target should never be nil"))
      (codegen target exp evaluate-env)
      (irc-first-value-if-multiple-value target)
      )))


(defun codegen-call (result form old-env)
  "Make and fill a value frame with the evaluated arguments and invoke the function with the value frame"
  ;; setup the ActivationFrame for passing arguments to this function in the setup arena
  (let ((sym (car form))
	(cont-block (irc-basic-block-create "call-cont-block")))
    (if (or (eq sym 'core:DECLARE) (eq sym 'cmp:DECLARE))
	(break "About to codegen-call a DECLARE expression - that means you didn't remove it from the head of a body and process it before codegen - See codegen-macrolet"))
    (let* ((new-env (irc-new-unbound-value-environment
		     old-env
		     :number-of-arguments (length (cdr form))
		     :label "call-args"))
	   traceid)
      (with-try new-env
	(progn
	  (setq traceid (trace-enter-call-scope new-env form))
	  (codegen-fill-value-frame (irc-renv new-env) (cdr form) old-env old-env)
	  (let* ((classified (classify-function-lookup old-env sym)))
	    (if (eq (car classified) 'core::global-function)
		(codegen-global-function-call result sym new-env)
		(codegen-lexical-function-call result (caddr classified) (cadddr classified) new-env))))
      	((cleanup)
	 (trace-exit-call-scope new-env traceid))))
    )
  )



(defun codegen-progn (result forms env)
  "Evaluate forms discarding results but keep last one"
  (if forms
      (do* ((cur forms (cdr cur))
	    (form (car cur) (car cur)))
	   ((endp cur) nil)
	(codegen result form env))
      (codegen-ltv-nil result env)))




(defun codegen-multiple-value-call (result rest env)
  (let ((accumulate-multiple-value-results (irc-alloca-tsp env "acc-multiple-value-results"))
	(func (irc-alloca-tsp env "func"))
	(accumulated-af (irc-alloca-afsp env "accumulated-activation-frame")))
    (irc-call env "makeNil" accumulate-multiple-value-results)
    (do* ((cur (cdr rest) (cdr cur))
	  (form (car cur) (car cur)))
	 ((endp cur) nil)
      (codegen result form env)
      (irc-call env "prependMultipleValues" accumulate-multiple-value-results result)
      )
    (irc-call env "makeValueFrameFromReversedCons" accumulated-af accumulate-multiple-value-results (irc-renv env) )
    (codegen func (car rest) env)
    (irc-call env "invokePossibleMultipleValueFunction" result func accumulated-af)
    ))


(defun codegen-multiple-value-prog1 (result rest env)
  (let ((temp-val (irc-alloca-tsp env "temp-val")))
    (codegen result (car rest) env)
    (do* ((cur (cdr rest) (cdr cur))
	  (form (car cur) (car cur)))
	 ((endp cur) nil)
      (codegen temp-val form env)
      )
    ))








(defun codegen-to-end-or-flow-change (result forms env)
  "Evaluate forms discarding results - if you hit a (go xxx) form then stop generating"
  (let ((last (do* ((cur forms (cdr cur))
		    (form (car cur) (car cur)))
		   ((or (endp cur)
			(and (consp form)
			     (or (eq (car form) 'go)
				 (eq (car form) 'return-from)))) form)
		(codegen result form env))))
    (when last
      (codegen result last env))))


(defun codegen-special-var-reference (var &optional env)
  (irc-call env "symbolValueReference" (irc-global-symbol var) (bformat nil "<special-var:%s>" var )))


(defun codegen-lexical-var-reference (depth-index env)
  (let ((renv (irc-renv env))
	(depth (car depth-index))
	(index (cadr depth-index)))
    (irc-call env"lexicalValueReference" (jit-constant-i32 depth) (jit-constant-i32 index) renv)))

(defun codegen-setq (result setq-pairs env)
  "Carry out setq for a collection of pairs"
  (if setq-pairs
      (do* ((cur setq-pairs (cddr cur))
	    (cur-var (car cur) (car cur))
	    (cur-expr (cadr cur) (cadr cur))
	    )
	   ((endp cur) nil)
	(cmp-log "Compiling setq for target[%s]\n" cur-var)
	(let* ((classified (irc-classify-variable env cur-var))
	       (target-ref (if (eq (car classified) 'core::special-var)
			       (codegen-special-var-reference cur-var env)
			       (codegen-lexical-var-reference (cddr classified) env))))
	  (codegen target-ref cur-expr env)
	  (irc-first-value-if-multiple-value target-ref)
	  (unless (cddr cur)
	    (irc-call env "copyTsp" result target-ref))))
      (codegen-special-var-lookup result 'nil env))
  )




(defun gather-lexical-variable-names (classified-symbols)
  (let (result)
    (mapc #'(lambda (x) (if (eq (car x) 'core:lexical-var)
			    (setq result (cons (cadr x) result))))
	  classified-symbols)
    (let ((rev-res (nreverse result)))
      (make-array (length rev-res) :initial-contents rev-res))))




(defun codegen-fill-let/let*-environment ( new-env vars number-of-lexical-vars exps parent-env evaluate-env)
  "Evaluate each of the exps in the evaluate-env environment
and put the values into the activation frame for new-env.
env is the parent environment of the (result-af) value frame"
  (let ((result-af (irc-renv new-env))
	(temp-val (irc-alloca-tsp new-env "temp-val")))
    (dbg-set-current-debug-location-here)
    ;;  (with-setup-insert-point env
    (irc-call parent-env "makeValueFrame" result-af (jit-constant-i32 number-of-lexical-vars) (irc-renv parent-env)) ;; irc-call "activationFrameParentRef" (irc-renv env)))
    ;;    )
    (cmp-log "About to generate code for exps: %s\n" exps)
    (irc-attach-debugging-info-to-value-frame result-af (gather-lexical-variable-names vars) new-env)
    (do* ((cur-req vars (cdr cur-req))
	  (classified-target (car cur-req) (car cur-req))
	  (cur-exp exps (cdr cur-exp))
	  (exp (car cur-exp) (car cur-exp)))
	 ((endp cur-req) nil)
      (let* ((target-head (car classified-target))
	     (target-idx (cdr classified-target)))
	(dbg-set-current-debug-location-here)
	(codegen temp-val exp evaluate-env)
	(irc-first-value-if-multiple-value temp-val)
	(compile-bind-target new-env temp-val classified-target)))))





(defun codegen-let/let* (operator-symbol result parts env)
  (dbg-set-current-debug-location-here)
  (let ((assignments (car parts))
	(body (cdr parts)))
    (multiple-value-bind (variables expressions)
	(separate-pair-list assignments)
      (multiple-value-bind (declares code docstring specials )
	  (process-declarations body)
	(cmp-log "About to create lambda-list-handler\n")
	(dbg-set-current-debug-location-here)
	(let* ((lambda-list-handler (make-lambda-list-handler variables declares 'core::function))
	       (new-env (irc-new-unbound-value-environment
			 env
			 :lambda-list-handler lambda-list-handler
			 :label (symbol-name operator-symbol)))
	       ;; There is a huge problem with setting evaluate-env for 'let
	       ;; in that the let evaluate-environment is not the same environment 
	       ;; used by with-try and codegen-fill-let/let*-environment
	       ;; The problem is that the code generated within codegen-fill-let/let*-environment
	       ;; connects to the env dispatcher rather than the new-env dispatcher
	       (evaluate-env (cond
			       ((eq operator-symbol 'let) env)   ;;; This is a problem right here
			       ((eq operator-symbol 'let*) new-env)
			       (t (error "let/let* doesn't understand operator symbol[~a]" operator-symbol))))
	       traceid)
	  (with-try new-env
	    (progn
	      (irc-branch-to-and-begin-block (irc-basic-block-create
					      (bformat nil "%s-start"
						       (symbol-name operator-symbol))))
	      (setq traceid (if (eq operator-symbol 'let)
				(trace-enter-let-scope new-env code)
				(trace-enter-let*-scope new-env code)))
	      (multiple-value-bind (reqvars)
		  (process-lambda-list-handler lambda-list-handler)
		(codegen-fill-let/let*-environment new-env (cdr reqvars)
						   (number-of-lexical-variables lambda-list-handler)
						   expressions env evaluate-env)
		;; Evaluate each of the expressions into the runtime environment
		;; Evaluate the body with the runtimeenvironment
		;;	  (let* ((filled-value-frame (codegen-fill-value-frame (irc-renv new-env) expressions evaluate-env)))
		(cmp-log "About to evaluate codegen-progn\n")
		(codegen-progn result code new-env)))
	    ((cleanup)
	     (if (eq operator-symbol 'let)
		 (trace-exit-let-scope new-env traceid)
		 (trace-exit-let*-scope new-env traceid))
	     (irc-unwind-environment new-env)
	     ))
	  )
	)))
  (cmp-log "Done codegen-let/let*\n")
  )










(defun compile-if-cond (cond env)
  "Generate code for cond that writes into result and then calls isTrue function that returns a boolean"
  (let (test-result)
    (let ((test-temp-store (irc-alloca-tsp env "if-cond-tsp")))
      (codegen test-temp-store cond env)
      (irc-first-value-if-multiple-value test-temp-store)
      (setq test-result (llvm-sys:create-icmp-eq *irbuilder* (irc-call env "isTrueTsp" test-temp-store) (jit-constant-i32 1) "ifcond")))
    test-result))




(defun codegen-if (result rest env)
  "See Kaleidoscope example for if/then/else"
  (let ((icond (car rest))
	(ithen (cadr rest))
	(ielse (caddr rest)))
    ;; codegen-if-cond generates code that returns true if cond evaluates to something other than nil
    (let ((condv (compile-if-cond icond env)))
;;      (unless condv (break "condv is nil") (return-from codegen-if nil)) ;; REALLY? return?????
      ;;Here we diverge a bit from the Kaleidoscope demo
      ;; condv should already return a bool so we don't have to convert it to one
      ;; example had: CondV = Builder.CreateFCmpONE(CondV,ConstantFP::get(getGlobalContext(),APFloat(0.0)),"ifcond")
;;      (break "make sure that divergence with kaleidoscope demo works")
      ;;      (setq condv (llvm-sys:create-fcmp-one))
      (let* ((thefunction (irc-current-function))
	     ;; Create blocks for the then and else cases. Insert the 'then' block at 
	     ;; the end of the function
	     (thenbb (irc-basic-block-create "then" thefunction))
	     (elsebb (irc-basic-block-create "else" ))
	     (mergebb (irc-basic-block-create "ifcont" )))
	(llvm-sys:create-cond-br *irbuilder* condv thenbb elsebb nil)
	(irc-set-insert-point thenbb)
	(dbg-set-current-debug-location-here)
	(irc-low-level-trace)
	(let ((thenv (codegen result #|REALLY? put result in result?|# ithen env)))
;;	  (unless thenv (break "thenv is nil") (return-from codegen-if nil))  ;; REALLY? return?????
	  (irc-branch-if-no-terminator-inst mergebb)
	  ;; Codegen of 'Then' can change the current block, update thenbb for the PHI.
	  ;; ---weird! We overwrite thenbb here!
	  (setq thenbb (llvm-sys:get-insert-block *irbuilder*))
	  ;; Emit else block
	  ;; Here I diverge a little bit because I'll use my '(append-basic-block thefunction XXXX)' function
	  (irc-begin-block elsebb)
	  ;; REALLY? Again- do I use result here?
	  (let ((elsev (codegen result ielse env)))
;;	    (unless elsev (break "elsev is nil") (return-from codegen-if nil)) ;; REALLY? return???
	    (irc-branch-if-no-terminator-inst mergebb)
	    ;;Codegen of 'else' can change the current block, update elsebb for the phi
	    (setq elsebb (llvm-sys:get-insert-block *irbuilder*))
	    ;; emit mergeblock
	    ;; Again use my 'append-basic-block' function
	    (irc-begin-block mergebb)
	    ;; HOW DO I CREATE-PHI??????????????
#|	    (let ((pn (llvm-sys:create-phi *irbuilder* +void+ 2 "iftmp")))
	    (llvm-sys:add-incoming pn thenv thenbb)
	    (llvm-sys:add-incoming pn elsev elsebb)
	    pn)
	      |#
	    ))))
    ;; Kaleidoscope demo has...
    ;;  PHINode *PN = Builder.CreatePHI(Type::getDoubleTy(getGlobalContext()), 2,"iftmp");
    ;;  PN->addIncoming(ThenV, ThenBB);
    ;;  PN->addIncoming(ElseV, ElseBB);
    ;;  return PN;
))






(defun extract-section (begin end)
  "Extract a section of a list from begin up to but not including end"
  (do* ((cur begin (cdr cur))
	(result nil))
       ((eq cur end) (cdr (nreverse result)))
    (setq result (cons (car cur) result))))



(defun tagbody.enumerate-tag-blocks (code tagbody-env)
  (let (result (index 0))
    (mapl #'(lambda (x) 
	      (if (and (car x) (symbolp (car x)))
		  (progn
		      (setq result (cons (list index (irc-basic-block-create (format nil "tagbody-~a-~a" (car x) index)) x) result))
		      (add-tag tagbody-env (car x) result)
		      (setq index (+ 1 index))))) code)
    (nreverse result)))




(defun codegen-tagbody (result rest env)
  "Extract tags and code from (rest) and create an alist that maps
tag symbols to code and llvm-ir basic-blocks. Store the alist in the symbol-to-block-alist
metadata of the tagbody-env. These can be accessed by (go XXXX) special operators to
jump to blocks within this tagbody."
  (dbg-set-current-debug-location-here)
  (unless (and (car rest) (symbolp (car rest))) (push (gensym) rest)) ;; stick a dummy tag at the head if there isn't one
  (let ((tagbody-env (make-tagbody-environment env)))
      (let ((enumerated-tag-blocks (tagbody.enumerate-tag-blocks rest tagbody-env)))
	(let ((debug-macro (macroexpand '
			    (with-try tagbody-env
			      (mapl #'(lambda (cur)
					(let* ((tag-begin (car cur))
					       (tag-end (cadr cur))
					       (section-block (cadr tag-begin))
					       (section-next-block (cadr tag-end))
					       (section (extract-section (caddr tag-begin) (caddr tag-end))))
					  (irc-branch-if-no-terminator-inst section-block)
					  (irc-begin-block section-block)
					  (codegen-progn result section tagbody-env)
					  (when section-next-block (irc-branch-if-no-terminator-inst section-next-block))
					  ))
				    enumerated-tag-blocks)
			      ((cleanup) (codegen-ltv-nil result env))
			      ((typeid-core-go exception-ptr)
			       (let* ((go-index (irc-call env "tagbodyGoIndexElseRethrow" exception-ptr))
				      (default-block (irc-basic-block-create "switch-default"))
				      (sw (irc-switch go-index default-block (length enumerated-tag-blocks))))
				 (mapc #'(lambda (one) (llvm-sys:add-case sw (jit-constant-i32 (car one))
									  (cadr one))) enumerated-tag-blocks)
				 (irc-begin-block default-block)
				 (irc-call tagbody-env "throwIllegalSwitchValue"
					   go-index (jit-constant-i32 (length enumerated-tag-blocks)))
				 ))))))
	  )
	(with-try tagbody-env
	  (mapl #'(lambda (cur)
		    (let* ((tag-begin (car cur))
			   (tag-end (cadr cur))
			   (section-block (cadr tag-begin))
			   (section-next-block (cadr tag-end))
			   (section (extract-section (caddr tag-begin) (caddr tag-end))))
		      (irc-branch-if-no-terminator-inst section-block)
		      (irc-begin-block section-block)
		      (codegen-progn result section tagbody-env)
		      (when section-next-block (irc-branch-if-no-terminator-inst section-next-block))
		      ))
		enumerated-tag-blocks)
	  ((cleanup) (codegen-ltv-nil result env))
	  ((typeid-core-go exception-ptr)
	   (let* ((go-index (irc-call env "tagbodyGoIndexElseRethrow" exception-ptr))
		  (default-block (irc-basic-block-create "switch-default"))
		  (sw (irc-switch go-index default-block (length enumerated-tag-blocks))))
	     (mapc #'(lambda (one) (llvm-sys:add-case sw (jit-constant-i32 (car one))
						      (cadr one))) enumerated-tag-blocks)
	     (irc-begin-block default-block)
	     (irc-call tagbody-env "throwIllegalSwitchValue"
		       go-index (jit-constant-i32 (length enumerated-tag-blocks)))
	     )))
	)
    ))



(defun codegen-go (result rest env)
  (dbg-set-current-debug-location-here)
  (let* ((tag (car rest))
	 (classified-tag (classify-tag env tag)))
    (if classified-tag
	(let ((depth (car classified-tag))
	      (index (cdr classified-tag)))
	  (irc-call env "throw_Go" (jit-constant-i32 depth) (jit-constant-i32 index)))
	(error "go to unknown tag ~a" tag))))








(defun codegen-block (result block-symbol body env)
  "codegen-block using the try macro"
  (let ((block-env (irc-new-block-environment env :name block-symbol))
	traceid)
    (let ((block-start (irc-basic-block-create
			(bformat nil "block-%s-start" (symbol-name block-symbol)))))
      (irc-br block-start)
      (irc-begin-block block-start))
    (with-try block-env
      (progn
	(setq traceid (trace-enter-block-scope block-env `(block ,block-symbol ,body)))
	(codegen-progn result body block-env))
      ((cleanup)
       (trace-exit-block-scope block-env traceid)
       (irc-unwind-environment block-env))
      ((typeid-core-return-from exception-ptr)
       (irc-call block-env "blockHandleReturnFrom" result exception-ptr))
      )
    ))





(defun codegen-return-from (result rest env)
  (dbg-set-current-debug-location-here)
  (let* ((block-symbol (car rest))
	 (return-form (cadr rest))
	 (recognizes-block-symbol (recognizes-block-symbol env block-symbol)))
    (if recognizes-block-symbol
	(let ((block-depth (calculate-block-depth env block-symbol)))
	  (codegen result return-form env)
	  (irc-call env "throwReturnFrom" (jit-constant-i32 block-depth) result))
	(error "Unrecognized block symbol ~a" block-symbol))))



(defun codegen-fill-function-frame (function-env functions parent-env closure-env)
  "Create a closure for each of the function bodies in the flet/labels and put the closures into the activation frame in (result-af). (env) is the parent environment of the (result-af) value frame"
  (let ((result-af (irc-renv function-env)))
    (dbg-set-current-debug-location-here)
    ;;  (with-setup-insert-point env
    (irc-call parent-env "makeFunctionFrame" result-af (jit-constant-i32 (length functions)) (irc-renv parent-env)) ;; irc-call "activationFrameParentRef" (irc-renv env)))
    ;;    )
    (cmp-log "About to generate code for args\n")
    (do* ((cur functions (cdr cur)))
	 ((endp cur) nil)
      (let* ((fn (car cur))
	     (fn-name (car fn))
	     (fn-lambda `(lambda-block ,fn-name ,@(cdr fn)))
	     (fn-classified (classify-function-lookup function-env fn-name))
	     (fn-index (or (cadddr fn-classified) (error "Could not find lexical function ~a" fn-name)))
	     (target (irc-call parent-env "functionFrameReference" result-af (jit-constant-i32 fn-index)
			       (bformat nil "%s-ref-%d" (llvm-sys:get-name result-af) fn-index) )))
	(codegen-closure target fn-lambda closure-env)))))





(defun codegen-flet/labels (operator-symbol result rest env)
  (let* ((functions (car rest))
	 (body (cdr rest))
	 (function-env (irc-new-function-value-environment env :functions functions)))
    (multiple-value-bind (declares code docstring specials)
	(process-declarations body nil) ;; don't expect docstring
      (let ((evaluate-env (cond
			    ((eq operator-symbol 'flet) env)
			    ((eq operator-symbol 'labels) function-env)
			    (t (error "flet/labels doesn't understand operator symbol[~a]" operator-symbol))))
	    traceid)
	(with-try evaluate-env
	  (progn
	    (irc-branch-to-and-begin-block (irc-basic-block-create
					    (bformat nil "%s-start"
						     (symbol-name operator-symbol))))
	    (setq traceid (if (eq operator-symbol 'flet)
			      (trace-enter-flet-scope function-env code)
			      (trace-enter-labels-scope function-env code)))
	    (codegen-fill-function-frame function-env functions env evaluate-env)
	    (codegen-progn result code function-env))
	  ((cleanup)
	   (if (eq operator-symbol 'flet)
			      (trace-exit-flet-scope function-env traceid)
			      (trace-exit-labels-scope function-env traceid))
	   (irc-unwind-environment function-env)))
	))))



(defun codegen-macrolet (result rest env)
  (let* ((macros (car rest))
	 (body (cdr rest))
	 (macro-env (irc-new-macrolet-environment env)))
    (mapc #'(lambda (macro-def &aux (name (car macro-def))
				 (vl (cadr macro-def))
				 (macro-body (cddr macro-def)))
	      (let* ((lambda-block (parse-macro name vl macro-body))
		     (macro-fn (eval (list 'function lambda-block))))
		(set-kind macro-fn :macro)
		(add-macro macro-env name macro-fn)))
	  macros )
    (multiple-value-bind (declares code docstring specials )
	(process-declarations body)
      (augment-environment-with-declares macro-env declares)
      (codegen-progn result code macro-env))))










(defun codegen-eval-when (result rest env)
;;  (break "codegen-eval-when")
  (let ((situations (car rest))
	(body (cdr rest)))
    (when (member :execute situations)
      (codegen-progn result body env))))



(defun codegen-the (result rest env)
  (codegen result (cadr rest) env))


(defun codegen-locally (result rest env)
  (multiple-value-bind (declarations code doc-string specials)
      (process-declarations rest nil)
    (let ((new-env (irc-new-unbound-value-environment
		    env
		    :number-of-arguments (length specials)
		    :label "locally-env")))
      ;; TODO: A runtime environment will be created with space for the specials
      ;; but they aren't used - get rid of them
      (irc-call env "makeValueFrame" (irc-renv new-env)
		(jit-constant-i32 0)
		(irc-renv env))
      (dolist (sp specials)
	(value-environment-define-special-binding new-env sp))
      (codegen-progn result code new-env)
      )))






(defun codegen-unwind-protect (result rest env)
  (let* ((up-env (irc-new-unwind-protect-environment env))
	 (protected-form (car rest))
	 (unwind-form (cadr rest)))
    ;;Codegen the protected-form unwind to the unwind-landing-pad-block
    (with-try up-env
      (progn
	(irc-branch-to-and-begin-block (irc-basic-block-create "unwind-protect-start"))
	(codegen result protected-form up-env))
      ((cleanup)
       (codegen result unwind-form up-env)
       (irc-unwind-environment up-env))
      )
    ))




(defun codegen-throw (result rest env)
  (let ((tag (car rest))
	(result-form (cadr rest)))
    (let ((tag-store (irc-alloca-tsp env "tag-store"))
	  (result-form-store (irc-alloca-tsp env "result-form-store")))
      (codegen tag-store tag env)
      (codegen result-form-store result-form env)
      (irc-call env "throwCatchThrow" tag-store result-form-store))))


(defun codegen-catch (result rest env)
  (let* ((catch-env (irc-new-catch-environment env))
	 (tag (car rest))
	 (body (cdr rest))
	 (tag-store (irc-alloca-tsp catch-env "tag-store"))
	 (tag-unwind-store (irc-alloca-tsp catch-env "tag-unwind-store"))
	 traceid)
    (codegen tag-store tag catch-env)
    (irc-call catch-env "catchStoreTag" tag-unwind-store tag-store)
    (with-try catch-env
      (progn
	(setq traceid (trace-enter-catch-scope catch-env rest))
	(codegen-progn result body catch-env)
	)
      ((cleanup)
       (trace-exit-catch-scope catch-env traceid)
       (irc-unwind-environment catch-env))
      ((typeid-core-catch-throw exception-ptr)
       (irc-call catch-env "catchIfTagMatchesStoreResultElseRethrow"
		 result tag-store exception-ptr))
      ((all-other-exceptions)
       (progn
	 (irc-call catch-env "catchUnwind" tag-unwind-store)
	 (irc-rethrow catch-env)
	 ))
      )))


(defun codegen-load-time-value (result rest env)
  (let* ((form (car rest))
	 (read-only-p (cadr rest)))
    (if *generate-compile-file-load-time-values*
	(with-load-time-value (ltv-ref result form env)
	  :coalesce-hash-table *load-time-value-coalesce*
	  :maker (codegen result form env))
	(codegen result form nil))))



(defun split-vars-declares-forms (parts)
  (let ((vars (car parts))
	(rest (cdr parts))
	cur declares result)
    (if (not rest)
	(setq result (values vars nil nil))
	(tagbody
	 top
	   (setq cur (car rest))
	   (if (or (not rest) (not (eq (car cur) 'declare))) (go done))
	   (setq declares (cons cur declares))
	   (setq rest (cdr rest))
	   (go top)
	 done	   ))
    (values vars (nreverse declares) rest)))
#|
	(split-vars-declares-forms '((a b) (declare z1) (declare z2) (print "Hi")))
	(split-vars-declares-forms '((a b) ))
	|#
   

(defparameter *nexti* 10000)
(defun codegen-dbg-i32 (result rest env)
  (let ((giveni (car rest)))
    (if (null giveni)
	(progn
	  (setq giveni *nexti*)
	  (setq *nexti* (+ 1 *nexti*))))
    (irc-call env "debugPrintI32" (jit-constant-i32 giveni))))


(defun codegen-special-operator (result head rest env)
  (cond
    ((eq head 'progn) (codegen-progn result rest env))
    ((eq head 'setq) (codegen-setq result rest env))
    ((eq head 'let) (codegen-let/let* 'let result rest env))
    ((eq head 'let*) (codegen-let/let* 'let* result rest env))
    ((eq head 'if) (codegen-if result rest env)) 
    ((eq head 'function) (codegen-function result (car rest) env))
    ((eq head 'block) (codegen-block result (car rest) (cdr rest) env))
    ((eq head 'return-from) (codegen-return-from result rest env))
    ((eq head 'tagbody) (codegen-tagbody result rest env))
    ((eq head 'go) (codegen-go result rest env))
    ((eq head 'multiple-value-call) (codegen-multiple-value-call result rest env))
    ((eq head 'multiple-value-prog1) (codegen-multiple-value-prog1 result rest env ))
    ((eq head 'flet) (codegen-flet/labels 'flet result rest env))
    ((eq head 'labels) (codegen-flet/labels 'labels result rest env))
    ((eq head 'eval-when) (codegen-eval-when result rest env)) 
    ((eq head 'the) (codegen-the result rest env ))
    ((eq head 'locally) (codegen-locally result rest env))
    ((eq head 'quote) (codegen-quote result rest env)) ;; `(quote ,@(cdr form)))
    ((eq head 'throw) (codegen-throw result rest env))
    ((eq head 'unwind-protect) (codegen-unwind-protect result rest env ))
    ((eq head 'catch) (codegen-catch result rest env))
    ((eq head 'macrolet) (codegen-macrolet result rest env))
    ((eq head 'dbg-i32) (codegen-dbg-i32 result rest env))
    ((eq head 'load-time-value) (codegen-load-time-value result rest env))

    ((eq head 'progv) (codegen-progv result rest env))
    ((eq head 'symbol-macrolet) (codegen-symbol-macrolet result rest env ))
    (t (error "Unknown special operator: ~a" head))
    )
)



(defun compile-macroexpand (form env)
  (macroexpand form env))

(defun codegen-application (result form env)
  "A macro function or a regular function"
  ;;  (break "codegen-application")
  (if (macro-function (car form) env)
      (multiple-value-bind (expansion expanded-p)
	  (compile-macroexpand form env)
	(cmp-log "MACROEXPANDed form[%s] expanded to [%s]\n" form expansion )
	(codegen result expansion env))
      ;; It's a regular function call
      (codegen-call result form env)))



(defun augmented-special-operator-p (x)
  (or (special-operator-p x) (eq x 'cmp:dbg-i32)))


(defun codegen-atom (result obj env)
  "Generate code to generate the load-time-value of the atom "
  (if *generate-compile-file-load-time-values*
      (cond
	((null obj) (codegen-ltv-nil result env))
	((integerp obj) (codegen-ltv-integer result obj env))
	((stringp obj) (codegen-ltv-string result obj env))
	((floatp obj) (codegen-ltv-float result obj env))
	((symbolp obj) (codegen-symbol result obj env))
	((characterp obj) (codegen-ltv-character result obj env))
	((arrayp obj) (codegen-array result obj env))
	((hash-table-p obj) (codegen-ltv-container result obj env))
	(t (error "Add support to codegen the atom type ~a - value: ~a" (class-name obj) obj )))
      ;; Below is how we compile atoms for COMPILE - literal objects are passed into the
      ;; default module without coalescence.
      (codegen-run-time-value result obj env)))


(defun codegen (result form env)
  (declare (optimize (debug 3)))
  (let* ((parse-pos (if (consp form)
			(walk-to-find-parse-pos form)
			nil))
	 (*current-form* form)
	 (*current-env* env)
	 (*current-line-number* (if parse-pos
				    (source-file-info-lineno parse-pos)
				    0))
	 (*current-column* (if parse-pos
			       (source-file-info-column parse-pos)
			       0)))
    (cmp-log "codegen %s\n" form)
    (if (atom form)
	(codegen-atom result form env)
	(let ((head (car form))
	      (rest (cdr form)))
	  (cmp-log "About to codegen special-operator or application for: %s\n" form)
	  ;;	(trace-linenumber-column (walk-to-find-parse-pos form) env)
	  (if (and head (symbolp head) (augmented-special-operator-p head))
	      (codegen-special-operator result head rest env)
	      (codegen-application result form env))))))



;;------------------------------------------------------------
;;
;; Create a repl function
;; It takes no arguments, returns an object and is invoked using apply
;;
;;


(defun compile-thunk (name form env)
  "Compile the form into an llvm function and return that function"
  (dbg-set-current-debug-location-here)
  (let ((fn (with-new-function (fn fn-env :function-name name :parent-env env :function-form form)
	      (set-env-file.dbg fn-env)
	      (let* ((given-name (llvm-sys:get-name fn)))
		;; Map the function argument names
		(cmp-log "Creating repl function with name: %s\n" given-name)
		(let ((result (car (llvm-sys:get-argument-list fn))))
		  ;;	(break "codegen repl form")
		  (dbg-set-current-debug-location-here)
		  (codegen result form fn-env)
		  (dbg-set-current-debug-location-here) 
		  )))))
    (cmp-log "Dumping the repl function\n")
;;    (log-dump fn)
    (irc-verify-function fn t)
    fn))
#|
    (let ((closure (llvm-sys:get-closure *the-execution-engine* 'repl fn nil 'function)))
      (llvm-sys:dump fn)
      (break "Just created closure - check fn above")
      (cmp-log "About to return lisp-function\n")
      (values closure  fn))
    ))
|#



#+(or)
(defun compile-general-function (name lambda-list-handler code old-env &optional is-macro)
  (cmp-log "general function name: %s\n" name)
  (cmp-log "general function code: %s\n" code)
  (dbg-set-current-debug-location-here)
  (let ((fn (compile-lambda/lambda-block (symbol-name name) lambda-list-handler nil "" code old-env)))
    (values (llvm-sys:get-closure *the-execution-engine*
				  name fn (irc-environment-activation-frame old-env)
				  (if is-macro
				      :macro
				      :function))
	    fn)))



(defun compile-interpreted-function (name func)
  (let ((is-macro (macrop func))
	(code (get-code func))
	(env (closed-environment func))
	(lambda-list-handler (get-lambda-list-handler func)))
    (dbg-set-current-debug-location-here)

    (let* ((fn (compile-lambda/lambda-block (symbol-name name)
					    lambda-list-handler nil "" code env))
	   (closure (llvm-sys:get-closure *the-execution-engine*
					  name fn
					  (irc-environment-activation-frame env)
					  (if is-macro
					      :macro
					      :function))))
      (core::setf-symbol-function name closure))))



(defun compile (name &optional definition)
  "See CLHS compile"
  (cond
    ((functionp definition)
     (error "Handle compile with definition = function"))
    ((consp definition)
     (let ((fn (compile-lambda-function definition nil (if name
							   (symbol-name name)
							   "lambda"))))
       (when name
	 (error "Save the compiled function in the symbol name: ~a" name))
       fn))
    ((null definition)
     (let ((func (symbol-function name)))
       (cond
	 ((is-assignable-to func (find-class 'interpreted))
	  (compile-interpreted-function name func)))))
    (t (error "Illegal combination of arguments for compile: ~a ~a" name definition))))



(defun compile-run (form)
  (let* ((fn (compile-thunk "repl" form nil))
	 (closure (llvm-sys:get-closure *the-execution-engine* 'repl fn nil 'function)))
    (llvm-sys:dump fn)
    (break "Just created closure - check fn above")
    (cmp-log "About to return lisp-function\n")
    (debug-log-on)
    (let ((r (apply closure nil)))
      (bformat t "\n\nRESULT OF JIT EVALUATION-----> %s\n" r)
      r)))




  
