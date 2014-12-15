;;;
;;;    File: cmpast.lsp
;;;

;; Copyright (c) 2014, Christian E. Schafmeister
;;
;; CLASP is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Library General Public
;; License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.
;;
;; See directory 'clasp/licenses' for full details.
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;; -^-

;;
(in-package :cmp)

(defstruct ast-node
  env)

(defstruct (ast-function :include ast-node) )

(defstruct (ast-function-symbol-lookup (:include ast-function)))

(defstruct (ast-global-function-lookup (:include ast-function-symbol-lookup))
  symbol)

(defstruct (ast-lexical-function-lookup (:include ast-function-symbol-lookup))
  classified)

(defstruct (ast-function-setf-symbol-lookup (:include ast-function)))
(defstruct (ast-global-setf-function-lookup (:include ast-function-setf-symbol-lookup))
  setf-func)
(defstruct (ast-lexical-setf-function-lookup (:include ast-function-setf-symbol-lookup))
  classified)

(defstruct (ast-closure (:include ast-node))
  lambda-or-lambda-block
  func-name-str
  compiled-fn)

(defstruct (ast-lambda-function (:include ast-node)))

(defstruct (ast-lambda/lambda-block (:include ast-node)))



(defun parse-macro (name vl body &optional env)
  (multiple-value-bind (lblock ppn doc)
      (si::expand-defmacro name vl body)
    lblock))

(export 'parse-macro)



(defun augment-environment-with-declares (env declares)
  ;; Do nothing for now
  nil
  )


;;
;; codegen for atoms
;;








#+(or)
(defun compile-only-required-arguments (lambda-list-handler old-env activation-frame)
  "Create a new environment that expands env with the required arguments in lambda-list-handler.
Also allocate a new runtime-env and copy the activation-frame into it because it only contains required arguments.
Return the new environment."
  (irc-new-value-environment
   old-env
   :number-of-arguments (number-of-lexical-variables lambda-list-handler)
   :label "copy-req-args"
   :fill-runtime-form (lambda (new-env) (irc-intrinsic "copyAFsp" (irc-renv new-env) activation-frame))))




(defun compile-arguments (fn-name	; passed for logging only
			  lambda-list-handler ; llh for function
			  function-env
			  closed-over-renv
			  argument-holder
			  new-env
			  )
  (cmp-log "compile-arguments closed-over-renv: %s\n" closed-over-renv)
  ;; This is where we make the value frame for the passed arguments
  ;;
  ;; keywords:  ESCAPE ANALYSIS escape analysis TODO
  ;; Currently all bindings are on the heap
  ;; - we should move some/all of these bindings onto the stack
  ;; There should be information in new-env??? that can tell us what
  ;; variables can go on the stack and what variables can go on the heap
  (irc-make-value-frame (irc-renv new-env) (number-of-lexical-variables lambda-list-handler))
  (if (lambda-list-handler-required-lexical-arguments-only-p lambda-list-handler)
      (compile-copy-only-required-arguments new-env
					    lambda-list-handler
					    (irc-renv new-env)
					    argument-holder
					    )
      (let ((lexical-names (names-of-lexical-variables lambda-list-handler)))
	;;	(break "I need to deal with more complex lambda-list that contains more than required arguments")
	(cmp-log "lambda-list-handler for fn %s --> %s\n" fn-name lambda-list-handler)
	(cmp-log "Gathered lexical variables for fn %s --> %s\n" fn-name lexical-names)
	(compile-lambda-list-code lambda-list-handler
				  function-env
				  argument-holder
				  new-env)
	(dbg-set-current-debug-location-here)
	))
;;  (irc-intrinsic "debugInspectActivationFrame" closed-over-renv)
;;  (irc-intrinsic "debugInspectActivationFrame" (irc-renv new-env))
;;  (irc-intrinsic "gdb")
  (irc-intrinsic "setParentOfActivationFrame" (irc-renv new-env) closed-over-renv)
  (irc-attach-debugging-info-to-value-frame (irc-renv new-env) lambda-list-handler new-env)
  )




(defparameter *lambda-args-num* 0)

(defvar *all-funcs-for-one-compile* nil
  "All functions for one COMPILE are accumulated in this dynamic variable.
COMPILE-FILE just throws this away")

WORKING HERE WORKING HERE
(defun cast-lambda/lambda-block (name ; Name of function that will result from this lambda
				    lambda-list-handler
				    declares  ; lambda declares
				    docstring ; lambda docstring
				    code      ; code for lambda
				    env-around-lambda ; environment of the lambda
				    &key wrap-block ; wrap code in a block
				      block-name)   ; name of block
  (setq *lambda-args-num* (1+ *lambda-args-num*))
  (cmp-log "About to compile-lambda/lambda-block for lambda-list-handler: %s\n" lambda-list-handler)
  (cmp-log "         compile-lambda/lambda-block code: %s\n" code)
  (cmp-log "      compile-lambda/lambda-block env-around-lambda: %s\n" env-around-lambda)
  (or (stringp name) (error "name must be a string"))
  (let ((fn
	 (with-new-function (fn fn-env
				:function-name name
				:parent-env env-around-lambda
				:function-form code)
	   (let* ((arguments (llvm-sys:get-argument-list fn))
		  (result (first arguments))
		  (closed-over-renv (second arguments))
		  (argument-holder #-varargs (third arguments) ; if #-varargs then use argument-activation-frame (third arguments)
				   #+varargs (list (third arguments) (fourth arguments))) ; if #+varargs then use (list nargs va-list)
		  traceid
		  (new-env (irc-new-value-environment
			    fn-env
			    :lambda-list-handler lambda-list-handler
			    :label (bformat nil "lambda-args-%s-" *lambda-args-num*)
			    :fill-runtime-form (lambda (lambda-args-env)
						 (compile-arguments name
								    lambda-list-handler
								    fn-env
								    closed-over-renv
								    argument-holder
								    lambda-args-env
								    )))))
	     (dbg-set-current-debug-location-here)
;;;		(irc-attach-debugging-info-to-value-frame (irc-renv new-env) lambda-list-handler new-env)
	     (with-try new-env
	       (if wrap-block
		   (codegen-block result (list* block-name code) new-env)
		   (codegen-progn result code new-env))
	       ((cleanup)
		(irc-unwind-environment new-env)))
	     )
	   )))
    (cmp-log-dump fn)
    (irc-verify-function fn)
    (push fn *all-funcs-for-one-compile*)
    fn
    ))





(defun cast-lambda-function (lambda env &key (function-name-as-string "lambda"))
  (let* (block-name lambda-list body)
    (if (eq (car lambda) 'ext::lambda-block)
	(setq block-name (function-block-name (cadr lambda))
	      ;; function-name-as-string could be (setf xxx) or xxx - should I use (function-block-name (cadr lambda))?
	      function-name-as-string (bformat nil "%s" (cadr lambda))
	      lambda-list (caddr lambda)
	      body (cdddr lambda))
	(setq lambda-list (cadr lambda)
	      body (cddr lambda)))
    (multiple-value-bind (declares code docstring specials )
	(process-declarations body)
      (cmp-log "About to create lambda-list-handler\n")
      (dbg-set-current-debug-location-here)
      (let ((lambda-list-handler (make-lambda-list-handler lambda-list declares 'core::function)))
	(let ((lambda-function (cast-lambda/lambda-block function-name-as-string
							 lambda-list-handler
							 declares
							 docstring
							 code
							 env
							 :wrap-block (eq (car lambda) 'ext::lambda-block)
							 :block-name block-name
							 )))
	  ;; Now I have the lambda-function which is a llvm::Function* and the environment (env)
	  ;; I want to generate code that takes these and returns a Closure object
	  ;;
	  lambda-function)))))



(defun cast-closure (lambda-or-lambda-block env &key (func-name-str "lambda"))
  "codegen a closure.  If result is defined then put the compiled function into result
- otherwise return the cons of llvm-sys::Function_sp's that were compiled for the lambda"
  (let* ((function-name (cond   ;; function-name can be a symbol or a cons of the form (SETF _name_)
			  ((eq (car lambda-or-lambda-block) 'lambda) 'lambda)
			  ((eq (car lambda-or-lambda-block) 'ext::lambda-block) (cadr lambda-or-lambda-block))
			  (t 'unknown-func-name-str-in-codegen-closure)))
	 (compiled-fn (cast-lambda-function lambda-or-lambda-block env :function-name-as-string func-name-str)))
    (make-ast-closure :lambda-or-lambda-block lambda-or-lambda-block
		      :func-name-str func-name-str
		      :compiled-fn compiled-fn
		      :env env)))




(defun cast-global-function-lookup (sym env)
  (make-ast-function-symbol-global-lookup :symbol sym :env env))


(defun cast-global-setf-function-lookup (result setf-function-name env)
  (let ((setf-symbol (cadr setf-function-name)))
    (irc-intrinsic "setfSymbolFunctionRead" result (irc-global-setf-symbol setf-symbol env))
  ))


(defun cast-lexical-function-lookup (reference env)
  (make-ast-function-symbol-lexical-lookup :classified reference :env env))

(defun cast-function-symbol-lookup (func env)
  "Classify the function and look it up and put it in result"
  (let* ((classified (classify-function-lookup env func)))
    (if (eq (car classified) 'core::global-function)
	(cast-global-function-lookup func env)
	(cast-lexical-function-lookup (caddr classified) (cadddr classified) env)))
)


(defun codegen-function-setf-symbol-lookup (result setf-func env)
  "Classify the (setf XXXX) function and put it in the result"
  (let* ((classified (classify-function-lookup env setf-func)))
    (if (eq (car classified) 'core::global-function)
	(cast-global-setf-function-lookup  :setf-func setf-func :env env)
	(cast-lexical-setf-function-lookup :classified classified :env env))))





(defun cast-function (rest env)
  "Return IR code for a function or closure"
  (let ((name-or-lambda (car rest)))
  (assert-result-isa-llvm-value result)
  (dbg-set-current-debug-location-here)
  (cmp-log "About to codegen-function for: %s\n" name-or-lambda)
  (cond
    ((and name-or-lambda (symbolp name-or-lambda))
     (cast-function-symbol-lookup name-or-lambda env))
    ((and (consp name-or-lambda)
	  (eq (car name-or-lambda) 'setf))
     (cast-function-setf-symbol-lookup name-or-lambda env))
    ((and (consp name-or-lambda)
	  (or (eq (car name-or-lambda) 'lambda)
	      (eq (car name-or-lambda) 'ext::lambda-block)))
     (cast-closure name-or-lambda env))
    (t (error "FUNCTION special operator only supports symbol names or lambda expression - you gave: ~a" name-or-lambda)))))









(defun codegen-progn (result forms env)
  "Evaluate forms discarding results but keep last one"
  (cmp-log "About to codegen-progn with forms: %s\n" forms)
  (if forms
      (let ((temp-val (irc-alloca-tsp env :label "temp")))
	(do* ((cur forms (cdr cur))
	      (form (car cur) (car cur)))
	     ((endp cur) nil)
	  (if (not (null (cdr cur)))
	      (codegen temp-val form env)
	      (codegen result form env))))
      (codegen-literal result nil env)))



(defun codegen-progv (result args env)
  (cmp-log "Started codegen-progv\n")
  (let ((symbols (car args))
	(values (cadr args))
	(forms (cddr args))
	(evaluated-symbols (irc-alloca-tsp env :label "symbols"))
	(evaluated-values (irc-alloca-tsp env :label "values"))
	(save-specials (irc-alloca-i8* env :label "specials")))
    (cmp-log "Evaluating symbols: %s\n" symbols)
    (codegen evaluated-symbols symbols env)
    (cmp-log "Evaluating values: %s\n" values)
    (codegen evaluated-values values env)
    (cmp-log "About to setup evaluation of forms env: %s\n" env)
    (with-try env
      (progn
	(cmp-log "About to call progvSaveSpecials\n")
	(irc-intrinsic "progvSaveSpecials" save-specials evaluated-symbols evaluated-values)
	(cmp-log "About to codegen-progn with: %s\n" forms)
	(codegen-progn result forms env))
      ((cleanup)
       (irc-intrinsic "progvRestoreSpecials" save-specials)))))



(defun codegen-multiple-value-call (result rest env)
  (with-dbg-lexical-block (env rest)
    (let ((accumulate-results (irc-alloca-tsp env :label "acc-multiple-value-results"))
	  (temp-mv-result (irc-alloca-tmv env :label "temp-mv-result"))
	  (funcDesignator (irc-alloca-tsp env :label "funcDesignator"))
	  (func (irc-alloca-Function_sp env :label "func"))
	  (accumulated-af (irc-alloca-afsp env :label "accumulated-activation-frame")))
      (codegen-literal accumulate-results nil env)
      (do* ((cur (cdr rest) (cdr cur))
	    (form (car cur) (car cur)))
	   ((endp cur) nil)
	(codegen temp-mv-result form env)
	(irc-intrinsic "prependMultipleValues" accumulate-results temp-mv-result)
	)
      (irc-intrinsic "makeValueFrameFromReversedCons" accumulated-af accumulate-results (jit-constant-i32 (irc-next-environment-id)))
      (irc-intrinsic "setParentOfActivationFrame" accumulated-af (irc-renv env))
      (codegen funcDesignator (car rest) env)
      (irc-intrinsic "va_coerceToFunction" func funcDesignator)
      (irc-intrinsic "FUNCALL_activationFrame" result func accumulated-af) ; used to use temp-mv-result  as result
;;      (irc-intrinsic "copyTmvOrSlice" result temp-mv-result)
      )
    ))


(defun codegen-multiple-value-prog1 (result rest env)
  (with-dbg-lexical-block (env rest)
    (let ((temp-mv-result (irc-alloca-tmv env :label "temp-mv-result"))
	  (saved-values (irc-alloca-tsp env :label "multiple-value-prog1-saved-values"))
	  (temp-val (irc-alloca-tsp env :label "temp-val")))
      ;; See the interpreter sp_multipleValueCall
      (codegen temp-mv-result (car rest) env)
      (irc-intrinsic "saveValues" saved-values temp-mv-result)
      (do* ((cur (cdr rest) (cdr cur))
	    (form (car cur) (car cur)))
	   ((endp cur) nil)
	(codegen temp-val form env)
	)
      (irc-intrinsic "loadValues" temp-mv-result saved-values)
      (irc-intrinsic "copyTmvOrSlice" result temp-mv-result)
      )))









(defun codegen-special-var-reference (var &optional env)
  (irc-intrinsic "symbolValueReference" (irc-global-symbol var env) (bformat nil "<special-var:%s>" var )))


(defun codegen-lexical-var-reference (depth-index env)
  (let ((renv (irc-renv env))
	(depth (car depth-index))
	(index (cadr depth-index)))
    (irc-intrinsic "lexicalValueReference" (jit-constant-i32 depth) (jit-constant-i32 index) renv)))

(defun codegen-setq (result setq-pairs env)
  "Carry out setq for a collection of pairs"
  (let ((temp-res (irc-alloca-tsp env :label "tsetq")))
    (if setq-pairs
	(do* ((cur setq-pairs (cddr cur))
	      (cur-var (car cur) (car cur))
	      (cur-expr (cadr cur) (cadr cur))
	      )
	     ((endp cur) nil)
	  (cmp-log "Compiling setq for target[%s]\n" cur-var)
	  (let ((expanded (macroexpand cur-var env)))
	    (if (eq expanded cur-var)
		;; symbol was not macroexpanded use SETQ
		(progn
		  (cmp-log "The symbol[%s] was not macroexpanded - using SETQ to set it\n" cur-var)
		  (let* ((classified (irc-classify-variable env cur-var))
			 (target-ref (if (eq (car classified) 'ext:special-var)
					 (codegen-special-var-reference cur-var env)
					 (codegen-lexical-var-reference (cddr classified) env))))
		    (codegen temp-res cur-expr env)
		    (irc-intrinsic "copyTsp" target-ref temp-res)
		    )
		  )
		;; symbol was macroexpanded use SETF
		(progn
		  (cmp-log "The symbol[%s] was macroexpanded to result[%s] setting with SETF\n" cur-var expanded)
		  (codegen temp-res `(setf ,expanded ,cur-expr) env)
		  )
		))
	  (unless (cddr cur)
	    (irc-intrinsic "copyTsp" result temp-res))

	  )
	;; There were no pairs, return nil
	(codegen-literal result nil env)))
  )




(defun gather-lexical-variable-names (classified-symbols)
  (error "This may now be redundant given LambdaListHandler_O::namesOfAllLexicalVariables")
  (let (result)
    (mapc #'(lambda (x) (if (eq (car x) 'ext:heap-var)
			    (setq result (cons (cadr x) result))))
	  classified-symbols)
    (let ((rev-res (nreverse result)))
      (make-array (length rev-res) :initial-contents rev-res))))




(defun codegen-fill-let/let*-environment (operator-symbol
					  new-env lambda-list-handler
					  exps parent-env evaluate-env)
  "Evaluate each of the exps in the evaluate-env environment
and put the values into the activation frame for new-env.
env is the parent environment of the (result-af) value frame"
  (multiple-value-bind (reqvars)
      (process-lambda-list-handler lambda-list-handler)
    (let ((number-of-lexical-vars (number-of-lexical-variables lambda-list-handler))
	  (result-af (irc-renv new-env)))
      (dbg-set-current-debug-location-here)
      (irc-make-value-frame result-af number-of-lexical-vars)
      (irc-intrinsic "setParentOfActivationFrame" result-af (irc-renv parent-env))
      (dbg-set-current-debug-location-here)
      (irc-attach-debugging-info-to-value-frame (irc-renv new-env)
						lambda-list-handler
						new-env)
      ;; Save all special variables
      (do* ((cur-req (cdr reqvars) (cdr cur-req))
	    (classified-target (car cur-req) (car cur-req)))
	   ((endp cur-req) nil)
	(compile-save-if-special new-env classified-target))
      ;; Now generate code for let
      (cmp-log "About to generate code for exps: %s\n" exps)
      (do* ((cur-req (cdr reqvars) (cdr cur-req))
	    (classified-target (car cur-req) (car cur-req))
	    (cur-exp exps (cdr cur-exp))
	    (exp (car cur-exp) (car cur-exp)))
	   ((endp cur-req) nil)
	(let* ((target-head (car classified-target))
	       (target-idx (cdr classified-target)))
	  (dbg-set-current-debug-location-here)
	  (with-target-reference-do (target-ref classified-target new-env)
	    (dbg-set-current-source-pos evaluate-env exp)
	    (codegen target-ref exp evaluate-env)))))))





(defun codegen-let/let* (operator-symbol result parts env)
  (with-dbg-lexical-block (env parts)
    (let ((assignments (car parts))
	  (body (cdr parts)))
      (multiple-value-bind (variables expressions)
	  (separate-pair-list assignments)
	(multiple-value-bind (declares code docstring specials )
	    (process-declarations body)
	  (cmp-log "About to create lambda-list-handler\n")
	  (dbg-set-current-debug-location-here)
	  (let* ((lambda-list-handler (make-lambda-list-handler variables declares 'core::function))
		 (new-env (irc-new-unbound-value-environment-of-size
			   env
			   :number-of-arguments (number-of-lexical-variables lambda-list-handler) ;; lambda-list-handler lambda-list-handler
			   :label (symbol-name operator-symbol)))
		 ;; There is a huge problem with setting evaluate-env for 'let
		 ;; in that the let evaluate-environment is not the same environment
		 ;; used by with-try and codegen-fill-let/let*-environment
		 ;; The problem is that the code generated within codegen-fill-let/let*-environment
		 ;; connects to the env dispatcher rather than the new-env dispatcher
		 (evaluate-env (cond
				 ((eq operator-symbol 'let) env) ;;; This is a problem right here
				 ((eq operator-symbol 'let*) new-env)
				 (t (error "let/let* doesn't understand operator symbol[~a]" operator-symbol))))
		 traceid)
	    (with-try new-env
	      (progn
		(irc-branch-to-and-begin-block (irc-basic-block-create
						(bformat nil "%s-start"
							 (symbol-name operator-symbol))))
		(if (eq operator-symbol 'let)
		    (trace-enter-let-scope new-env code)
		    (trace-enter-let*-scope new-env code))
		(codegen-fill-let/let*-environment operator-symbol
						   new-env
						   lambda-list-handler
						   expressions env evaluate-env)
		(irc-intrinsic "trace_setActivationFrameForIHSTop" (irc-renv new-env))
		(irc-single-step-callback new-env)
		(cmp-log "About to evaluate codegen-progn\n")
		(codegen-progn result code new-env))
	      ((cleanup)
	       (if (eq operator-symbol 'let)
		   (trace-exit-let-scope new-env traceid)
		   (trace-exit-let*-scope new-env traceid))
	       (irc-unwind-environment new-env)
	       ))
	    )
	  ))))
  (cmp-log "Done codegen-let/let*\n")
  )

(defun codegen-let (result rest env)
  (codegen-let/let* 'let result rest env))

(defun codegen-let* (result rest env)
  (codegen-let/let* 'let* result rest env))












(defun compile-if-cond (cond env)
  "Generate code for cond that writes into result and then calls isTrue function that returns a boolean"
  (let (test-result)
    (let ((test-temp-store (irc-alloca-tsp env :label "if-cond-tsp")))
      (codegen test-temp-store cond env)
      (setq test-result (llvm-sys:create-icmp-eq *irbuilder* (irc-intrinsic "isTrueTsp" test-temp-store) (jit-constant-i32 1) "ifcond")))
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
      ;; Create blocks for the then and else cases. Insert the 'then' block at
      ;; the end of the function
      (let* ((thenbb (irc-basic-block-create "then" *current-function*))
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
		      (setq result (cons (list index (irc-basic-block-create (bformat nil "tagbody-%s-%s" (car x) index)) x) result))
		      (add-tag tagbody-env (car x) result)
		      (setq index (+ 1 index))))) code)
    (nreverse result)))




(defun codegen-tagbody (result rest env)
  "Extract tags and code from (rest) and create an alist that maps
tag symbols to code and llvm-ir basic-blocks. Store the alist in the symbol-to-block-alist
metadata of the tagbody-env. These can be accessed by (go XXXX) special operators to
jump to blocks within this tagbody."
  (assert-result-isa-llvm-value result)
  (dbg-set-current-debug-location-here)
  (unless (and (car rest) (symbolp (car rest))) (push (gensym) rest)) ;; stick a dummy tag at the head if there isn't one
  (let* ((tagbody-env (irc-new-tagbody-environment env))
	 (enumerated-tag-blocks (tagbody.enumerate-tag-blocks rest tagbody-env)))
    ;; If the GO spec.ops. are in the same function we could use simple cleanup and branches for TAGBODY/GO
    ;; so save the function
    (irc-make-tagbody-frame env (irc-renv tagbody-env))
    (irc-intrinsic "setParentOfActivationFrame" (irc-renv tagbody-env) (irc-renv env))
    (irc-low-level-trace :tagbody)
    (setf-metadata tagbody-env 'tagbody-function *current-function*)
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
      ((cleanup) (codegen-literal result nil env))
      ((typeid-core-dynamic-go exception-ptr)
       (let* ((go-index (irc-intrinsic "tagbodyDynamicGoIndexElseRethrow" (irc-renv tagbody-env) exception-ptr))
	      (default-block (irc-basic-block-create "switch-default"))
	      (sw (irc-switch go-index default-block (length enumerated-tag-blocks))))
	 (mapc #'(lambda (one) (llvm-sys:add-case sw (jit-constant-i32 (car one))
						  (cadr one))) enumerated-tag-blocks)
	 (irc-begin-block default-block)
	 (irc-intrinsic "throwIllegalSwitchValue"
		   go-index (jit-constant-i32 (length enumerated-tag-blocks)))
	 ))
      )
    ))



(defun codegen-go (result rest env)
  (dbg-set-current-debug-location-here)
  (let* ((tag (car rest))
	 (classified-tag (classify-tag env tag)))
    (if (and classified-tag (eq (car classified-tag) 'dynamic-go))
	(let ((depth (cadr classified-tag))
	      (index (cddr classified-tag)))
	  (irc-low-level-trace :go)
	  (irc-intrinsic "throw_DynamicGo" (jit-constant-i32 depth) (jit-constant-i32 index) (irc-renv env)))
	(error "go to unknown classified tag ~a ~a" tag classified-tag))))








(defun codegen-block (result rest env)
  "codegen-block using the try macro"
  (let ((block-symbol (car rest))
	(body (cdr rest)))
    (with-dbg-lexical-block (env body)
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
	   (irc-intrinsic "blockHandleReturnFrom" result exception-ptr))
	  )
	))))





(defun codegen-return-from (result rest env)
  (dbg-set-current-debug-location-here)
  (let* ((temp-mv-result (irc-alloca-tmv env :label "temp-mv-result"))
	 (block-symbol (car rest))
	 (return-form (cadr rest))
	 (recognizes-block-symbol (recognizes-block-symbol env block-symbol)))
    (if recognizes-block-symbol
	(let ((block-depth (calculate-block-depth env block-symbol)))
	  (codegen temp-mv-result return-form env)
	  (irc-low-level-trace)
	  (irc-intrinsic "throwReturnFrom" (jit-constant-i32 block-depth) temp-mv-result))
	(error "Unrecognized block symbol ~a" block-symbol))))



(defun codegen-fill-function-frame (operator-symbol function-env functions parent-env closure-env)
  "Create a closure for each of the function bodies in the flet/labels and put the closures into the activation frame in (result-af). (env) is the parent environment of the (result-af) value frame"
  (let ((result-af (irc-renv function-env)))
    (dbg-set-current-debug-location-here)
    (irc-intrinsic "makeFunctionFrame" result-af (jit-constant-i32 (length functions)) (irc-renv parent-env)) ;; irc-intrinsic (irc-renv env)))
    ;;    )
    (cmp-log "About to generate code for args\n")
    (do* ((cur functions (cdr cur)))
	 ((endp cur) nil)
      (let* ((fn (car cur))
	     (fn-name (car fn))
	     (fn-lambda `(ext::lambda-block ,fn-name ,@(cdr fn)))
	     (fn-classified (classify-function-lookup function-env fn-name))
	     (fn-index (or (cadddr fn-classified) (error "Could not find lexical function ~a" fn-name)))
	     (target (irc-intrinsic "functionFrameReference" result-af (jit-constant-i32 fn-index)
			       (bformat nil "%s-ref-%d" (llvm-sys:get-name result-af) fn-index) )))
	(codegen-closure target fn-lambda closure-env)))))





(defun codegen-flet/labels (operator-symbol result rest env)
  (with-dbg-lexical-block (env rest)
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
	      (codegen-fill-function-frame operator-symbol function-env functions env evaluate-env)
	      (irc-intrinsic "trace_setActivationFrameForIHSTop" (irc-renv function-env))
	      (codegen-progn result code function-env))
	    ((cleanup)
	     (if (eq operator-symbol 'flet)
		 (trace-exit-flet-scope function-env traceid)
		 (trace-exit-labels-scope function-env traceid))
	     (irc-unwind-environment function-env)))
	  )))))

(defun codegen-flet (result rest env)
  (codegen-flet/labels 'flet result rest env))

(defun codegen-labels (result rest env)
  (codegen-flet/labels 'labels result rest env))



(defun codegen-macrolet (result rest env)
  (let* ((macros (car rest))
	 (body (cdr rest))
	 (macro-env (irc-new-macrolet-environment env)))
    (mapc #'(lambda (macro-def &aux (name (car macro-def))
				 (vl (cadr macro-def))
				 (macro-body (cddr macro-def)))
	      (let* ((lambdablock (parse-macro name vl macro-body))
		     (macro-fn (eval (list 'function lambdablock))))
		(set-kind macro-fn :macro)
		(add-macro macro-env name macro-fn)))
	  macros )
    (multiple-value-bind (declares code docstring specials )
	(process-declarations body)
      (augment-environment-with-declares macro-env declares)
      (codegen-progn result code macro-env))))






(defun codegen-symbol-macrolet (result rest env)
  (let* ((macros (car rest))
	 (body (cdr rest))
	 (macro-env (irc-new-symbol-macrolet-environment env)))
    (mapc #'(lambda (macro-def &aux (name (car macro-def))
				 (form (cadr macro-def)))
	      (let ((expander #'(lambda (whole env) (declare (ignore whole env)) form)))
		(add-symbol-macro macro-env name expander)))
	      macros )
    (codegen-locally result body macro-env)))










(defun codegen-eval-when (result rest env)
;;  (break "codegen-eval-when")
  (let ((situations (car rest))
	(body (cdr rest)))
    (if (or (member 'core:eval situations) (member :execute situations))
	(codegen-progn result body env)
	(codegen-literal result nil env))))



(defun codegen-the (result rest env)
  (codegen result (cadr rest) env))

(defun codegen-truly-the (result rest env)
  (codegen result (cadr rest) env))


(defun codegen-locally (result rest env)
  (with-dbg-lexical-block (env rest)
    (multiple-value-bind (declarations code doc-string specials)
	(process-declarations rest nil)
      (let ((new-env (irc-new-unbound-value-environment-of-size
		      env
		      :number-of-arguments (length specials)
		      :label "locally-env")))
	;; TODO: A runtime environment will be created with space for the specials
	;; but they aren't used - get rid of them
	(irc-make-value-frame (irc-renv new-env) 0)
	(irc-intrinsic "setParentOfActivationFrame" (irc-renv new-env) (irc-renv env))
	(dolist (sp specials)
	  (value-environment-define-special-binding new-env sp))
	(codegen-progn result code new-env)
	))))






(defun codegen-unwind-protect (result rest env)
  (with-dbg-lexical-block (env rest)
    (let* ((up-env (irc-new-unwind-protect-environment env))
	   (protected-form (car rest))
	   (unwind-form (cadr rest))
	   (temp-mv-result (irc-alloca-tmv env :label "temp-mv-result"))
	   (saved-values (irc-alloca-tsp env :label "unwind-protect-saved-values"))
	   )
      ;;Codegen the protected-form unwind to the unwind-landing-pad-block
      (with-try up-env
	(progn
	  (irc-branch-to-and-begin-block (irc-basic-block-create "unwind-protect-start"))
	  (codegen temp-mv-result protected-form up-env)
	  (irc-intrinsic "saveValues" saved-values temp-mv-result)
	  )
	((cleanup)
	 (let ((temp-val (irc-alloca-tsp up-env :label "temp")))
	   (codegen temp-val unwind-form up-env))
	 (irc-intrinsic "loadValues" temp-mv-result saved-values)
	 (irc-intrinsic "copyTmvOrSlice" result temp-mv-result)
	 (irc-unwind-environment up-env))
	)
      )))




(defun codegen-throw (result rest env)
  (let ((tag (car rest))
	(result-form (cadr rest)))
    (let ((tag-store (irc-alloca-tsp env :label "tag-store"))
	  (result-mv-form-store (irc-alloca-tmv env :label "result-mv-form-store")))
      (codegen tag-store tag env)
      (codegen result-mv-form-store result-form env)
      (irc-low-level-trace)
      (irc-intrinsic "throwCatchThrow" tag-store result-mv-form-store))))


(defun codegen-catch (result rest env)
  (with-dbg-lexical-block (env rest)
    (let* ((catch-env (irc-new-catch-environment env))
	   (tag (car rest))
	   (body (cdr rest))
	   (tag-store (irc-alloca-tsp catch-env :label "tag-store"))
	   (tag-unwind-store (irc-alloca-tsp catch-env :label "tag-unwind-store"))
	   traceid)
      (codegen tag-store tag catch-env)
      (irc-intrinsic "catchStoreTag" tag-unwind-store tag-store)
      (with-try catch-env
	(progn
	  (setq traceid (trace-enter-catch-scope catch-env rest))
	  (codegen-progn result body catch-env)
	  )
	((cleanup)
	 (trace-exit-catch-scope catch-env traceid)
	 (irc-unwind-environment catch-env))
	((typeid-core-catch-throw exception-ptr)
	 (irc-intrinsic "catchIfTagMatchesStoreResultElseRethrow"
		   result tag-store exception-ptr))
	((all-other-exceptions)
	 (progn
	   (irc-intrinsic "catchUnwind" tag-unwind-store)
	   (irc-rethrow catch-env)
	   ))
	))))


(defun codegen-load-time-value (result rest env)
  (cmp-log "Starting codegen-load-time-value rest: %s\n" rest)
  (let* ((form (car rest))
	 (read-only-p (cadr rest)))
    (if *generate-compile-file-load-time-values*
	(multiple-value-bind (index fn)
	    (compile-ltv-thunk "load-time-value-func" form nil)
	  ;; Invoke the repl function here
	  (with-ltv-function-codegen (result ltv-env)
	    (irc-intrinsic "invokeLlvmFunction" result fn (irc-renv ltv-env)))
	  (irc-intrinsic "getLoadTimeValue" result *load-time-value-holder-global-var* (jit-constant-i32 index)))
	(progn
	  (cmp-log "About to generate load-time-value for COMPILE")
;;	  (break "Handle load-time-value for COMPILE")
	  (let ((ltv (eval form)))
	    (codegen-rtv/all result ltv env))))))



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
    (irc-intrinsic "debugPrintI32" (jit-constant-i32 giveni))))



(defun codegen-special-operator (result head rest env)
  (cmp-log "entered codegen-special-operator head: %s rest: %s\n" head rest)
  (assert-result-isa-llvm-value result)
  (cmp-log "About to set source pos\n")
  (dbg-set-current-source-pos env rest)
  (cmp-log "About to do case on head: %s\n" head)
  (let ((function (gethash head *special-operator-dispatch* 'nil)))
    (if function
	(funcall function result rest env)
	(error "Unknown special operator : ~a" head)))
  )



(defun compile-macroexpand (form env)
  (cmp-log "Entered compile-macroexpand with: %s\n" form)
  (let ((result (macroexpand form env)))
    (cmp-log "Leaving compile-macroexpand with result ===> %s\n" result)
    result))

(defun codegen-application (result form env)
  "A macro function or a regular function"
  ;;  (break "codegen-application")
  (assert-result-isa-llvm-value result)
  (dbg-set-current-source-pos env form)
  (if (macro-function (car form) env)
      (progn
	(multiple-value-bind (expansion expanded-p)
	    (compile-macroexpand form env)
	  (cmp-log "MACROEXPANDed form[%s] expanded to [%s]\n" form expansion )
	  (irc-low-level-trace)
	  (codegen result expansion env)))
      ;; It's a regular function call
      (progn
	(irc-low-level-trace)
	(codegen-call result form env))))



(defun augmented-special-operator-p (x)
  (or (special-operator-p x) (eq x 'cmp:dbg-i32) (eq x 'core:truly-the)))


(defun codegen-atom (result obj env)
  "Generate code to generate the load-time-value of the atom "
  (if (symbolp obj)
      (codegen-symbol-value result obj env)
      (if *generate-compile-file-load-time-values*
	  (cond
	    ((null obj) (codegen-ltv/nil result env))
	    ((integerp obj) (codegen-ltv/integer result obj env))
	    ((stringp obj) (codegen-ltv/string result obj env))
	    ((floatp obj) (codegen-ltv/float result obj env))
            ((complexp obj) (codegen-ltv/complex result obj env))
	    ((characterp obj) (codegen-ltv/character result obj env))
	    ((arrayp obj) (codegen-ltv/array result obj env))
	    ((hash-table-p obj) (codegen-ltv/container result obj env))
	    (t (error "In codegen-atom add support to codegen the atom type ~a - value: ~a" (class-name obj) obj )))
	  ;; Below is how we compile atoms for COMPILE - literal objects are passed into the
	  ;; default module without coalescence.
	  (codegen-rtv result obj env))))


(defun codegen (result form env)
  (declare (optimize (debug 3)))
  (assert-result-isa-llvm-value result)
  (multiple-value-bind (source-file file-pos lineno column)
      (walk-to-find-source-info form)
    (let* ((*current-form* form)
	   (*current-env* env)
	   (*current-filepos* (if file-pos file-pos 0))
	   (*current-lineno* (if lineno lineno 0))
	   (*current-column* (if column column 0)))
      (cmp-log "codegen stack-used[%d bytes]\n" (stack-used))
      (cmp-log "codegen evaluate-depth[%d]  %s\n" (evaluate-depth) form)
      ;;
      ;; If a *code-walker* is defined then invoke the code-walker
      ;; with the current form and environment
      (when *code-walker*
	(setq form (funcall *code-walker* form env)))
      (if (atom form)
	  (codegen-atom result form env)
	  (let ((head (car form))
		(rest (cdr form)))
	    (cmp-log "About to codegen special-operator or application for: %s\n" form)
	    ;;	(trace-linenumber-column (walk-to-find-parse-pos form) env)
	    (if (and head (symbolp head) (augmented-special-operator-p head))
		(progn
		  (cmp-log "About to codegen-special-operator: %s %s\n" head rest)
		  (codegen-special-operator result head rest env))
		(progn
		  (cmp-log "About to codegen-application: %s\n" form)
		  (codegen-application result form env))))))))



;;------------------------------------------------------------
;;
;; Create a repl function
;; It takes no arguments, returns an object and is invoked using apply
;;
;;


(defun compile-thunk (name form env)
  "Compile the form into an llvm function and return that function"
  (dbg-set-current-debug-location-here)
  (or (stringp name) (error "name must be a string"))
  (let ((fn (with-new-function (fn fn-env :function-name name :parent-env env :function-form form)
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
    (cmp-log-dump fn)
    (irc-verify-function fn t)
    fn))



(defun compile-interpreted-function (name func)
  "Compile an interpreted function, return the LLVM function and the environment that it should
be wrapped with to make a closure"
  (let ((code (get-code func))
	(env (closed-environment func))
	(fn-kind (function-kind func))
	(lambda-list-handler (get-lambda-list-handler func)))
    (dbg-set-current-debug-location-here)
    (let ((fn (compile-lambda/lambda-block (symbol-name name) lambda-list-handler nil "" code env)))
      (values fn fn-kind env))))



(defun compile* (name &optional definition env)
  "Returns an LLVM Function and the environment to wrap with the function"
  (cond
    ((functionp definition)
     (error "Handle compile with definition = function"))
    ((consp definition)
     (cmp-log "compile* form: %s\n" definition)
     (let* ((fn (compile-lambda-function definition
					 env
					 :function-name-as-string (if name
								      (symbol-name name)
								      "lambda"))))
       (values fn :function env)))
    ((null definition)
     (let ((func (symbol-function name)))
       (cond
	 ((interpreted-function-p func)
	  (compile-interpreted-function name func)))))
    (t (error "Illegal combination of arguments for compile: ~a ~a"
	      name definition))))




(defun compile-in-env (name &optional definition env)
  "Compile in the given environment"
  (with-compiler-env ()
    (multiple-value-bind (*the-module*
			  *run-time-execution-engine*
			  *the-function-pass-manager*)
	(create-run-time-module-for-compile)
      (declare (special *the-module*
			*run-time-execution-engine*
			*the-function-pass-manager*))
      (define-primitives-in-module *the-module*)
      (let* ((*run-time-value-holder-global-var*
	      (llvm-sys:make-global-variable *the-module*
					     +run-and-load-time-value-holder-global-var-type+
					     nil
					     'llvm-sys:external-linkage nil ;; (llvm-sys:constant-pointer-null-get +run-and-load-time-value-holder-global-var-type+)
					     *run-time-literals-external-name*)))
	(with-compilation-unit ()
            (with-module (:module *the-module* :function-pass-manager *the-function-pass-manager*)
              (with-irbuilder (env (llvm-sys:make-irbuilder *llvm-context*))
                (let* ((truename (if *load-truename*
                                     (namestring *load-truename*)
                                     "compile-in-env"))
                       (*gv-source-path-name* (jit-make-global-string-ptr
                                               truename
                                               "source-path-name"))
                       (*all-funcs-for-one-compile* nil))
                  (multiple-value-bind (fn function-kind wrapped-env warnp failp)
                      (with-dibuilder (*the-module*)
                        (with-dbg-compile-unit (nil truename)
                          (with-dbg-file-descriptor (nil truename)
                            (multiple-value-bind (fn fn-kind wrenv warnp failp)
                                (compile* name definition env)
                              (values fn fn-kind wrenv warnp failp)
                              ))))
                    (cmp-log "------------  Finished building MCJIT Module - about to get-compiled-function  Final module follows...\n")
                    (cmp-log-dump *the-module*)
                    (let* ((compiled-function
                            (progn
                              (cmp-log "About to get-compiled-function with fn %s\n" fn)
                              (llvm-sys:get-compiled-function *run-time-execution-engine* name fn
                                                              (irc-environment-activation-frame wrapped-env)
                                                              function-kind))))
                      (set-associated-funcs compiled-function *all-funcs-for-one-compile*)
                      (when name (setf-symbol-function name compiled-function))
                      (values compiled-function warnp failp)))))))))))



(defun compile (name &optional definition)
  (compile-in-env name definition nil))


(defun test-debug ()
  (print "First")
  (debug "test")
  (print "Last"))


(defun disassemble (desig)
  (multiple-value-bind (func-or-lambda name)
      (cond
	((symbolp desig) (if (fboundp desig)
			     (values (fdefinition desig) desig)
			     (error "No function bound to ~A" desig)))
	((functionp desig) (multiple-value-bind (fn-lambda closurep name)
			       (function-lambda-expression desig)
			     (values desig name)))
	(t (error "Unknown argument ~a passed to disassemble" desig)))
    (setq name (if name name 'lambda))
    (bformat t "Disassembling function: %s\n" (repr func-or-lambda))
    (cond
      ((functionp func-or-lambda)
       (let ((fn func-or-lambda))
	 (cond
	   ((compiled-function-p fn)
	    (llvm-sys:disassemble* fn))
	   ((interpreted-function-p fn)
	    (let ((code (get-code fn))
		  (env (closed-environment fn))
		  (lambda-list-handler (get-lambda-list-handler fn)))
	      (let* ((fn (compile-lambda/lambda-block name
						      lambda-list-handler nil "" code env))
		     (compiled-function (llvm-sys:get-compiled-function *run-time-execution-engine*
									name fn
									(irc-environment-activation-frame env)
									(function-kind fn))))
		(llvm-sys:disassemble* compiled-function))))
	   (t (error "Unknown disassemble target")))))
      ((and (consp desig) (or (eq (car desig) 'lambda) (eq (car desig) 'ext::lambda-block)))
       (let ((funcs (codegen-closure nil desig nil)))
	 (dolist (i funcs)
	   (llvm-sys:dump i))))
      (t (error "Cannot disassemble")))))
