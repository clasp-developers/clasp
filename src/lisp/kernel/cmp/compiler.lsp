;;;
;;;    File: compiler.lsp
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

(defun augment-environment-with-declares (env declares)
  (let (specials)
    (mapc (lambda (decl)
            (when (eq (car decl) 'cl:special)
              (dolist (s (cdr decl))
                (setq specials (cons s specials))))) declares)
    (if specials
        (make-value-environment-for-locally-special-entries (nreverse specials) env)
        env)))

(defparameter *active-compilation-source-database* nil)
(defun do-one-source-database (closure)
  (if (null *active-compilation-source-database*)
      (let ((*active-compilation-source-database* t)
            (core:*source-database* (core:make-source-manager)))
        (do-one-source-database closure))
      (unwind-protect
           (funcall closure)
        (core:source-manager-empty core:*source-database*))))
(defmacro with-one-source-database (&rest body)
  `(do-one-source-database #'(lambda () ,@body)))


(defun compile-arguments (fn-name	; passed for logging only
			  lambda-list-handler ; llh for function
			  function-env
			  closure
			  argument-holder
			  new-env)
  (cmp-log "compile-arguments closure: %s\n" closure)
  ;; This is where we make the value frame for the passed arguments
  ;;
  ;; keywords:  ESCAPE ANALYSIS escape analysis TODO
  ;; Currently all bindings are on the heap
  ;; - we should move some/all of these bindings onto the stack
  ;; There should be information in new-env??? that can tell us what
  ;; variables can go on the stack and what variables can go on the heap
  (irc-make-value-frame (irc-renv new-env) (number-of-lexical-variables lambda-list-handler))
  (irc-intrinsic "setParentOfActivationFrameFromClosure" (irc-renv new-env) closure)
  (cmp-log "lambda-list-handler for fn %s --> %s\n" fn-name lambda-list-handler)
  (cmp-log "Gathered lexical variables for fn %s --> %s\n" fn-name (names-of-lexical-variables lambda-list-handler))
  (compile-lambda-list-code lambda-list-handler
                            function-env
                            argument-holder
                            new-env)
  (dbg-set-current-debug-location-here)
  ;;  (irc-intrinsic "debugInspectActivationFrame" closed-over-renv)
  ;;  (irc-intrinsic "debugInspectActivationFrame" (irc-renv new-env))
  ;;  (irc-intrinsic "gdb")
  ;;
  ;; The setParentOfActivationFrame should be set before we compile the lambda-list-code
  ;; otherwise it won't be able to access the closed over environment
  ;;  (irc-intrinsic "setParentOfActivationFrame" (irc-renv new-env) closed-over-renv)
  (dbg-attach-debugging-info-to-value-frame (irc-renv new-env) lambda-list-handler new-env))

(defparameter *lambda-args-num* 0)

(defvar *all-functions-for-one-compile* nil
  "All functions for one COMPILE are accumulated in this dynamic variable.
COMPILE-FILE just throws this away.   Return (values llvm-function lambda-name lambda-list)")



(defun generate-llvm-function-from-code ( ;; Symbol xxx or (setf xxx) name of the function that is assigned to this code by
					 ;; lambda-block or COMPILE
					 given-name
                                        ; generated from lambda-list
					 lambda-list-handler
                                        ; lambda declares as a list of conses 
					 declares
                                        ; lambda docstring
					 docstring
                                        ; lambda code
					 code
                                        ; environment of the lambda
					 env-around-lambda
                                        ; key argument: T if code should be wrapped in a block with block-name
					 &key wrap-block ; wrap code in a block
                                        ; Name of the block to wrap in
                                         block-name )
  "This is where llvm::Function are generated from code, declares, 
lambda-list-handler, environment.
All code generation comes through here.   Return (llvm:function lambda-name)
Could return more functions that provide lambda-list for swank for example"
  (setq *lambda-args-num* (1+ *lambda-args-num*))
  (let* ((name (core:extract-lambda-name-from-declares declares (or given-name 'cl:lambda)))
	 (fn (with-new-function (fn fn-env result
				    :function-name name
				    :parent-env env-around-lambda
				    :function-form code)
	       (cmp-log "Starting new function name: %s\n" name)
	       (let ((arguments (llvm-sys:get-argument-list fn))
                     traceid)
                 (multiple-value-bind (closure argument-holder)
                     (parse-function-arguments arguments)
                   (let ((new-env (progn
                                    (cmp-log "Creating new-value-environment for arguments\n")
                                    (irc-new-value-environment
                                     fn-env
                                     :lambda-list-handler lambda-list-handler
                                     :label (bformat nil "lambda-args-%s-" *lambda-args-num*)
                                     :fill-runtime-form (lambda (lambda-args-env)
                                                          (compile-arguments name
                                                                             lambda-list-handler
                                                                             fn-env
                                                                             closure
                                                                             argument-holder
                                                                             lambda-args-env
                                                                             ))))))
                     (dbg-set-current-debug-location-here)
                     (with-try new-env
                       (if wrap-block
                           (codegen-block result (list* block-name code) new-env)
                           (codegen-progn result code new-env))
                       ((cleanup)
                        (irc-unwind-environment new-env)))))))))
    (cmp-log "About to dump the function constructed by generate-llvm-function-from-code\n")
    (cmp-log-dump fn)
    (irc-verify-function fn)
    (push fn *all-functions-for-one-compile*)
    ;; Return the llvm Function and the symbol/setf name
    (if (null name) (error "The lambda name is nil"))
    (values fn name (core:lambda-list-handler-lambda-list lambda-list-handler))))

(defun generate-llvm-function-from-interpreted-function (fn)
  "Extract everything necessary to compile an interpreted function and
then compile it and return (values compiled-llvm-function lambda-name)"
  (let ((lambda-list-handler (function-lambda-list-handler fn))
	(declares (function-declares fn))
	(docstring (function-docstring fn))
	(code (function-source-code fn))
	(env (closed-environment fn)))
    (generate-llvm-function-from-code nil lambda-list-handler declares docstring code env)))

(defun generate-lambda-expression-from-interpreted-function (fn)
  (let* ((lambda-list-handler (function-lambda-list-handler fn))
	 (lambda-list (lambda-list-handler-lambda-list lambda-list-handler))
	 (declares (function-declares fn))
	 (docstring (function-docstring fn))
	 (code (function-source-code fn))
	 (env (closed-environment fn)))
    (when docstring (setq docstring (list docstring)))
    #+(or)(progn
	    (bformat t "lambda-list = %s\n" lambda-list)
	    (bformat t "declares    = %s\n" declares)
	    (bformat t "docstring   = %s\n" docstring)
	    (bformat t "code        = %s\n" code)
	    (bformat t "env         = %s\n" env))
    (values `(lambda ,lambda-list ,@docstring (declare ,@declares) ,@code) env)))

(defun function-name-from-lambda (name)
    (cond
      ((symbolp name) (symbol-name name))
      ((consp name) (bformat nil "%s" name))
      (t (error "Add support for function-name-from-lambda with ~a as arg" name))))

(defun compile-lambda-function (lambda-or-lambda-block env )
  "Return the same things that generate-llvm-function-from-code returns"
  (dbg-set-current-debug-location-here)
  (let* (wrap-block block-name lambda-list body lambda-block-name)
    (if (eq (car lambda-or-lambda-block) 'ext::lambda-block)
	(setq wrap-block t
	      block-name (function-block-name (cadr lambda-or-lambda-block))
	      lambda-block-name (cadr lambda-or-lambda-block) ;; bformat nil "%s" (cadr lambda))
	      lambda-list (caddr lambda-or-lambda-block)
	      body (cdddr lambda-or-lambda-block))
	(setq lambda-list (cadr lambda-or-lambda-block)
	      body (cddr lambda-or-lambda-block)))
    (multiple-value-bind (declares code docstring specials )
	(process-declarations body t)
      (cmp-log "About to create lambda-list-handler\n")
      (dbg-set-current-debug-location-here)
      (let ((lambda-list-handler (make-lambda-list-handler lambda-list declares 'core::function)))
	(generate-llvm-function-from-code nil
					  lambda-list-handler
					  declares
					  docstring
					  code
					  env
					  :wrap-block wrap-block
					  :block-name block-name)))))

(defun codegen-closure (result lambda-or-lambda-block env #|&key (func-name-str "lambda")|#)
  "codegen a closure.  If result is defined then put the compiled function into result
- otherwise return the cons of llvm-sys::Function_sp's that were compiled for the lambda"
  (assert-result-isa-llvm-value result)
  (multiple-value-bind (compiled-fn lambda-name lambda-list)
      (compile-lambda-function lambda-or-lambda-block env)
    (if (null lambda-name) (error "The lambda doesn't have a name"))
    (if result
        (let ((funcs (compile-reference-to-literal (if *generate-compile-file-load-time-values*
                                                       nil
                                                       *all-functions-for-one-compile*)))
              (lambda-list (compile-reference-to-literal lambda-list)))
          ;; TODO:   Here walk the source code in lambda-or-lambda-block and
          ;; get the line-number/column for makeCompiledFunction
          (irc-intrinsic "makeCompiledFunction" 
                         result 
                         compiled-fn 
                         *gv-source-file-info-handle* 
                         (cmp:irc-size_t-*current-source-pos-info*-filepos)
                         (cmp:irc-size_t-*current-source-pos-info*-lineno)
                         (cmp:irc-size_t-*current-source-pos-info*-column)
                         (compile-reference-to-literal lambda-name)
                         funcs 
                         (irc-renv env)
                         lambda-list)
          *all-functions-for-one-compile*))))

(defun codegen-global-function-lookup (result sym env)
  (irc-intrinsic "symbolFunctionRead" result (irc-global-symbol sym env)))

(defun codegen-global-setf-function-lookup (result setf-function-name env)
  (let ((setf-symbol (cadr setf-function-name)))
    (irc-intrinsic "setfSymbolFunctionRead" result (irc-global-setf-symbol setf-symbol env))))


(defun codegen-lexical-function-lookup (result depth index env)
  (irc-intrinsic "lexicalFunctionRead" result (jit-constant-i32 depth) (jit-constant-i32 index) (irc-renv env)))

(defun codegen-function-symbol-lookup (result func env)
  "Classify the function and look it up and put it in result"
  (let* ((classified (classify-function-lookup env func)))
    (if (eq (car classified) 'core::global-function)
	(codegen-global-function-lookup result func env)
	(codegen-lexical-function-lookup result (caddr classified) (cadddr classified) env))))

(defun codegen-function-setf-symbol-lookup (result setf-func env)
  "Classify the (setf XXXX) function and put it in the result"
  (let* ((classified (classify-function-lookup env setf-func)))
    (if (eq (car classified) 'core::global-function)
	(codegen-global-setf-function-lookup result setf-func env)
	(codegen-lexical-function-lookup result (caddr classified) (cadddr classified) env))))

(defun codegen-function (result rest env)
  "Return IR code for a function or closure"
  (let ((name-or-lambda (car rest)))
    (assert-result-isa-llvm-value result)
    (dbg-set-current-debug-location-here)
    (cmp-log "About to codegen-function for: %s\n" name-or-lambda)
    (cond
      ((and name-or-lambda (symbolp name-or-lambda))
       (codegen-function-symbol-lookup result name-or-lambda env))
      ((and (consp name-or-lambda)
            (eq (car name-or-lambda) 'setf))
       (codegen-function-setf-symbol-lookup result name-or-lambda env))
      ((and (consp name-or-lambda)
            (or (eq (car name-or-lambda) 'lambda)
                (eq (car name-or-lambda) 'ext::lambda-block)))
       (dbg-set-current-debug-location-here)
       (codegen-closure result name-or-lambda env))
      (t (error "FUNCTION special operator only supports symbol names or lambda expression - you gave: ~a" name-or-lambda)))))

(defun codegen-progn (result forms env)
  "Evaluate forms discarding results but keep last one"
  (cmp-log "About to codegen-progn with forms: %s\n" forms)
  (if forms
      (let ((temp-val (irc-alloca-tsp :label "temp")))
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
	(evaluated-symbols (irc-alloca-tsp :label "symbols"))
	(evaluated-values (irc-alloca-tsp :label "values"))
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
  (with-dbg-lexical-block (rest)
    (let* ((function-form (car rest))
           (forms (cdr rest)))
      (if (= (length forms) 1)
          (let ((temp-mv-result (irc-alloca-tmv env :label "temp-mv-result"))
                (funcDesignator (irc-alloca-tsp :label "funcDesignator"))
                (form (car forms)))
            (codegen funcDesignator function-form env)
            (codegen temp-mv-result form env)
            (irc-intrinsic "saveToMultipleValue0" temp-mv-result)
            (let ((register-ret (irc-intrinsic "cc_call_multipleValueOneFormCall" (irc-extract-value (irc-load funcDesignator) (list 0)))))
              (irc-store-result result register-ret)))
          (codegen result `(core:multiple-value-funcall
                            ,function-form
                            ,@(mapcar (lambda (x) `#'(lambda () (progn ,x))) forms))
                   env)))))
          

(defun codegen-multiple-value-prog1 (result rest env)
  (with-dbg-lexical-block (rest)
    (let ((first-form (car rest))
          (forms (cdr rest)))
      (codegen result `(funcall 'core::multiple-value-prog1-function
                                (lambda () ,first-form)
                                (lambda () (progn ,@forms))) env)))
  #+(or)(with-dbg-lexical-block (rest)
          (let ((temp-mv-result (irc-alloca-tmv env :label "temp-mv-result"))
                (saved-values (irc-alloca-tsp :label "multiple-value-prog1-saved-values"))
                (temp-val (irc-alloca-tsp :label "temp-val")))
            ;; See the interpreter sp_multipleValueCall
            (codegen temp-mv-result (car rest) env)
            (irc-intrinsic "saveValues" saved-values temp-mv-result)
            (do* ((cur (cdr rest) (cdr cur))
                  (form (car cur) (car cur)))
                 ((endp cur) nil)
              (codegen temp-val form env)
              )
            (irc-intrinsic "loadValues" temp-mv-result saved-values)
            (irc-intrinsic "copyTmvOrSlice" result temp-mv-result))))

(defun codegen-special-var-reference (var &optional env)
  (irc-intrinsic "symbolValueReference" (irc-global-symbol var env) (bformat nil "<special-var:%s>" (symbol-name var) )))

(defun codegen-lexical-var-reference (depth-index env)
  (let ((renv (irc-renv env))
	(depth (car depth-index))
	(index (cadr depth-index)))
    (irc-intrinsic "lexicalValueReference" (jit-constant-i32 depth) (jit-constant-i32 index) renv)))

(defun codegen-setq (result setq-pairs env)
  "Carry out setq for a collection of pairs"
  (let ((temp-res (irc-alloca-tsp :label "tsetq")))
    (if setq-pairs
	(do* ((cur setq-pairs (cddr cur))
	      (cur-var (car cur) (car cur))
	      (cur-expr (cadr cur) (cadr cur)))
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
		    (irc-intrinsic "copyTsp" target-ref temp-res)))
		;; symbol was macroexpanded use SETF
		(progn
		  (cmp-log "The symbol[%s] was macroexpanded to result[%s] setting with SETF\n" cur-var expanded)
		  (codegen temp-res `(setf ,expanded ,cur-expr) env))))
	  (unless (cddr cur)
	    (irc-intrinsic "copyTsp" result temp-res)))
	;; There were no pairs, return nil
	(codegen-literal result nil env))))




(defun gather-lexical-variable-names (classified-symbols)
  (error "This may now be redundant given LambdaListHandler_O::namesOfAllLexicalVariables")
  (let (result)
    (mapc #'(lambda (x) (if (eq (car x) 'ext:lexical-var)
			    (setq result (cons (cadr x) result))))
	  classified-symbols)
    (let ((rev-res (nreverse result)))
      (make-array (length rev-res) :initial-contents rev-res))))




(defun codegen-fill-let-environment (new-env lambda-list-handler
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
;;      (dbg-set-activation-frame-for-ihs-top (irc-renv new-env))
      (irc-intrinsic "setParentOfActivationFrame" result-af (irc-renv parent-env))
      (dbg-set-current-debug-location-here)
      (dbg-attach-debugging-info-to-value-frame (irc-renv new-env)
						lambda-list-handler
						new-env)
      ;; Save all special variables
      (do* ((cur-req (cdr reqvars) (cdr cur-req))
	    (classified-target (car cur-req) (car cur-req)))
	   ((endp cur-req) nil)
	(compile-save-if-special new-env classified-target))
      ;;
      ;; Generate allocas for all of the temporary values
      ;;
      (let ((temps (make-array (length reqvars) :adjustable t :fill-pointer 0)))
	(do* ((cur-req (cdr reqvars) (cdr cur-req))
	      (cur-exp exps (cdr cur-exp))
	      (exp (car cur-exp) (car cur-exp))
	      (temp (irc-alloca-tsp :label "let") 
		    (irc-alloca-tsp :label "let")))
	     ((endp cur-req) nil)
	  (vector-push-extend temp temps)
	  (dbg-set-current-source-pos exp)
	  (codegen temp exp evaluate-env))
	;; Now generate code for let
	(cmp-log "About to generate code for exps: %s\n" exps)
	(do* ((cur-req (cdr reqvars) (cdr cur-req))
	      (classified-target (car cur-req) (car cur-req))
	      (tempidx 0 (1+ tempidx)))
	     ((endp cur-req) nil)
	  (let* ((target-head (car classified-target))
		 (target-idx (cdr classified-target)))
	    (with-target-reference-do (target-ref classified-target new-env)
	      (irc-store (irc-load (elt temps tempidx)) target-ref))))))))



(defun codegen-fill-let*-environment (new-env lambda-list-handler
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
;;      (dbg-set-activation-frame-for-ihs-top (irc-renv new-env))
      (irc-intrinsic "setParentOfActivationFrame" result-af (irc-renv parent-env))
      (dbg-set-current-debug-location-here)
      (dbg-attach-debugging-info-to-value-frame (irc-renv new-env)
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
	  (with-target-reference-do (target-ref classified-target new-env)
	    (dbg-set-current-source-pos exp)
	    (codegen target-ref exp evaluate-env)))))))





(defun codegen-let/let* (operator-symbol result parts env)
  (with-dbg-lexical-block (parts)
    (let ((assignments (car parts))
	  (body (cdr parts)))
      (multiple-value-bind (variables expressions)
	  (separate-pair-list assignments)
	(multiple-value-bind (declares code docstring specials )
	    (process-declarations body t)
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
						(bformat nil "%s-start" (symbol-name operator-symbol))))
		(if (eq operator-symbol 'let)
		    (codegen-fill-let-environment new-env lambda-list-handler expressions env evaluate-env)
		    (codegen-fill-let*-environment new-env lambda-list-handler expressions env evaluate-env))
		(cmp-log "About to evaluate codegen-progn\n")
		(codegen-progn result code new-env))
	      ((cleanup)
;;               (dbg-set-activation-frame-for-ihs-top (irc-renv new-env))
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





#||
#+(or)(defun codegen-primop-consp (result rest env)
  (let* ((value (car rest))
         (tag (llvm-sys:create-and *irbuilder* (llvm-sys:create-bit-cast value +uintptr_t+) (jit-constant-uintptr_t +tag-mask+) "tag-only"))
         (consp-tag-match (llvm-sys:create-icmp-eq tag (jit-constant-uintptr_t +cons-tag+))))
    (
         
         (consp-true-block (irc-basic-block-create "consp-true"))
         (consp-false-block
    (
||#




(defun compile-if-cond (cond env)
  "Generate code for cond that writes into result and then calls isTrue function that returns a boolean"
  (let (test-result)
    (let ((test-temp-store (irc-alloca-tsp :label "if-cond-tsp")))
      (codegen test-temp-store cond env)
      (setq test-result (llvm-sys:create-icmp-eq *irbuilder* (irc-intrinsic "isTrue" test-temp-store) (jit-constant-i32 1) "ifcond")))
    test-result))




(defun codegen-if (result rest env)
  "See Kaleidoscope example for if/then/else"
  (let ((icond (car rest))
	(ithen (cadr rest))
	(ielse (caddr rest)))
    (when (cdddr rest)
      (format t "codegen-if (cdddr rest) = ~a ~%" (cdddr rest))
      (compiler-error (cdddr rest) "too many arguments for if"))
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
	(let ((thenv (progn
                       (dbg-set-current-source-pos ithen)
                       (codegen result #|REALLY? put result in result?|# ithen env))))
	  ;;	  (unless thenv (break "thenv is nil") (return-from codegen-if nil))  ;; REALLY? return?????
	  (irc-branch-if-no-terminator-inst mergebb)
	  ;; Codegen of 'Then' can change the current block, update thenbb for the PHI.
	  ;; ---weird! We overwrite thenbb here!
	  (setq thenbb (llvm-sys:get-insert-block *irbuilder*))
	  ;; Emit else block
	  (irc-begin-block elsebb)
	  ;; REALLY? Again- do I use result here?
	  (let ((elsev (progn
                         (dbg-set-current-source-pos ielse)
                         (codegen result ielse env))))
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
    (cmp-log "codegen-tagbody tagbody environment: %s\n" tagbody-env)
    (let ((frame (irc-intrinsic "pushTagbodyFrame" (irc-renv tagbody-env))))
      (with-try tagbody-env
	(progn
	  (let ((go-blocks nil))
	    (mapl #'(lambda (cur)
		      (let* ((tag-begin (car cur))
			     (tag-end (cadr cur))
			     (section-block (cadr tag-begin)))
			(push section-block go-blocks)))
		  enumerated-tag-blocks)
	    (let ((go-vec (make-array (length go-blocks) :initial-contents (nreverse go-blocks))))
	      (setf-metadata tagbody-env 'tagbody-blocks go-vec)))
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
		enumerated-tag-blocks))
        ;;        ((cleanup) (codegen-literal result nil env))
	((cleanup) (irc-unwind-environment tagbody-env))
        ((typeid-core-dynamic-go exception-ptr)
         (let* ((go-index (irc-intrinsic "tagbodyDynamicGoIndexElseRethrow" exception-ptr frame))
                (default-block (irc-basic-block-create "switch-default"))
                (sw (irc-switch go-index default-block (length enumerated-tag-blocks))))
           (mapc #'(lambda (one) (llvm-sys:add-case sw (jit-constant-size_t (car one))
                                                    (cadr one))) enumerated-tag-blocks)
           (irc-begin-block default-block)
           (irc-intrinsic "throwIllegalSwitchValue"
                          go-index (jit-constant-size_t (length enumerated-tag-blocks))))))
      (irc-intrinsic "exceptionStackUnwind" frame)
      (codegen-literal result nil env)
      )))



(defun codegen-go (result rest env)
  (dbg-set-current-debug-location-here)
  (let* ((tag (car rest))
	 (classified-tag (classify-tag env tag)))
    (cond
      ((and classified-tag (eq (car classified-tag) 'dynamic-go))
       (let ((depth (cadr classified-tag))
	     (index (caddr classified-tag)))
	 (irc-low-level-trace :go)
	 (irc-intrinsic "throwDynamicGo" (jit-constant-size_t depth) (jit-constant-size_t index) (irc-renv env))))
      ((and classified-tag (eq (car classified-tag) 'local-go))
       (let ((depth (cadr classified-tag))
	     (index (caddr classified-tag))
	     (tagbody-env (cadddr classified-tag)))
	 (cmp-log "Target tagbody environment: %s  tag: %s\n" tagbody-env tag)
	 (let* ((go-vec (lookup-metadata tagbody-env 'tagbody-blocks))
		(go-block (elt go-vec index)))
	   (irc-unwind-into-environment env tagbody-env)
	   (irc-br go-block)
	   (irc-begin-block (irc-basic-block-create "after-go"))
	   )
	 ))
      (error "go to unknown classified tag ~a ~a" tag classified-tag))))








(defun codegen-block (result rest env)
  "codegen-block using the try macro"
  (let* ((block-symbol (car rest))
         (body (cdr rest)))
    (or (symbolp block-symbol) (error "The block name ~a is not a symbol" block-symbol))
    (with-dbg-lexical-block (body)
      (let* ((block-env (irc-new-block-environment env :name block-symbol))
             traceid)
	(let ((block-start (irc-basic-block-create
			    (bformat nil "block-%s-start" (symbol-name block-symbol))))
	      (local-return-block (irc-basic-block-create (bformat nil "local-return-%s-block" (symbol-name block-symbol))))
	      (after-return-block (irc-basic-block-create (bformat nil "after-return-%s-block" (symbol-name block-symbol))))
	      )
	  (setf-metadata block-env :local-return-block local-return-block)
	  (irc-br block-start)
	  (irc-begin-block block-start)
          (let* ((frame (irc-intrinsic "pushBlockFrame" (irc-global-symbol block-symbol block-env))))
            (with-try block-env
	      (codegen-progn result body block-env)
              ((cleanup)
               (irc-unwind-environment block-env))
              ((typeid-core-return-from exception-ptr)
               (irc-intrinsic "blockHandleReturnFrom" result exception-ptr frame)))
	    (irc-br after-return-block)
	    (irc-begin-block local-return-block)
	    (irc-intrinsic "restoreFromMultipleValue0" result)
	    (irc-br after-return-block)
	    (irc-begin-block after-return-block)
            (irc-intrinsic "exceptionStackUnwind" frame)
	    ))))))





(defun codegen-return-from (result rest env)
  (dbg-set-current-debug-location-here)
  (let* ((temp-mv-result (irc-alloca-tmv env :label "temp-mv-result"))
	 (block-symbol (car rest))
	 (return-form (cadr rest)))
    (multiple-value-bind (recognizes-block-symbol inter-function block-env)
	(classify-return-from-symbol env block-symbol)
      (if recognizes-block-symbol
	  (if inter-function
	      (progn
		(codegen temp-mv-result return-form env)
		(irc-intrinsic "saveToMultipleValue0" temp-mv-result)
		(irc-low-level-trace)
		(irc-intrinsic "throwReturnFrom" (irc-global-symbol block-symbol env)))
	      (let* ((local-return-block (lookup-metadata block-env :local-return-block))
		     (saved-values (irc-alloca-tsp :label "return-from-unwind-saved-values")))
		(codegen temp-mv-result return-form env)
		(irc-intrinsic "saveValues" saved-values temp-mv-result) ;; moved saveValues here
		(irc-unwind-into-environment env block-env)
		(irc-intrinsic "loadValues" temp-mv-result saved-values)
		(irc-intrinsic "saveToMultipleValue0" temp-mv-result)
		(irc-br local-return-block)
		(irc-begin-block (irc-basic-block-create "after-return-from"))
		))
	  (error "Unrecognized block symbol ~a" block-symbol)))))


(defun generate-lambda-block (name lambda-list raw-body)
  "Generate a (lambda ... (block name ...)) after extracting declares and docstring from raw-body"
  (multiple-value-bind (decl body doc)
      (process-declarations raw-body t)
    (when decl (setq decl (list (cons 'declare decl))))
    (when doc (setq doc (list doc)))
    `(lambda ,lambda-list 
       ,@doc 
       (declare (core:lambda-name ,name)) 
       ,@decl (block ,(si::function-block-name name) 
		,@body))))


(defun codegen-fill-function-frame (operator-symbol function-env functions parent-env closure-env)
  "Create a closure for each of the function bodies in the flet/labels and put the closures into the activation frame in (result-af). (env) is the parent environment of the (result-af) value frame"
  (let ((result-af (irc-renv function-env)))
    (dbg-set-current-debug-location-here)
    (irc-intrinsic "makeFunctionFrame" result-af (jit-constant-i32 (length functions)) (irc-renv parent-env))
    ;;    )
    (cmp-log "About to generate code for args\n")
    (do* ((cur functions (cdr cur)))
	 ((endp cur) nil)
      (let* ((fn (car cur))
	     (fn-name (car fn))
	     #+(or)(fn-lambda `(ext::lambda-block ,fn-name ,@(cdr fn)))
	     (fn-lambda-list (cadr fn))
	     (fn-raw-body (cddr fn))
	     (fn-lambda (generate-lambda-block fn-name fn-lambda-list fn-raw-body))
	     (fn-classified (classify-function-lookup function-env fn-name))
	     (fn-index (or (cadddr fn-classified) (error "Could not find lexical function ~a" fn-name)))
	     (target (irc-intrinsic "functionFrameReference" result-af (jit-constant-i32 fn-index)
			       (bformat nil "%s-ref-%d" (llvm-sys:get-name result-af) fn-index) )))
	(codegen-closure target fn-lambda closure-env)))))





(defun codegen-flet/labels (operator-symbol result rest env)
  (with-dbg-lexical-block (rest)
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
;;              (dbg-set-activation-frame-for-ihs-top (irc-renv function-env))
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
	      (let* ((lambdablock (core:parse-macro name vl macro-body))
		     (macro-fn (eval (list 'function lambdablock))))
		(set-kind macro-fn :macro)
		(add-macro macro-env name macro-fn)))
	  macros )
    (multiple-value-bind (declares code docstring specials )
	(process-declarations body t)
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
    (if (or (member 'cl:eval situations) (member :execute situations))
	(codegen-progn result body env)
	(codegen-literal result nil env))))



(defun codegen-the (result rest env)
  (codegen result (cadr rest) env))

(defun codegen-truly-the (result rest env)
  (codegen result (cadr rest) env))


(defun codegen-locally (result rest env)
  (with-dbg-lexical-block (rest)
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

#+(or)(defun codegen-unwind-protect (result rest env)
  (with-dbg-lexical-block (rest)
    (let* ((protected-form (car rest))
	   (unwind-form `(progn ,@(cdr rest)))
	   (up-env (irc-make-unwind-protect-environment unwind-form env))
	   (temp-mv-result (irc-alloca-tmv env :label "temp-mv-result"))
	   (saved-values (irc-alloca-tsp :label "unwind-protect-saved-values"))
	   )
      ;;Codegen the protected-form unwind to the unwind-landing-pad-block
      (with-try up-env
	(progn
	  (irc-branch-to-and-begin-block (irc-basic-block-create "unwind-protect-start"))
	  (codegen temp-mv-result protected-form up-env)
          (irc-intrinsic "saveToMultipleValue0" temp-mv-result)
          #|(irc-intrinsic "saveValues" saved-values temp-mv-result)|# ;; saveValues was here
	  )
	((cleanup)
         (irc-intrinsic "saveValues" saved-values temp-mv-result) ;; moved saveValues here
	 (irc-unwind-environment up-env)
	 (irc-intrinsic "loadValues" temp-mv-result saved-values)
	 (irc-intrinsic "copyTmvOrSlice" result temp-mv-result)
	 ))
      )))




#+(or)
(defun codegen-catch (result rest env)
  (with-dbg-lexical-block (rest)
    (let* ((catch-env (irc-new-catch-environment env))
	   (tag (car rest))
	   (body (cdr rest))
	   (tag-store (irc-alloca-tsp :label "tag-store"))
	   (catch-frame-index (irc-alloca-i32-no-init catch-env :label "catch-frame-index"))
	   traceid)
      (codegen tag-store tag catch-env)
      (let ((frame (irc-intrinsic "pushCatchFrame" tag-store)))
        (with-try catch-env
          (progn
            (setq traceid (trace-enter-catch-scope catch-env rest))
            (codegen-progn result body catch-env)
            )
          ((cleanup)
           (trace-exit-catch-scope catch-env traceid)
           (irc-unwind-environment catch-env))
          ((typeid-core-catch-throw exception-ptr)
           (irc-intrinsic "ifCatchFrameMatchesStoreResultElseRethrow"
                          result frame exception-ptr))
          ((all-other-exceptions)
           (progn
             (irc-rethrow catch-env)
             ))
          )
        (irc-intrinsic "exceptionStackUnwind" frame)
        ))))


#+(or)
(defun codegen-throw (result rest env)
  (let ((tag (car rest))
	(result-form (cadr rest)))
    (let ((tag-store (irc-alloca-tsp :label "tag-store"))
	  (result-mv-form-store (irc-alloca-tmv env :label "result-mv-form-store")))
      (codegen tag-store tag env)
      (codegen result-mv-form-store result-form env)
      (irc-intrinsic "saveToMultipleValue0" result-mv-form-store)
      (irc-low-level-trace)
      (irc-intrinsic "throwCatchThrow" tag-store))))






(defun codegen-load-time-value (result rest env)
  (cmp-log "Starting codegen-load-time-value rest: %s\n" rest)
  (let* ((form (car rest))
	 (read-only-p (cadr rest)))
    (if *generate-compile-file-load-time-values*
	(multiple-value-bind (index fn)
	    (compile-ltv-thunk form)
	  ;; Invoke the repl function here
          (with-ltv-function-codegen (ltv-result ltv-env)
            (irc-intrinsic "invokeTopLevelFunction" 
                           ltv-result 
                           fn
                           (jit-constant-unique-string-ptr "load-time-value")
                           *gv-source-file-info-handle*
                           (cmp:irc-size_t-*current-source-pos-info*-filepos)
                           (cmp:irc-size_t-*current-source-pos-info*-lineno)
                           (cmp:irc-size_t-*current-source-pos-info*-column)
                           *load-time-value-holder-global-var*))
	  (irc-intrinsic "getLoadTimeValue" result *load-time-value-holder-global-var* (jit-constant-i32 index)))
	(progn
	  (cmp-log "About to generate load-time-value for COMPILE")
          ;;	  (break "Handle load-time-value for COMPILE")
	  (let ((ltv (eval form)))
	    (codegen-rtv result ltv env))))))



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

(defun codegen-debug-message (result rest env)
  (let ((message (jit-constant-unique-string-ptr (car rest))))
    (irc-intrinsic "debugMessage" message)))


(defun codegen-llvm-inline (result result-env-body compiler-env)
  (destructuring-bind ((result-name env-name) &body body)
      result-env-body
    (eval `(let ((,result-name ,result)
                 (,env-name ,compiler-env))
             ,@body))))


(defun codegen-special-operator (result head rest env)
  (cmp-log "entered codegen-special-operator head: %s rest: %s\n" head rest)
  (assert-result-isa-llvm-value result)
  (cmp-log "About to set source pos\n")
  (dbg-set-current-source-pos rest)
  (cmp-log "About to do case on head: %s\n" head)
  (let* ((functions (gethash head *special-operator-dispatch* 'nil))
         (function (cadr functions)))
    (if function
	(funcall function result rest env)
	(error "Unknown special operator : ~a" head)))
  )


(defun codegen-multiple-value-foreign-call (result form evaluate-env)
  "Evaluate each of the arguments into an alloca and invoke the function"
  ;; setup the ActivationFrame for passing arguments to this function in the setup arena
  (assert-result-isa-llvm-value result)
  (let* ((intrinsic-name (car form))
         (nargs (length (cdr form)))
         args
         (temp-result (irc-alloca-tsp)))
    (dbg-set-invocation-history-stack-top-source-pos form)
    ;; evaluate the arguments into the array
    ;;  used to be done by --->    (codegen-evaluate-arguments (cdr form) evaluate-env)
    (do* ((cur-exp (cdr form) (cdr cur-exp))
          (exp (car cur-exp) (car cur-exp))
          (i 0 (+ 1 i)))
         ((endp cur-exp) nil)
      (codegen temp-result exp evaluate-env)
      (push (irc-smart-ptr-extract (irc-load temp-result)) args))
    (let* ((func (or (llvm-sys:get-function cmp:*the-module* intrinsic-name)
                     (let ((arg-types (make-list (length args) :initial-element cmp:+t*+))
                           (varargs nil))
                       (llvm-sys:function-create
                        (llvm-sys:function-type-get cmp:+return_type+ arg-types varargs)
                        'llvm-sys::External-linkage
                        intrinsic-name
                        *the-module*))))
           (result-in-registers
            (llvm-sys:create-call-array-ref cmp:*irbuilder* func (nreverse args) "intrinsic")))
      (irc-store-result result result-in-registers)))
  (irc-low-level-trace :flow))

(defun codegen-foreign-call (result form evaluate-env)
  "Evaluate each of the arguments into an alloca and invoke the function"
  ;; setup the ActivationFrame for passing arguments to this function in the setup arena
  (assert-result-isa-llvm-value result)
  (let* ((foreign-name (car form))
         (nargs (length (cdr form)))
         args
         (temp-result (irc-alloca-tsp)))
    (dbg-set-invocation-history-stack-top-source-pos form)
    ;; evaluate the arguments into the array
    ;;  used to be done by --->    (codegen-evaluate-arguments (cdr form) evaluate-env)
    (do* ((cur-exp (cdr form) (cdr cur-exp))
          (exp (car cur-exp) (car cur-exp))
          (i 0 (+ 1 i)))
         ((endp cur-exp) nil)
      (codegen temp-result exp evaluate-env)
      (push (irc-smart-ptr-extract (irc-load temp-result)) args))
    (let* ((func (or (llvm-sys:get-function cmp:*the-module* foreign-name)
                     (let ((arg-types (make-list (length args) :initial-element cmp:+t*+)))
                       (llvm-sys:function-create
                        (llvm-sys:function-type-get cmp:+t*+ arg-types nil)
                        'llvm-sys::External-linkage
                        foreign-name
                        *the-module*))))
           (result-in-t*
            (llvm-sys:create-call-array-ref cmp:*irbuilder* func (nreverse args) "foreign-function")))
      (irc-store-result-t* result result-in-t*))))

(defun codegen-foreign-call-pointer (result form evaluate-env)
  "Evaluate each of the arguments into an alloca and invoke the function pointer"
  ;; setup the ActivationFrame for passing arguments to this function in the setup arena
  (assert-result-isa-llvm-value result)
  (let* ((nargs (length (cdr form)))
         args
         (temp-result (irc-alloca-tsp)))
    (dbg-set-invocation-history-stack-top-source-pos form)
    ;; evaluate the arguments into the array
    ;;  used to be done by --->    (codegen-evaluate-arguments (cdr form) evaluate-env)
    (do* ((cur-exp (cdr form) (cdr cur-exp))
          (exp (car cur-exp) (car cur-exp))
          (i 0 (+ 1 i)))
         ((endp cur-exp) nil)
      (codegen temp-result exp evaluate-env)
      (push (irc-smart-ptr-extract (irc-load temp-result)) args))
    (codegen temp-result (car form) evaluate-env)
    (let* ((arg-types (make-list (length args) :initial-element cmp:+t*+))
           (varargs nil)
           (function-type (llvm-sys:function-type-get cmp:+t*+ arg-types varargs))
           (function-pointer-type (llvm-sys:type-get-pointer-to function-type))
           (pointer-t* (irc-smart-ptr-extract (irc-load temp-result)))
           (function-pointer (llvm-sys:create-bit-cast cmp:*irbuilder* (irc-intrinsic "cc_getPointer" pointer-t*) function-pointer-type "cast-function-pointer"))
           (result-in-t*
            (llvm-sys:create-call-function-pointer cmp:*irbuilder* function-type function-pointer (nreverse args) "fn-ptr-call-result")))
      (irc-store-result-t* result result-in-t*))
    (irc-low-level-trace :flow)))

(defun codegen-application (result form env)
  "A compiler macro function, macro function or a regular function"
  (assert-result-isa-llvm-value result)
  (dbg-set-current-source-pos form)
  (cond
    ;; A compiler macro
    ((and (symbolp (car form))
          (not (core:lexical-function (car form) env))
          (not (core:lexical-macro-function (car form) env))
          (not (core:declared-global-notinline-p (car form)))
          (let ((expansion (core:compiler-macroexpand form env)))
            (if (eq expansion form)
                nil
                (progn
                  (codegen result expansion env)
                  t)))))
    ;; A regular macro
    ((and (symbolp (car form))
          (not (core:lexical-function (car form) env))
          (macro-function (car form) env))
     (multiple-value-bind (expansion expanded-p)
         (macroexpand form env)
       (cmp-log "MACROEXPANDed form[%s] expanded to [%s]\n" form expansion )
       (irc-low-level-trace)
       (codegen result expansion env)))
     ;; It's a regular function call
    (t (codegen-call result form env))))




;;
;; Why does this duplicate so much functionality from codegen-literal
(defun codegen-atom (result obj env)
  "Generate code to generate the load-time-value of the atom "
  (if *generate-compile-file-load-time-values*
      (cond
        ((null obj) (codegen-ltv/nil result))
        ((integerp obj) (codegen-ltv/integer result obj))
        ((stringp obj) (codegen-ltv/string result obj))
        ((pathnamep obj) (codegen-ltv/pathname result obj))
        ((packagep obj) (codegen-ltv/package result obj))
        ((core:built-in-class-p obj) (codegen-ltv/built-in-class result obj env))
        ((floatp obj) (codegen-ltv/float result obj))
        ((core:ratio-p obj) (codegen-ltv/container result obj env))
        ((complexp obj) (codegen-ltv/container result obj env))
        ;; symbol would be here
        ((characterp obj) (codegen-ltv/character result obj))
        ((arrayp obj) (codegen-ltv/array result obj env))
        ;; cons would be here
        ((hash-table-p obj) (codegen-ltv/container result obj env))
        (t (error "In codegen-atom add support to codegen the atom type ~a - value: ~a" (class-name (class-of obj)) obj )))
      ;; Below is how we compile atoms for COMPILE - literal objects are passed into the
      ;; default module without coalescence.
      (codegen-rtv result obj env)))

;;; Return true if the symbol should be treated as a special operator
;;; Special operators that are handled as macros are exempt
(defun treat-as-special-operator-p (sym)
  #+clc(clc-env:treat-as-special-operator-p sym)
  #-clc(cond
         ((eq sym 'cl:unwind-protect) nil)  ;; handled with macro
         ((eq sym 'cl:catch) nil)           ;; handled with macro
         ((eq sym 'cl:throw) nil)           ;; handled with macro
         ((eq sym 'core:debug-message) t)   ;; special operator
         ((eq sym 'core:multiple-value-foreign-call) t) ;; Call intrinsic functions
         ((eq sym 'core:foreign-call-pointer) t) ;; Call function pointers
         ((eq sym 'core:foreign-call) t) ;; Call foreign function
         (t (special-operator-p sym))))
(export 'treat-as-special-operator-p)

(defun codegen (result form env)
  (declare (optimize (debug 3)))
  (assert-result-isa-llvm-value result)
  (multiple-value-bind (source-directory source-filename lineno column)
      (dbg-set-current-source-pos form)
    (let* ((*current-form* form)
           (*current-env* env))
      (cmp-log "codegen stack-used[%d bytes]\n" (stack-used))
      (cmp-log "codegen evaluate-depth[%d]  %s\n" (evaluate-depth) form)
      ;;
      ;; If a *code-walker* is defined then invoke the code-walker
      ;; with the current form and environment
      (when *code-walker*
        (setq form (funcall *code-walker* form env)))
      (if (atom form)
          (if (symbolp form)
              (codegen-symbol-value result form env)
              (codegen-literal result form env))
          (let ((head (car form))
                (rest (cdr form)))
            (cmp-log "About to codegen special-operator or application for: %s\n" form)
            ;;  (trace-linenumber-column (walk-to-find-parse-pos form) env)
            (cond
              ((treat-as-special-operator-p head)
               (codegen-special-operator result head rest env))
              ((and head (consp head) (eq (car head) 'cl:lambda))
               (codegen result `(funcall ,head ,@rest) env))
              ((and head (symbolp head))
               (codegen-application result form env))
              (t
               (error "Handle codegen of cons: ~a" form))))))))

;;------------------------------------------------------------
;;
;; Create a repl function
;; It takes no arguments, returns an object and is invoked using apply
;;
;;


(defun compile-thunk (name form env)
  "Compile the form into an llvm function and return that function"
  (dbg-set-current-debug-location-here)
  (let ((fn (with-new-function (fn
                                fn-env
                                result
                                :function-name name
                                :parent-env env
                                :function-form form)
	      (let* ((given-name (llvm-sys:get-name fn)))
		;; Map the function argument names
		(cmp-log "Creating repl function with name: %s\n" given-name)
                ;;	(break "codegen repl form")
                (dbg-set-current-debug-location-here)
                (codegen result form fn-env)
                (dbg-set-current-debug-location-here)
                ))))
    (cmp-log "Dumping the repl function\n")
    (cmp-log-dump fn)
    (irc-verify-function fn t)
    fn))


#||
(defun compile-interpreted-function (name func)
  "Compile an interpreted function, return the LLVM function and the environment that it should
be wrapped with to make a closure"
  (let ((code (get-code func))
	(env (closed-environment func))
	(fn-kind (function-kind func))
	(lambda-list-handler (get-lambda-list-handler func)))
    (dbg-set-current-debug-location-here)
    (let ((fn (generate-llvm-function-from-code (symbol-name name) lambda-list-handler nil "" code env)))
      (values fn fn-kind env))))
||#

(defvar *optimizations-on* t)
(defun do-optimization (module)
  (when (and module *optimizations-on*)
    (let* ((pass-manager-builder (llvm-sys:make-pass-manager-builder))
           (mpm (llvm-sys:make-pass-manager))
           (fpm (llvm-sys:make-function-pass-manager module)))
      (llvm-sys:pass-manager-builder-setf-opt-level pass-manager-builder 3)
      (llvm-sys:pass-manager-builder-setf-size-level pass-manager-builder 1)
      (llvm-sys:pass-manager-builder-setf-inliner pass-manager-builder (llvm-sys:create-always-inliner-legacy-pass))
      (llvm-sys:populate-function-pass-manager pass-manager-builder fpm)
;;    (llvm-sys:populate-module-pass-manager pass-manager-builder mpm)
      (llvm-sys:populate-ltopass-manager pass-manager-builder mpm)
      (llvm-sys:do-initialization fpm)
      (let ((funcs (llvm-sys:module-get-function-list module)))
        (dolist (func funcs)
          (llvm-sys:function-pass-manager-run fpm func)))
      (llvm-sys:do-finalization fpm)
      (llvm-sys:pass-manager-run mpm module))))

(defmacro with-module (( &key module
                              (optimize t)
                              source-namestring
                              source-file-info-handle
                              source-debug-namestring
                              (source-debug-offset 0)
                              (source-debug-use-lineno t)) &rest body)
  `(let* ((*the-module* ,module)
 	  #+(or)(*generate-load-time-values* t)
	  (*gv-source-namestring* (jit-make-global-string-ptr ,source-namestring "source-namestring"))
	  (*gv-source-debug-namestring* (jit-make-global-string-ptr (if ,source-debug-namestring
									,source-debug-namestring
									,source-namestring) "source-debug-namestring"))
	  (*source-debug-offset* ,source-debug-offset)
	  (*source-debug-use-lineno* ,source-debug-use-lineno)
	  (*gv-source-file-info-handle* (make-gv-source-file-info-handle ,module ,source-file-info-handle)))
     (or *the-module* (error "with-module *the-module* is NIL"))
     (prog1
         (with-irbuilder ((llvm-sys:make-irbuilder *llvm-context*))
           ,@body)
       (when ,optimize (do-optimization ,module)))))




(defun clasp-compile* (bind-to-name &optional definition env pathname)
  "Compile the definition"
  (cmp-log "--- Entered clasp-compile*")
  (multiple-value-bind (fn function-kind wrapped-env lambda-name warnp failp)
      (with-debug-info-generator (:module *the-module* 
                                          :pathname pathname)
        (multiple-value-bind (llvm-function-from-lambda lambda-name)
            (compile-lambda-function definition env)
          (or llvm-function-from-lambda (error "There was no function returned by compile-lambda-function inner: ~a" llvm-function-from-lambda))
          (or lambda-name (error "Inner lambda-name is nil - this shouldn't happen"))
          (values llvm-function-from-lambda :function env lambda-name)))
    (or lambda-name (error "lambda-name is nil - this shouldn't happen"))
    (cmp-log "------------  Finished building MCJIT Module - about to finalize-engine  Final module follows...\n")
    (or fn (error "There was no function returned by compile-lambda-function outer: ~a" fn))
    (cmp-log "fn --> %s\n" fn)
    (cmp-log-dump *the-module*)
    (when *dump-module-on-completion*
      (llvm-sys:dump *the-module*)
      (core::fflush))
    (cmp-log "About to test and maybe set up the *run-time-execution-engine*\n")
    (if (not *run-time-execution-engine*)
	;; SETUP THE *run-time-execution-engine* here for the first time
	;; using the current module in *the-module*
	;; At this point the *the-module* will become invalid because
	;; the execution-engine will take ownership of it
	(setq *run-time-execution-engine* (create-run-time-execution-engine *the-module*))
	(llvm-sys:add-module *run-time-execution-engine* *the-module*))
    ;; At this point the Module in *the-module* is invalid because the
    ;; execution-engine owns it
    (cmp-log "The execution-engine now owns the module\n")
    (setq *the-module* nil)
    (cmp-log "About to finalize-engine with fn %s\n" fn)
    (let* ((fn-name (llvm-sys:get-name fn)) ;; this is the name of the function - a string
           (cspi (ext:current-source-location))
	   (compiled-function
	    (llvm-sys:finalize-engine-and-register-with-gc-and-get-compiled-function
	     *run-time-execution-engine*
	     lambda-name
	     fn ;; This may not be valid anymore
	     (irc-environment-activation-frame wrapped-env)
	     core:*current-source-file-info*
	     (core:source-pos-info-filepos cspi)
	     (core:source-pos-info-lineno cspi)
	     nil ; lambda-list, NIL for now - but this should be extracted from definition
	     )))
      (values compiled-function warnp failp))))

(defvar *compile-counter* 0)
(defvar *compile-duration-ns* 0)

(defun compile-with-hook (compile-hook name &optional definition env pathname)
  "Dispatch to clasp compiler or cleavir-clasp compiler if available.
We could do more fancy things here - like if cleavir-clasp fails, use the clasp compiler as backup."
  (let ((compile-start-ns (core:clock-gettime-nanoseconds)))
    (setf *compile-counter* (+ 1 *compile-counter*))
    (unwind-protect
         (progn
           (if compile-hook
               (progn
                 (funcall compile-hook name definition env pathname))
               (clasp-compile* name definition env pathname)))
      (let* ((compile-end-ns (core:clock-gettime-nanoseconds))
             (this-compile-ns (- compile-end-ns compile-start-ns)))
        (setf *compile-duration-ns* (+ *compile-duration-ns* this-compile-ns))
        (if core:*notify-on-compile*
            (progn
              (bformat t "Compiled name: %s\n  form: %s\n  Took: %8.3f seconds\n" 
                       name definition (float (/ this-compile-ns 1000000000.0)))
              (core:ihs-backtrace)))))))


(defun compile-in-env (bind-to-name &optional definition env compile-hook &aux conditions)
  "Compile in the given environment"
  (with-compiler-env (conditions)
    (let ((*the-module* (create-run-time-module-for-compile)))
      (define-primitives-in-module *the-module*)
      (let* ((*run-time-values-table-global-var*
	      (llvm-sys:make-global-variable *the-module*
                                             +run-and-load-time-value-holder-global-var-type+
					     nil
					     'llvm-sys:external-linkage
					     nil
					     *run-time-values-table-name*))
	     (pathname (if *load-pathname*
			   (namestring *load-pathname*)
			   "repl-code"))
	     (handle (multiple-value-bind (the-source-file-info the-handle)
			 (core:source-file-info pathname)
		       the-handle)))
	(with-module (:module *the-module*
                              :source-namestring (namestring pathname)
                              :source-file-info-handle handle)
          (let ((*all-functions-for-one-compile* nil))
            (multiple-value-bind (compiled-function warnp failp)
                (compile-with-hook compile-hook bind-to-name definition env pathname)
              (when bind-to-name
                (let ((lambda-list (cadr definition)))
                  #+(or)(format t "Setting function name: ~a definition: ~a~%" bind-to-name definition)
                  #+(or)(format t "*all-functions-for-one-compile* -> ~a~%" cmp:*all-functions-for-one-compile*)
                  (core:fset bind-to-name compiled-function nil t lambda-list)
                  #+(or)(setf-symbol-function bind-to-name compiled-function)
                  ))
              (values compiled-function warnp failp))))))))


(defun compile* (compile-hook name &optional definition)
  (cond
    ((functionp definition)
     (error "Handle compile with definition = function"))
    ((consp definition)
     (cmp-log "compile form: %s\n" definition)
     (compile-in-env name definition nil compile-hook))
    ((null definition)
     (let ((func (symbol-function name)))
       (cond
	 ((interpreted-function-p func)
	  (dbg-set-current-debug-location-here)
	  ;; Recover the lambda-expression from the interpreted-function
	  (multiple-value-bind (lambda-expression wrapped-env)
	      (generate-lambda-expression-from-interpreted-function func)
	    (cmp-log "About to compile  name: %s  lambda-expression: %s wrapped-env: %s\n" name lambda-expression wrapped-env)
	    (compile-in-env name lambda-expression wrapped-env compile-hook)))
         (t (warn "We have lost the original function definition for ~s. Compilation failed." func)
            (values func t nil)))))
    (t (error "Illegal combination of arguments for compile: ~a ~a"
	      name definition))))

(defun compile (&rest args)
  ;; Use the *cleavir-compile-hook* to determine which compiler to use
  ;; if nil == bclasp
  ;; if #'clasp-cleavir:cleavir-compile-t1expr == cclasp
  (apply #'compile* *cleavir-compile-hook* args))


(defun bclasp-compile (name form)
  (let ((*cleavir-compile-hook* nil)
        (core:*use-cleavir-compiler* nil))
    (compile name form)))


(defun test-debug ()
  (print "First")
  (debug "test")
  (print "Last"))


(defun disassemble (desig)
  (multiple-value-bind (func-or-lambda name)
      (cond
        ((null desig) (error "No function provided"))
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
	    (format t "This is a interpreted function - compile it first~%"))
	   (t (error "Unknown target for disassemble: ~a" fn)))))
      ((and (consp desig) (or (eq (car desig) 'lambda) (eq (car desig) 'ext::lambda-block)))
       (let ((funcs (codegen-closure nil desig nil)))
	 (dolist (i funcs)
	   (llvm-sys:dump i))))
      (t (error "Cannot disassemble")))))


(defun compiler-stats ()
  (bformat t "Accumulated finalization time %s\n" llvm-sys:*accumulated-llvm-finalization-time*)
  (bformat t "Most recent finalization time %s\n" llvm-sys:*most-recent-llvm-finalization-time*)
  (bformat t "Number of compilations %s\n" llvm-sys:*number-of-llvm-finalizations*)
)
(export 'compiler-stats)
