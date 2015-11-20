;;;
;;;    File: lambdalistva.lsp
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


(in-package :compiler)


  


;;--------------------------------------------------
;;--------------------------------------------------
;;
;; Calling functions
;;
;;--------------------------------------------------
;;--------------------------------------------------


(defun cmp-lookup-function (fn-designator evaluate-env)
  "Return a pointer to a core::Closure"
  (if (atom fn-designator)
      (let ((classified (classify-function-lookup evaluate-env fn-designator)))
	(if (eq (car classified) 'core::global-function)
	    (irc-intrinsic "va_symbolFunction" (irc-global-symbol fn-designator evaluate-env))
	    (irc-intrinsic "va_lexicalFunction"
			   (jit-constant-i32 (caddr classified))
			   (jit-constant-i32 (cadddr classified))
			   (irc-renv evaluate-env))))
      (if (eq (car fn-designator) 'cl:lambda)
	  (error "Handle lambda expressions at head of form")
	  (error "Illegal head of form: ~a" fn-designator))))



(defun codegen-call (result form evaluate-env)
  "Evaluate each of the arguments into an alloca and invoke the function"
  ;; setup the ActivationFrame for passing arguments to this function in the setup arena
  (assert-result-isa-llvm-value result)
  (let* ((head (car form)))
    (cond
      ((and (atom head) (symbolp head))
       (let ((nargs (length (cdr form)))
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
         (let ((closure (cmp-lookup-function head evaluate-env)))
	   (irc-low-level-trace :flow)
           (irc-funcall result closure (reverse args))
	   (irc-low-level-trace :flow))
         ))
      ((and (consp head) (eq head 'lambda))
       (error "Handle lambda applications"))
      (t (compiler-error form "Illegal head for form %s" head)))))





;;--------------------------------------------------
;;--------------------------------------------------
;;
;; Handling arguments that are passed into functions
;;
;;--------------------------------------------------
;;--------------------------------------------------



(defun compile-error-if-not-enough-arguments (nargs lv-required-number-of-arguments)
  "If nargs < lv-required-number-of-arguments then throw an exception - no cleanup needed because no new environment was created yet"
  (let* ((error-block (irc-basic-block-create "error"))
	 (cont-block (irc-basic-block-create "continue")))
    (let ((cmp (irc-icmp-slt nargs lv-required-number-of-arguments "enough-args")))
      (irc-cond-br cmp error-block cont-block)
      (irc-begin-block error-block)
      (irc-intrinsic "va_notEnoughArgumentsException" *gv-current-function-name* nargs lv-required-number-of-arguments )
      (irc-unreachable)
      (irc-begin-block cont-block)
      )))


(defun compile-error-if-wrong-number-of-arguments (nargs required-number-of-arguments)
  "If nargs /= lv-required-number-of-arguments then throw an exception - no cleanup needed/nothing was created"
  (let* ((error-block (irc-basic-block-create "error"))
	 (cont-block (irc-basic-block-create "continue"))
	 (given-number-of-arguments nargs )
	 (required-number-of-arguments (jit-constant-size_t required-number-of-arguments))
	 (cmp (irc-icmp-ne given-number-of-arguments required-number-of-arguments "correct-num-args")))
    (irc-cond-br cmp error-block cont-block)
    (irc-begin-block error-block)
    (compile-error-if-not-enough-arguments given-number-of-arguments required-number-of-arguments)
    (irc-intrinsic "va_tooManyArgumentsException" *gv-current-function-name* given-number-of-arguments required-number-of-arguments)
    (irc-unreachable)
    (irc-begin-block cont-block)
    ))

(defun compile-error-if-too-many-arguments (nargs maximum-number-of-arguments)
  "If nargs > lv-maximum-number-of-arguments then throw an exception - no cleanup needed/nothing was created"
  (let* ((error-block (irc-basic-block-create "error"))
	 (cont-block (irc-basic-block-create "continue"))
	 (given-number-of-arguments nargs )
	 (maximum-number-of-arguments (jit-constant-size_t maximum-number-of-arguments))
	 (cmp (irc-icmp-sgt given-number-of-arguments maximum-number-of-arguments "max-num-args")))
    (irc-cond-br cmp error-block cont-block)
    (irc-begin-block error-block)
    (irc-intrinsic "va_tooManyArgumentsException" *gv-current-function-name* given-number-of-arguments maximum-number-of-arguments)
    (irc-unreachable)
    (irc-begin-block cont-block)
    ))

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

(defmacro with-target-reference-no-bind-do ((target-ref target env) &rest body)
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
;;     (define-binding-in-value-environment* ,env ,target)
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
	 (let ((,i1-target-is-bound-gs (irc-trunc (irc-intrinsic "isBound" ,target-ref) +i1+))
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
                                   args
				   new-env
				   entry-arg-idx ;; this is now in a register
				   )
  (irc-branch-to-and-begin-block (irc-basic-block-create "process-required-arguments"))
  (compile-error-if-not-enough-arguments (calling-convention-nargs args)
					 (jit-constant-size_t (car reqargs)))
  (dbg-set-current-debug-location-here)
  ;; First save any special values
  (do* ((cur-req (cdr reqargs) (cdr cur-req))
	(target (car cur-req) (car cur-req)))
       ((endp cur-req) ())
    (compile-save-if-special new-env target))
  ;; Now copy the required arguments into their targets
  (do* ((cur-req (cdr reqargs) (cdr cur-req))
	(target (car cur-req) (car cur-req))
	(arg-idx entry-arg-idx (irc-add arg-idx (jit-constant-size_t 1) "arg-idx")))
       ((endp cur-req) arg-idx)
    (let* ((target-idx (cdr target))
	   (val-ref (calling-convention-args.va-arg args arg-idx target-idx)))
      (with-target-reference-do (tref target new-env) ; run-time binding
	(irc-intrinsic "copyTspTptr" tref val-ref)))))


(defun compile-optional-arguments (optargs old-env
				   args ;; nargs va-list
				   new-env
				   entry-arg-idx )
  (irc-branch-to-and-begin-block (irc-basic-block-create "process-optional-arguments"))
  (cmp-log "About to compile-optional-arguments: %s\n" optargs)
  ;; First save any special values
  (do* ((cur-opt (cdr optargs) (cdddr cur-opt))
	(target (car cur-opt) (car cur-opt))
	(init-form (cadr cur-opt) (cadr cur-opt))
	(flag (caddr cur-opt) (caddr cur-opt)))
       ((endp cur-opt) ())
    (compile-save-if-special new-env target)
    (when flag
      (compile-save-if-special new-env flag)))
  ;; Copy argument values or evaluate init forms
  (do* ((cur-opt (cdr optargs) (cdddr cur-opt))
	(target (car cur-opt) (car cur-opt))
	(init-form (cadr cur-opt) (cadr cur-opt))
	(flag (caddr cur-opt) (caddr cur-opt))
	(temp-result (irc-alloca-tsp :label "temp-result"))
	(arg-idx entry-arg-idx (irc-add arg-idx (jit-constant-size_t 1) "arg-idx")))
       ((endp cur-opt) arg-idx)
    (let ((arg-block (irc-basic-block-create "opt-arg"))
	  (init-block (irc-basic-block-create "opt-init"))
	  (cont-block (irc-basic-block-create "opt-cont"))
	  (cmp (irc-icmp-slt arg-idx (calling-convention-nargs args) "enough-given-args")))
      (irc-cond-br cmp arg-block init-block)
      (irc-begin-block arg-block)
      (let ((val-ref (calling-convention-args.va-arg args arg-idx)))
	(with-target-reference-no-bind-do (target-ref target new-env) ; run-time af binding
	  (irc-intrinsic "copyTspTptr" target-ref val-ref))
	(when flag
	  (with-target-reference-no-bind-do (flag-ref flag new-env) ; run-time AF binding
	    (irc-intrinsic "copyTsp" flag-ref (compile-reference-to-literal t new-env))))
	(irc-br cont-block)
	(irc-begin-block init-block)
	(with-target-reference-no-bind-do (target-ref target new-env) ; codegen init-form into target-ref
	  ;; Use new-env so that symbols already defined in this lambda can be accessed
	  (codegen target-ref init-form new-env))
	(when flag
	  (with-target-reference-no-bind-do (flag-ref flag new-env) ; copy nil into flag-ref
	    (irc-intrinsic "copyTsp" flag-ref (compile-reference-to-literal nil new-env))))
	(irc-br cont-block)
	(irc-begin-block cont-block)))
    (define-binding-in-value-environment* new-env target)
    (when flag
      (define-binding-in-value-environment* new-env flag))
    ))




(defun compile-rest-arguments (rest-var old-env
			       args     ; nargs va-list
			       new-env entry-arg-idx)
  (irc-branch-to-and-begin-block (irc-basic-block-create "process-rest-arguments"))
  (with-target-reference-do (rest-ref rest-var new-env)
    (irc-intrinsic "copyTspTptr"
                   rest-ref
                   (irc-intrinsic "cc_gatherRestArguments"
                                  (calling-convention-nargs args)
                                  (calling-convention-va-list args)
                                  entry-arg-idx *gv-current-function-name* ))))

(defun compile-key-arguments-parse-arguments (keyargs
					      lambda-list-allow-other-keys
					      old-env
                                              args ; nargs va-list
					      new-env
					      entry-arg-idx
					      sawkeys )
  "saw-aok keeps track if &allow-other-keys was defined or if :allow-other-keys t/nil was seen.
   saw-aok can have the values (2[&a-o-k or :a-o-k t], 1[:a-o-k nil] or 0 [no &a-o-k or :a-o-k]) "
  (let ((process-kw-args-block (irc-basic-block-create "process-kw-args")))
    (irc-branch-to-and-begin-block process-kw-args-block)
    (let* ((entry-saw-aok (jit-constant-size_t (if lambda-list-allow-other-keys 2 0)))
	   (entry-bad-kw-idx (jit-constant-size_t 65536))
	   (aok-ref (compile-reference-to-literal :allow-other-keys old-env))
	   (loop-kw-args-block (irc-basic-block-create "loop-kw-args"))
	   (kw-exit-block (irc-basic-block-create "kw-exit-block"))
	   (loop-cont-block (irc-basic-block-create "loop-cont"))
	   (kw-start-block (irc-basic-block-create "kw-begin-block")))
      (irc-branch-to-and-begin-block kw-start-block)
      (let ((entry-arg-idx_lt_nargs (irc-icmp-slt entry-arg-idx (calling-convention-nargs args))) )
	(irc-cond-br entry-arg-idx_lt_nargs loop-kw-args-block kw-exit-block))
      (irc-begin-block loop-kw-args-block)
      (let* ((phi-saw-aok (irc-phi +size_t+ 2 "phi-saw-aok"))
	     (phi-arg-idx (irc-phi +size_t+ 2 "phi-reg-arg-idx"))
	     (phi-bad-kw-idx (irc-phi +size_t+ 2 "phi-bad-kw-idx")) )
	(irc-phi-add-incoming phi-saw-aok entry-saw-aok kw-start-block)
	(irc-phi-add-incoming phi-arg-idx entry-arg-idx kw-start-block)
	(irc-phi-add-incoming phi-bad-kw-idx entry-bad-kw-idx kw-start-block)
	(irc-low-level-trace)
	(let* ((arg-ref (calling-convention-args.va-arg args phi-arg-idx))
               (arg-idx+1 (irc-add phi-arg-idx (jit-constant-size_t 1)))
               (kw-arg-val (calling-convention-args.va-arg args arg-idx+1)))
	  (irc-intrinsic "cc_ifNotKeywordException" arg-ref phi-arg-idx (calling-convention-va-list args))
	  (let* ((eq-aok-ref-and-arg-ref (irc-trunc (irc-intrinsic "compareTspTptr" aok-ref arg-ref) +i1+)) ; compare arg-ref to a-o-k
		 (aok-block (irc-basic-block-create "aok-block"))
		 (possible-kw-block (irc-basic-block-create "possible-kw-block"))
		 (advance-arg-idx-block (irc-basic-block-create "advance-arg-idx-block"))
		 (bad-kw-block (irc-basic-block-create "bad-kw-block"))
                 (use-kw-block (irc-basic-block-create "use-kw-block"))
		 (good-kw-block (irc-basic-block-create "good-kw-block"))
		 )
	    (irc-cond-br eq-aok-ref-and-arg-ref aok-block possible-kw-block)
	    (irc-begin-block aok-block)
	    (let* ((loop-saw-aok (irc-intrinsic "cc_allowOtherKeywords"
					   phi-saw-aok
                                           kw-arg-val)))
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
		       (test-kw-and-arg (irc-intrinsic "matchKeywordOnce" kw-ref arg-ref (elt sawkeys idx)))
                       (no-kw-match (irc-icmp-eq test-kw-and-arg (jit-constant-size_t 0)))
		       (matched-kw-block (irc-basic-block-create "matched-kw-block"))
		       (not-seen-before-kw-block (irc-basic-block-create "not-seen-before-kw-block"))
                       )
		  (irc-cond-br no-kw-match next-kw-test-block matched-kw-block)
		  (irc-begin-block matched-kw-block)
		  (let ((kw-seen-already (irc-icmp-eq test-kw-and-arg (jit-constant-size_t 2))))
                    (irc-cond-br kw-seen-already good-kw-block not-seen-before-kw-block)
                    (irc-begin-block not-seen-before-kw-block)
                    (with-target-reference-no-bind-do (target-ref target new-env) ; run-time binding
                      (irc-intrinsic "copyTspTptr" target-ref kw-arg-val))
                    ;; Set the boolean flag to indicate that we saw this key
                    (irc-store (jit-constant-i8 1) (elt sawkeys idx))
                    (when flag
                      (with-target-reference-no-bind-do (flag-ref flag new-env)
                        (irc-intrinsic "copyTsp" flag-ref (compile-reference-to-literal t new-env))))
                    (irc-br good-kw-block)
                    (irc-begin-block next-kw-test-block))))
	      ;; We fell through all the keyword tests - this might be a unparameterized keyword
	    (irc-branch-to-and-begin-block bad-kw-block) ; fall through to here if no kw recognized
	    (let ((loop-bad-kw-idx (irc-intrinsic "cc_trackFirstUnexpectedKeyword"
					     phi-bad-kw-idx phi-arg-idx)))
	      (irc-low-level-trace)
	      (irc-br advance-arg-idx-block)
	      (irc-begin-block good-kw-block) ; jump to here if kw was recognized
	      (irc-low-level-trace)
	      (irc-br advance-arg-idx-block)
	      ;; Now advance the arg-idx, finish up the phi-nodes
	      ;; and if we ran out of arguments branch out of the loop else branch to the top of the loop
	      (irc-begin-block advance-arg-idx-block)
	      (let* ((phi-arg-bad-good-aok (irc-phi +size_t+ 3 "phi-this-was-aok"))
		     (phi.aok-bad-good.bad-kw-idx (irc-phi +size_t+ 3 "phi.aok-bad-good.bad-kw-idx")))
		(irc-phi-add-incoming phi-arg-bad-good-aok loop-saw-aok aok-block)
		(irc-phi-add-incoming phi-arg-bad-good-aok phi-saw-aok bad-kw-block)
		(irc-phi-add-incoming phi-arg-bad-good-aok phi-saw-aok good-kw-block)
		(irc-phi-add-incoming phi.aok-bad-good.bad-kw-idx phi-bad-kw-idx aok-block)
		(irc-phi-add-incoming phi.aok-bad-good.bad-kw-idx loop-bad-kw-idx bad-kw-block)
		(irc-phi-add-incoming phi.aok-bad-good.bad-kw-idx phi-bad-kw-idx good-kw-block)
		(irc-low-level-trace)
		(let* ((loop-arg-idx (irc-add phi-arg-idx (jit-constant-size_t 2)))
		       (loop-arg-idx_lt_nargs (irc-icmp-slt loop-arg-idx (calling-convention-nargs args))))
		  (irc-phi-add-incoming phi-saw-aok phi-arg-bad-good-aok advance-arg-idx-block)
		  (irc-phi-add-incoming phi-bad-kw-idx phi.aok-bad-good.bad-kw-idx advance-arg-idx-block)
		  (irc-phi-add-incoming phi-arg-idx loop-arg-idx advance-arg-idx-block)
		  (irc-cond-br loop-arg-idx_lt_nargs loop-kw-args-block loop-cont-block)
		  (irc-begin-block loop-cont-block)
		  (irc-intrinsic "cc_ifBadKeywordArgumentException" phi-arg-bad-good-aok phi.aok-bad-good.bad-kw-idx arg-ref)
		  (let ((kw-done-block (irc-basic-block-create "kw-done-block")))
		    (irc-branch-to-and-begin-block kw-done-block)
		    (irc-branch-to-and-begin-block kw-exit-block)
		    (let ((phi-arg-idx-final (irc-phi +size_t+ 2 "phi-arg-idx-final")))
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
      (with-target-reference-no-bind-do (target-ref target new-env) ; run-time binding
	(irc-low-level-trace)
	(codegen target-ref init-form new-env))
      (irc-low-level-trace)
      (when flag
	(irc-low-level-trace)
	(with-target-reference-no-bind-do (flag-ref flag new-env) ; run-time binding
	  (irc-intrinsic "copyTsp" flag-ref (compile-reference-to-literal nil new-env))))
      (irc-low-level-trace)
      (irc-branch-to-and-begin-block next-kw-block))
    (define-binding-in-value-environment* new-env target)
    (when flag
      (define-binding-in-value-environment* new-env flag))
    ))



(defun compile-key-arguments (keyargs
			      lambda-list-allow-other-keys
			      old-env
                              args ; nargs va-list
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
                                                          args ; nargs va-list
							  new-env
							  entry-arg-idx
							  sawkeys)))
      (irc-low-level-trace)
      (compile-key-arguments-fill-in-init-forms keyargs new-env sawkeys )
      (irc-low-level-trace)
      arg-idx)))





(defun compile-throw-if-excess-keyword-arguments ( args ;; nargs va-list
                                                  arg-idx)
  (irc-intrinsic "va_ifExcessKeywordArgumentsException" *gv-current-function-name* (calling-convention-nargs args) (calling-convention-va-list args) arg-idx))





(defun compile-aux-arguments (auxargs old-env new-env)
  (let ((aux-arguments-block (irc-basic-block-create "process-aux-arguments")))
    (irc-br aux-arguments-block)
    (irc-begin-block aux-arguments-block))
  ;; save special variables
  (do* ((cur-aux (cdr auxargs) (cddr cur-aux))
	(target (car cur-aux) (car cur-aux))
	(init-form (cadr cur-aux) (cadr cur-aux))
	(temp-result (irc-alloca-tsp :label "temp-result")))
       ;; TODO: setup one temp-result and use it for all types of args
       ((endp cur-aux) ())
    (compile-save-if-special new-env target))
  ;; Now process the arguments
  (do* ((cur-aux (cdr auxargs) (cddr cur-aux))
	(target (car cur-aux) (car cur-aux))
	(init-form (cadr cur-aux) (cadr cur-aux))
	(temp-result (irc-alloca-tsp :label "temp-result")))
       ((endp cur-aux) ())
    ;; Copy the argument into _temp-result_
    (cmp-log "Compiling aux init-form %s\n" init-form)
    (cmp-log "    in environment: %s\n" new-env)
    (with-target-reference-do (target-ref target new-env) ; run-time binding
      (codegen target-ref init-form new-env))))




(defun compile-general-lambda-list-code (lambda-list-handler
                                         old-env
                                         args
                                         new-env)
;;;				 &aux (nargs (first argument-holder)) (va-list (second argument-holder)))
  "Fill the dest-activation-frame with values using the
lambda-list-handler/env/argument-activation-frame"
  ;; Declare the arg-idx i32 that stores the current index in the argument-activation-frame
  (dbg-set-current-debug-location-here)
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys auxargs)
      (process-lambda-list-handler lambda-list-handler)
    (let* ((arg-idx (jit-constant-size_t 0))
	   (opt-arg-idx (compile-required-arguments reqargs
						    old-env
						    args ;; nargs va-list
						    new-env arg-idx))
	   (rest/key-arg-idx (if (/= 0 (car optargs))
				 (compile-optional-arguments optargs
							     old-env
							     args ; nargs va-list
							     new-env opt-arg-idx)
				 opt-arg-idx))
	   (dummy (when rest-var
		    (compile-rest-arguments rest-var
					    old-env
					    args ; nargs va-list
					    new-env
					    rest/key-arg-idx)))
	   (last-arg-idx (if key-flag
			     (compile-key-arguments keyargs
						    allow-other-keys
						    old-env
						    args ; nargs va-list
						    new-env
						    rest/key-arg-idx)
			     rest/key-arg-idx)))
      (unless rest-var
        (if key-flag
            (compile-throw-if-excess-keyword-arguments args last-arg-idx)
            (compile-error-if-too-many-arguments (calling-convention-nargs args) (+ (car reqargs) (car optargs)))))
      (when (/= 0 (car auxargs))
	(compile-aux-arguments auxargs old-env new-env))
      )
    ))


(defun compile-<=3-required-arguments (reqargs
                                       old-env
                                       args
                                       new-env)
;;;				 &aux (nargs (first argument-holder)) (va-list (second argument-holder)))
  "Fill the dest-activation-frame with values using the
lambda-list-handler/env/argument-activation-frame"
  ;; First save any special values
  (compile-error-if-wrong-number-of-arguments (calling-convention-nargs args) (car reqargs))
  (do* ((cur-req (cdr reqargs) (cdr cur-req))
	(target (car cur-req) (car cur-req)))
       ((endp cur-req) ())
    (compile-save-if-special new-env target))
  ;; Declare the arg-idx i32 that stores the current index in the argument-activation-frame
  (dbg-set-current-debug-location-here)
  (let ((fixed-args (calling-convention-register-args args)))
    (do* ((cur-target (cdr reqargs) (cdr cur-target))
          (cur-fixed-args fixed-args (cdr cur-fixed-args))
          (target (car cur-target) (car cur-target))
          (arg (car cur-fixed-args) (car cur-fixed-args)))
         ((null cur-target))
      (let ((tsp-arg (irc-insert-value (llvm-sys:undef-value-get +tsp+) arg (list 0) "arg")))
        (with-target-reference-do (tref target new-env) (irc-store tsp-arg tref))))))



(defun compile-lambda-list-code (lambda-list-handler
                                 old-env
                                 args
                                 new-env)
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys auxargs)
      (process-lambda-list-handler lambda-list-handler)
    (let ((req-opt-only (and (not rest-var)
                             (not key-flag)
                             (eql 0 (car keyargs))
                             (eql 0 (car auxargs))
                             (not allow-other-keys)))
          (num-req (car reqargs))
          (num-opt (car optargs)))
      (cond
        ;; Special cases (foo) (foo x) (foo x y) (foo x y z)  - passed in registers
        ((and req-opt-only (<= num-req 3) (eql 0 num-opt) )
         (compile-<=3-required-arguments reqargs old-env args new-env))
        ;; Test for
        ;; (x &optional y)
        ;; (x y &optional z)
        (t
         (compile-general-lambda-list-code lambda-list-handler old-env args new-env))))))
