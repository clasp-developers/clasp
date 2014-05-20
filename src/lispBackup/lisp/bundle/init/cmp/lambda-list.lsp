

(in-package :compiler)


(defun compile-error-if-not-enough-arguments (env activation-frame number-of-passed-arguments lv-required-number-of-arguments)
  "If number-of-passed-arguments < lv-required-number-of-arguments then throw an exception - no cleanup needed because no new environment was created yet"
  (let* ((fn (irc-current-function))
	 (error-block (irc-basic-block-create "error"))
	 (cont-block (irc-basic-block-create "continue")))
    (let ((cmp (irc-icmp-slt number-of-passed-arguments lv-required-number-of-arguments "enough-args")))
      (irc-cond-br cmp error-block cont-block)
      (irc-begin-block error-block)
      (irc-call env "throwNotEnoughArgumentsException" *gv-current-function-name* activation-frame number-of-passed-arguments lv-required-number-of-arguments )
      (irc-unreachable)
      (irc-begin-block cont-block)
      )))


(defun compile-error-if-wrong-number-of-arguments (env lv-src-activation-frame required-number-of-arguments)
  "If lv-given-number-of-arguments /= lv-required-number-of-arguments then throw an exception - no cleanup needed/nothing was created"
  (let* ((fn (irc-current-function))
	 (error-block (irc-basic-block-create "error"))
	 (cont-block (irc-basic-block-create "continue"))
	 (lv-given-number-of-arguments (irc-call env "activationFrameSize" lv-src-activation-frame "given-num-args"))
	 (lv-required-number-of-arguments (jit-constant-i32 required-number-of-arguments))
	 (cmp (irc-icmp-ne lv-given-number-of-arguments lv-required-number-of-arguments "correct-num-args")))
    (irc-cond-br cmp error-block cont-block)
    (irc-begin-block error-block)
    (compile-error-if-not-enough-arguments env lv-src-activation-frame lv-given-number-of-arguments lv-required-number-of-arguments)
    (irc-call env "throwTooManyArgumentsException" *gv-current-function-name* lv-src-activation-frame lv-given-number-of-arguments lv-required-number-of-arguments)
    (irc-unreachable)
    (irc-begin-block cont-block)
    ))

(defun compile-save-and-bind-special (env target-symbol val)
  (let ((saved-special-val (irc-alloca-tsp env (bformat nil "saved->%s" (symbol-name target-symbol)))))
    (irc-call env "symbolValueRead" saved-special-val (irc-global-symbol target-symbol))
    (irc-call env "copyTsp" (codegen-special-var-reference target-symbol env) val)
    (irc-push-unwind env `(symbolValueRestore ,saved-special-val ,target-symbol))
    (value-environment-define-special-binding env target-symbol)))

(defun compile-bind-lexical (env target-symbol target-index val)
  (let ((ptr (irc-call env "valueFrameReference"
		       (irc-renv env)
		       (jit-constant-i32 target-index)
		       (bformat nil "dest-frame-%d" target-index))))
    (dbg-set-current-debug-location-here)
    (value-environment-define-lexical-binding env target-symbol target-index)
    (irc-call env "copyTsp" ptr val))
)



(defun compile-bind-target (env val target)
  (cond
    ((eq (car target) 'special-var)
     (compile-save-and-bind-special env (cdr target) val))
    ((eq (car target) 'lexical-var)
     (compile-bind-lexical env (cadr target) (cddr target) val))
    (t (error "Illegal target type[~a] for optional argument" target))))




(defun compile-inc (i32-ref)
  (let ((incd (irc-add (irc-load i32-ref) (jit-constant-i32 1))))
    (irc-store incd i32-ref)))


(defun compile-required-arguments (reqargs env
				   argument-activation-frame
				   new-env arg-idx number-of-passed-arguments)
  (let ((required-arguments-block (irc-basic-block-create "process-required-arguments")))
    (irc-br required-arguments-block)
    (irc-begin-block required-arguments-block))
  (compile-error-if-not-enough-arguments env argument-activation-frame number-of-passed-arguments (jit-constant-i32 (car reqargs)))
  ;;  (compile-debug-print-af "In compile-required-arguments argument-activation-frame-->" argument-activation-frame)
  (dbg-set-current-debug-location-here)
  (do* ((cur-req (cdr reqargs) (cdr cur-req))
	(target (car cur-req) (car cur-req)))
       ((endp cur-req) ())
    (let* ((target-head (car target))
	   (target-idx (cdr target))
	   (val (progn
		  (dbg-set-current-debug-location-here)
		  (irc-call env "valueFrameReference" argument-activation-frame
			    (irc-load arg-idx "arg-idx-val")
			    (bformat nil "%s-get-%d" (llvm-sys:get-name argument-activation-frame) target-idx )))))
      (compile-bind-target new-env val target)
      (compile-inc arg-idx)))
  )




(defun compile-optional-arguments (optargs old-env
				   argument-activation-frame new-env arg-idx number-of-passed-arguments)
  (let ((optional-arguments-block (irc-basic-block-create "process-optional-arguments")))
    (irc-br optional-arguments-block)
    (irc-begin-block optional-arguments-block))
#|  (compile-error-if-not-enough-arguments old-env argument-activation-frame number-of-passed-arguments
					 (jit-constant-i32 (car optargs)))
|#
  (cmp-log "About to compile-optional-arguments: %s\n" optargs)
  (do* ((cur-opt (cdr optargs) (cdddr cur-opt))
	(target (car cur-opt) (car cur-opt))
	(init-form (cadr cur-opt) (cadr cur-opt))
	(flag (caddr cur-opt) (caddr cur-opt))
	(temp-result (irc-alloca-tsp new-env "temp-result")))
       ((endp cur-opt) ())
    (let ((fn (irc-current-function))
	  (arg-block (irc-basic-block-create "opt-arg"))
	  (init-block (irc-basic-block-create "opt-init"))
	  (cont-block (irc-basic-block-create "opt-cont")))
      (let ((cmp (irc-icmp-slt (irc-load arg-idx "arg-idx-val") number-of-passed-arguments "enough-given-args")))
	(irc-cond-br cmp arg-block init-block)
	(irc-begin-block arg-block)
	(let ((val (irc-call old-env "valueFrameReference" argument-activation-frame
			     (irc-load arg-idx "arg-idx-val")
			     (bformat nil "%s-get-%d" (llvm-sys:get-name argument-activation-frame) (cdr target) ))))
	  (dbg-set-current-debug-location-here)
	  (irc-call old-env "copyTsp" temp-result val))
	(when flag
	  (compile-bind-target new-env (compile-reference-to-literal t new-env) flag))
	(irc-br cont-block) ;; Evaluate the initialization form into _temp-result_
	(irc-begin-block init-block)
	(codegen temp-result init-form new-env) ;; Use new-env so that symbols already defined in this lambda can be accessed
	(when flag
	  (compile-bind-target new-env (compile-reference-to-literal nil new-env) flag))
	(irc-br cont-block) 
	;; Now copy the value from _temp-result_ to the target which is either a special or lexical variable
	(irc-begin-block cont-block)
	(compile-bind-target new-env temp-result target)
	(compile-inc arg-idx)))) )


(defun compile-rest-arguments (rest-var old-env argument-activation-frame new-env arg-idx number-of-passed-arguments)
  (let ((rest-arguments-block (irc-basic-block-create "process-rest-arguments")))
    (irc-br rest-arguments-block)
    (irc-begin-block rest-arguments-block))
  (let ((temp-rest (irc-alloca-tsp new-env "temp-rest")))
    (dbg-set-current-debug-location-here)
    (irc-call old-env "fillRestTarget" temp-rest argument-activation-frame
	      (irc-load arg-idx "arg-idx-val") *gv-current-function-name* )
    (compile-bind-target new-env temp-rest rest-var)) )



(defun compile-key-arguments (keyargs
			      lambda-list-allow-other-keys
			      old-env
			      argument-activation-frame
			      new-env
			      arg-idx
			      number-of-passed-arguments)
  (let ((key-arguments-block (irc-basic-block-create "process-key-arguments")))
    (irc-br key-arguments-block)
    (irc-begin-block key-arguments-block))
  (let* ((i32-allow-other-keywords (if lambda-list-allow-other-keys
				       (jit-constant-i32 1)
				       (jit-constant-i32 0)))
	 (will-allow-other-keywords (irc-call old-env "checkForAllowOtherKeywords"
					      i32-allow-other-keywords
					      argument-activation-frame
					      (irc-load arg-idx "arg-idx-val"))))
    (do* ((cur-key-arg (cdr keyargs) (cddddr cur-key-arg))
	  (key (car cur-key-arg) (car cur-key-arg))
	  (target (cadr cur-key-arg) (cadr cur-key-arg))
	  (init-form (caddr cur-key-arg) (caddr cur-key-arg))
	  (flag (cadddr cur-key-arg) (cadddr cur-key-arg))
	  (temp-result (irc-alloca-tsp new-env "temp-result")))
	 ((endp cur-key-arg) ())
      (let ((fn (irc-current-function))
	    (arg-block (irc-basic-block-create "key-arg"))
	    (init-block (irc-basic-block-create "key-init"))
	    (check-other-keywords-block (irc-basic-block-create "key-check-other-keywords"))
	    (cont-block (irc-basic-block-create "key-cont"))
	    (global-keyword-ptr (irc-global-symbol key)))
	(let* ((found (irc-call old-env "lookupKeyword"
				temp-result
				global-keyword-ptr
				argument-activation-frame
				(irc-load arg-idx "arg-idx-val")))
	       (cmp (irc-icmp-eq found (jit-constant-i32 1))))
	  (irc-cond-br cmp arg-block init-block)
	  (irc-begin-block arg-block)
	  ;; Copy true into flag
	  (when flag
	    (compile-bind-target new-env (compile-reference-to-literal t new-env) flag))
	  (irc-br cont-block)
	  ;; Evaluate the initialization form into _temp-result_
	  (irc-begin-block init-block)
	  (codegen temp-result init-form new-env) 
	  (when flag
	    (compile-bind-target new-env (compile-reference-to-literal nil new-env) flag))
	  (irc-br cont-block)
	  ;; Now copy the value from _temp-result_ to the target which is either a special or lexical variable
	  (irc-begin-block cont-block)
	  (compile-bind-target new-env temp-result target) )))
    (let ((fn (irc-current-function))
	  (check-if-other-keywords-block (irc-basic-block-create "key-check-if-other-keywords"))
	  (done-keywords-block (irc-basic-block-create "done-keywords"))
	  (cmp (irc-icmp-eq will-allow-other-keywords (jit-constant-i32 1))))
      (irc-cond-br cmp done-keywords-block check-if-other-keywords-block)
      (irc-begin-block check-if-other-keywords-block)
      (irc-call old-env "throwIfOtherKeywords" argument-activation-frame
		(irc-load arg-idx "arg-idx-val") *gv-current-function-name*)
      (irc-br done-keywords-block)
      (irc-begin-block done-keywords-block)
      )))


(defun compile-throw-if-excess-keyword-arguments (env argument-activation-frame arg-idx)
  (irc-call env "throwIfExcessKeywordArguments" *gv-current-function-name* argument-activation-frame (irc-load arg-idx "arg-idx-val") ))




(defun compile-aux-arguments (auxargs old-env new-env)
  (let ((aux-arguments-block (irc-basic-block-create "process-aux-arguments")))
    (irc-br aux-arguments-block)
    (irc-begin-block aux-arguments-block))
  (do* ((cur-aux (cdr auxargs) (cddr cur-aux))
	(target (car cur-aux) (car cur-aux))
	(init-form (cadr cur-aux) (cadr cur-aux))
	(temp-result (irc-alloca-tsp new-env "temp-result")))
       ;; TODO: setup one temp-result and use it for all types of args
       ((endp cur-aux) ())
    ;; Copy the argument into _temp-result_
    (codegen temp-result init-form new-env) ;; use new-env so that previously bound symbols in this lambda-list can be used in init-forms
    (compile-bind-target new-env temp-result target) ))




(defun compile-lambda-list-code (lambda-list-handler old-env argument-activation-frame new-env)
  "Fill the dest-activation-frame with values using the
lambda-list-handler/env/argument-activation-frame"
  ;; Declare the arg-idx i32 that stores the current index in the argument-activation-frame
  (dbg-set-current-debug-location-here)
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys auxargs)
      (process-lambda-list-handler lambda-list-handler)
    (let ((arg-idx (irc-alloca-i32 old-env 0 "arg-idx"))
	  (number-of-passed-arguments (irc-call old-env "activationFrameSize" argument-activation-frame)))
      (compile-required-arguments reqargs
				  old-env
				  argument-activation-frame
				  new-env arg-idx
				  number-of-passed-arguments)
      (when (/= 0 (car optargs))
	(compile-optional-arguments optargs
				    old-env
				    argument-activation-frame
				    new-env arg-idx
				    number-of-passed-arguments)) 
      (if rest-var
	  (compile-rest-arguments rest-var
				  old-env
				  argument-activation-frame
				  new-env
				  arg-idx
				  number-of-passed-arguments))
      (if key-flag
	  (compile-key-arguments keyargs
				 allow-other-keys
				 old-env
				 argument-activation-frame
				 new-env arg-idx
				 number-of-passed-arguments)
	  (unless rest-var
	    (compile-throw-if-excess-keyword-arguments old-env argument-activation-frame arg-idx)))
      (when (/= 0 (car auxargs))
	(compile-aux-arguments auxargs old-env new-env))
      )))



