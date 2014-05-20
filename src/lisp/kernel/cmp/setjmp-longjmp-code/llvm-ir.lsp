;;
;; Wrappers for llvm-ir instruction generation functions 
;;
;; All instructions talk to the special variable *irbuilder*
;;

(in-package :compiler)


(defun irc-single-step-callback (env)
  (irc-call env "singleStepCallback" ))


(defun irc-attach-debugging-info-to-value-frame (af symbol-list-designator env)
  (let ((symbol-names (cond
			((lambda-list-handler-p symbol-list-designator)
			 (let ((symbol-list (names-of-all-lexical-variables symbol-list-designator)))
			   (make-array (length symbol-list) :initial-contents symbol-list)))
			(t (error "Handle symbol-list-designator: ~a" symbol-list-designator)))))
    (when *debug-attach-debugging-info-to-value-frames*
      ;;    (break "About to codegen attach-debugging-info")
      (when symbol-names
	(let* ((ltv-idx (codegen-literal nil symbol-names env))
	       (ltv-ref (compile-reference-to-load-time-value ltv-idx env)))
	  (irc-call env "attachDebuggingInfoToValueFrame" af ltv-ref))))))






(defun handle-exit-scope (scope-info env)
  (let ((scope-exit-fn (bformat nil "trace_exit%sScope" (cadr scope-info)))
	(scope-level (caddr scope-info))
	(scope-msg (cadddr scope-info)))
    (irc-call env scope-exit-fn scope-level scope-msg)))


(defun irc-personality-function ()
  (get-function-or-error *the-module* "__gxx_personality_v0"))

(defun irc-set-cleanup (landpad val)
  (llvm-sys:set-cleanup landpad val))


(defun irc-create-landing-pad (num-clauses &optional (name ""))
    (llvm-sys:create-landing-pad *irbuilder* +exception-struct+ (irc-personality-function) num-clauses name))

(defun irc-add-clause (landpad type)
  (llvm-sys:add-clause landpad type))

(defun irc-switch (go-value default-block num-cases)
  (llvm-sys:create-switch *irbuilder* go-value default-block num-cases nil))


(defun irc-gep (str index &optional (name "gep"))
  (llvm-sys:create-gep0 *irbuilder* str (jit-constant-i32 index) name ))


(defun irc-exception-typeid** (name)
  (exception-typeid**-from-name name))

(defun irc-exception-typeid* (name)
  (exception-typeid*-from-name name))

(defun irc-preserve-exception-info (env lpad)
  (let ((exn.slot (lookup-metadata env :exn.slot))
	(ehselector.slot (lookup-metadata env :ehselector.slot)))
    (let ((exception-structure (llvm-sys:create-extract-value *irbuilder* lpad (list 0) "")))
      (llvm-sys:create-store *irbuilder* exception-structure exn.slot nil))
    (let ((exception-selector (llvm-sys:create-extract-value *irbuilder* lpad (list 1) "")))
      (llvm-sys:create-store *irbuilder* exception-selector ehselector.slot nil))
    (values exn.slot ehselector.slot)))



(defmacro with-catch ((exn.slot exception-ptr env) &rest body)
  (let ((exn-gs (gensym)))
    `(let* ((,exn-gs (llvm-sys:create-load-value-twine *irbuilder* ,exn.slot "exn"))
	    (,exception-ptr (irc-call ,env "__cxa_begin_catch" ,exn-gs)))
       ,@body
       (irc-call ,env "__cxa_end_catch"))))


#|
(defun irc-save-exception-info (env lpad)
  (let ((exception-structure (llvm-sys:create-extract-value *irbuilder* lpad (list 0) "")))
    (llvm-sys:create-store *irbuilder* exception-structure (irc-function-exn.slot env) nil))
  (let ((exception-selector (llvm-sys:create-extract-value *irbuilder* lpad (list 1) "")))
    (llvm-sys:create-store *irbuilder* exception-selector (irc-function-ehselector.slot env) nil)))
|#

(defparameter *use-unwind-resume* nil)





(defun irc-generate-resume-code (exn.slot ehselector.slot env)
  (dbg-set-current-debug-location-here)
  (let ((exn7 (llvm-sys:create-load-value-twine *irbuilder* exn.slot "exn7")))
    (if *use-unwind-resume*
	(progn
	  (irc-call env "_Unwind_Resume" exn7)
	  (llvm-sys:create-unreachable *irbuilder*)
	  )
	(let ((sel (llvm-sys:create-load-value-twine *irbuilder* ehselector.slot "sel")))
	  (let* ((undef (llvm-sys:undef-value-get +exception-struct+ ))
		 (lpad.val (llvm-sys:create-insert-value *irbuilder*
							 undef exn7 '(0) "lpad.val")))
	    (debug-print-i32 90)
	    (let ((lpad.val8 (llvm-sys:create-insert-value *irbuilder*
							   lpad.val sel '(1) "lpad.val8")))
	      (debug-print-i32 91)
	      (llvm-sys:create-resume *irbuilder* lpad.val8)))))
    ))

(defun irc-rethrow (env)
  (dbg-set-current-debug-location-here)
  (unless env (error "env should not be nil"))
  (irc-call env "__cxa_rethrow")
;;  (llvm-sys:create-unreachable *irbuilder*)
  )



(defun irc-set-function-for-environment (env fn)
  (setf-metadata env :function fn))

(defun irc-get-function-for-environment (env)
  (lookup-metadata env :function))

(defun irc-setup-cleanup-return-block (env)
  (let ((bblock (irc-basic-block-create "func-cleanup-return-block")))
    (setf-metadata env :cleanup-return-block bblock)))

(defun irc-get-cleanup-return-block (env)
  (lookup-metadata env :cleanup-return-block))


(defun irc-setup-cleanup-landing-pad-block (env)
  "Setup a cleanup landing-pad and code to save the exception info for the current function environment"
  (let ((cleanup-landing-pad-block (irc-basic-block-create "func-cleanup-landing-pad")))
    (setf-metadata env :cleanup-landing-pad-block cleanup-landing-pad-block)
    ))

(defun irc-get-cleanup-landing-pad-block (env)
  (lookup-metadata env :cleanup-landing-pad-block))




(defun irc-setup-exception-handler-cleanup-block (env)
  (let ((cleanup-block (irc-basic-block-create "func-ehcleanup")))
    (setf-metadata env :exception-handler-cleanup-block cleanup-block)
    cleanup-block)
  )

(defun irc-get-exception-handler-cleanup-block (env)
  (lookup-metadata env :exception-handler-cleanup-block))


(defun irc-setup-exception-handler-resume-block (env)
  (let ((cleanup-block (irc-basic-block-create "func-ehresume")))
    (setf-metadata env :exception-handler-resume-block cleanup-block)))

(defun irc-get-exception-handler-resume-block (env)
  (lookup-metadata env :exception-handler-resume-block))


(defun irc-setup-terminate-landing-pad-block (env)
  (let ((cleanup-block (irc-basic-block-create "func-terminate-lpad")))
    (setf-metadata env :terminate-landing-pad-block cleanup-block)))

(defun irc-get-terminate-landing-pad-block (env)
  (lookup-metadata env :terminate-landing-pad-block))





(defun irc-classify-variable (env var)
  "Lookup the variable in the lexical environment - if not found then check if it is a special"
  (let* ((classified (classify-value env var)))
    (if classified
	classified
	(cons 'core::special-var var))))
#|
	(if (special-p var)
	    (cons 'core::special-var var)
	    (error "Could not find variable %s in lexical/global environment" var)))))
|#



(defun irc-new-unbound-function-value-environment (old-env &key number-of-functions (label "function-frame"))
  "Create a new function environment and a new runtime environment"
  (let* ((new-env (make-function-value-environment number-of-functions old-env))
	 (new-renv (irc-alloca-afsp new-env :label label)))
    (or new-renv (error "The new-renv is nil - it shouldn't be"))
    (irc-set-renv new-env new-renv)
    new-env))


(defun irc-new-function-value-environment (old-env &key functions (label "function-frame"))
  "Create a new function environment and a new runtime environment"
  (let ((new-env (irc-new-unbound-function-value-environment old-env :number-of-functions (length functions))))
    (dolist (fn functions)
      (bind-function new-env (car fn) nil))
    new-env))


(defun irc-new-macrolet-environment (old-env)
  "Create a new function environment and a new runtime environment"
  (let* ((new-env (make-macrolet-environment old-env)))
    new-env))


(defun irc-new-symbol-macrolet-environment (old-env)
  "Create a new symbol-macrolet environment and a new runtime environment"
  (let* ((new-env (make-symbol-macrolet-environment old-env)))
    new-env))




(defun irc-new-unbound-value-environment (old-env &key lambda-list-handler
						    number-of-arguments (label "value-frame"))
  "Create a new environment and a new runtime environment"
  (or (not (and lambda-list-handler number-of-arguments))
      (error "Only pass one of :lambda-list-handler or :number-of-arguments"))
  (let* ((new-env (cond
		    (lambda-list-handler (make-value-environment lambda-list-handler old-env))
		    (number-of-arguments (make-value-environment-for-number-of-entries number-of-arguments old-env))
		    (t (error "You must provide either a lambda-list-handler or number-of-arguments"))))
	 (new-renv (irc-alloca-afsp new-env :label label)))
    (or new-renv (error "The new-renv is nil - it shouldn't be"))
    (irc-set-renv new-env new-renv)
    new-env))




(defun irc-new-value-environment (old-env
				  &key lambda-list-handler
				    number-of-arguments fill-runtime-form (label "env") )
  "Create a new environment and a new runtime environment"
  (or fill-runtime-form
      (error "You must provide a fill-runtime-form~%
 - if you want to fill later then use irc-new-unbound-value-environment"))
  (let ((new-env (irc-new-unbound-value-environment
		  old-env
		  :lambda-list-handler lambda-list-handler
		  :number-of-arguments number-of-arguments
		  :label label )))
    (when fill-runtime-form
      (funcall fill-runtime-form new-env))
    new-env))



(defun irc-new-unbound-value-environment-of-size (old-env &key number-of-arguments (label "value-frame"))
  "Create a new environment and a new runtime environment"
  (or number-of-arguments
      (error "Only pass one of :lambda-list-handler or :number-of-arguments"))
  (let* ((new-env (make-value-environment-for-number-of-entries number-of-arguments old-env))
	 (new-renv (irc-alloca-afsp-value-frame-of-size new-env number-of-arguments :label label)))
    (or new-renv (error "The new-renv is nil - it shouldn't be"))
    (irc-set-renv new-env new-renv)
    new-env))



(defun irc-new-value-environment-of-size (old-env &key number-of-arguments fill-runtime-form (label "env") )
  "Create a new environment and a new runtime environment"
  (or fill-runtime-form (error "You must provide a fill-runtime-form - if you want to fill later then use irc-new-unbound-value-environment"))
  (let ((new-env (irc-new-unbound-value-environment-of-size old-env
							    :number-of-arguments number-of-arguments
							    :label label )))
    (when fill-runtime-form
      (funcall fill-runtime-form new-env))
    new-env))









(defun irc-new-block-environment (old-env &key name)
  (let ((new-env (make-block-environment name old-env)))
    new-env))


(defun irc-new-catch-environment (old-env)
  (make-catch-environment old-env))


(defun irc-new-unwind-protect-environment (old-env)
  (make-unwind-protect-environment old-env))



	

	

(defun irc-set-renv (env renv)
  (set-runtime-environment env renv))

(defun irc-renv (env)
  (let ((renv (runtime-environment (current-visible-environment env))))
    (if renv
	(progn
	  (cmp-log "Returning non-nil renv\n")
	  renv)
	(let ((nil-renv (irc-call env "activationFrameNil")))
	  (cmp-log "Returning nil renv: %s\n" nil-renv)
	  nil-renv))))


(defun irc-i32-current-line-number ()
  (jit-constant-i32 *current-line-number*))

(defun irc-i32-current-column ()
  (jit-constant-i32 *current-column*))




(defun irc-generate-terminate-code (env)
      (let* ((personality-function (get-function-or-error *the-module* "__gxx_personality_v0"))
	     (landpad (llvm-sys:create-landing-pad *irbuilder* +exception-struct+ personality-function 1 "")))
	(llvm-sys:add-clause landpad (llvm-sys:constant-pointer-null-get +i8*+))
	(dbg-set-current-debug-location-here)
	(irc-low-level-trace)
	(irc-call env "brcl_terminate" *gv-source-path-name* (irc-i32-current-line-number) (irc-i32-current-column) *gv-current-function-name* )
	(llvm-sys:create-unreachable *irbuilder*)
	))


(defun irc-generate-unwind-protect-landing-pad-code (env)
      (let* ((personality-function (get-function-or-error *the-module* "__gxx_personality_v0"))
	     (landpad (llvm-sys:create-landing-pad *irbuilder* +exception-struct+ personality-function 1 "")))
	(llvm-sys:add-clause landpad (llvm-sys:constant-pointer-null-get +i8*+))
	(dbg-set-current-debug-location-here)
	(irc-low-level-trace)
	))





;; ---------------------------------------------------------------------------------------
;;
;; Environment unwinding
;;






(defun irc-make-unwind-protect-environment (unwind-form parent-env)
  (let ((new-env (make-unwind-protect-environment parent-env)))
    (setf-metadata new-env :unwind-form unwind-form)
    new-env))


(defun irc-unwind-unwind-protect-environment (env)
  (let ((unwind-form (local-metadata env :unwind-form))
	(unwind-result (irc-alloca-tsp env :label "unwind-result")))
    (codegen unwind-result unwind-form env)))


(defun irc-do-unwind-environment (env)
  (let ((unwind (local-metadata env :unwind)))
    (dolist (cc unwind)
      (let ((head (car cc)))
	(cond
	  ((eq head 'exit-lexical-scope) (error "Depreciated"));;(handle-exit-scope cc env))
	  ((eq head 'symbolValueRestore) (irc-call env "copyTsp" (irc-symbol-value-ref env (caddr cc)) (cadr cc) ))
	  (t (error (bformat nil "Unknown cleanup code: %s" cc))))
	)))
  )

(defun irc-unwind-environment (env)
  (cond
    ((unwind-protect-environment-p env)
     (irc-unwind-unwind-protect-environment env))
    ;; Do nothing for now with other environments
    (t nil))
  (irc-do-unwind-environment env)
  )



#||  ;; Depreciated
(defun irc-unwind-to-environment (begin-env end-env)
  "Unwind the environments from begin-env to end-env"
  (do* ((cur-env begin-env (get-parent-environment cur-env)))
       ((eq cur-env end-env) (irc-unwind-environment cur-env))
    (irc-unwind-environment cur-env))
  )
||#



(defun irc-cleanup-function-environment (env fntraceid)
  "Generate the code to cleanup the environment"
  (if env
      (progn
	(trace-exit-function-scope env fntraceid)
	(irc-do-unwind-environment env)
	(let ((cleanup (local-metadata env :cleanup)))
	  ;;      (cmp-log "Cleaning up env: %s\n" env)
	  (cmp-log "About to cleanup local-metadata :cleanup --> %s\n" cleanup)
	  (dolist (cc cleanup)
	    (let ((h (car cc)))
	      (cond
		((null h) (bformat t "Cleanup code of NIL!!!!!\n"))
		((eq h 'destructTsp) (irc-call env "destructTsp" (cadr cc)))
		((eq h 'destructTmv) (irc-call env "destructTmv" (cadr cc)))
		((eq h 'destructAFsp) (irc-call env "destructAFsp" (cadr cc)))
		((eq h 'exit-lexical-scope) (handle-exit-scope cc env))
		(t (break (bformat nil "Unknown cleanup code: %s" cc))))
	      ))))))










(defun irc-function-cleanup-and-return (env fntraceid)
  (when env
    (irc-cleanup-function-environment env fntraceid)
    (llvm-sys:create-ret-void *irbuilder*)
    (irc-begin-landing-pad-block (irc-get-cleanup-landing-pad-block env) (irc-get-function-for-environment env))
    (let* ((personality-function (get-function-or-error *the-module* "__gxx_personality_v0"))
	   (landpad (llvm-sys:create-landing-pad *irbuilder*
						 +exception-struct+
						 personality-function 0 "")))
      (declare (special *the-function-pass-manager*))
      (llvm-sys:set-cleanup landpad t)
      (dbg-set-current-debug-location-here)
      (irc-low-level-trace)
      (multiple-value-bind (exn.slot ehselector.slot)
	  (irc-preserve-exception-info env landpad)
	(debug-print-i32 100)
	(irc-branch-to-and-begin-block (irc-get-exception-handler-cleanup-block env))
	(with-landing-pad (irc-get-terminate-landing-pad-block env)
	  (irc-cleanup-function-environment env fntraceid))
	(irc-branch-to-and-begin-block (irc-get-exception-handler-resume-block env))
	(debug-print-i32 101)
	(irc-generate-resume-code exn.slot ehselector.slot env))
      (irc-begin-landing-pad-block (irc-get-terminate-landing-pad-block env))
      (irc-generate-terminate-code env)
      (cmp-log "About to verify the function in irc-function-cleanup-and-return\n")
      (irc-verify-function *current-function*)
      (when *the-function-pass-manager*
	(llvm-sys:function-pass-manager-run *the-function-pass-manager* *current-function*)
	))))






    
  



#|
(defun irc-rename-insert-block (name)
  "Rename the current insertion block to something more useful for reading/debugging IR"
  (let ((current-block (llvm-sys:get-insert-block *irbuilder*)))
    (llvm-sys:set-name current-block name)))
|#

(defun irc-basic-block-create (name &optional function)
  "Create a llvm::BasicBlock with (name) in the (function)"
  (llvm-sys:basic-block-create *llvm-context* (bformat nil "%s%s" *block-name-prefix* name) function))


(defun irc-get-insert-block ()
  (llvm-sys:get-insert-block *irbuilder*))

(defun irc-append-basic-block (function theblock)
  "Append the basic block to the _function_. If the _function_ is not passed then use the current function"
  (llvm-sys:append-basic-block function theblock))

(defun irc-set-insert-point (theblock)
  "Set the current insert point"
  (llvm-sys:set-insert-point-basic-block *irbuilder* theblock))



;;  "Control if low-level block tracing is on or off"
(defparameter *next-low-level-trace-index* 1000000001)
(defmacro irc-low-level-trace ()
  `(if *low-level-trace*
      (progn
	(let ((llt (get-function-or-error *the-module* "lowLevelTrace")))
	  (llvm-sys:create-call1 *irbuilder* llt (jit-constant-i32 *next-low-level-trace-index*) ""))
	(setq *next-low-level-trace-index* (+ 1 *next-low-level-trace-index*)))
      nil))


(defun irc-begin-landing-pad-block (theblock &optional (function *current-function*))
  "This doesn't invoke low-level-trace - it would interfere with the landing pad"
  (irc-append-basic-block function theblock)
  (irc-set-insert-point theblock)
  )


(defun irc-begin-block (theblock &optional (function *current-function*))
  "This invokes a low-level trace at the top of the block"
  (irc-append-basic-block function theblock)
  (irc-set-insert-point theblock)
  (dbg-set-current-debug-location-here)
  )


(defun irc-branch-to-and-begin-block (theblock)
  (irc-br theblock)
  (irc-begin-block theblock))

(defun irc-icmp-slt (lhs rhs &optional (name ""))
  (llvm-sys:create-icmp-slt *irbuilder* lhs rhs name))



(defun irc-icmp-ne (lhs rhs &optional (name ""))
  (llvm-sys:create-icmp-ne *irbuilder* lhs rhs name))

(defun irc-icmp-eq (lhs rhs &optional (name ""))
  (llvm-sys:create-icmp-eq *irbuilder* lhs rhs name))


(defun irc-cond-br (icond true false &optional branchWeights)
  (llvm-sys:create-cond-br *irbuilder* icond true false branchWeights))

(defun irc-prev-inst-terminator-inst-p ()
  (let ((cur-block (irc-get-insert-block)))
    (if cur-block
	(if (llvm-sys:basic-block-empty cur-block)
	    nil
	    (llvm-sys:terminator-inst-p (llvm-sys:basic-block-back cur-block)))
	nil)))
    


(defun irc-br (block)
  (unless block
    (error "Destination block is nil!!!"))
  (when (irc-prev-inst-terminator-inst-p)
    (error "About to create a second branch from ~a" (irc-get-insert-block)))
  (llvm-sys:create-br *irbuilder* block))


(defun irc-branch-if-no-terminator-inst (block)
  (when (not (irc-prev-inst-terminator-inst-p))
    (llvm-sys:create-br *irbuilder* block)))



(defun irc-add (lhs rhs &optional (label ""))
  (llvm-sys:create-add *irbuilder* lhs rhs label nil nil))




(defun irc-load (ptr &optional (label ""))
  (llvm-sys:create-load-value-twine *irbuilder* ptr label))

(Defun irc-store (val result &optional (label ""))
  (llvm-sys:create-store *irbuilder* val result nil))



(defun irc-phi (return-type num-reserved-values &optional (label "phi"))
  (llvm-sys:create-phi *irbuilder* return-type num-reserved-values label))

(defun irc-phi-add-incoming (phi-node value basic-block)
  (llvm-sys:add-incoming phi-node value basic-block))


(defun irc-unreachable ()
  (llvm-sys:create-unreachable *irbuilder*))


(defun irc-trunc (value type &optional (label "trunc"))
  (llvm-sys:create-trunc *irbuilder* value type label))



(defun irc-function-create (lisp-function-name body env &key (linkage 'llvm-sys:internal-linkage))
  "Returns the new function, the lexical environment for the function 
and the block that cleans up the function and rethrows exceptions,
followed by the traceid for this function and then the current insert block,
and then the irbuilder-alloca, and irbuilder-body"
  (let* ((llvm-function-name (jit-function-name lisp-function-name))
	 (fn (llvm-sys:function-create +fn-tmv*-afsp*+
				       linkage
				       llvm-function-name
				       *the-module*))
	 (func-env (make-function-container-environment env))
	 cleanup-block traceid
	 (irbuilder-cur (llvm-sys:make-irbuilder *llvm-context*))
	 (irbuilder-alloca (llvm-sys:make-irbuilder *llvm-context*))
	 (irbuilder-body (llvm-sys:make-irbuilder *llvm-context*))
	 )
    (let ((args (llvm-sys:get-argument-list fn)))
      (mapcar #'(lambda (arg argname) (llvm-sys:set-name arg argname))
	      (llvm-sys:get-argument-list fn) '("result-ptr" "activation-frame-ptr"))
      )
    (let ((bb (irc-basic-block-create "entry" fn)))
      (llvm-sys:set-insert-point-basic-block irbuilder-cur bb))
    ;; Setup exception handling and cleanup landing pad
    (irc-set-function-for-environment func-env fn)
    (with-irbuilder (func-env irbuilder-cur)
      (let* ((body (irc-basic-block-create "body" fn))
	     (entry-branch (irc-br body)))
	(llvm-sys:set-insert-point-instruction irbuilder-alloca entry-branch)
	(llvm-sys:set-insert-point-basic-block irbuilder-body body)))
    (irc-setup-cleanup-return-block func-env)
    (irc-setup-cleanup-landing-pad-block func-env) ;; used in irc-function-cleanup-and-return
    (setq cleanup-block (irc-setup-exception-handler-cleanup-block func-env)) ;; used in irc-function-cleanup-and-return
    (irc-setup-exception-handler-resume-block func-env)
    (irc-setup-terminate-landing-pad-block func-env)
    (setf-metadata func-env :cleanup ())
    (let ((*current-function* fn)
	  (exn.slot (irc-alloca-i8* func-env :irbuilder irbuilder-alloca :label "exn.slot"))
	  (ehselector.slot (irc-alloca-i32 func-env 0
					   :irbuilder irbuilder-alloca
					   :label "ehselector.slot")))
      (setf-metadata func-env :exn.slot exn.slot)
      (setf-metadata func-env :ehselector.slot ehselector.slot)
      (with-irbuilder (func-env irbuilder-body)
	(setq traceid (trace-enter-function-scope fn func-env body))))
    (values fn func-env cleanup-block traceid irbuilder-alloca irbuilder-body)))



#|(llvm-sys:create-in-bounds-gep *irbuilder* (llvm-sys:get-or-create-uniqued-string-global-variable *the-module* *current-function-name* (bformat nil ":::func-name-%s" *current-function-name*)) (list (jit-constant-i32 0) (jit-constant-i32 0)) "fn-name") 
|#


(defparameter *exception-handler-cleanup-block* nil)
(defparameter *exception-clause-types-to-handle* nil)

(defmacro with-new-function ((fn fn-env &key (function-name "function") parent-env function-form (linkage ''llvm-sys:internal-linkage)) &rest body)
  "Create a new function with {function-name} and {parent-env} - return the function"
  (let ((cleanup-block-gs (gensym "cleanup-block"))
	(traceid-gs (gensym "traceid"))
	(irbuilder-alloca (gensym))
	(irbuilder-body (gensym)))
    `(multiple-value-bind (,fn ,fn-env ,cleanup-block-gs ,traceid-gs ,irbuilder-alloca ,irbuilder-body)
	 (irc-function-create ,function-name ',function-form ,parent-env :linkage ,linkage)
       (let* ((*current-function* ,fn)
	      (*current-function-name* (llvm-sys:get-name ,fn))
	      (*irbuilder-function-alloca* ,irbuilder-alloca)
	      (*irbuilder-function-body* ,irbuilder-body))
	 (with-irbuilder (,fn-env *irbuilder-function-body*)
	   (with-dbg-function (,fn-env ,function-name
				       :linkage-name *current-function-name*
				       :function ,fn
				       :function-type +fn-tmv*-afsp*+
				       :form ,function-form )
	     (with-dbg-lexical-block (,fn-env ,function-form)
	       (let* ((*gv-current-function-name* (jit-make-global-string-ptr *current-function-name* "fn-name"))
		      (*exception-handler-cleanup-block* (irc-get-exception-handler-cleanup-block ,fn-env))
		      (*exception-clause-types-to-handle* nil))
		 (with-landing-pad (irc-get-cleanup-landing-pad-block ,fn-env)
		   ,@body
		   )
		 (with-landing-pad (irc-get-terminate-landing-pad-block ,fn-env)
		   (irc-function-cleanup-and-return ,fn-env ,traceid-gs))
		 ,fn)))))
       )))





(defun irc-push-cleanup (env cleanup-code)
  (multiple-value-bind (cleanup-cur found metadata-env)
      (lookup-metadata env :cleanup)
    (push-metadata metadata-env :cleanup cleanup-code)))


(defun irc-push-unwind (env unwind-code)
  "Push code that should be executed when this environment is left"
  (push-metadata env :unwind unwind-code))



(defmacro with-alloca-insert-point (env irbuilder
				    &key alloca init cleanup)
  "Switch to the alloca-insert-point and generate code to alloca a local variable.
Within the _irbuilder_ dynamic environment...
- insert the given alloca instruction using the provided irbuilder 
- insert the initialization code right after the alloca
- setup the :cleanup code for this alloca
- finally restore the insert-point to the end of the basic block that we entered this macro with."
  (let ((alloca-sym (gensym))
	(cleanup-gs (gensym))
	(found-gs (gensym))
	(metadata-env-gs (gensym)))
    `(with-irbuilder (,env ,irbuilder)
       (let ((,alloca-sym ,alloca))
	 (when ,init (funcall ,init ,alloca-sym))
	 (when ,cleanup
	   (multiple-value-bind (,cleanup-gs ,found-gs ,metadata-env-gs) (lookup-metadata env :cleanup)
	     (push-metadata ,metadata-env-gs :cleanup (funcall ,cleanup ,alloca-sym))))
	 ,alloca-sym))
    ))


(defmacro with-irbuilder ((env irbuilder) &rest code)
  "Set *irbuilder* to the given IRBuilder"
  `(let ((*irbuilder* ,irbuilder))
     (cmp-log "Switching to irbuilder --> %s\n" ',irbuilder)
     (prog1 (progn
	      ,@code)
       (cmp-log "Leaving irbuilder --> %s\n" ',irbuilder))
     ))


(defun irc-alloca-tmv (env &key (irbuilder *irbuilder-function-alloca*) (label ""))
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* +tmv+ (jit-constant-i32 1) label)
    :init (lambda (a) (irc-call env "newTmv" a))
    :cleanup (lambda (a) (list 'destructTmv a))))


(defun irc-alloca-tsp (env &key (irbuilder *irbuilder-function-alloca*) (label ""))
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* +tsp+ (jit-constant-i32 1) label)
    :init (lambda (a) (irc-call env "newTsp" a))
    :cleanup (lambda (a) (list 'destructTsp a))))

(defun irc-alloca-afsp (env &key (irbuilder *irbuilder-function-alloca*) (label ""))
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* +afsp+ (jit-constant-i32 1) label)
    :init (lambda (a) (irc-call env "newAFsp" a))
    :cleanup (lambda (a) (list 'destructAFsp a))))

(defconstant +make-value-frames-at-alloca-time+ nil)

(defun irc-alloca-afsp-value-frame-of-size (env size &key (irbuilder *irbuilder-function-alloca*) (label ""))
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* +afsp+ (jit-constant-i32 1) label)
    :init (lambda (a) (if +make-value-frames-at-alloca-time+
			  (irc-call env "newAFsp_ValueFrameOfSize" a (jit-constant-i32 size))
			  (irc-call env "newAFsp" a)
			  ))
    :cleanup (lambda (a) (list 'destructAFsp a))))

(defun irc-use-existing-value-frame (env result-af size)
  "If we +make-value-frames-at-alloca-time+ then we dont have
to make them within the body of the function"
  (unless +make-value-frames-at-alloca-time+
    (irc-call env "makeValueFrame" result-af (jit-constant-i32 size))))


(defun irc-alloca-i32-no-init (env &key (irbuilder *irbuilder-function-alloca*) (label "i32-"))
  "Allocate space for an i32"
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* +i32+ (jit-constant-i32 1) label)
    :init nil))


(defun irc-alloca-i32 (env init-val &key (irbuilder *irbuilder-function-alloca*) (label "i32-"))
  "Allocate space for an i32"
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* +i32+ (jit-constant-i32 1) label)
    :init (lambda (a) (irc-store (jit-constant-i32 init-val) a))))


(defun irc-alloca-i8* (env &key (irbuilder *irbuilder-function-alloca*) (label "i8*-"))
  "Allocate space for an i8*"
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* +i8*+ (jit-constant-i32 1) label)))


(defun irc-alloca-setjmp.buf (env &key (irbuilder *irbuilder-function-alloca*) (label "setjmp.buf"))
  "Allocate space for a setjmp.buf structure (5 words)"
  (with-alloca-insert-point env irbuilder
      :alloca (llvm-sys::create-alloca *irbuilder* +setjmp.buf+ (jit-constant-i32 1) label)))

(defun irc-setjmp.buf-set-jump-address ( env setjmp.buf address)
  (irc-call env "setjmp_set_jump_address" setjmp.buf address))

(defun irc-setjmp.buf-user0-set-i32 ( env setjmp.buf val)
  (irc-call env "setjmp_user0_set_i32" setjmp.buf (jit-constant-i32 val)))

(defun irc-setjmp.buf-user0-get-i32 ( env setjmp.buf)
  (irc-call env "setjmp_user0_get_i32" setjmp.buf))

(defun irc-setjmp.buf-user0-allocate-set-tmv ( env setjmp.buf tmv-val)
  (irc-call env "setjmp_user0_allocate_set_tmv" setjmp.buf tmv-val))

(defun irc-setjmp.buf-user0-get-tmv ( env result setjmp.buf)
  (irc-call env "setjmp_user0_get_tmv" result setjmp.buf))

(defun irc-setjmp.buf-user0-delete-tmv ( env setjmp.buf)
  (irc-call env "setjmp_user0_delete_tmv" setjmp.buf))







(defun irc-create-invoke (func args unwind-dest label)
  (unless unwind-dest (error "unwind-dest should not be nil"))
  (let ((normal-dest (irc-basic-block-create "normal-dest")))
    (unless normal-dest (error "normal-dest should not be nil"))
    (cmp-log "--------------- About to create-invoke -----------\n")
    (cmp-log "    Current basic-block: %s\n" (llvm-sys:get-name (llvm-sys:get-insert-block *irbuilder*)))
    (cmp-log "            Unwind dest: %s\n" (llvm-sys:get-name unwind-dest))
    (if (and unwind-dest (eq (llvm-sys:get-insert-block *irbuilder*) unwind-dest))
	(error "The unwind dest ~a should never be the same as the current block ~a"
	       (if unwind-dest
		   (llvm-sys:get-name unwind-dest)
		   "NIL")
	       (if (llvm-sys:get-insert-block *irbuilder*)
		   (llvm-sys:get-name (llvm-sys:get-insert-block *irbuilder*))
		   "NIL")))
    (let ((code (llvm-sys:create-invoke *irbuilder* func normal-dest unwind-dest args label)))
      (irc-begin-block normal-dest)
      (unless code (error "irc-create-invoke returning nil"))
      (when (llvm-sys:does-not-return func)
	(irc-unreachable)
	(irc-begin-block (irc-basic-block-create "from-invoke-that-never-returns")))
      code)))


(defun irc-create-call (func args label)
  (let ((a1 (car args))
	(a2 (cadr args))
	(a3 (caddr args))
	(a4 (cadddr args))
	(a5 (car (cddddr args))))
    (let ((code (case (length args)
		  (5 (llvm-sys:create-call5 *irbuilder* func a1 a2 a3 a4 a5 label))
		  (4 (llvm-sys:create-call4 *irbuilder* func a1 a2 a3 a4 label))
		  (3 (llvm-sys:create-call3 *irbuilder* func a1 a2 a3 label))
		  (2 (llvm-sys:create-call2 *irbuilder* func a1 a2 label))
		  (1 (llvm-sys:create-call1 *irbuilder* func a1 label))
		  (0 (llvm-sys:create-call0 *irbuilder* func label ))
		  (otherwise (error "illegal irc-call to ~a" func )))))
      (unless code (error "irc-create-call returning nil"))
      code)))


(defparameter *current-unwind-landing-pad-dest* nil)

(defmacro with-landing-pad (unwind-landing-pad-dest &rest body)
  `(progn
     (cmp-log "Setting *current-unwind-landing-pad-!dest* to %s\n" (llvm-sys:get-name ,unwind-landing-pad-dest))
     (let ((*current-unwind-landing-pad-dest* ,unwind-landing-pad-dest))
       ,@body
       )
     (cmp-log "<<<<< Restored *current-unwind-landing-pad-dest* to %s\n"
	      (if *current-unwind-landing-pad-dest*
		  (llvm-sys:get-name *current-unwind-landing-pad-dest*)
		  "NIL"))
     )
  )


                    
(defun irc-invoke-or-call (env func args label)
  "If env is within a lexical unwindable environment (unwind-protect or catch) 
then create a function invocation that unwinds to the unwindable environments unwind-dest.
Otherwise just create a function call"
  (if (llvm-sys:does-not-throw func)
      (irc-create-call func args label)
      (progn
	(unless *current-unwind-landing-pad-dest* (error "*current-unwind-landing-pad-dest* is nil"))
	(irc-create-invoke func args *current-unwind-landing-pad-dest* label))))



    

(defun irc-call (env function-name &rest args &aux (label ""))
  (let* ((func (get-function-or-error *the-module* function-name (car args)))
	 (last-arg (car (last args)))
	 (real-args args))
    (when (stringp last-arg)
      (setq real-args (nbutlast args))
      (setq label last-arg))
    (throw-if-mismatched-arguments function-name real-args)
;;    (mapc #'(lambda (x) (unless (or #|(not x)|# (llvm-sys:valuep x)) (error "All arguments for ~a must be llvm:Value types or nil but ~a isn't - you passed: ~a" function-name x real-args))) real-args)
    (let* ((args real-args)
	   (code (irc-invoke-or-call env func args label)))
      code)))




;; Helper functions





(defun irc-verify-function (fn &optional (continue t))
  (cmp-log "At top of irc-verify-function\n")
#||
  (if (is-debug-compiler-on)
      (llvm-sys:dump fn))
||#
  (let ((failed-verify (llvm-sys:verify-function fn 'llvm-sys:return-status-action)))
     (if failed-verify
	 (progn
	   (bformat t "!!!!!!!!!!! Function failed to verify !!!!!!!!!!!!!!!!!!!\n")
	   (bformat t "---------------- dumping function to assist in debugging\n")
	   (llvm-sys:dump fn)
	   (bformat t "!!!!!!!!!!! ------- see above ------- !!!!!!!!!!!!!!!!!!!\n")
	   (llvm-sys:verify-function fn 'llvm-sys:print-message-action)
	   (if continue
	       (break "Failed function verify - type c to keep going")
	       (error "Failed function verify")))
	 (cmp-log "--------------  Function verified OK!!!!!!!\n"))))


(defun get-function-or-error (module name &optional first-argument)
  "Return the function with (name) or throw an error.
If the *primitives* hashtable says that the function with (name) requires a first argument type indicated by
+tsp*-or-tmv*+ then use the first argument type to create a function-name prefixed with sp_ or mv_"
  (let ((primitive-entry (gethash name *primitives*)))
    (unless primitive-entry (error "Could not find function ~a in *primitives*" name))
    (let* ((required-first-argument-type (car (cadr primitive-entry)))
	   (dispatch-name (dispatch-function-name name (if (eq required-first-argument-type +tsp*-or-tmv*+)
							   (llvm-sys:get-type first-argument)
							   nil))))
      (let ((f (llvm-sys:get-function module dispatch-name)))
	(if (llvm-sys:valid f)
	    f
	    (error "Could not find function: ~a" dispatch-name))))))


(defun irc-global-symbol (sym env)
  "Return an llvm GlobalValue for a symbol"
  (compile-reference-to-symbol sym env)
  )


(defun irc-global-setf-symbol (sym env)
  "Return an llvm GlobalValue for a function name of the form (setf XXXX).
   Pass XXXX as the sym to this function."
  (compile-reference-to-symbol sym env)
  )






(defun irc-symbol-value-ref (env sym)
  "Return a reference to the symbol-value"
  (irc-call env "symbolValueReference" (irc-global-symbol sym env)))



(defun irc-environment-activation-frame (env)
  (if env
      (environment-activation-frame env)
      nil))


