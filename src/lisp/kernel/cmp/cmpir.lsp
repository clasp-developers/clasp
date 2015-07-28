;;
;;;    File: cmpir.lsp
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
;; Wrappers for llvm-ir instruction generation functions 
;;
;; All instructions talk to the special variable *irbuilder*
;;

(in-package :compiler)


(defun irc-next-environment-id ()
    (prog1 *next-environment-id*
      (incf *next-environment-id*)))

(defun irc-single-step-callback (env)
  (irc-intrinsic "singleStepCallback" ))


(defun irc-attach-debugging-info-to-value-frame (af symbol-list-designator env)
  (let ((symbol-names (cond
			((lambda-list-handler-p symbol-list-designator)
			 (names-of-lexical-variables-for-debugging symbol-list-designator))
			(t (error "Handle symbol-list-designator: ~a" symbol-list-designator)))))
    (when *debug-attach-debugging-info-to-value-frames*
      ;;    (break "About to codegen attach-debugging-info")
      (when symbol-names
	(let* ((ltv-idx (codegen-literal nil symbol-names env))
	       (ltv-ref (compile-reference-to-load-time-value ltv-idx env)))
	  (irc-intrinsic "attachDebuggingInfoToValueFrame" af ltv-ref))))))





(defun handle-exit-scope (scope-info env)
  (let ((scope-exit-fn (bformat nil "trace_exit%sScope" (cadr scope-info)))
	(scope-level (caddr scope-info))
	(scope-msg (cadddr scope-info)))
    (irc-intrinsic scope-exit-fn scope-level scope-msg)))

(defun irc-t ()
  (compile-reference-to-literal t nil))


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


(defun irc-gep (array indices &optional (name "gep"))
  (llvm-sys:create-in-bounds-gep *irbuilder* array indices name ))


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
	    (,exception-ptr (irc-intrinsic "__cxa_begin_catch" ,exn-gs)))
       ,@body
       (irc-intrinsic "__cxa_end_catch"))))


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
	  (irc-intrinsic "_Unwind_Resume" exn7)
	  (irc-unreachable)
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
  (irc-intrinsic "__cxa_rethrow")
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
  (or env (error "env must be supplied"))
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
  (let* ((classified (classify-variable env var)))
    (if classified
	classified
	(cons 'ext:special-var var))))
#|
	(if (special-p var)
	    (cons 'ext:special-var var)
	    (error "Could not find variable %s in lexical/global environment" var)))))
|#



(defun irc-new-unbound-function-value-environment (new-env &key number-of-functions (label "function-frame"))
  "Create a new function environment and a new runtime environment"
  (let* ((new-renv (irc-alloca-afsp new-env :label label)))
    (or new-renv (error "The new-renv is nil - it shouldn't be"))
    (irc-set-renv new-env new-renv)
    new-env))


(defun irc-new-function-value-environment (new-env &key functions (label "function-frame"))
  "Create a new function environment and a new runtime environment"
  (dolist (fn functions)
    (bind-function new-env (car fn) #'(lambda () (print "Dummy func"))))
  new-env)

#||
  (let ((new-env (irc-new-unbound-function-value-environment old-env :number-of-functions (length functions))))
||#

(defun irc-new-tagbody-environment (new-env &key (label "tagbody-frame"))
  "Create a new tagbody environment and a new runtime environment"
  (let* ((new-renv (irc-alloca-afsp new-env :label label)))
    (or new-renv (error "The new-renv is nil - it shouldn't be"))
    (irc-set-renv new-env new-renv)
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



(defun irc-new-unbound-value-environment-of-size (new-env size &key (label "value-frame"))
  "Create a new environment and a new runtime environment"
  (let* ((new-renv (irc-alloca-afsp-value-frame-of-size
                    new-env
                    size
                    :label label)))
    (or new-renv (error "The new-renv is nil - it shouldn't be"))
    (irc-set-renv new-env new-renv)
    new-env))

(defun irc-new-value-environment-of-size (new-env &key fill-runtime-form (label "env") )
  "Create a new environment and a new runtime environment"
  (or fill-runtime-form (error "You must provide a fill-runtime-form - if you want to fill later then use irc-new-unbound-value-environment"))
  (irc-new-unbound-value-environment-of-size new-env :label label )
  (when fill-runtime-form
    (funcall fill-runtime-form new-env))
  new-env)

(defun irc-new-catch-environment (old-env)
  (make-catch-environment old-env))


(defun irc-set-renv (env renv)
  (set-runtime-environment env renv))

(defun irc-renv (env)
  (let ((renv (runtime-environment (current-visible-environment env))))
    (if renv
	(progn
	  (cmp-log "Returning non-nil renv\n")
	  renv)
	(let ((nil-renv (compile-reference-to-literal nil env))) ;; (irc-intrinsic "activationFrameNil")))
	  (cmp-log "Returning nil renv: %s\n" nil-renv)
	  nil-renv))))

(defun irc-parent-renv (env)
  (let ((renv (runtime-environment (current-visible-environment (get-parent-environment env)))))
    (if renv
	(progn
	  (cmp-log "Returning non-nil renv\n")
	  renv)
	(let ((nil-renv (compile-reference-to-literal nil env))) ;; (irc-intrinsic "activationFrameNil")))
	  (cmp-log "Returning nil renv: %s\n" nil-renv)
	  nil-renv))))


(defun irc-i64-*current-source-pos-info*-filepos ()
  (jit-constant-i64 (core:source-pos-info-filepos *current-source-pos-info*)))

(defun irc-i32-*current-source-pos-info*-lineno ()
  (jit-constant-i32 (core:source-pos-info-lineno *current-source-pos-info*)))

(defun irc-i32-*current-source-pos-info*-column ()
  (jit-constant-i32 (core:source-pos-info-column *current-source-pos-info*)))




(defun irc-generate-terminate-code ()
      (let* ((personality-function (get-function-or-error *the-module* "__gxx_personality_v0"))
	     (landpad (llvm-sys:create-landing-pad *irbuilder* +exception-struct+ personality-function 1 "")))
	(llvm-sys:add-clause landpad (llvm-sys:constant-pointer-null-get +i8*+))
	(dbg-set-current-debug-location-here)
	(irc-low-level-trace)
	(irc-intrinsic "clasp_terminate" *gv-source-pathname* 
		       (irc-i32-*current-source-pos-info*-lineno) 
		       (irc-i32-*current-source-pos-info*-column) 
		       *gv-current-function-name* )
	(irc-unreachable)
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






(defun irc-make-unwind-protect-environment (cleanup-code parent-env)
  (let ((new-env (make-unwind-protect-environment cleanup-code parent-env)))
    new-env))


(defun irc-unwind-unwind-protect-environment (env)
  (let ((unwind-form (unwind-protect-environment-cleanup-form env))
	(unwind-result (irc-alloca-tsp env :label "unwind-result")))
    ;; Generate the unwind-form code in the parent environment of the unwind-protect form
    (codegen unwind-result unwind-form (get-parent-environment env))
    ))


(defun irc-do-unwind-environment (env)
  (cmp-log "irc-do-unwind-environment for: %s\n" env)
  (let ((unwind (local-metadata env :unwind)))
    (dolist (cc unwind)
      (let ((head (car cc)))
	(cond
	  ((eq head 'symbolValueRestore)
	   (cmp-log "popDynamicBinding of %s\n" (cadr cc))
	   (irc-intrinsic "popDynamicBinding" (irc-global-symbol (cadr cc) env)))
	  (t (error (bformat nil "Unknown cleanup code: %s" cc))))
	)))
  )

(defun irc-unwind-environment (env)
  (cmp-log "in irc-unwind-environment with: %s u-p-e?: %s\n" (type-of env) (unwind-protect-environment-p env))
  (when (unwind-protect-environment-p env)
    (irc-unwind-unwind-protect-environment env))
  (irc-do-unwind-environment env)
  )



(defun irc-unwind-into-environment (begin-env end-env)
  "Unwind the environments from begin-env to end-env"
  (do* ((cur-env begin-env (get-parent-environment cur-env)))
       ((eq cur-env end-env) (irc-unwind-environment cur-env))
    (irc-unwind-environment cur-env))
  )














    
  



#|
(defun irc-rename-insert-block (name)
  "Rename the current insertion block to something more useful for reading/debugging IR"
  (let ((current-block (llvm-sys:get-insert-block *irbuilder*)))
    (llvm-sys:set-name current-block name)))
|#

(defun irc-basic-block-create (name &optional function)
  "Create a llvm::BasicBlock with (name) in the (function)"
  (let ((bb (llvm-sys:basic-block-create *llvm-context* (bformat nil "%s%s" *block-name-prefix* name) function)))
    (cmp-log "Created basic block <*block-name-prefix* = %s>  <name=%s>: %s\n" *block-name-prefix* name bb)
    bb))

(defun irc-get-insert-block ()
  (llvm-sys:get-insert-block *irbuilder*))

(defun irc-append-basic-block (function theblock)
  "Append the basic block to the _function_. If the _function_ is not passed then use the current function"
  (llvm-sys:append-basic-block function theblock))

(defun irc-set-insert-point (theblock)
  "Set the current insert point"
  (llvm-sys:set-insert-point-basic-block *irbuilder* theblock))



;;  "Control if low-level block tracing is on or off"
;;
;;
;;  You can do things like:
;; Put (push :flow *features*) / (pop *features*)
;;   around a function and it will get low-level-trace commands inserted before
;;   every function call and within every landing pad.

(defparameter *next-low-level-trace-index* 1000000001)
(defun irc-low-level-trace (&optional where)
  (if (member where *features*)
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

(defun irc-ret-void ()
  (llvm-sys:create-ret-void *irbuilder*))

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




(defun irc-load (source &optional (label ""))
  (llvm-sys:create-load-value-twine *irbuilder* source label))

(defun irc-store (val destination &optional (label ""))
  (llvm-sys:create-store *irbuilder* val destination nil))



(defun irc-phi (return-type num-reserved-values &optional (label "phi"))
  (llvm-sys:create-phi *irbuilder* return-type num-reserved-values label))

(defun irc-phi-add-incoming (phi-node value basic-block)
  (llvm-sys:add-incoming phi-node value basic-block))


(defun irc-unreachable ()
  (irc-intrinsic "unreachableError" )
  (llvm-sys:create-unreachable *irbuilder*))


(defun irc-trunc (value type &optional (label "trunc"))
  (llvm-sys:create-trunc *irbuilder* value type label))





#|(llvm-sys:create-in-bounds-gep *irbuilder* (llvm-sys:get-or-create-uniqued-string-global-variable *the-module* *current-function-name* (bformat nil ":::func-name-%s" *current-function-name*)) (list (jit-constant-i32 0) (jit-constant-i32 0)) "fn-name") 
|#


(defparameter *exception-handler-cleanup-block* nil)
(defparameter *exception-clause-types-to-handle* nil)

(defmacro with-new-function (( ;; FN is bound to the function being created
			      fn
			      ;; FN-ENV is bound to the function environment
			      fn-env
			      &key
			      ;; The function name can be a symbol or a string.
			      ;; if its a string it is used unchanged
			      ;; if its a symbol then it is mangled by appending "FN-SYMB." to it
			      ;; if its a cons of the form (setf ...) then its mangled by appending FN-SETF. to it
			      (function-name "function")
			      ;; Specify the LLVM type of the function
			      (function-type '+fn-prototype+ function-type-p)
			      ;; List the names of the arguments - this must match what
			      ;; is passed to the the :FUNCTION-TYPE keyword above
			      (argument-names '+fn-prototype-argument-names+ argument-names-p)
			      ;; This is the parent environment of the new function environment that
			      ;; will be returned
			      parent-env
			      ;; This is the form that will be compiled as the function code
			      function-form
			      ;; This is the LLVM linkage - DONT USE "private" linkage - I encountered some
			      ;; pretty subtle bugs with exception handling (I think) when I did that.
			      ;; I currently use llvm-sys:internal-linkage or llvm-sys:external-linkage
			      (linkage ''llvm-sys:internal-linkage)
			      )
			     &rest body)
  "Create a new function with {function-name} and {parent-env} - return the function"
  (cmp-log "Expanding with-new-function name: %s\n" function-name)
  (let ((cleanup-block-gs (gensym "cleanup-block"))
	(traceid-gs (gensym "traceid"))
	(irbuilder-alloca (gensym))
	(irbuilder-body (gensym)))
    `(multiple-value-bind (,fn ,fn-env ,cleanup-block-gs #| ,traceid-gs |# ,irbuilder-alloca ,irbuilder-body)
	 (irc-function-create ,function-name ',function-form ,parent-env
			      :function-type ,function-type
			      :argument-names ,argument-names
			      :linkage ,linkage)
;;       (format t "cmpir.lsp:660 entering with-new-function~%")
       (let* ((*current-function* ,fn)
	      (*current-function-name* (llvm-sys:get-name ,fn))
	      (*irbuilder-function-alloca* ,irbuilder-alloca)
	      (*irbuilder-function-body* ,irbuilder-body))
         (with-dbg-function (,function-name
                             :linkage-name *current-function-name*
                             :function ,fn
                             :function-type ,function-type
                             :form ,function-form )
           (with-dbg-lexical-block (,function-form)
;;             (format t "cmpir.lsp 670   Setting current-source-pos~%")
             (dbg-set-current-source-pos-for-irbuilder ,function-form ,irbuilder-alloca)
;;             (format t "cmpir.lsp 673   Done setting alloca current-source-pos~%")
             (dbg-set-current-source-pos-for-irbuilder ,function-form ,irbuilder-body)
;;             (format t "cmpir.lsp 676   Done setting body current-source-pos~%")
;;             (format t "cmpir.lsp 678   Done setting current-source-pos~%")
             (with-irbuilder (*irbuilder-function-body*)
	       (or *the-module* (error "with-new-function *the-module* is NIL"))
	       (let* ((*gv-current-function-name* (jit-make-global-string-ptr *current-function-name* "fn-name"))
		      (*exception-handler-cleanup-block* (irc-get-exception-handler-cleanup-block ,fn-env))
		      (*exception-clause-types-to-handle* nil))
                 (cmp-log "with-landing-pad around body\n")
		 (with-landing-pad (irc-get-cleanup-landing-pad-block ,fn-env)
		   ,@body
		   )
                 (cmp-log "with-landing-pad around irc-function-cleanup-and-return\n")
;;                 (format t "cmpir.lsp:660 leaving with-new-function~%")
		 (with-landing-pad (irc-get-terminate-landing-pad-block ,fn-env)
		   (irc-function-cleanup-and-return ,fn-env #||,*current-invocation-history-frame*||#))
		 ,fn)))))
       )))



(defun irc-function-create (lisp-function-name body env
			    &key (function-type +fn-prototype+ function-type-p)
			      ;; If the first argument is NOT meant to be a returned structure then set this to nil
			      (first-argument-struct-ret t)
			      (argument-names '("result-ptr" "activation-frame-ptr") argument-names-p)
			      (linkage 'llvm-sys:internal-linkage))
  "Returns the new function, the lexical environment for the function 
and the block that cleans up the function and rethrows exceptions,
followed by the traceid for this function and then the current insert block,
and then the irbuilder-alloca, irbuilder-body."
  (when (or function-type-p argument-names-p)
    (when (not (and function-type-p argument-names-p))
      (error "If you provide one of function-type or argument-names you must provide both")))
  (let* ((llvm-function-name (jit-function-name lisp-function-name))
	 (fn (llvm-sys:function-create function-type
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
	      (llvm-sys:get-argument-list fn) argument-names)
      ;; Set the first argument attribute to be sret
      (if (and args first-argument-struct-ret)
	  (let ((attribute-set (llvm-sys:attribute-set-get *llvm-context* 1 (list 'llvm-sys:attribute-struct-ret))))
	    (llvm-sys:add-attr (first args) attribute-set))))
    (let ((bb (irc-basic-block-create "entry" fn)))
      (llvm-sys:set-insert-point-basic-block irbuilder-cur bb))
    ;; Setup exception handling and cleanup landing pad
    (irc-set-function-for-environment func-env fn)
    (with-irbuilder (irbuilder-cur)
      (let* ((body-block (irc-basic-block-create "body" fn))
	     (entry-branch (irc-br body-block)))
	(llvm-sys:set-insert-point-instruction irbuilder-alloca entry-branch)
	(llvm-sys:set-insert-point-basic-block irbuilder-body body-block)))
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
      (values fn func-env cleanup-block irbuilder-alloca irbuilder-body))))






(defun irc-function-cleanup-and-return (env)
  (when env
    (let ((return-block (irc-basic-block-create "return-block")))
      (irc-br return-block)
      (irc-begin-landing-pad-block (irc-get-cleanup-landing-pad-block env)
				   (irc-get-function-for-environment env))
      (let* ((personality-function (get-function-or-error *the-module* "__gxx_personality_v0"))
	     (landpad (llvm-sys:create-landing-pad *irbuilder*
						   +exception-struct+
						   personality-function 0 "")))
	(llvm-sys:set-cleanup landpad t)
	(dbg-set-current-debug-location-here)
	(irc-low-level-trace)
	(multiple-value-bind (exn.slot ehselector.slot)
	    (irc-preserve-exception-info env landpad)
	  (irc-branch-to-and-begin-block (irc-get-exception-handler-cleanup-block env))
	  (with-landing-pad (irc-get-terminate-landing-pad-block env)
	    (irc-cleanup-function-environment env #| invocation-history-frame |# ))
	  (irc-branch-to-and-begin-block (irc-get-exception-handler-resume-block env))
	  (irc-generate-resume-code exn.slot ehselector.slot env))
	(irc-begin-landing-pad-block (irc-get-terminate-landing-pad-block env))
	(irc-generate-terminate-code)
	;; put the return-block at the end of the function to see if that fixes exception handling problem
	(progn
	  (irc-begin-block return-block)
	  (irc-cleanup-function-environment env #| invocation-history-frame |# ) ;; Why the hell was this commented out?
          #|	  (irc-cleanup-function-environment env invocation-history-frame )  |#
	  (llvm-sys:create-ret-void *irbuilder*))
	(cmp-log "About to verify the function in irc-function-cleanup-and-return\n")
	(irc-verify-function *current-function*)))))


(defun irc-cleanup-function-environment (env #||invocation-history-frame||#)
  "Generate the code to cleanup the environment"
  (if env
      (progn
;;	(dbg-pop-invocation-history-stack)
	(irc-do-unwind-environment env)
	(let ((cleanup (local-metadata env :cleanup)))
	  ;;      (cmp-log "Cleaning up env: %s\n" env)
	  (cmp-log "About to cleanup local-metadata :cleanup --> %s\n" cleanup)
	  (dolist (cc cleanup)
	    (let ((h (car cc)))
	      (cond
		((functionp h) (funcall h (cadr cc)))
		((null h) (bformat t "Cleanup code of NIL!!!!!\n"))
		((eq h 'destructTsp) (irc-dtor "destructTsp" (cadr cc)))
		((eq h 'destructTmv) (irc-dtor "destructTmv" (cadr cc)))
		((eq h 'destructAFsp) (irc-dtor "destructAFsp" (cadr cc)))
		((eq h 'exit-lexical-scope) (handle-exit-scope cc env))
		((stringp h) (irc-intrinsic h (cadr cc)))
		(t (break (bformat nil "Unknown cleanup code: %s" h))))
	      ))))))


(defun irc-dtor (name obj)
  (declare (special *compiler-suppress-dtors*))
  (unless *compiler-suppress-dtors* (irc-intrinsic name obj))
  )








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
    `(with-irbuilder (,irbuilder)
       (let ((,alloca-sym ,alloca))
	 (when ,init (funcall ,init ,alloca-sym))
	 (when ,cleanup
	   (multiple-value-bind (,cleanup-gs ,found-gs ,metadata-env-gs) (lookup-metadata env :cleanup)
	     (push-metadata ,metadata-env-gs :cleanup (list ,cleanup ,alloca-sym))
	     ))
	 ,alloca-sym))
    ))


(defmacro with-irbuilder ((irbuilder) &rest code)
  "Set *irbuilder* to the given IRBuilder"
  (let ((irbuilder-desc (gensym)))
    `(let ((*irbuilder* ,irbuilder))
       (cmp-log "Switching to irbuilder --> %s\n" (bformat nil "%s" ,irbuilder))
       (prog1 (progn
                ,@code)
         (cmp-log "Leaving irbuilder --> %s\n" (bformat nil "%s" ,irbuilder))))))

(defun irc-alloca-tsp[MV] (env &key (irbuilder *irbuilder-function-alloca*) (label ""))
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* +tsp[MV]+ (jit-constant-i32 1) label)
    :init nil
    :cleanup nil))

(defun irc-alloca-tmv (env &key (irbuilder *irbuilder-function-alloca*) (label ""))
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* +tmv+ (jit-constant-i32 1) label)
    :init (lambda (a) (irc-intrinsic "newTmv" a))
    :cleanup (lambda (a) (irc-dtor "destructTmv" a))))

(defun irc-alloca-tsp-array (env &key (num 1) (irbuilder *irbuilder-function-alloca*) (label ""))
  (cmp-log "irc-alloca-tsp label: %s for %s\n" label irbuilder)
  (with-alloca-insert-point
      env irbuilder
      :alloca (llvm-sys::create-alloca *irbuilder* +tsp+ (jit-constant-i32 num) label)
      :init (lambda (a)
	      (dotimes (i num)
		(irc-intrinsic "newTsp" (irc-gep a (list (jit-constant-i32 i))))))
      :cleanup (lambda (a)
		 (dotimes (i num)
		   (irc-dtor "destructTsp" (irc-gep a (list (jit-constant-i32 i))))))))

(defun irc-alloca-tsp-variable-array (env &key num (irbuilder *irbuilder-function-alloca*) (label ""))
  (or num (error ":num keyword argument must be passed"))
  (cmp-log "irc-alloca-tsp label: %s for %s\n" label irbuilder)
  (with-alloca-insert-point
      env irbuilder
      :alloca (llvm-sys::create-alloca *irbuilder* +tsp+ num label)
      :init (lambda (a)
	      (dotimes (i num)
		(irc-intrinsic "newTsp" (irc-gep a (list (jit-constant-i32 i))))))
      :cleanup (lambda (a)
		 (dotimes (i num)
		   (irc-dtor "destructTsp" (irc-gep a (list (jit-constant-i32 i))))))))


(defun irc-alloca-t* (env &key (irbuilder *irbuilder-function-alloca*) (label ""))
  "Allocate a T_O* on the stack"
  (with-alloca-insert-point
      env irbuilder
      :alloca (llvm-sys:create-alloca *irbuilder* +t*+ (jit-constant-i32 1) label)
      :init (lambda (a))
      :cleanup (lambda (a))))


(defun irc-alloca-tsp (env &key (irbuilder *irbuilder-function-alloca*) (label ""))
  (cmp-log "irc-alloca-tsp label: %s for %s\n" label irbuilder)
  (with-alloca-insert-point
      env irbuilder
      :alloca (llvm-sys::create-alloca *irbuilder* +tsp+ (jit-constant-i32 1) label)
      :init (lambda (a)) ;; (irc-intrinsic "newTsp" a))
      :cleanup (lambda (a) )))  ;;irc-dtor "destructTsp" a))))

(defun irc-alloca-Function_sp (env &key (irbuilder *irbuilder-function-alloca*) (label ""))
  (cmp-log "irc-alloca-Function_sp label: %s for %s\n" label irbuilder)
  (with-alloca-insert-point
      env irbuilder
      :alloca (llvm-sys::create-alloca *irbuilder* +Function_sp+ (jit-constant-i32 1) label)
      :init (lambda (a) );;(irc-intrinsic "newFunction_sp" a))
      :cleanup (lambda (a))));; (irc-dtor "destructFunction_sp" a))))

(defun irc-alloca-afsp (env &key (irbuilder *irbuilder-function-alloca*) (label ""))
  (cmp-log "irc-alloca-afsp label: %s for %s\n" label irbuilder)
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* +afsp+ (jit-constant-i32 1) label)
    :init (lambda (a) );;(irc-intrinsic "newAFsp" a))
    :cleanup (lambda (a)))); (irc-dtor "destructAFsp" a))))

(defun irc-alloca-afsp-value-frame-of-size (env size &key (irbuilder *irbuilder-function-alloca*) (label ""))
  (cmp-log "irc-alloca-afsp-value-frame-of-size label: %s for %s\n" label irbuilder)
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* +afsp+ (jit-constant-i32 1) label)
    :init (lambda (a) ); (irc-intrinsic "newAFsp" a))
    :cleanup (lambda (a)))); (irc-dtor "destructAFsp" a))))

(defun irc-make-value-frame (result-af size)
  (irc-intrinsic "makeValueFrame" result-af (jit-constant-i32 size) (jit-constant-i32 (irc-next-environment-id))))

(defun irc-make-tagbody-frame (env result-af)
  (irc-intrinsic "makeTagbodyFrame" result-af))


(defun irc-alloca-i32-no-init (env &key (irbuilder *irbuilder-function-alloca*) (label "i32-"))
  "Allocate space for an i32"
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* +i32+ (jit-constant-i32 1) label)
    :init nil))

(defun irc-alloca-i8 (env init-val &key (irbuilder *irbuilder-function-alloca*) (label "i8-"))
  "Allocate space for an i8"
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* +i8+ (jit-constant-i32 1) label)
    :init (lambda (a) (irc-store (jit-constant-i8 init-val) a))))


(defun irc-alloca-i32 (env init-val &key (irbuilder *irbuilder-function-alloca*) (label "i32-"))
  "Allocate space for an i32"
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* +i32+ (jit-constant-i32 1) label)
    :init (lambda (a) (irc-store (jit-constant-i32 init-val) a))))

(defun irc-alloca-size_t (env init-val &key (irbuilder *irbuilder-function-alloca*) (label "size_t-"))
  "Allocate space for an size_t"
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* +size_t+ (jit-constant-size_t 1) label)
    :init (lambda (a) (irc-store (jit-constant-size_t init-val) a))))


(defun irc-alloca-i8* (env &key (irbuilder *irbuilder-function-alloca*) (label "i8*-"))
  "Allocate space for an i8*"
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* +i8*+ (jit-constant-i32 1) label)))


(defun irc-allocal-lisp-compiled-function-ihf (env &key (irbuilder *irbuilder-function-alloca*) (label "ihf"))
  "Allocate space for a LispCompiledFunctionIHF structure"
  (with-alloca-insert-point env irbuilder
			    :alloca (llvm-sys:create-alloca *irbuilder* +LispCompiledFunctionIHF+ (jit-constant-i32 1) label)))




; ----------------------------------------------------------------------
(defun null-tsp ()
  (llvm-sys:constant-struct-get +tsp+ (list (llvm-sys:constant-pointer-null-get +t*+))))

(defun null-t-ptr ()
  (llvm-sys:constant-pointer-null-get +t*+))

;----------------------------------------------------------------------

(defun irc-store-multiple-values (offset values)
  "When passing more arguments than can be passed in registers the extra arguments
are written into the current multiple-valles array offset by the number of arguments
that are passed in registers.
Write T_O* pointers into the current multiple-values array starting at the (offset)"
  (let ((multiple-values-array (irc-intrinsic "getMultipleValues" (jit-constant-i32 offset))))
    (do* ((idx 0 (1+ idx))
	  (values values (cdr values))
	  (value (car values) (car values)))
	 ((null values) nil)
      (let ((ptr (llvm-sys:create-geparray *irbuilder* multiple-values-array (list (cmp:jit-constant-i32 0) (cmp:jit-constant-i32 idx) #||(cmp:jit-constant-i32 0)||# ) "idx")))
	(irc-store value ptr)))
    multiple-values-array))
	
	
(defun irc-struct-gep (struct idx &optional (label ""))
  (llvm-sys:create-struct-gep *irbuilder* struct idx label ))

(defun irc-insert-value (struct val idx-list &optional (label ""))
  (llvm-sys:create-insert-value *irbuilder* struct val idx-list label))

(defun irc-set-smart-ptr (tsp-val t-ptr-val)
  (irc-insert-value tsp-val t-ptr-val (list 0)))

(defun irc-extract-value (struct idx-list &optional (label ""))
  (llvm-sys:create-extract-value *irbuilder* struct idx-list label))

(defun irc-smart-ptr-extract (smart-ptr)
  "Extract the t-ptr from the smart-ptr"
  (irc-extract-value smart-ptr (list 0)))

(defun irc-funcall (result closure args &optional (label ""))
  (let* ((nargs (length args))
         (real-args (make-array core:+number-of-fixed-arguments+ :initial-element (null-t-ptr))))
    (let ((extra-args (do* ((idx 0 (1+ idx))
			    (arg-list args (cdr arg-list))
			    (arg (car arg-list) (car arg-list)))
			   ((or (null arg-list) (>= idx core:+number-of-fixed-arguments+)) arg-list)
			(setf (aref real-args idx) arg))))
      (when extra-args
	(irc-store-multiple-values core:+number-of-fixed-arguments+ extra-args))
      (let ((real-args-list (map 'list (lambda (x) x) real-args)))
	(irc-intrinsic-args "FUNCALL" (list* result closure (jit-constant-i32 nargs) real-args-list) :label label)))))
  
;----------------------------------------------------------------------




(defun irc-create-invoke (function-name args unwind-dest &optional (label ""))
  (check-debug-info-setup *irbuilder*)
  (unless unwind-dest (error "unwind-dest should not be nil"))
  (let ((func (get-function-or-error *the-module* function-name (car args)))
	(normal-dest (irc-basic-block-create "normal-dest")))
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


(defun irc-create-call (function-name args &optional (label ""))
  (check-debug-info-setup *irbuilder*)
  (let* ((func (get-function-or-error *the-module* function-name (car args)))
	 (ra args)
         (code (case (length args)
                 (0 (llvm-sys:create-call-array-ref *irbuilder* func nil label ))
                 (1 (llvm-sys:create-call1 *irbuilder* func (pop ra) label))
                 (2 (llvm-sys:create-call2 *irbuilder* func (pop ra) (pop ra) label))
                 (3 (llvm-sys:create-call3 *irbuilder* func (pop ra) (pop ra) (pop ra) label))
                 (4 (llvm-sys:create-call4 *irbuilder* func (pop ra) (pop ra) (pop ra) (pop ra) label))
                 (5 (llvm-sys:create-call5 *irbuilder* func (pop ra) (pop ra) (pop ra) (pop ra) (pop ra) label))
                 (otherwise
		  (llvm-sys:create-call-array-ref *irbuilder* func ra label)))))
    (unless code (error "irc-create-call returning nil"))
    code))


(defparameter *current-unwind-landing-pad-dest* nil)

(defmacro with-landing-pad (unwind-landing-pad-dest &rest body)
  `(progn
     (cmp-log "Setting *current-unwind-landing-pad-!dest* to %s\n" (llvm-sys:get-name ,unwind-landing-pad-dest))
     (let ((*current-unwind-landing-pad-dest* ,unwind-landing-pad-dest))
       ,@body
       )))


                    
(defun irc-invoke-or-call (function-name args label)
  "If env is within a lexical unwindable environment (unwind-protect or catch) 
then create a function invocation that unwinds to the unwindable environments unwind-dest.
Otherwise just create a function call"
  (let ((func (get-function-or-error *the-module* function-name (car args))))
    (or func (error "Could not get function ~a" function-name))
    (if (or (llvm-sys:does-not-throw func) (null *current-unwind-landing-pad-dest*))
	(irc-create-call function-name args label)
	(irc-create-invoke function-name args *current-unwind-landing-pad-dest* label))))


(defun irc-intrinsic-args (function-name args &key (label "") suppress-arg-type-checking)
  (let* ((last-arg (car (last args)))
	 (real-args args))
    (when (stringp last-arg)
      (setq real-args (nbutlast args))
      (setq label last-arg))
    (if (not suppress-arg-type-checking)
        (throw-if-mismatched-arguments function-name real-args))
;;    (mapc #'(lambda (x) (unless (or #|(not x)|# (llvm-sys:valuep x)) (error "All arguments for ~a must be llvm:Value types or nil but ~a isn't - you passed: ~a" function-name x real-args))) real-args)
    (let* ((args real-args)
	   (code (irc-invoke-or-call function-name args label)))
      code)))

(defun irc-intrinsic (function-name &rest args &aux (label ""))
  (irc-intrinsic-args function-name args :label label))




;; Helper functions





(defun irc-verify-function (fn &optional (continue t))
  (cmp-log "At top of irc-verify-function  ---- about to verify-function - if there is a problem it will not return\n")
  (multiple-value-bind (failed-verify error-msg)
      (llvm-sys:verify-function fn)
    (if failed-verify
        (progn
          (bformat t "!!!!!!!!!!! Function in module failed to verify !!!!!!!!!!!!!!!!!!!\n")
          (bformat t "---------------- dumping function to assist in debugging\n")
          (llvm-sys:dump fn)
          (bformat t "!!!!!!!!!!! ------- see above ------- !!!!!!!!!!!!!!!!!!!\n")
          (bformat t "llvm::verifyFunction error[%s]\n" error-msg)
          (if continue
	      (break "Error when trying to verify-function")
              (error "Failed function verify")))
        (cmp-log "--------------  Function verified OK!!!!!!!\n"))))


(defun get-function-or-error (module name &optional first-argument)
  "Return the function with (name) or throw an error.
If the *primitives* hashtable says that the function with (name) requires a first argument type indicated by
+tsp*-or-tmv*+ then use the first argument type to create a function-name prefixed with sp_ or mv_"
  (let ((primitive-entry (gethash name *primitives*)))
    (unless primitive-entry (error "Could not find function ~a in *primitives*" name))
    (let* ((required-first-argument-type (car (cadr primitive-entry)))
	   (dispatch-name (dispatch-function-name name (if (and first-argument (equal required-first-argument-type +tsp*-or-tmv*+))
							   (llvm-sys:get-type first-argument)
							   nil))))
      (let ((f (llvm-sys:get-function module dispatch-name)))
        (or f (error "Could not llvm-sys:get-function ~a with name: ~a" dispatch-name name))
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
  (irc-intrinsic "symbolValueReference" (irc-global-symbol sym env)))



(defun irc-environment-activation-frame (env)
  (if env
      (environment-activation-frame env)
      nil))
