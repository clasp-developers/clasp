
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


(defun irc-single-step-callback (env)
  (irc-intrinsic "singleStepCallback" ))


(defun handle-exit-scope (scope-info env)
  (let ((scope-exit-fn (bformat nil "trace_exit%sScope" (cadr scope-info)))
	(scope-level (caddr scope-info))
	(scope-msg (cadddr scope-info)))
    (irc-intrinsic scope-exit-fn scope-level scope-msg)))

(defun irc-lexical-function-lookup (classified start-env)
  (let* ((depth (third classified))
         (index (fourth classified))
         (function-env (fifth classified))
         (start-renv (irc-load (irc-renv start-env)))
         (instruction (irc-intrinsic "va_lexicalFunction"
                                     (jit-constant-size_t depth)
                                     (jit-constant-size_t index)
                                     start-renv)))
    #+optimize-bclasp
    (push (make-lexical-function-reference :instruction instruction
                                           :depth depth
                                           :index index
                                           :start-env start-env
                                           :start-renv start-renv
                                           :function-env function-env)
          *lexical-function-references*)
    instruction))

(defun irc-personality-function ()
  (get-or-declare-function-or-error *the-module* "__gxx_personality_v0"))

(defun irc-set-cleanup (landpad val)
  (llvm-sys:set-cleanup landpad val))


(defun irc-create-landing-pad (num-clauses &optional (name ""))
    (llvm-sys:create-landing-pad *irbuilder* %exception-struct% num-clauses name))

(defun irc-add-clause (landpad type)
  (llvm-sys:add-clause landpad type))

(defun irc-switch (go-value default-block num-cases)
  (llvm-sys:create-switch *irbuilder* go-value default-block num-cases nil nil))

(defun irc-add-case (switch val block)
  (llvm-sys:add-case switch val block))


(defun irc-gep (array indices &optional (name "gep"))
  (let ((indices (mapcar (lambda (x) (if (fixnump x) (jit-constant-intptr_t x) x)) indices)))
    (llvm-sys:create-in-bounds-gep *irbuilder* array indices name )))

(defun irc-in-bounds-gep-type (type value indices &optional (label "gep"))
  (llvm-sys:create-in-bounds-geptype *irbuilder* type value indices label))

(defun irc-exception-typeid** (name)
  (exception-typeid**-from-name name))

(defun irc-exception-typeid* (name)
  (exception-typeid*-from-name name))


#+(or)
(defmacro with-catch ((exn.slot exception-ptr) &rest body)
  (let ((exn-gs (gensym)))
    `(let* ((,exn-gs (llvm-sys:create-load-value-twine *irbuilder* ,exn.slot "exn"))
	    (,exception-ptr (irc-intrinsic "__cxa_begin_catch" ,exn-gs)))
       ,@body
       (irc-intrinsic "__cxa_end_catch"))))

#+(or)
(defmacro with-catch ((exn.slot exception-ptr) &rest body)
  (let ((exn-gs (gensym)))
    `(let ((,exn-gs (llvm-sys:create-load-value-twine *irbuilder* ,exn.slot "exn")))
       (unwind-protect
            (let ((,exception-ptr (irc-intrinsic "__cxa_begin_catch" ,exn-gs)))
              ,@body)
         (irc-intrinsic "__cxa_end_catch")))))



#|
(defun irc-save-exception-info (env lpad)
  (let ((exception-structure (llvm-sys:create-extract-value *irbuilder* lpad (list 0) "")))
    (llvm-sys:create-store *irbuilder* exception-structure (irc-function-exn.slot env) nil))
  (let ((exception-selector (llvm-sys:create-extract-value *irbuilder* lpad (list 1) "")))
    (llvm-sys:create-store *irbuilder* exception-selector (irc-function-ehselector.slot env) nil)))
|#

(defparameter *use-unwind-resume* nil)





(defun irc-generate-resume-code (exn.slot ehselector.slot env)
  (let ((exn7 (llvm-sys:create-load-value-twine *irbuilder* exn.slot "exn7")))
    (if *use-unwind-resume*
	(progn
	  (irc-intrinsic "_Unwind_Resume" exn7)
	  (irc-unreachable))
	(let ((sel (llvm-sys:create-load-value-twine *irbuilder* ehselector.slot "sel")))
	  (let* ((undef (llvm-sys:undef-value-get %exception-struct% ))
		 (lpad.val (llvm-sys:create-insert-value *irbuilder*
							 undef exn7 '(0) "lpad.val")))
	    (debug-print-i32 90)
	    (let ((lpad.val8 (llvm-sys:create-insert-value *irbuilder*
							   lpad.val sel '(1) "lpad.val8")))
	      (debug-print-i32 91)
	      (llvm-sys:create-resume *irbuilder* lpad.val8)))))))

(defun irc-rethrow (env)
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




(defun irc-new-unbound-function-value-environment (old-env &key number-of-functions (label "function-frame"))
  "Create a new function environment and a new runtime environment"
  (let* ((new-env (make-function-value-environment number-of-functions old-env))
	 (new-renv (irc-alloca-af* new-env :label label)))
    (or new-renv (error "The new-renv is nil - it shouldn't be"))
    (irc-set-renv new-env new-renv)
    new-env))


(defun irc-new-function-value-environment (old-env &key functions (label "function-frame"))
  "Create a new function environment and a new runtime environment"
  (let ((new-env (irc-new-unbound-function-value-environment old-env :number-of-functions (length functions))))
    (dolist (fn functions)
      (bind-function new-env (car fn) #'(lambda () (print "Dummy func"))))
    new-env))



(defun irc-new-unbound-tagbody-environment (old-env &key (label "tagbody-frame"))
  "Create a new tagbody environment and a new runtime environment"
  (let* ((new-env (make-tagbody-environment old-env))
	 (new-renv (irc-alloca-af* new-env :label label)))
    (or new-renv (error "The new-renv is nil - it shouldn't be"))
    (irc-set-renv new-env new-renv)
    new-env))


(defun irc-new-tagbody-environment (old-env &key (label "function-frame"))
  "Create a new tagbody environment and a new runtime environment"
  (let ((new-env (irc-new-unbound-tagbody-environment old-env )))
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
	 (new-renv (irc-alloca-af* new-env :label label)))
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
	 (new-renv (irc-alloca-af*-value-frame-of-size new-env number-of-arguments :label label)))
    (or new-renv (error "The new-renv is nil - it shouldn't be"))
    (irc-set-renv new-env new-renv)
    new-env))


#+(or)
(defun irc-new-value-environment-of-size (old-env &key number-of-arguments fill-runtime-form (label "env") )
  "Create a new environment and a new runtime environment"
  (or fill-runtime-form (error "You must provide a fill-runtime-form - if you want to fill later then use irc-new-unbound-value-environment"))
  (let ((new-env (irc-new-unbound-value-environment-of-size old-env
							    :number-of-arguments number-of-arguments
							    :label label )))
    (when fill-runtime-form
      (funcall fill-runtime-form new-env))
    new-env))



(defun irc-make-block-environment-set-parent (name parent-env)
  (let* ((block-env (make-block-environment name parent-env))
         (new-renv (irc-alloca-af* block-env :label "block-renv"))
         (size (jit-constant-size_t 1))
         (visible-ancestor-environment (current-visible-environment parent-env t))
         (parent-renv-ref (if (core:function-container-environment-p visible-ancestor-environment)
                              (let ((closure (core:function-container-environment-closure visible-ancestor-environment)))
                                (irc-intrinsic "activationFrameReferenceFromClosure" closure))
                              (irc-renv visible-ancestor-environment)))
         (parent-renv (irc-load parent-renv-ref))
         (instr (irc-intrinsic "makeBlockFrameSetParent" parent-renv)))
    (irc-store instr new-renv)
    (irc-set-renv block-env new-renv)
    (values block-env instr (list parent-renv))))

#+(or)
(defun irc-new-block-environment (old-env &key name)
  (let* ((block-env (make-block-environment name old-env))
         (block-renv (irc-make-block-frame-set-parent block-env old-env)))
    (irc-set-renv block-env block-renv)
    block-env))

(defun irc-new-catch-environment (old-env)
  (make-catch-environment old-env))


(defun irc-set-renv (env renv)
  (set-runtime-environment env renv))


(defun irc-renv (env)
  (let ((renv (runtime-environment (current-visible-environment env))))
    (if renv
	(progn
	  (cmp-log "Returning non-nil renv%N")
	  renv)
	(let ((nil-renv (compile-reference-to-literal nil))) ;; (irc-intrinsic "activationFrameNil")))
	  (cmp-log "Returning nil renv: %s%N" nil-renv)
	  nil-renv))))

(defun irc-make-value-frame (result-af size)
  (let ((vf (irc-intrinsic "makeValueFrame" (jit-constant-size_t size))))
    (irc-store vf result-af)))

(defun irc-make-value-frame-set-parent (new-env fnsize parent-env)
  (let* ((new-renv (irc-renv new-env))
         (size (jit-constant-size_t fnsize))
         (visible-ancestor-environment (current-visible-environment parent-env t))
         (parent-renv-ref (if (core:function-container-environment-p visible-ancestor-environment)
                              (let ((closure (core:function-container-environment-closure visible-ancestor-environment)))
                                (irc-intrinsic "activationFrameReferenceFromClosure" closure))
                              (irc-renv visible-ancestor-environment)))
         (parent-renv (irc-load parent-renv-ref))
         (instr (irc-intrinsic "makeValueFrameSetParent" size parent-renv))
         #+debug-lexical-depth(frame-unique-id (gctools:next-lexical-depth-counter))
         #+debug-lexical-depth(set-frame-unique-id (progn 
                                                     (irc-intrinsic "setFrameUniqueId" (jit-constant-size_t frame-unique-id) instr))))
    #+optimize-bclasp
    (setf (gethash new-env *make-value-frame-instructions*)
          (make-value-frame-maker-reference :instruction instr
                                            :new-env new-env
                                            :new-renv new-renv
                                            :parent-env visible-ancestor-environment
                                            :parent-renv parent-renv
                                            #+debug-lexical-depth :frame-unique-id #+debug-lexical-depth frame-unique-id
                                            #+debug-lexical-depth :set-frame-unique-id #+debug-lexical-depth (list set-frame-unique-id (list (jit-constant-size_t frame-unique-id) instr))))
    (irc-store instr new-renv)
    instr))


(defun irc-set-parent (new-renv parent-env)
  (let ((visible-ancestor-environment (current-visible-environment parent-env t)))
    ;;    (core:bformat *debug-io* "irc-set-parent-of-activation-frame parent-> %s%N" visible-ancestor-environment)
    ;;    (core:bformat *debug-io* "irc-set-parent-of-activation-frame parent is f-c-e-p -> %s%N" (core:function-container-environment-p visible-ancestor-environment))
    (if (core:function-container-environment-p visible-ancestor-environment)
        (progn
          (error "Only value-frames should directly access the function-container-environment")
          #+(or)(let ((closure (core:function-container-environment-closure visible-ancestor-environment)))
                  ;;          (core:bformat *debug-io* "setParentOfActivationFrameFromClosure to %s%N" visible-ancestor-environment)
                  (irc-intrinsic "setParentOfActivationFrameFromClosure"
                                 new-renv
                                 closure)))
        (let ((parent-renv2 (irc-renv visible-ancestor-environment)))
          ;;          (core:bformat *debug-io* "setParentOfActivationFrame to %s%N" visible-ancestor-environment)
          (irc-intrinsic "setParentOfActivationFrame"
                         new-renv
                         (irc-load parent-renv2))))))

(defun irc-parent-renv (env)
  (let ((renv (runtime-environment (current-visible-environment (get-parent-environment env)))))
    (if renv
	(progn
	  (cmp-log "Returning non-nil renv%N")
	  renv)
	(let ((nil-renv (compile-reference-to-literal nil))) ;; (irc-intrinsic "activationFrameNil")))
	  (cmp-log "Returning nil renv: %s%N" nil-renv)
	  nil-renv))))

(defun irc-size_t (num)
  (jit-constant-size_t num))

(defun irc-literal (lit &optional (label "literal"))
  (irc-load (literal:compile-reference-to-literal lit)))

(defun irc-t ()
  (irc-literal t "T"))

(defun irc-nil ()
  (irc-literal nil "NIL"))

(defun irc-intrinsic-invoke-if-landing-pad-or-call (function-name args &optional (label "") (maybe-landing-pad *current-unwind-landing-pad-dest*))
  ;; FIXME:   If the current function has a landing pad - then use INVOKE
  (if maybe-landing-pad
      (irc-intrinsic-invoke function-name args maybe-landing-pad label)
      (irc-intrinsic-call function-name args label)))    

(defun irc-size_t-*current-source-pos-info*-filepos ()
  (let ((csp core:*current-source-pos-info*))
    (jit-constant-size_t (core:source-pos-info-filepos csp))))
(defun irc-size_t-*current-source-pos-info*-lineno ()
  (let ((csp core:*current-source-pos-info*))
    (jit-constant-size_t (core:source-pos-info-lineno csp))))
(defun irc-size_t-*current-source-pos-info*-column ()
  (let ((csp core:*current-source-pos-info*))
    (jit-constant-size_t (core:source-pos-info-column *current-source-pos-info*))))




#++
(defun irc-generate-terminate-code ()
      (let* ((landpad (irc-create-landing-pad 1)))
	(llvm-sys:add-clause landpad (llvm-sys:constant-pointer-null-get %i8*%))
	(irc-low-level-trace)
	(irc-intrinsic "clasp_terminate" (irc-constant-string-ptr *gv-source-namestring*)
		       (irc-size_t-*current-source-pos-info*-lineno) 
		       (irc-size_t-*current-source-pos-info*-column) 
		       (irc-constant-string-ptr *gv-current-function-name* ))
	(irc-unreachable)
	))


(defun irc-generate-unwind-protect-landing-pad-code (env)
      (let* ((landpad (irc-create-landing-pad 1)))
	(llvm-sys:add-clause landpad (llvm-sys:constant-pointer-null-get %i8*%))
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
	(unwind-result (irc-alloca-t*)))
    ;; Generate the unwind-form code in the parent environment of the unwind-protect form
    (codegen unwind-result unwind-form (get-parent-environment env))
    ))


(defun irc-do-unwind-environment (env)
  (cmp-log "irc-do-unwind-environment for: %s%N" env)
  (let ((unwind (local-metadata env :unwind)))
    (dolist (cc unwind)
      (let ((head (car cc)))
	(cond
	  ((eq head 'symbolValueRestore)
	   (cmp-log "popDynamicBinding of %s%N" (cadr cc))
	   (irc-intrinsic "popDynamicBinding" (irc-global-symbol (cadr cc) env)))
	  (t (error (bformat nil "Unknown cleanup code: %s" cc))))
	)))
  )

(defun irc-unwind-environment (env)
  (cmp-log "in irc-unwind-environment with: %s u-p-e?: %s%N" (type-of env) (unwind-protect-environment-p env))
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

(defun irc-basic-block-create (name &optional (function *current-function*))
  "Create a llvm::BasicBlock with (name) in the (function)"
  (let ((bb (llvm-sys:basic-block-create *llvm-context* (bformat nil "%s%s" *block-name-prefix* name) function)))
    (cmp-log "Created basic block  <*block-name-prefix* = %s>  <name=%s>: bb-> %s%N" *block-name-prefix* name bb)
    bb))

(defun irc-get-insert-block ()
  (llvm-sys:get-insert-block *irbuilder*))

(defun irc-append-basic-block (function theblock)
  "Append the basic block to the _function_. If the _function_ is not passed then use the current function"
  (or (null (llvm-sys:get-parent theblock)) (error "irc-append-basic-block the block ~a already has a parent ~a" theblock (llvm-sys:get-parent theblock)))
  (llvm-sys:append-basic-block function theblock))

(defun irc-set-insert-point-instruction (instruction &optional (irbuilder *irbuilder*))
  (llvm-sys:set-insert-point-instruction irbuilder instruction))


(defun irc-set-insert-point-basic-block (theblock &optional (irbuilder *irbuilder*))
  "Set the current insert point.  Signal an error if *irbuilder* jumps to a different function."
  (when (llvm-sys:get-insert-block irbuilder)
    (let* ((irbuilder-cur-basic-block (llvm-sys:get-insert-block irbuilder))
           (irbuilder-cur-function    (llvm-sys:get-parent irbuilder-cur-basic-block))
           (theblock-function         (llvm-sys:get-parent theblock)))
      (unless (equal irbuilder-cur-function theblock-function)
        (error "The IRBuilder ~a that is currently in function ~a is being told to jump functions when its insert point is being set to ~a in ~a" irbuilder irbuilder-cur-function theblock theblock-function))))
  (llvm-sys:set-insert-point-basic-block irbuilder theblock))



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
        (let ((llt (get-or-declare-function-or-error *the-module* "lowLevelTrace")))
          (llvm-sys:create-call-array-ref *irbuilder* llt (list (jit-constant-i32 *next-low-level-trace-index*)) ""))
        (setq *next-low-level-trace-index* (+ 1 *next-low-level-trace-index*)))
      nil))


(defun irc-begin-landing-pad-block (theblock &optional (function *current-function*))
  "This doesn't invoke low-level-trace - it would interfere with the landing pad"
  (or (llvm-sys:get-parent theblock) (error "irc-begin-landing-pad-block>> The block ~a doesn't have a parent" theblock))
  #+(or)(irc-append-basic-block function theblock)
  (irc-set-insert-point-basic-block theblock)
  )


(defun irc-begin-block (theblock &optional (function *current-function*))
  "This invokes a low-level trace at the top of the block"
  (or function (error "The current function isn't defined - it must be defined to add a basic-block to"))
  (or (llvm-sys:get-parent theblock) (error "irc-begin-block>> The block ~a doesn't have a parent" theblock))
  #+(or)(irc-append-basic-block function theblock)
  (irc-set-insert-point-basic-block theblock)
  )


(defun irc-branch-to-and-begin-block (theblock)
  (irc-br theblock)
  (irc-begin-block theblock))

(defun irc-icmp-ule (lhs rhs &optional (name ""))
  (llvm-sys:create-icmp-ule *irbuilder* lhs rhs name))

(defun irc-icmp-uge (lhs rhs &optional (name ""))
  (llvm-sys:create-icmp-uge *irbuilder* lhs rhs name))

(defun irc-icmp-ult (lhs rhs &optional (name ""))
  (llvm-sys:create-icmp-ult *irbuilder* lhs rhs name))

(defun irc-icmp-ugt (lhs rhs &optional (name ""))
  (llvm-sys:create-icmp-ugt *irbuilder* lhs rhs name))

(defun irc-icmp-slt (lhs rhs &optional (name ""))
  (llvm-sys:create-icmp-slt *irbuilder* lhs rhs name))

(defun irc-icmp-sgt (lhs rhs &optional (name ""))
  (llvm-sys:create-icmp-sgt *irbuilder* lhs rhs name))

(defun irc-icmp-sge (lhs rhs &optional (name ""))
  (llvm-sys:create-icmp-sge *irbuilder* lhs rhs name))

(defun irc-icmp-sle (lhs rhs &optional (name ""))
  (llvm-sys:create-icmp-sle *irbuilder* lhs rhs name))

(defun irc-icmp-ne (lhs rhs &optional (name ""))
  (llvm-sys:create-icmp-ne *irbuilder* lhs rhs name))

(defun irc-icmp-eq (lhs rhs &optional (name ""))
  (llvm-sys:create-icmp-eq *irbuilder* lhs rhs name))


(defun irc-cond-br (icond true false &optional branchWeights)
  (llvm-sys:create-cond-br *irbuilder* icond true false branchWeights))

(defun irc-ptr-to-int (val int-type &optional (label "ptrtoint"))
  (llvm-sys:create-ptr-to-int *irbuilder* val int-type label))

(defun irc-int-to-ptr (val ptr-type &optional (label "inttoptr"))
  (llvm-sys:create-int-to-ptr *irbuilder* val ptr-type label))

(defun irc-maybe-cast-integer-to-t* (val &optional (label "fixnum-to-t*"))
  "If it's a fixnum then cast it - otherwise just return it - it should already be a t*"
  (if (typep val '(integer #.(- (expt 2 63)) #.(- (expt 2 63) 1)))
      (llvm-sys:create-int-to-ptr *irbuilder* (jit-constant-i64 val) %t*% label)
      (if (equal (llvm-sys:get-type val) %t*%)
          val
          (error "The val ~s type ~s is not a t* or fixnum " val (type-of val)))))
  

(defun irc-ret-void ()
  (llvm-sys:create-ret-void *irbuilder*))

(defun irc-ret (val)
  (llvm-sys:create-ret *irbuilder* val))

(defun irc-undef-value-get (type)
  (llvm-sys:undef-value-get type))

(defun irc-prev-inst-terminator-inst-p ()
  (let ((cur-block (irc-get-insert-block)))
    (cmp-log "irc-prev-inst-terminator-inst-p dumping current block:%N")
    (cmp-log "    cur-block -> %s%N" cur-block)
    (if cur-block
	(if (= (llvm-sys:basic-block-size cur-block) 0)
	    nil
            (progn
              (llvm-sys:terminator-inst-p (llvm-sys:basic-block-back cur-block))))
	nil)))
    
(defun irc-br (block &optional (where "undefined"))
  (or block (error "Destination block ~a is nil!!!" where))
  (llvm-sys:create-br *irbuilder* block))

(defun irc-branch-if-no-terminator-inst (block)
;;; For now always create a branch - testing if the last instruction
;;; is a terminator is not a good thing to do
;;; and it's causing a crash
  (when (not (irc-prev-inst-terminator-inst-p))
          (llvm-sys:create-br *irbuilder* block))
  #+(or)(llvm-sys:create-br *irbuilder* block))

(defun irc-add (lhs rhs &optional (label ""))
  (llvm-sys:create-add *irbuilder* lhs rhs label nil nil))

(defun irc-sub (lhs rhs &optional (label ""))
  (llvm-sys:create-sub *irbuilder* lhs rhs label nil nil))

(defun irc-load (source &optional (label ""))
  (llvm-sys:create-load-value-twine *irbuilder* source label))

;;; Loads a t* from a t** or a tsp* depending on the type of source
(defun irc-load-t* (source &optional (label ""))
  (let ((source-type (llvm-sys:get-type source)))
    (cond
      ((equal source-type %t*%) source) ;; pass it through
      ((equal source-type %t**%)
       (llvm-sys:create-load-value-twine *irbuilder* source label))
      ((equal source-type %tsp*%)
       (let ((val-tsp (llvm-sys:create-load-value-twine *irbuilder* source label)))
         (irc-extract-value val-tsp (list 0) "t*-part")))
      (t (error "Cannot irc-load-t* from ~s" source)))))

;;; irc-store generates code for the following situations
;;;   t* -> tsp
;;;   t* -> tmv
;;;   tsp -> tsp
;;;   tsp -> tmv
(defun irc-store (val destination &optional (label ""))
  (let ((val-type (llvm-sys:get-type val))
        (dest-contained-type (llvm-sys:get-contained-type (llvm-sys:get-type destination) 0)))
    (if (equal val-type dest-contained-type)
        (llvm-sys:create-store *irbuilder* val destination nil)
        (cond
          ;; Write into %t**%
          ((and (equal val-type %t**%)
                (equal dest-contained-type %t*%))
           (llvm-sys:create-store *irbuilder* (irc-load val) destination nil))
          ((and (equal val-type %t*%)
                (equal dest-contained-type %t*%))
           (llvm-sys:create-store *irbuilder* val destination nil))
          ((and (equal val-type %tsp%)
                (equal dest-contained-type %t*%))
           (let ((t* (irc-smart-ptr-extract val)))
             (llvm-sys:create-store *irbuilder* t* destination nil)))
          ((and (equal val-type %tmv%)
                (equal dest-contained-type %t*%))
           (let ((ptr (irc-extract-value val (list 0) "t*-part")))
             (llvm-sys:create-store *irbuilder* ptr destination nil)))
          ;; Write into %tsp*%
          ((and (equal val-type %t*%)
                (equal dest-contained-type %tsp%))
           (let* ((ptr val)
                  (undef (llvm-sys:undef-value-get %tsp%))
                  (tsp0 (llvm-sys:create-insert-value *irbuilder* undef ptr '(0) "tsp0")))
             (llvm-sys:create-store *irbuilder* tsp0 destination nil)))
          ((and (equal val-type %t**%)
                (equal dest-contained-type %tsp%))
           (let* ((ptr (irc-load val))
                  (undef (llvm-sys:undef-value-get %tsp%))
                  (tsp0 (llvm-sys:create-insert-value *irbuilder* undef ptr '(0) "tsp0")))
             (llvm-sys:create-store *irbuilder* tsp0 destination nil)))
          ((and (equal val-type %return_type%)
                (equal dest-contained-type %tsp%))
           (let* ((val (irc-intrinsic "valueOrNilIfZero" val))
                  (undef (llvm-sys:undef-value-get %tsp%))
                  (tsp0 (llvm-sys:create-insert-value *irbuilder* undef val '(0) "tsp0")))
             (llvm-sys:create-store *irbuilder* tsp0 destination nil)))
          ;; Write into %tmv*%
          ((and (equal val-type %return_type%)
                (equal dest-contained-type %tmv%))
           (let* ((result-in-registers val)
                  (ret0 (irc-extract-value result-in-registers (list 0)))
                  (nret (irc-extract-value result-in-registers (list 1)))
                  (undef (llvm-sys:undef-value-get %tmv%))
                  (tmv0 (llvm-sys:create-insert-value *irbuilder* undef ret0 '(0) "tmv0"))
                  (tmv1 (llvm-sys:create-insert-value *irbuilder* tmv0 nret '(1) "tmv1")))
             (llvm-sys:create-store *irbuilder* tmv1 destination nil)))
        
          ((and (equal val-type %t*%)
                (equal dest-contained-type %tmv%))
           (let* ((ptr val)
                  (undef (llvm-sys:undef-value-get %tmv%))
                  (tmv0 (llvm-sys:create-insert-value *irbuilder* undef ptr '(0) "tmv0"))
                  (tmv1 (llvm-sys:create-insert-value *irbuilder* tmv0 (jit-constant-uintptr_t 1) '(1) "tmv1")))
             (llvm-sys:create-store *irbuilder* tmv1 destination nil)))
          ((and (equal val-type %t**%)
                (equal dest-contained-type %tmv%))
           (let* ((ptr (irc-load val))
                  (undef (llvm-sys:undef-value-get %tmv%))
                  (tmv0 (llvm-sys:create-insert-value *irbuilder* undef ptr '(0) "tmv0"))
                  (tmv1 (llvm-sys:create-insert-value *irbuilder* tmv0 (jit-constant-uintptr_t 1) '(1) "tmv1")))
             (llvm-sys:create-store *irbuilder* tmv1 destination nil)))
          ((and (equal val-type %tsp%)
                (equal dest-contained-type %tmv%))
           (let* ((ptr (irc-extract-value val (list 0) "t*-part"))
                  (undef (llvm-sys:undef-value-get %tmv%))
                  (tmv0 (llvm-sys:create-insert-value *irbuilder* undef ptr '(0) "tmv0"))
                  (tmv1 (llvm-sys:create-insert-value *irbuilder* tmv0 (jit-constant-uintptr_t 1) '(1) "tmv1")))
             (llvm-sys:create-store *irbuilder* tmv1 destination nil)))
          (t (if (equal (llvm-sys:get-context val-type) (llvm-sys:get-context dest-contained-type))
                 (error "!!! Mismatch in irc-store between val type ~a and destination contained type ~a%N" val-type dest-contained-type)
                 (error "!!! Mismatch in irc-store involving the val type ~a and desintation contained type ~a - the type LLVMContexts don't match - so they were defined in different threads!" val-type dest-contained-type)))))))

(defun irc-phi (return-type num-reserved-values &optional (label "phi"))
  (llvm-sys:create-phi *irbuilder* return-type num-reserved-values label))

(defun irc-phi-add-incoming (phi-node value basic-block)
  (llvm-sys:add-incoming phi-node value basic-block))


(defun irc-unreachable ()
  (irc-intrinsic "unreachableError" )
  (llvm-sys:create-unreachable *irbuilder*))


(defun irc-trunc (value type &optional (label "trunc"))
  (llvm-sys:create-trunc *irbuilder* value type label))


(defun irc-and (x y &optional (label "and"))
  (llvm-sys:create-and-value-value *irbuilder* x y label))

(defun irc-va_arg (valist type &optional (name "vaarg"))
  (llvm-sys:create-vaarg *irbuilder* valist type name))




#|(llvm-sys:create-in-bounds-gep *irbuilder* (llvm-sys:get-or-create-uniqued-string-global-variable *the-module* *current-function-name* (bformat nil ":::func-name-%s" *current-function-name*)) (list (jit-constant-i32 0) (jit-constant-i32 0)) "fn-name") 
|#


(defparameter *default-function-attributes* '(llvm-sys:attribute-uwtable
                                              ("no-frame-pointer-elim" "true")
                                              "no-frame-pointer-elim-non-leaf"))
(defmacro with-new-function
    (( ;; FN is bound to the function being created
      fn
      ;; FN-ENV is bound to the function environment
      fn-env
      ;; RESULT is bound to the %tmv% result
      result
      &key
      ;; The function name can be a symbol or a string.
      ;; if its a string it is used unchanged
      ;; if its a symbol then it is mangled by appending "FN-SYMB." to it
      ;; if its a cons of the form (setf ...) then its mangled by appending FN-SETF. to it
      (function-name "function")
      ;; Specify the LLVM type of the function
      (function-type '%fn-prototype% function-type-p)
      ;; List the names of the arguments - this must match what
      ;; is passed to the the :FUNCTION-TYPE keyword above
      (argument-names '+fn-prototype-argument-names+ argument-names-p)
      ;; This is the parent environment of the new function environment that
      ;; will be returned
      parent-env
      ;; This is the form that will be compiled as the function code
      function-form
      ;; Set attributes - list of symbols, or string or string pairs
      (function-attributes *default-function-attributes* function-attributes-p )
      ;; This is the LLVM linkage - DONT USE "private" linkage - I encountered some
      ;; pretty subtle bugs with exception handling (I think) when I did that.
      ;; I currently use llvm-sys:internal-linkage or llvm-sys:external-linkage
      (linkage ''llvm-sys:internal-linkage)
      ;; If the generated function returns void then indicate with this keyword
        return-void
        ;; info for the function description
        function-info
      )
     &rest body)
  "Create a new function with {function-name} and {parent-env} - return the function"
  (cmp-log "Expanding with-new-function name: %s%N" function-name)
  (let ((cleanup-block-gs (gensym "cleanup-block"))
	(traceid-gs (gensym "traceid"))
	(irbuilder-alloca (gensym))
        (temp (gensym))
	(irbuilder-body (gensym))
        (function-description (gensym)))
    `(multiple-value-bind (,fn ,fn-env ,cleanup-block-gs ,irbuilder-alloca ,irbuilder-body ,result ,function-description)
	 (irc-bclasp-function-create ,function-name ,parent-env
                                     :function-type ,function-type
                                     :argument-names ,argument-names
                                     :function-attributes ',function-attributes
                                     :linkage ,linkage
                                     :function-info ,function-info)
       (let* ((*current-function* ,fn)
              (*current-function-description* ,function-description)
              (*current-function-name* (llvm-sys:get-name ,fn))
              (*irbuilder-function-alloca* ,irbuilder-alloca)
              (*irbuilder-function-body* ,irbuilder-body)
              (*gv-current-function-name* (module-make-global-string *current-function-name* "fn-name")))
         (with-irbuilder (*irbuilder-function-body*)
           (with-new-function-prepare-for-try (,fn *irbuilder-function-alloca*)
             (with-try
                 (with-dbg-function (,function-name
                                     :linkage-name *current-function-name*
                                     :function ,fn
                                     :function-type ,function-type
                                     :form ,function-form )
                   (with-dbg-lexical-block (,function-form)
                     (dbg-set-current-source-pos-for-irbuilder ,irbuilder-alloca *current-form-lineno*)
                     (dbg-set-current-source-pos-for-irbuilder ,irbuilder-body *current-form-lineno*)
                     (with-irbuilder (*irbuilder-function-body*)
                       (or *the-module* (error "with-new-function *the-module* is NIL"))
                       (cmp-log "with-landing-pad around body%N")
                       ;; I don't think the with-landing-pad is necessary because with-try provides it
                       #+(or)
                       (with-landing-pad (irc-get-cleanup-landing-pad-block ,fn-env)
                         ,@body)
                       (progn ,@body))))
               ((cleanup)
                (irc-cleanup-function-environment ,fn-env)
                #++(with-landing-pad (irc-get-terminate-landing-pad-block ,fn-env)
                     (irc-function-cleanup-and-return ,fn-env ,result :return-void ,return-void)))))
           (if ,return-void
               (llvm-sys:create-ret-void *irbuilder*)
               (llvm-sys:create-ret *irbuilder* (irc-load ,result)))
           ,fn)))))

(defun function-description-name (function)
  (let ((function-name (llvm-sys:get-name function)))
    (core:bformat nil "%s^DESC" function-name)))

(defun irc-function-create (function-type linkage llvm-function-name module
                            &key
                              (function-attributes *default-function-attributes* function-attributes-p ))
  (let* ((fn (llvm-sys:function-create function-type
                                       linkage
                                       llvm-function-name
                                       module)))
    (dolist (temp function-attributes)
      (cond
        ((symbolp temp) (llvm-sys:add-fn-attr fn temp))
        ((stringp temp) (llvm-sys:add-fn-attr2string fn temp ""))
        ((and (consp temp) (stringp (car temp)) (stringp (cadr temp)))
         (llvm-sys:add-fn-attr2string fn (car temp) (cadr temp)))
        (t (error "Illegal function attribute ~a" temp))))
    fn))


(defun irc-simple-function-create (llvm-function-name function-type linkage module
                                   &key (function-attributes *default-function-attributes* function-attributes-p )
                                     argument-names ;;; '("result-ptr" "activation-frame-ptr") argument-names-p))
                                     )
  "A simple function creator - set personality and arguments and function-attributes.
But no irbuilders or basic-blocks. Return the fn."
  (let ((fn (irc-function-create function-type
                                 linkage
                                 llvm-function-name
                                 module
                                 :function-attributes function-attributes)))
    (llvm-sys:set-personality-fn fn (irc-personality-function))
    (mapcar #'(lambda (arg argname) (llvm-sys:set-name arg argname))
            (llvm-sys:get-argument-list fn) argument-names)
    fn))

(defun parse-declares-for-source-info (declares)
  (dolist (one-declare declares)
    (when (eq (car one-declare) 'core:lambda-name)
      (return-from parse-declares-for-source-info (values t
                                                          (second one-declare)
                                                          (fourth one-declare)
                                                          (fifth one-declare)
                                                          (sixth one-declare))))))

(defstruct (function-info (:type vector) :named)
  function-name
  (source-file-name cmp:*source-file-name*)
  lambda-list docstring declares form lineno column filepos
  (source-debug-file-name cmp:*source-debug-file-name*)
  (source-debug-offset cmp:*source-debug-offset*)
  (source-debug-use-lineno-p cmp:*source-debug-use-lineno-p*))

(defun irc-create-function-description (llvm-function-name fn module function-info)
  "If **generate-code** then create a function-description block from function info.
    Otherwise we are code-walking - and do something else that is appropriate."
  (unless function-info
    (error "function info is NIL for ~a" llvm-function-name))
  (let ((function-name (function-info-function-name function-info))
        (source-file-name (function-info-source-file-name function-info))
        source-info-name
        (lambda-list (function-info-lambda-list function-info))
        (docstring (function-info-docstring function-info))
        (lineno (function-info-lineno function-info))
        (column (function-info-column function-info))
        (filepos (function-info-filepos function-info))
        (declares (function-info-declares function-info))
        (source-debug-file-name (function-info-source-debug-file-name function-info))
        (source-debug-offset (function-info-source-debug-offset function-info))
        (source-debug-use-lineno-p (function-info-source-debug-use-lineno-p function-info)))
    (multiple-value-bind (found-source-info n l c f)
        (parse-declares-for-source-info declares)
      (when found-source-info
        (when n (setf source-info-name n))
        (when l (setf lineno l))
        (when c (setf column c))
        (when f (setf filepos f)))
 #+(or)
      (progn
        (core:bformat t "--------------------------%N")
        (core:bformat t "found-source-info: %s%N" found-source-info)
        (core:bformat t "source-file-name: %s%N" source-file-name)
        (core:bformat t "source-info-name: %s%N" source-info-name)
        (core:bformat t "llvm-function-name: %s%N" llvm-function-name)
        (core:bformat t "function-name: %s%N" function-name)
        (core:bformat t "declares: %s%N" declares)
        (core:bformat t "form: %s%N" (function-info-form function-info))
        (core:bformat t "lambda-list: %s%N" lambda-list)
        (core:bformat t "docstring: %s%N" docstring)
        (core:bformat t "lineno: %s%N" lineno)
        (core:bformat t "column: %s%N" column)
        (core:bformat t "filepos: %s%N" filepos))
      (let ((source-file-name-index (literal:reference-literal source-file-name))
            (function-name-index (literal:reference-literal function-name t))
            (lambda-list-index (literal:reference-literal lambda-list t))
            (docstring-index (literal:reference-literal docstring t))
            (declare-index (literal:reference-literal declares t))
            (source-debug-file-name-index (literal:reference-literal source-debug-file-name)))
        #+(or)
        (progn
          (core:bformat t "source-file-name-index: %s%N" source-file-name-index)
          (core:bformat t "function-name-index: %s%N" function-name-index)
          (core:bformat t "lambda-list-index: %s%N" lambda-list-index)
          (core:bformat t "docstring-index: %s%N" docstring-index)
          (core:bformat t "declare-index: %s%N" declare-index))
        (unless lambda-list-index (error "There is no lambda-list-index"))
        (unless docstring-index (error "There is no docstring-index"))
        (unless lineno (error "There is no lineno"))
        (unless column (error "There is no column"))
        (unless filepos (error "There is no filepos"))
        (llvm-sys:make-global-variable
         module
         %function-description%
         t
         'llvm-sys:internal-linkage
         (llvm-sys:constant-struct-get %function-description%
                                       (list
                                        fn
                                        literal::*gcroots-in-module*
                                        (jit-constant-i32 source-file-name-index)
                                        (jit-constant-i32 function-name-index)
                                        (jit-constant-i32 lambda-list-index)
                                        (jit-constant-i32 docstring-index)
                                        (jit-constant-i32 declare-index)
                                        (jit-constant-i32 lineno)
                                        (jit-constant-i32 column)
                                        (jit-constant-i32 filepos)
                                        (jit-constant-i32 0) ; macroP
                                        (jit-constant-i32 source-debug-file-name-index)
                                        (jit-constant-i32 source-debug-offset)
                                        (jit-constant-i32 (if source-debug-use-lineno-p 1 0))
                                        )
                                       )
         (function-description-name fn))))))



(defun irc-bclasp-function-create (lisp-function-name env
                                   &key
                                     (function-type %fn-prototype% function-type-p)
                                     (function-attributes *default-function-attributes* function-attributes-p)
                                     ;; If the first argument is NOT meant to be a returned structure then set this to nil
                                     (argument-names '("result-ptr" "activation-frame-ptr") argument-names-p)
                                     (linkage 'llvm-sys:internal-linkage)
                                     function-info)
  "Returns the new function, the lexical environment for the function 
and the block that cleans up the function and rethrows exceptions,
followed by the traceid for this function and then the current insert block,
and then the irbuilder-alloca, irbuilder-body."
  (when (or function-type-p argument-names-p)
    (when (not (and function-type-p argument-names-p))
      (error "If you provide one of function-type or argument-names you must provide both")))
  (let* ((llvm-function-name (jit-function-name lisp-function-name))
         (fn (irc-simple-function-create llvm-function-name
                                         function-type
                                         linkage
                                         *the-module*
                                         :function-attributes function-attributes
                                         :argument-names argument-names))
         (fn-description (irc-create-function-description
                          llvm-function-name
                          fn
                          *the-module*
                          function-info))
         (*current-function* fn)
	 (func-env (make-function-container-environment env (car (llvm-sys:get-argument-list fn)) fn))
	 cleanup-block traceid
	 (irbuilder-cur (llvm-sys:make-irbuilder *llvm-context*))
	 (irbuilder-alloca (llvm-sys:make-irbuilder *llvm-context*))
	 (irbuilder-body (llvm-sys:make-irbuilder *llvm-context*)))
    (let ((entry-bb (irc-basic-block-create "entry" fn)))
      (irc-set-insert-point-basic-block entry-bb irbuilder-cur))
    ;; Setup exception handling and cleanup landing pad
    (irc-set-function-for-environment func-env fn)
    (with-irbuilder (irbuilder-cur)
      (let* ((body-bb (irc-basic-block-create "body" fn))
	     (entry-branch (irc-br body-bb)))
	(irc-set-insert-point-instruction entry-branch irbuilder-alloca)
	(irc-set-insert-point-basic-block body-bb irbuilder-body)))
    (setf-metadata func-env :cleanup ())
    (let ((result               (irc-alloca-tmv func-env :irbuilder irbuilder-alloca :label "result")))
      (values fn func-env cleanup-block irbuilder-alloca irbuilder-body result fn-description))))


(defun irc-cclasp-function-create (llvm-function-type linkage llvm-function-name module function-info)
  "Create a function and a function description for a cclasp function"
  (let* ((fn (irc-function-create llvm-function-type linkage llvm-function-name module))
         (fn-description (irc-create-function-description llvm-function-name fn module function-info)))
    (values fn fn-description)))

#+(or)
(defun irc-function-cleanup-and-return (env result &key return-void)
  (let ((return-block (irc-basic-block-create "return-block")))
    (cmp-log "About to irc-br return-block%N")
    (irc-br return-block)
    (irc-begin-block return-block)
    (irc-cleanup-function-environment env #| invocation-history-frame |# ) ;; Why the hell was this commented out?
    #|	  (irc-cleanup-function-environment env invocation-history-frame )  |#
    (if return-void
        (llvm-sys:create-ret-void *irbuilder*)
        (llvm-sys:create-ret *irbuilder* (irc-load result))))
  (irc-verify-function *current-function*))
  

(defun irc-cleanup-function-environment (env #||invocation-history-frame||#)
  "Generate the code to cleanup the environment"
  (if env
      (progn
;;	(dbg-pop-invocation-history-stack)
	(irc-do-unwind-environment env)
	(let ((cleanup (local-metadata env :cleanup)))
	  ;;      (cmp-log "Cleaning up env: %s%N" env)
	  (cmp-log "About to cleanup local-metadata :cleanup --> %s%N" cleanup)
	  (dolist (cc cleanup)
	    (let ((h (car cc)))
	      (cond
		((functionp h) (funcall h (cadr cc)))
		((null h) (bformat t "Cleanup code of NIL!!!!!%N"))
		((eq h 'destructTsp) (irc-dtor "destructTsp" (cadr cc)))
		((eq h 'destructTmv) (irc-dtor "destructTmv" (cadr cc)))
		((eq h 'destructAFsp) (irc-dtor "destructAFsp" (cadr cc)))
		((eq h 'exit-lexical-scope) (handle-exit-scope cc env))
		((stringp h) (irc-intrinsic h (cadr cc)))
		(t (break (bformat nil "Unknown cleanup code: %s" h))))
	      ))))))

(defun irc-pointer-cast (from totype &optional (label ""))
  (llvm-sys:create-pointer-cast *irbuilder* from totype label))

(defun irc-bit-cast (from totype &optional (label "irc-bit-cast"))
  (llvm-sys:create-bit-cast *irbuilder* from totype label))

(defun irc-irbuilder-status (&optional (irbuilder *irbuilder*) (label "current *irbuilder*"))
    (bformat t "%s -> %s%N" label irbuilder))

(defun irc-constant-string-ptr (global-string-var)
  (let ((ptr (llvm-sys:create-geparray *irbuilder* global-string-var (list (cmp:jit-constant-i32 0) (cmp:jit-constant-i32 0)) "ptr")))
    ptr))

(defun irc-dtor (name obj)
  (declare (special *compiler-suppress-dtors*))
  (unless *compiler-suppress-dtors* (irc-intrinsic name obj)))

(defun irc-push-cleanup (env cleanup-code)
  (multiple-value-bind (cleanup-cur found metadata-env)
      (lookup-metadata env :cleanup)
    (push-metadata metadata-env :cleanup cleanup-code)))

(defun irc-push-unwind (env unwind-code)
  "Push code that should be executed when this environment is left"
  (push-metadata env :unwind unwind-code))

(defmacro with-alloca-insert-point-no-cleanup (irbuilder &key alloca init)
  "Switch to the alloca-insert-point and generate code to alloca a local variable.
Within the _irbuilder_ dynamic environment...
- insert the given alloca instruction using the provided irbuilder 
- insert the initialization code (if provided) right after the alloca "
  (let ((alloca-sym (gensym))
	(found-gs (gensym)))
    `(with-irbuilder (,irbuilder)
       (let ((,alloca-sym ,alloca))
	 (when ,init (funcall ,init ,alloca-sym))
	 ,alloca-sym))))

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
	     (push-metadata ,metadata-env-gs :cleanup (list ,cleanup ,alloca-sym))))
	 ,alloca-sym))
    ))


(defmacro with-irbuilder ((irbuilder) &rest code)
  "Set *irbuilder* to the given IRBuilder"
  `(let ((*irbuilder* ,irbuilder))
     (cmp-log "Switching to irbuilder --> %s%N" (bformat nil "%s" ,irbuilder))
     (multiple-value-prog1 (progn ,@code)
       (cmp-log "Leaving irbuilder --> %s%N" (bformat nil "%s" ,irbuilder)))))


(defun irc-alloca-tmv (env &key (irbuilder *irbuilder-function-alloca*) (label ""))
  (with-alloca-insert-point-no-cleanup irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* %tmv% (jit-constant-i32 1) label)
    :init (lambda (a) (irc-intrinsic "newTmv" a))))

(defun irc-alloca-return-type (&key (irbuilder *irbuilder-function-alloca*) (label ""))
  (with-alloca-insert-point-no-cleanup
    irbuilder
    :alloca (llvm-sys:create-alloca irbuilder %return_type% (jit-constant-i32 1) label)))

(defun irc-alloca-t* (&key (irbuilder *irbuilder-function-alloca*) (label ""))
  "Allocate a T_O* on the stack"
  (with-alloca-insert-point-no-cleanup
    irbuilder
    :alloca (llvm-sys:create-alloca *irbuilder* %t*% (jit-constant-i32 1) label)))

(defun irc-alloca-af* (env &key (irbuilder *irbuilder-function-alloca*) (label ""))
  (cmp-log "irc-alloca-af* label: %s for %s%N" label irbuilder)
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* %af*% (jit-constant-i32 1) label)
    :init (lambda (a) );;(irc-intrinsic "newAFsp" a))
    :cleanup (lambda (a)))); (irc-dtor "destructAFsp" a))))

(defun irc-alloca-af*-value-frame-of-size (env size &key (irbuilder *irbuilder-function-alloca*) (label ""))
  (cmp-log "irc-alloca-af*-value-frame-of-size label: %s for %s%N" label irbuilder)
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* %af*% (jit-constant-i32 1) label)
    :init (lambda (a) ); (irc-intrinsic "newAFsp" a))
    :cleanup (lambda (a)))); (irc-dtor "destructAFsp" a))))

(defun irc-alloca-i32-no-init (&key (irbuilder *irbuilder-function-alloca*) (label "i32-"))
  "Allocate space for an i32"
  (llvm-sys::create-alloca irbuilder %i32% (jit-constant-i32 1) label))

(defun irc-alloca-i8 (env init-val &key (irbuilder *irbuilder-function-alloca*) (label "i8-"))
  "Allocate space for an i8"
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* %i8% (jit-constant-i32 1) label)
    :init (lambda (a) (irc-store (jit-constant-i8 init-val) a))))


(defun irc-alloca-i32 (env init-val &key (irbuilder *irbuilder-function-alloca*) (label "i32-"))
  "Allocate space for an i32"
  (with-alloca-insert-point env irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* %i32% (jit-constant-i32 1) label)
    :init (lambda (a) (irc-store (jit-constant-i32 init-val) a))))

(defun irc-alloca-va_list (&key (irbuilder *irbuilder-function-alloca*) (label "va_list"))
  "Alloca space for an va_list"
  (with-alloca-insert-point-no-cleanup irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* %va_list% (jit-constant-size_t 1) label)
    :init nil))

(defun irc-alloca-invocation-history-frame (&key (irbuilder *irbuilder-function-alloca*) (label "va_list"))
  "Alloca space for an va_list"
  (with-alloca-insert-point-no-cleanup irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* %InvocationHistoryFrame% (jit-constant-size_t 1) label)
    :init nil))

(defun irc-alloca-size_t (&key (irbuilder *irbuilder-function-alloca*) (label "va_list"))
  "Alloca space for an va_list"
  (with-alloca-insert-point-no-cleanup irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* %size_t% (jit-constant-size_t 1) label)
    :init nil))

(defun irc-alloca-register-save-area (&key (irbuilder *irbuilder-function-alloca*) (label "va_list"))
  "Alloca space for an va_list"
  (with-alloca-insert-point-no-cleanup irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* %register-save-area% (jit-constant-size_t 1) label)
    :init nil))

(defun irc-alloca-vaslist (&key (irbuilder *irbuilder-function-alloca*) (label "va_list"))
  "Alloca space for an vaslist"
  (with-alloca-insert-point-no-cleanup irbuilder
    :alloca (llvm-sys::create-alloca *irbuilder* %vaslist% (jit-constant-size_t 1) label)
    :init nil))

(defun irc-alloca-i8* (&key (irbuilder *irbuilder-function-alloca*) (label "i8*-"))
  "Allocate space for an i8*"
  (llvm-sys::create-alloca irbuilder %i8*% (jit-constant-i32 1) label))



; ----------------------------------------------------------------------
(defun null-tsp ()
  (llvm-sys:constant-struct-get %tsp% (list (llvm-sys:constant-pointer-null-get %t*%))))

(defun null-t-ptr ()
  (llvm-sys:constant-pointer-null-get %t*%))

;----------------------------------------------------------------------

#+(or)
(defun irc-store-multiple-values (offset values &optional va-list)
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
;;;    (XXXXXXX)
    (irc-low-level-trace)
    multiple-values-array))
	
	
(defun irc-struct-gep (struct idx &optional (label ""))
  (llvm-sys:create-struct-gep *irbuilder* struct idx label ))

(defun irc-insert-value (struct val idx-list &optional (label ""))
  (llvm-sys:create-insert-value *irbuilder* struct val idx-list label))

(defun irc-set-smart-ptr (tsp-val t-ptr-val)
  (irc-insert-value tsp-val t-ptr-val (list 0)))

(defun irc-extract-value (struct idx-list &optional (label ""))
  (let ((struct-type (llvm-sys:get-type struct)))
    (when (or (equal struct-type %t*%)
              (equal struct-type %t**%))
      (error "You cannot extract from simple type ~s value ~s" struct-type struct)))
  (llvm-sys:create-extract-value *irbuilder* struct idx-list label))

(defun irc-smart-ptr-extract (smart-ptr &optional (label ""))
  "Extract the t-ptr from the smart-ptr"
  (unless (equal (llvm-sys:get-type smart-ptr) %tsp%)
    (error "The argument ~s is not a tsp" smart-ptr))
  (irc-extract-value smart-ptr (list 0) label))


(defun irc-store-result-t* (result result-in-registers)
  (let ((ret0 result-in-registers)
        (nret (jit-constant-size_t 1))
        (return-type (llvm-sys:get-type result)))
    (if (equal return-type %tsp*%)
        (let* ((undef (llvm-sys:undef-value-get %tsp%))
               (ret-tsp (llvm-sys:create-insert-value *irbuilder* undef ret0 '(0) "ret0")))
          (irc-store ret-tsp result))
        (let* ((undef (llvm-sys:undef-value-get %tmv%))
               (ret-tmv0 (llvm-sys:create-insert-value *irbuilder* undef ret0 '(0) "ret0"))
               (ret-tmv1 (llvm-sys:create-insert-value *irbuilder* ret-tmv0 nret '(1) "nret")))
          (irc-store ret-tmv1 result)))))

;;; Store the result in a +result_type+ value (result-in-registers)
;;; in a T_mv or T_sp value
(defun irc-store-result (result result-in-registers)
  (let ((ret0 (irc-extract-value result-in-registers (list 0)))
        (nret (irc-extract-value result-in-registers (list 1)))
        (return-type (llvm-sys:get-type result)))
    (if (equal return-type %tsp*%)
        (let* ((undef (llvm-sys:undef-value-get %tsp%))
               (ret-tsp (llvm-sys:create-insert-value *irbuilder* undef ret0 '(0) "ret0")))
          (irc-store ret-tsp result))
        (let* ((undef (llvm-sys:undef-value-get %tmv%))
               (ret-tmv0 (llvm-sys:create-insert-value *irbuilder* undef ret0 '(0) "ret0"))
               (ret-tmv1 (llvm-sys:create-insert-value *irbuilder* ret-tmv0 nret '(1) "nret")))
          (irc-store ret-tmv1 result)))))

(defun irc-calculate-entry (closure)
  (let* ((closure-uintptr        (irc-ptr-to-int closure %uintptr_t%))
         (entry-point-addr-uint  (irc-add closure-uintptr (jit-constant-uintptr_t (- +closure-entry-point-offset+ +general-tag+)) "entry-point-addr-uint"))
         (entry-point-addr       (irc-int-to-ptr entry-point-addr-uint %fn-prototype**% "entry-point-addr"))
         (entry-point            (irc-load entry-point-addr "entry-point")))
    entry-point))

(defun irc-calculate-real-args (args)
  (let* ((nargs                  (length args))
         #+debug-guard-exhaustive-validate
         (_                      (mapc (lambda (arg) (irc-intrinsic "cc_ensure_valid_object" arg)) args))
;;; If there are < core:+number-of-fixed-arguments+ pad the list up to that
	 (real-args              (if (< nargs core:+number-of-fixed-arguments+)
                                     (append args (make-list (- core:+number-of-fixed-arguments+ nargs)
                                                             :initial-element (null-t-ptr)))
                                     args)))
    real-args))

(defun irc-funcall-results-in-registers (closure args &optional (label ""))
  (let* ((entry-point         (irc-calculate-entry closure))   ; Calculate the function pointer
         (real-args           (irc-calculate-real-args args))  ; fill in NULL for missing register arguments
         (result-in-registers (irc-call-or-invoke entry-point (list* closure (jit-constant-size_t (length args)) real-args) *current-unwind-landing-pad-dest* label)))
    result-in-registers))

(defun irc-funcall (result closure args &optional (label ""))
  (let ((result-in-registers (irc-funcall-results-in-registers closure args label)))
    (irc-store-result result result-in-registers)))

;----------------------------------------------------------------------

(defun get-primitives ()
  (if (and (boundp '*primitives*) *primitives*)
      *primitives*
      (setf *primitives* (primitives-in-thread))))

(defun throw-if-mismatched-arguments (fn-name args)
  (let* ((info (gethash fn-name (get-primitives)))
         (_ (unless info
              (core:bformat *debug-io* "Unknown primitive %s%N" fn-name)
              (error "Unknown primitive ~a" fn-name)))
         (return-ty (primitive-return-type info))
         (required-args-ty (primitive-argument-types info))
         (passed-args-ty (mapcar #'(lambda (x)
                                     (if (llvm-sys:llvm-value-p x)
                                         (if (llvm-sys:valid x)
                                             (llvm-sys:get-type x)
                                             (progn
                                               (core:bformat *debug-io* "Invalid (NULL pointer value about to be passed to intrinsic function%N")
                                               (error "Invalid (NULL pointer) value ~a about to be passed to intrinsic function ~a" x fn-name)))
                                         (core:class-name-as-string x)))
                                 args))
         (i 0))
    (declare (ignore _))
    (unless (primitive-varargs info)
      (unless (= (length required-args-ty) (length passed-args-ty))
        (error "Constructing call to intrinsic ~a - mismatch in the number of arguments, expected ~a - received ~a"
               fn-name (length required-args-ty) (length passed-args-ty))))
    (mapc #'(lambda (x y z)
              (unless (equal x y)
                (core:bformat *debug-io* "Constructing call to intrinsic %s - mismatch of arg#%s value[%s], expected type %s - received type %s%N" fn-name i z x y)
                (error "Constructing call to intrinsic ~a - mismatch of arg#~a value[~a], expected type ~a - received type ~a" fn-name i z x y))
              (setq i (1+ i)))
          required-args-ty passed-args-ty args)))

(defun irc-create-invoke (entry-point args unwind-dest &optional (label ""))
  ;;  (check-debug-info-setup *irbuilder*)
  (unless unwind-dest (error "unwind-dest should not be nil"))
  (unless (null (stringp entry-point)) (error "entry-point for irc-create-invoke cannot be a string - it is ~a" entry-point))
  (let ((normal-dest (irc-basic-block-create "normal-dest")))
    (if (and unwind-dest (eq (llvm-sys:get-insert-block *irbuilder*) unwind-dest))
        (error "The unwind dest ~a should never be the same as the current block ~a"
               (if unwind-dest
                   (llvm-sys:get-name unwind-dest)
                   "NIL")
               (if (llvm-sys:get-insert-block *irbuilder*)
                   (llvm-sys:get-name (llvm-sys:get-insert-block *irbuilder*))
                   "NIL")))
    (let ((code (llvm-sys:create-invoke *irbuilder* entry-point normal-dest unwind-dest args label)))
      (irc-begin-block normal-dest)
      (unless code (error "irc-create-invoke returning nil"))
      code)))

(defun irc-create-call (entry-point args &optional (label ""))
  ;;(throw-if-mismatched-arguments function-name args)
  (llvm-sys:create-call-array-ref *irbuilder* entry-point args label nil))

(defun irc-create-invoke-default-unwind (function-name args &optional (label ""))
  (or *current-unwind-landing-pad-dest* (error "irc-create-invoke-default-unwind was called when *current-unwind-landing-pad-dest* was NIL - check the outer with-landing-pad macro"))
  (irc-create-invoke function-name args *current-unwind-landing-pad-dest* label))

(defun irc-call-or-invoke (function args &optional (landing-pad *current-unwind-landing-pad-dest*) (label ""))
                                        ;  (bformat t "irc-call-or-invoke function: %s%N" function)
  (if landing-pad
      (progn
        (irc-create-invoke function args landing-pad label))
      (progn
        (irc-create-call function args label))))

(defun irc-intrinsic-call-or-invoke (function-name args &optional (label "") (landing-pad *current-unwind-landing-pad-dest*))
  "landing-pad is either a landing pad or NIL (depends on function)"
  (throw-if-mismatched-arguments function-name args)
  (multiple-value-bind (the-function primitive-info)
      (get-or-declare-function-or-error *the-module* function-name)
    (let* ((function-throws (not (llvm-sys:does-not-throw the-function)))
           (code            (cond
                              ((and landing-pad function-throws)
                               (irc-create-invoke the-function args landing-pad label))
                              (t
                               (irc-create-call the-function args label))))
           (_               (when (llvm-sys:does-not-return the-function)
                              (irc-unreachable)
                              (irc-begin-block (irc-basic-block-create "from-invoke-that-never-returns")))))
      #+(or)(warn "Does add-param-attr work yet")
      ;;; FIXME: Do I need to add attributes to the return value of the call
      (dolist (index-attributes (primitive-argument-attributes primitive-info))
        (let ((index (car index-attributes))
              (attributes (cdr index-attributes)))
          (dolist (attribute attributes)
            (llvm-sys:add-param-attr code index attribute))))
      code)))

(defun irc-intrinsic-call (function-name args &optional (label ""))
  (irc-intrinsic-call-or-invoke function-name args label nil))

(defun irc-intrinsic-invoke (function-name args &optional (landing-pad *current-unwind-landing-pad-dest*) (label ""))
  (irc-intrinsic-call-or-invoke function-name args label landing-pad))
  
(defun irc-intrinsic (function-name &rest args &aux (label ""))
  (let* ((last-arg (car (last args)))
	 (real-args args))
    (when (stringp last-arg)
      (setq real-args (nbutlast args))
      (setq label last-arg))
    (irc-intrinsic-call-or-invoke function-name args label *current-unwind-landing-pad-dest*)))

;; Helper functions



(defun irc-verify-module (module return-action)
  (when *verify-llvm-modules*
    (llvm-sys:verify-module module return-action)))

(defun irc-verify-module-safe (module)
  (multiple-value-bind (found-errors error-message)
      (progn
        (cmp-log "About to verify module prior to writing bitcode%N")
        (irc-verify-module *the-module* 'llvm-sys::return-status-action))
    (cmp-log "Done verify module%N")
    (if found-errors
        (progn
          (format t "Module error: ~a~%" error-message)
          (break "Verify module found errors")))))

(defun irc-verify-function (fn &optional (continue t))
  (when *verify-llvm-functions*
    (cmp-log "At top of irc-verify-function  ---- about to verify-function - if there is a problem it will not return%N")
    (multiple-value-bind (failed-verify error-msg)
        (llvm-sys:verify-function fn)
      (if failed-verify
          (progn
            (bformat t "!!!!!!!!!!! Function in module failed to verify !!!!!!!!!!!!!!!!!!!%N")
            (bformat t "---------------- dumping function to assist in debugging%N")
            (cmp:dump-function fn)
            (bformat t "!!!!!!!!!!! ------- see above ------- !!!!!!!!!!!!!!!!!!!%N")
            (bformat t "llvm::verifyFunction error[%s]%N" error-msg)
            (if continue
                (break "Error when trying to verify-function")
                (error "Failed function verify")))
          (cmp-log "--------------  Function verified OK!!!!!!!%N")))))

(defun declare-function-in-module (module dispatch-name primitive-info)
  (let ((return-ty (primitive-return-type primitive-info))
        (argument-types (primitive-argument-types primitive-info))
        (return-attributes (primitive-return-attributes primitive-info))
        (argument-attributes (primitive-argument-attributes primitive-info))
        (varargs (getf (primitive-properties primitive-info) :varargs))
        (does-not-throw (getf (primitive-properties primitive-info) :does-not-throw))
        (does-not-return (getf (primitive-properties primitive-info) :does-not-return))
        fnattrs)
    (when does-not-throw (push 'llvm-sys:attribute-no-unwind fnattrs))
    (when does-not-return (push 'llvm-sys:attribute-no-return fnattrs))
    (push '("no-frame-pointer-elim" "true") fnattrs)
    (push "no-frame-pointer-elim-non-leaf" fnattrs)
    (let ((function (cmp:irc-function-create (llvm-sys:function-type-get return-ty argument-types varargs)
                                             'llvm-sys::External-linkage
                                             dispatch-name
                                             module
                                             :function-attributes fnattrs)))
      #+(or)(bformat t "Created function: %s arg-ty: %s%N" function argument-types)
      (when return-attributes
        (dolist (attribute return-attributes)
          (llvm-sys:add-return-attr function attribute)))
      (dolist (index-attributes argument-attributes)
        (let ((index (car index-attributes))
              (attributes (cdr index-attributes)))
          (dolist (attribute attributes)
            (llvm-sys:add-param-attr function index attribute))))
      function)))

(defun get-or-declare-function-or-error (module name)
  (let ((info (gethash name (get-primitives))))
    #++(bformat t "   --> %s%N" info)
    (unless info (error "Could not find function ~a in *primitives*" name))
    (let ((dispatch-name name))
      (let ((func (llvm-sys:get-function module dispatch-name)))
        (unless func
          (setf func (declare-function-in-module module dispatch-name info)))
        #++(bformat t "     FUNCTION -> %s%N" func)
        (values func info)))))

(defun irc-global-symbol (sym env)
  "Return an llvm GlobalValue for a symbol"
  (irc-load (literal:compile-reference-to-literal sym)))

(defun irc-global-setf-symbol (sym env)
  "Return an llvm GlobalValue for a function name of the form (setf XXXX).
   Pass XXXX as the sym to this function."
  (irc-load (literal:compile-reference-to-literal sym)))

(defun irc-symbol-value-ref (env sym)
  "Return a reference to the symbol-value"
  (irc-intrinsic "symbolValueReference" (irc-global-symbol sym env)))



(defun irc-environment-activation-frame (env)
  (if env
      (environment-activation-frame env)
      nil))
