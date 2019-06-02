
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

(defun irc-gep-variable (array indices &optional (label "gep"))
  (llvm-sys:create-in-bounds-gep *irbuilder* array indices label))

(defun irc-in-bounds-gep-type (type value indices &optional (label "gep"))
  (llvm-sys:create-in-bounds-geptype *irbuilder* type value indices label))

(defun irc-exception-typeid** (name)
  (exception-typeid*-from-name name))

(defun irc-exception-typeid* (name)
  (exception-typeid*-from-name name))

(defun irc-set-function-for-environment (env fn)
  (setf-metadata env :function fn))

(defun irc-get-function-for-environment (env)
  (lookup-metadata env :function))


(defun irc-new-unbound-function-value-environment (old-env &key number-of-functions (label "function-frame"))
  "Create a new function environment and a new runtime environment"
  (let* ((new-env (make-function-value-environment number-of-functions old-env))
	 (new-renv (alloca-af* :label label)))
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
	 (new-renv (alloca-af* :label label)))
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
	 (new-renv (alloca-af* :label label)))
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
	 (new-renv (alloca-af* :label label)))
    (or new-renv (error "The new-renv is nil - it shouldn't be"))
    (irc-set-renv new-env new-renv)
    new-env))

(defun irc-make-block-environment-set-parent (name parent-env)
  (let* ((block-env (make-block-environment name parent-env))
         (new-renv (alloca-af* :label "block-renv"))
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

(defun irc-set-renv (env renv)
  (set-runtime-environment env renv))

(defun irc-renv (env)
  (let ((renv (runtime-environment (current-visible-environment env))))
    (if renv
	(progn
	  (cmp-log "Returning non-nil renv%N")
	  renv)
	(let ((nil-renv (literal:compile-reference-to-literal nil))) ;; (irc-intrinsic "activationFrameNil")))
	  (cmp-log "Returning nil renv: %s%N" nil-renv)
	  nil-renv))))

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
    (irc-t*-result instr new-renv)
    instr))


(defun irc-set-parent (new-renv parent-env)
  (let ((visible-ancestor-environment (current-visible-environment parent-env t)))
    (if (core:function-container-environment-p visible-ancestor-environment)
        (progn
          (error "Only value-frames should directly access the function-container-environment")
          #+(or)(let ((closure (core:function-container-environment-closure visible-ancestor-environment)))
                  (irc-intrinsic "setParentOfActivationFrameFromClosure"
                                 new-renv
                                 closure)))
        (let ((parent-renv2 (irc-renv visible-ancestor-environment)))
          (irc-intrinsic "setParentOfActivationFrame"
                         new-renv
                         (irc-load parent-renv2))))))

(defun irc-parent-renv (env)
  (let ((renv (runtime-environment (current-visible-environment (get-parent-environment env)))))
    (if renv
	(progn
	  (cmp-log "Returning non-nil renv%N")
	  renv)
	(let ((nil-renv (literal:compile-reference-to-literal nil))) ;; (irc-intrinsic "activationFrameNil")))
	  (cmp-log "Returning nil renv: %s%N" nil-renv)
	  nil-renv))))

(defun irc-size_t (num)
  (jit-constant-size_t num))

(defun irc-literal (lit &optional (label "literal"))
  (irc-load (literal:compile-reference-to-literal lit) label))

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

;; ---------------------------------------------------------------------------------------
;;
;; Environment unwinding
;;


(defun irc-unwind-unwind-protect-environment (env)
  (let ((unwind-form (unwind-protect-environment-cleanup-form env))
	(unwind-result (alloca-t*)))
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

(defun irc-basic-block-create (name &optional (function *current-function*))
  "Create a llvm::BasicBlock with (name) in the (function)"
  (llvm-sys:basic-block-create *llvm-context* name function))

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
      (unless (llvm-sys:function-equal irbuilder-cur-function theblock-function)
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

(defun irc-fdefinition (symbol &optional (label ""))
  (let* ((untagged-symbol (irc-untag-general symbol %symbol*%))
         (fdefinition (irc-load (irc-struct-gep %symbol% untagged-symbol +symbol.function-index+))))
    fdefinition))

(defun irc-setf-fdefinition (symbol &optional (label ""))
  (let* ((untagged-symbol (irc-untag-general symbol %symbol*%))
         (setf-fdefinition (irc-load (irc-struct-gep %symbol% untagged-symbol +symbol.setf-function-index+))))
    setf-fdefinition))

(defun irc-untag-general (tagged-ptr &optional (type %t*%))
  #+(or)(let* ((ptr-i8* (irc-bit-cast tagged-ptr %i8*%))
         (ptr-untagged (irc-gep ptr-i8* (list (- +general-tag+)))))
          (irc-bit-cast ptr-untagged type))
  (let* ((ptr-int (irc-ptr-to-int tagged-ptr %uintptr_t%))
         (ptr-adjusted (irc-sub ptr-int (jit-constant-i64 1))))
    (irc-int-to-ptr ptr-adjusted type)))

(defun irc-untag-cons (tagged-ptr &optional (type %cons*%))
  (let* ((ptr-i8* (irc-bit-cast tagged-ptr %i8*%))
         (ptr-untagged (irc-gep ptr-i8* (list (- +cons-tag+)))))
    (irc-bit-cast ptr-untagged type)))

(defun irc-int-to-ptr (val ptr-type &optional (label "inttoptr"))
  (llvm-sys:create-int-to-ptr *irbuilder* val ptr-type label))

(defun irc-untag-fixnum (t* fixnum-type &optional (label "fixnum"))
  "Given a T* fixnum llvm::Value, returns a Value of the given type
representing the fixnum with the tag shaved off."
  (irc-lshr (irc-ptr-to-int t* fixnum-type) +fixnum-shift+
            :exact t ; fixnum tag is zero.
            :label label))

(defun irc-tag-fixnum (int &optional (label "fixnum"))
  "Given an llvm::Value of integer type, returns a T* value
representing a tagged fixnum."
  ;; :NUW T tells LLVM the top bits are zero, i.e. no overflow is possible.
  ;; NOTE: It's okay if the int is short (e.g. a bit) as inttoptr zexts.
  ;; (If the int is too long, it truncates - don't think we ever do that, though)
  (irc-int-to-ptr (irc-shl int +fixnum-shift+ :nuw t) %t*% label))

(defun irc-maybe-cast-integer-to-t* (val &optional (label "fixnum-to-t*"))
  "If it's a fixnum then cast it - otherwise just return it - it should already be a t*"
  (if (typep val '(integer #.(- (expt 2 63)) #.(- (expt 2 63) 1)))
      (llvm-sys:create-int-to-ptr *irbuilder* (jit-constant-i64 val) %t*% label)
      (if (llvm-sys:type-equal (llvm-sys:get-type val) %t*%)
          val
          (error "The val ~s type ~s is not a t* or fixnum " val (type-of val)))))


(defun irc-rack (instance-tagged)
  (let* ((instance* (irc-untag-general instance-tagged %instance*%))
         (rack (irc-load (irc-struct-gep %instance% instance* +instance.rack-index+) "rack-tagged")))
    rack))

(defun irc-instance-slot-address (instance index)
  "Return a %t**% a pointer to a slot in the rack of an instance"
  (let* ((rack-tagged (irc-rack instance))
         (rack* (irc-untag-general rack-tagged %rack*%))
         (data0* (irc-struct-gep %rack% rack* +rack.data-index+))
         (dataN* (irc-gep data0* (list 0 index))))
    dataN*))

(defun irc-read-slot (instance index)
  "Read a value from the rack of an instance"
  (let ((dataN* (irc-instance-slot-address instance index)))
    (irc-load dataN*)))

(defun irc-write-slot (instance index value)
  "Write a value into the rack of an instance"
  (let ((dataN* (irc-instance-slot-address instance index)))
    (irc-store value dataN*)))

(defun irc-value-frame-parent (value-frame)
  "Return the parent of the value-frame"
  (let* ((value-frame* (irc-untag-general value-frame))
         (parent* (irc-struct-gep %value-frame% value-frame* +value-frame.parent-index+)))
    (irc-load parent*)))

(defun irc-nth-value-frame-parent (value-frame depth)
  "Return the nth parent of the value-frame"
  (let ((cur value-frame))
    (dotimes (idx depth)
      (setq cur (irc-value-frame-parent cur)))
    cur))

(defun irc-value-frame-value (value-frame index)
  "Return the nth cell of the value-frame"
  (let* ((value-frame* (irc-untag-general value-frame))
         (data0* (irc-struct-gep %value-frame% value-frame* +value-frame.data-index+))
         (dataN* (irc-gep data0* (list 0 index))))
    (irc-load dataN*)))

(defun irc-depth-index-value-frame (value-frame depth index)
  (let* ((depth-value-frame (irc-nth-value-frame-parent value-frame depth)))
    (irc-value-frame-value depth-value-frame index)))
  
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

(defun irc-srem (lhs rhs &optional (label ""))
  (llvm-sys:create-srem *irbuilder* lhs rhs label))

(defun irc-udiv (dividend divisor &key (label "") exact)
  (llvm-sys:create-udiv cmp:*irbuilder* dividend divisor label exact))
(defun irc-urem (dividend divisor &key (label ""))
  (llvm-sys:create-urem cmp:*irbuilder* dividend divisor label))

(defun irc-shl (value shift &key (label "") nuw nsw)
  "If shift is an integer, generate shl with a constant uint64.
Otherwise do a variable shift."
  (if (integerp shift)
      (llvm-sys:create-shl-value-uint64
       cmp:*irbuilder* value shift label nuw nsw)
      (llvm-sys:create-shl-value-value
       cmp:*irbuilder* value shift label nuw nsw)))

(defun irc-lshr (value shift &key (label "") exact)
  "If shift is an integer, generate lshr with a constant uint64.
Otherwise do a variable shift."
  (if (integerp shift)
      (llvm-sys:create-lshr-value-uint64
       cmp:*irbuilder* value shift label exact)
      (llvm-sys:create-lshr-value-value
       cmp:*irbuilder* value shift label exact)))

(defun irc-load (source &optional (label ""))
  (llvm-sys:create-load-value-twine *irbuilder* source label))

(defun irc-store (val destination &optional (label "") (is-volatile nil))
  ;; Mismatch in store type sis a very common bug we hit when rewriting codegen.
  ;; LLVM doesn't deal with it gracefully except with a debug build, so we just
  ;; check it ourselves. We also check that types are in the same context-
  ;; another reasonably common issue.
  ;; FIXME: Label is unused - llvm store doesn't return a value so it can't have one.
  ;; Fix code so it doesn't pass a label here, if possible, then remove the parameter.
  (let ((val-type (llvm-sys:get-type val))
        (dest-contained-type (llvm-sys:get-contained-type (llvm-sys:get-type destination) 0)))
    (cond ((llvm-sys:type-equal val-type dest-contained-type)
           (llvm-sys:create-store *irbuilder* val destination is-volatile))
          ((llvm-sys:llvmcontext-equal
            (llvm-sys:get-context val-type) (llvm-sys:get-context dest-contained-type))
           (error "BUG: Mismatch in irc-store between val type ~a and destination contained type ~a%N"
                  val-type dest-contained-type))
          (t
           (error "BUG: Mismatch in irc-store involving the val type ~a and destination type ~a -
the type LLVMContexts don't match - so they were defined in different threads!"
                  val-type dest-contained-type)))))

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
        (function-metadata-name (gensym))
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
           (with-new-function-prepare-for-try (,fn)
             (with-try
                 (with-dbg-function (,function-name
                                  :lineno (core:source-pos-info-lineno core:*current-source-pos-info*)
                                  :linkage-name *current-function-name*
                                  :function ,fn
                                  :function-type ,function-type)
                   (with-dbg-lexical-block (:lineno (core:source-pos-info-lineno core:*current-source-pos-info*))
                     (when core:*current-source-pos-info*
                       (let ((lineno (core:source-pos-info-lineno core:*current-source-pos-info*)))
                         (dbg-set-irbuilder-source-location-impl ,irbuilder-alloca lineno 0 *dbg-current-scope*)
                         (dbg-set-irbuilder-source-location-impl ,irbuilder-body lineno 0 *dbg-current-scope*)))
                     (with-irbuilder (*irbuilder-function-body*)
                       (or *the-module* (error "with-new-function *the-module* is NIL"))
                       (cmp-log "with-landing-pad around body%N")
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

(defstruct (function-info (:type vector) :named
                          (:constructor %make-function-info
                              (function-name lambda-list docstring declares form
                               source-pathname lineno column filepos)))
  function-name
  lambda-list docstring declares form
  source-pathname lineno column filepos)

(defun make-function-info (&key function-name lambda-list docstring declares form
                             lineno column filepos)
  ;; FIXME: *current-source-pos-info* could be installed already knowing about offsets
  ;; and such, thus leaving the handling waaaaay up in compile-file.
  (%make-function-info function-name lambda-list docstring declares form
                       *source-debug-pathname* lineno column
                       (+ filepos *source-debug-offset*)))

(defconstant +maxi32+ 4294967295)

(defun irc-create-function-description (llvm-function-name fn module function-info)
  "If **generate-code** then create a function-description block from function info.
    Otherwise we are code-walking - and do something else that is appropriate."
  (unless function-info
    (error "function info is NIL for ~a" llvm-function-name))
  (let ((function-name (function-info-function-name function-info))
        (source-pathname (function-info-source-pathname function-info))
        source-info-name
        (lambda-list (function-info-lambda-list function-info))
        (docstring (function-info-docstring function-info))
        (lineno (function-info-lineno function-info))
        (column (function-info-column function-info))
        (filepos (function-info-filepos function-info))
        (declares (function-info-declares function-info)))
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
        (core:bformat t "source-pathname: %s%N" source-pathname)
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
      (let* ((source-pathname.function-name-index (literal:reference-literal (cons source-pathname function-name)))
             (lambda-list.docstring-index (literal:reference-literal (cons lambda-list docstring))))
        #+(or)
        (progn
          (core:bformat t "source-pathname.function-name-index: %s%N" source-pathname.function-name-index)
          (core:bformat t "lambda-list.docstring-index: %s%N" lambda-list.docstring-index))
        (unless lambda-list.docstring-index (error "There is no lambda-list.docstring-index"))
        (unless lineno (error "There is no lineno"))
        (unless column (error "There is no column"))
        (unless filepos (error "There is no filepos"))
        (llvm-sys:make-global-variable
         module
         %function-description%
         t
         'llvm-sys:internal-linkage
         (llvm-sys:constant-struct-get %function-description%
                                       (progn
                                         (progn
                                           (when (> lineno +maxi32+) (error "lineno ~a out of bounds for i32" lineno))
                                           (when (> column +maxi32+) (error "column ~a out of bounds for i32" column))
                                           (when (> filepos +maxi32+) (error "filepos ~a out of bounds for i32" filepos)))
                                         (list
                                          fn
                                          literal::*gcroots-in-module*
                                          (jit-constant-size_t source-pathname.function-name-index)
                                          (jit-constant-size_t lambda-list.docstring-index)
                                          (jit-constant-i32 lineno)
                                          (jit-constant-i32 column)
                                          (jit-constant-i32 filepos))))
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
    (let ((result (let ((*irbuilder-function-alloca* irbuilder-alloca)) (alloca-tmv "result"))))
      (values fn func-env cleanup-block irbuilder-alloca irbuilder-body result fn-description))))


(defun irc-cclasp-function-create (llvm-function-type linkage llvm-function-name module function-info)
  "Create a function and a function description for a cclasp function"
  (let* ((fn (irc-function-create llvm-function-type linkage llvm-function-name module))
         (fn-description (irc-create-function-description llvm-function-name fn module function-info)))
    (values fn fn-description)))

(defun irc-cleanup-function-environment (env)
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

(defun irc-bit-cast (from totype &optional (label "bit-cast"))
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

(defmacro with-irbuilder ((irbuilder) &rest code)
  "Set *irbuilder* to the given IRBuilder"
  `(let ((*irbuilder* ,irbuilder))
     (cmp-log "Switching to irbuilder --> %s%N" (bformat nil "%s" *irbuilder*))
     (multiple-value-prog1 (progn ,@code)
       (cmp-log "Leaving irbuilder --> %s%N" (bformat nil "%s" *irbuilder*)))))

;;; ALLOCA functions

(defun alloca (type size &optional (label ""))
  (let ((alloca
          (llvm-sys:create-alloca *irbuilder-function-alloca*
                                  type (jit-constant-i32 size) label)))
    (llvm-sys:set-alignment alloca 8) ; 8-byte alignment
    alloca))

(defun alloca-return (&optional (label "")) (alloca %return-type% 1 label))
(defun alloca-t* (&optional (label "")) (alloca %t*% 1 label))
(defun alloca-tmv (&optional (label "")) (alloca %tmv% 1 label))
(defun alloca-af* (&key (label "")) (alloca %af*% 1 label))
(defun alloca-i8 (size &optional (label "var")) (alloca %i8% size label))
(defun alloca-i8* (&optional (label "i8*-")) (alloca %i8*% 1 label))
(defun alloca-i32 (&optional (label "i32-")) (alloca %i32% 1 label))
(defun alloca-va_list (&optional (label "va_list")) (alloca %va_list% 1 label))
(defun alloca-size_t (&optional (label "var")) (alloca %size_t% 1 label))
(defun alloca-vaslist (&key (label "va_list")) (alloca %vaslist% 2 label))

(defun alloca-dx-list (&key length (label "dx-list"))
  ;; Unlike most allocas, we want dx object allocas to be done inline with the code,
  ;; as the length will have been computed at runtime. Kinda like a C VLA.
  ;; So we use *irbuilder*, and don't send the length through constantization.
  (llvm-sys:create-alloca *irbuilder* %cons% length label))

(defun alloca-temp-values (size &optional (label "temp-values"))
  ;; Also VLA
  (llvm-sys:create-alloca *irbuilder* %t*% size label))

(defun irc-register-save-area (&key (irbuilder *irbuilder-function-alloca*) (label "va_list"))
  "Alloca space for a register save area, and keep it in the stack map."
  (with-irbuilder (irbuilder)
    (let ((rsa
            (llvm-sys:create-alloca *irbuilder* %register-save-area% (jit-constant-size_t 1) label)))
      (irc-intrinsic "llvm.experimental.stackmap" (jit-constant-i64 1234567) (jit-constant-i32 0)
                     rsa)
      rsa)))

(defun null-t-ptr ()
  (llvm-sys:constant-pointer-null-get %t*%))

(defun undef-t-ptr ()
  (llvm-sys:undef-value-get %t*%))

(defun irc-struct-gep (type struct idx &optional (label ""))
  (llvm-sys:create-struct-gep *irbuilder* type struct idx label ))

(defun irc-insert-value (struct val idx-list &optional (label ""))
  (llvm-sys:create-insert-value *irbuilder* struct val idx-list label))

(defun irc-extract-value (struct idx-list &optional (label ""))
  ;; Sanity check - maybe unnecessary?
  (let ((struct-type (llvm-sys:get-type struct)))
    (when (or (llvm-sys:type-equal struct-type %t*%)
              (llvm-sys:type-equal struct-type %t**%))
      (error "You cannot extract from simple type ~s value ~s" struct-type struct)))
  (llvm-sys:create-extract-value *irbuilder* struct idx-list label))

(defun irc-smart-ptr-extract (smart-ptr &optional (label ""))
  "Extract the t-ptr from the smart-ptr"
  (unless (llvm-sys:type-equal (llvm-sys:get-type smart-ptr) %tsp%)
    (error "The argument ~s is not a tsp" smart-ptr))
  
  (irc-extract-value smart-ptr (list 0) label))

(defun irc-t*-result (t* result)
  (let ((return-type (llvm-sys:get-type result)))
    (cond ((llvm-sys:type-equal return-type %t**%)
           (irc-store t* result))
          ((llvm-sys:type-equal return-type %tmv*%)
           (let* ((undef (llvm-sys:undef-value-get %tmv%))
                  (ret-tmv0 (llvm-sys:create-insert-value *irbuilder* undef t* '(0) "ret0"))
                  (one (jit-constant-size_t 1))
                  (ret-tmv1 (llvm-sys:create-insert-value *irbuilder* ret-tmv0 one '(1) "nret")))
             (irc-store ret-tmv1 result)))
          (t (error "Unknown return-type in irc-t*-result")))))

(defun irc-tmv-result (tmv result)
  (let ((return-type (llvm-sys:get-type result)))
    (cond ((llvm-sys:type-equal return-type %t**%)
           (let ((primary (irc-extract-value tmv (list 0))))
             (irc-store primary result)))
          ((llvm-sys:type-equal return-type %tmv*%)
           (irc-store tmv result))
          (t (error "Unknown return-type in irc-tmv-result")))))

(defun irc-tsp-result (tsp result)
  (irc-t*-result (irc-smart-ptr-extract tsp) result))


(defun irc-calculate-entry (closure &optional (label "entry-point-gep"))
  (let* ((closure-i8*           (irc-bit-cast closure %i8*%))
         (entry-point-addr-i8*  (irc-gep closure-i8* (list (- +closure-entry-point-offset+ +general-tag+))))
         (entry-point-addr-fp** (irc-bit-cast entry-point-addr-i8* %fn-prototype**%))
         (entry-point           (irc-load entry-point-addr-fp** (core:bformat nil "%s-gep" label))))
    entry-point))


#+(or)
(defun irc-calculate-entry-ptr (closure &optional (label "entry-point"))
  (let* ((closure-uintptr        (irc-ptr-to-int closure %uintptr_t%))
         (entry-point-addr-uint  (irc-add closure-uintptr (jit-constant-uintptr_t (- +closure-entry-point-offset+ +general-tag+)) "entry-point-addr-uintff"))
         (entry-point-addr       (irc-int-to-ptr entry-point-addr-uint %fn-prototype**% "entry-point-addr"))
         (entry-point            (irc-load entry-point-addr label)))
    entry-point))

#+(or)
(progn
  (defparameter *use-gep* nil)
  (defun irc-calculate-entry (closure &optional (label "entry-point"))
    (if *use-gep*
        (irc-calculate-entry-gep closure label)
        (irc-calculate-entry-ptr closure label))))

;;; Our present convention is that Lisp functions uniformly have
;;; (closure nargs arg0 .. argm ...) as parameters, where m is
;;; core:+number-of-fixed-arguments+; the final ... will be a va_list,
;;; which is more expensive.
;;; If we want to pass fewer than four arguments, we still need to pass
;;; enough to match the type; since the function will check nargs and
;;; ignore anything past, undef is fine. (NULL results in pointless
;;; register zeroing.)
(defun irc-calculate-real-args (args)
  (let* ((nargs                  (length args))
         #+debug-guard-exhaustive-validate
         (_                      (mapc (lambda (arg) (irc-intrinsic "cc_ensure_valid_object" arg)) args))
;;; If there are < core:+number-of-fixed-arguments+ pad the list up to that
	 (real-args              (if (< nargs core:+number-of-fixed-arguments+)
                                     (append args (make-list (- core:+number-of-fixed-arguments+ nargs)
                                                             :initial-element (undef-t-ptr)))
                                     args)))
    real-args))

(defun irc-funcall-results-in-registers (closure args &optional (label ""))
  (let* ((entry-point         (irc-calculate-entry closure label))
         (real-args           (irc-calculate-real-args args))  ; fill in NULL for missing register arguments
         (result-in-registers (irc-call-or-invoke entry-point (list* closure (jit-constant-size_t (length args)) real-args) *current-unwind-landing-pad-dest*)))
    result-in-registers))

(defun irc-funcall (result closure args &optional label)
  (unless label
    (setf label "unlabeled-function"))
  (let ((result-in-registers (irc-funcall-results-in-registers closure args label)))
    (irc-tmv-result result-in-registers result)))

;----------------------------------------------------------------------

(defun get-primitives ()
  (if (and (boundp '*primitives*) *primitives*)
      *primitives*
      (setf *primitives* (primitives-in-thread))))

(defun throw-if-mismatched-arguments (fn-name args)
  (let* ((info (gethash fn-name (get-primitives)))
         (_ (unless info
              (error "Unknown primitive ~a" fn-name)))
         (return-ty (primitive-return-type info))
         (required-args-ty (primitive-argument-types info))
         (passed-args-ty (mapcar #'(lambda (x)
                                     (if (llvm-sys:llvm-value-p x)
                                         (if (llvm-sys:valid x)
                                             (llvm-sys:get-type x)
                                             (progn
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
              (unless (llvm-sys:type-equal x y)
                (error "Constructing call to intrinsic ~a - mismatch of arg#~a value[~a], expected type ~a - received type ~a" fn-name i z x y))
              (setq i (1+ i)))
          required-args-ty passed-args-ty args)))

(defun irc-create-invoke (entry-point args unwind-dest &optional (label ""))
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
  #+debug-compiler(throw-if-mismatched-arguments function-name args)
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
  (llvm-sys:verify-module module return-action))

(defun irc-verify-module-safe (module)
  (when *verify-llvm-modules*
    (core:bformat t "Entered irc-verify-module-safe%N")
    (llvm-sys:dump-module module)
    (multiple-value-bind (found-errors error-message)
        (progn
          (cmp-log "About to verify module prior to writing bitcode%N")
          (irc-verify-module *the-module* 'llvm-sys::return-status-action))
      (cmp-log "Done verify module%N")
      (core:bformat t "irc-verify-module-safe found-errors: %s  message: %s%N" found-errors error-message)
      (if found-errors
          (progn
            (format t "Module error: ~a~%" error-message)
            (error "Verify module found errors"))))))

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
  (multiple-value-bind (ref literal-name)
      (literal:compile-reference-to-literal sym)
    (let ((label (if literal-name
                     literal-name
                     "")))
      (values (irc-load ref label) label))))

(defun irc-global-setf-symbol (sym env)
  "Return an llvm GlobalValue for a function name of the form (setf XXXX).
   Pass XXXX as the sym to this function."
  (irc-load (literal:compile-reference-to-literal sym)))

(defun irc-environment-activation-frame (env)
  (if env
      (environment-activation-frame env)
      nil))
