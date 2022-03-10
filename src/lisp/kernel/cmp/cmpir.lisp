
;;
;;;    File: cmpir.lisp
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


(defstruct (xep-arity (:type vector) :named)
  "This describes one arity/entry-point for a 'xep-group'.  
arity: - the arity of the function (:general-entry|0|1|2|3...|5) 
function-or-placeholder - the llvm function or a placeholder for 
                          the literal compiler to generate a pointer 
                          to a fixed arity trampoline. "
  arity ; arity of this entry point (:general-entry or an integer 0...n)
  function-or-placeholder ; The function object for this entry point
  )

(defun arity-code (arity)
  (cond
    ((eq arity :general-entry)
     0)
    (t (1+ arity))))

(defstruct (cleavir-lambda-list-analysis (:type vector) :named)
  "An analysis of the cleavir lambda list.  It breaks down the cleavir lambda-list
into parts with a layout that comes from ECL's 'parse_lambda_list' function.
That layout looks like:
! Process the arguments and return the components
 * context may be: ordinary, macro, destructuring, deftype,
 * define_method_combination, defsetf  HOWEVER ECL>>clos/method.lisp:line 402 passes T!!!!
 * and determines the
 * valid sytax. The output is made of several values:
 *
 * VALUES(0) = (N req1 ... )			; required values
 * VALUES(1) = (N opt1 init1 flag1 ... )	; optional values
 * VALUES(2) = rest-var				; rest-variable, if any
 * VALUES(3) = key-flag				; T if &key was supplied
 * VALUES(4) = (N key1 var1 init1 flag1 ... )	; keyword arguments
 * VALUES(5) = allow-other-keys			; flag &allow-other-keys
 * VALUES(6) = (aux1 init1 ... )		; auxiliary variables - pairs of auxN initN
 * Values(7) = whole-var			; whole-variable, if any
 * Values(8) = environment-var			; environment-variable, if any
 *
 * 1) The prefix 'N' is an integer value denoting the number of
 * variables which are declared within this section of the lambda
 * list.
 *
 * 2) The INIT* arguments are lisp forms which are evaluated when
 * no value is provided.
 *
 * 3) The FLAG* arguments is the name of a variable which holds a
 * boolean value in case an optional or keyword argument was
 * provided. If it is NIL, no such variable exists.
 *
 * Return true if bindings will be defined and false if not.
"
  cleavir-lambda-list
  req-opt-only-p
  (lambda-list-arguments nil)
  (required (list 0)) ; default zero required
  (optional (list 0)) ; default zero optional
  rest
  key-flag
  key-count
  aok-p
  aux-p
  va-rest-p)

(defun cleavir-lambda-list-analysis-min-nargs (cll-analysis)
  (car (cleavir-lambda-list-analysis-required cll-analysis)))

(defun cleavir-lambda-list-analysis-max-nargs (cll-analysis)
  (+ (cleavir-lambda-list-analysis-min-nargs cll-analysis)
     (car (cleavir-lambda-list-analysis-optional cll-analysis))))

(defun process-cleavir-lambda-list-analysis (cll-analysis)
  (values
   (cleavir-lambda-list-analysis-required cll-analysis)
   (cleavir-lambda-list-analysis-optional cll-analysis)
   (cleavir-lambda-list-analysis-rest cll-analysis)
   (cleavir-lambda-list-analysis-key-flag cll-analysis)
   (cleavir-lambda-list-analysis-key-count cll-analysis)
   (cleavir-lambda-list-analysis-aok-p cll-analysis)
   (cleavir-lambda-list-analysis-aux-p cll-analysis)
   (cleavir-lambda-list-analysis-va-rest-p cll-analysis)))

(defun ensure-cleavir-lambda-list (lambda-list)
  "This used to test for cleavir-lambda-lists but that's hard and 
we may not need this anymore.  Just return the argument.
Leave calls to this in the code whereever you need a cleavir-lambda-list do document that we need one.
Maybe in the future we will want to actually put a test here."
  lambda-list ;; (error "ensure-cleavir-lambda-list doesn't know what to do with ~s" lambda-list))
  )

(defun ensure-cleavir-lambda-list-analysis (arg)
  (unless (cleavir-lambda-list-analysis-p arg)
    (error "This ~a is not a cleavir-lambda-list-analysis - it must be" arg))
  arg)

(defun process-bir-lambda-list (lambda-list)
  "Temporary until bir:lambda-list can return a cleavir-lambda-list-analysis"
  (let* ((cleavir-lambda-list (ensure-cleavir-lambda-list lambda-list))
         (cleavir-lambda-list-analysis (calculate-cleavir-lambda-list-analysis cleavir-lambda-list)))
    (process-cleavir-lambda-list-analysis cleavir-lambda-list-analysis)))

(defun generate-function-for-arity-p (arity cll-analysis)
  "Return T if a function should be generated for this arity.
If nil then insert a general_entry_point_redirect_x function"
  (if (eq arity :general-entry)
      t
      (if (cleavir-lambda-list-analysis-req-opt-only-p cll-analysis)
          (let* ((nreq (car (cleavir-lambda-list-analysis-required cll-analysis)))
                 (nopt (car (cleavir-lambda-list-analysis-optional cll-analysis))))
            (not (or (< arity nreq)
                     (< (+ nreq nopt) arity))))
          nil)))
  
(defstruct (xep-group (:type vector) :named)
  "xep-group describes a group of xep functions.
name - the common, unadorned name of the xep function
cleavir-lambda-list-analysis - the cleavir-lambda-list-analysis that applies to the entire xep-group
arities - a list of xep-arity
entry-point-reference - an index into the literal vector that stores the GeneralEntryPoint_O for this xep-group.
local-function - the lcl function that all of the xep functions call."
  name
  cleavir-lambda-list-analysis
  arities
  entry-point-reference
  local-function)

(defun ensure-xep-function-not-placeholder (fn)
  (when (literal:general-entry-placeholder-p fn)
    (error "~a must be a xep-function" fn))
  fn)

(defun xep-group-lookup (xep-group arity)
  (dolist (entry (xep-group-arities xep-group))
    (let ((entry-arity (xep-arity-arity entry)))
      (when (eql entry-arity arity)
        (return-from xep-group-lookup entry))))
  (error "Could not find arity ~a in xep-group" arity))

(defstruct (bclasp-llvm-function-info (:type vector) :named)
  "A bclasp-llvm-function-info stores info about a single xep-group xep function. Do we need all these function-infos?"
  local-function
  xep-function
  function-description-reference)

(defstruct (function-info (:type list) :named
                          (:constructor %make-function-info
                              (function-name
                               lambda-list
                               cleavir-lambda-list-analysis
                               docstring
                               declares
                               source-pathname
                               lineno
                               column
                               filepos)))
  "A function-info stores info about a single xep-group xep function.  Do we need all these function-infos?"
  function-name
  lambda-list cleavir-lambda-list-analysis docstring declares
  source-pathname lineno column filepos)

(defun make-function-info (&key function-name
                             (cleavir-lambda-list-analysis (make-cleavir-lambda-list-analysis))
                             lambda-list
                             docstring
                             declares
                             spi)
  (let ((lineno 0) (column 0) (filepos 0) (source-pathname "-unknown-file-"))
    (when spi
      (setf source-pathname (core:file-scope-pathname
                             (core:file-scope
                              (core:source-pos-info-file-handle spi)))
            lineno (core:source-pos-info-lineno spi)
            ;; FIXME: Why 1+?
            column (1+ (core:source-pos-info-column spi))
            filepos (core:source-pos-info-filepos spi)))
    (%make-function-info function-name lambda-list cleavir-lambda-list-analysis docstring declares
                         source-pathname lineno column filepos)))


(defun irc-single-step-callback ()
  (irc-intrinsic "singleStepCallback" ))

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

(defun irc-arity-info (arity)
  "Return the (values register-save-words entry-index) for the arity"
  (cond
    ((eq arity :general-entry) (values 3 0))
    ((fixnump arity) (values (+ 1 arity) (+ 1 arity)))
    (t (error "irc-arity-info Illegal arity ~a" arity))))

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
  (let ((new-env (irc-new-unbound-function-value-environment old-env :number-of-functions (length functions) :label label)))
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
  (let ((new-env (irc-new-unbound-tagbody-environment old-env :label label)))
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

(defun irc-make-local-block-environment-set-parent (name parent-env)
  "This returns a block environment that is only good for local return-from's"
  (let ((block-env (make-block-environment name parent-env)))
    (core:set-invisible block-env t)
    (values block-env nil nil)))

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
  (jit-constant-size_t (core:source-pos-info-filepos core:*current-source-pos-info*)))
(defun irc-size_t-*current-source-pos-info*-lineno ()
  (jit-constant-size_t (core:source-pos-info-lineno core:*current-source-pos-info*)))
(defun irc-size_t-*current-source-pos-info*-column ()
  (jit-constant-size_t (core:source-pos-info-column *current-source-pos-info*)))

;; ---------------------------------------------------------------------------------------
;;
;; Environment unwinding
;;

(defun irc-environment-has-cleanup (env)
  "Return NIL if the environment doesn't need unwinding."
  (let ((unwind (local-metadata env :unwind)))
    unwind))

(defun irc-unwind-unwind-protect-environment (env)
  (let ((unwind-form (unwind-protect-environment-cleanup-form env))
	(unwind-result (alloca-t*)))
    ;; Generate the unwind-form code in the parent environment of the unwind-protect form
    (codegen unwind-result unwind-form (get-parent-environment env))
    ))

(defun irc-do-unwind-environment (env)
  "Unwind the environment and return whatever was unwound"
  (cmp-log "irc-do-unwind-environment for: %s%N" env)
  (let ((unwind (local-metadata env :unwind)))
    (dolist (cc unwind)
      (let ((head (car cc)))
	(cond
	  ((eq head 'symbolValueRestore)
           (destructuring-bind (cmd symbol alloca) cc
             (declare (ignore cmd))
             (cmp-log "popDynamicBinding of %s%N" symbol)
             (irc-intrinsic "popDynamicBinding" (irc-global-symbol symbol env) alloca)))
	  (t (error (bformat nil "Unknown cleanup code: %s" cc))))))))

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
  (llvm-sys:basic-block-create (thread-local-llvm-context) name function))

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
  #+debug-compiler
  (let ((irbuilder-cur-basic-block (llvm-sys:get-insert-block irbuilder)))
    (when irbuilder-cur-basic-block
      (let* ((irbuilder-cur-function    (llvm-sys:get-parent irbuilder-cur-basic-block))
             (theblock-function         (llvm-sys:get-parent theblock)))
        (unless (llvm-sys:function-equal irbuilder-cur-function theblock-function)
          (error "The IRBuilder ~a that is currently in function ~a is being told to jump functions when its insert point is being set to ~a in ~a" irbuilder irbuilder-cur-function theblock theblock-function)))))
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
          (llvm-sys:create-call-function-pointer *irbuilder* llt (list (jit-constant-i32 *next-low-level-trace-index*)) ""))
        (setq *next-low-level-trace-index* (+ 1 *next-low-level-trace-index*)))
      nil))


(defun irc-begin-landing-pad-block (theblock)
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

(defun irc-cond-br (icond true false &optional branchWeights unpredictable)
  (llvm-sys:create-cond-br *irbuilder* icond true false branchWeights unpredictable))

(defun irc-ptr-to-int (val int-type &optional (label "ptrtoint"))
  (llvm-sys:create-ptr-to-int *irbuilder* val int-type label))

(defun irc-fdefinition (symbol &optional (label ""))
  (irc-load-atomic (c++-field-ptr info.%symbol% symbol :function) :label label))

(defun irc-setf-fdefinition (symbol &optional (label ""))
  (irc-load-atomic (c++-field-ptr info.%symbol% symbol :setf-function)
                   :label label))

(defun irc-untag-general (tagged-ptr &optional (type %t**%))
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

(defun irc-sext (val &optional (destty %fixnum%) (label "sext"))
  (llvm-sys:create-sext *irbuilder* val destty label))

(defun irc-zext (val &optional (destty %fixnum%) (label "sext"))
  (llvm-sys:create-zext *irbuilder* val destty label))

(defun irc-untag-fixnum (t* fixnum-type &optional (label "fixnum"))
  "Given a T* fixnum llvm::Value, returns a Value of the given type
representing the fixnum with the tag shaved off."
  (irc-ashr (irc-ptr-to-int t* fixnum-type) +fixnum-shift+
            :exact t ; fixnum tag is zero.
            :label label))

(defun irc-tag-fixnum (int &optional (label "fixnum"))
  "Given an llvm::Value of integer type, returns a T* value
representing a tagged fixnum."
  ;; :NSW T tells LLVM that the bits shifted out will match the sign bit,
  ;; which is true for fixnums.
  ;; NOTE: It's okay if the int is short (e.g. a bit) as inttoptr zexts.
  ;; (If the int is too long, it truncates - don't think we ever do that, though)
  (irc-int-to-ptr (irc-shl int +fixnum-shift+ :nsw t) %t*% label))

(defun irc-unbox-vaslist (t* &optional (label "vaslist-v*"))
  ;; FIXME: Probably we should untag by masking instead of subttraction
  (let* ((ptr-int (irc-ptr-to-int t* %uintptr_t%))
         (ptr-adjusted (irc-sub ptr-int (jit-constant-i64 +vaslist0-tag+)))
         (ptr-ptr (irc-int-to-ptr ptr-adjusted %vaslist*%)))
    (irc-load ptr-ptr label)))

;;; "tag" rather than "box" because we don't allocate here, just tag an
;;; existing pointer.
(defun irc-tag-vaslist (ptr &optional (label "vaslist-v*"))
  "Given a word aligned ptr, add the vaslist tag"
  (let* ((ptr-i8* (irc-bit-cast ptr %i8*%))
         (ptr-tagged (irc-gep ptr-i8* (list (jit-constant-i64 +vaslist0-tag+)) label)))
    ptr-tagged))

;;; NOTE: Unsafe. Cleavir inserts this only after type checks on safety > 0.
(defun irc-unbox-single-float (t* &optional (label "single-float"))
  (irc-intrinsic-call "cc_unbox_single_float" (list t*) label)
  (irc-bit-cast
   (irc-trunc (irc-lshr (irc-ptr-to-int t* %i64%) +single-float-shift+) %i32%)
   %float% label))

(defun irc-box-single-float (sf &optional (label "single-float"))
  ;; Do it inline since it's pretty trivial
  (irc-int-to-ptr
   (llvm-sys:create-or-value-uint64
    *irbuilder*
    (irc-shl (irc-zext (irc-bit-cast sf %i32%) %i64%) +single-float-shift+)
    +single-float-tag+ "")
   %t*% label))

;;; TODO: Should be unsafe for the same reason as above.
;;;       Checking here is redundant.
;;; TODO: Inline this - it's just a memory load, unlike boxing
(defun irc-unbox-double-float (t* &optional (label "double-float"))
  (irc-intrinsic-call "cc_unbox_double_float" (list t*) label))
(defun irc-box-double-float (double &optional (label "double-float"))
  (irc-intrinsic-call "to_object_double" (list double) label))

(defun irc-maybe-cast-integer-to-t* (val &optional (label "fixnum-to-t*"))
  "If it's a fixnum then cast it - otherwise just return it - it should already be a t*"
  (if (typep val '(integer #.(- (expt 2 63)) #.(1- (expt 2 63))))
      (llvm-sys:create-int-to-ptr *irbuilder* (jit-constant-i64 val) %t*% label)
      (if (llvm-sys:type-equal (llvm-sys:get-type val) %t*%)
          val
          (error "The val ~s type ~s is not a t* or fixnum " val (type-of val)))))


(defun irc-rack-address (instance-tagged)
  (let ((instance* (irc-untag-general instance-tagged %instance*%)))
    (irc-struct-gep %instance% instance* +instance.rack-index+)))

(defun irc-rack (instance-tagged)
  (irc-load-atomic (irc-rack-address instance-tagged) :label "rack-tagged"))

(defun irc-rack-set (instance-tagged rack)
  (irc-store-atomic rack (irc-rack-address instance-tagged)))

(defun irc-rack-slot-address (rack-tagged index)
  (let* ((rack* (irc-untag-general rack-tagged %rack*%))
         ;; Address of the start of the data vector.
         (data0* (irc-struct-gep %rack% rack* +rack.data-index+)))
    (irc-gep data0* (list 0 index))))

(defun irc-instance-slot-address (instance index)
  "Return a %t**% a pointer to a slot in the rack of an instance"
  (irc-rack-slot-address (irc-rack instance) index))

(defun irc-rack-read (rack index &key (order 'llvm-sys:monotonic))
  (irc-load-atomic (irc-rack-slot-address rack index) :order order))

(defun irc-rack-write (rack index value &key (order 'llvm-sys:monotonic))
  (irc-store-atomic value (irc-rack-slot-address rack index) :order order))

(defun irc-read-slot (instance index)
  "Read a value from the rack of an instance"
  (let ((dataN* (irc-instance-slot-address instance index)))
    (irc-load-atomic dataN*)))

(defun irc-write-slot (instance index value)
  "Write a value into the rack of an instance"
  (let ((dataN* (irc-instance-slot-address instance index)))
    (irc-store-atomic value dataN*)
    value))

(defun irc-value-frame-parent (renv)
  (let* ((value-frame (irc-untag-general renv %value-frame*%))
         (parent-tsp* (irc-struct-gep %value-frame% value-frame +value-frame.parent-index+))
         (parent-t** (irc-struct-gep %tsp% parent-tsp* 0))
         (parent-t* (irc-load parent-t**)))
    parent-t*))

(defun irc-nth-value-frame-parent (value-frame depth)
  "Return the nth parent of the value-frame"
  (let ((cur value-frame))
    (dotimes (idx depth)
      (setq cur (irc-value-frame-parent cur)))
    cur))

(defun irc-value-frame-reference (value-frame index)
  "Return the nth cell of the value-frame"
  (let* ((value-frame* (irc-untag-general value-frame %value-frame*%))
         (data0tsp* (irc-struct-gep %value-frame% value-frame* +value-frame.data-index+))
         (dataNtsp* (irc-gep data0tsp* (list 0 index)))
         (dataNt** (irc-struct-gep %tsp% dataNtsp* 0)))
    dataNt**))

(defun irc-depth-index-value-frame-reference (value-frame depth index)
  (let* ((depth-value-frame (irc-nth-value-frame-parent value-frame depth)))
    (irc-value-frame-reference depth-value-frame index)))

(defun irc-make-irbuilder-insert-before-instruction (instruction)
  (let ((irbuilder (llvm-sys:make-irbuilder (thread-local-llvm-context))))
    (if instruction
        (progn
          (irc-set-insert-point-instruction instruction irbuilder)
          irbuilder)
        (error "There must be an instruction to insert before"))))

(defun irc-insert-bclasp-lexical-reference (instruction depth index start-renv)
  (let ((irbuilder (irc-make-irbuilder-insert-before-instruction instruction)))
    (with-irbuilder (irbuilder)
      (irc-depth-index-value-frame-reference start-renv depth index))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Array functions
;;;

(defun irc-real-array-displacement (tarray)
  (irc-load-atomic (c++-field-ptr info.%mdarray% tarray :data)
                   :label "real-array-displacement"))

(defun irc-real-array-index-offset (tarray)
  (irc-load-atomic (c++-field-ptr info.%mdarray% tarray :displaced-index-offset)
                   :label "real-array-displaced-index"))

(defun irc-array-total-size (tarray)
  (irc-load-atomic (c++-field-ptr info.%mdarray% tarray :array-total-size)
                   :label "array-total-size"))

(defun irc-array-rank (tarray)
  (irc-load-atomic (c++-field-ptr info.%mdarray% tarray :rank)
                   :label "array-rank"))

(defun irc-array-dimension (tarray axis)
  (let* ((dims (c++-field-ptr info.%mdarray% tarray :dimensions))
         (axisN* (irc-gep dims (list 0 axis))))
    (irc-load-atomic axisN*)))

(defun irc-header-stamp (object)
  (let* ((object* (irc-untag-general object))
         (byte-ptr (irc-bit-cast object* %i8*%))
         (byte-addr
           (irc-gep byte-ptr
                    (list (jit-constant-i64 (+ +header-stamp-offset+ (- +header-size+))))))
         (header-stamp-type (cond
                              ((= 4 +header-stamp-size+) %i32*%)
                              ((= 8 +header-stamp-size+) %i64*%)
                              (t (error "illegal +header-stamp-size+ ~a expected 4 or 8" +header-stamp-size+))))
         (header-addr (irc-bit-cast byte-addr header-stamp-type))
         (value32 (irc-load header-addr))
         (value64 (irc-zext value32 %i64%))
         (valuet* (irc-int-to-ptr value64 %t*%)))
    valuet*))

(defun irc-rack-stamp (object)
  (let* ((instance (irc-untag-general object))
         (instance* (irc-bit-cast instance %instance*%))
         (racks* (irc-struct-gep %instance% instance* +instance.rack-index+))
         (rack (irc-load racks* "rack-tagged"))
         (rack* (irc-untag-general rack %rack*%))
         (stamp* (irc-struct-gep %rack% rack* +rack.stamp-index+))
         (stamp-fixnum* (irc-bit-cast stamp* %t**%)))
    (irc-load stamp-fixnum*)))

(defun irc-wrapped-stamp (object)
  (let* ((wrapped (irc-untag-general object))
         (wrapped* (irc-bit-cast wrapped %wrapped-pointer*%))
         (stamp* (irc-struct-gep %wrapped-pointer% wrapped* +wrapped-pointer.stamp-index+))
         (stamp-fixnum* (irc-bit-cast stamp* %t**%)))
    (irc-load stamp-fixnum*)))

(defun irc-derivable-stamp (object)
  (let* ((derivable (irc-untag-general object))
         (i8* (irc-bit-cast derivable %i8*%))
         (stamp-i64
           (irc-intrinsic "cc_read_derivable_cxx_stamp_untagged_object" i8*)))
    (irc-int-to-ptr stamp-i64 %t*% "derivable-stamp")))

(defun irc-ret-void ()
  (llvm-sys:create-ret-void *irbuilder*))

(defun irc-ret (val)
  (llvm-sys:create-ret *irbuilder* val))

(defun irc-ret-null-t* ()
  (llvm-sys:create-ret *irbuilder* (llvm-sys:constant-pointer-null-get %t*%)))

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
  (when (not (irc-prev-inst-terminator-inst-p))
          (llvm-sys:create-br *irbuilder* block)))

(defun irc-add (lhs rhs &optional (label ""))
  (llvm-sys:create-add *irbuilder* lhs rhs label nil nil))

(defun irc-sub (lhs rhs &optional (label ""))
  (llvm-sys:create-sub *irbuilder* lhs rhs label nil nil))

(defun irc-sdiv (lhs rhs &key (label "") exact)
  (llvm-sys:create-sdiv *irbuilder* lhs rhs label exact))
(defun irc-srem (lhs rhs &key (label ""))
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

(defun irc-ashr (value shift &key (label "") exact)
  "If shift is an integer, generate ashr with a constant uint64.
Otherwise do a variable shift."
  (if (integerp shift)
      (llvm-sys:create-ashr-value-uint64
       cmp:*irbuilder* value shift label exact)
      (llvm-sys:create-ashr-value-value
       cmp:*irbuilder* value shift label exact)))

(defun irc-fence (order &optional (label ""))
  (llvm-sys:create-fence *irbuilder* order 1 #+(or)'llvm-sys:system label))

(defun irc-load (source &optional (label "") is-volatile)
  (llvm-sys:create-load-value-bool-twine *irbuilder* source is-volatile label))

(defun irc-load-atomic (source
                        &key (label "") (align 8) (order 'llvm-sys:monotonic))
  (let ((inst (irc-load source label)))
    ;; atomic loads require an explicit alignment.
    (llvm-sys:set-alignment inst align)
    (llvm-sys:set-atomic inst order 1 #+(or)'llvm-sys:system)
    inst))

(defun irc-store (val destination &optional (is-volatile nil))
  ;; Mismatch in store types is a very common bug we hit when rewriting codegen.
  ;; LLVM doesn't deal with it gracefully except with a debug build, so we just
  ;; check it ourselves. We also check that types are in the same context-
  ;; another reasonably common issue.
  (let ((val-type (llvm-sys:get-type val))
        (dest-contained-type (llvm-sys:get-contained-type (llvm-sys:get-type destination) 0)))
    ;;(bformat t "irc-store val-type: %s    dest-contained-type: %s%N" val-type dest-contained-type)
    (cond ((llvm-sys:type-equal val-type dest-contained-type)
           (llvm-sys:create-store *irbuilder* val destination is-volatile))
          ((llvm-sys:llvmcontext-equal
            (llvm-sys:get-context val-type) (llvm-sys:get-context dest-contained-type))
           (cmp-log-dump-module *the-module*)
           (error "BUG: Mismatch in irc-store between val type ~a and destination contained type ~a"
                  val-type dest-contained-type))
          (t
           (error "BUG: Mismatch in irc-store involving the val type ~a and destination type ~a -
the type LLVMContexts don't match - so they were defined in different threads!"
                  val-type dest-contained-type)))))

(defun irc-store-atomic (val destination
                         &key (is-volatile nil) (align 8)
                              (order 'llvm-sys:monotonic))
  (let ((inst (irc-store val destination is-volatile)))
    (llvm-sys:set-alignment inst align) ; atomic stores require an explicit alignment.
    (llvm-sys:set-atomic inst order 1 #+(or)'llvm-sys:system)
    inst))

(defun reduce-failure-order (order)
  (case order
    ((llvm-sys:sequentially-consistent llvm-sys:acquire llvm-sys:monotonic)
     order)
    ((llvm-sys:acquire-release) 'llvm-sys:acquire)
    ((llvm-sys:release) 'llvm-sys:monotonic)))

(defun irc-%cmpxchg (ptr cmp new order)
  ;; Sanity check I'm putting in when this is new that should maybe be removed, future reader
  (let ((cmp-type (llvm-sys:get-type cmp)))
    (unless (and (llvm-sys:type-equal cmp-type (llvm-sys:get-type new))
                 (llvm-sys:type-equal cmp-type
                                      (llvm-sys:get-contained-type (llvm-sys:get-type ptr) 0)))
      (error "BUG: Type mismatch in IRC-%CMPXCHG")))
  ;; actual gen
  (llvm-sys:create-atomic-cmp-xchg *irbuilder*
                                   ptr cmp new
                                   8 ; llvm::MaybeAlign(0)
                                   order
                                   (reduce-failure-order order)
                                   1 #+(or)'llvm-sys:system))

(defun irc-cmpxchg (ptr cmp new
                    &key (label "") (order 'llvm-sys:sequentially-consistent))
  ;; cmpxchg instructions involve two memory orders: First, the "success" order
  ;; for the RMW operation, and second, the "failure" order for only the read
  ;; in case the comparison fails. I don't honestly know how this works at the
  ;; machine level. For now we only allow passing in one order, which is used
  ;; for both (except we have to reduce it...)
  ;; cmpxchg returns [value, flag] where flag is true iff the swap was done.
  ;; since we're doing a strong exchange (for now), value = cmp iff the swap
 ;;; was done too, so we don't really need the flag.
  ;; Of course we might want to work with the flag directly instead, but that's
  ;; a reorganization at a higher level.
  (irc-extract-value (irc-%cmpxchg ptr cmp new order) (list 0) label))

(defun translate-rmw-op (op)
  (cond ((eq op :xchg) 'llvm-sys:xchg)
        ((eq op :add)  'llvm-sys:add)
        ((eq op :sub)  'llvm-sys:sub)
        ((eq op :and)  'llvm-sys:and)
        ((eq op :nand) 'llvm-sys:nand)
        ((eq op :or)   'llvm-sys:or)
        ((eq op :xor)  'llvm-sys:xor)
        ((eq op :max)  'llvm-sys:max)
        ((eq op :min)  'llvm-sys:min)
        ((eq op :umax) 'llvm-sys:umax)
        ((eq op :umin) 'llvm-sys:umin)
        ((eq op :fadd) 'llvm-sys:fadd)
        ((eq op :fsub) 'llvm-sys:fsub)
        (t (error "Unknown atomic RMW operation: ~s" op))))

(defun irc-atomicrmw (pointer value operation &optional (label "old"))
  (declare (ignore label)) ; LLVM can't name atomic RMWs for whatever reason?
  (llvm-sys:create-atomic-rmw *irbuilder*
                              (translate-rmw-op operation)
                              pointer value
                              'llvm-sys:sequentially-consistent
                              1 #+(or)'llvm-sys:system))

(defun irc-phi (return-type num-reserved-values &optional (label "phi"))
  (llvm-sys:create-phi *irbuilder* return-type num-reserved-values label))

(defun irc-phi-add-incoming (phi-node value basic-block)
  (unless value
    (cmp-log-dump-module *the-module*)
    (error "value is NULL for phi-node ~a basic-block ~a" phi-node basic-block))
  (llvm-sys:add-incoming phi-node value basic-block))


(defun irc-unreachable ()
  (llvm-sys:create-unreachable *irbuilder*))


(defun irc-trunc (value type &optional (label "trunc"))
  (llvm-sys:create-trunc *irbuilder* value type label))


(defun irc-and (x y &optional (label "and"))
  (llvm-sys:create-and-value-value *irbuilder* x y label))
(defun irc-or (x y &optional (label "or"))
  (llvm-sys:create-or-value-value *irbuilder* x y label))
(defun irc-xor (x y &optional (label "xor"))
  (llvm-sys:create-xor-value-value *irbuilder* x y label))

(defun irc-not (x &optional (label "not"))
  ;; NOTE: LLVM does not have a "not" instruction. What it generates is
  ;; a XOR with an all-1s constant, which amounts to the same.
  (llvm-sys:create-not *irbuilder* x label))

(defun irc-va_arg (valist* type &optional (name "vaarg"))
  (llvm-sys:create-vaarg *irbuilder* valist* type name))

(defun irc-vaslist-args-address (vaslist-v &optional (label "vaslist_address"))
  (c++-field-ptr info.%vaslist% vaslist-v :args label))

(defun irc-vaslist-nargs-address (vaslist-v
                                  &optional (label "vaslist_remaining_nargs_address"))
  (c++-field-ptr info.%vaslist% vaslist-v :nargs label))

(defun irc-make-vaslist (nvals vals &optional (label "saved-values"))
  (let* ((undef (llvm-sys:undef-value-get %vaslist%))
         (s1 (llvm-sys:create-insert-value *irbuilder* undef vals '(0) label))
         (s2 (llvm-sys:create-insert-value *irbuilder* s1 nvals '(1)
                                           label)))
    s2))

(defun irc-vaslist-values (vaslist &optional (label "values"))
  (irc-extract-value vaslist '(0) label))

(defun irc-vaslist-nvals (vaslist &optional (label "nvals"))
  (irc-extract-value vaslist '(1) label))

;;; Get the nth value of a vaslist. This entails checking the number
;;; of values and possibly returning a constant nil if needed.
;;; (That is, this works like CL:NTH. The "N" argument is treated as an
;;;  unsigned untagged integer, and an upper out of bounds just results in NIL.)
;;; This requires branching. We could alternately use the llvm select
;;; instruction, but I'm not as sure how it works (especially the "MDFrom"
;;; argument to CreateSelect).
(defun irc-vaslist-nth (n vaslist &optional (label "primary"))
  (let ((novalues (irc-basic-block-create "vaslist-primary-no-values"))
        (values (irc-basic-block-create "vaslist-primary-values"))
        (merge (irc-basic-block-create "vaslist-primary-merge")))
    (irc-cond-br
     (irc-icmp-ult n (irc-vaslist-nvals vaslist))
     values novalues)
    (irc-begin-block novalues)
    (let ((null (irc-literal nil "NIL")))
      (irc-br merge)
      (irc-begin-block values)
      (let ((primary (irc-load (cmp:irc-gep (irc-vaslist-values vaslist)
                                            (list n))
                               "primary")))
        (irc-br merge)
        (irc-begin-block merge)
        (let ((phi (irc-phi %t*% 2 label)))
          (irc-phi-add-incoming phi null novalues)
          (irc-phi-add-incoming phi primary values)
          phi)))))

;;; Given a vaslist, return a new vaslist with all values but the primary.
;;; If the vaslist is already empty, it is returned.
;;; N is treated as an untagged unsigned integer, and if it's above the upper
;;; bound an empty vaslist, NIL is returned.
(defun irc-vaslist-nthcdr (n vaslist &optional (label "rest"))
  (let* ((nvals (irc-vaslist-nvals vaslist))
         (real-n (irc-intrinsic "llvm.umin.i64" n nvals))
         (new-nvals (irc-sub nvals real-n))
         (vals (irc-vaslist-values vaslist))
         (new-vals (irc-gep vals (list real-n))))
    (irc-make-vaslist new-nvals new-vals label)))

;;; Ditto the above, but it's LAST instead of NTHCDR.
(defun irc-vaslist-last (n vaslist &optional (label "rest"))
  (let* ((nvals (irc-vaslist-nvals vaslist))
         (new-nvals (irc-intrinsic "llvm.umin.i64" n nvals))
         (skip (irc-sub nvals new-nvals))
         (vals (irc-vaslist-values vaslist))
         (new-vals (irc-gep vals (list skip))))
    (irc-make-vaslist new-nvals new-vals label)))

;;; ...and finally, BUTLAST, which is actually the simplest.
(defun irc-vaslist-butlast (n vaslist &optional (label "rest"))
  (let* ((nvals (irc-vaslist-nvals vaslist))
         (real-n (irc-intrinsic "llvm.umin.i64" n nvals))
         (new-nvals (irc-sub nvals real-n))
         (new-vals (irc-vaslist-values vaslist)))
    (irc-make-vaslist new-nvals new-vals label)))

(defparameter *default-function-attributes*
  #+cclasp '(llvm-sys:attribute-uwtable
             ("frame-pointer" "all"))
  #-cclasp '(llvm-sys:attribute-uwtable
             ("frame-pointer" "all"))
  )

(defun setup-wrong-number-of-arguments-xep (arity xep-group calling-convention)
  ;; Remove the xep corresponding to the arity from the xep-group and replace it with a place-holder that the literal compiler will recognize
  ;; Do nothing for now.
  (declare (ignore arity xep-group calling-convention))
  )


(defun layout-xep-function* (arity function-info xep-group calling-convention func-env)
  (let* ((local-function (xep-group-local-function xep-group))
         (cleavir-lambda-list-analysis (function-info-cleavir-lambda-list-analysis function-info))
         (variables (cleavir-lambda-list-analysis-lambda-list-arguments cleavir-lambda-list-analysis)))
    (cmp-log "lambda list variables = %s%N" variables)
    (let ((rev-output-bindings '()))
      (with-irbuilder (*irbuilder-function-alloca*)
        (dolist (var variables)
          (push (cons var (alloca-t* (string var))) rev-output-bindings))
        (cmp-log "output-bindings = %s%N" rev-output-bindings))
      (let ((output-bindings (nreverse rev-output-bindings)))
        ;; Parse lambda list.
        (with-irbuilder (*irbuilder-function-body*)
          (let ((good-args (compile-lambda-list-code
                            (function-info-cleavir-lambda-list-analysis function-info)
                            calling-convention
                            arity
                            :argument-out (lambda (value datum)
                                            (cmp-log "argument-out: %s %s%N" value datum)
                                            (let ((alloca (cdr (assoc datum output-bindings))))
                                              (irc-t*-result value alloca))))))
            (cond
              ((not good-args)
               ;; If the arity of the xep doesn't match the lambda-list then
               ;; we want to call a standard wrong-number-of-arguments entry point
               ;; Delete the current entry-point function from the xep-group
               ;;  and replace it with a place-holder that the literal compiler will recognize
               ;;  and redirect to a standard entry-point for the wrong number of arguments.
               (setup-wrong-number-of-arguments-xep arity xep-group calling-convention))
              (t
               (let ((call-block (irc-basic-block-create "docall")))
                 (irc-br call-block)
                 (irc-begin-block call-block))
               (let* ((cleavir-lambda-list-analysis (function-info-cleavir-lambda-list-analysis function-info))
                      (cleavir-arguments (cleavir-lambda-list-analysis-lambda-list-arguments cleavir-lambda-list-analysis))
                      (new-env (irc-new-unbound-value-environment-of-size
                                func-env
                                :number-of-arguments (length output-bindings)
                                :label "arguments-env")))
                 (declare (ignore cleavir-arguments new-env))
                 ;; Now generate llvm registers for each argument
                 (let ((rev-call-args '()))
                   (push (calling-convention-closure calling-convention) rev-call-args)
                   (dolist (binding output-bindings)
                     (let ((alloca-arg (cdr binding)))
                       (push (irc-load alloca-arg) rev-call-args)))
                   (let ((call-args (nreverse rev-call-args)))
                     (cmp:irc-ret
                      (let ((function-type (llvm-sys:get-function-type local-function)))
                        (cmp-log "(length variables) %s%N" (length variables))
                        (cmp-log "variables: %s%N" variables)
                        (cmp-log "function-type %s%N" function-type)
                        (cmp-log "local-function %s%N" local-function)
                        (cmp-log "length call-args: %s%N" (length call-args))
                        (cmp-log "call-args: %s%N" call-args)
                        (cmp:irc-create-call-wft
                         function-type
                         local-function
                         call-args)))))
                 (cmp-log-dump-module *the-module*)
                 (cmp-log "layout-xep-group* xep-group: %s%N" xep-group))))))))))

(defun layout-xep-function (xep-arity function-info xep-group function-name parent-env)
  (let* ((arity (xep-arity-arity xep-arity))
         (xep-fn (xep-arity-function-or-placeholder xep-arity)))
    (cmp-log "xep-fn = %s%N" xep-fn)
    (if (literal:general-entry-placeholder-p xep-fn)
        (progn
          ;;(format t "layout-xep-function skipping arity ~a for ~a~%" arity xep-fn)
          )
        (let ((*current-function* xep-fn)
              (func-env (make-function-container-environment
                         parent-env (car (llvm-sys:get-argument-list xep-fn)) xep-fn))
              (irbuilder-cur (llvm-sys:make-irbuilder (thread-local-llvm-context)))
              (irbuilder-alloca (llvm-sys:make-irbuilder (thread-local-llvm-context)))
              (irbuilder-body (llvm-sys:make-irbuilder (thread-local-llvm-context)))
              (entry-bb (irc-basic-block-create "entry" xep-fn)))
          (cmp-log "entry-bb = %s%N" entry-bb)
          (irc-set-insert-point-basic-block entry-bb irbuilder-cur)
          ;; Setup exception handling and cleanup landing pad
          (irc-set-function-for-environment func-env xep-fn)
          (with-irbuilder (irbuilder-cur)
            (let* ((body-bb (irc-basic-block-create "body" xep-fn))
                   (entry-branch (irc-br body-bb)))
              (irc-set-insert-point-instruction entry-branch irbuilder-alloca)
              (irc-set-insert-point-basic-block body-bb irbuilder-body)))
          (let* ((*current-function* xep-fn)
                 (*current-function-name* function-name)
                 (*irbuilder-function-alloca* irbuilder-alloca)
                 (*irbuilder-function-body* irbuilder-body)
                 (*gv-current-function-name* (module-make-global-string
                                              *current-function-name* "fn-name")))
            (with-guaranteed-*current-source-pos-info* ()
              (with-dbg-function (:lineno (core:source-pos-info-lineno core:*current-source-pos-info*)
                                  :function xep-fn
                                  :function-type (llvm-sys:get-function-type xep-fn))
                (with-dbg-lexical-block (:lineno (core:source-pos-info-lineno
                                                  core:*current-source-pos-info*))
                  (when core:*current-source-pos-info*
                    (dbg-set-irbuilder-source-location irbuilder-alloca
                                                       core:*current-source-pos-info*)
                    (dbg-set-irbuilder-source-location irbuilder-body
                                                       core:*current-source-pos-info*))
                  (with-irbuilder (irbuilder-body)
                    (let ((*current-unwind-landing-pad-dest* nil))
                      (cmp-log "xep-fn2 = %s%N" xep-fn)
                      (let ((arguments      (llvm-sys:get-argument-list xep-fn)))
                        (declare (ignorable arguments))
                        (cmp-log "arguments -> %s%N" arguments)
                        (let* ((rest-alloc     (alloca-t* "rest"))
                               (callconv       (setup-calling-convention
                                                xep-fn
                                                arity
                                                :debug-on core::*debug-bclasp*
                                                :cleavir-lambda-list-analysis (function-info-cleavir-lambda-list-analysis function-info)
                                                :rest-alloc rest-alloc)))
                          (cmp-log "Setup the lambda-list handler and callconf %s%N" callconv)
                          (layout-xep-function* arity function-info xep-group callconv func-env)))))))))))))


(defun layout-xep-group (function-info xep-group function-name parent-env)
  (dolist (xep-arity (xep-group-arities xep-group))
    (layout-xep-function xep-arity function-info xep-group function-name parent-env)))


(defun do-new-function (body-fn function-symbol parent-env function-attributes linkage return-void function-info)
  (declare (ignore function-attributes))
  (let ((cleavir-lambda-list-analysis (function-info-cleavir-lambda-list-analysis function-info)))
    (cmp-log "Entered do-new-function %s%N" (function-info-cleavir-lambda-list-analysis function-info))
    (cmp-log "     (lambda-list-arguments cleavir-lambda-list) -> %s%N" (cleavir-lambda-list-analysis-lambda-list-arguments (function-info-cleavir-lambda-list-analysis function-info)))
    (let* ((argument-names (list* "closure" (mapcar (lambda (sym) (string sym)) (cleavir-lambda-list-analysis-lambda-list-arguments cleavir-lambda-list-analysis))))
           (num-args (length argument-names))
           (local-function-type (llvm-sys:function-type-get
                                 (lookup-type :tmv)
                                 (make-list num-args :initial-element (lookup-type :t*))))
           (function-name (jit-function-name (jit-function-name function-symbol)))
           (function-description (irc-make-function-description function-info function-name))
           )
      (cmp-log "argument-names: %s%N" argument-names)
      (cmp-log "local-function-type: %s%N" local-function-type)
      (multiple-value-bind (local-fn local-entry-point)
          (irc-local-function-create local-function-type linkage function-name *the-module* function-description)
        (llvm-sys:set-personality-fn local-fn (irc-personality-function))
        (let ((*current-function* local-fn)
              (func-env (make-function-container-environment parent-env (car (llvm-sys:get-argument-list local-fn)) local-fn))
              (irbuilder-cur (llvm-sys:make-irbuilder (thread-local-llvm-context)))
              (irbuilder-alloca (llvm-sys:make-irbuilder (thread-local-llvm-context)))
              (irbuilder-body (llvm-sys:make-irbuilder (thread-local-llvm-context))))
          (let ((entry-bb (irc-basic-block-create "entry" local-fn)))
            (irc-set-insert-point-basic-block entry-bb irbuilder-cur))
          ;; Setup exception handling and cleanup landing pad
          (irc-set-function-for-environment func-env local-fn)
          (with-irbuilder (irbuilder-cur)
            (let* ((body-bb (irc-basic-block-create "body" local-fn))
                   (entry-branch (irc-br body-bb)))
              (irc-set-insert-point-instruction entry-branch irbuilder-alloca)
              (irc-set-insert-point-basic-block body-bb irbuilder-body)))
          (setf-metadata func-env :cleanup ())
          (with-guaranteed-*current-source-pos-info* ()
            (let* ((result (let ((*irbuilder-function-alloca* irbuilder-alloca)) (alloca-tmv "result")))
                   (*current-function* local-fn)
                   (*current-function-name* (llvm-sys:get-name local-fn))
                   (*irbuilder-function-alloca* irbuilder-alloca)
                   (*irbuilder-function-body* irbuilder-body)
                   (*gv-current-function-name* (module-make-global-string *current-function-name* "fn-name")))
              (with-irbuilder (*irbuilder-function-body*)
                (with-new-function-prepare-for-try (local-fn)
                  (with-dbg-function (:lineno (core:source-pos-info-lineno core:*current-source-pos-info*)
                                      :function local-fn
                                      :function-type local-function-type)
                    (with-dbg-lexical-block (:lineno (core:source-pos-info-lineno core:*current-source-pos-info*))
                      (when core:*current-source-pos-info*
                        (dbg-set-irbuilder-source-location irbuilder-alloca
                                                           core:*current-source-pos-info*)
                        (dbg-set-irbuilder-source-location irbuilder-body
                                                           core:*current-source-pos-info*))
                      (with-irbuilder (*irbuilder-function-body*)
                        (or *the-module* (error "with-new-function *the-module* is NIL"))
                        (cmp-log "with-landing-pad around body%N")
                        (let* ((function-arguments (rest (llvm-sys:get-argument-list local-fn)))
                               (cleavir-arguments (cleavir-lambda-list-analysis-lambda-list-arguments cleavir-lambda-list-analysis)))
                          (cmp-log "function-arguments: %s%N" function-arguments)
                          (cmp-log "cleavir-arguments: %s%N" cleavir-arguments)
                          (let ((output-bindings (mapcar (lambda (name var) (cons name (cons 'ext:llvm-register-var var))) cleavir-arguments function-arguments))
                                (new-env (irc-new-unbound-value-environment-of-size
                                          func-env
                                          :number-of-arguments (length (cleavir-lambda-list-analysis-lambda-list-arguments cleavir-lambda-list-analysis))
                                          :label "arguments-env")))
                            (irc-make-value-frame-set-parent new-env (length output-bindings) func-env)
                            (mapc (lambda (ob)
                                    (cmp-log "Adding to environment: %s%N" ob)
                                    (core:value-environment-define-lexical-binding new-env (car ob) (cdr ob)))
                                  output-bindings)
                            (funcall body-fn local-fn new-env result)))
                        (cmp-log "ReturnAA%N")
                        #+(or)(irc-verify-no-function-environment-cleanup func-env))
                      (cmp-log "ReturnA%N")
                      (if return-void
                          (irc-ret-void)
                          (irc-ret (irc-load result)))))))))
          (cmp-log "ReturnB%N")
          (let ((xep-group (irc-xep-functions-create cleavir-lambda-list-analysis
                                                     linkage
                                                     (string function-name)
                                                     *the-module*
                                                     function-description
                                                     local-fn
                                                     local-entry-point)))
            (cmp-log "xep-group = %s%N" xep-group)
            (layout-xep-group function-info xep-group function-name parent-env)
            xep-group))))))
  
(defmacro with-new-function
    ((;; LOCAL-FN is bound to the function being created
      local-fn
      ;; FN-ENV is bound to the function environment
      fn-env
      ;; RESULT is bound to the %tmv% result
      fn-result
      &key
        ;; The function name can be a symbol or a string.
        ;; if its a string it is used unchanged
        ;; if its a symbol then it is mangled by appending "FN-SYMB." to it
        ;; if its a cons of the form (setf ...) then its mangled by appending FN-SETF. to it
      (function-name "function")
      ;; This is the parent environment of the new function environment that
      ;; will be returned
      parent-env
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
  (declare (ignore function-attributes-p))
  `(do-new-function (lambda (,local-fn ,fn-env ,fn-result)
                      (declare (ignorable ,local-fn))
                      (progn
                        ,@body))
     ,function-name
     ,parent-env
     ',function-attributes
     ,linkage
     ,return-void
     ,function-info))

(defun function-description-name (function)
  (let ((function-name (llvm-sys:get-name function)))
    (core:bformat nil "%s^DESC" function-name)))

(defun irc-function-create (function-type linkage function-name module
                            &key
                              (function-attributes *default-function-attributes* function-attributes-p ))
  (let* ((fn (llvm-sys:function-create function-type
                                       linkage
                                       function-name
                                       module)))
    (dolist (temp function-attributes)
      (cond
        ((symbolp temp) (llvm-sys:add-fn-attr fn temp))
        ((stringp temp) (llvm-sys:add-fn-attr2string fn temp ""))
        ((and (consp temp) (stringp (car temp)) (stringp (cadr temp)))
         (llvm-sys:add-fn-attr2string fn (car temp) (cadr temp)))
        (t (error "Illegal function attribute ~a" temp))))
    fn))


(defun irc-simple-function-create (function-name function-type linkage module
                                   &key (function-attributes *default-function-attributes* function-attributes-p )
                                     argument-names ;;; '("result-ptr" "activation-frame-ptr") argument-names-p))
                                     )
  "A simple function creator - set personality and arguments and function-attributes.
But no irbuilders or basic-blocks. Return the fn."
  (let ((fn (irc-function-create function-type
                                 linkage
                                 function-name
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

(defconstant +maxi32+ 4294967295)

(defstruct (entry-point-reference (:type vector) :named)
  "Store an index into the literal vector for an entry-point. 
index - the index into the literal vector
kind - :global or :local - for debugging.
function-description - for debugging."
  index
  kind ; for debugging (:global or :local)
  function-description ; for debugging 
  )

(defstruct (function-description-placeholder (:type vector) :named)
  function function-name source-pathname lambda-list docstring declares lineno column filepos
  )

(defun irc-make-function-description (function-info &optional llvm-function-name)
  (unless function-info
    (error "function info is NIL for ~a" (or llvm-function-name "undefined")))
  (let ((function-name (function-info-function-name function-info))
        (source-pathname (function-info-source-pathname function-info))
        source-info-name
        (lambda-list (function-info-lambda-list function-info))
        (docstring (function-info-docstring function-info))
        (lineno (function-info-lineno function-info))
        (column (function-info-column function-info))
        (filepos (function-info-filepos function-info))
        (declares (function-info-declares function-info)))
    (declare (ignorable source-info-name))
    (multiple-value-bind (found-source-info n l c f)
        (parse-declares-for-source-info declares)
      (when found-source-info
        (when n (setf source-info-name n))
        (when l (setf lineno l))
        (when c (setf column c))
        (when f (setf filepos f)))
      (sys:make-function-description :function-name function-name
                                     :source-pathname source-pathname
                                     :lambda-list lambda-list
                                     :docstring docstring
                                     :declares declares
                                     :lineno lineno
                                     :column column
                                     :filepos filepos))))

(defun irc-create-global-entry-point-reference (xep-arity-list module function-description local-entry-point-reference)
  (declare (ignore module))
  (let* ((entry-point-generator (let ((entry-point-indices (literal:register-xep-function-indices xep-arity-list)))
                                  (sys:make-global-entry-point-generator
                                   :entry-point-functions entry-point-indices
                                   :function-description function-description
                                   :local-entry-point-index (entry-point-reference-index local-entry-point-reference))))
         (index (literal:reference-literal entry-point-generator)))
    (make-entry-point-reference :index index :kind :global :function-description function-description)))

(defun irc-create-local-entry-point-reference (local-fn module function-description)
  (declare (ignore module))
  (let* ((entry-point-generator (let ((entry-point-index (literal:register-local-function-index local-fn)))
                                 (sys:make-local-entry-point-generator
                                  :entry-point-functions (list entry-point-index)
                                  :function-description function-description)))
         (index (literal:reference-literal entry-point-generator)))
    (make-entry-point-reference :index index :kind :local :function-description function-description)))


(defun irc-local-function-create (llvm-function-type linkage function-name module function-description)
  "Create a local function and no function description is needed"
  (let* ((local-function-name (concatenate 'string function-name "-lcl"))
         (fn (irc-function-create llvm-function-type linkage local-function-name module))
         (local-entry-point-reference (irc-create-local-entry-point-reference fn module function-description)))         
    (values fn local-entry-point-reference)))

(defparameter *multiple-entry-points* nil)
(defun irc-xep-functions-create (cleavir-lambda-list-analysis linkage function-name module function-description local-function local-entry-point-reference)
  "Create a function and a function description for a cclasp function"
  ;; MULTIPLE-ENTRY-POINT first return value is list of entry points
  (let ((rev-xep-aritys '()))
    (dolist (arity (list* :general-entry (subseq (list 0 1 2 3 4 5 6 7 8) +entry-point-arity-begin+ +entry-point-arity-end+)))
      (cmp-log "Creating xep function for %s%N" arity)
      (let* ((xep-function-name (concatenate 'string function-name (format nil "-xep~a" (if (eq arity :general-entry) "" arity))))
             (fn (if (generate-function-for-arity-p arity cleavir-lambda-list-analysis)
                     (let* ((function-type (fn-prototype arity))
                            (function (irc-function-create function-type linkage xep-function-name module)))
                       #+(or)(when (eq arity :general-entry)
                         (format t "About to add-param-attr for function: ~a~%" function)
                         (llvm-sys:add-param-attr function 2 'llvm-sys:attribute-in-alloca))
                       function)
                     (literal:make-general-entry-placeholder :arity arity
                                                             :name xep-function-name
                                                             :cleavir-lambda-list-analysis cleavir-lambda-list-analysis)
                     ))
             (xep-arity (make-xep-arity :arity arity :function-or-placeholder fn)))
        (push xep-arity rev-xep-aritys)))
    (cmp-log "Created xep-arities%N")
    (let* ((xep-aritys (nreverse rev-xep-aritys))
           (entry-point-reference (irc-create-global-entry-point-reference xep-aritys
                                                                           module
                                                                           function-description
                                                                           local-entry-point-reference))
           (entry-point-info (make-xep-group :name function-name
                                             :cleavir-lambda-list-analysis cleavir-lambda-list-analysis
                                             :arities xep-aritys
                                             :entry-point-reference entry-point-reference
                                             :local-function local-function)))
      (cmp-log "Created entry-point-info %s%N" entry-point-info)
      entry-point-info)))

(defun irc-verify-no-function-environment-cleanup (env)
  (let ((unwind (local-metadata env :unwind)))
    (when unwind (error "There are clauses in the function environment that need to be evaluated for cleanup: ~a" unwind))))

(defun irc-cleanup-function-environment (env)
  "This may be deprecated by just calling irc-do-unwind-environment!!!!!!
  Generate the code to cleanup the environment"
  (if env (irc-do-unwind-environment env)))

(defun irc-pointer-cast (from totype &optional (label ""))
  (llvm-sys:create-pointer-cast *irbuilder* from totype label))

(defun irc-bit-cast (from totype &optional (label "bit-cast"))
  (llvm-sys:create-bit-cast *irbuilder* from totype label))

(defun irc-irbuilder-status (&optional (irbuilder *irbuilder*) (label "current *irbuilder*"))
    (bformat t "%s -> %s%N" label irbuilder))

#+(or)
(defun irc-constant-string-ptr (global-string-var)
  (let* ((type (llvm-sys:get-pointer-element-type
                                      (llvm-sys:get-scalar-type
                                       (llvm-sys:get-type
                                        global-string-var))))
         (ptr (llvm-sys:create-geparray *irbuilder* global-string-var (list (cmp:jit-constant-i32 0) (cmp:jit-constant-i32 0)) "ptr")))
    ptr))

(defun irc-dtor (name obj)
  (declare (special *compiler-suppress-dtors*))
  (unless *compiler-suppress-dtors* (irc-intrinsic name obj)))

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

(defun alloca (type size &optional (label "") (alignment 8))
  (let ((alloca
          (llvm-sys:create-alloca *irbuilder-function-alloca*
                                  type (jit-constant-i32 size) label)))
    (llvm-sys:set-alignment alloca alignment) ; 8-byte alignment
    alloca))

(defun alloca-return (&optional (label "")) (alloca %return-type% 1 label))
(defun alloca-t* (&optional (label "")) (alloca %t*% 1 label))
(defun alloca-tmv (&optional (label "")) (alloca %tmv% 1 label))
(defun alloca-af* (&key (label "")) (alloca %af*% 1 label))
(defun alloca-i8 (size &key (alignment 8) (label "var"))
  (let ((al (alloca %i8% size label)))
    (llvm-sys:set-alignment al alignment)
    al))

(defun alloca-i8* (&optional (label "i8*-")) (alloca %i8*% 1 label))



(defun alloca-i32 (&optional (label "i32-")) (alloca %i32% 1 label))
(defun alloca-size_t (&optional (label "var")) (alloca %size_t% 1 label))
(defun alloca-vaslist (&key (label "vaslist")) (alloca %vaslist% 2 label +vaslist-alignment+))

(defun alloca-dx-list (&key length (label "dx-list"))
  ;; Unlike most allocas, we want dx object allocas to be done inline with the code,
  ;; as the length will have been computed at runtime. Kinda like a C VLA.
  ;; So we use *irbuilder*, and don't send the length through constantization.
  (let ((instr (llvm-sys:create-alloca *irbuilder* %cons% length label)))
    (llvm-sys:set-alignment instr +alignment+)
    instr))

(defun alloca-temp-values (size &optional (label "temp-values"))
  ;; Also VLA
  (llvm-sys:create-alloca *irbuilder* %t*% size label))

(defun alloca-arguments (size &optional (label "callargs"))
  (llvm-sys:create-alloca *irbuilder-function-alloca* (llvm-sys:array-type-get %t*% size) (jit-constant-i64 1) label))

(defun alloca-register-save-area (arity &key (irbuilder *irbuilder-function-alloca*) (label "vaslist"))
  "Alloca space for a register save area, and keep it in the stack map."
  (with-irbuilder (irbuilder)
    (multiple-value-bind (words index)
        (irc-arity-info arity)
      (let ((rsa (alloca (llvm-sys:array-type-get %t*% words) 1 label)))
        (unless (member :sanitizer=thread *features*)
          ;; Don't generate calls to llvm.experimental.stackmap when :sanitizer=thread feature is on
          (irc-intrinsic "llvm.experimental.stackmap" (jit-constant-i64 (logior #xDEAD0000 index)) (jit-constant-i32 0)
                         rsa))
        rsa))))

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

(defun irc-make-tmv (nret val0)
  (let* ((undef (llvm-sys:undef-value-get %tmv%))
         (ret-tmv0 (llvm-sys:create-insert-value *irbuilder* undef val0 '(0) "ret0"))
         (ret-tmvn (llvm-sys:create-insert-value *irbuilder* ret-tmv0 nret '(1) "nret")))
    ret-tmvn))

(defun irc-tmv-primary (tmv &optional (label "primary-return-value"))
  (irc-extract-value tmv '(0) label))

(defun irc-tmv-nret (tmv &optional (label "nret"))
  (irc-extract-value tmv '(1) label))

(defun irc-t*-result (t* result)
  (let ((return-type (llvm-sys:get-type result)))
    (cond ((llvm-sys:type-equal return-type %t**%)
           (irc-store t* result))
          ((llvm-sys:type-equal return-type %tmv*%)
           (irc-store (irc-make-tmv (jit-constant-size_t 1) t*) result))
          (t (error "Unknown return-type in irc-t*-result")))))

(defun irc-tmv-result (tmv result)
  (let ((return-type (llvm-sys:get-type result)))
    (cond ((llvm-sys:type-equal return-type %t**%)
           (let ((primary (irc-tmv-primary tmv)))
             (irc-store primary result)))
          ((llvm-sys:type-equal return-type %tmv*%)
           (irc-store tmv result))
          (t (error "Unknown return-type in irc-tmv-result")))))

(defun irc-tsp-result (tsp result)
  (irc-t*-result (irc-smart-ptr-extract tsp) result))

(defun irc-arity-index (arity)
  (cond
    ((eq arity :general-entry)
     0)
    ((fixnump arity)
     (+ 1 arity))
    (t (error "irc-arity-index Illegal arity ~a" arity))))

(defun irc-calculate-entry (closure arity function-type &optional (label "ep-gep"))
  (declare (ignore label))
  (let* ((global-entry-point** (c++-field-ptr info.%function% closure :global-entry-point "global-entry-point**"))
         (global-entry-point* (irc-load global-entry-point** "global-entry-point*"))
         (ep-i8** (c++-field-ptr info.%global-entry-point% global-entry-point* :entry-points "ep-i8**"))
         (arity-index (irc-arity-index arity))
         (ep-bc (irc-bit-cast ep-i8** %entry-point-vector*% "ep-bc"))
         (ep-arity-i8** (irc-gep ep-bc (list 0 arity-index) (format nil "xep-~a-i8**" arity)))
         (ep-arity-i8*  (irc-load ep-arity-i8** (format nil "xep-~a-i8*" arity))))
    (cmp-log "Got ep-arity-i8* -> %s%N" ep-arity-i8*)
    (prog1 (irc-bit-cast ep-arity-i8* function-type "ep")
      (cmp-log-dump-module *the-module*))))


;;; Our present convention is that Lisp functions uniformly have
;;; (closure nargs arg0 .. argm ...) as parameters, where m is
;;; core:+number-of-fixed-arguments+; the final ... will be a vaslist,
;;; which is more expensive.
;;; If we want to pass fewer than four arguments, we still need to pass
;;; enough to match the type; since the function will check nargs and
;;; ignore anything past, undef is fine. (NULL results in pointless
;;; register zeroing.)

                  
(defun irc-calculate-real-args (arity closure arguments)
  (if (eq arity :general-entry)
      (let ((arg-buffer (alloca-arguments (length arguments) "call-args"))
            (idx 0))
        (dolist (arg arguments)
          (let ((arg-gep (irc-gep arg-buffer (list 0 idx))))
            (incf idx)
            (irc-store arg arg-gep)))
        (list closure (jit-constant-size_t (length arguments)) (irc-bit-cast arg-buffer %t**%)))
      (list* closure arguments)))

(defstruct (call-info (:type vector) :named)
  entry-point
  real-args
  function-type
  function*-type)

(defun irc-calculate-arity (arguments)
  (cond
    ((and (<= +entry-point-arity-begin+ (length arguments))
          (< (length arguments) +entry-point-arity-end+))
     (- (length arguments) +entry-point-arity-begin+))
    (t :general-entry)))

(defun irc-calculate-call-info (closure arguments)
  (let* ((arity (irc-calculate-arity arguments))
         (function-type (fn-prototype arity))
         (function*-type (llvm-sys:type-get-pointer-to function-type))
         (entry-point (irc-calculate-entry closure arity function*-type))
         (real-args (irc-calculate-real-args arity closure arguments)))
    (make-call-info :entry-point entry-point
                    :function-type function-type
                    :function*-type function*-type
                    :real-args real-args)))

(defun irc-funcall-results-in-registers (closure args &optional (label ""))
  (declare (ignore label))
  ;; (bformat t "irc-funcall-results-in-register-wft closure: %s%N" closure)
  (let ((call-info (irc-calculate-call-info closure args)))
    (let ((result-in-registers (irc-call-or-invoke (call-info-function-type call-info)
                                                   (call-info-entry-point call-info)
                                                   (call-info-real-args call-info)
                                                   *current-unwind-landing-pad-dest*)))
      result-in-registers)))

(defun irc-funcall (result closure args &optional label)
  (unless label
    (setf label "unlabeled-function"))
  (let ((result-in-registers (irc-funcall-results-in-registers closure args label)))
    (irc-tmv-result result-in-registers result)))

;;; Given LLVM values for a closure, an argcount, and an array of arguments,
;;; generate a call or invoke. This means calling the general entry point.
(defun irc-apply (closure argcount args &optional (label ""))
  (let* ((function-type (fn-prototype :general-entry))
         (function*-type (llvm-sys:type-get-pointer-to function-type))
         (entry-point
           (irc-calculate-entry closure :general-entry function*-type))
         (real-args (list closure argcount args)))
    (irc-call-or-invoke function-type entry-point real-args
                        *current-unwind-landing-pad-dest*
                        label)))

;----------------------------------------------------------------------

(defun get-primitives ()
  *primitives*)

(defun throw-if-mismatched-arguments (fn-name args)
  (let* ((info (gethash fn-name (get-primitives)))
         (_ (unless info
              (error "Unknown primitive ~a" fn-name)))
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

(defun irc-create-invoke-wft (function-type entry-point args unwind-dest &optional (label ""))
  ;;(bformat t "irc-create-invoke-wft entry-point: %s%N" entry-point)
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
    (let* ((code (llvm-sys:create-invoke *irbuilder* function-type entry-point normal-dest unwind-dest args label)))
      (irc-begin-block normal-dest)
      (unless code (error "irc-create-invoke returning nil"))
      code)))

(defparameter *debug-create-call* nil)

(defun irc-create-call-wft (function-type entry-point args &optional (label ""))
  ;;(throw-if-mismatched-arguments function-name args)
  (if *debug-create-call* (bformat t "irc-create-call-wft function-type: %s entry-point: %s args: %s%N" function-type entry-point args ))
  (llvm-sys:create-call-function-pointer *irbuilder* function-type entry-point args label nil))

(defun irc-create-invoke-default-unwind (function-name args &optional (label ""))
  (or *current-unwind-landing-pad-dest* (error "irc-create-invoke-default-unwind was called when *current-unwind-landing-pad-dest* was NIL - check the outer with-landing-pad macro"))
  (irc-create-invoke-wft (llvm-sys:get-function-type function-name) function-name args *current-unwind-landing-pad-dest* label))

(defun irc-call-or-invoke (function-type function args &optional (landing-pad *current-unwind-landing-pad-dest*) (label ""))
  (if landing-pad
      (progn
        (irc-create-invoke-wft function-type function args landing-pad label))
      (progn
        (irc-create-call-wft function-type function args label))))

(defun irc-intrinsic-call-or-invoke (function-name args &optional (label "") (landing-pad *current-unwind-landing-pad-dest*))
  "landing-pad is either a landing pad or NIL (depends on function)"
  #+debug-compiler(throw-if-mismatched-arguments function-name args)
  (multiple-value-bind (the-function primitive-info)
      (get-or-declare-function-or-error *the-module* function-name)
    (let* ((function-throws (not (llvm-sys:does-not-throw the-function)))
           (function-type (llvm-sys:get-function-type the-function))
           (code            (cond
                              ((and landing-pad function-throws)
                               (irc-create-invoke-wft function-type the-function args landing-pad label))
                              (t
                               (irc-create-call-wft function-type the-function args label)))))
      (when (llvm-sys:does-not-return the-function)
        (irc-unreachable)
        (irc-begin-block (irc-basic-block-create "from-invoke-that-never-returns")))
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
  
(defun irc-intrinsic (function-name &rest args)
  (irc-intrinsic-call-or-invoke function-name args))

;; Helper functions



(defun irc-verify-module (module return-action)
  (llvm-sys:verify-module module return-action))

(defun irc-verify-module-safe (module)
  (when *verify-llvm-modules*
    (multiple-value-bind (found-errors error-message)
        (irc-verify-module module 'llvm-sys::return-status-action)
      (when found-errors
        (llvm-sys:dump-module module)
        (format t "Module error: ~a~%" error-message)
        (error "Verify module found errors")))))

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
        (returns-twice (getf (primitive-properties primitive-info) :returns-twice))
        function-attributes)
    (when does-not-throw (push 'llvm-sys:attribute-no-unwind function-attributes))
    (when does-not-return (push 'llvm-sys:attribute-no-return function-attributes))
    (when returns-twice (push 'llvm-sys:attribute-returns-twice function-attributes))
    (push '("frame-pointer" "all") function-attributes)
    (let ((function (irc-function-create (llvm-sys:function-type-get return-ty argument-types varargs)
                                             'llvm-sys::External-linkage
                                             dispatch-name
                                             module
                                             :function-attributes function-attributes)))
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
  (declare (ignore env))
  "Return an llvm GlobalValue for a symbol"
  (multiple-value-bind (ref literal-name)
      (literal:compile-reference-to-literal sym)
    (let ((label (if literal-name
                     literal-name
                     "")))
      (values (irc-load ref label) label))))

(defun irc-global-setf-symbol (sym env)
  (declare (ignore env))
  "Return an llvm GlobalValue for a function name of the form (setf XXXX).
   Pass XXXX as the sym to this function."
  (irc-load (literal:compile-reference-to-literal sym)))

(defun irc-environment-activation-frame (env)
  (if env
      (environment-activation-frame env)
      nil))
