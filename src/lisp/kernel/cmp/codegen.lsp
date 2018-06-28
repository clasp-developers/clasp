(in-package #:cmp)

;;;; bclasp compilation.
;;; bclasp-compile* and compile-thunk are entries from compile and compile-file resp.
;;; codegen is the main event.






(defun generate-llvm-function-from-code (
                                         ;; Symbol xxx or (setf xxx) name of the function that is
                                         ;; assigned to this code by
                                         ;; lambda-block or COMPILE
                                         given-name
                                         ;; generated from lambda-list
                                         lambda-list
                                         ;; lambda declares as a list of conses
                                         declares
                                         ;; lambda docstring
                                         docstring
                                         ;; lambda code
                                         code
                                         ;; environment of the lambda
                                         env-around-lambda
                                         ;; key argument: T if code should be wrapped in a block with block-name
                                         &key wrap-block ; wrap code in a block
                                           ;; Name of the block to wrap in
                                         block-name
                                         (linkage 'llvm-sys:internal-linkage))
  "This is where llvm::Function are generated from code, declares, 
lambda-list, environment.
All code generation comes through here.   Return (llvm:function lambda-name)
Could return more functions that provide lambda-list for swank for example"
  (setq *lambda-args-num* (1+ *lambda-args-num*))
  (multiple-value-bind (cleavir-lambda-list new-body)
      (transform-lambda-parts lambda-list declares code)
    (cmp-log "generate-llvm-function-from-code%N")
    (cmp-log "cleavir-lambda-list -> %s%N" cleavir-lambda-list)
    (cmp-log "new-body -> %s%N" new-body)
;;;    (bformat *debug-io* "old  -> %s %s %s %s%N" lambda-list-handler declares docstring code)
;;;    (bformat *debug-io* "new-body -> %s%N" new-body)
    (let* ((name (core:extract-lambda-name-from-declares declares (or given-name 'cl:lambda)))
           (fn (with-new-function (fn fn-env result
                                      :function-name name
                                      :parent-env env-around-lambda
                                      :function-form new-body
                                      :linkage linkage
                                      :function-info (make-function-info
                                                      :function-name name
                                                      :lambda-list lambda-list
                                                      :docstring docstring
                                                      :declares declares
                                                      :form code
                                                      :lineno (core:source-pos-info-lineno core:*current-source-pos-info*)
                                                      :column (core:source-pos-info-column core:*current-source-pos-info*)
                                                      :filepos (core:source-pos-info-filepos core:*current-source-pos-info*))
                                      )
                 (cmp-log "Starting new function name: %s%N" name)
                 ;; The following injects a debugInspectT_sp at the start of the body
                 ;; it will print the address of the literal which must correspond to an entry in the
                 ;; load time values table
                 #+(or)(irc-intrinsic-call "debugInspectT_sp" (list (literal:compile-reference-to-literal :This-is-a-test)))
                 (let* ((arguments      (llvm-sys:get-argument-list fn))
                        (callconv       (bclasp-setup-calling-convention arguments lambda-list core::*debug-bclasp* #|!DEBUG-ON|#)))
                   (calling-convention-maybe-push-invocation-history-frame callconv)
                   (let ((new-env (bclasp-compile-lambda-list-code cleavir-lambda-list fn-env callconv)))
                     (cmp-log "Created new register environment -> %s%N" new-env)
                     (with-try
                         (progn
                           (if wrap-block
                               (codegen-block result (list* block-name (list new-body)) new-env)
                               (codegen-progn result (list new-body) new-env)))
                       ((cleanup)
                        (cmp-log "About to calling-convention-maybe-pop-invocation-history-frame%N")
                        (calling-convention-maybe-pop-invocation-history-frame callconv)
                        (irc-unwind-environment new-env))))))))
      (cmp-log "About to dump the function constructed by generate-llvm-function-from-code%N")
      (cmp-log-dump-function fn)
      (irc-verify-function fn)
      ;; Return the llvm Function and the symbol/setf name
      (if (null name) (error "The lambda name is nil"))
      (values fn name lambda-list))))


(defun compile-lambda-function (lambda-or-lambda-block &optional env &key (linkage 'llvm-sys:internal-linkage))
  "Compile a lambda form and return an llvm-ir function that evaluates it.
Return the same things that generate-llvm-function-from-code returns"
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
      (cmp-log "About to create lambda-list-handler%N")
      (generate-llvm-function-from-code nil
                                        lambda-list
                                        declares
                                        docstring
                                        code
                                        env
                                        :wrap-block wrap-block
                                        :block-name block-name
                                        :linkage linkage))))

(defun generate-llvm-function-from-interpreted-function (fn)
  "Extract everything necessary to compile an interpreted function and
then compile it and return (values compiled-llvm-function lambda-name)"
  (let ((lambda-list (core:lambda-list-handler-lambda-list (function-lambda-list-handler fn)))
	(declares (function-declares fn))
	(docstring (function-docstring fn))
	(code (function-source-code fn))
	(env (closed-environment fn)))
    (generate-llvm-function-from-code nil lambda-list declares docstring code env :linkage 'llvm-sys:external-linkage)))

(defun generate-lambda-expression-from-interpreted-function (fn)
  (let* ((lambda-list-handler (function-lambda-list-handler fn))
	 (lambda-list (core:lambda-list-handler-lambda-list lambda-list-handler))
	 (declares (function-declares fn))
	 (docstring (docstring fn))
	 (code (code fn))
	 (env (closed-environment fn)))
    (when docstring (setq docstring (list docstring)))
    #+(or)(progn
	    (bformat t "lambda-list = %s%N" lambda-list)
	    (bformat t "declares    = %s%N" declares)
	    (bformat t "docstring   = %s%N" docstring)
	    (bformat t "code        = %s%N" code)
	    (bformat t "env         = %s%N" env))
    (values `(lambda ,lambda-list ,@docstring (declare ,@declares) ,@code) env)))

(defun function-name-from-lambda (name)
    (cond
      ((symbolp name) (symbol-name name))
      ((consp name) (bformat nil "%s" name))
      (t (error "Add support for function-name-from-lambda with ~a as arg" name))))

(defun compile-to-module (form env pathname &key (linkage 'llvm-sys:internal-linkage))
  (with-lexical-variable-optimizer (t)
      (multiple-value-bind (fn function-kind wrapped-env lambda-name)
          (with-debug-info-generator (:module *the-module* :pathname pathname)
            (multiple-value-bind (llvm-function-from-lambda lambda-name)
                (compile-lambda-function form env :linkage linkage)
              (or llvm-function-from-lambda (error "There was no function returned by compile-lambda-function inner: ~a" llvm-function-from-lambda))
              (or lambda-name (error "Inner lambda-name is nil - this shouldn't happen"))
              (values llvm-function-from-lambda :function env lambda-name)))
        (or lambda-name (error "lambda-name is nil - this shouldn't happen"))
        (or fn (error "There was no function returned by compile-lambda-function outer: ~a" fn))
        (cmp-log "fn --> %s%N" fn)
        (cmp-log-dump-module *the-module*)
        (values fn function-kind wrapped-env lambda-name))))

(defun compile-to-module-with-run-time-table (definition env pathname &key (linkage 'llvm-sys:internal-linkage))
  (let* (fn function-kind wrapped-env lambda-name)
    (multiple-value-bind (ordered-raw-constants-list constants-table startup-fn shutdown-fn)
        (literal:with-rtv
            (multiple-value-setq (fn function-kind wrapped-env lambda-name)
              (compile-to-module definition env pathname :linkage linkage)))
      (values fn function-kind wrapped-env lambda-name ordered-raw-constants-list constants-table startup-fn shutdown-fn))))

(defun bclasp-compile* (bind-to-name &optional definition env pathname &key (linkage 'llvm-sys:internal-linkage))
  "Compile the definition"
  (multiple-value-bind (fn function-kind wrapped-env lambda-name ordered-raw-constants-list constants-table startup-fn shutdown-fn)
      (compile-to-module-with-run-time-table definition env pathname :linkage linkage)
    (quick-module-dump *the-module* "preoptimize")
    (let ((compiled-function (jit-add-module-return-function *the-module* fn startup-fn shutdown-fn ordered-raw-constants-list)))
      compiled-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Codegen proper

(defun codegen-lookup-function (fn-designator evaluate-env)
  "Return a pointer to a core::Closure"
  (if (atom fn-designator)
      (let ((classified (function-info evaluate-env fn-designator)))
	(if (eq (car classified) 'core::global-function)
	    (irc-intrinsic "va_symbolFunction" (irc-global-symbol fn-designator evaluate-env))
            (irc-lexical-function-lookup classified evaluate-env)))
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
             (temp-result (irc-alloca-t*)))
         ;; evaluate the arguments into the array
         ;;  used to be done by --->    (codegen-evaluate-arguments (cdr form) evaluate-env)
         (do* ((cur-exp (cdr form) (cdr cur-exp))
               (exp (car cur-exp) (car cur-exp))
               (i 0 (+ 1 i)))
              ((endp cur-exp) nil)
           (codegen temp-result exp evaluate-env)
           (push (irc-load temp-result) args))
         (let ((closure (codegen-lookup-function head evaluate-env)))
	   (irc-low-level-trace :flow)
           (irc-funcall result closure (reverse args))
	   (irc-low-level-trace :flow))))
      ((and (consp head) (eq head 'lambda))
       (error "Handle lambda applications"))
      (t (compiler-error form "Illegal head for form %s" head)))))

(defun codegen-application (result form env)
  "A compiler macro function, macro function or a regular function"
  (assert-result-isa-llvm-value result)
  (dbg-set-current-source-pos form)
  (prog1
      (cond
        ;; A compiler macro
        ((and (symbolp (car form))
              (not (core:lexical-function (car form) env))
              (not (core:lexical-macro-function (car form) env))
              (not (core:declared-global-notinline-p (car form)))
              (let ((expansion (core:bclasp-compiler-macroexpand form env)))
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
           (declare (core:lambda-name codegen-application--about-to-macroexpand))
           (cmp-log "MACROEXPANDed form[%s] expanded to [%s]%N" form expansion )
           (irc-low-level-trace)
           (codegen result expansion env) 
           ))
        ;; It's a regular function call
        (t
         (codegen-call result form env))) 
    ))

(defun codegen-special-operator (result head rest env)
  (cmp-log "entered codegen-special-operator head: %s rest: %s%N" head rest)
  (assert-result-isa-llvm-value result)
  (cmp-log "About to set source pos%N")
  (dbg-set-current-source-pos rest)
  (cmp-log "About to do case on head: %s%N" head)
  (let* ((functions (gethash head *special-operator-dispatch* 'nil))
         (function (cadr functions)))
    (if function
	(funcall function result rest env)
	(error "Unknown special operator : ~a" head)))
  )

;;; Return true if the symbol should be treated as a special operator
;;; Special operators that are handled as macros are exempt
(defun treat-as-special-operator-p (sym)
  (cond
    ((eq sym 'cl:unwind-protect) nil)     ;; handled with macro
    ((eq sym 'cl:multiple-value-prog1) nil)     ;; handled with macro
    ((eq sym 'cl:catch) nil)              ;; handled with macro
    ((eq sym 'cl:throw) nil)              ;; handled with macro
    ((eq sym 'cl:progv) nil)              ;; handled with macro
    ((eq sym 'core:debug-message) t)      ;; special operator
    ((eq sym 'core:multiple-value-foreign-call) t) ;; Call intrinsic functions
    ((eq sym 'core:foreign-call-pointer) t) ;; Call function pointers
    ((eq sym 'core:foreign-call) t)         ;; Call foreign function
    ((eq sym 'core:bind-va-list) t)         ;; bind-va-list
    (t (special-operator-p sym))))

(export 'treat-as-special-operator-p)

(defun codegen (result form env)
;;;  (declare (optimize (debug 3)))
  (assert-result-isa-llvm-value result) 
  (multiple-value-bind (source-directory source-filename lineno column)
      (dbg-set-current-source-pos form)
    (let* ((*current-form* form)
           (*current-env* env))
      (cmp-log "codegen stack-used[%d bytes]%N" (stack-used))
      (cmp-log "codegen evaluate-depth[%d]  %s%N" (evaluate-depth) form)
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
            (cmp-log "About to codegen special-operator or application for: %s%N" form)
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

(defun compile-thunk (name form env optimize)
  "Compile the form into an llvm function and return that function"
  (with-lexical-variable-optimizer (optimize)
    (let ((top-level-func (with-new-function (fn
                                              fn-env
                                              result
                                              :function-name name
                                              :parent-env env
                                              :function-form form
                                              :function-info (make-function-info
                                                              :function-name name
                                                              :lambda-list nil
                                                              :docstring nil
                                                              :declares nil
                                                              :form form
                                                              :lineno (core:source-pos-info-lineno core:*current-source-pos-info*)
                                                              :column 0
                                                              :filepos (core:source-pos-info-filepos core:*current-source-pos-info*))
                                              )
                            (let* ((given-name (llvm-sys:get-name fn)))
                              ;; Map the function argument names
                              (cmp-log "Creating repl function with name: %s%N" given-name)
                              ;;	(break "codegen repl form") 
                              (codegen result form fn-env)))))
      (cmp-log "Dumping the repl function%N")
      (cmp-log-dump-function top-level-func)
      (irc-verify-function top-level-func t)
      top-level-func)))
