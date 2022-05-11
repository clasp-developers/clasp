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
                                         original-declares
                                         ;; lambda docstring
                                         docstring
                                         ;; lambda code
                                         code
                                         ;; environment of the lambda
                                         env-around-lambda
                                         &key
                                         (linkage 'llvm-sys:internal-linkage))
  "This is where llvm::Function are generated from code, declares, 
lambda-list, environment.
All code generation comes through here.   Return (llvm:function lambda-name)
Could return more functions that provide lambda-list for swank for example"
  (setq *lambda-args-num* (1+ *lambda-args-num*))
  (multiple-value-bind (cleavir-lambda-list-analysis new-body)
      (transform-lambda-parts lambda-list original-declares code)
    (let ((declares (core:canonicalize-declarations original-declares)))
      (cmp-log "generate-llvm-function-from-code%N")
      (cmp-log "cleavir-lambda-list-analysis -> {}%N" cleavir-lambda-list-analysis)
      (cmp-log "new-body -> {}%N" new-body)
;;;    (core:fmt *error-output* "old  -> {} {} {} {}%N" lambda-list-handler declares docstring code)
;;;    (core:fmt *error-output* "new-body -> {}%N" new-body)
      (let ((name (core:extract-lambda-name-from-declares
                   declares (or given-name
                                `(cl:lambda ,(lambda-list-for-name lambda-list))))))
        (let* ((xep-group 
                 (with-new-function (local-fn fn-env result
                                              :function-name name
                                              :parent-env env-around-lambda
                                              :linkage linkage
                                              :function-info (make-function-info
                                                              :function-name name
                                                              :lambda-list lambda-list
                                                              :cleavir-lambda-list-analysis cleavir-lambda-list-analysis
                                                              :docstring docstring
                                                              :declares declares
                                                              :spi core:*current-source-pos-info*))
                   (cmp-log "Starting new function name: {}%N" name)
                   ;; The following injects a debugInspectT_sp at the start of the body
                   ;; it will print the address of the literal which must correspond to an entry in the
                   ;; load time values table
                   #+(or)(irc-intrinsic-call "debugInspectT_sp" (list (literal:compile-reference-to-literal :This-is-a-test)))
                   (let* ((arguments      (llvm-sys:get-argument-list local-fn))
                          (callconv       :was-callconv ))
                     (declare (ignorable arguments) (ignore callconv))
                     (cmp-log "argument-list {}%N" arguments)
                     (cmp-log "fn-env -> {}%N" fn-env)
                     (let ((new-env fn-env)) ; (irc-make-unbound-value-environment-of-size fn-env:was-new-env-codegen #+(or)(bclasp-compile-lambda-list-code fn-env callconv)))
                       (cmp-log "Created new register environment -> {}%N" new-env)
                       ;; I am not certain - but I suspect that (irc-environment-has-cleanup new-env) is always FALSE
                       ;; in which case the (with-try ...) can be removed
                       ;; Christian Schafmeister June 2019
                       (if (irc-environment-has-cleanup new-env)
                           (with-try "TRY.func"
                             (codegen-progn result (list new-body) new-env)
                             ((cleanup)
                              (irc-unwind-environment new-env)))
                           (codegen-progn result (list new-body) new-env))))))
               (local-fn (xep-group-local-function xep-group)))
          (cmp-log "About to dump the function constructed by generate-llvm-function-from-code%N")
          (cmp-log-dump-function local-fn)
          (unless *suppress-llvm-output* (irc-verify-function local-fn))
          ;; Return the llvm Function and the symbol/setf name
          (if (null name) (error "The lambda name is nil"))
          (when (null xep-group) (error "The xep-group is nil"))
          (when (null local-fn) (error "The local-fn is nil"))
          (make-bclasp-llvm-function-info :xep-function xep-group
                                          :local-function local-fn
                                          ))))))

;;; Given a lambda list, return a lambda list suitable for display purposes.
;;; This means only the external interface is required.
;;; No default values, no -p variables, no &aux.
(defun lambda-list-for-name (raw-lambda-list)
  (multiple-value-bind (req opt rest keyflag keys aok-p)
      (core:process-lambda-list raw-lambda-list 'function)
    `(,@(rest req)
      ,@(unless (zerop (first opt)) '(&optional))
      ,@(let ((optnames nil))
          (do ((opts (rest opt) (cdddr opts)))
              ((null opts) (nreverse optnames))
            (push (first opts) optnames)))
      ,@(when rest `(&rest ,rest))
      ,@(when keyflag '(&key))
      ,@(let ((keykeys nil))
          (do ((key (rest keys) (cddddr key)))
              ((null key) keykeys)
            (push (first key) keykeys)))
      ,@(when aok-p '(&allow-other-keys)))))

(defun compile-lambda-function (lambda-or-lambda-block &optional env &key (linkage 'llvm-sys:internal-linkage))
  "Compile a lambda form and return an llvm-ir function that evaluates it.
Return the same things that generate-llvm-function-from-code returns"
  (let ((lambda-list (cadr lambda-or-lambda-block))
	(body (cddr lambda-or-lambda-block)))
    (multiple-value-bind (declares code docstring)
	(process-declarations body t)
      (cmp-log "About to create lambda-list-handler%N")
      (generate-llvm-function-from-code nil
                                        lambda-list
                                        declares
                                        docstring
                                        code
                                        env
                                        :linkage linkage))))
#+(or)
(defun generate-llvm-function-from-interpreted-function (fn)
  "Extract everything necessary to compile an interpreted function and
then compile it and return (values compiled-llvm-function lambda-name)"
  (let ((lambda-list (core:lambda-list-handler-lambda-list (function-lambda-list-handler fn)))
	#+(or)(declares (function-declares fn))
	(docstring (function-docstring fn))
	(code (interpreted-source-code fn))
	(env (closed-environment fn)))
    (generate-llvm-function-from-code nil lambda-list #|declares|# nil docstring code env :linkage 'llvm-sys:internal-linkage)))

(defun generate-lambda-expression-from-interpreted-function (fn)
  (let* ((lambda-list-handler (function-lambda-list-handler fn))
	 (lambda-list (core:lambda-list-handler-lambda-list lambda-list-handler))
	 #+(or)(declares (function-declares fn))
	 (docstring (function-docstring fn))
	 (code (interpreted-source-code fn))
	 (env (closed-environment fn)))
    (when docstring (setq docstring (list docstring)))
    #+(or)(progn
	    (core:fmt t "lambda-list = {}%N" lambda-list)
	    #+(or)(core:fmt t "declares    = {}%N" declares)
	    (core:fmt t "docstring   = {}%N" docstring)
	    (core:fmt t "code        = {}%N" code)
	    (core:fmt t "env         = {}%N" env))
    (values `(lambda ,lambda-list ,@docstring #| (declare ,@declares)|# ,@code) env)))

(defun function-name-from-lambda (name)
    (cond
      ((symbolp name) (symbol-name name))
      ((consp name) (core:fmt nil "{}" name))
      (t (error "Add support for function-name-from-lambda with ~a as arg" name))))

(defun potentially-save-module ()
  (when *save-module-for-disassemble*
    (setq *saved-module-from-clasp-jit*
          (with-output-to-string (*standard-output*)
            (llvm-sys:dump-module *the-module* *standard-output*)))))

(defun compile-to-module (&key definition env pathname (linkage 'llvm-sys:internal-linkage))
  (with-lexical-variable-optimizer (t)
    (multiple-value-bind (fn function-kind wrapped-env)
        (with-debug-info-generator (:module *the-module* :pathname pathname)
          (let* ((function-info (compile-lambda-function definition env :linkage linkage))
            ;; (multiple-value-bind (llvm-function-from-lambda lambda-name function-description-reference)
                 (llvm-function-from-lambda (bclasp-llvm-function-info-xep-function function-info)))
            (or llvm-function-from-lambda (error "There was no function returned by compile-lambda-function inner: ~a" llvm-function-from-lambda))
            (values llvm-function-from-lambda :function env)))
      (or fn (error "There was no function returned by compile-lambda-function outer: ~a" fn))
      (potentially-save-module)
      (cmp-log "fn --> {}%N" fn)
      (cmp-log-dump-module *the-module*)
      (values fn function-kind wrapped-env))))

(defun compile-to-module-with-run-time-table (&key definition env pathname (linkage 'llvm-sys:internal-linkage))
  (let* (fn function-kind wrapped-env)
    (multiple-value-bind (ordered-raw-constants-list constants-table startup-shutdown-id)
        (literal:with-rtv
            (multiple-value-setq (fn function-kind wrapped-env)
              (compile-to-module
               :definition definition
               :env env
               :pathname pathname
               :linkage linkage)))
      (values fn function-kind wrapped-env ordered-raw-constants-list constants-table startup-shutdown-id))))

(defun bclasp-compile* (definition env pathname
                        &key (linkage 'llvm-sys:internal-linkage) name)
  "Compile the definition using the bclasp compiler"
  (when core:*debug-startup*
    (core:monitor-write (core:fmt nil "startup bclasp-compile* form: {}%N" definition)))
  (multiple-value-bind (fn function-kind wrapped-env ordered-raw-constants-list constants-table startup-shutdown-id)
      (compile-to-module-with-run-time-table
       :definition definition
       :env env
       :pathname pathname
       :linkage linkage)
    (declare (ignore function-kind wrapped-env constants-table))
    (quick-module-dump *the-module* "preoptimize")
    (let ((compiled-function (jit-add-module-return-function *the-module* fn startup-shutdown-id ordered-raw-constants-list :name name)))
      compiled-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Codegen proper

(defun codegen-lookup-function (fn-designator evaluate-env)
  "Return a pointer to a core::Closure"
  (if (atom fn-designator)
      (let ((classified (function-info evaluate-env fn-designator)))
	(if (eq (car classified) 'core::global-function)
            (multiple-value-bind (symbol label)
                (irc-global-symbol fn-designator evaluate-env)
              (values (irc-fdefinition symbol) label))
            (values (irc-lexical-function-lookup classified evaluate-env) "lexical-func")))
      (if (eq (car fn-designator) 'cl:lambda)
	  (error "Handle lambda expressions at head of form")
	  (error "Illegal head of form: ~a" fn-designator))))

;; given an llvm value for the function (which is an actual function, not a name or lambda),
;; and a list of argument forms, codegen a call.
(defun codegen-call (result fn args env &optional (label ""))
  (let ((argsz nil) (temp (alloca-t* "arg")))
    (dolist (arg args)
      (codegen temp arg env)
      (push (irc-load temp) argsz))
    (irc-funcall result fn (nreverse argsz) label)))

;; Codegen a call to a named function.
(defun codegen-named-call (result fname args env)
  (assert-result-isa-llvm-value result)
  (multiple-value-bind (closure label)
      (codegen-lookup-function fname env)
    (codegen-call result closure args env label)))

(defun codegen-lambda-form (result lambda args env)
  (let ((temp-closure (alloca-t*)))
    (codegen temp-closure lambda env)
    (codegen-call result (irc-load temp-closure) args env)))

(defun codegen-special-operator (result head rest env)
  (cmp-log "entered codegen-special-operator head: {} rest: {}%N" head rest)
  (assert-result-isa-llvm-value result)
  (cmp-log "About to set source pos%N")
  (cmp-log "About to do case on head: {}%N" head)
  (let ((function (gethash head *special-operator-dispatch* 'nil)))
    (if function
	(funcall function result rest env)
	(error "Unknown special operator : ~a" head))))

;;; Return true if the symbol should be treated as a special operator
;;; Special operators that are handled as macros are exempt
(defun treat-as-special-operator-p (sym)
  (and (symbolp sym)
       ;;; perhaps the test (symbolp sym) should be done in the callers
       ;;; done here, since special-operator-p should type-error on a non-symbol
       ;;; and bclasp is calling treat-as-special-operator-p on forms too
       (cond
         ((eq sym 'cl:unwind-protect) nil)     ;; handled with macro
         ((eq sym 'cl:catch) nil)              ;; handled with macro
         ((eq sym 'cl:throw) nil)              ;; handled with macro
         ((eq sym 'cl:progv) nil)              ;; handled with macro
         ((eq sym 'core:debug-message) t)      ;; special operator
         ((eq sym 'core:debug-break) t)      ;; special operator
         ((eq sym 'core:multiple-value-foreign-call) t) ;; Call intrinsic functions
         ((eq sym 'core:foreign-call-pointer) t) ;; Call function pointers
         ((eq sym 'core:foreign-call) t)         ;; Call foreign function
         ((eq sym 'core:bind-vaslist) t)         ;; bind-vaslist
         ((eq sym 'core::vector-length) t)
         ((eq sym 'core::%array-dimension) t)
         ((eq sym 'core::fence) t)
         ((eq sym 'cleavir-primop:car) t)
         ((eq sym 'cleavir-primop:cdr) t)
         ((eq sym 'core::car-atomic) t)
         ((eq sym 'core::cdr-atomic) t)
         ((eq sym 'core::rplaca-atomic) t)
         ((eq sym 'core::rplacd-atomic) t)
         ((eq sym 'core::cas-car) t)
         ((eq sym 'core::cas-cdr) t)
         ((eq sym 'cleavir-primop:funcall) t)
         ((eq sym 'cleavir-primop:unreachable) t)
         ((eq sym 'cleavir-primop:case) t)
         ((eq sym 'core:vaslist-pop) t)
         ((eq sym 'core:vaslist-length) t)
         ((eq sym 'core::local-block) t)
         ((eq sym 'core::local-tagbody) t)
         ((eq sym 'core::header-stamp-case) t)
         ((eq sym 'core::header-stamp) t)
         ((eq sym 'core::derivable-stamp) t)
         ((eq sym 'core::wrapped-stamp) t)
         ((eq sym 'core::rack-stamp) t)
         ((eq sym 'core:instance-ref) t)
         ((eq sym 'core:instance-set) t)
         ((eq sym 'core::instance-cas) t)
         ((eq sym 'core:instance-rack) t)
         ((eq sym 'core:instance-rack-set) t)
         ((eq sym 'core:rack-ref) t)
         ((eq sym 'core:rack-set) t)
         ((eq sym 'core::atomic-rack-read) t)
         ((eq sym 'core::atomic-rack-write) t)
         ((eq sym 'core::cas-rack) t)
         ((eq sym 'core:defcallback) t)
         (t (special-operator-p sym)))))

(export 'treat-as-special-operator-p)

(defun codegen-cons (result form env)
  (let ((head (car form))
        (rest (cdr form)))
    (cmp-log "About to codegen special-operator or application for: {}%N" form)
    (cond
      ;; special form
      ((treat-as-special-operator-p head)
       (codegen-special-operator result head rest env))
      ;; lambda form
      ((and (consp head) (eq (car head) 'cl:lambda))
       (codegen-lambda-form result head rest env))
      ;; invalid
      ((not (symbolp head))
       (error "Invalid form head: ~a" head))
      ;; compiler macro
      ((and (not (core:lexical-function head env))
            (not (core:lexical-macro-function head env))
            (not (core:declared-global-notinline-p head))
            (not (member head *notinlines*))
            (let ((expansion (core:compiler-macroexpand form env)))
              (if (eq expansion form)
                  nil
                  (progn
                    (codegen result expansion env)
                    t)))))
      ;; macro
      ((and (not (core:lexical-function head env))
            (macro-function head env))
       (codegen result (macroexpand-1 form env) env))
      ;; regular function call
      (t
       (codegen-named-call result head rest env)))))

(defun codegen (result form env)
;;;  (declare (optimize (debug 3)))
  (assert-result-isa-llvm-value result)
  (cmp-log "codegen stack-used[{} bytes]%N" (stack-used))
  ;;
  ;; If a *code-walker* is defined then invoke the code-walker
  ;; with the current form and environment
  (when *code-walker*
    (setq form (funcall *code-walker* form env)))
  (cond ((symbolp form) (codegen-symbol-value result form env))
        ((consp form) (codegen-cons result form env))
        (t (codegen-literal result form env))))

(defun compile-thunk (name form env optimize)
  "Compile the form into an llvm function and return that function"
  (with-lexical-variable-optimizer (optimize)
    (let ((top-level-func (with-new-function (local-fn
                                              fn-env
                                              result
                                              :function-name name
                                              :parent-env env
                                              :linkage cmp:*default-linkage*
                                              :function-info (make-function-info
                                                              :function-name name
                                                              :lambda-list nil
                                                              :docstring nil
                                                              :declares nil
                                                              :spi core:*current-source-pos-info*))
                            ;; Map the function argument names
                            (cmp-log "Creating repl function with name: {}%N" (llvm-sys:get-name local-fn))
                            ;;	(break "codegen repl form") 
                            (codegen result form fn-env))))
      (cmp-log "Dumping the repl function%N")
      (cmp-log-dump-function top-level-func)
      (unless *suppress-llvm-output* (irc-verify-function top-level-func t))
      top-level-func)))
