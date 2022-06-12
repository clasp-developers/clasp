(in-package #:cmp)

;;;; bclasp compilation of special forms.

(export 'llvm-inline)

(core:defconstant-equal +special-operator-dispatch+
  '(
    (progn codegen-progn)
    (core:bind-vaslist codegen-bind-vaslist)
    (if codegen-if)
    (block  codegen-block)
    (core::local-block  codegen-local-block)
    (return-from  codegen-return-from)
    (setq codegen-setq)
    (let codegen-let)
    (let* codegen-let*)
    (function  codegen-function)
    (tagbody codegen-tagbody)
    (core::local-tagbody codegen-local-tagbody)
    (go codegen-go)
    (multiple-value-call  codegen-multiple-value-call)
    (multiple-value-prog1 codegen-multiple-value-prog1)
    (flet  codegen-flet)
    (labels  codegen-labels)
    (eval-when  codegen-eval-when)
    (the  codegen-the)
    (locally  codegen-locally)
    (quote  codegen-quote)
    (macrolet  codegen-macrolet)
    (dbg-i32  codegen-dbg-i32)
    (load-time-value  codegen-load-time-value)
    (core:multiple-value-foreign-call codegen-multiple-value-foreign-call)
    (core:foreign-call codegen-foreign-call)
    (core:foreign-call-pointer codegen-foreign-call-pointer)
    (symbol-macrolet  codegen-symbol-macrolet)
    (core::vector-length codegen-vector-length)
    (core::%array-dimension codegen-%array-dimension)
    (core::fence codegen-fence)
    (cleavir-primop:car codegen-car)
    (cleavir-primop:cdr codegen-cdr)
    (core::car-atomic codegen-atomic-car)
    (core::cdr-atomic codegen-atomic-cdr)
    (core::rplaca-atomic codegen-atomic-rplaca)
    (core::rplacd-atomic codegen-atomic-rplacd)
    (core::cas-car codegen-cas-car)
    (core::cas-cdr codegen-cas-cdr)
    (cleavir-primop:funcall codegen-primop-funcall)
    (cleavir-primop:unreachable codegen-unreachable)
    (cleavir-primop:case codegen-primop-case)
    (core:vaslist-pop codegen-vaslist-pop)
    (core:vaslist-length codegen-vaslist-length)
    (core::header-stamp-case codegen-header-stamp-case)
    (core::header-stamp codegen-header-stamp)
    (core::rack-stamp codegen-rack-stamp)
    (core::derivable-stamp codegen-derivable-stamp)
    (core::wrapped-stamp codegen-wrapped-stamp)
    (core:instance-ref codegen-instance-ref)
    (core:instance-set codegen-instance-set)
    (core::instance-cas codegen-instance-cas)
    (core:instance-rack codegen-instance-rack)
    (core:instance-rack-set codegen-instance-rack-set)
    (core:rack-ref codegen-rack-ref)
    (core:rack-set codegen-rack-set)
    (core::atomic-rack-read codegen-atomic-rack-read)
    (core::atomic-rack-write codegen-atomic-rack-write)
    (core::cas-rack codegen-cas-rack)
    (llvm-inline codegen-llvm-inline)
    (:gc-profiling codegen-gc-profiling)
    (core::debug-message codegen-debug-message)
    (core::debug-break codegen-debug-break)
    (core:defcallback codegen-defcallback)
    ))

(defun make-dispatch-table (alist)
  (let ((hash (make-hash-table :size (max 128 (* 2 (length alist))) :test #'eq)))
    (dolist (entry alist)
      (let ((name (first entry))
	    (codegen-function (second entry)))
	(core::hash-table-setf-gethash hash name codegen-function)))
    hash))

(defvar *special-operator-dispatch* (make-dispatch-table +special-operator-dispatch+))

;;; FUNCTION

(defun codegen-closure (result lambda-or-lambda-block env)
  "codegen a closure.  If result is defined then put the compiled function into result
- otherwise return the cons of llvm-sys::Function_sp's that were compiled for the lambda"
  (assert-result-isa-llvm-value result)
  (let* ((bclasp-llvm-function-info (compile-lambda-function lambda-or-lambda-block env))
         (xep-group (bclasp-llvm-function-info-xep-function bclasp-llvm-function-info)))
    (cmp-log "codegen-closure xep-group {}%N" xep-group)
    (if result
        ;; TODO:   Here walk the source code in lambda-or-lambda-block and
        ;; get the line-number/column for makeCompiledFunction
        (let* ((runtime-environment (irc-load (irc-renv env)))
               (fnptr (irc-intrinsic "makeCompiledFunction" 
                                     (literal:constants-table-value (cmp:entry-point-reference-index (xep-group-entry-point-reference xep-group)))
                                     runtime-environment)))
          (irc-t*-result fnptr result)))
    (values)))

(defun codegen-global-function-lookup (result sym env)
  ;; Was symbolFunctionRead
  (let ((symbol-function (irc-fdefinition (irc-global-symbol sym env))))
    (irc-t*-result symbol-function result)))

(defun codegen-global-setf-function-lookup (result setf-function-name env)
  (let* ((setf-symbol (cadr setf-function-name))
         (setf-symbol-function (irc-setf-fdefinition (irc-global-setf-symbol setf-symbol env))))
    (irc-t*-result setf-symbol-function result)))

(defun codegen-lexical-function-lookup (result classified env)
  (let ((lexical-function (irc-lexical-function-lookup classified env)))
    (irc-t*-result lexical-function result)))

(defun codegen-function-symbol-lookup (result func env)
  "Classify the function and look it up and put it in result"
  (let* ((classified (function-info env func)))
    (if (eq (car classified) 'core::global-function)
	(codegen-global-function-lookup result func env)
	(codegen-lexical-function-lookup result classified env))))

(defun codegen-function-setf-symbol-lookup (result setf-func env)
  "Classify the (setf XXXX) function and put it in the result"
  (let* ((classified (function-info env setf-func)))
    (if (eq (car classified) 'core::global-function)
	(codegen-global-setf-function-lookup result setf-func env)
	(codegen-lexical-function-lookup result classified env))))

(defun codegen-function (result rest env)
  "Return IR code for a function or closure"
  (let ((name-or-lambda (car rest)))
    (assert-result-isa-llvm-value result)
    (cmp-log "About to codegen-function for: {}%N" name-or-lambda)
    (cond
      ((and name-or-lambda (symbolp name-or-lambda))
       (codegen-function-symbol-lookup result name-or-lambda env))
      ((and (consp name-or-lambda)
            (eq (car name-or-lambda) 'setf))
       (codegen-function-setf-symbol-lookup result name-or-lambda env))
      ((and (consp name-or-lambda)
            (or (eq (car name-or-lambda) 'lambda)
                (eq (car name-or-lambda) 'ext::lambda-block)))
       (codegen-closure result name-or-lambda env))
      (t (error "FUNCTION special operator only supports symbol names or lambda expression - you gave: ~a" name-or-lambda)))))

;;; PROGN

(defun codegen-progn (result forms env)
  "Evaluate forms discarding results but keep last one"
  (cmp-log "About to codegen-progn with forms: {}%N" forms)
  (cmp-log "Dumping the module%N")
  (cmp-log-dump-module *the-module*)
  (if forms
      (let ((temp-val (alloca-t* "temp")))
        (do* ((cur forms (cdr cur))
              (form (car cur) (car cur)))
             ((endp cur) nil)
          (if (cdr cur)
              (codegen temp-val form env)
              (codegen result form env))))
      (codegen-literal result nil env)))

;;; IF

(defun codegen-if (result rest env)
  "See Kaleidoscope example for if/then/else"
  (let ((icond (car rest))
	(ithen (cadr rest))
	(ielse (caddr rest)))
    (when (cdddr rest)
      (format t "codegen-if (cdddr rest) = ~a ~%" (cdddr rest))
      (compiler-error nil "too many arguments for if"))
    (let* ((thenbb (irc-basic-block-create "then"))
           (elsebb (irc-basic-block-create "else"))
           (mergebb (irc-basic-block-create "ifcont")))
      ;; We have block references so we can compile the branch now.
      (compile-if-cond icond env thenbb elsebb)

      ;; Compile the THEN branch.
      (irc-begin-block thenbb)
      (irc-low-level-trace)
      (codegen result ithen env)
      (irc-branch-if-no-terminator-inst mergebb)

      ;; Compile the ELSE branch.
      (irc-begin-block elsebb)
      (irc-low-level-trace)
      (codegen result ielse env)
      (irc-branch-if-no-terminator-inst mergebb)

      ;; Everything after starts at the block THEN and ELSE merge into.
      (irc-begin-block mergebb))))

;;; PROGV

(defmacro progv (symbols values &body forms)
  `(core:progv-function ,symbols ,values
                        (lambda ()
                          (declare (core:lambda-name core::progv-lambda))
                          (progn ,@forms))))

;;; MULTIPLE-VALUE-CALL

(defun codegen-multiple-value-call (result rest env)
  (with-dbg-lexical-block ()
    (let* ((function-form `(core:coerce-fdesignator ,(car rest)))
           (forms (cdr rest)))
      (if (= (length forms) 1)
          (let ((temp-mv-result (alloca-tmv "temp-mv-result"))
                (funcDesignator (alloca-t* "funcDesignator"))
                (form (car forms)))
            (codegen funcDesignator function-form env)
            (codegen temp-mv-result form env)
;;            (irc-intrinsic "saveToMultipleValue0" temp-mv-result)
            (let ((register-ret (irc-intrinsic "cc_call_multipleValueOneFormCallWithRet0"
                                               (irc-load funcDesignator)
                                               (irc-load temp-mv-result))))
              (irc-tmv-result register-ret result)))
          (codegen result
                   `(core:multiple-value-funcall
                     ,function-form
                     ,@(mapcar (lambda (x)
                                 `#'(lambda ()
                                      (declare (core:lambda-name core::mvc-argument-lambda))
                                      (progn ,x)))
                               forms))
                   env)))))

;;; MULTIPLE-VALUE-PROG1

(defun codegen-multiple-value-prog1 (result rest env)
  (let ((first-form (first rest))
        (forms (rest rest)))
    (if (null forms) ; triviality check
        (codegen result first-form env)
        ;; Evaluate the first form into a new tmv,
        ;; then alloca a vector to store the values in,
        ;; save the values in,
        ;; run the rest of the forms,
        ;; and finally the stored values are the result.
        (let ((tmvp (alloca-tmv "mvp1-tmv")))
          (codegen tmvp first-form env)
          (let* ((tmv (irc-load tmvp))
                 (primary (irc-tmv-primary tmv))
                 (nvals (irc-tmv-nret tmv))
                 (temp (alloca-temp-values nvals "mvp1-temp")))
            (irc-intrinsic "cc_save_values" nvals primary temp)
            ;; we have extracted what we need from the tmv, so just reuse it
            ;; (and then discard it cruelly)
            (codegen-progn tmvp forms env)
            (irc-tmv-result (irc-intrinsic "cc_load_values" nvals temp) result))))))

;;; SETQ

#+(or)
(defun codegen-special-var-reference (var &optional env)
  (core::fmt t "In codegen-special-var-reference for {}%N" var)
  (irc-intrinsic "symbolValueReference" (irc-global-symbol var env) (core:fmt nil "<special-var:{}>" (symbol-name var) )))

(defun codegen-setq (result setq-pairs env)
  "Carry out setq for a collection of pairs"
  (let ((temp-res (alloca-t* "tsetq")))
    (if setq-pairs
	(do* ((cur setq-pairs (cddr cur))
	      (cur-var (car cur) (car cur))
	      (cur-expr (cadr cur) (cadr cur)))
	     ((endp cur) nil)
	  (cmp-log "Compiling setq for target[{}]%N" cur-var)
	  (let ((expanded (macroexpand cur-var env)))
	    (if (eq expanded cur-var)
		;; symbol was not macroexpanded use SETQ
		(progn
		  (cmp-log "The symbol[{}] was not macroexpanded - using SETQ to set it%N" cur-var)
		  (let* ((classified (variable-info env cur-var)))
                    (cond
                      ((eq (car classified) 'ext:special-var)
                       (codegen temp-res cur-expr env)
                       (let* ((symbol-t* (irc-global-symbol cur-var env))
                              (val (irc-load temp-res)))
                         (irc-intrinsic "cc_setSymbolValue" symbol-t* val))
                       #+(or)
                       (let ((special-target-ref (codegen-special-var-reference cur-var env)))
                         (codegen temp-res cur-expr env)
                         (irc-t*-result (irc-load temp-res) special-target-ref)))
                      ((eq (car classified) 'ext:lexical-var)
                       (let ((symbol (cadr classified))
                             (depth (third classified))
                             (index (fourth classified))
                             (dest-env (fifth classified)))
                         (let ((lexical-target-ref (codegen-lexical-var-reference symbol
                                                                                  depth
                                                                                  index
                                                                                  env
                                                                                  dest-env)))
                           (codegen temp-res cur-expr env)
                           (irc-t*-result (irc-load temp-res) lexical-target-ref))))
                      (t (error "Handle codegen-setq with ~s" classified)))
                    ))
		;; symbol was macroexpanded use SETF
		(progn
		  (cmp-log "The symbol[{}] was macroexpanded to result[{}] setting with SETF%N"
                           cur-var expanded)
		  (codegen temp-res `(setf ,expanded ,cur-expr) env))))
	  (unless (cddr cur)
	    (irc-t*-result (irc-load temp-res) result)))
	;; There were no pairs, return nil
	(codegen-literal result nil env))))

;;; LET, LET*

(defun new-notinlines (declarations)
  ;; NOTE: If there are duplicates in the code this will have
  ;; them as well, but that shouldn't meaningfully affect anything.
  (append *notinlines*
          ;; This REMOVE gets all the NOTINLINE declarations.
          ;; ...but we can't use it since we don't have REMOVE yet.
          #+(or)
          (mapcar #'second
                  (remove 'notinline :test-not #'eq :key #'car))
          (let ((result nil))
            (dolist (decl declarations result)
              (when (eq (first decl) 'notinline)
                (push (second decl) result))))))

(defun codegen-fill-let-environment (new-env reqvars
                                     exps parent-env evaluate-env)
  "Evaluate each of the exps in the evaluate-env environment
and put the values into the activation frame for new-env."
  (declare (ignore parent-env))
  ;;
  ;; Generate allocas for all of the temporary values
  ;;
  (let ((temps (make-array (length reqvars) :adjustable t :fill-pointer 0)))
    (do* ((cur-req (cdr reqvars) (cdr cur-req))
          (cur-exp exps (cdr cur-exp))
          (exp (car cur-exp) (car cur-exp))
          (temp (alloca-t* "let") (alloca-t* "let")))
         ((endp cur-req) nil)
      (vector-push-extend temp temps)
      (codegen temp exp evaluate-env))
    ;; Now generate code for let
    (cmp-log "About to generate code for exps: {}%N" exps)
    (do* ((cur-req (cdr reqvars) (cdr cur-req))
          (classified-target (car cur-req) (car cur-req))
          (tempidx 0 (1+ tempidx)))
         ((endp cur-req) nil)
      (cond
        ((eq (car classified-target) 'ext:special-var)
         (let ((symbol-t* (irc-global-symbol (cdr classified-target) new-env))
               (val (irc-load (elt temps tempidx))))
           (irc-intrinsic "cc_setTLSymbolValue" symbol-t* val)))
        ((eq (car classified-target) 'ext:lexical-var)
         (with-target-reference-do (target-ref classified-target new-env)
           (irc-t*-result (irc-load (elt temps tempidx)) target-ref)))
        (t (error "Illegal target ~s" classified-target))))))

(defun codegen-fill-let*-environment (new-env reqvars
                                      exps parent-env evaluate-env)
  (declare (ignorable parent-env))
  "Evaluate each of the exps in the evaluate-env environment
and put the values into the activation frame for new-env."
  (cmp-log "entered codegen-fill-let*-environment%N")
  (cmp-log "   new-env -> {}%N" new-env)
  (cmp-log "   parent-env -> {}%N" parent-env)
  (cmp-log "   evaluate-env -> {}%N" evaluate-env)
  ;; Now generate code for let
  (cmp-log "About to generate code for exps: {}%N" exps)
  (let ((temp-var (alloca-t* "special-save")))
    (do* ((cur-req (cdr reqvars) (cdr cur-req))
          (classified-target (car cur-req) (car cur-req))
          (cur-exp exps (cdr cur-exp))
          (exp (car cur-exp) (car cur-exp)))
         ((endp cur-req) nil)
      (cond
        ((eq (car classified-target) 'ext:special-var)
         (codegen temp-var exp evaluate-env)
         (let* ((symbol-name (cdr classified-target))
                (symbol-t* (irc-global-symbol symbol-name new-env))
                (val (irc-load temp-var)))
           (irc-intrinsic "cc_setTLSymbolValue" symbol-t* val)))
        ((eq (car classified-target) 'ext:lexical-var)
         (with-target-reference-do (target-ref classified-target new-env)
           (codegen target-ref exp evaluate-env)))
        (t (error "Illegal target ~s" classified-target))))))

(defun codegen-let/let* (operator-symbol result parts env)
  (with-dbg-lexical-block ()
    (let ((assignments (car parts))
          (body (cdr parts)))
      (multiple-value-bind (variables expressions)
          (separate-pair-list assignments)
        (multiple-value-bind (declares code)
            (process-declarations body t)
          (cmp-log "About to create lambda-list-handler%N")
          (let* ((lambda-list-handler (make-lambda-list-handler variables declares 'core::function))
                 (new-env (irc-new-unbound-value-environment-of-size
                           env
                           :number-of-arguments (number-of-lexical-variables lambda-list-handler) ;; lambda-list-handler lambda-list-handler
                           :label (symbol-name operator-symbol)))
                 (evaluate-env (cond
                                 ((eq operator-symbol 'let) env) ;;; This is a problem right here
                                 ((eq operator-symbol 'let*) new-env)
                                 (t (error "let/let* doesn't understand operator symbol[~a]" operator-symbol))))
                 (reqvars
                   (process-lambda-list-handler lambda-list-handler))
                 (number-of-lexical-vars
                   (number-of-lexical-variables lambda-list-handler))
                 (dynenv-mems nil)
                 (dynenv-cons-mems nil)
                 (dynenv-conses nil)
                 (*notinlines* (new-notinlines declares)))
            (declare (ignore dynenv-conses))
            ;; alloca dynenv space for each special.
            (dolist (target (rest reqvars))
              (when (eq (car target) 'ext:special-var)
                (push (alloca-i8 +cons-size+ :alignment +alignment+
                                             :label "dynenv-cons-mem")
                      dynenv-cons-mems)
                (push (alloca-i8 +binding-dynenv-size+
                                 :alignment +alignment+
                                 :label "binding-dynenv-mem")
                      dynenv-mems)))
            ;; get binding.
            (flet ((bind ()
                     (irc-branch-to-and-begin-block
                      (irc-basic-block-create
                       (core:fmt nil "{}-start" (symbol-name operator-symbol))))
                     (irc-make-value-frame-set-parent
                      new-env number-of-lexical-vars env)
                     ;; Save all special variables
                     (do* ((cur-req (cdr reqvars) (cdr cur-req))
                           (target (car cur-req) (car cur-req))
                           (dynenv-mems dynenv-mems)
                           (dynenv-cons-mems dynenv-cons-mems))
                          ((endp cur-req) nil)
                       (when (eq (car target) 'ext:special-var)
                         (compile-save-special new-env target (pop dynenv-mems)
                                               (pop dynenv-cons-mems))))
                     (if (eq operator-symbol 'let)
                         (codegen-fill-let-environment
                          new-env reqvars expressions env evaluate-env)
                         (codegen-fill-let*-environment
                          new-env reqvars expressions env evaluate-env))
                     (cmp-log "About to evaluate codegen-progn%N")
                     (codegen-progn result code new-env)))
              (if dynenv-mems ; we are binding specials
                  (let ((old-de-stack (irc-intrinsic "cc_get_dynenv_stack")))
                    (with-try "TRY.let/let*"
                      (bind)
                      ((cleanup)
                       (irc-unwind-environment new-env)
                       (irc-intrinsic "cc_set_dynenv_stack" old-de-stack))))
                  (bind))))))))
  (cmp-log "Done codegen-let/let*%N"))

(defun codegen-let (result rest env)
  (codegen-let/let* 'let result rest env))

(defun codegen-let* (result rest env)
  (codegen-let/let* 'let* result rest env))

;;; If

;;; see typeq.lisp for continuation
(defun compile-typeq-condition (cond env thenb elseb)
  (let ((object (second cond))
        (type (third cond)))
    (when (cdddr cond)
      (format t "compile-typeq-condition (cdddr cond) = ~a~%" (cdddr cond))
      (compiler-error nil "too many arguments for typeq"))
    (let ((value (alloca-t* "if-typeq-tsp")))
      (codegen value object env)
      (let ((object-raw (irc-load value)))
        (case type
          ((fixnum) (compile-tag-check object-raw +fixnum-mask+ +fixnum00-tag+ thenb elseb))
          ((cons) (compile-tag-check object-raw +immediate-mask+ +cons-tag+ thenb elseb))
          ((character) (compile-tag-check object-raw +immediate-mask+ +character-tag+ thenb elseb))
          ((single-float) (compile-tag-check object-raw +immediate-mask+ +single-float-tag+ thenb elseb))
          ((core:general) (compile-tag-check object-raw +immediate-mask+ +general-tag+ thenb elseb))
          (t
           (let ((header-value-min-max (gethash type core:+type-header-value-map+)))
             (when (null header-value-min-max)
               (format t "typeq type = ~a~%" type)
               (compiler-error nil "unknown type for typeq: {}" type))
             (compile-header-check header-value-min-max object-raw thenb elseb))))))))

(defmacro define-tag-check (name mask tag)
  `(defun ,name (cond env thenb elseb)
     (let ((object (second cond))
           (value (alloca-t* "if-tag-tsp")))
       (codegen value object env)
       (compile-tag-check (irc-load value) ,mask ,tag thenb elseb))))

(define-tag-check compile-fixnump +fixnum-mask+ +fixnum00-tag+)
(define-tag-check compile-consp +immediate-mask+ +cons-tag+)
(define-tag-check compile-characterp +immediate-mask+ +character-tag+)
(define-tag-check compile-single-float-p +immediate-mask+ +single-float-tag+)
(define-tag-check compile-generalp +immediate-mask+ +general-tag+)

(defmacro define-fixnum-cmp (name operator)
  `(defun ,name (cond env thenb elseb)
     (let ((n1form (second cond)) (n2form (third cond))
           (n1 (alloca-t* "fixnum-cmp-1"))
           (n2 (alloca-t* "fixnum-cmp-2")))
       (codegen n1 n1form env)
       (codegen n2 n2form env)
       (irc-cond-br
        (,operator (irc-load n1) (irc-load n2) "fixnum-cmp")
        thenb elseb))))

(define-fixnum-cmp compile-fixnum-less-condition irc-icmp-slt)
(define-fixnum-cmp compile-fixnum-lte-condition irc-icmp-sle)
(define-fixnum-cmp compile-fixnum-equal-condition irc-icmp-eq)

;;; this is exactly the same as fixnum-equal, but has different implications,
;;; so it's separate.
(defun compile-eq-condition (cond env thenb elseb)
  (let ((n1form (second cond)) (n2form (third cond))
        (n1 (alloca-t* "eq-1"))
        (n2 (alloca-t* "eq-2")))
    (codegen n1 n1form env)
    (codegen n2 n2form env)
    (irc-cond-br
     (irc-icmp-eq (irc-load n1) (irc-load n2) "eq")
     thenb elseb)))

(defun compile-general-condition (cond env thenb elseb)
  "Generate code for cond that branches to one of the provided successor blocks"
  (let ((test-temp-store (alloca-t* "if-cond-tsp")))
    (codegen test-temp-store cond env)
    (irc-cond-br
     (irc-icmp-eq (irc-load test-temp-store) (irc-nil))
     elseb thenb)))

(defun compile-if-cond (cond env thenb elseb)
  (if (consp cond)
      (case (first cond)
        ((cleavir-primop:typeq)
         (compile-typeq-condition cond env thenb elseb))
        ((cleavir-primop:fixnum-less)
         (compile-fixnum-less-condition cond env thenb elseb))
        ((cleavir-primop:fixnum-not-greater)
         (compile-fixnum-lte-condition cond env thenb elseb))
        ((cleavir-primop:fixnum-equal)
         (compile-fixnum-equal-condition cond env thenb elseb))
        ((cleavir-primop:eq)
         (compile-eq-condition cond env thenb elseb))
        ((core:fixnump) (compile-fixnump cond env thenb elseb))
        ((consp) (compile-consp cond env thenb elseb))
        ((characterp) (compile-characterp cond env thenb elseb))
        ((core:single-float-p) (compile-single-float-p cond env thenb elseb))
        ((core:generalp) (compile-generalp cond env thenb elseb))
        (otherwise (compile-general-condition cond env thenb elseb)))
      (compile-general-condition cond env thenb elseb)))

;;; TAGBODY, GO

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
                    (setq result (cons (list index (irc-basic-block-create (core:fmt nil "tagbody-{}-{}" (car x) index)) x) result))
                    (add-tag tagbody-env (car x) result)
                    (setq index (+ 1 index)))))
          code)
    (nreverse result)))

(defun codegen-tagbody (result rest env)
  "Extract tags and code from (rest) and create an alist that maps
tag symbols to code and llvm-ir basic-blocks. Store the alist in the symbol-to-block-alist
metadata of the tagbody-env. These can be accessed by (go XXXX) special operators to
jump to blocks within this tagbody."
  (assert-result-isa-llvm-value result)
  ;; stick a dummy tag at the head if there isn't one
  (unless (and (car rest) (symbolp (car rest)))
    (push (gensym "PREFIX") rest))
  (let* ((tagbody-env (irc-new-tagbody-environment env))
         (jmp-buf* (alloca %jmp-buf-tag% 1 "tagbody-jmp-buf"))
	 (enumerated-tag-blocks (tagbody.enumerate-tag-blocks rest tagbody-env))
         ;; If the GO spec.ops. are in the same function we could
         ;; use simple cleanup and branches for TAGBODY/GO
         ;; so save the function
         (renv (irc-load (irc-renv env)))
         (instruction (irc-intrinsic "makeTagbodyFrameSetParent" renv))
         (go-blocks (mapcar #'cadr enumerated-tag-blocks))
         (go-vec (make-array (length go-blocks)
                             :initial-contents go-blocks)))
    (irc-t*-result instruction (irc-renv tagbody-env))
    (irc-low-level-trace :tagbody)
    (cmp-log "codegen-tagbody tagbody environment: {}%N" tagbody-env)
    (core:setf-local-blocks tagbody-env go-vec)
    (let* ((tagbody-renv (irc-renv tagbody-env))
           ;; We cannot use llvm.frameaddress to get a handle, like we do in
           ;; cclasp, because here each tagbody and block has its own switch
           ;; encoding and they all share the frameaddress.
           (vhandle (irc-bit-cast tagbody-renv %i8*%))
           (old-de-stack (irc-intrinsic "cc_get_dynenv_stack"))
           (dyncons-space (alloca-i8 +cons-size+
                                     :alignment +alignment+ :label "block-de-cons"))
           (dynenv (irc-intrinsic "cc_createAndPushTagbodyDynenv"
                                  dyncons-space vhandle jmp-buf*))
           (new-de-stack (irc-intrinsic "cc_get_dynenv_stack"))
           (handle (irc-intrinsic "initializeTagbodyClosure" tagbody-renv dynenv))
           (default-block (irc-basic-block-create "tagbody-default"))
           ;; This block is jumped to both by the setjmp and by the
           ;; code in the landing pad.
           (switch-block (irc-basic-block-create "tagbody-switch"))
           ;; This block is necessary because WITH-TRY generates a
           ;; jump to a new block, and so must be called while we
           ;; have a valid and yet-to-be-terminated block.
           (main-block (irc-basic-block-create "tagbody-start"))
           (before-block (irc-get-insert-block))
           (sj (irc-intrinsic "_setjmp" jmp-buf*))
           (_0 (irc-branch-to-and-begin-block switch-block))
           (switch-phi (irc-phi %i32% 2 "tag"))
           (sw (irc-switch switch-phi default-block
                           (length enumerated-tag-blocks))))
      (declare (ignore _0))
      #+optimize-bclasp
      (setf (gethash tagbody-env *tagbody-frame-info*)
            (make-tagbody-frame-info :tagbody-environment tagbody-env
                                     :make-tagbody-frame-instruction instruction
                                     :initialize-tagbody-closure handle))
      (irc-phi-add-incoming switch-phi sj before-block)
      (irc-add-case sw (jit-constant-i32 0) main-block)
      (irc-begin-block main-block)
      (with-try "TRY.tagbody"
        (mapl #'(lambda (cur)
                  (let* ((tag-begin (car cur))
                         (tag-end (cadr cur))
                         (index (car tag-begin))
                         (section-block (cadr tag-begin))
                         (section-next-block (cadr tag-end))
                         (section (extract-section (caddr tag-begin) (caddr tag-end))))
                    (irc-branch-if-no-terminator-inst section-block)
                    ;; 1+ to deal with setjmp only returning 0 the
                    ;; first time.
                    (irc-add-case sw (jit-constant-i32 (1+ index))
                                  section-block)
                    (irc-begin-block section-block)
                    (codegen-progn result section tagbody-env)
                    (when section-next-block
                      (irc-branch-if-no-terminator-inst section-next-block))))
              enumerated-tag-blocks)
        ((cleanup)
         (irc-intrinsic "cc_set_dynenv_stack" new-de-stack)
         (irc-unwind-environment tagbody-env))
        ((typeid-core-unwind exception-ptr)
         (let ((go-index (irc-intrinsic
                          "tagbodyHandleDynamicGoIndex_or_rethrow"
                          exception-ptr vhandle))
               (cur-block (irc-get-insert-block)))
           (irc-phi-add-incoming switch-phi go-index cur-block)
           ;; End the catch and jump back into the main code
           (cmp:with-landing-pad nil
             (irc-intrinsic "__cxa_end_catch"))
           ;; Go
           (irc-br switch-block)
           ;; We generate the default-block here for a cheap reason:
           ;; WITH-TRY expects to be able to generate a call to
           ;; __cxa_catch, even though we don't actually need to
           ;; here. KLUDGE.
           (irc-begin-block default-block)
           (irc-intrinsic "throwIllegalSwitchValue"
                          switch-phi (jit-constant-size_t
                                      (length enumerated-tag-blocks))))))
      ;; We're finally out of the tagbody, so remove it from the
      ;; dynamic environment.
      (irc-intrinsic "cc_set_dynenv_stack" old-de-stack)
      (codegen-literal result nil env))))

(defun codegen-local-tagbody (result rest env)
  "For local go's only. Extract tags and code from (rest) and create an alist that maps
tag symbols to code and llvm-ir basic-blocks. Store the alist in the symbol-to-block-alist
metadata of the tagbody-env. These can be accessed by (go XXXX) special operators to
jump to blocks within this tagbody."
  (assert-result-isa-llvm-value result)
  (unless (and (car rest) (symbolp (car rest))) (push (gensym) rest)) ;; stick a dummy tag at the head if there isn't one
  (let ((tagbody-env (irc-new-tagbody-environment env)))
    (core:set-invisible tagbody-env t) ; Local tagbody-env doesn't generate a renv so it's 'invisible' 
    (let ((enumerated-tag-blocks (tagbody.enumerate-tag-blocks rest tagbody-env)))
      (irc-low-level-trace :tagbody)
      (cmp-log "codegen-tagbody tagbody environment: {}%N" tagbody-env)
      (let ((go-blocks nil))
        (mapc #'(lambda (tag-begin)
                  (let ((section-block (cadr tag-begin)))
                    (push section-block go-blocks)))
              enumerated-tag-blocks)
        (let ((go-vec (make-array (length go-blocks) :initial-contents (nreverse go-blocks))))
          (core:setf-local-blocks tagbody-env go-vec)))
      (mapl #'(lambda (cur)
                (let* ((tag-begin (car cur))
                       (tag-end (cadr cur))
                       (section-block (cadr tag-begin))
                       (section-next-block (cadr tag-end))
                       (section (extract-section (caddr tag-begin) (caddr tag-end))))
                  (irc-branch-if-no-terminator-inst section-block)
                  (irc-begin-block section-block)
                  (codegen-progn result section tagbody-env)
                  (when section-next-block (irc-branch-if-no-terminator-inst section-next-block))))
            enumerated-tag-blocks)
      (codegen-literal result nil env))))

(defun codegen-go (result rest env)
  (declare (ignore result))
  (let* ((tag (car rest))
	 (classified-tag (classify-tag env tag)))
    (cond
      ((and classified-tag (eq (car classified-tag) 'dynamic-go))
       (let ((depth (cadr classified-tag))
	     (index (caddr classified-tag))
             (tagbody-env (cadddr classified-tag))
             (start-renv (irc-load (irc-renv env))))
         #+optimize-bclasp
         (let ((frame-info (gethash tagbody-env *tagbody-frame-info*)))
           (unless frame-info (error "Could not find frame-info for tagbody-env ~s" tagbody-env))
           (setf (tagbody-frame-info-needed frame-info) t))
	 (irc-low-level-trace :go)
	 (let ((instruction (irc-intrinsic "throwDynamicGo"
                                           (jit-constant-size_t depth)
                                           (jit-constant-size_t index)
                                           start-renv)))
           #+optimize-bclasp
           (push (make-throw-dynamic-go :instruction instruction
                                        :depth depth
                                        :index index
                                        :start-env env
                                        :start-renv start-renv
                                        :tagbody-env tagbody-env)
                 *throw-dynamic-go-instructions*)
           instruction)))
      ((and classified-tag (eq (car classified-tag) 'local-go))
       (let ((index (caddr classified-tag))
	     (tagbody-env (cadddr classified-tag)))
	 (cmp-log "Target tagbody environment: {}  tag: {}%N" tagbody-env tag)
	 (let* ((go-vec (core:local-blocks tagbody-env))
		(go-block (elt go-vec index)))
	   (irc-unwind-into-environment env tagbody-env)
	   (irc-br go-block "go-block")
	   (irc-begin-block (irc-basic-block-create "after-go")))))
      (t (error "go to unknown classified tag ~a ~a" tag classified-tag)))))

;;; BLOCK, RETURN-FROM

(defun codegen-block (result rest env)
  "codegen-block using the try macro"
  (let* ((block-symbol (car rest))
         (body (cdr rest)))
    (or (symbolp block-symbol) (error "The block name ~a is not a symbol" block-symbol))
    (with-dbg-lexical-block ()
      (multiple-value-bind (block-env make-block-frame-instruction)
          (irc-make-block-environment-set-parent block-symbol env)
	(let* ((jmp-buf* (alloca %jmp-buf-tag% 1 "block-jmp-buf"))
               (block-renv (irc-renv block-env))
               (vhandle (irc-bit-cast block-renv %i8*%))
               (old-de-stack (irc-intrinsic "cc_get_dynenv_stack"))
               (dynenv-cons-mem (alloca-i8 +cons-size+ :alignment +alignment+
                                           :label "dynenv-cons-mem"))
               (dynenv (irc-intrinsic "cc_createAndPushBlockDynenv"
                                      dynenv-cons-mem vhandle jmp-buf*))
               (handle (irc-intrinsic "initializeBlockClosure" block-renv dynenv))
               (sj (irc-intrinsic "_setjmp" jmp-buf*))
               (block-start (irc-basic-block-create
			     (core:fmt nil "block-{}-start" (symbol-name block-symbol))))
	       (nonlocal-return-block (irc-basic-block-create (core:fmt nil "nonlocal-return-{}-block" (symbol-name block-symbol))))
	       (local-return-block (irc-basic-block-create (core:fmt nil "local-return-{}-block" (symbol-name block-symbol))))
	       (after-return-block (irc-basic-block-create (core:fmt nil "after-return-{}-block" (symbol-name block-symbol)))))
	  (core:setf-local-return-block block-env local-return-block)
          (core:setf-local-return-value block-env result)
          (irc-cond-br (irc-icmp-eq sj (jit-constant-i32 0))
                       block-start nonlocal-return-block)
	  (irc-begin-block block-start)
          #+optimize-bclasp
          (let ((info (make-block-frame-info :block-environment block-env
                                             :block-symbol block-symbol
                                             :make-block-frame-instruction make-block-frame-instruction
                                             :initialize-block-closure-instruction handle)))
            (setf (gethash block-env *block-frame-info*) info))
          (with-try "TRY.block"
            (codegen-progn result body block-env)
            ((cleanup)
             (irc-intrinsic "cc_set_dynenv_stack" old-de-stack)
             (irc-unwind-environment block-env))
            ((typeid-core-unwind exception-ptr)
             (let ((handle-instruction (irc-intrinsic "blockHandleReturnFrom_or_rethrow" exception-ptr vhandle)))
               (irc-tmv-result handle-instruction result))))
          (irc-br after-return-block "after-return-block")
          (irc-begin-block nonlocal-return-block)
          (let ((val (irc-intrinsic "restoreFromMultipleValue0")))
            (irc-tmv-result val result))
          (irc-br after-return-block)
          (irc-begin-block local-return-block)
          (irc-br after-return-block)
          (irc-begin-block after-return-block)
          (irc-intrinsic "cc_set_dynenv_stack" old-de-stack))))))

(defun codegen-local-block (result rest env)
  "codegen-local-block for local return-froms only"
  (let* ((block-symbol (car rest))
         (body (cdr rest)))
    (or (symbolp block-symbol) (error "The block name ~a is not a symbol" block-symbol))
    (with-dbg-lexical-block ()
      (multiple-value-bind (block-env)
          (irc-make-local-block-environment-set-parent block-symbol env)
	(let ((block-start (irc-basic-block-create
			    (core:fmt nil "block-{}-start" (symbol-name block-symbol))))
	      (local-return-block (irc-basic-block-create (core:fmt nil "local-return-{}-block" (symbol-name block-symbol))))
	      (after-return-block (irc-basic-block-create (core:fmt nil "after-return-{}-block" (symbol-name block-symbol)))))
	  (core:setf-local-return-block block-env local-return-block)
          (core:setf-local-return-value block-env result)
	  (irc-br block-start "block-start")
	  (irc-begin-block block-start)
          (codegen-progn result body block-env)
          (irc-br after-return-block "after-return-block")
          (irc-begin-block local-return-block)
          (irc-br after-return-block)
          (irc-begin-block after-return-block))))))

(defun codegen-return-from (result rest env)
  (declare (ignore result))
  (let* ((temp-mv-result (alloca-tmv "temp-mv-result"))
	 (block-symbol (car rest))
	 (return-form (cadr rest)))
    (multiple-value-bind (recognizes-block-symbol inter-function block-env)
	(classify-return-from-symbol env block-symbol)
      (if recognizes-block-symbol
          (if inter-function
              (let ((depth (core:calculate-runtime-visible-environment-depth env block-env))
                    (frame-info (gethash block-env *block-frame-info*)))
                (unless frame-info (error "Could not find frame-info for block ~s" block-symbol))
                (setf (block-frame-info-needed frame-info) t) ; mark the block closure as needed since we have a return-from in an inner function
                (codegen temp-mv-result return-form env)
                (irc-intrinsic "saveToMultipleValue0" temp-mv-result)
                (irc-low-level-trace)
                (let* ((return-from-call (irc-intrinsic "throwReturnFrom" (jit-constant-size_t depth) (irc-load (irc-renv env))))
                       (start-renv (irc-load (irc-renv env))))
                  #+optimize-bclasp
                  (push (make-throw-return-from :instruction return-from-call
                                                :depth depth
                                                :start-env env
                                                :start-renv start-renv
                                                :block-env block-env
                                                :block-symbol block-symbol)
                        *throw-return-from-instructions*)))
              (let ((local-return-block (core:local-return-block block-env))
                    (local-return-value (core:local-return-value block-env)))
                (codegen local-return-value return-form env)
                (irc-unwind-into-environment env block-env)
                #+(or)(let ((saved-values (irc-intrinsic "saveValues" temp-mv-result)))
                  (irc-unwind-into-environment env block-env)
                  (irc-intrinsic "loadValues" temp-mv-result saved-values))
                ;;(irc-intrinsic "saveToMultipleValue0" temp-mv-result)
                (irc-br local-return-block "local-return-block")
                (irc-begin-block (irc-basic-block-create "after-return-from"))))
          (error "Unrecognized block symbol ~a" block-symbol)))))

;;; FLET, LABELS3

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
  (declare (ignore operator-symbol))
  (let ((result-af (irc-renv function-env)))
    (let* ((parent-renv (irc-load (irc-renv parent-env)))
           (val (irc-intrinsic "makeFunctionFrame"
                              (jit-constant-i32 (length functions))
                              parent-renv)))
      (irc-t*-result val result-af))
    ;;    )
    (cmp-log "About to generate code for args%N")
    (do* ((cur functions (cdr cur)))
	 ((endp cur) nil)
      (let* ((fn (car cur))
	     (fn-name (car fn))
	     #+(or)(fn-lambda `(ext::lambda-block ,fn-name ,@(cdr fn)))
	     (fn-lambda-list (cadr fn))
	     (fn-raw-body (cddr fn))
	     (fn-lambda (generate-lambda-block fn-name fn-lambda-list fn-raw-body))
	     (fn-classified (function-info function-env fn-name))
	     (fn-index (or (cadddr fn-classified) (error "Could not find lexical function ~a" fn-name)))
	     (target (irc-intrinsic "functionFrameReference" (irc-load result-af) (jit-constant-i32 fn-index) )))
	(codegen-closure target fn-lambda closure-env)))))

(defun codegen-flet/labels (operator-symbol result rest env)
  (with-dbg-lexical-block ()
    (let* ((functions (car rest))
	   (body (cdr rest))
	   (function-env (irc-new-function-value-environment env :functions functions)))
      (multiple-value-bind (declares code)
	  (process-declarations body nil) ;; don't expect docstring
	(let ((evaluate-env (cond
			      ((eq operator-symbol 'flet) env)
			      ((eq operator-symbol 'labels) function-env)
			      (t (error "flet/labels doesn't understand operator symbol[~a]" operator-symbol))))
              (*notinlines* (new-notinlines declares)))
	  (irc-branch-to-and-begin-block (irc-basic-block-create
					  (core:fmt nil "{}-start"
						   (symbol-name operator-symbol))))
	  (codegen-fill-function-frame operator-symbol function-env functions env evaluate-env)
	  (codegen-progn result code function-env))))))

(defun codegen-flet (result rest env)
  (codegen-flet/labels 'flet result rest env))

(defun codegen-labels (result rest env)
  (codegen-flet/labels 'labels result rest env))

;;; MACROLET

(defun augment-environment-with-declares (env declares)
  (let (specials)
    (mapc (lambda (decl)
            (when (eq (car decl) 'cl:special)
              (dolist (s (cdr decl))
                (setq specials (cons s specials))))) declares)
    (if specials
        (make-value-environment-for-locally-special-entries (nreverse specials) env)
        env)))

(defun codegen-macrolet (result rest env)
  (let* ((macros (car rest))
	 (body (cdr rest))
	 (macro-env (irc-new-macrolet-environment env)))
    (mapc #'(lambda (macro-def &aux (name (car macro-def))
				 (vl (cadr macro-def))
				 (macro-body (cddr macro-def)))
	      (let* ((lambdablock (ext:parse-macro name vl macro-body))
		     (macro-fn (eval (list 'function lambdablock))))
;;;		(core:set-kind macro-fn :macro)
		(add-macro macro-env name macro-fn)))
	  macros )
    (multiple-value-bind (declares code)
	(process-declarations body t)
      (augment-environment-with-declares macro-env declares)
      (let ((*notinlines* (new-notinlines declares)))
        (codegen-progn result code macro-env)))))

;;; SYMBOL-MACROLET

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

;;; EVAL-WHEN

(defun codegen-eval-when (result rest env)
;;  (break "codegen-eval-when")
  (let ((situations (car rest))
	(body (cdr rest)))
    (if (or (member 'cl:eval situations) (member :execute situations))
	(codegen-progn result body env)
	(codegen-literal result nil env))))

;;; THE

(defun codegen-the (result rest env)
  (codegen result (cadr rest) env))

;;; LOCALLY

(defun codegen-locally (result rest env)
  (with-dbg-lexical-block ()
    (multiple-value-bind (declarations code doc-string specials)
	(process-declarations rest nil)
      (declare (ignore doc-string))
      (let ((new-env (irc-new-unbound-value-environment-of-size
		      env
		      :number-of-arguments (length specials)
		      :label "locally-env"))
            (*notinlines* (new-notinlines declarations)))
	;; TODO: A runtime environment will be created with space for the specials
	;; but they aren't used - get rid of them
        (irc-make-value-frame-set-parent new-env 0 env) ; (irc-intrinsic "setParentOfActivationFrame" (irc-renv new-env) (irc-renv env))
	(dolist (sp specials)
	  (value-environment-define-special-binding new-env sp))
	(codegen-progn result code new-env)
	))))

;;; QUOTE

(defun codegen-quote (result rest env)
  (codegen-literal result (car rest) env))

;;; LOAD-TIME-VALUE

(defun codegen-load-time-value (result rest env)
  (declare (ignore env))
  (cmp-log "Starting codegen-load-time-value rest: {}%N" rest)
  (let* ((form (car rest))
	 (read-only-p (cadr rest)))
    (declare (ignore read-only-p))
;;; Currently if read-only-p is T there is no
;;; coalescence performed - this could be added as an optimization
    (if *generate-compile-file-load-time-values*
        (let ((index (literal:load-time-value-from-thunk (literal:compile-load-time-value-thunk form))))
          (irc-t*-result (literal:constants-table-value index) result))
        (let ((ltv (eval form)))
          (literal:codegen-rtv-bclasp result ltv)))))

;;; CORE::VECTOR-LENGTH

(defun gen-vector-length (vector)
  (let* (;; Since we're not touching the data, the element type is irrelevant.
         (type (llvm-sys:type-get-pointer-to (simple-vector-llvm-type 't)))
         (cast (irc-bit-cast vector type)) ; treat the vector as a vector
         ;; find the location of the length
         (length-address
           (irc-gep-variable cast (list (jit-constant-i32 0)
                                        (jit-constant-i32 +simple-vector-length-slot+))
                             "vector-length-address"))
         (untagged-length (irc-load length-address "vector-length")))
    (irc-tag-fixnum untagged-length "vector-length")))

(defun codegen-vector-length (result rest env)
  (let ((form (car rest))
        (vector (alloca-t* "vector-alloca")))
    (codegen vector form env)
    (irc-t*-result (gen-vector-length (irc-load vector "vector")) result)))

;;; CORE::%ARRAY-DIMENSION

(defun gen-%array-dimension (array axisn)
  (let* ((untagged-axisn (irc-untag-fixnum axisn %i64% "untagged-axisn"))
         (untagged-dim (irc-array-dimension array untagged-axisn)))
    (irc-tag-fixnum untagged-dim "array-dimension")))

(defun codegen-%array-dimension (result rest env)
  (let ((array-form (first rest))
        (array-alloca (alloca-t* "array-alloca"))
        (axis-form (second rest))
        (axis-alloca (alloca-t* "axis-alloca")))
    (codegen array-alloca array-form env)
    (codegen axis-alloca axis-form env)
    (irc-t*-result (gen-%array-dimension (irc-load array-alloca) (irc-load axis-alloca))
                   result)))

;;; FENCE

(defun order-spec->order (order-spec)
  (case order-spec
    ((:unordered) 'llvm-sys:unordered)
    ((:relaxed) 'llvm-sys:monotonic)
    ((:acquire) 'llvm-sys:acquire)
    ((:release) 'llvm-sys:release)
    ((:acquire-release) 'llvm-sys:acquire-release)
    ((:sequentially-consistent) 'llvm-sys:sequentially-consistent)
    (t (error "BUG: Unknown atomic order specifier ~a" order-spec))))

(defun gen-fence (order-spec)
  (when (or (eq order-spec :unordered) (eq order-spec :relaxed))
    (error "Can't generate a fence with ~a ordering" order-spec))
  (irc-fence (order-spec->order order-spec)))

(defun codegen-fence (result rest env)
  (declare (ignore result env))
  (gen-fence (first rest)))

;;; CLEAVIR-PRIMOP:CAR, CLEAVIR-PRIMOP:CDR

(defun gen-memref-address (tpointer offset)
  (irc-bit-cast
   ;; memref/set use byte addressing, so treat these as i8 arrays
   (irc-gep-variable (irc-bit-cast tpointer %i8*%)
                     ;; llvm doesn't actually have signed types,
                     ;; so the u is a misnomer - don't sweat it.
                     (list (cmp:make-uintptr_t offset)))
   %t**% "memref-set-addr"))

(defun codegen-car (result rest env)
  (let ((cons-form (first rest))
        (cons-alloca (alloca-t* "cons")))
    (codegen cons-alloca cons-form env)
    (irc-t*-result (irc-load-atomic (gen-memref-address
                                     (irc-load cons-alloca)
                                     (- +cons-car-offset+ +cons-tag+)))
                   result)))

(defun codegen-cdr (result rest env)
  (let ((cons-form (first rest))
        (cons-alloca (alloca-t* "cons")))
    (codegen cons-alloca cons-form env)
    (irc-t*-result (irc-load-atomic (gen-memref-address
                                     (irc-load cons-alloca)
                                     (- +cons-cdr-offset+ +cons-tag+)))
                   result)))

;;; ATOMIC CAR, CDR, RPLACA, RPLACD, plus CAS

(defun codegen-atomic-car (result rest env)
  (let ((order (order-spec->order (first rest)))
        (cons-form (second rest))
        (cons-alloca (alloca-t* "cons")))
    (codegen cons-alloca cons-form env)
    (irc-t*-result (irc-load-atomic (gen-memref-address
                                     (irc-load cons-alloca)
                                     (- +cons-car-offset+ +cons-tag+))
                                    :order order)
                   result)))

(defun codegen-atomic-cdr (result rest env)
  (let ((order (order-spec->order (first rest)))
        (cons-form (second rest))
        (cons-alloca (alloca-t* "cons")))
    (codegen cons-alloca cons-form env)
    (irc-t*-result (irc-load-atomic (gen-memref-address
                                     (irc-load cons-alloca)
                                     (- +cons-cdr-offset+ +cons-tag+))
                                    :order order)
                   result)))

(defun codegen-atomic-rplaca (result rest env)
  (let ((order (order-spec->order (first rest)))
        (nv-form (second rest))
        (cons-form (third rest))
        (cons-alloca (alloca-t* "cons"))
        (nv-alloca (alloca-t* "nv")))
    (codegen nv-alloca nv-form env)
    (codegen cons-alloca cons-form env)
    (let ((nv (irc-load nv-alloca)))
      (irc-store-atomic nv (gen-memref-address
                            (irc-load cons-alloca)
                            (- +cons-car-offset+ +cons-tag+))
                        :order order)
      (irc-t*-result nv result))))

(defun codegen-atomic-rplacd (result rest env)
  (let ((order (order-spec->order (first rest)))
        (nv-form (second rest))
        (cons-form (third rest))
        (cons-alloca (alloca-t* "cons"))
        (nv-alloca (alloca-t* "nv")))
    (codegen nv-alloca nv-form env)
    (codegen cons-alloca cons-form env)
    (let ((nv (irc-load nv-alloca)))
      (irc-store-atomic nv (gen-memref-address
                            (irc-load cons-alloca)
                            (- +cons-cdr-offset+ +cons-tag+))
                        :order order)
      (irc-t*-result nv result))))

(defun codegen-cas-car (result rest env)
  (let ((order (order-spec->order (first rest)))
        (cmp-form (second rest)) (nv-form (third rest))
        (cons-form (fourth rest))
        (cmp-alloca (alloca-t* "cmp")) (nv-alloca (alloca-t* "nv"))
        (cons-alloca (alloca-t* "cons")))
    (codegen cmp-alloca cmp-form env)
    (codegen nv-alloca nv-form env)
    (codegen cons-alloca cons-form env)
    (irc-t*-result
     (irc-cmpxchg (gen-memref-address (irc-load cons-alloca)
                                      (- +cons-car-offset+ +cons-tag+))
                  (irc-load cmp-alloca) (irc-load nv-alloca)
                  :order order)
     result)))

(defun codegen-cas-cdr (result rest env)
  (let ((order (order-spec->order (first rest)))
        (cmp-form (second rest)) (nv-form (third rest))
        (cons-form (fourth rest))
        (cmp-alloca (alloca-t* "cmp")) (nv-alloca (alloca-t* "nv"))
        (cons-alloca (alloca-t* "cons")))
    (codegen cmp-alloca cmp-form env)
    (codegen nv-alloca nv-form env)
    (codegen cons-alloca cons-form env)
    (irc-t*-result
     (irc-cmpxchg (gen-memref-address (irc-load cons-alloca)
                                      (- +cons-cdr-offset+ +cons-tag+))
                  (irc-load cmp-alloca) (irc-load nv-alloca)
                  :order order)
     result)))

;;; CLEAVIR-PRIMOP:FUNCALL

(defun codegen-primop-funcall (result rest env)
  (let ((func (first rest))
        (funcy (alloca-t* "function"))
        (args (rest rest)))
    (codegen funcy func env)
    (codegen-call result (irc-load funcy) args env)))

;;; CLEAVIR-PRIMOP:UNREACHABLE

(defun codegen-unreachable (result rest env)
  (declare (ignore result rest env))
  (irc-unreachable)
  ;; This is necessary if we keep generating more instructions.
  ;; I don't think we actually do - and clasp compiles without this-
  ;; but if we were to, the llvm error would be kind of hard to understand,
  ;; so I'm leaving this in.
  (irc-begin-block (irc-basic-block-create "unreachable")))

;;; CORE:VASLIST-LENGTH
;;; Get the count of remaining args in a vaslist.

(defun gen-vaslist-length (vaslist)
  (irc-tag-fixnum
   (irc-load
    (irc-vaslist-nargs-address vaslist))))

(defun codegen-vaslist-length (result rest env)
  (let ((form (car rest))
        (vaslist (alloca-t* "vaslist-length-vaslist")))
    (codegen vaslist form env)
    (irc-t*-result (gen-vaslist-length (irc-load vaslist)) result)))

;;; CORE:VASLIST-POP
;;; Remove one item from the vaslist and return it.
;;; Without DEBUG_BUILD, does not actually check if there is an element to pop.
;;; Use caution.

(defun gen-vaslist-pop (vaslist)
  ;; We need to decrement the remaining nargs, then return va_arg.
  (let* ((nargs* (irc-vaslist-nargs-address vaslist))
         (nargs (irc-load nargs*))
         (nargs-- (irc-sub nargs (jit-constant-size_t 1))))
    ;; Decrement.
    (irc-store nargs-- nargs*)
    (let* ((args* (irc-vaslist-args-address vaslist))
           (args (irc-load args*))
           (args++ (irc-gep args (list 1))))
      ;; Increment
      (irc-store args++ args*)
      ;; va_arg.
      (irc-load args))))

(defun codegen-vaslist-pop (result rest env)
  (let ((form (car rest))
        (vaslist (alloca-t* "vaslist-pop-vaslist")))
    (codegen vaslist form env)
    (irc-t*-result (gen-vaslist-pop (irc-load vaslist)) result)))

;;; CLEAVIR-PRIMOP:CASE
;;; CL:CASE when all the keys are immediates. Generated by the compiler macro.
;;; Always has a default. Keys are always lists of objects.

(defun codegen-primop-case (result rest env)
  (let* ((keyform (first rest)) (cases (rest rest))
         (default (rest (first (last cases))))
         (defaultb (irc-basic-block-create "case-default"))
         (mergeb (irc-basic-block-create "case-merge"))
         (main-cases (butlast cases))
         (ncases (let ((sum 0))
                   (dolist (case main-cases sum)
                     (incf sum (length (first case))))))
         (keyt (alloca-t* "case-key")))
    (codegen keyt keyform env)
    (let* ((key64 (irc-ptr-to-int (irc-load keyt) %i64%))
           (sw (irc-switch key64 defaultb ncases)))
      (dolist (case main-cases)
        (let ((keys (first case))
              (body (rest case))
              (block (irc-basic-block-create "case-case")))
          (dolist (key keys)
            (let ((val (core:create-tagged-immediate-value-or-nil key)))
              (irc-add-case sw (jit-constant-i64 val) block)))
          (irc-begin-block block)
          (codegen-progn result body env)
          (irc-branch-if-no-terminator-inst mergeb)))
      (irc-begin-block defaultb)
      (codegen-progn result default env)
      (irc-branch-if-no-terminator-inst mergeb))
    (irc-begin-block mergeb)))

;;; (CORE:HEADER-STAMP-CASE stamp b1 b2 b3 b4)
;;; Branch to one of four places depending on the where tag.

(defun codegen-header-stamp-case (result rest env)
  (let ((stampf (first rest))
        (stampt (alloca-t* "stamp"))
        (derivable (second rest))
        (derivableb (irc-basic-block-create "derivable"))
        (rack (third rest))
        (rackb (irc-basic-block-create "rack"))
        (wrapped (fourth rest))
        (wrappedb (irc-basic-block-create "wrapped"))
        (header (fifth rest))
        (headerb (irc-basic-block-create "header"))
        (defaultb (irc-basic-block-create "impossible-default"))
        (mergeb (irc-basic-block-create "header-stamp-case-after")))
    (codegen stampt stampf env)
    (let* ((stamp-i64 (irc-ptr-to-int (irc-load stampt) %i64%))
           (where (irc-and stamp-i64 (jit-constant-i64 +where-tag-mask+)))
           (sw (irc-switch where defaultb 4)))
      (irc-add-case sw (jit-constant-i64 +derivable-where-tag+) derivableb)
      (irc-add-case sw (jit-constant-i64 +rack-where-tag+) rackb)
      (irc-add-case sw (jit-constant-i64 +wrapped-where-tag+) wrappedb)
      (irc-add-case sw (jit-constant-i64 +header-where-tag+) headerb)
      ;; Now generate all these blocks.
      (irc-begin-block derivableb)
      (codegen result derivable env)
      (irc-branch-if-no-terminator-inst mergeb)
      (irc-begin-block rackb)
      (codegen result rack env)
      (irc-branch-if-no-terminator-inst mergeb)
      (irc-begin-block wrappedb)
      (codegen result wrapped env)
      (irc-branch-if-no-terminator-inst mergeb)
      (irc-begin-block headerb)
      (codegen result header env)
      (irc-branch-if-no-terminator-inst mergeb)
      ;; Generate the default block, which is just unreachable.
      (irc-begin-block defaultb)
      (irc-unreachable)
      ;; Done
      (irc-begin-block mergeb))))

;;; CORE:HEADER-STAMP

(defun codegen-header-stamp (result rest env)
  (let ((form (car rest))
        (object (alloca-t* "read-stamp-obj")))
    (codegen object form env)
    (irc-t*-result (irc-header-stamp (irc-load object)) result)))

;;; CORE:RACK-STAMP

(defun codegen-rack-stamp (result rest env)
  (let ((form (car rest))
        (object (alloca-t* "read-stamp-obj")))
    (codegen object form env)
    (irc-t*-result (irc-rack-stamp (irc-load object)) result)))

;;; CORE:WRAPPED-STAMP

(defun codegen-wrapped-stamp (result rest env)
  (let ((form (car rest))
        (object (alloca-t* "read-stamp-obj")))
    (codegen object form env)
    (irc-t*-result (irc-wrapped-stamp (irc-load object)) result)))

;;; CORE:DERIVABLE-STAMP

(defun codegen-derivable-stamp (result rest env)
  (let ((form (car rest))
        (object (alloca-t* "read-stamp-obj")))
    (codegen object form env)
    (irc-t*-result (irc-derivable-stamp (irc-load object)) result)))

;;; CORE:INSTANCE-REF

(defun gen-instance-ref (instance index)
  (irc-read-slot instance (irc-untag-fixnum index %size_t% "slot-location")))

(defun codegen-instance-ref (result rest env)
  (let ((instance (first rest)) (index (second rest))
        (instancet (alloca-t* "instance-ref-instance"))
        (indext (alloca-t* "instance-ref-index")))
    (codegen instancet instance env)
    (codegen indext index env)
    (irc-t*-result (gen-instance-ref (irc-load instancet) (irc-load indext))
                   result)))

;;; CORE:INSTANCE-SET

(defun gen-instance-set (instance index value)
  (irc-write-slot instance (irc-untag-fixnum index %size_t% "slot-location") value))

(defun codegen-instance-set (result rest env)
  (let ((instance (first rest)) (index (second rest)) (value (third rest))
        (instancet (alloca-t* "instance-set-instance"))
        (indext (alloca-t* "instance-set-index"))
        (valuet (alloca-t* "instance-set-value")))
    (codegen instancet instance env)
    (codegen indext index env)
    (codegen valuet value env)
    (irc-t*-result
     (gen-instance-set (irc-load instancet) (irc-load indext) (irc-load valuet))
     result)))

;;; CORE:INSTANCE-CAS

(defun gen-instance-cas (instance index old new)
  (irc-cmpxchg
   (irc-instance-slot-address
    instance (irc-untag-fixnum index %size_t% "slot-location"))
   old new))

(defun codegen-instance-cas (result rest env)
  (let ((old (first rest)) (new (second rest))
        (instance (third rest)) (index (fourth rest))
        (instancet (alloca-t* "instance")) (indext (alloca-t* "index"))
        (oldt (alloca-t* "old")) (newt (alloca-t* "new")))
    (codegen instancet instance env)
    (codegen indext index env)
    (codegen oldt old env)
    (codegen newt new env)
    (irc-t*-result
     (gen-instance-cas (irc-load instancet) (irc-load indext)
                       (irc-load oldt) (irc-load newt))
     result)))

;;; CORE:INSTANCE-RACK

(defun gen-instance-rack (instance) (irc-rack instance))

(defun codegen-instance-rack (result rest env)
  (let ((instance (first rest))
        (instancet (alloca-t* "instance-rack-instance")))
    (codegen instancet instance env)
    (irc-t*-result (gen-instance-rack (irc-load instancet)) result)))

;;; CORE:INSTANCE-RACK-SET

(defun gen-instance-rack-set (instance rack)
  (irc-rack-set instance rack)
  rack)

(defun codegen-instance-rack-set (result rest env)
  (let ((instance (first rest)) (rack (second rest))
        (instancet (alloca-t* "instance"))
        (rackt (alloca-t* "rack")))
    (codegen instancet instance env)
    (codegen rackt rack env)
    (irc-t*-result
     (gen-instance-rack-set (irc-load instancet) (irc-load rackt))
     result)))

;;; CORE:RACK-REF

(defun gen-rack-ref (rack index &key (order 'llvm-sys:monotonic))
  (irc-rack-read rack (irc-untag-fixnum index %size_t% "slot-location")
                 :order order))

(defun codegen-rack-ref (result rest env)
  (let ((rack (first rest)) (index (second rest))
        (rackt (alloca-t* "rack-ref-rack"))
        (indext (alloca-t* "rack-ref-index")))
    (codegen rackt rack env)
    (codegen indext index env)
    (irc-t*-result (gen-rack-ref (irc-load rackt) (irc-load indext))
                   result)))

;;; CORE:RACK-SET

(defun gen-rack-set (rack index value &key (order 'llvm-sys:monotonic))
  (irc-rack-write rack (irc-untag-fixnum index %size_t% "slot-location") value
                  :order order)
  value)

(defun codegen-rack-set (result rest env)
  (let ((rack (first rest)) (index (second rest)) (value (third rest))
        (rackt (alloca-t* "rack-set-rack"))
        (indext (alloca-t* "rack-set-index"))
        (valuet (alloca-t* "rack-set-value")))
    (codegen rackt rack env)
    (codegen indext index env)
    (codegen valuet value env)
    (irc-t*-result
     (gen-rack-set (irc-load rackt) (irc-load indext) (irc-load valuet))
     result)))

;;; CORE::ATOMIC-RACK-READ, CORE::ATOMIC-RACK-WRITE

(defun codegen-atomic-rack-read (result rest env)
  (let ((order (order-spec->order (first rest)))
        (rack (second rest)) (index (third rest))
        (rackt (alloca-t* "rack-ref-rack"))
        (indext (alloca-t* "rack-ref-index")))
    (codegen rackt rack env)
    (codegen indext index env)
    (irc-t*-result (irc-rack-read
                    (irc-load rackt)
                    (irc-untag-fixnum
                     (irc-load indext) %size_t% "slot-location")
                    :order order)
                   result)))

(defun codegen-atomic-rack-write (result rest env)
  (let ((order (order-spec->order (first rest)))
        (nv (second rest)) (rack (third rest)) (index (fourth rest))
        (nvt (alloca-t* "rack-set-value"))
        (rackt (alloca-t* "rack-set-rack"))
        (indext (alloca-t* "rack-set-index")))
    (codegen nvt nv env)
    (codegen rackt rack env)
    (codegen indext index env)
    (let ((nv (irc-load nvt)))
      (irc-rack-write (irc-load rackt)
                      (irc-untag-fixnum
                       (irc-load indext) %size_t% "slot-location")
                      nv
                      :order order)
      (irc-t*-result nv result))))

(defun codegen-cas-rack (result rest env)
  (let ((order (order-spec->order (first rest)))
        (old (second rest)) (nv (third rest))
        (rack (fourth rest)) (index (fifth rest))
        (oldt (alloca-t* "old")) (newt (alloca-t* "new"))
        (rackt (alloca-t* "rack")) (indext (alloca-t* "index")))
    (codegen oldt old env)
    (codegen newt nv env)
    (codegen rackt rack env)
    (codegen indext index env)
    (irc-t*-result
     (irc-cmpxchg (irc-rack-slot-address (irc-load rackt)
                                         (irc-untag-fixnum
                                          (irc-load indext)
                                          %size_t% "slot-location"))
                  (irc-load oldt) (irc-load newt)
                  :order order)
     result)))

;;; DBG-i32

(defparameter *nexti* 10000)
(defun codegen-dbg-i32 (result rest env)
  (declare (ignore result env))
  (let ((giveni (car rest)))
    (if (null giveni)
	(progn
	  (setq giveni *nexti*)
	  (setq *nexti* (+ 1 *nexti*))))
    (irc-intrinsic "debugPrintI32" (jit-constant-i32 giveni))))

;;; DEBUG-MESSAGE

(defun codegen-debug-message (result rest env)
  (declare (ignore result env))
  (let ((message (jit-constant-unique-string-ptr (car rest))))
    (irc-intrinsic "debugMessage" message)))

(defun codegen-debug-break (result rest env)
  (declare (ignore result rest env))
  (irc-intrinsic "debugBreak"))

;;; LLVM-INLINE

(defun codegen-llvm-inline (result result-env-body compiler-env)
  (destructuring-bind ((result-name env-name) &body body)
      result-env-body
    (eval `(let ((,result-name ,result)
                 (,env-name ,compiler-env))
             ,@body))))

#+(or)
(defmacro blog (fmt &rest fargs)
  `(core:fmt *error-output* ,fmt ,@fargs))
(defmacro blog (fmt &rest fargs)
  (declare (ignore fmt fargs))
  nil)

;;; core:bind-vaslist
(defun codegen-bind-vaslist (result form evaluate-env)
  (let ((lambda-list (first form))
        (vaslist     (second form))
        (body        (cddr form)))
    (blog "evaluate-env -> {}%N" evaluate-env)
    (multiple-value-bind (declares code)
        (process-declarations body t)
      (let ((canonical-declares (core:canonicalize-declarations declares)))
        (multiple-value-bind (cleavir-lambda-list-analysis new-body rest-alloc)
            (transform-lambda-parts lambda-list canonical-declares code)
          (blog "got cleavir-lambda-list-analysis -> {}%N" cleavir-lambda-list-analysis)
          (let ((eval-vaslist (alloca-t* "bind-vaslist")))
            (codegen eval-vaslist vaslist evaluate-env)
            (let* ((lvaslist (irc-load eval-vaslist "lvaslist"))
                   (src-remaining-nargs* (irc-vaslist-nargs-address lvaslist))
                   (src-args* (irc-vaslist-args-address lvaslist))
                   (local-args* (alloca-vaslist :label "local-vaslist"))
                   (_ (vaslist-start local-args* (irc-load src-remaining-nargs*) (irc-load src-args*)))
                   (callconv (make-calling-convention :closure (llvm-sys:constant-pointer-null-get %i8*%)
                                                      :nargs (irc-load src-remaining-nargs*)
                                                      :vaslist* local-args*
                                                      :rest-alloc rest-alloc
                                                      :cleavir-lambda-list-analysis cleavir-lambda-list-analysis)))
              (declare (ignore _))
              ;; See comment in cleavir bind-vaslist w/r/t safep.
              (let ((new-env (bclasp-compile-lambda-list-code evaluate-env callconv :general-entry :safep nil))
                    (*notinlines* (new-notinlines canonical-declares)))
                (codegen-let/let* (car new-body) result (cdr new-body) new-env)))))))))

;;; MULTIPLE-VALUE-FOREIGN-CALL

(defun codegen-multiple-value-foreign-call (result form evaluate-env)
  "Evaluate each of the arguments into an alloca and invoke the function"
  ;; setup the ActivationFrame for passing arguments to this function in the setup arena
  (assert-result-isa-llvm-value result)
  ;;(core:fmt t "In codegen-multiple-value-foreign-call codegen form: {}%N" form)
  (let* ((intrinsic-name (car form))
         args
         (temp-result (alloca-t*)))
    ;; evaluate the arguments into the array
    ;;  used to be done by --->    (codegen-evaluate-arguments (cdr form) evaluate-env)
    (do* ((cur-exp (cdr form) (cdr cur-exp))
          (exp (car cur-exp) (car cur-exp))
          (i 0 (+ 1 i)))
         ((endp cur-exp) nil)
      ;;(core:fmt t "In codegen-multiple-value-foreign-call codegen arg[{}] -> {}%N" i exp)
      (codegen temp-result exp evaluate-env)
      (push (irc-load temp-result) args))
    (let* ((func (or (llvm-sys:get-function *the-module* intrinsic-name)
                     (let ((arg-types (make-list (length args) :initial-element %t*%))
                           (varargs nil))
                       (irc-function-create
                        (llvm-sys:function-type-get %return-type% arg-types varargs)
                        'llvm-sys::External-linkage
                        intrinsic-name
                        *the-module*))))
           (function-type (llvm-sys:get-function-type func))
           (result-in-registers
             (irc-call-or-invoke function-type func (nreverse args))))
      (irc-tmv-result result-in-registers result)))
  (irc-low-level-trace :flow))

;;; FOREIGN-CALL, FOREIGN-CALL-POINTER

(defun function-type-create-on-the-fly (foreign-types)
  (let ((arg-types (mapcar (lambda (type)
                             (clasp-ffi::safe-translator-type type))
                           (second foreign-types)))
        (varargs nil))
    (llvm-sys:function-type-get (clasp-ffi::safe-translator-type (first foreign-types)) arg-types varargs)))

(defun evaluate-foreign-arguments (fargs foreign-types temp-result evaluate-env)
  (let (args)
    (do* ((cur-exp fargs (cdr cur-exp))
          (exp (car cur-exp) (car cur-exp))
          (type-cur (second foreign-types) (cdr type-cur))
          (type (car type-cur) (car type-cur))
          (i 0 (+ 1 i)))
         ((endp cur-exp) nil)
      ;;(core:fmt t "In codegen-multiple-value-foreign-call codegen arg[{}] -> {}%N" i exp)
      (codegen temp-result exp evaluate-env)
      (push (irc-intrinsic-call (clasp-ffi::from-translator-name type)
                             (list (irc-load temp-result))) args))
    args))

(defun codegen-foreign-call (result form evaluate-env)
  "Evaluate each of the arguments into an alloca and invoke the function"
  ;; setup the ActivationFrame for passing arguments to this function in the setup arena
  (assert-result-isa-llvm-value result)
  ;;(core:fmt t "In codegen-multiple-value-foreign-call codegen form: {}%N" form)
  (let* ((foreign-types (first form))
         (intrinsic-name (second form))
         (fargs (cddr form))
         (temp-result (alloca-t*)))
    ;; evaluate the arguments into the array
    ;;  used to be done by --->    (codegen-evaluate-arguments (cddr form) evaluate-env)
    (let* ((args (evaluate-foreign-arguments fargs foreign-types temp-result evaluate-env))
           (function-type (function-type-create-on-the-fly foreign-types))
           (func (or (llvm-sys:get-function *the-module* intrinsic-name)
                     (irc-function-create
                      function-type
                      'llvm-sys::External-linkage
                      intrinsic-name
                      *the-module*)))
           (foreign-result
            (irc-call-or-invoke function-type function-type func (nreverse args)))
           (result-in-t*
            (if (eq :void (first foreign-types))
                (irc-intrinsic-call (clasp-ffi::to-translator-name (first foreign-types)) nil) ; returns :void
                (irc-intrinsic-call (clasp-ffi::to-translator-name (first foreign-types))
                                 (list foreign-result)))))
      (irc-t*-result result-in-t* result)))
  (irc-low-level-trace :flow))

(defun codegen-foreign-call-pointer (result form evaluate-env)
  "Evaluate each of the arguments into an alloca and invoke the function pointer"
  ;; setup the ActivationFrame for passing arguments to this function in the setup arena
  (assert-result-isa-llvm-value result)
  (let* ((foreign-types (first form))
         (func-pointer (second form))
         (fargs (cddr form))
         (temp-result (alloca-t*)))
    ;; evaluate the arguments into the array
    (let ((args (evaluate-foreign-arguments fargs foreign-types temp-result evaluate-env))
          (function-type (function-type-create-on-the-fly foreign-types)))
      ;; evaluate the function pointer
      (codegen temp-result func-pointer evaluate-env)
      (let* ((function-pointer-type (llvm-sys:type-get-pointer-to function-type))
             (pointer-t* (irc-load temp-result))
             (function-pointer (llvm-sys:create-bit-cast *irbuilder* (irc-intrinsic "cc_getPointer" pointer-t*) function-pointer-type "cast-function-pointer"))
             (foreign-result
              (irc-call-or-invoke function-type function-pointer (nreverse args)))
             (result-in-t*
              (if (eq :void (first foreign-types))
                  (irc-intrinsic-call (clasp-ffi::to-translator-name (first foreign-types)) nil) ; returns :void
                  (irc-intrinsic-call (clasp-ffi::to-translator-name (first foreign-types)) (list foreign-result)))))
        (irc-t*-result result-in-t* result)))
    (irc-low-level-trace :flow)))

;;; DEFCALLBACK

;;; shared with cleavir.
;;; What we're doing here is defining a C function
;;; that calls the translators on its arguments, passes those translated arguments
;;; to a Lisp closure, then translates the primary return value of that function
;;; back to C and returns it (or if the C function is return type void, doesn't).
(defun gen-defcallback (c-name convention
                        return-type-name return-translator-name
                        argument-type-names argument-translator-names
                        parameters place-holder closure-value)
  (declare (ignore convention place-holder))         ; FIXME
  ;; parameters should be a list of symbols, i.e. lambda list with only required.
  (unless (= (length argument-type-names) (length parameters) (length argument-translator-names))
    (error "BUG: Callback function parameters and types have a length mismatch"))
;;; Generate a variable and put the closure in it.
  (let* ((closure-literal-slot-index (literal:new-table-index))
         (closure-var-name (core:fmt nil "{}_closure_var" c-name)))
    #+(or)(progn
            (format t "gen-defcallback - the (literal::literal-machine-table-index literal::*literal-machine*) -> ~d~%" (literal::literal-machine-table-index literal::*literal-machine*))
            (format t "gen-defcallback cmp:*load-time-value-holder-global-var* -> ~a~%" cmp:*load-time-value-holder-global-var*)
            (format t "gen-defcallback closure-value -> ~a~%" closure-value)
            (format t "gen-defcallback closure-literal-slot-index -> ~a~%" closure-literal-slot-index))
    (irc-t*-result closure-value (literal:constants-table-reference closure-literal-slot-index))
    ;; Now generate the C function.
    ;; We don't actually "do" anything with it- just leave it there to be linked/used like a C function.
    (with-landing-pad nil ; Since we're in a new function (which should never be an unwind dest)
      (let* ((c-argument-names (mapcar #'string parameters))
             (return-type (clasp-ffi:safe-translator-type return-type-name))
             (argument-types (mapcar #'clasp-ffi:safe-translator-type argument-type-names))
             (c-function-type (llvm-sys:function-type-get return-type argument-types))
             (new-func (llvm-sys:function-create c-function-type
                                                 'llvm-sys:external-linkage
                                                 c-name
                                                 *the-module*))
             (*current-function* new-func)
             (*current-function-name* c-name))
        (unless (llvm-sys:llvmcontext-equal (llvm-sys:get-context *the-module*)
                                            (llvm-sys:get-context new-func))
          (error "The llvm-context for the~%module ~s~%the thread LLVMContext is ~s~% doesn't match the one for the new-func ~s~%the c-function-type context is ~s~% The function return-type context is: ~s~% The argument types are ~s~%"
                 (llvm-sys:get-context *the-module*)
                 (cmp:thread-local-llvm-context)
                 (llvm-sys:get-context new-func)
                 (llvm-sys:get-context c-function-type)
                 (llvm-sys:get-context return-type)
                 (mapcar #'llvm-sys:get-context argument-types)))
        (with-irbuilder ((llvm-sys:make-irbuilder (thread-local-llvm-context)))
          (let ((bb (irc-basic-block-create "entry" new-func)))
            (irc-set-insert-point-basic-block bb)
            (let* ((c-args (mapcar (lambda (arg argname)
                                     (llvm-sys:set-name arg argname)
                                     arg)
                                   (llvm-sys:get-argument-list new-func)
                                   c-argument-names))
                   ;; Generate code to translate the arguments.
                   (cl-args (mapcar (lambda (c-arg c-arg-name translator)
                                      (irc-intrinsic-call
                                       translator
                                       (list c-arg)
                                       (format nil "translated-~a" c-arg-name)))
                                    c-args c-argument-names argument-translator-names))
                   ;; Generate code to get the closure from the global variable from earlier.
                   (closure-to-call (irc-load (literal:constants-table-reference closure-literal-slot-index) closure-var-name))
                   ;; Generate the code to actually call the lisp function.
                   ;; results-in-registers keeps things in the basic tmv format, because
                   ;; here we don't need the store/load values dance.
                   ;; (The C function only gets/needs/wants the primary value.)
                   (cl-result (irc-funcall-results-in-registers
                               closure-to-call cl-args (core:fmt nil "{}_closure" c-name))))
              ;; Now generate a call the translator for the return value if applicable, then return.
              ;; NOTE: (eq return-type %void%) doesn't seem to work - and it's sketchy because it's a symbol macro
              (if (string= return-translator-name "from_object_void")
                  (irc-ret-void)
                  (let ((c-result (irc-intrinsic-call
                                   return-translator-name
                                   ;; get the 0th value.
                                   (list (irc-tmv-primary cl-result))
                                   "c-result")))
                    (irc-ret c-result)))
              )))))))

(defun codegen-defcallback (result form env)
  (declare (ignore result))             ; no return value
  (destructuring-bind (c-name convention
                       return-type return-translator-name
                       argument-types argument-translator-names
                       parameters place-holder function)
      form
    (let ((closure-temp (alloca-t*)))
      (codegen closure-temp function env)
      (gen-defcallback c-name convention
                       return-type return-translator-name
                       argument-types argument-translator-names
                       parameters place-holder (irc-load closure-temp)))))
