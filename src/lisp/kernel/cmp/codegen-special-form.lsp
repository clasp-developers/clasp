(in-package #:cmp)

;;;; bclasp compilation of special forms.

(export 'llvm-inline)

(core:defconstant-equal +special-operator-dispatch+
  '(
    (progn codegen-progn convert-progn)
    (core:bind-va-list codegen-bind-va-list nil)
    (if codegen-if convert-if)
    (block  codegen-block convert-block)
    (return-from  codegen-return-from convert-return-from)
    (setq codegen-setq convert-setq)
    (let codegen-let convert-let)
    (let* codegen-let* convert-let*)
    (function  codegen-function convert-function)
    (tagbody codegen-tagbody convert-tagbody)
    (go codegen-go convert-go)
    (multiple-value-call  codegen-multiple-value-call convert-multiple-value-call)
    (multiple-value-prog1 codegen-multiple-value-prog1 convert-multiple-value-prog1)
    (flet  codegen-flet convert-flet)
    (labels  codegen-labels convert-labels)
    (eval-when  codegen-eval-when convert-eval-when)
    (the  codegen-the convert-the convert-the convert-the)
    (locally  codegen-locally convert-locally)
    (quote  codegen-quote convert-quote)
    (macrolet  codegen-macrolet convert-macrolet)
    (dbg-i32  codegen-dbg-i32 convert-dbg-i32)
    (load-time-value  codegen-load-time-value convert-load-time-value)
    (core:multiple-value-foreign-call codegen-multiple-value-foreign-call convert-multiple-value-foreign-call)
    (core:foreign-call codegen-foreign-call convert-foreign-call)
    (core:foreign-call-pointer codegen-foreign-call-pointer convert-foreign-call-pointer)
    (symbol-macrolet  codegen-symbol-macrolet convert-symbol-macrolet)
    (core::vector-length codegen-vector-length convert-vector-length)
    (core::%array-dimension codegen-%array-dimension convert-%array-dimension)
    (cleavir-primop:car codegen-car convert-car)
    (cleavir-primop:cdr codegen-cdr convert-cdr)
    (core:vaslist-pop codegen-vaslist-pop convert-vaslist-pop)
    (core:instance-stamp codegen-instance-stamp convert-instance-stamp)
    (core:instance-ref codegen-instance-ref convert-instance-ref)
    (core:instance-set codegen-instance-set convert-instance-set)
    (llvm-inline codegen-llvm-inline convert-llvm-inline)
    (:gc-profiling codegen-gc-profiling convert-gc-profiling)
    (core::debug-message codegen-debug-message convert-debug-message)
    (core::debug-break codegen-debug-break convert-debug-break)
    (core:defcallback codegen-defcallback convert-defcallback)
    ))

(defun make-dispatch-table (alist)
  (let ((hash (make-hash-table :size (max 128 (* 2 (length alist))) :test #'eq)))
    (dolist (entry alist)
      (let ((name (first entry))
	    (codegen-function (second entry))
            (convert-function (third entry)))
	(core::hash-table-setf-gethash hash name (list convert-function codegen-function))))
    hash))

(defvar *special-operator-dispatch* (make-dispatch-table +special-operator-dispatch+))

;;; FUNCTION

(defun codegen-closure (result lambda-or-lambda-block env &key (linkage 'llvm-sys:internal-linkage))
  "codegen a closure.  If result is defined then put the compiled function into result
- otherwise return the cons of llvm-sys::Function_sp's that were compiled for the lambda"
  (assert-result-isa-llvm-value result)
  (multiple-value-bind (compiled-fn lambda-name lambda-list)
      (compile-lambda-function lambda-or-lambda-block env)
    (if (null lambda-name) (error "The lambda doesn't have a name"))
    (if result
        (let* ((lambda-list (irc-load (literal:compile-reference-to-literal lambda-list)))
               (llvm-function-name (llvm-sys:get-name compiled-fn))
               (function-description (llvm-sys:get-named-global *the-module* (function-description-name compiled-fn))))
          (unless function-description
            (error "Could not find function-description for function name: ~a lambda: ~a" llvm-function-name lambda-or-lambda-block))
          ;; TODO:   Here walk the source code in lambda-or-lambda-block and
          ;; get the line-number/column for makeCompiledFunction
          (let* ((runtime-environment (irc-load (irc-renv env)))
                 (fnptr (irc-intrinsic "makeCompiledFunction" 
                                       compiled-fn
                                       (cmp:irc-bit-cast function-description %i8*%)
                                       runtime-environment)))
            (irc-t*-result fnptr result))
          (values compiled-fn lambda-name)))))

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
    (cmp-log "About to codegen-function for: %s%N" name-or-lambda)
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
  (cmp-log "About to codegen-progn with forms: %s%N" forms)
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
      (dbg-set-current-source-pos ithen)
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
  `(core:progv-function ,symbols ,values (lambda () (progn ,@forms))))

;; MULTIPLE-VALUE-CALL

(defun codegen-multiple-value-call (result rest env)
  (with-dbg-lexical-block ()
    (let* ((function-form (car rest))
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
          (codegen result `(core:multiple-value-funcall
                            ,function-form
                            ,@(mapcar (lambda (x) `#'(lambda () (progn ,x))) forms))
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
                 (primary (irc-extract-value tmv '(0)))
                 (nvals (irc-extract-value tmv '(1)))
                 (temp (alloca-temp-values nvals "mvp1-temp")))
            (irc-intrinsic "cc_save_values" nvals primary temp)
            ;; we have extracted what we need from the tmv, so just reuse it
            ;; (and then discard it cruelly)
            (codegen-progn tmvp forms env)
            (irc-tmv-result (irc-intrinsic "cc_load_values" nvals temp) result))))))

;;; SETQ

(defun codegen-special-var-reference (var &optional env)
  (irc-intrinsic "symbolValueReference" (irc-global-symbol var env) (bformat nil "<special-var:%s>" (symbol-name var) )))

(defun codegen-setq (result setq-pairs env)
  "Carry out setq for a collection of pairs"
  (let ((temp-res (alloca-t* "tsetq")))
    (if setq-pairs
	(do* ((cur setq-pairs (cddr cur))
	      (cur-var (car cur) (car cur))
	      (cur-expr (cadr cur) (cadr cur)))
	     ((endp cur) nil)
	  (cmp-log "Compiling setq for target[%s]%N" cur-var)
	  (let ((expanded (macroexpand cur-var env)))
	    (if (eq expanded cur-var)
		;; symbol was not macroexpanded use SETQ
		(progn
		  (cmp-log "The symbol[%s] was not macroexpanded - using SETQ to set it%N" cur-var)
		  (let* ((classified (variable-info env cur-var))
			 (target-ref
                           (cond
                             ((eq (car classified) 'ext:special-var)
                              (codegen-special-var-reference cur-var env))
                             ((eq (car classified) 'ext:lexical-var)
                              (let ((symbol (cadr classified))
                                    (depth (third classified))
                                    (index (fourth classified))
                                    (dest-env (fifth classified)))
                                (codegen-lexical-var-reference symbol
                                                               depth
                                                               index
                                                               env
                                                               dest-env)))
                             (t (error "Handle codegen-setq with ~s" classified)))
                           ))
		    (codegen temp-res cur-expr env)
		    (irc-t*-result (irc-load temp-res) target-ref)))
		;; symbol was macroexpanded use SETF
		(progn
		  (cmp-log "The symbol[%s] was macroexpanded to result[%s] setting with SETF%N"
                           cur-var expanded)
		  (codegen temp-res `(setf ,expanded ,cur-expr) env))))
	  (unless (cddr cur)
	    (irc-t*-result (irc-load temp-res) result)))
	;; There were no pairs, return nil
	(codegen-literal result nil env))))

;;; LET, LET*

(defun codegen-fill-let-environment (new-env lambda-list-handler
				     exps parent-env evaluate-env)
  "Evaluate each of the exps in the evaluate-env environment
and put the values into the activation frame for new-env.
env is the parent environment of the (result-af) value frame"
  (multiple-value-bind (reqvars)
      (process-lambda-list-handler lambda-list-handler)
    (let ((number-of-lexical-vars (number-of-lexical-variables lambda-list-handler))
	  (result-af (irc-renv new-env)))
;;      (dbg-set-activation-frame-for-ihs-top (irc-renv new-env))
      (irc-make-value-frame-set-parent new-env number-of-lexical-vars parent-env) ;(irc-intrinsic "setParentOfActivationFrame" result-af (irc-renv parent-env))
      ;; Save all special variables
      (do* ((cur-req (cdr reqvars) (cdr cur-req))
	    (classified-target (car cur-req) (car cur-req)))
	   ((endp cur-req) nil)
	(compile-save-if-special new-env classified-target))
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
	  (dbg-set-current-source-pos exp)
	  (codegen temp exp evaluate-env))
	;; Now generate code for let
	(cmp-log "About to generate code for exps: %s%N" exps)
	(do* ((cur-req (cdr reqvars) (cdr cur-req))
	      (classified-target (car cur-req) (car cur-req))
	      (tempidx 0 (1+ tempidx)))
	     ((endp cur-req) nil)
	  (let* ((target-head (car classified-target))
		 (target-idx (cdr classified-target)))
	    (with-target-reference-do (target-ref classified-target new-env)
	      (irc-t*-result (irc-load (elt temps tempidx)) target-ref))))))))

(defun codegen-fill-let*-environment (new-env lambda-list-handler
				     exps parent-env evaluate-env)
  "Evaluate each of the exps in the evaluate-env environment
and put the values into the activation frame for new-env.
env is the parent environment of the (result-af) value frame"
  (cmp-log "entered codegen-fill-let*-environment%N")
  (cmp-log "   new-env -> %s%N" new-env)
  (cmp-log "   parent-env -> %s%N" parent-env)
  (cmp-log "   evaluate-env -> %s%N" evaluate-env)
  (multiple-value-bind (reqvars)
      (process-lambda-list-handler lambda-list-handler)
    (let ((number-of-lexical-vars (number-of-lexical-variables lambda-list-handler))
	  (result-af (irc-renv new-env)))
;;      (dbg-set-activation-frame-for-ihs-top (irc-renv new-env))
      (irc-make-value-frame-set-parent new-env number-of-lexical-vars parent-env)
      ;; Save all special variables
      (do* ((cur-req (cdr reqvars) (cdr cur-req))
	    (classified-target (car cur-req) (car cur-req)))
	   ((endp cur-req) nil)
	(compile-save-if-special new-env classified-target))
      ;; Now generate code for let
      (cmp-log "About to generate code for exps: %s%N" exps)
      (do* ((cur-req (cdr reqvars) (cdr cur-req))
	    (classified-target (car cur-req) (car cur-req))
	    (cur-exp exps (cdr cur-exp))
	    (exp (car cur-exp) (car cur-exp)))
	   ((endp cur-req) nil)
	(let* ((target-head (car classified-target))
	       (target-idx (cdr classified-target)))
	  (with-target-reference-do (target-ref classified-target new-env)
	    (dbg-set-current-source-pos exp)
	    (codegen target-ref exp evaluate-env)))))))

(defun codegen-let/let* (operator-symbol result parts env)
  (with-dbg-lexical-block ()
    (let ((assignments (car parts))
	  (body (cdr parts)))
      (multiple-value-bind (variables expressions)
	  (separate-pair-list assignments)
	(multiple-value-bind (declares code docstring specials )
	    (process-declarations body t)
	  (cmp-log "About to create lambda-list-handler%N")
	  (let* ((lambda-list-handler (make-lambda-list-handler variables declares 'core::function))
		 (new-env (irc-new-unbound-value-environment-of-size
			   env
			   :number-of-arguments (number-of-lexical-variables lambda-list-handler) ;; lambda-list-handler lambda-list-handler
			   :label (symbol-name operator-symbol)))
		 ;; There is a huge problem with setting evaluate-env for 'let
		 ;; in that the let evaluate-environment is not the same environment
		 ;; used by with-try and codegen-fill-let/let*-environment
		 ;; The problem is that the code generated within codegen-fill-let/let*-environment
		 ;; connects to the env dispatcher rather than the new-env dispatcher
		 (evaluate-env (cond
				 ((eq operator-symbol 'let) env) ;;; This is a problem right here
				 ((eq operator-symbol 'let*) new-env)
				 (t (error "let/let* doesn't understand operator symbol[~a]" operator-symbol))))
		 traceid)
	    (with-try
                (progn
                  (irc-branch-to-and-begin-block (irc-basic-block-create
                                                  (bformat nil "%s-start" (symbol-name operator-symbol))))
                  (if (eq operator-symbol 'let)
                      (codegen-fill-let-environment new-env lambda-list-handler expressions env evaluate-env)
                      (codegen-fill-let*-environment new-env lambda-list-handler expressions env evaluate-env))
                  (cmp-log "About to evaluate codegen-progn%N")
                  (codegen-progn result code new-env))
	      ((cleanup)
               ;;               (dbg-set-activation-frame-for-ihs-top (irc-renv new-env))
	       (irc-unwind-environment new-env)
	       ))
	    )
	  ))))
  (cmp-log "Done codegen-let/let*%N"))

(defun codegen-let (result rest env)
  (codegen-let/let* 'let result rest env))

(defun codegen-let* (result rest env)
  (codegen-let/let* 'let* result rest env))

;;; If

;;; see typeq.lsp for continuation
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
          ((fixnum) (base-type-check object-raw +fixnum-mask+ +fixnum-tag+ thenb elseb))
          ((cons) (base-type-check object-raw +immediate-mask+ +cons-tag+ thenb elseb))
          ((character) (base-type-check object-raw +immediate-mask+ +character-tag+ thenb elseb))
          ((single-float) (base-type-check object-raw +immediate-mask+ +single-float-tag+ thenb elseb))
          (t
           (let ((header-value-min-max (gethash type core:+type-header-value-map+)))
             (when (null header-value-min-max)
               (format t "typeq type = ~a~%" type)
               (compiler-error nil "unknown type for typeq: %s" type))
             (compile-header-check header-value-min-max object-raw thenb elseb))))))))

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
    (let ((test-result (llvm-sys:create-icmp-eq *irbuilder*
                                                (irc-intrinsic "isTrue" (irc-load test-temp-store))
                                                (jit-constant-i32 1)
                                                "ifcond")))
      (irc-cond-br test-result thenb elseb))))

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
		      (setq result (cons (list index (irc-basic-block-create (bformat nil "tagbody-%s-%s" (car x) index)) x) result))
		      (add-tag tagbody-env (car x) result)
		      (setq index (+ 1 index))))) code)
    (nreverse result)))

(defun codegen-tagbody (result rest env)
  "Extract tags and code from (rest) and create an alist that maps
tag symbols to code and llvm-ir basic-blocks. Store the alist in the symbol-to-block-alist
metadata of the tagbody-env. These can be accessed by (go XXXX) special operators to
jump to blocks within this tagbody."
  (assert-result-isa-llvm-value result)
  (unless (and (car rest) (symbolp (car rest))) (push (gensym) rest)) ;; stick a dummy tag at the head if there isn't one
  (let* ((tagbody-env (irc-new-tagbody-environment env))
	 (enumerated-tag-blocks (tagbody.enumerate-tag-blocks rest tagbody-env)))
    ;; If the GO spec.ops. are in the same function we could use simple cleanup and branches for TAGBODY/GO
    ;; so save the function
    (let* ((renv (irc-load (irc-renv env)))
           (instruction (irc-intrinsic "makeTagbodyFrameSetParent" renv)))
      (irc-t*-result instruction (irc-renv tagbody-env))
      (irc-low-level-trace :tagbody)
      (setf-metadata tagbody-env 'tagbody-function *current-function*)
      (cmp-log "codegen-tagbody tagbody environment: %s%N" tagbody-env)
      (let ((handle (irc-intrinsic "initializeTagbodyClosure" (irc-renv tagbody-env))))
        #+optimize-bclasp
        (setf (gethash tagbody-env *tagbody-frame-info*)
              (make-tagbody-frame-info :tagbody-environment tagbody-env
                                       :make-tagbody-frame-instruction (list instruction (list (irc-load (irc-renv env))))
                                       :initialize-tagbody-closure (list handle (list (irc-renv tagbody-env)))))
        (with-try
            (progn
              (let ((go-blocks nil))
                (mapl #'(lambda (cur)
                          (let* ((tag-begin (car cur))
                                 (tag-end (cadr cur))
                                 (section-block (cadr tag-begin)))
                            (push section-block go-blocks)))
                      enumerated-tag-blocks)
                (let ((go-vec (make-array (length go-blocks) :initial-contents (nreverse go-blocks))))
                  (setf-metadata tagbody-env 'tagbody-blocks go-vec)))
              (mapl #'(lambda (cur)
                        (let* ((tag-begin (car cur))
                               (tag-end (cadr cur))
                               (section-block (cadr tag-begin))
                               (section-next-block (cadr tag-end))
                               (section (extract-section (caddr tag-begin) (caddr tag-end))))
                          (irc-branch-if-no-terminator-inst section-block)
                          (irc-begin-block section-block)
                          (codegen-progn result section tagbody-env)
                          (when section-next-block (irc-branch-if-no-terminator-inst section-next-block))
                          ))
                    enumerated-tag-blocks))
          ;;        ((cleanup) (codegen-literal result nil env))
          ((cleanup) (irc-unwind-environment tagbody-env))
          ((typeid-core-dynamic-go exception-ptr)
           (let* ((go-index (irc-intrinsic "tagbodyHandleDynamicGoIndex_or_rethrow" exception-ptr handle))
                  (default-block (irc-basic-block-create "switch-default"))
                  (sw (irc-switch go-index default-block (length enumerated-tag-blocks))))
             (mapc #'(lambda (one) (irc-add-case sw (jit-constant-size_t (car one)) (cadr one)))
                   enumerated-tag-blocks)
             (irc-begin-block default-block)
             (irc-intrinsic "throwIllegalSwitchValue"
                            go-index (jit-constant-size_t (length enumerated-tag-blocks))))))
        (codegen-literal result nil env)))))

(defun codegen-go (result rest env)
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
       (let ((depth (cadr classified-tag))
	     (index (caddr classified-tag))
	     (tagbody-env (cadddr classified-tag)))
	 (cmp-log "Target tagbody environment: %s  tag: %s%N" tagbody-env tag)
	 (let* ((go-vec (lookup-metadata tagbody-env 'tagbody-blocks))
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
      (multiple-value-bind (block-env make-block-frame-instruction make-block-frame-instruction-arguments)
          (irc-make-block-environment-set-parent block-symbol env)
	(let ((block-start (irc-basic-block-create
			    (bformat nil "block-%s-start" (symbol-name block-symbol))))
	      (local-return-block (irc-basic-block-create (bformat nil "local-return-%s-block" (symbol-name block-symbol))))
	      (after-return-block (irc-basic-block-create (bformat nil "after-return-%s-block" (symbol-name block-symbol)))))
	  (setf-metadata block-env :local-return-block local-return-block)
	  (irc-br block-start "block-start")
	  (irc-begin-block block-start)
          (let ((handle (irc-intrinsic "initializeBlockClosure" (irc-renv block-env))))
            #+optimize-bclasp
            (let ((info (make-block-frame-info :block-environment block-env
                                               :block-symbol block-symbol
                                               :make-block-frame-instruction (list make-block-frame-instruction  make-block-frame-instruction-arguments)
                                               :initialize-block-closure-instruction (list handle (list (irc-renv block-env))))))
              #+debug-lexical-depth
              (let* ((frame-unique-id (gctools:next-lexical-depth-counter))
                     (set-frame-unique-id (irc-intrinsic "setFrameUniqueId" (jit-constant-size_t frame-unique-id) (irc-load (irc-renv block-env)))))
                (setf (block-frame-info-frame-unique-id info) frame-unique-id)
                (setf (block-frame-info-set-frame-unique-id info) (list set-frame-unique-id (list (jit-constant-size_t frame-unique-id) (irc-load (irc-renv block-env))))))
              (setf (gethash block-env *block-frame-info*) info))
            (with-try
                (codegen-progn result body block-env)
              ((cleanup)
               (irc-unwind-environment block-env))
              ((typeid-core-return-from exception-ptr)
               (let ((handle-instruction (irc-intrinsic "blockHandleReturnFrom_or_rethrow" exception-ptr handle)))
                 (irc-tmv-result handle-instruction result))))
            (irc-br after-return-block "after-return-block")
            (irc-begin-block local-return-block)
            (let ((val (irc-intrinsic "restoreFromMultipleValue0")))
              (irc-tmv-result val result))
            (irc-br after-return-block "after-return-block-2")
            (irc-begin-block after-return-block)))))))

(defun codegen-return-from (result rest env)
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
                (let* (#+debug-lexical-depth (frame-info (gethash block-env *block-frame-info*))
                       #+debug-lexical-depth (frame-unique-id (block-frame-info-frame-unique-id frame-info))
                       #+debug-lexical-depth (ensure-frame-unique-id (irc-intrinsic "ensureFrameUniqueId" (jit-constant-size_t frame-unique-id) (jit-constant-size_t depth) (irc-load (irc-renv env))))
                       (return-from-call (irc-intrinsic "throwReturnFrom" (jit-constant-size_t depth) (irc-load (irc-renv env))))
                       (start-renv (irc-load (irc-renv env))))
                  #+optimize-bclasp
                  (push (make-throw-return-from :instruction return-from-call
                                                :depth depth
                                                :start-env env
                                                :start-renv start-renv
                                                :block-env block-env
                                                :block-symbol block-symbol
                                                #+debug-lexical-depth :ensure-frame-unique-id
                                                #+debug-lexical-depth (list ensure-frame-unique-id
                                                                            (list (jit-constant-size_t frame-unique-id) (jit-constant-size_t depth) (irc-load (irc-renv env)))))
                        *throw-return-from-instructions*)))
              (let* ((local-return-block (lookup-metadata block-env :local-return-block)))
                (codegen temp-mv-result return-form env)
                
                (irc-unwind-into-environment env block-env)
                #+(or)(let ((saved-values (irc-intrinsic "saveValues" temp-mv-result)))
                  (irc-unwind-into-environment env block-env)
                  (irc-intrinsic "loadValues" temp-mv-result saved-values))
                (irc-intrinsic "saveToMultipleValue0" temp-mv-result)
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
	     (target (irc-intrinsic "functionFrameReference" (irc-load result-af) (jit-constant-i32 fn-index)
			       (bformat nil "%s-ref-%d" (llvm-sys:get-name result-af) fn-index) )))
	(codegen-closure target fn-lambda closure-env)))))

(defun codegen-flet/labels (operator-symbol result rest env)
  (with-dbg-lexical-block ()
    (let* ((functions (car rest))
	   (body (cdr rest))
	   (function-env (irc-new-function-value-environment env :functions functions)))
      (multiple-value-bind (declares code docstring specials)
	  (process-declarations body nil) ;; don't expect docstring
	(let ((evaluate-env (cond
			      ((eq operator-symbol 'flet) env)
			      ((eq operator-symbol 'labels) function-env)
			      (t (error "flet/labels doesn't understand operator symbol[~a]" operator-symbol)))))
	  (with-try
	    (progn
	      (irc-branch-to-and-begin-block (irc-basic-block-create
					      (bformat nil "%s-start"
						       (symbol-name operator-symbol))))
	      (codegen-fill-function-frame operator-symbol function-env functions env evaluate-env)
;;              (dbg-set-activation-frame-for-ihs-top (irc-renv function-env))
	      (codegen-progn result code function-env))
	    ((cleanup)
	     (irc-unwind-environment function-env)))
	  )))))

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
    (multiple-value-bind (declares code docstring specials )
	(process-declarations body t)
      (augment-environment-with-declares macro-env declares)
      (codegen-progn result code macro-env))))

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
      (let ((new-env (irc-new-unbound-value-environment-of-size
		      env
		      :number-of-arguments (length specials)
		      :label "locally-env")))
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
  (cmp-log "Starting codegen-load-time-value rest: %s%N" rest)
  (let* ((form (car rest))
	 (read-only-p (cadr rest)))
;;; Currently if read-only-p is T there is no
;;; coalescence performed - this could be added as an optimization
    (if *generate-compile-file-load-time-values*
        (let ((index (literal:with-load-time-value (literal:compile-load-time-value-thunk form))))
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
         (untagged-dim (irc-intrinsic-call "cc_arrayDimension"
                                           (list array untagged-axisn))))
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
    (irc-t*-result (irc-load (gen-memref-address
                              (irc-load cons-alloca)
                              (- +cons-car-offset+ +cons-tag+)))
                   result)))

(defun codegen-cdr (result rest env)
  (let ((cons-form (first rest))
        (cons-alloca (alloca-t* "cons")))
    (codegen cons-alloca cons-form env)
    (irc-t*-result (irc-load (gen-memref-address
                              (irc-load cons-alloca)
                              (- +cons-cdr-offset+ +cons-tag+)))
                   result)))

;;; CORE:VASLIST-POP

(defun codegen-vaslist-pop (result rest env)
  (let ((form (car rest))
        (vaslist (alloca-t* "vaslist-pop-vaslist")))
    (codegen vaslist form env)
    (irc-t*-result (irc-intrinsic "cx_vaslist_pop" (irc-load vaslist)) result)))

;;; CORE:INSTANCE-STAMP

(defun codegen-instance-stamp (result rest env)
  (let ((form (car rest))
        (object (alloca-t* "instance-stamp-instance")))
    (codegen object form env)
    (if *test-ir*
        (let ((new-stamp (irc-read-stamp (irc-load object))))
          (irc-t*-result (irc-intrinsic "cx_read_stamp" (irc-load object) new-stamp) result))
        (irc-t*-result (irc-intrinsic "cx_read_stamp" (irc-load object) (jit-constant-i64 0)) result))))

;;; CORE:INSTANCE-REF
;;; the gen- are for cclasp. At the moment they're unused, but that's just because
;;; the runtime fastgf compiler uses bclasp so it's a bit lower priority.
(defun gen-instance-ref (instance index)
  (irc-read-slot instance (irc-untag-fixnum index %size_t% "slot-location"))
  #+(or)(irc-intrinsic "cc_read_slot" instance
                 (irc-untag-fixnum index %size_t% "slot-location")))

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
  (irc-write-slot instance (irc-untag-fixnum index %size_t% "slot-location") value)
  #+(or)(irc-intrinsic "cc_write_slot" instance
                       (irc-untag-fixnum index %size_t% "slot-location")
                       value))

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

;;; DBG-i32

(defparameter *nexti* 10000)
(defun codegen-dbg-i32 (result rest env)
  (let ((giveni (car rest)))
    (if (null giveni)
	(progn
	  (setq giveni *nexti*)
	  (setq *nexti* (+ 1 *nexti*))))
    (irc-intrinsic "debugPrintI32" (jit-constant-i32 giveni))))

;;; DEBUG-MESSAGE

(defun codegen-debug-message (result rest env)
  (let ((message (jit-constant-unique-string-ptr (car rest))))
    (irc-intrinsic "debugMessage" message)))

(defun codegen-debug-break (result rest env)
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
  `(core:bformat *error-output* ,fmt ,@fargs))
(defmacro blog (fmt &rest fargs) nil)

;;; core:bind-va-list
(defun codegen-bind-va-list (result form evaluate-env)
  (let ((lambda-list (first form))
        (vaslist     (second form))
        (body        (cddr form)))
    (blog "evaluate-env -> %s%N" evaluate-env)
    (multiple-value-bind (declares code docstring specials)
        (process-declarations body t)
      (let ((canonical-declares (core:canonicalize-declarations declares)))
        (multiple-value-bind (cleavir-lambda-list new-body rest-alloc)
            (transform-lambda-parts lambda-list canonical-declares code)
          (blog "got cleavir-lambda-list -> %s%N" cleavir-lambda-list)
          (let ((debug-on nil)
                (eval-vaslist (alloca-t* "bind-vaslist")))
            (codegen eval-vaslist vaslist evaluate-env)
            (let* ((lvaslist (irc-load eval-vaslist "lvaslist"))
                   (src-remaining-nargs* (irc-intrinsic "cc_vaslist_remaining_nargs_address" lvaslist))
                   (src-va_list* (irc-intrinsic "cc_vaslist_va_list_address" lvaslist "vaslist_address"))
                   (local-va_list* (alloca-va_list "local-va_list"))
                   (_             (irc-intrinsic-call "llvm.va_copy" (list (irc-pointer-cast local-va_list* %i8*%)
                                                                           (irc-pointer-cast src-va_list* %i8*%))))
                   (callconv (make-calling-convention-impl :nargs (irc-load src-remaining-nargs*)
                                                           :va-list* local-va_list*
                                                           :rest-alloc rest-alloc
                                                           :cleavir-lambda-list cleavir-lambda-list)))
              ;; See comment in cleavir bind-va-list w/r/t safep.
              (let ((new-env (bclasp-compile-lambda-list-code evaluate-env callconv :safep nil)))
                (irc-intrinsic-call "llvm.va_end" (list (irc-pointer-cast local-va_list* %i8*%)))
                (codegen-let/let* (car new-body) result (cdr new-body) new-env)))))))))

;;; MULTIPLE-VALUE-FOREIGN-CALL

(defun codegen-multiple-value-foreign-call (result form evaluate-env)
  "Evaluate each of the arguments into an alloca and invoke the function"
  ;; setup the ActivationFrame for passing arguments to this function in the setup arena
  (assert-result-isa-llvm-value result)
  ;;(bformat t "In codegen-multiple-value-foreign-call codegen form: %s%N" form)
  (let* ((intrinsic-name (car form))
         (nargs (length (cdr form)))
         args
         (temp-result (alloca-t*)))
    ;; evaluate the arguments into the array
    ;;  used to be done by --->    (codegen-evaluate-arguments (cdr form) evaluate-env)
    (do* ((cur-exp (cdr form) (cdr cur-exp))
          (exp (car cur-exp) (car cur-exp))
          (i 0 (+ 1 i)))
         ((endp cur-exp) nil)
      ;;(bformat t "In codegen-multiple-value-foreign-call codegen arg[%d] -> %d%N" i exp)
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
           (result-in-registers
             (irc-call-or-invoke func (nreverse args))))
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
      ;;(bformat t "In codegen-multiple-value-foreign-call codegen arg[%d] -> %d%N" i exp)
      (codegen temp-result exp evaluate-env)
      (push (irc-intrinsic-call (clasp-ffi::from-translator-name type)
                             (list (irc-load temp-result))) args))
    args))

(defun codegen-foreign-call (result form evaluate-env)
  "Evaluate each of the arguments into an alloca and invoke the function"
  ;; setup the ActivationFrame for passing arguments to this function in the setup arena
  (assert-result-isa-llvm-value result)
  ;;(bformat t "In codegen-multiple-value-foreign-call codegen form: %s%N" form)
  (let* ((foreign-types (first form))
         (intrinsic-name (second form))
         (fargs (cddr form))
         (nargs (length fargs))
         (temp-result (alloca-t*)))
    ;; evaluate the arguments into the array
    ;;  used to be done by --->    (codegen-evaluate-arguments (cddr form) evaluate-env)
    (let* ((args (evaluate-foreign-arguments fargs foreign-types temp-result evaluate-env))
           (func (or (llvm-sys:get-function *the-module* intrinsic-name)
                     (irc-function-create
                      (function-type-create-on-the-fly foreign-types)
                      'llvm-sys::External-linkage
                      intrinsic-name
                      *the-module*)))
           (foreign-result
            (irc-call-or-invoke func (nreverse args)))
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
         (nargs (length fargs))
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
              (cmp::irc-call-or-invoke function-pointer (nreverse args)))
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
                        return-type return-translator-name
                        argument-types argument-translator-names
                        parameters place-holder closure-value)
  (declare (ignore convention))         ; FIXME
  ;; parameters should be a list of symbols, i.e. lambda list with only required.
  (unless (= (length argument-types) (length parameters) (length argument-translator-names))
    (error "BUG: Callback function parameters and types have a length mismatch"))
  ;;; Generate a variable and put the closure in it.
  (let* ((closure-literal-slot-index (literal:lookup-literal-index place-holder))
         (closure-var-name (core:bformat nil "%s_closure_var" c-name)))
    (irc-t*-result closure-value (literal:constants-table-reference closure-literal-slot-index))
    ;; Now generate the C function.
    ;; We don't actually "do" anything with it- just leave it there to be linked/used like a C function.
    (with-landing-pad nil ; Since we're in a new function (which should never be an unwind dest)
      (let* ((c-argument-names (mapcar #'string parameters))
             (c-function-type (llvm-sys:function-type-get return-type argument-types))
             (new-func (llvm-sys:function-create c-function-type
                                                 'llvm-sys:external-linkage
                                                 c-name
                                                 *the-module*))
             (*current-function* new-func)
             (*current-function-name* c-name))
        (with-irbuilder ((llvm-sys:make-irbuilder *llvm-context*))
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
                               closure-to-call cl-args (core:bformat nil "%s_closure" c-name))))
              ;; Now generate a call the translator for the return value if applicable, then return.
              ;; NOTE: (eq return-type %void%) doesn't seem to work - and it's sketchy because it's a symbol macro
              (if (string= return-translator-name "from_object_void")
                  (irc-ret-void)
                  (let ((c-result (irc-intrinsic-call
                                   return-translator-name
                                   ;; get the 0th value.
                                   (list (irc-extract-value cl-result (list 0) "primary"))
                                   "c-result")))
                    (irc-ret c-result))))))))))

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
