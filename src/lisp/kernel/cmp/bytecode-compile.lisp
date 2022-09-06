#+(or)
(defpackage #:compile-to-vm
  (:use #:cl)
  (:shadow #:compile))

(in-package #:cmp)

(setq *print-circle* t)

(defmacro logf (message &rest args)
  (declare (ignore message args))
  nil)
#+(or)
(progn
  (defvar *bclog* (progn
                    (format t "!~%!~%!   Opening /tmp/allcode.log - logging all bytecode compilation~%!~%!~%")
                    (open "/tmp/allcode.log" :direction :output :if-exists :supersede)))
  (defun log-function (cfunction compile-info bytecode)
    (format *bclog* "Name: ~s~%" (cmp:cfunction/name cfunction))
    (let ((*print-circle* t))
      (format *bclog* "Form: ~s~%" (car compile-info))
      (format *bclog* "Bytecode: ~s~%" bytecode)
      (finish-output *bclog*)))
  (defmacro logf (message &rest args)
    `(format *bclog* ,message ,@args)))

;;; FIXME: New package
(macrolet ((defcodes (&rest names)
             `(progn
                ,@(let ((forms nil))
                    (do ((i 0 (1+ i))
                         (names names (cdr names)))
                        ((endp names) forms)
                      (push `(defconstant ,(first names) ,i) forms)))
                (defparameter *codes* '(,@names))
                #-clasp ; collides with core:decode, and we don't need it.
                (defun decode (code)
                  (nth code '(,@names))))))
  (defcodes +ref+ +const+ +closure+
    +call+ +call-receive-one+ +call-receive-fixed+
    +bind+ +set+
    +make-cell+ +cell-ref+ +cell-set+
    +make-closure+ +make-uninitialized-closure+ +initialize-closure+
    +return+
    +bind-required-args+ +bind-optional-args+
    +listify-rest-args+ +parse-key-args+
    +jump-8+ +jump-16+ +jump-24+
    +jump-if-8+ +jump-if-16+ +jump-if-24+
    +jump-if-supplied-8+ +jump-if-supplied-16+
    +check-arg-count<=+ +check-arg-count>=+ +check-arg-count=+
    +push-values+ +append-values+ +pop-values+
    +mv-call+ +mv-call-receive-one+ +mv-call-receive-fixed+
    +entry+
    +exit-8+ +exit-16+ +exit-24+
    +entry-close+
    +catch-8+ +catch-16+
    +throw+ +catch-close+
    +special-bind+ +symbol-value+ +symbol-value-set+ +unbind+
    +progv+
    +fdefinition+
    +nil+
    +eq+
    +push+ +pop+
    +long+))

;;;

(defun (setf cmp:lexical-var-info/closed-over-p) (new info)
  (cmp:lexical-var-info/setf-closed-over-p info new))
(defun (setf cmp:lexical-var-info/set-p) (new info)
  (cmp:lexical-var-info/setf-set-p info new))

(defun make-symbol-macro-var-info (expansion)
  (cmp:symbol-macro-var-info/make
   (lambda (form env) (declare (ignore form env)) expansion)))

(defun make-null-lexical-environment ()
  (cmp:lexenv/make nil nil nil nil 0))

(defun make-lexical-environment (parent &key (vars (cmp:lexenv/vars parent))
                                          (tags (cmp:lexenv/tags parent))
                                          (blocks (cmp:lexenv/blocks parent))
                                          (frame-end (cmp:lexenv/frame-end parent))
                                          (funs (cmp:lexenv/funs parent)))
  (cmp:lexenv/make vars tags blocks funs frame-end))

(deftype lambda-expression () '(cons (eql lambda) (cons list list)))

(defun context-module (context)
  (cmp:cfunction/module (cmp:context/cfunction context)))

(defun bytecompile (lambda-expression
                    &optional (env (make-null-lexical-environment)))
  (check-type lambda-expression lambda-expression)
  (logf "vvvvvvvv bytecompile ~%Form: ~s~%" lambda-expression)
  (let* ((module (cmp:module/make))
         (lambda-list (cadr lambda-expression))
         (body (cddr lambda-expression)))
    (logf "-------- About to link~%")
    (multiple-value-prog1
        (link-function (compile-lambda lambda-list body env module) (cons lambda-expression env))
      (logf "^^^^^^^^^ Compile done~%"))))

(defun compile-form (form env context)
  (when *code-walker*
    (setq form (funcall sys:*code-walker* form env)))
  (cond ((symbolp form) (compile-symbol form env context))
        ((consp form) (compile-cons (car form) (cdr form) env context))
        (t (compile-literal form env context))))

(defun compile-load-time-value (form env context)
  (if *generate-compile-file-load-time-values*
      (error "Handle compile-file")
      (let ((value (eval form)))
        (compile-literal value env context))))

(defun compile-symbol (form env context)
  (let ((info (cmp:var-info form env)))
    (cond ((typep info 'cmp:symbol-macro-var-info)
           (let ((expander
                   (cmp:symbol-macro-var-info/expander info)))
             (compile-form (funcall *macroexpand-hook* expander form env)
                           env context)))
          ;; A symbol macro could expand into something with arbitrary side
          ;; effects so we always have to compile that, but otherwise, if no
          ;; values are wanted, we want to not compile anything.
          ((eql (cmp:context/receiving context) 0))
          (t
           (cond
             ((typep info 'cmp:lexical-var-info)
              (cond ((eq (cmp:lexical-var-info/cfunction info)
                         (cmp:context/cfunction context))
                     (assemble-maybe-long
                      context +ref+
                      (cmp:lexical-var-info/frame-index info)))
                    (t
                     (setf (cmp:lexical-var-info/closed-over-p info) t)
                     (assemble-maybe-long context
                                          +closure+ (context/closure-index context info))))
              (context/maybe-emit-cell-ref context info))
             ((typep info 'cmp:special-var-info)
              (assemble-maybe-long context +symbol-value+
                                   (context/literal-index context form)))
             ((typep info 'cmp:constant-var-info)
              (return-from compile-symbol ; don't pop again.
                (compile-literal (cmp:constant-var-info/value info)
                                 env context)))
             ((null info)
              (warn "Unknown variable ~a: treating as special" form)
              (assemble context +symbol-value+
                        (context/literal-index context form)))
             (t (error "BUG: Unknown info ~a" info)))
           (when (eq (cmp:context/receiving context) t)
             (assemble context +pop+))))))

(defun compile-cons (head rest env context)
  (logf "compile-cons ~s~%" (list* head rest))
  (cond
    ((eq head 'progn) (compile-progn rest env context))
    ((eq head 'let) (compile-let (first rest) (rest rest) env context))
    ((eq head 'let*) (compile-let* (first rest) (rest rest) env context))
    ((eq head 'flet) (compile-flet (first rest) (rest rest) env context))
    ((eq head 'labels) (compile-labels (first rest) (rest rest) env context))
    ((eq head 'setq) (compile-setq rest env context))
    ((eq head 'if) (compile-if (first rest) (second rest) (third rest) env context))
    ((eq head 'function) (compile-function (first rest) env context))
    ((eq head 'tagbody) (compile-tagbody rest env context))
    ((eq head 'go) (compile-go (first rest) env context))
    ((eq head 'block) (compile-block (first rest) (rest rest) env context))
    ((eq head 'return-from) (compile-return-from (first rest) (second rest) env context))
    ;; handled by macros
    #-clasp
    ((eq head 'catch) (compile-catch (first rest) (rest rest) env context))
    #-clasp
    ((eq head 'throw) (compile-throw (first rest) (second rest) env context))
    #-clasp
    ((eq head 'progv)
     (compile-progv (first rest) (second rest) (rest (rest rest)) env context))
    ((eq head 'quote) (compile-literal (first rest) env context))
    ((eq head 'load-time-value) (compile-load-time-value (first rest) env context))
    ((eq head 'symbol-macrolet)
     (compile-symbol-macrolet (first rest) (rest rest) env context))
    #+clasp
    ((eq head 'macrolet)
     (compile-macrolet (first rest) (rest rest) env context))
    ((eq head 'multiple-value-call)
     (compile-multiple-value-call (first rest) (rest rest) env context))
    ((eq head 'multiple-value-prog1)
     (compile-multiple-value-prog1 (first rest) (rest rest) env context))
    ((eq head 'locally) (compile-locally rest env context))
    ((eq head 'eval-when) (compile-eval-when (first rest) (rest rest) env context))
    ((eq head 'the) ; don't do anything.
     (compile-form (second rest) env context))
    (t ; function call or macro
     (let ((info (cmp:fun-info head env)))
       (cond
         ((typep info 'cmp:global-macro-info)
          (let* ((expander
                   (cmp:global-macro-info/expander info))
                 (expanded
                   (funcall *macroexpand-hook* expander (cons head rest) env)))
            (compile-form expanded env context)))
         ((typep info 'cmp:local-macro-info)
          (let* ((expander
                   (cmp:local-macro-info/expander info))
                 (expanded
                   (funcall *macroexpand-hook* expander (cons head rest) env)))
            (compile-form expanded env context)))
         ((typep info '(or cmp:global-fun-info
                        cmp:local-fun-info
                        null))
          ;; unknown function warning handled by compile-function
          ;; note we do a double lookup of the fun info,
          ;; which is inefficient in the compiler (generated code is ok)
          (compile-function head env (context/sub context 1))
          (do ((args rest (rest args))
               (arg-count 0 (1+ arg-count)))
              ((endp args)
               (context/emit-call context arg-count))
            (compile-form (first args) env (context/sub context 1))))
         (t (error "BUG: Unknown info ~a" info)))))))

(defun compile-progn (forms env context)
  (do ((forms forms (rest forms)))
      ((null (rest forms))
       (compile-form (first forms) env context))
    (compile-form (first forms) env (context/sub context 0))))

(defun compile-locally (body env context)
  (multiple-value-bind (decls body docs specials)
      (core:process-declarations body nil)
    (declare (ignore decls docs))
    (compile-progn body (if specials (lexenv/add-specials env specials) env) context)))

(defun compile-eval-when (situations body env context)
  (if (or (member 'cl:eval situations) (member :execute situations))
      (compile-progn body env context)
      (compile-literal nil env context)))

(defun canonicalize-binding (binding)
  (if (consp binding)
      (values (first binding) (second binding))
      (values binding nil)))

(defun compile-let (bindings body env context)
  (multiple-value-bind (decls body docs specials)
      (core:process-declarations body nil)
    (declare (ignore decls docs))
    (let ((lexical-binding-count 0)
          (special-binding-count 0)
          (post-binding-env (lexenv/add-specials env specials)))
      (dolist (binding bindings)
        (multiple-value-bind (var valf) (canonicalize-binding binding)
          (compile-form valf env (context/sub context 1))
          (cond ((or (member var specials)
                     (typep (cmp:var-info var env)
                            'cmp:special-var-info))
                 (incf special-binding-count)
                 (context/emit-special-bind context var))
                (t
                 (setq post-binding-env
                       (lexenv/bind-vars post-binding-env (list var) context))
                 (incf lexical-binding-count)
                 (context/maybe-emit-make-cell
                  context (cmp:var-info var post-binding-env))))))
      (context/emit-bind context lexical-binding-count
                 (cmp:lexenv/frame-end env))
      (compile-progn body post-binding-env context)
      (context/emit-unbind context special-binding-count))))

(defun compile-let* (bindings body env context)
  (multiple-value-bind (decls body docs specials)
      (core:process-declarations body nil)
    (declare (ignore decls docs))
    (let ((special-binding-count 0))
      (dolist (binding bindings)
        (let ((var (if (consp binding) (car binding) binding))
              (valf (if (and (consp binding) (consp (cdr binding)))
                        (cadr binding)
                        'nil)))
          (compile-form valf env (context/sub context 1))
          (cond ((or (member var specials) (ext:specialp var))
                 (incf special-binding-count)
                 (setq env (lexenv/add-specials env (list var)))
                 (context/emit-special-bind context var))
                (t
                 (let ((frame-start (cmp:lexenv/frame-end env)))
                   (setq env (lexenv/bind-vars env (list var) context))
                   (context/maybe-emit-make-cell
                    context (cmp:var-info var env))
                   (assemble-maybe-long context +set+ frame-start))))))
      (compile-progn body
                     (if specials
                         ;; We do this to make sure special declarations get
                         ;; through even if this form doesn't bind them.
                         ;; This creates duplicate alist entries for anything
                         ;; that _is_ bound here, but that's not a big deal.
                         (lexenv/add-specials env specials)
                         env)
                     context)
      (context/emit-unbind context special-binding-count))))

(defun compile-setq (pairs env context)
  (if (null pairs)
      (unless (eql (cmp:context/receiving context) 0)
        (assemble context +nil+))
      (do ((pairs pairs (cddr pairs)))
          ((endp pairs))
        (let ((var (car pairs))
              (valf (cadr pairs))
              (rest (cddr pairs)))
          (compile-setq-1 var valf env
                          (if rest
                              (context/sub context 0)
                              context))))))

(defun compile-setq-1 (var valf env context)
  (let ((info (cmp:var-info var env)))
    (cond
      ((typep info 'cmp:symbol-macro-var-info)
       (let ((expansion
               (funcall *macroexpand-hook*
                        (cmp:symbol-macro-var-info/expander info)
                        var env)))
         (compile-form `(setf ,expansion ,valf) env context)))
      ((typep info '(or null cmp:special-var-info))
       (when (null info)
         (warn "Unknown variable ~a: treating as special" var))
       (compile-form valf env (context/sub context 1))
       ;; If we need to return the new value, stick it into a new local
       ;; variable, do the set, then return the lexical variable.
       ;; We can't just read from the special, since some other thread may
       ;; alter it.
       (let ((index (cmp:lexenv/frame-end env)))
         (unless (eql (cmp:context/receiving context) 0)
           (assemble-maybe-long context +set+ index)
           (assemble-maybe-long context +ref+ index)
           ;; called for effect, i.e. to keep frame size correct
           (lexenv/bind-vars env (list var) context))
         (assemble-maybe-long context +symbol-value-set+ (context/literal-index context var))
         (unless (eql (cmp:context/receiving context) 0)
           (assemble-maybe-long context +ref+ index)
           (when (eql (cmp:context/receiving context) t)
             (assemble context +pop+)))))
      ((typep info 'cmp:lexical-var-info)
       (let ((localp (eq (cmp:lexical-var-info/cfunction info)
                         (cmp:context/cfunction context)))
             (index (cmp:lexenv/frame-end env)))
         (unless localp
           (setf (cmp:lexical-var-info/closed-over-p info) t))
         (setf (cmp:lexical-var-info/set-p info) t)
         (compile-form valf env (context/sub context 1))
         ;; similar concerns to specials above.
         (unless (eql (cmp:context/receiving context) 0)
           (assemble-maybe-long context +set+ index)
           (assemble-maybe-long context +ref+ index)
           (lexenv/bind-vars env (list var) context))
         (cond (localp
                (context/emit-lexical-set context info))
               ;; Don't emit a fixup if we already know we need a cell.
               (t
                (assemble-maybe-long context +closure+ (context/closure-index context info))
                (assemble context +cell-set+)))
         (unless (eql (cmp:context/receiving context) 0)
           (assemble-maybe-long context +ref+ index)
           (when (eql (cmp:context/receiving context) t)
             (assemble context +pop+)))))
      (t (error "BUG: Unknown var info ~a" info)))))

(defun compile-flet (definitions body env context)
  (let ((fun-vars '())
        (funs '())
        (fun-count 0)
        ;; HACK FIXME
        (frame-slot (cmp:lexenv/frame-end env)))
    (dolist (definition definitions)
      (let ((name (first definition))
            (fun-var (gensym "FLET-FUN")))
        (compile-function `(lambda ,(second definition)
                             (block ,(core:function-block-name name)
                               (locally ,@(cddr definition))))
                          env (context/sub context 1))
        (push fun-var fun-vars)
        (push (cons name (cmp:local-fun-info/make
                          (cmp:lexical-var-info/make
                           frame-slot
                           (cmp:context/cfunction context))))
              funs)
        (incf frame-slot)
        (incf fun-count)))
    (context/emit-bind context fun-count (cmp:lexenv/frame-end env))
    (let ((env (make-lexical-environment
                (lexenv/bind-vars env fun-vars context)
                :funs (append funs (cmp:lexenv/funs env)))))
      (compile-locally body env context))))

(defun compile-labels (definitions body env context)
  (let ((fun-count 0)
        (funs '())
        (fun-vars '())
        (closures '())
        (env env)
        (frame-start (cmp:lexenv/frame-end env))
        (frame-slot (cmp:lexenv/frame-end env)))
    (dolist (definition definitions)
      (let ((name (first definition))
            (fun-var (gensym "LABELS-FUN")))
        (push fun-var fun-vars)
        (push (cons name (cmp:local-fun-info/make
                          (cmp:lexical-var-info/make
                           frame-slot
                           (cmp:context/cfunction context))))
              funs)
        (incf frame-slot)
        (incf fun-count)))
    (let ((frame-slot (cmp:lexenv/frame-end env))
          (env (make-lexical-environment
                (lexenv/bind-vars env fun-vars context)
                :funs (append funs (cmp:lexenv/funs env)))))
      (dolist (definition definitions)
        (let* ((name (first definition))
               (fun (compile-lambda (second definition)
                                    `((block ,(core:function-block-name name)
                                        (locally ,@(cddr definition))))
                                    env
                                    (context-module context)))
               (literal-index (context/literal-index context fun)))
          (cond ((zerop (length (cmp:cfunction/closed fun)))
                 (assemble-maybe-long context +const+ literal-index))
                (t
                 (push (cons fun frame-slot) closures)
                 (assemble-maybe-long context
                                      +make-uninitialized-closure+ literal-index))))
        (incf frame-slot))
      (context/emit-bind context fun-count frame-start)
      (dolist (closure closures)
        (dotimes (i (length (cmp:cfunction/closed (car closure))))
          (reference-lexical-info (aref (cmp:cfunction/closed (car closure)) i)
                                  context))
        (assemble-maybe-long context +initialize-closure+ (cdr closure)))
      (compile-progn body env context))))

(defun compile-if (condition then else env context)
  (compile-form condition env (context/sub context 1))
  (let ((then-label (cmp:label/make))
        (done-label (cmp:label/make)))
    (context/emit-jump-if context then-label)
    (compile-form else env context)
    (context/emit-jump context done-label)
    (label/contextualize then-label context)
    (compile-form then env context)
    (label/contextualize done-label context)))

;;; Push the immutable value or cell of lexical in CONTEXT.
(defun reference-lexical-info (info context)
  (if (eq (cmp:lexical-var-info/cfunction info)
          (cmp:context/cfunction context))
      (assemble-maybe-long context +ref+
                           (cmp:lexical-var-info/frame-index info))
      (assemble-maybe-long context +closure+ (context/closure-index context info))))

(defun compile-function (fnameoid env context)
  (unless (eql (cmp:context/receiving context) 0)
    (if (typep fnameoid 'lambda-expression)
        (let* ((cfunction (compile-lambda (cadr fnameoid) (cddr fnameoid)
                                          env (context-module context)))
               (closed (cmp:cfunction/closed cfunction)))
          (dotimes (i (length closed))
            (reference-lexical-info (aref closed i) context))
          (if (zerop (length closed))
              (assemble-maybe-long context
                                   +const+ (context/literal-index context cfunction))
              (assemble-maybe-long context +make-closure+
                                   (context/literal-index context cfunction))))
        (let ((info (cmp:fun-info fnameoid env)))
          (cond
            ((typep info '(or cmp:global-fun-info null))
             #-(or clasp-min aclasp bclasp)
             (when (null info) (warn "Unknown function ~a" fnameoid))
             (assemble-maybe-long context +fdefinition+
                                  (context/literal-index context fnameoid)))
            ((typep info 'cmp:local-fun-info)
             (reference-lexical-info
              (cmp:local-fun-info/fun-var info) context))
            (t (error "BUG: Unknown fun info ~a" info)))))
    (when (eql (cmp:context/receiving context) t)
      (assemble context +pop+))))

;;; (list (car list) (car (FUNC list)) (car (FUNC (FUNC list))) ...)
(defun collect-by (func list)
  (let ((col nil))
    (do ((list list (funcall func list)))
        ((endp list) (nreverse col))
      (push (car list) col))))

(defun compile-with-lambda-list (lambda-list body env context)
  (multiple-value-bind (decls body docs specials)
      (core:process-declarations body t)
    (declare (ignore docs decls))
    (multiple-value-bind (required optionals rest key-flag keys aok-p aux)
        (core:process-lambda-list lambda-list 'function)
      (let* ((function (cmp:context/cfunction context))
             (entry-point (cmp:cfunction/entry-point function))
             (min-count (first required))
             (optional-count (first optionals))
             (max-count (+ min-count optional-count))
             (key-count (first keys))
             (more-p (or rest key-flag))
             (new-env (lexenv/bind-vars env (cdr required) context))
             (special-binding-count 0)
             ;; An alist from optional and key variables to their local indices.
             ;; This is needed so that we can properly mark any that are special as
             ;; such while leaving them temporarily "lexically" bound during
             ;; argument parsing.
             (opt-key-indices nil))
        (label/contextualize entry-point context)
        ;; Generate argument count check.
        (cond ((and (> min-count 0) (= min-count max-count) (not more-p))
               (assemble-maybe-long context +check-arg-count=+ min-count))
              (t
               (when (> min-count 0)
                 (assemble-maybe-long context +check-arg-count>=+ min-count))
               (unless more-p
                 (assemble-maybe-long context +check-arg-count<=+ max-count))))
        (unless (zerop min-count)
          ;; Bind the required arguments.
          (assemble-maybe-long context +bind-required-args+ min-count)
          (dolist (var (cdr required))
            ;; We account for special declarations in outer environments/globally
            ;; by checking the original environment - not our new one - for info.
            (cond ((or (member var specials)
                       (typep (cmp:var-info var env)
                              'cmp:special-var-info))
                   (let ((info (cmp:var-info var new-env)))
                     (assemble-maybe-long
                      context +ref+
                      (cmp:lexical-var-info/frame-index info)))
                   (context/emit-special-bind context var))
                  (t
                   (context/maybe-emit-encage
                    context (cmp:var-info var new-env)))))
          (setq new-env (lexenv/add-specials new-env (intersection specials (cdr required)))))
        (unless (zerop optional-count)
          ;; Generate code to bind the provided optional args; unprovided args will
          ;; be initialized with the unbound marker.
          (assemble-maybe-long context +bind-optional-args+ min-count optional-count)
          (let ((optvars (collect-by #'cdddr (cdr optionals))))
            ;; Mark the locations of each optional. Note that we do this even if
            ;; the variable will be specially bound.
            (setq new-env (lexenv/bind-vars new-env optvars context))
            ;; Add everything to opt-key-indices.
            (dolist (var optvars)
              (let ((info (cmp:var-info var new-env)))
                (push (cons var (cmp:lexical-var-info/frame-index info))
                      opt-key-indices)))
            ;; Re-mark anything that's special in the outer context as such, so that
            ;; default initforms properly treat them as special.
            (let ((specials (remove-if-not
                             (lambda (sym)
                               (typep (cmp:var-info sym env)
                                      'cmp:special-var-info))
                             optvars)))
              (when specials
                (setq new-env (lexenv/add-specials new-env specials))))))
        (when key-flag
          ;; Generate code to parse the key args. As with optionals, we don't do
          ;; defaulting yet.
          (let ((key-names (collect-by #'cddddr (cdr keys))))
            (context/emit-parse-key-args
             context max-count key-count
             (context/literal-index context (first key-names))
             (lexenv/frame-end new-env) aok-p)
            ;; emit-parse-key-args establishes the first key in the literals.
            ;; now do the rest.
            (dolist (key-name (rest key-names))
              (context/literal-index context key-name)))
          (let ((keyvars (collect-by #'cddddr (cddr keys))))
            (setq new-env (lexenv/bind-vars new-env keyvars context))
            (dolist (var keyvars)
              (let ((info (cmp:var-info var new-env)))
                (push (cons var (cmp:lexical-var-info/frame-index info))
                      opt-key-indices)))
            (let ((specials (remove-if-not
                             (lambda (sym)
                               (typep (cmp:var-info sym env)
                                      'cmp:special-var-info))
                             keyvars)))
              (when specials
                (setq new-env (lexenv/add-specials new-env specials))))))
        ;; Generate defaulting code for optional args, and special-bind them
        ;; if necessary.
        (unless (zerop optional-count)
          (do ((optionals (cdr optionals) (cdddr optionals))
               (optional-label (cmp:label/make) next-optional-label)
               (next-optional-label (cmp:label/make) (cmp:label/make)))
              ((endp optionals) (label/contextualize optional-label context))
            (label/contextualize optional-label context)
            (let* ((optional-var (car optionals))
                   (defaulting-form (cadr optionals)) (supplied-var (caddr optionals))
                   (optional-special-p
                     (or (member optional-var specials)
                         (typep (cmp:var-info optional-var env)
                                'cmp:special-var-info)))
                   (index (cdr (assoc optional-var opt-key-indices)))
                   (supplied-special-p
                     (and supplied-var
                          (or (member supplied-var specials)
                              (typep (cmp:var-info supplied-var env)
                                     'cmp:special-var-info)))))
              (setq new-env
                    (compile-optional/key-item optional-var defaulting-form index
                                               supplied-var next-optional-label
                                               optional-special-p supplied-special-p
                                               context new-env))
              (when optional-special-p (incf special-binding-count))
              (when supplied-special-p (incf special-binding-count)))))
        ;; &rest
        (when rest
          (assemble-maybe-long context +listify-rest-args+ max-count)
          (assemble-maybe-long context +set+ (frame-end new-env))
          (setq new-env (lexenv/bind-vars new-env (list rest) context))
          (cond ((or (member rest specials)
                     (typep (cmp:var-info rest env)
                            'cmp:special-var-info))
                 (assemble-maybe-long
                  +ref+ (cmp:var-info rest new-env) context)
                 (context/emit-special-bind context rest)
                 (incf special-binding-count 1)
                 (setq new-env (lexenv/add-specials new-env (list rest))))
                (t (context/maybe-emit-encage
                    context (cmp:var-info rest new-env)))))
        ;; Generate defaulting code for key args, and special-bind them if necessary.
        (when key-flag
          (do ((keys (cdr keys) (cddddr keys))
               (key-label (cmp:label/make) next-key-label)
               (next-key-label (cmp:label/make) (cmp:label/make)))
              ((endp keys) (label/contextualize key-label context))
            (label/contextualize key-label context)
            (let* ((key-var (cadr keys)) (defaulting-form (caddr keys))
                   (index (cdr (assoc key-var opt-key-indices)))
                   (supplied-var (cadddr keys))
                   (key-special-p
                     (or (member key-var specials)
                         (typep (cmp:var-info key-var env)
                                'cmp:special-var-info)))
                   (supplied-special-p
                     (and supplied-var
                          (or (member supplied-var specials)
                              (typep (cmp:var-info key-var env)
                                     'cmp:special-var-info)))))
              (setq new-env
                    (compile-optional/key-item key-var defaulting-form index
                                               supplied-var next-key-label
                                               key-special-p supplied-special-p
                                               context new-env))
              (when key-special-p (incf special-binding-count))
              (when supplied-special-p (incf special-binding-count)))))
        ;; Generate aux and the body as a let*.
        ;; We have to convert from process-lambda-list's aux format
        ;; (var val var val) to let* bindings.
        ;; We repeat the special declarations so that let* will know the auxs
        ;; are special, and so that any free special declarations are processed.
        (let ((bindings nil))
          (do ((aux aux (cddr aux)))
              ((endp aux)
               (compile-let* (nreverse bindings)
                             `((declare (special ,@specials)) ,@body)
                             new-env context))
            (push (list (car aux) (cadr aux)) bindings)))
        ;; Finally, clean up any special bindings.
        (context/emit-unbind context special-binding-count)))))

;;; Compile an optional/key item and return the resulting environment.
(defun compile-optional/key-item (var defaulting-form var-index supplied-var next-label
                                  var-specialp supplied-specialp context env)
  (flet ((default (suppliedp specialp var info)
           (cond (suppliedp
                  (cond (specialp
                         (assemble-maybe-long context +ref+ var-index)
                         (context/emit-special-bind context var))
                        (t
                         (context/maybe-emit-encage context info))))
                 (t
                  (compile-form defaulting-form env
                                (context/sub context 1))
                  (cond (specialp
                         (context/emit-special-bind context var))
                        (t
                         (context/maybe-emit-make-cell context info)
                         (assemble-maybe-long context +set+ var-index))))))
         (supply (suppliedp specialp var info)
           (if suppliedp
               (compile-literal t env (context/sub context 1))
               (assemble context +nil+))
           (cond (specialp
                  (context/emit-special-bind context var))
                 (t
                  (context/maybe-emit-make-cell context info)
                  (assemble-maybe-long
                   context +set+
                   (cmp:lexical-var-info/frame-index info))))))
    (let ((supplied-label (cmp:label/make))
          (varinfo (cmp:var-info var env)))
      (when supplied-var
        (setq env (lexenv/bind-vars env (list supplied-var) context)))
      (let ((supplied-info (cmp:var-info supplied-var env)))
        (context/emit-jump-if-supplied context supplied-label var-index)
        (default nil var-specialp var varinfo)
        (when supplied-var
          (supply nil supplied-specialp supplied-var supplied-info))
        (context/emit-jump context next-label)
        (label/contextualize supplied-label context)
        (default t var-specialp var varinfo)
        (when supplied-var
          (supply t supplied-specialp supplied-var supplied-info))
        (when var-specialp
          (setq env (lexenv/add-specials env (list var))))
        (when supplied-specialp
          (setq env (lexenv/add-specials env (list supplied-var))))
        env))))

;;; Compile the lambda in MODULE, returning the resulting
;;; CFUNCTION.
(defun compile-lambda (lambda-list body env module)
  (multiple-value-bind (decls sub-body docs)
      (core:process-declarations body t)
    ;; we pass the original body w/declarations to compile-with-lambda-list
    ;; so that it can do its own special handling.
    (declare (ignore sub-body))
    (let* ((name (or (core:extract-lambda-name-from-declares decls)
                     `(lambda ,(lambda-list-for-name lambda-list))))
           (function (cmp:cfunction/make module name docs lambda-list))
           (context (cmp:context/make t function))
           (env (make-lexical-environment env :frame-end 0)))
      (cmp:cfunction/setf-index
       function
       (vector-push-extend function
                           (cmp:module/cfunctions module)))
      (compile-with-lambda-list lambda-list body env context)
      (assemble context +return+)
      function)))

(defun go-tag-p (object) (typep object '(or symbol integer)))

(defun compile-tagbody (statements env context)
  (let* ((new-tags (cmp:lexenv/tags env))
         (tagbody-dynenv (gensym "TAG-DYNENV"))
         (env (lexenv/bind-vars env (list tagbody-dynenv) context))
         (dynenv-info (cmp:var-info tagbody-dynenv env)))
    (dolist (statement statements)
      (when (go-tag-p statement)
        (push (list* statement dynenv-info (cmp:label/make))
              new-tags)))
    (let ((env (make-lexical-environment env :tags new-tags)))
      ;; Bind the dynamic environment.
      (assemble context +entry+ (cmp:lexical-var-info/frame-index dynenv-info))
      ;; Compile the body, emitting the tag destination labels.
      (dolist (statement statements)
        (if (go-tag-p statement)
            (label/contextualize (cddr (assoc statement (cmp:lexenv/tags env))) context)
            (compile-form statement env (context/sub context 0))))))
  (assemble context +entry-close+)
  ;; return nil if we really have to
  (unless (eql (cmp:context/receiving context) 0)
    (assemble context +nil+)
    (when (eql (cmp:context/receiving context) t)
      (assemble context +pop+))))

(defun compile-go (tag env context)
  (let ((pair (assoc tag (cmp:lexenv/tags env))))
    (if pair
        (destructuring-bind (dynenv-info . tag-label) (cdr pair)
          (reference-lexical-info dynenv-info context)
          (context/emit-exit context tag-label))
        (error "The GO tag ~a does not exist." tag))))

(defun compile-block (name body env context)
  (let* ((block-dynenv (gensym "BLOCK-DYNENV"))
         (env (lexenv/bind-vars env (list block-dynenv) context))
         (dynenv-info (cmp:var-info block-dynenv env))
         (label (cmp:label/make))
         (normal-label (cmp:label/make)))
    ;; Bind the dynamic environment.
    (assemble context +entry+ (cmp:lexical-var-info/frame-index dynenv-info))
    (let ((env (make-lexical-environment
                env
                :blocks (acons name (cons dynenv-info label) (cmp:lexenv/blocks env)))))
      ;; Force single values into multiple so that we can uniformly PUSH afterward.
      (compile-progn body env context))
    (when (eql (cmp:context/receiving context) 1)
      (context/emit-jump context normal-label))
    (label/contextualize label context)
    ;; When we need 1 value, we have to make sure that the
    ;; "exceptional" case pushes a single value onto the stack.
    (when (eql (cmp:context/receiving context) 1)
      (assemble context +push+)
      (label/contextualize normal-label context))
    (assemble context +entry-close+)))

(defun compile-return-from (name value env context)
  (compile-form value env (context/sub context t))
  (let ((pair (assoc name (cmp:lexenv/blocks env))))
    (if pair
        (destructuring-bind (dynenv-info . block-label) (cdr pair)
          (reference-lexical-info dynenv-info context)
          (context/emit-exit context block-label))
        (error "The block ~a does not exist." name))))

(defun compile-catch (tag body env context)
  (compile-form tag env (context/sub context 1))
  (let ((target (cmp:label/make)))
    (context/emit-catch context target)
    (compile-progn body env context)
    (assemble context +catch-close+)
    (label/contextualize target context)))

(defun compile-throw (tag result env context)
  (compile-form tag env (context/sub context 1))
  (compile-form result env (context/sub context t))
  (assemble context +throw+))

(defun compile-progv (symbols values body env context)
  (compile-form symbols env (context/sub context 1))
  (compile-form values env (context/sub context 1))
  (assemble context +progv+)
  (compile-progn body env context)
  (context/emit-unbind context 1))

(defun compile-symbol-macrolet (bindings body env context)
  (let ((smacros nil))
    (dolist (binding bindings)
      (push (cons (car binding) (make-symbol-macro-var-info (cadr binding)))
            smacros))
    (compile-locally body (make-lexical-environment
                           env
                           :vars (append (nreverse smacros) (cmp:lexenv/vars env)))
                     context)))

#+clasp
(defun compile-macrolet (bindings body env context)
  (let ((macros nil))
    (dolist (binding bindings)
      (let* ((name (car binding)) (lambda-list (cadr binding))
             (body (cddr binding))
             (eform (ext:parse-macro name lambda-list body env))
             (aenv (lexenv/macroexpansion-environment env))
             (expander (bytecompile eform aenv))
             (info (cmp:local-macro-info/make expander)))
        (push (cons name info) macros)))
    (compile-locally body (make-lexical-environment
                           env :funs (append macros (cmp:lexenv/funs env)))
                     context)))

(defun compile-multiple-value-call (function-form forms env context)
  (compile-form function-form env (context/sub context 1))
  (let ((first (first forms))
        (rest (rest forms)))
    (compile-form first env (context/sub context t))
    (when rest
      (assemble context +push-values+)
      (dolist (form rest)
        (compile-form form env (context/sub context t))
        (assemble context +append-values+))
      (assemble context +pop-values+)))
  (context/emit-mv-call context))

(defun compile-multiple-value-prog1 (first-form forms env context)
  (compile-form first-form env context)
  (unless (member (cmp:context/receiving context) '(0 1))
    (assemble context +push-values+))
  (dolist (form forms)
    (compile-form form env (context/sub context 0)))
  (unless (member (cmp:context/receiving context) '(0 1))
    (assemble context +pop-values+)))
