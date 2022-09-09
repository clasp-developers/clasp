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

(defun make-lexical-environment (parent &key (vars (cmp:lexenv/vars parent))
                                          (tags (cmp:lexenv/tags parent))
                                          (blocks (cmp:lexenv/blocks parent))
                                          (frame-end (cmp:lexenv/frame-end parent))
                                          (funs (cmp:lexenv/funs parent)))
  (cmp:lexenv/make vars tags blocks funs frame-end))

(deftype lambda-expression () '(cons (eql lambda) (cons list list)))
#+(or)
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

(defun compile-load-time-value (form env context)
  (if *generate-compile-file-load-time-values*
      (error "Handle compile-file")
      (let ((value (eval form)))
        (compile-literal value env context))))

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
            (cond ((special-binding-p var specials env)
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
             (context/new-literal-index context (first key-names))
             (lexenv/frame-end new-env) aok-p)
            ;; emit-parse-key-args establishes the first key in the literals.
            ;; now do the rest.
            (dolist (key-name (rest key-names))
              (context/new-literal-index context key-name)))
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
                     (special-binding-p optional-var specials env))
                   (index (cdr (assoc optional-var opt-key-indices)))
                   (supplied-special-p
                     (and supplied-var
                          (special-binding-p supplied-var specials env))))
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
          (cond ((special-binding-p rest specials env)
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
                     (special-binding-p key-var specials env))
                   (supplied-special-p
                     (and supplied-var
                          (special-binding-p supplied-var specials env))))
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
