(in-package #:cmp)

;;;; Bclasp top-level-form processor.

;;; t1expr is called by compile-file. Eventually this file will call
;;; compile-thunk (in codegen.lsp) to do the actual compiling.

(defun compile-top-level (form)
  (literal:with-top-level-form
   (dbg-set-current-source-pos form)
    (compile-thunk 'repl form nil t)
    ;; After this literals are codegen'd into the RUN-ALL function
    ;; by with-top-level-form
    ))

(defun t1progn (rest env)
  "All forms in progn at top level are top level forms"
  (dolist (form rest)
    (t1expr form env)))

(defun t1eval-when (rest env)
  (let ((situations (car rest))
	(body (cdr rest)))
    (when (or (member 'cl:compile situations) (member :compile-toplevel situations))
      (cmp-log "Performing eval-when :compile-toplevel side-effects%N")
      (cmp-log "Evaluating: %s%N" body)
      (funcall core:*eval-with-env-hook* `(progn ,@body) env)
      (cmp-log "Done eval-when compile-toplevel side-effects%N"))
    (when (or (member 'cl:load situations) (member :load-toplevel situations))
      (cmp-log "Compiling body due to :load-toplevel --> %s%N" body)
      ;; Each subform is a top-level form
      (dolist (subform body)
	(t1expr subform env))
      (cmp-log "Done compiling body due to :load-toplevel%N"))))

(defun t1locally (rest env)
  (multiple-value-bind (declares code docstring specials)
      (process-declarations rest nil)
    (declare (ignore specials docstring))
    ;; TODO: Do something with the declares!!!!!  They should be put into the environment
    (t1progn code (augment-environment-with-declares env declares))))

(defun parse-macrolet (body)
  (let ((macros (mapcar (lambda (macro-def)
                        (let ((macro-fn (eval (ext:parse-macro (car macro-def)
                                                               (cadr macro-def)
                                                               (cddr macro-def)))))
;;;                          (set-kind macro-fn :macro)
                          (cons (car macro-def) macro-fn)))
                      (car body))))
    (multiple-value-bind (decls macrolet-body)
        (core:process-declarations (cdr body) nil)
      (values macros decls macrolet-body))))

(defun parse-symbol-macrolet (body)
  (let ((macros (mapcar (lambda (macro-def)
                          (cons (car macro-def)
                                (constantly (cadr macro-def))))
                             (car body))))
    (multiple-value-bind (decls symbol-macrolet-body)
        (core:process-declarations (cdr body) nil)
      (values macros decls symbol-macrolet-body))))

(export '(parse-macrolet parse-symbol-macrolet))


(defun t1macrolet (rest env)
  (multiple-value-bind (macros declares body)
      (parse-macrolet rest)
    (let ((macro-env (irc-new-macrolet-environment env)))
      (mapc (lambda (macro)
              (core:add-macro macro-env (car macro) (cdr macro)))
            macros)
      (t1progn body (augment-environment-with-declares macro-env declares)))))

(defun t1symbol-macrolet (rest env)
  (multiple-value-bind (macros declares body)
      (parse-symbol-macrolet rest)
    (let ((macro-env (irc-new-symbol-macrolet-environment env)))
      (mapc (lambda (macro)
              (core:add-symbol-macro macro-env (car macro) (cdr macro)))
            macros)
      (t1progn body (augment-environment-with-declares macro-env declares)))))


(defun t1expr (form &optional env)
  (cmp-log "t1expr-> %s%N" form)
  (push form core:*top-level-form-stack*)
  (unwind-protect
       (let ((head (if (atom form) form (car form))))
         (cond
           ((eq head 'cl:eval-when) (t1eval-when (cdr form) env))
           ((eq head 'cl:progn) (t1progn (cdr form) env))
           ((eq head 'cl:locally) (t1locally (cdr form) env))
           ((eq head 'cl:macrolet) (t1macrolet (cdr form) env))
           ((eq head 'cl:symbol-macrolet) (t1symbol-macrolet (cdr form) env))
           ((and (listp form)
                 ;;(symbolp (car form))
                 (not (core:lexical-function (car form) env))
                 (not (core:lexical-macro-function (car form) env))
                 (not (core:declared-global-notinline-p (car form)))
                 (let ((expansion (core:bclasp-compiler-macroexpand form env)))
                   (if (eq expansion form)
                       nil
                       (progn
                         (t1expr expansion env)
                         t)))))
           ((macro-function head env)
            (let ((expanded (macroexpand form env)))
              (t1expr expanded env)))
           (t (compile-top-level form))))
    (pop core:*top-level-form-stack*)))
