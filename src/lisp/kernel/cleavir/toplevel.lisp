(in-package :clasp-cleavir)

(defun augment-environment-with-declares (declares &optional env)
  ;; FIXME: Cleavir should export some interface.
  (cleavir-generate-ast::augment-environment-with-declarations
   env
   (cleavir-generate-ast::canonicalize-declarations (list (cons 'declare declares)) env)))

(defun augment-environment-with-macrolet (macros env)
  (mapc (lambda (macro)
          (setq env (cleavir-environment:add-local-macro env (car macro) (cdr macro))))
        macros)
  env)

(defun augment-environment-with-symbol-macrolet (macros env)
  (mapc (lambda (macro)
          (setq env (cleavir-environment:add-local-symbol-macro env (car macro) (cdr macro))))
        macros)
  env)

;;; Attempts to evaluate the given form in the given environment.
;;; On complex enough forms, it will call DEFAULT on the form and
;;; environment instead.
;;; The idea is we pick off some common cases and then fall back
;;; to a slower full evaluator, such as the compiler.
;;; FIXME: We should use this in bclasp, maybe?
(defun simple-eval (form env default)
  (labels ((recurse (form env)
             (simple-eval form env default))
           (map-recurse (forms env)
             (mapcar (lambda (form) (recurse form env)) forms))
           (eval-progn (forms env)
             (loop for (form . next) on forms
                   if next do (recurse form env)
                     else return (recurse form env)))
           (default (form env)
             (funcall default form env)))
    (let ((form (macroexpand form env)))
      (typecase form
        (symbol (symbol-value form))
        (cons
         (let* ((op (car form)) (args (cdr form))
                (arg-length (length args)))
           (cond
             ;; lambda form or invalid
             ((not (symbolp op))
              (default form env))
             ;; function call
             ((and (fboundp op)
                   (not (special-operator-p op)))
              (apply op (map-recurse args env)))
             ;; operation
             (t
              (case op
                ((quote)
                 (assert (= arg-length 1))
                 (first args))
                ((function)
                 (assert (= arg-length 1))
                 (let ((name (first args)))
                   (if (core:valid-function-name-p name)
                       (fdefinition name)
                       (default form env))))
                ((if)
                 (destructuring-bind (condition then &optional else) args
                   (recurse (if (recurse condition env) then else) env)))
                ((setq)
                 (when (oddp arg-length)
                   (error "Expected an even number of arguments for setq"))
                 (loop with result = nil
                       for (symbol expression) on args by #'cddr
                       for value = (recurse expression env)
                       do (setf result
                                (if (ext:specialp symbol)
                                    ;; special variable set
                                    (setf (symbol-value symbol) value)
                                    ;; symbol macro, or invalid, or who knows?
                                    (default `(setq ,symbol ',value) env)))
                       finally (return result)))
                ((progn) (eval-progn args env))
                ((eval-when) ; :execute only
                 (assert (>= arg-length 1))
                 (let ((situations (first args)) (body (rest args)))
                   (assert (listp situations))
                   (when (or (member :execute situations)
                             (member 'eval situations))
                     (eval-progn body env))))
                ((locally)
                 (multiple-value-bind (decls body) (core:process-declarations args nil)
                   (eval-progn body (augment-environment-with-declares decls env))))
                ((catch)
                 (assert (>= arg-length 1))
                 (let ((tag (first args)) (body (rest args)))
                   (catch (recurse tag env) (eval-progn body env))))
                ((throw)
                 (assert (= arg-length 2))
                 (let ((tag (first args)) (form (second args)))
                   (throw (recurse tag env) (recurse form env))))
                ((progv)
                 (assert (>= arg-length 2))
                 (let ((symbols (first args)) (values (second args)) (body (cddr args)))
                   (progv (recurse symbols env) (recurse values env)
                     (eval-progn body env))))
                ((unwind-protect)
                 (assert (>= arg-length 1))
                 (let ((protected (first args)) (cleanup (rest args)))
                   (unwind-protect (recurse protected env)
                     (eval-progn cleanup env))))
                (t ; unknown
                 (default form env)))))))
        ;; constant
        (t form)))))

(defgeneric cclasp-eval-with-env (form env))

(defmethod cclasp-eval-with-env (form env)
  (funcall
   (cclasp-compile-in-env nil
                          ;; PROGN is needed to avoid processing declarations
                          `(lambda () (progn ,form))
                          env)))

(defmethod cclasp-eval-with-env (form (env core:value-frame))
  (cclasp-eval-with-env form (core:get-parent-environment env)))

(defun cclasp-eval (form &optional env)
  (simple-eval form env #'cclasp-eval-with-env))
