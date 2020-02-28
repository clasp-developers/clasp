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
;;; FIXME: Replace ASSERT with more explicit errors.
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

;;; FIXME: duplicate code
(defun simple-eval-cst (cst env default)
  (labels ((recurse (cst env)
             (simple-eval-cst cst env default))
           (map-recurse (list-cst env)
             (loop for cstl = list-cst then (cst:rest cstl)
                   until (cst:null cstl)
                   collect (recurse (cst:first cstl) env)))
           (eval-progn (list-cst env)
             (loop with result = nil
                   for cstl = list-cst
                     then (cst:rest cstl)
                   until (cst:null cstl)
                   do (setf result (recurse (cst:first cstl) env))
                   finally (return result))
             #+(or)(loop for cstl = list-cst then next
                   for cst = (cst:first cstl)
                   for next = (cst:rest cstl)
                   if (not (cst:null next))
                     do (recurse cst env)
                   else return (recurse cst env)))
           (cst-length (cst)
             (loop for cstl = cst then (cst:rest cstl)
                   until (cst:null cstl)
                   summing 1))
           (default (cst env)
             (funcall default cst env)))
    (multiple-value-bind (expansion expandedp)
        (macroexpand (cst:raw cst) env)
      (let ((cst (if expandedp
                     (cst:reconstruct expansion cst *cst-client*)
                     cst)))
        (etypecase cst
          (cst:atom-cst
           (let ((raw (cst:raw cst)))
             (if (symbolp raw)
                 (symbol-value raw)
                 ;; self-evaluating
                 raw)))
          (cst:cons-cst
           (let* ((op (cst:first cst))
                  (args (cst:rest cst))
                  (arg-length (cst-length args)))
             (if (not (typep op 'cst:atom-cst))
                 ;; lambda form or invalid
                 (default cst env)
                 (let ((op (cst:raw op)))
                   (case op
                     ((quote)
                      (assert (= arg-length 1))
                      (cst:raw (cst:first args)))
                     ((function)
                      (assert (= arg-length 1))
                      (let* ((name (cst:first args))
                             (rname (cst:raw name)))
                        (if (core:valid-function-name-p rname)
                            (fdefinition rname)
                            ;; lambda
                            (default cst env))))
                     ((if)
                      (ecase arg-length
                        ((2)
                         (when (recurse (cst:first args) env)
                           (recurse (cst:first (cst:rest args)) env)))
                        ((3)
                         (recurse (if (recurse (cst:first args) env)
                                      (cst:first (cst:rest args))
                                      (cst:first (cst:rest (cst:rest args))))
                                  env))))
                     ((setq)
                      (when (oddp arg-length)
                        (error "Expected an even number of arguments for setq"))
                      (loop with result = nil
                            for cstl = args then (cst:rest (cst:rest cstl))
                            until (cst:null cstl)
                            do (let* ((symbol-cst (cst:first cstl))
                                      (symbol (cst:raw symbol-cst))
                                      (value-cst (cst:first (cst:rest cstl)))
                                      (value (recurse value-cst env)))
                                 (setf result
                                       (if (ext:specialp symbol)
                                           (setf (symbol-value symbol) value)
                                           (default (cst:cstify
                                                     (list
                                                      (make-instance 'cst:atom-cst
                                                                   :raw 'setq)
                                                      symbol-cst value-cst))
                                                    env))))
                            finally (return result)))
                     ((progn) (eval-progn args env))
                     ((eval-when) ; :execute only
                      (assert (>= arg-length 1))
                      (let ((situations (cst:raw (cst:first args)))
                            (body (cst:rest args)))
                        (assert (listp situations))
                        (when (or (member :execute situations)
                                  (member 'eval situations))
                          (eval-progn body env))))
                     ;; locally can't be handled as easily since it parses the body.
                     ;; catch, throw, progv, unwind-protect macroexpand,
                     ;; and also handling them worse is probably okay.
                     (otherwise
                      (if (and (fboundp op) (not (special-operator-p op)))
                          (apply (fdefinition op) (map-recurse args env))
                          (default cst env)))))))))))))

(defgeneric cclasp-eval-with-env (form env))

(defmethod cclasp-eval-with-env (form env)
  (funcall
   (cclasp-compile-in-env
    ;; PROGN is needed to avoid processing declarations
    `(lambda () (progn ,form)) env)))

(defmethod cclasp-eval-with-env (form (env core:value-frame))
  (cclasp-eval-with-env form (core:get-parent-environment env)))

(defun cclasp-eval (form &optional env)
  (simple-eval form env #'cclasp-eval-with-env))
