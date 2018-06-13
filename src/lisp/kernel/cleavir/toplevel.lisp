(in-package :clasp-cleavir)

(defun function-name-p (name)
  (or (symbolp name)
      (and (consp name)
           (eq (car name) 'setf)
           (consp (cdr name))
           (null (cddr name)))))

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


(defgeneric cclasp-eval-with-env (form env))

(defun run-thunk (thunk)
  (funcall thunk))

(defmethod cclasp-eval-with-env (form env)
;;  (format t "cclasp-eval eval: ~a~%" form)
  (flet ((eval-progn (body &optional (penv env))
           (loop for (form . next) on body
              if next
              do (cclasp-eval form penv)
              else
              return (cclasp-eval form penv)))
         (eval-compile (form)
           (let ((thunk (cclasp-compile-in-env nil
                                           ;; PROGN is needed to avoid processing DECLARE as a declaration
                                               `(lambda () (progn ,form)) env)))
             (run-thunk thunk))))
    (let ((form (macroexpand form env)))
      (typecase form
        (symbol
         (symbol-value form))
        (atom
         form)
        (cons
         (let* ((name (car form))
                (body (cdr form))
                (arg-length (length body)))
           (cond ((not (symbolp name)) ; lambda form (or invalid)
                  (eval-compile form))
                 ((and (fboundp name)
                       (not (special-operator-p name)))
                  (apply name (loop for arg in body
                                 collect (cclasp-eval arg env))))
                 (t
                  (case name
                    (quote
                     (assert (= arg-length 1))
                     (car body))
                    (function
                     (assert (= arg-length 1))
                     (let ((name (car body)))
                       (if (function-name-p name)
                           (fdefinition name)
                           (eval-compile form))))
                    (progn
                      (eval-progn body))
                    (eval-when
                        (assert (listp (car body)))
                      (when (or (member :execute (car body))
                                (member 'eval (car body)))
                        (eval-progn (cdr body))))
                    (locally
                        (multiple-value-bind (decls body) (core:process-declarations body nil)
                          (eval-progn body (augment-environment-with-declares decls env))))
                    (t (eval-compile form)))))))))))

(defmethod cclasp-eval-with-env (form (env core:value-frame))
  (cclasp-eval-with-env form (core:get-parent-environment env)))

(defun cclasp-eval (form &optional env)
  (cclasp-eval-with-env form env))
