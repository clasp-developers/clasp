(defpackage #:env-eval
  (:use #:cl))

(in-package #:env-eval)

(defun ltv-evaluator (env)
  (lambda (form)
    (cond ((constantp form) ;; FIXME: constant form value would be good.
           (eval form))
          ;; treated specially for the cicularity ending reasons verbosely explained
          ;; around the cleavir-generate-ast methods.
          ((and (consp form)
                (member (first form) '(sicl-genv:function-cell sicl-genv:variable-cell
                                       sicl-genv:variable-unbound)))
           ;; we have a form like (variable-cell '*print-circle* *global-environment*)
           ;; the second form should always be a quoted symbol, at least in the forms
           ;; produced by us.
           (funcall (first form) (eval (second form)) env))
          (t (error "Weird load-time-value: ~a" form)))))

(defun eval-fallback (form env)
  (let* ((wrapped `(lambda () ,form)) ; could skip if form is a lambda expression already
         (ast (cleavir-generate-ast:generate-ast wrapped env nil))
         (toplevel-ast (cleavir-ast-transformations:hoist-load-time-value ast))
         (ltv-forms (cleavir-ast:forms toplevel-ast))
         (reform (fake-ast:ast->form ast))
         (ltvs (mapcar (ltv-evaluator env) forms))
         (function (compile nil reform)))
    (apply function ltvs)))

(defun eval-in-env (form environment)
  ;; This is supposed to work with genvs as well as entries.
  ;; With entries, we get macros from them, but skip to the global for variable/function bindings.
  ;; Entries can't have any actual _bindings_, after all, just descriptions.
  ;; And this function should only be called with entries with no lexical bindings etc.,
  ;; toplevel forms, as it were.
  (let ((global (cleavir-env:global-environment environment)))
    (labels ((mexpand-1 (form env)
               (typecase form
                 (symbol (multiple-value-bind (expander expansion)
                             (sicl-genv:symbol-macro form env)
                           (if expander
                               (values expansion t)
                               (values form nil))))
                 (cons (if (symbolp (first form))
                           (let ((mf (cleavir-env:macro-function (first form) env)))
                             (if mf
                                 ;; FIXME: should be genv's mehook, probably.
                                 (values (funcall *macroexpand-hook* mf form env) t)
                                 (values form nil)))
                           (values form nil)))
                 (t (values form nil))))
             (mexpand (form env)
               (loop with ever-expanded = nil
                     do (multiple-value-bind (expansion expanded)
                            (mexpand-1 form env)
                          (if expanded
                              (setf ever-expanded t form expansion)
                              (return-from mexpand (values form ever-expanded))))))
             (subeval (form)
               (eval-in-env form environment)))
      (let ((form (mexpand form environment)))
        (typecase form
          (symbol
           (let ((info (cleavir-env:variable-info environment form)))
             (etypecase info
               (cleavir-env:constant-variable-info
                (values (sicl-genv:constant-variable form global)))
               (cleavir-env:special-variable-info
                (symbol-value-from-cell
                 form (sicl-genv:variable-cell form global)
                 (sicl-genv:variable-unbound form global))))))
          (cons
           (if (symbolp (first form))
               (if (sicl-genv:special-operator (first form) global)
                   (eval-fallback form environment)
                   (apply (car (sicl-genv:function-cell (first form) global))
                          (mapcar #'subeval (rest form))))
               (eval-fallback form environment)))
          (t form))))))
