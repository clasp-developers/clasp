(in-package #:clasp-sandbox)

(defclass sandbox-environment (sicl-simple-environment:simple-environment) ())

;;; Hooking it up with us

;;; Like cclasp-eval-with-env but pays more attention to the environment.
;;; Used to be cclasp-eval-with-env, but then when you passed a cleavir-env:entry
;;; it would use the clasp top when it shouldn't.
(defun eval-with-genv (form environment)
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
               (eval-with-genv form environment))
             (evalcompile (form env)
               (funcall (cmp:compile-in-env nil `(lambda () (progn ,form))
                                            environment cmp:*cleavir-compile-hook*
                                            'llvm-sys:external-linkage))))
      (let ((form (mexpand form environment)))
        (typecase form
          (symbol
           (core:symbol-value-from-cell
            form
            (sicl-genv:variable-cell form global)
            (sicl-genv:variable-unbound form global)))
          (cons
           (if (symbolp (first form))
               (if (sicl-genv:special-operator (first form) global)
                   (evalcompile form environment)
                   (apply (car (sicl-genv:function-cell (first form) global))
                          (mapcar #'subeval (rest form))))
               (evalcompile form environment)))
          (t form))))))

;;; Might want to move this, or have cleavir define it or something...?
(defmethod sicl-genv:symbol-macro (symbol (env cleavir-env::entry))
  (sicl-genv:symbol-macro symbol (cleavir-env::next env)))
(defmethod sicl-genv:symbol-macro (symbol (env cleavir-env:special-variable))
  (if (eq (cleavir-env:name env) symbol)
      (values nil nil) ; shadowed
      (call-next-method)))

;;; constants can't be rebound, so just go straight to the top.
(defmethod sicl-genv:constant-variable (symbol (env cleavir-env::entry))
  (sicl-genv:constant-variable symbol (cleavir-env::next env)))

;;; FIXME: move this probably
;;; Define in the compiler environment.
;;; has the compile-time side effect of defun, i.e., makes the compiler know it exists
;;; so that references to it pass silently
(defun declare-function (name lambda-list environment)
  ;; if it has a type, it's "known" already, so skip.
  (unless (sicl-genv:function-type name environment)
    (setf (sicl-genv:function-type name environment)
          `(function ,(cleavir-code-utilities:lambda-list-type-specifier lambda-list) *))))

(defun repl-print (values &optional (stream *terminal-io*))
  (fresh-line stream)
  (dolist (v values)
    (prin1 v stream)
    (terpri stream)))

(defun repl (environment &optional (stream *terminal-io*))
  (let ((sicl-genv:*global-environment* environment) ; FIXME: see comments in cleavir.lisp
        ;; This is kind of bad: we bind these as thread local so that we can set them
        ;; easily and having that be visible from within the evaluator.
        +++ ++ + /// // / *** ** * -)
    (declare (special sicl-genv:*global-environment*))
    (loop
      (with-simple-restart (abort "Return to REPL.")
        (fresh-line stream)
        ;; FIXME: should get package from the environment
        (let ((package-name (package-name *package*)))
          (when package-name (write-string package-name stream))
          (write-string "> " stream)
          ;; FIXME: should READ "in the environment", which means using its READ probably
          (setf - (read stream))
          (let ((values (multiple-value-list (clasp-cleavir::cclasp-eval - environment))))
            (shiftf +++ ++ + -)
            (shiftf /// // / values)
            (shiftf *** ** * (car values))
            (repl-print values stream)))))))

(defun sandbox-compile-file-hook (env)
  (lambda (form)
    (clasp-cleavir::cleavir-compile-file-form form env)))
