(in-package #:clasp-sandbox)

(defclass sandbox-environment (sicl-simple-environment:simple-environment) ())

;;; Hooking it up with us

(defmethod clasp-cleavir::cclasp-eval-with-env (form (environment sandbox-environment))
  (labels ((mexpand-1 (form env)
             (typecase form
               (symbol (multiple-value-bind (expander expansion)
                           (sicl-genv:symbol-macro form env)
                         (if expander
                             (values expansion t)
                             (values form nil))))
               (cons (if (symbolp (first form))
                         (let ((mf (sicl-genv:macro-function (first form) env)))
                           (if mf
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
             (clasp-cleavir::cclasp-eval-with-env form environment))
           (evalcompile (form env)
             (funcall (cmp:compile-in-env nil `(lambda () (progn ,form))
                                          environment cmp:*cleavir-compile-hook*
                                          'llvm-sys:external-linkage))))
    (let ((form (mexpand form environment)))
      (typecase form
        (symbol
         (core:symbol-value-from-cell
          form
          (sicl-genv:variable-cell form environment)
          (sicl-genv:variable-unbound form environment)))
        (cons
         (if (symbolp (first form))
             (if (sicl-genv:special-operator (first form) environment)
                 (evalcompile form environment)
                 (apply (car (sicl-genv:function-cell (first form) environment))
                        (mapcar #'subeval (rest form))))
             (evalcompile form environment)))
        (t form)))))

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
