(in-package #:clasp-sandbox)

(defun read-interpreter-info (env fname)
  (with-open-file (s fname)
    (loop for x = (read s nil nil)
          until (null x)
          do (destructuring-bind (sym symbol-macro special constant class fbound macro special-operator) x
               ;; skip redefinitions
               (unless (cleavir-env:variable-info env sym)
                 (cond (symbol-macro (setf (sicl-genv:symbol-macro sym env) (first symbol-macro)))
                       (constant (setf (sicl-genv:constant-variable sym env) (first constant)))
                       (special (setf (sicl-genv:special-variable sym env nil) nil))))
               (unless (sicl-genv:find-class sym env)
                 (when class
                   ;; hopefully the definition hasn't changed!
                   (setf (sicl-genv:find-class sym env) (find-class sym))))
               (unless (cleavir-env:function-info env sym)
                 (when fbound
                   (cond (special-operator (setf (sicl-genv:special-operator sym env) t))
                         ;; again, hopefully the definition is ok.
                         ;; I think the only macro is core:backquote, which isn't redefined.
                         (macro (setf (sicl-genv:macro-function sym env)
                                      (macro-function sym)))
                         ;; declare the function
                         (t (setf (sicl-genv:function-type sym env) '(function * *)))))))))
  (values))
