(in-package #:clasp-sandbox)

;;;; Clone parts of the toplevel environment

;;; compile-only-p flags control whether only stuff for a compiler environment is cloned.
;;; If it's T, fdefinitions and specials aren't copied at all, not even to say they exist.

(defun import-variable-info (env source name &optional compile-only-p)
  (let ((info (cleavir-env:variable-info source name)))
    (etypecase info
      (cleavir-env:constant-variable-info
       (setf (sicl-genv:constant-variable name env) (cleavir-env:value info)))
      (cleavir-env:symbol-macro-info
       (setf (sicl-genv:symbol-macro name env) (cleavir-env:expansion info))
       (setf (sicl-genv:variable-type name env) (cleavir-env:type info)))
      (cleavir-env:special-variable-info
       (unless compile-only-p
         (if (sicl-genv:boundp name source)
             (setf (sicl-genv:special-variable name env t) (sicl-genv:symbol-value name souce))
             (setf (sicl-genv:special-variable name env nil) nil))
         (setf (sicl-genv:variable-type name env) (cleavir-env:type info))))
      (null))))

(defun import-operator-info (env source name &optional compile-only-p)
  (let ((info (cleavir-env:function-info source name)))
    (etypecase info
      (cleavir-env:special-operator-info
       (setf (sicl-genv:special-operator name env) t))
      (cleavir-env:global-macro-info
       (setf (sicl-genv:macro-function name env) (cleavir-env:expander info))
       (setf (sicl-genv:compiler-macro-function name env) (cleavir-env:compiler-macro info)))
      (cleavir-env:global-function-info
       (unless compile-only-p
         (setf (sicl-genv:fdefinition name env) (sicl-genv:fdefinition name source)) ; not cleavir-env, note.
         (setf (sicl-genv:function-type name env) (cleavir-env:type info))
         (setf (sicl-genv:function-inline name env) (cleavir-env:inline info))
         (setf (sicl-genv:function-ast name env) (cleavir-env:ast info))
         (setf (sicl-genv:compiler-macro-function name env) (cleavir-env:compiler-macro info))))
      (null))))

;;; Can't take an info because cleavir doesn't have info for types.
;;; Doesn't take a compile-only-p since it's all in the compiler.
(defun import-type-info (env source name)
  (let ((class (sicl-genv:find-class name source))
        (expander (sicl-genv:type-expander name source)))
    (cond (class ;; could do a compiler class here, but it should be ok.
           (setf (sicl-genv:find-class name env) class))
          (expander
           (setf (sicl-genv:type-expander name env)
                 expander)))))

;;; No compile-only-p since it's in the compiler (about macroexpansions)
(defun import-setf-info (env name)
  ;; the reader is defined in setf.lisp.
  (let ((s (sicl-genv:setf-expander name source)))
    (when s
      (setf (sicl-genv:setf-expander name env) s))))

(defun import-symbol (dest source symbol &optional compile-only-p)
  #+(or) ; not important
  (setf (sicl-genv:declaration symbol dest)
        (sicl-genv:declaration symbol source))
  (import-variable-info dest (cleavir-env:variable-info source s) compile-only-p)
  (import-operator-info dest (cleavir-env:function-info source s) compile-only-p)
  (import-setf-info dest source s)
  (import-operator-info env (cleavir-env:function-info source `(setf ,s)) compile-only-p)
  (import-type-info dest source s)
  ;; probably not required
  #+(or)
  (unless compile-only-p
    (setf (sicl-genv:symbol-plist s env) (sicl-genv:symbol-plist s source))))

(defun import-package (package dest source &optional compile-only-p)
  (pushnew package (sicl-genv:packages dest))
  (do-external-symbols (s package)
    (import-symbol dest source s compile-only-p)))
