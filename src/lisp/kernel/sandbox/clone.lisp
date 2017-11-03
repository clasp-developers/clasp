(in-package #:clasp-sandbox)

;;;; Clone parts of the toplevel environment

;;; compile-only-p flags control whether only stuff for a compiler environment is cloned.
;;; If it's T, fdefinitions and specials aren't copied at all, not even to say they exist.

(defun import-variable-info (env info &optional compile-only-p)
  (symbol-macrolet ((name (cleavir-env:name info)))
    (etypecase info
      (cleavir-env:constant-variable-info
       (setf (sicl-genv:constant-variable name env) (cleavir-env:value info)))
      (cleavir-env:symbol-macro-info
       (setf (sicl-genv:symbol-macro name env) (cleavir-env:expansion info))
       (setf (sicl-genv:variable-type name env) (cleavir-env:type info)))
      (cleavir-env:special-variable-info
       (unless compile-only-p
         ;; skips info. bad
         (if (boundp name)
             (setf (sicl-genv:special-variable name env t) (symbol-value name))
             (setf (sicl-genv:special-variable name env nil) nil))
         (setf (sicl-genv:variable-type name env) (cleavir-env:type info))))
      (null))))

(defun import-operator-info (env info &optional compile-only-p)
  (symbol-macrolet ((name (cleavir-env:name info)))
    (etypecase info
      (cleavir-env:special-operator-info
       (setf (sicl-genv:special-operator name env) t))
      (cleavir-env:global-macro-info
       (setf (sicl-genv:macro-function name env) (cleavir-env:expander info))
       (setf (sicl-genv:compiler-macro-function name env) (cleavir-env:compiler-macro info)))
      (cleavir-env:global-function-info
       (unless compile-only-p
         (setf (sicl-genv:fdefinition name env) (fdefinition name)) ; skips info. bad
         (setf (sicl-genv:function-type name env) (cleavir-env:type info))
         (setf (sicl-genv:function-inline name env) (cleavir-env:inline info))
         (setf (sicl-genv:function-ast name env) (cleavir-env:ast info))
         (setf (sicl-genv:compiler-macro-function name env) (cleavir-env:compiler-macro info))))
      (null))))

;;; Can't take an info because cleavir doesn't have info for types.
;;; Doesn't take a compile-only-p since it's all in the compiler.
(defun import-type-info (env name)
  (let ((class (find-class name nil))
        (expander (core:get-sysprop name 'core::deftype-definition)))
    (cond (class ;; could do a compiler class here, but it should be ok.
           (setf (sicl-genv:find-class name env) class))
          (expander
           (setf (sicl-genv:type-expander name env)
                 ;;; bridge between clasp and sicl format.
                 (lambda (spec env)
                   (declare (ignore env))
                   (apply expander (cdr spec))))))))

;;; No compile-only-p since it's in the compiler (about macroexpansions)
(defun import-setf-info (env name)
  ;; the reader is defined in setf.lisp.
  (let ((s (sicl-genv:setf-expander name env)))
    (when s
      (setf (sicl-genv:setf-expander name env) s))))

(defun import-from-toplevel (env &optional compile-only-p)
  ;; TODO: packages
  (let ((declarations (cleavir-env:declarations env)))
    (loop for decl in declarations
          do (setf (sicl-genv:declaration decl env) t)))
  ;; do-all-symbols is allowed to process symbols more than once due to
  ;; inheritance relationships, so we have to fix that ourselves
  (let ((seen (make-hash-table :test #'eq)))
    (do-all-symbols (s)
      (unless (eq (symbol-package s) (find-package "KEYWORD")) ; clasp is broken on variable-info of keywords.
        (cond ((gethash s seen) nil)
              (t
               (setf (gethash s seen) t)
               (import-variable-info env (cleavir-env:variable-info nil s) compile-only-p)
               (import-operator-info env (cleavir-env:function-info nil s) compile-only-p)
               (import-setf-info env s)
               (import-operator-info env (cleavir-env:function-info nil `(setf ,s)) compile-only-p)
               (import-type-info env s)
               ;; probably not required
               (unless compile-only-p
                 (setf (sicl-genv:symbol-plist s env) (symbol-plist s))))))))
  (values))
