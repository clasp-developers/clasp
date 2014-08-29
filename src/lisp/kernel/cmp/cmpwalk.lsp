
(in-package :cmp)

(defun code-walk-using-compiler (form env &key code-walker-function)
  "This is used in clos/method.lsp to code walk defmethod bodies"
  (let* ((module (llvm-create-module "code-walk-for-defmethod"))
	 (fpm #+(or)(create-function-pass-manager-for-compile-file module))
	 (*code-walker* code-walker-function))
    (define-primitives-in-module module)
    (with-compilation-unit (:module module :function-pass-manager fpm)
      (let ((*gv-source-path-name* (jit-make-global-string-ptr "code-walk-using-compiler" "source-path-name"))
            (*gv-source-file-info-handle* (llvm-sys:make-global-variable *the-module*
                                                                         +i32+ ; type
                                                                         nil ; constant
                                                                         'llvm-sys:internal-linkage
                                                                         (jit-constant-i32 0)
                                                                         "source-file-info-handle"))
            )
	(with-load-time-value-unit (ltv-init-fn)
	  (compile-in-env nil form env)))
      (llvm-sys::module-delete module)
      )))


(export 'code-walk-using-compiler)
