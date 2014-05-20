
(defparameter *compile-verbose* nil)
(defparameter *compile-print* nil)



(defun compile-main-function (name all-functions )
  (let* ((fn (create-repl-function name))
	 (given-name (llvm-sys:get-name fn))
	 (bb (irc-basic-block-create "entry" fn)))
    ;; Map the function argument names
    (irc-set-insert-point bb)
    (let* ((env (irc-new-value-environment
		 nil
		 :number-of-arguments 0
		 :label "dummy"
		 :fill-runtime-form (lambda (new-env) (irc-call "makeValueFrameWithNilParent"
								(irc-renv new-env)
								(irc-constant-i32 0)))))
	   (result (irc-alloca-tsp env "main-result")))
      (dolist (f all-functions)
	(irc-call "invokeLlvmFunction" result f (irc-renv env)))
      (irc-cleanup env)
      (llvm-sys:create-ret-void *irbuilder*)
      )))


(defun compile-file (input-path-name &key (output-file "./_module.bc") (verbose *compile-verbose*)  (print *compile-print*) (external-format :default) )
  "See CLHS compile-file"
  (when verbose
    (bformat t "; Compiling file: %s\n" input-path-name))
  (let ((*the-module* (llvm-sys:make-module input-path-name *llvm-context*))
	(sin (open input-path-name :direction :input))
	(eof-value (gensym))
	repl-functions)
    (do ((form (read sin nil eof-value) (read sin nil eof-value)))
	((eq form eof-value) nil)
      (when print
	(bformat t "; Compiling: %s\n" form))
      (let ((f (compile-repl-function "repl" form)))
	(push f repl-functions)))
    (compile-main-function "main" repl-functions)
    (bformat t "---- Dumping module for debugging\n")
    (llvm-sys:dump *the-module*)
    (debug-write-module *the-module* output-file-name)
    )
    
