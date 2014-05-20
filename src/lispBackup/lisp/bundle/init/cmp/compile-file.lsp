

(in-package :cmp)

(defparameter *compile-verbose* nil )
(defparameter *compile-print* nil )



(defun compile-main-function (name ltv-manager-fn all-functions )
  (let ((fn (with-new-function (fn fn-env
				   :function-name name
				   :parent-env nil
				   :linkage 'llvm-sys:external-linkage)
	      (let* ((given-name (llvm-sys:get-name fn))
		     (result (car (llvm-sys:get-argument-list fn))))
		(irc-call nil "invokeLlvmFunction" result ltv-manager-fn (irc-renv fn-env))
		(dolist (f all-functions)
		  (irc-call nil "invokeLlvmFunction" result f (irc-renv fn-env)))
		(codegen-ltv-nil result fn-env))
	      )))
;;    (log-dump fn)
    fn
    ))



(defmacro with-compilation-unit ((&key override module function-pass-manager ) &rest body)
  `(let* ((*repl-functions* nil)
	  (*the-module* ,module)
	  (*the-function-pass-manager* ,function-pass-manager))
     ,@body
     )
)



#|
(defun t1defmacro (form)
  (destructuring-bind (name lambda-list &rest body)
      form
    (multiple-value-bind (function pprint doc-string)
        (expand-defmacro name lambda-list body)
      (let ((fn (cmp-eval function nil ))) ;; *cmp-env*)))
        (cmp-env-register-global-macro name fn))
      (t1expr* (macroexpand `(DEFMACRO ,@form))))))
|#


(defun t1progn (form)
  "All forms in progn at top level are top level forms"
  (dolist (subform (cdr form))
    (t1expr subform)))


(defun t1defparameter (form)
  "defparameter at the top level declares a variable in the global dynamic environment"
  (compile-top-level form))




(defun t1eval-when (form)
  (let ((situations (cadr form))
	(body (cddr form)))
    (when (or (member 'core:compile situations) (member :compile-toplevel situations))
      (cmp-log "Performing eval-when compile-toplevel side-effects\n")
      (cmp-log "Evaluating: %s\n" body)
      (eval `(progn ,@body))
      (cmp-log "Done eval-when compile-toplevel side-effects\n"))
    (when (or (member 'core:load situations) (member :load-toplevel situations))
      (cmp-log "Compiling body due to :load-toplevel --> %s\n" body)
      (compile-top-level `(progn ,@body))
      (cmp-log "Done compiling body due to :load-toplevel\n")
      )
    ))


(defun describe-form (form)
  (case (car form)
    (core:*fset    (let* ((name (cadr (cadr form)))
			  (is-macro (cadddr form))
			  (header (if is-macro
				      "defmacro"
				      "defun")))
		     (bformat t ";    %s %s\n" header name)))
    (otherwise ())))


(defun compile-top-level (form)
  (when *compile-print*
    (describe-form form))
  (let ((fn (compile-thunk "repl" form nil)))
    (push fn *repl-functions*)))



(defun compiler-macro-function (sym)
  nil)



(defun t1expr (form)
  (cmp-log "t1expr-> %s\n" form)
  (let ((head (if (atom form) form (car form))))
    (cond
      ((eq head 'cmp:eval-when) (t1eval-when form))
      ((eq head 'cmp:progn) (t1progn form))
      ((eq head 'cmp:defparameter)
       (eval `(defvar ,(cadr form)))
       (compile-top-level form))
      ((eq head 'cmp:defvar)
       (eval `(defvar ,(cadr form)))
       (compile-top-level form))
      ((compiler-macro-function head)
       (error "Handle compiler macro functions"))
      ((macro-function head)
       (let ((expanded (macroexpand form)))
	 (t1expr expanded)))
      (t (compile-top-level form)))
    ))
       


(defvar *use-function-pass-manager-for-compile-file* t)


(defun create-function-pass-manager-for-compile-file (module)
  (let ((fpm (llvm-sys:make-function-pass-manager module)))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:data-layout-copy (llvm-sys:get-data-layout *the-execution-engine*)))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-basic-alias-analysis-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-instruction-combining-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-reassociate-pass))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-gvnpass nil))
    (llvm-sys:function-pass-manager-add fpm (llvm-sys:create-cfgsimplification-pass))
    (llvm-sys:do-initialization fpm)
    fpm))



(defun create-llvm-module-for-compile-file (module-name)
  "Return a new module and the function-pass-manager for the module"
  (let ((module (llvm-sys:make-module module-name *llvm-context*))
	fpm)
    (if *use-function-pass-manager-for-compile-file*
	(setq fpm (create-function-pass-manager-for-compile-file module)))
    (values module fpm)))


(defun print-source-pos ()
  (bformat t "%s:%d:%d\n" *source-path-name* *current-line-number* *current-column*))

(defun compile-file (input-path-name &key (output-file nil) (verbose *compile-verbose*)  (print *compile-print*) (external-format :default) )
  "See CLHS compile-file"
  ;; TODO: Save read-table and package with unwind-protect
  (let ((readtable *readtable*)
	(package *package*))
    (multiple-value-bind (module fpm)
	(create-llvm-module-for-compile-file input-path-name)
      (let* ((sin (open input-path-name :direction :input))
	     (input-path (make-path input-path-name))
	     (module-name (bformat nil "___main_%s" (path-stem input-path)))
	     (output-path (if output-file
			      (path-designator output-file)
			      (replace-extension (copy-path input-path) ".bc")))
	     (eof-value (gensym)))
	(when verbose
	  (bformat t "; Compiling file: %s\n" (path-file-name input-path)))
	(unless output-file
	  (setq output-file (as-string output-path)))
	(cmp-log "About to start with-compilation-unit\n")
	(with-compilation-unit (:override nil
					  :module module
					  :function-pass-manager fpm)
	  (let* ((*source-path-name* input-path-name)
		 (*gv-source-path-name* (jit-make-global-string-ptr input-path-name "source-path-name"))
		 (*compile-print* print)
		 (*compile-verbose* verbose))
	    (define-primitives-in-*the-module*)
	    (let ((*generate-load-time-values* t))
	      (with-load-time-value-unit (ltv-init-fn)
		(do ((line-number (stream-line-number sin)
				  (stream-line-number sin))
		     (column (stream-column sin)
			     (stream-column sin))
		     (form (progn (let ((rd (read sin nil eof-value))) rd))
			   (progn (let ((rd (read sin nil eof-value))) rd))))
		    ((eq form eof-value) nil)
		  (let ((*current-line-number* line-number)
			(*current-column* column))
		    (t1expr form)
		    ))
		(compile-main-function module-name ltv-init-fn (nreverse *repl-functions*))))
	    (cmp-log "About to verify the module\n")
	    (multiple-value-bind (found-errors error-message)
		(llvm-sys:verify-module *the-module* 'llvm-sys:return-status-action)
	      (if found-errors
		  (break "Verify module found errors")))
	    (llvm-sys:write-bitcode-to-file *the-module* output-file)
	    )
	  )
	)
      (setq *readtable* readtable
	    *package* package))
    ))
  
    

(export 'compile-file)


