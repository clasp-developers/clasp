

(in-package :cmp)

(defparameter *compile-verbose* nil )
(defparameter *compile-print* nil )




(defun compile-main-function (name ltv-manager-fn  )
  (cmp-log "In compile-main-function\n")
  (let ((main-fn (with-new-function (main-fn fn-env
					     :function-name name
					     :parent-env nil
					     :linkage 'llvm-sys:external-linkage ;; 'llvm-sys:external-linkage
					     :function-type +fn-void+
					     :argument-names nil)
		   (irc-low-level-trace)
		   (let* ((given-name (llvm-sys:get-name main-fn)))
		     (irc-low-level-trace)
		     (cmp-log "About to add invokeLlvmFunctionVoid for ltv-manager-fn\n")
		     (irc-intrinsic "invokeLlvmFunctionVoid" ltv-manager-fn)
		     ))))
    ;;    (cmp-log-dump main-fn)
    (cmp-log "Done compile-main-function")
    )
  )


(defmacro with-compilation-unit ((&key override module function-pass-manager ) &rest body)
  `(let* ((*the-module* ,module)
	  (*the-function-pass-manager* ,function-pass-manager)
	  (*all-functions-for-one-compile* nil)
	  (*generate-load-time-values* t)
	  )
     (declare (special *the-function-pass-manager*))
     (with-irbuilder (nil (llvm-sys:make-irbuilder *llvm-context*))
       ,@body
       )
     )
  )




(defun describe-form (form)
  (case (car form)
    (core:*fset (let* ((name (cadr (cadr form)))
		       (is-macro (cadddr form))
		       (header (if is-macro
				   "defmacro"
				   "defun")))
		  (bformat t ";    %s %s\n" header name)))
    (otherwise ()))) ;; describe more forms here

(defun compile-top-level (form)
  (when *compile-print*
    (describe-form form))
  (let ((fn (compile-thunk "repl" form nil)))
    (with-ltv-function-codegen (result ltv-env)
      (irc-intrinsic "invokeLlvmFunction" result fn (irc-renv ltv-env)
                     ))))

(defun compiler-macro-function (sym) nil)



(defun t1progn (form)
  "All forms in progn at top level are top level forms"
  (dolist (subform (cdr form))
    (t1expr subform)))

(defun t1eval-when (form)
  (let ((situations (cadr form))
	(body (cddr form)))
    (when (or (member 'core:compile situations) (member :compile-toplevel situations))
      (cmp-log "Performing eval-when :compile-toplevel side-effects\n")
      (cmp-log "Evaluating: %s\n" body)
      (eval `(progn ,@body))
      (cmp-log "Done eval-when compile-toplevel side-effects\n"))
    (when (or (member 'core:load situations) (member :load-toplevel situations))
      (cmp-log "Compiling body due to :load-toplevel --> %s\n" body)
      ;; Each subform is a top-level form
      (dolist (subform body)
	(t1expr subform))
      (cmp-log "Done compiling body due to :load-toplevel\n")
      )
    ))

(defun t1locally (form)
  (error "Add support for cmp:t1locally"))

(defun t1macrolet (form)
  (error "Add support for cmp:t1macrolet"))

(defun t1symbol-macrolet (form)
  (error "Add support for cmp:t1symbol-macrolet"))


(defun t1expr (form)
  (cmp-log "t1expr-> %s\n" form)
  (let ((head (if (atom form) form (car form))))
    (cond
      ((eq head 'cl:eval-when) (t1eval-when form))
      ((eq head 'cl:progn) (t1progn form))
      ((eq head 'cl:locally) (t1locally form))
      ((eq head 'cl:macrolet) (t1macrolet form))
      ((eq head 'cl:symbol-macrolet) (t1symbol-macrolet form))
      ((eq head 'cl:defparameter)
       (eval `(defvar ,(cadr form)))
       (compile-top-level form))
      ((eq head 'cl:defvar)
       (eval `(defvar ,(cadr form)))
       (compile-top-level form))
      ((compiler-macro-function head)
       (error "Handle compiler macro functions"))
      ((macro-function head)
       (let ((expanded (macroexpand form)))
	 (t1expr expanded)))
      (t (compile-top-level form)))
    ))
       




(defun print-source-pos ()
  (bformat t "%s:%d:%d\n" *compile-file-pathname* *current-line-number* *current-column*))





(defun cfp-output-file-default (input-file)
  (let* ((defaults (merge-pathnames input-file *default-pathname-defaults*))
	 (retyped (make-pathname :type "bc" :defaults defaults)))
    retyped))

;;; Copied from sbcl sb!xc:compile-file-pathname
;;;   If INPUT-FILE is a logical pathname and OUTPUT-FILE is unsupplied,
;;;   the result is a logical pathname. If INPUT-FILE is a logical
;;;   pathname, it is translated into a physical pathname as if by
;;;   calling TRANSLATE-LOGICAL-PATHNAME.
;;; So I haven't really tried to make this precisely ANSI-compatible
;;; at the level of e.g. whether it returns logical pathname or a
;;; physical pathname. Patches to make it more correct are welcome.
(defun compile-file-pathname (input-file &key (output-file nil output-file-p) target-backend)
  (if output-file-p
      (merge-pathnames output-file (cfp-output-file-default input-file))
      (cond
        (target-backend
         (let ((target-host (string target-backend)))
           (load-logical-pathname-translations target-host)
           (merge-pathnames (make-pathname :host target-host) (cfp-output-file-default input-file))))
        (t
         (cfp-output-file-default input-file)))))


(defun cf-module-name (type pathname)
  "Create a module name from the TYPE (either :user or :kernel)
and the pathname of the source file - this will also be used as the module initialization function name"
  (string-downcase (bformat nil "___%s_%s" (string type) (pathname-name pathname))))

(defun compile-file (given-input-pathname
		     &key
		       (output-file (cfp-output-file-default given-input-pathname))
		       (verbose *compile-verbose*)
		       (print *compile-print*)
		       (external-format :default)
;;; type can be either :kernel or :user
		       (type :user)
                       target-backend
		       )
  "See CLHS compile-file"
  ;; TODO: Save read-table and package with unwind-protect
  (with-compiler-env ()
    (let ((input-pathname (probe-file given-input-pathname)))
      (multiple-value-bind (module function-pass-manager)
	  (create-llvm-module-for-compile-file (namestring input-pathname))
	(let* ((sin (open input-pathname :direction :input))
	       (module-name (cf-module-name type input-pathname))
	       (output-path (compile-file-pathname input-pathname :output-file output-file))
	       (eof-value (gensym)))
	  (when verbose
	    (bformat t "; Compiling file: %s\n" (namestring input-pathname)))
          (with-one-source-database
              (cmp-log "About to start with-compilation-unit\n")
          
            (with-compilation-unit (:override nil
                                              :module module
                                              :function-pass-manager function-pass-manager)
              (let* ((*compile-file-pathname* given-input-pathname)
                     (*compile-file-truename* (truename *compile-file-pathname*))
                     (*gv-source-path-name* (jit-make-global-string-ptr (namestring *compile-file-truename*) "source-pathname"))
                     (*compile-print* print)
                     (*compile-verbose* verbose))
                (with-dibuilder (*the-module*)
                  (with-dbg-compile-unit (nil *compile-file-truename*)
                    (with-dbg-file-descriptor (nil *compile-file-truename*)
                      ;;	    (let ((*generate-load-time-values* t))
                      (with-load-time-value-unit (ltv-init-fn)
                        (do ((line-number (stream-linenumber sin)
                                          (stream-linenumber sin))
                             (column (stream-column sin)
                                     (stream-column sin))
                             (form (progn (let ((rd (read sin nil eof-value))) rd))
                                   (progn (let ((rd (read sin nil eof-value))) rd))))
                            ((eq form eof-value) nil)
                          (let ((*current-line-number* line-number)
                                (*current-column* column))
                            (t1expr form)
                            ))
                        (compile-main-function output-file ltv-init-fn )
                        ))))
                (cmp-log "About to verify the module\n")
                (cmp-log-dump *the-module*)
                (multiple-value-bind (found-errors error-message)
                    (progn
                      (cmp-log "About to verify module prior to writing bitcode\n")
                      (llvm-sys:verify-module *the-module* 'llvm-sys:return-status-action)
                      )
                  (if found-errors
                      (break "Verify module found errors")))
                (bformat t "Writing bitcode to %s\n" (core:coerce-to-filename output-path))
                (ensure-directories-exist output-path)
                (llvm-sys:write-bitcode-to-file *the-module* (core:coerce-to-filename output-path))
                ))))))))
  
    

(export 'compile-file)


