
(in-package :cmp)

(defconstant +file.dbg+ :file.dbg)



(defmacro set-env-file.dbg (env)
  "Set the file.dbg metadata of the given environment to the current source file"
  (let ((filename-sym (gensym))
	(path-sym (gensym)))
    `(multiple-value-bind (,filename-sym ,path-sym) (source-file-name)
      (setf-metadata ,env +file.dbg+ (mdnode-file-descriptor ,filename-sym ,path-sym)))))

(defun mdstring (str)
  (llvm-sys:mdstring-get *llvm-context* str))

(defun mdnode (&rest stuff)
  (llvm-sys:mdnode-get *llvm-context* stuff))
  
(defun mdunused ()
  (mdstring "unused"))

(defun mdnode-file-descriptor (source-file-name source-directory)
  "Return a dw-tag-file-type mdnode"
  (mdnode (jit-constant-i32 (llvm-sys:dw-tag 'llvm-sys:dw-tag-file-type))
	  (mdstring source-file-name)
	  (mdstring source-directory)
	  (mdunused)))


(defun mdnode-source-line (line col env)
  (let ((scope (lookup-metadata env +file.dbg+)))
    (mdnode (jit-constant-i32 line) ;; line#
	    (jit-constant-i32 1) ;; col
	    scope
	    nil)))


(defparameter *llvm-metadata* (make-hash-table))


(defun dbg-set-current-debug-location (filename pathname lineno column)
  (let* ((scope-name (bformat nil "%s>>%s" pathname filename))
	 (scope (gethash scope-name *llvm-metadata*)))
    (unless scope
      (setq scope (mdnode-file-descriptor filename pathname))
      (core::hash-table-setf-gethash *llvm-metadata* scope-name scope))
    (let ((debugloc (llvm-sys:debug-loc-get lineno column scope)))
      (llvm-sys:set-current-debug-location *irbuilder* debugloc))))


(defmacro dbg-set-current-debug-location-here ()
  (let ((filename-gs (gensym))
	(path-gs (gensym))
	(lineno-gs (gensym))
	(column-gs (gensym)))
    `(multiple-value-bind (,filename-gs ,path-gs) (source-file-name)
       (multiple-value-bind (,lineno-gs ,column-gs) (source-line-column)
	 (dbg-set-current-debug-location ,filename-gs ,path-gs ,lineno-gs ,column-gs)))))





#|
(defmacro compile-debug-print-object (msg obj)
  "Insert a call to debugPrintObject"
  (let ((sym-line (gensym))
	(sym-col (gensym))
	(sym-filename (gensym))
	(sym-pathname (gensym))
	(sym-msg (gensym)))
    `(multiple-value-bind (,sym-line ,sym-col) (source-line-column)
       (multiple-value-bind (,sym-filename ,sym-pathname) (source-file-name)
	 (let ((,sym-msg (llvm-sys:make-string-global *the-module* (bformat nil "%s:%d --> %s : obj->%s" ,sym-filename ,sym-line ,msg ,obj))))
	   (irc-call "debugPrintObject" ,sym-msg ,obj))))))
|#


#|
(defmacro compile-debug-print-i32 (msg obj)
  "Insert a call to debugPrintI32"
  (let ((sym-line (gensym))
	(sym-col (gensym))
	(sym-filename (gensym))
	(sym-pathname (gensym))
	(sym-msg (gensym)))
    `(multiple-value-bind (,sym-line ,sym-col) (source-line-column)
       (multiple-value-bind (,sym-filename ,sym-pathname) (source-file-name)
	 (let ((,sym-msg (llvm-sys:make-string-global *the-module* (bformat nil "%s:%d --> %s" ,sym-filename ,sym-line ,msg))))
	   (irc-call "debugPrintI32" ,sym-msg ,obj))))))
|#

(defun debug-generate-source-code (form)
  (let ((all-code (bformat nil "%s" form)))
    ;; TODO:  Return only the first XXX characters of all-code
    (jit-make-global-string-ptr (subseq all-code 0 80))))




(defun trace-enter-lexical-scope ( scope-name env form )
  (let* ((scope-fn (bformat nil "trace_enter%sScope" scope-name))
	 (source-code (debug-generate-source-code form))
	 (scope-id (irc-call env scope-fn *gv-source-path-name*
			    (irc-i32-current-line-number)
			    (irc-i32-current-column)
			    (irc-renv env)
			    source-code
			    (bformat nil "trace-%s" scope-name))))
    scope-id))
;;      (irc-push-unwind env (list 'exit-lexical-scope scope-name scope-id source-code))))


(defun trace-exit-lexical-scope (scope-name env traceid)
  (let ((scope-fn (bformat nil "trace_exit%sScope" scope-name)))
    (irc-call env scope-fn traceid)))



(defun trace-enter-call-scope (env form)
  (trace-enter-lexical-scope "Call" env form))

(defun trace-exit-call-scope (env traceid)
  (trace-exit-lexical-scope "Call" env traceid))



(defun trace-enter-let-scope (env form)
  (trace-enter-lexical-scope "Let" env form))

(defun trace-exit-let-scope (env traceid)
  (trace-exit-lexical-scope "Let" env traceid))


(defun trace-enter-let*-scope (env form)
  (trace-enter-lexical-scope "LetSTAR" env form))

(defun trace-exit-let*-scope (env traceid)
  (trace-exit-lexical-scope "LetSTAR" env traceid))


(defun trace-enter-flet-scope (env form)
  (trace-enter-lexical-scope "Flet" env form))

(defun trace-exit-flet-scope (env traceid)
  (trace-exit-lexical-scope "Flet" env traceid))

(defun trace-enter-labels-scope (env form)
  (trace-enter-lexical-scope "Labels" env form))

(defun trace-exit-labels-scope (env traceid)
  (trace-exit-lexical-scope "Labels" env traceid))



(defun trace-enter-catch-scope (env form)
  (trace-enter-lexical-scope "Catch" env form ))

(defun trace-exit-catch-scope (env traceid)
  (trace-exit-lexical-scope "Catch" env traceid))



(defun trace-enter-block-scope (env form)
  (trace-enter-lexical-scope "Block" env  form))

(defun trace-exit-block-scope (env traceid)
  (trace-exit-lexical-scope "Block" env traceid))


(defun trace-enter-function-scope (fn env form)
  (let* ((source-code (debug-generate-source-code form))
	 (fn-arguments-af (cadr (llvm-sys:get-argument-list fn)))
	 (scope-id (irc-call env "trace_enterFunctionScope"
			     *gv-source-path-name*
			     (irc-i32-current-line-number)
			     (irc-i32-current-column)
			     fn-arguments-af
			     source-code
			     (bformat nil "trace-FN"))))
    scope-id
    )
  )

;;    (irc-push-cleanup env (list 'exit-lexical-scope "Function" scope-id source-code))))


(defun trace-exit-function-scope (env traceid)
  (irc-call env "trace_exitFunctionScope" traceid ))


(defun debug-gdb (env)
    (irc-call env "gdb"))
