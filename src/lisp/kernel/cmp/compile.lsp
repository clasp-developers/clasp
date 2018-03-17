(in-package #:cmp)

;;;; Top-level interface: CL:COMPILE

;;; Use the *cleavir-compile-hook* to determine which compiler to use
;;; if nil == bclasp. Code for the bclasp compiler is in codegen.lsp;
;;; look for bclasp-compile*.

(defparameter *lambda-args-num* 0)

(defmacro with-module (( &key module
                              (optimize nil)
                              (optimize-level :-O3)
                              source-namestring
                              source-file-info-handle
                              source-debug-namestring
                              (source-debug-offset 0)
                              (source-debug-use-lineno t)) &rest body)
  `(let* ((*the-module* ,module)
 	  #+(or)(*generate-load-time-values* t)
	  (*gv-source-namestring* (module-make-global-string ,source-namestring "source-namestring"))
	  (*gv-source-debug-namestring* (module-make-global-string (if ,source-debug-namestring
									,source-debug-namestring
									,source-namestring) "source-debug-namestring"))
	  (*source-debug-offset* ,source-debug-offset)
	  (*source-debug-use-lineno* ,source-debug-use-lineno)
	  (*gv-source-file-info-handle* (make-gv-source-file-info-handle ,module ,source-file-info-handle)))
     (or *the-module* (error "with-module *the-module* is NIL"))
     (multiple-value-prog1
         (with-irbuilder ((llvm-sys:make-irbuilder *llvm-context*))
           ,@body)
       (cmp-log "About to optimize-module\n")
       (when (and ,optimize ,optimize-level) (funcall ,optimize ,module ,optimize-level )))))

(defun compile-with-hook (compile-hook name &optional definition env pathname &key (linkage 'llvm-sys:internal-linkage))
  "Dispatch to clasp compiler or cleavir-clasp compiler if available.
We could do more fancy things here - like if cleavir-clasp fails, use the clasp compiler as backup."
  (if compile-hook
      (funcall compile-hook name definition env pathname :linkage linkage)
      (bclasp-compile* name definition env pathname :linkage linkage)))

(defun bclasp-compile (name form)
  (let ((*cleavir-compile-hook* nil)
        (core:*use-cleavir-compiler* nil))
    (compile name form)))

(defun compile-in-env (bind-to-name &optional definition env compile-hook (linkage 'llvm-sys:internal-linkage) &aux conditions)
  "Compile in the given environment"
  (with-compiler-env (conditions)
    (let* ((*the-module* (create-run-time-module-for-compile)))
      ;; Link the C++ intrinsics into the module
      (let* ((pathname (if *load-pathname*
			   (namestring *load-pathname*)
			   "repl-code"))
	     (handle (multiple-value-bind (the-source-file-info the-handle)
			 (core:source-file-info pathname)
		       the-handle)))
	(with-module (:module *the-module*
                              :optimize nil
                              :source-namestring (namestring pathname)
                              :source-file-info-handle handle)
          (cmp-log "Dumping module\n")
          (cmp-log-dump-module *the-module*)
          (multiple-value-bind (compiled-function warnp failp)
              (compile-with-hook compile-hook bind-to-name definition env pathname :linkage linkage)
            (when bind-to-name
              (let ((lambda-list (cadr definition)))
                (core:fset bind-to-name compiled-function nil lambda-list)))
            (values compiled-function warnp failp)))))))

(defun compile (name &optional definition)
  (multiple-value-bind (function warnp failp)
      ;; Get the actual compiled function and warnp+failp.
      (cond
        ((compiled-function-p definition)
         (values definition nil nil))
        ((interpreted-function-p definition)
         (dbg-set-current-debug-location-here)
         ;; Recover the lambda-expression from the interpreted-function
         (multiple-value-bind (lambda-expression wrapped-env)
             (generate-lambda-expression-from-interpreted-function definition)
           (cmp-log "About to compile  name: %s  lambda-expression: %s wrapped-env: %s\n" name lambda-expression wrapped-env)
           (compile-in-env name lambda-expression wrapped-env *cleavir-compile-hook* 'llvm-sys:external-linkage)))
        ((functionp definition)
         (error "COMPILE doesn't know how to handle this type of function"))
        ((and (consp definition) (eq (car definition) 'lambda))
         (cmp-log "compile form: %s\n" definition)
         (compile-in-env name definition nil *cleavir-compile-hook* 'llvm-sys:external-linkage))
        ((null definition)
         (let ((func (cond ((fboundp name) (fdefinition name))
                           ((and (symbolp name) (macro-function name)))
                           (t (error "No definition for ~a" name)))))
           (cond
             ((interpreted-function-p func)
              (dbg-set-current-debug-location-here)
              ;; Recover the lambda-expression from the interpreted-function
              (multiple-value-bind (lambda-expression wrapped-env)
                  (generate-lambda-expression-from-interpreted-function func)
                (cmp-log "About to compile  name: %s  lambda-expression: %s wrapped-env: %s\n" name lambda-expression wrapped-env)
                (compile-in-env name lambda-expression wrapped-env *cleavir-compile-hook* 'llvm-sys:external-linkage)))
             ((compiled-function-p func)
              (values func nil nil))
             ((core:cxx-instance-p func)
              (let ((user-func (clos:get-funcallable-instance-function func)))
                (when (and user-func (interpreted-function-p user-func))
                  (let ((compiled-user-func (compile nil user-func)))
                    (when (not (eq user-func compiled-user-func))
                      (clos:set-funcallable-instance-function func compiled-user-func)))))
              (values func nil nil))
             (t (error "COMPILE doesn't know how to handle this type of function")))))
        (t (error "Illegal combination of arguments for compile: ~a ~a, class-of definition ~a" name definition (class-of definition))))
    ;; Bind the name if applicable.
    (cond ((and (symbolp name) (macro-function name))
           (setf (macro-function name) function)
           (values name warnp failp))
          (name
           (setf (fdefinition name) function)
           (values name warnp failp))
          (t (values function warnp failp)))))

(defun compiler-stats ()
  (bformat t "Accumulated finalization time %s\n" llvm-sys:*accumulated-llvm-finalization-time*)
  (bformat t "Most recent finalization time %s\n" llvm-sys:*most-recent-llvm-finalization-time*)
  (bformat t "Number of compilations %s\n" llvm-sys:*number-of-llvm-finalizations*))

(export 'compiler-stats)

#+(or bclasp cclasp)
(progn
  (defun bclasp-compile (form &optional definition)
    (let ((cmp:*cleavir-compile-hook* nil)
          (cmp:*cleavir-compile-file-hook* nil)
          (core:*use-cleavir-compiler* nil)
          (core:*eval-with-env-hook* #'core:eval-with-env-default))
      (compile form definition)))
  (export 'bclasp-compile))
