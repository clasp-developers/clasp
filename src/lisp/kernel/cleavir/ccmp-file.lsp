(in-package '#:cleavir-cmp)




(defun compile-file-to-module (given-input-pathname output-path &key type)
  "Compile a lisp source file into an LLVM module.  type can be :kernel or :user
The output-path is used to create the name of the main function."
  ;; TODO: Save read-table and package with unwind-protect
    (let* ((input-pathname (probe-file given-input-pathname))
	   (sin (open input-pathname :direction :input))
           (eof-value (gensym))
           (module (create-llvm-module-for-compile-file (namestring input-pathname)))
           (module-name (cf-module-name type input-pathname))
           warnings-p failure-p)
      (when *compile-verbose*
        (bformat t "; Compiling file: %s\n" (namestring input-pathname)))
      (with-one-source-database
          (cmp-log "About to start with-compilation-unit\n")
        (with-module (:module module
                              :function-pass-manager (if *use-function-pass-manager-for-compile-file* (create-function-pass-manager-for-compile-file module)))
          (let* ((*compile-file-pathname* given-input-pathname)
                 (*compile-file-truename* (truename *compile-file-pathname*))
                 (*gv-source-path-name* (jit-make-global-string-ptr (namestring *compile-file-truename*) "source-pathname"))
                 (*gv-source-file-info-handle* (make-gv-source-file-info-handle-in-*the-module*)))
            (with-dibuilder (*the-module*)
              (with-dbg-compile-unit (nil *compile-file-truename*)
                (with-dbg-file-descriptor (nil *compile-file-truename*)
                  (with-load-time-value-unit (ltv-init-fn)
                    (do ((line-number (stream-linenumber sin)
                                      (stream-linenumber sin))
                         (column (stream-column sin)
                                 (stream-column sin))
                         (form (progn (let ((rd (read sin nil eof-value))) rd))
                               (progn (let ((rd (read sin nil eof-value))) rd))))
                        ((eq form eof-value) nil)
                      (let ((*current-lineno* line-number)
                            (*current-column* column))
                        (codegen form)))
                    (let ((main-fn (compile-main-function output-path ltv-init-fn )))
                      (make-boot-function-global-variable *the-module* main-fn)
                      (add-main-function *the-module*))))))
            (cmp-log "About to verify the module\n")
            (cmp-log-dump *the-module*)
            (multiple-value-bind (found-errors error-message)
                (progn
                  (cmp-log "About to verify module prior to writing bitcode\n")
                  (llvm-sys:verify-module *the-module* 'llvm-sys:return-status-action)
                  )
              (if found-errors
		  (progn
		    (format t "Module error: ~a~%" error-message)
		    (break "Verify module found errors")))))))
      module))
