(in-package :clasp-cleavir-ast-to-hir)



(defmethod cleavir-ast-to-hir:compile-ast ((ast clasp-cleavir-ast:precalc-symbol-reference-ast) context)
  (cleavir-ast-to-hir:check-context-for-one-value-ast context)
  (let ((temp (cleavir-ast-to-hir:make-temp)))
    (cleavir-ast-to-hir:compile-ast
     (clasp-cleavir-ast:precalc-symbol-reference-vector-ast ast)
     (cleavir-ast-to-hir:context (list temp)
				 (list (clasp-cleavir-hir:make-precalc-symbol-instruction
					temp
					(cleavir-ir:make-immediate-input (clasp-cleavir-ast:precalc-symbol-reference-index ast))
					(first (cleavir-ast-to-hir:results context))
					:successor (first (cleavir-ast-to-hir:successors context))
					:original-object (clasp-cleavir-ast:precalc-symbol-reference-ast-original-object ast)
					))
				 (cleavir-ast-to-hir:invocation context)))))



(defmethod cleavir-ast-to-hir:compile-ast ((ast clasp-cleavir-ast:precalc-value-reference-ast) context)
  (cleavir-ast-to-hir:check-context-for-one-value-ast context)
  (let ((temp (cleavir-ast-to-hir:make-temp)))
    (cleavir-ast-to-hir:compile-ast
     (clasp-cleavir-ast:precalc-value-reference-vector-ast ast)
     (cleavir-ast-to-hir:context (list temp)
				 (list (clasp-cleavir-hir:make-precalc-value-instruction
					temp
					(cleavir-ir:make-immediate-input (clasp-cleavir-ast:precalc-value-reference-index ast))
					(first (cleavir-ast-to-hir:results context))
					:successor (first (cleavir-ast-to-hir:successors context))
					:original-object (clasp-cleavir-ast:precalc-value-reference-ast-original-object ast)
					))
				 (cleavir-ast-to-hir:invocation context)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stores the current landing pad
;;;
;;; If *landing-pad* is set then all funcalls are converted to invoke
;;; layout-procedure resets *landing-pad*
;;;
(defvar *landing-pad* nil)

(defmethod cleavir-ast-to-hir:compile-ast ((ast clasp-cleavir-ast:unwind-protect-ast) context)
    (with-accessors ((results cleavir-ast-to-hir:results)
		     (successors cleavir-ast-to-hir:successors)
		     (invocation cleavir-ast-to-hir:invocation))
	context
  ;;  (cleavir-ast-to-hir:check-context-for-one-value-ast context)
      (let* ((save-temp (cleavir-ast-to-hir:make-temp))
	     (restore-mv (clasp-cleavir-hir:make-restore-multiple-values-return-instruction save-temp results (first successors)))
	     (cleanup-form (cleavir-ast-to-hir:compile-ast (clasp-cleavir-ast:cleanup-ast ast) 
							   (cleavir-ast-to-hir:context nil
										       (list restore-mv)
										       (cleavir-ast-to-hir:invocation context))))
	     (values-temp (make-instance 'cleavir-ir:values-location))
	     (save-mv (clasp-cleavir-hir:make-save-multiple-values-return-instruction values-temp save-temp cleanup-form))
	     (cleanup-instr (clasp-cleavir-hir:make-cleanup-instruction save-mv))
	     (landing-pad (clasp-cleavir-hir:make-landing-pad-instruction cleanup-instr))
	     (*landing-pad* landing-pad)
	     ;; Compile the protected form but use invokes rather than calls
	     (protected-form (cleavir-ast-to-hir:compile-ast (clasp-cleavir-ast:protected-ast ast)
							     (cleavir-ast-to-hir:context values-temp
											 (list save-mv)
											 (cleavir-ast-to-hir:invocation context))))
	     (try-instr (clasp-cleavir-hir:make-try-instruction 123456789 protected-form))
	     )
	try-instr)))



(defun make-call/invoke (&key inputs outputs successors)
  (if *landing-pad*
      (make-instance 'clasp-cleavir-hir:invoke-instruction
		     :inputs inputs
		     :outputs outputs
		     :successors (list (first successors) *landing-pad*))
      (make-instance 'cleavir-ir:funcall-instruction
		     :inputs inputs
		     :outputs outputs
		     :successors successors)))
      


(defmethod cleavir-ast-to-hir:compile-ast ((ast cleavir-ast:call-ast) context)
  (with-accessors ((results cleavir-ast-to-hir:results)
		   (successors cleavir-ast-to-hir:successors))
      context
    (let* ((all-args (cons (cleavir-ast:callee-ast ast)
			   (cleavir-ast:argument-asts ast)))
	   (temps (cleavir-ast-to-hir:make-temps all-args)))
      (cleavir-ast-to-hir:compile-arguments
       all-args
       temps
       (ecase (length successors)
	 (1
	  (if (typep results 'cleavir-ir:values-location)
	      (make-call/invoke
		:inputs temps
		:outputs (list results)
		:successors successors)
	      (let* ((values-temp (make-instance 'cleavir-ir:values-location)))
		(make-call/invoke
		  :inputs temps
		  :outputs (list values-temp)
		  :successors
		  (list (cleavir-ir:make-multiple-to-fixed-instruction
			 values-temp results (first successors)))))))
	 (2
	  (let* ((temp (cleavir-ir:new-temporary))
		 (values-temp (make-instance 'cleavir-ir:values-location))
		 (false (cleavir-ir:make-constant-input nil)))
	    (make-call/invoke
	      :inputs temps
	      :outputs (list values-temp)
	      :successors
	      (list (cleavir-ir:make-multiple-to-fixed-instruction
		     values-temp
		     (list temp)
		     (make-instance 'cleavir-ir:eq-instruction
		       :inputs (list temp false)
		       :outputs '()
		       :successors (reverse successors))))))))
       (cleavir-ast-to-hir:invocation context)))))


#||	 
	 (
	 )
    
  (let* ((restore-mv) (restore-mv (cleanup-branch) (cleavir-ast-to-hir:compile-ast (clasp-cleavir-ast:cleanup-ast ast) context)
	 (protected-branch (cleavir-ast-to-hir:compile-ast (clasp-cleavir-ast:protected-ast ast) (context '()
													  ()))
  (error "Convert the unwind-protect into a HIR try/cleanup/catch")
  (cleavir-ast-to-hir:compile-ast (cleavir-ast:protected-ast ast) context)
  (error "Save the multiple-values return")
  (cleavir-ast-to-hir:compile-ast (cleavir-ast:cleanup-ast ast) context)
  (error "restore the multiple-values return"))


||#
