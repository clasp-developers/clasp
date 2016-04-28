(in-package :clasp-cleavir-ast-to-hir)


;;; This implementation of bind-ast works if there are no non-local exits
;;; out of the scope of the bind-ast but fails if there are.
;;; There doesn't seem to be a way to unwind special variable bindings
;;; unless I doesn't work when there are non-local exits out of the scope
;;; of the bind-ast
#+(or)(defmethod cleavir-ast-to-hir::compile-ast ((ast cleavir-ast:bind-ast) context)
        (let* ((symbol-ast (cleavir-ast::symbol-ast ast))
               (value-ast (cleavir-ast:value-ast ast))
               (body-ast (cleavir-ast:body-ast ast))
               (invocation (cleavir-ast-to-hir:invocation context))
               (symbol-temp (cleavir-ast-to-hir:make-temp))
               (location-temp (cleavir-ast-to-hir:make-temp))
               (pop-instruction (clasp-cleavir-hir:make-pop-special-binding-instruction
                                 symbol-temp
                                 :successor (first (cleavir-ast-to-hir::successors context))))
               (body-instruction (cleavir-ast-to-hir:compile-ast body-ast (cleavir-ast-to-hir:context
                                                                           (cleavir-ast-to-hir::results context)
                                                                           (list pop-instruction)
                                                                           invocation)))
               (push-instruction (clasp-cleavir-hir:make-push-special-binding-instruction
                                  symbol-temp location-temp
                                  :successor body-instruction))
               (value-instruction (cleavir-ast-to-hir:compile-ast value-ast (cleavir-ast-to-hir:context
                                                                             (list location-temp)
                                                                             (list push-instruction)
                                                                             invocation)))
               (symbol-instruction (cleavir-ast-to-hir:compile-ast
                                    symbol-ast
                                    (cleavir-ast-to-hir:context
                                     (list symbol-temp)
                                     (list value-instruction)
                                     invocation))))
          symbol-instruction))


;;; The logic of this method is a bit twisted.  The reason is that we
;;; must create the ENTER-INSTRUCTION before we compile the body of
;;; the FUNCTION-AST.  The reason for that is that the
;;; ENTER-INSTRUCTION should be the value of the INVOCATION of the
;;; context when the body is compiled.  On the other hand, the result
;;; of compiling the body must be the successor of the ENTER-INSTRUCTION.
;;;
;;; We solve this problem by creating the ENTER-INSTRUCTION with a
;;; dummy successor.  Once the body has been compiled, we call
;;; REINITIALIZE-INSTANCE on the ENTER-INSTRUCTION to set the slots to
;;; their final values.
(defmethod cleavir-ast-to-hir::compile-ast ((ast clasp-cleavir-ast:named-function-ast) context)
  (cleavir-ast-to-hir::check-context-for-one-value-ast context)
  (let* ((ll (cleavir-ast-to-hir::translate-lambda-list (cleavir-ast:lambda-list ast)))
	 (enter (clasp-cleavir-hir:make-named-enter-instruction ll (clasp-cleavir-ast:lambda-name ast)))
	 (values (cleavir-ir:make-values-location))
	 (return (cleavir-ir:make-return-instruction (list values)))
	 (body-context (cleavir-ast-to-hir::context values (list return) enter))
	 (body (cleavir-ast-to-hir::compile-ast (cleavir-ast:body-ast ast) body-context)))
    (reinitialize-instance enter :successors (list body))
    (cleavir-ir:make-enclose-instruction
     (first (cleavir-ast-to-hir::results context))
     (first (cleavir-ast-to-hir::successors context))
     enter)))




(defmethod cleavir-ast-to-hir::compile-ast ((ast clasp-cleavir-ast:debug-message-ast) context)
  (cleavir-ast-to-hir::check-context-for-one-value-ast context)
  (format t "cleavir-ast-to-hir::compile-ast on debug-message-ast successors: ~a~%" (cleavir-ast-to-hir::successors context))
  (make-instance 'clasp-cleavir-hir:debug-message-instruction 
		 :debug-message (clasp-cleavir-ast:debug-message ast)
		 :successors (cleavir-ast-to-hir::successors context)))



(defmethod cleavir-ast-to-hir::compile-ast ((ast clasp-cleavir-ast:intrinsic-call-ast) context)
  (with-accessors ((results cleavir-ast-to-hir::results)
		   (successors cleavir-ast-to-hir::successors))
      context
    (let* ((all-args (clasp-cleavir-ast:argument-asts ast))
	   (temps (cleavir-ast-to-hir::make-temps all-args)))
      (cleavir-ast-to-hir:compile-arguments
       all-args
       temps
       (ecase (length successors)
	 (1
	  (if (typep results 'cleavir-ir:values-location)
	      (make-instance 'clasp-cleavir-hir:intrinsic-call-instruction
                             :function-name (clasp-cleavir-ast:function-name ast)
                             :inputs temps
                             :outputs (list results)
                             :successors successors)
	      (let* ((values-temp (make-instance 'cleavir-ir:values-location)))
		(make-instance 'clasp-cleavir-hir:intrinsic-call-instruction
                               :function-name (clasp-cleavir-ast:function-name ast)
                               :inputs temps
                               :outputs (list values-temp)
                               :successors
                               (list (cleavir-ir:make-multiple-to-fixed-instruction
                                      values-temp results (first successors)))))))
	 (2
	  (error "INTRINSIC-CALL-AST appears in a Boolean context.")))
       (cleavir-ast-to-hir::invocation context)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile precalculated value AST nodes to HIR
;;;
(defmethod cleavir-ast-to-hir::compile-ast ((ast clasp-cleavir-ast:precalc-symbol-reference-ast) context)
  (cleavir-ast-to-hir::check-context-for-one-value-ast context)
  (clasp-cleavir-hir:make-precalc-symbol-instruction
   (cleavir-ir:make-immediate-input (clasp-cleavir-ast:precalc-symbol-reference-index ast))
   (first (cleavir-ast-to-hir::results context))
   :successor (first (cleavir-ast-to-hir::successors context))
   :original-object (clasp-cleavir-ast:precalc-symbol-reference-ast-original-object ast)
   ))


(defmethod cleavir-ast-to-hir::compile-ast ((ast clasp-cleavir-ast:precalc-value-reference-ast) context)
  (cleavir-ast-to-hir::check-context-for-one-value-ast context)
  (clasp-cleavir-hir:make-precalc-value-instruction
   (cleavir-ir:make-immediate-input (clasp-cleavir-ast:precalc-value-reference-index ast))
   (first (cleavir-ast-to-hir::results context))
   :successor (first (cleavir-ast-to-hir::successors context))
   :original-object (clasp-cleavir-ast:precalc-value-reference-ast-original-object ast)
   ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stores the current landing pad
;;;
;;; If *landing-pad* is set then all funcalls are converted to invoke
;;; layout-procedure resets *landing-pad*
;;;
(defvar *landing-pad* nil)

#+(or)
(defmethod cleavir-ast-to-hir::compile-ast ((ast clasp-cleavir-ast:unwind-protect-ast) context)
  (with-accessors ((results cleavir-ast-to-hir::results)
		   (successors cleavir-ast-to-hir::successors)
		   (invocation cleavir-ast-to-hir::invocation))
      context
    ;;  (cleavir-ast-to-hir::check-context-for-one-value-ast context)
    (let* ((save-temp (cleavir-ast-to-hir::make-temp))
	   (restore-mv (clasp-cleavir-hir:make-restore-multiple-values-return-instruction save-temp results (first successors)))
	   (cleanup-form (cleavir-ast-to-hir::compile-ast
                          (clasp-cleavir-ast:cleanup-ast ast) 
                          (cleavir-ast-to-hir::context nil
                                                       (list restore-mv)
                                                       (cleavir-ast-to-hir::invocation context))))
	   (values-temp (make-instance 'cleavir-ir:values-location))
	   (save-mv (clasp-cleavir-hir:make-save-multiple-values-return-instruction values-temp save-temp cleanup-form))
	   (cleanup-instr (clasp-cleavir-hir:make-cleanup-instruction save-mv))
	   (landing-pad (clasp-cleavir-hir:make-landing-pad-instruction cleanup-instr))
	   (*landing-pad* landing-pad)
	   ;; Compile the protected form but use invokes rather than calls
	   (protected-form (cleavir-ast-to-hir::compile-ast (clasp-cleavir-ast:protected-ast ast)
							   (cleavir-ast-to-hir::context values-temp
										       (list save-mv)
										       (cleavir-ast-to-hir::invocation context))))
	   (try-instr (clasp-cleavir-hir:make-try-instruction 123456789 protected-form))
	   )
      try-instr)))



(defun make-call/invoke (&key inputs outputs successors (landing-pad *landing-pad*))
  (if landing-pad
      (prog1
	(make-instance 'clasp-cleavir-hir:invoke-instruction
		       :inputs inputs
		       :outputs outputs
		       :successors (list (first successors) landing-pad))
	(error "We should not add invoke-instruction here - we should do it at the MIR stage - otherwise it will screw up optimizations"))
      (make-instance 'cleavir-ir:funcall-instruction
		     :inputs inputs
		     :outputs outputs
		     :successors successors)))
      



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SETF-FDEFINITION-AST.

(defmethod cleavir-ast-to-hir::compile-ast ((ast clasp-cleavir-ast:setf-fdefinition-ast) context)
  (cleavir-ast-to-hir::check-context-for-one-value-ast context)
  (let ((temp (cleavir-ast-to-hir::make-temp)))
    (cleavir-ast-to-hir::compile-ast
     (cleavir-ast:name-ast ast)
     (cleavir-ast-to-hir::context
      (list temp)
      (list
       (clasp-cleavir-hir:make-setf-fdefinition-instruction
	temp
	(first (cleavir-ast-to-hir::results context))
	(first (cleavir-ast-to-hir::successors context))))
      (cleavir-ast-to-hir::invocation context)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generate a THROW-INSTRUCTION
;;;
(defmethod cleavir-ast-to-hir::compile-ast ((ast clasp-cleavir-ast:throw-ast) context)
  (with-accessors ((results cleavir-ast-to-hir::results)
		   (successors cleavir-ast-to-hir::successors)
		   (invocation cleavir-ast-to-hir::invocation))
      context
    (let* ((tag-temp (cleavir-ast-to-hir::make-temp))
	   (result-successor 
	    (let* ((new-successor (clasp-cleavir-hir:make-throw-instruction tag-temp))
		   (new-context (cleavir-ast-to-hir::context 
				 results
				 (list new-successor)
				 (cleavir-ast-to-hir::invocation context))))
	      (cleavir-ast-to-hir::compile-ast (clasp-cleavir-ast:result-ast ast) new-context))))
      (cleavir-ast-to-hir::compile-ast (clasp-cleavir-ast:tag-ast ast)
				      (cleavir-ast-to-hir::context (list tag-temp)
					       (list result-successor)
					       (cleavir-ast-to-hir::invocation context))))))
