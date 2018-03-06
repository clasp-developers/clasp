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
  (let* ((ll (cleavir-ast-to-hir::translate-lambda-list (cleavir-ast:lambda-list ast)))
	 (enter (clasp-cleavir-hir:make-named-enter-instruction ll (clasp-cleavir-ast:lambda-name ast) :origin (cleavir-ast:origin ast)))
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
  (cleavir-ast-to-hir::assert-context ast context 1 1)
  (format t "cleavir-ast-to-hir::compile-ast on debug-message-ast successors: ~a~%" (cleavir-ast-to-hir::successors context))
  (make-instance 'clasp-cleavir-hir:debug-message-instruction 
		 :debug-message (clasp-cleavir-ast:debug-message ast)
		 :successors (cleavir-ast-to-hir::successors context)))



(defmethod cleavir-ast-to-hir::compile-ast ((ast clasp-cleavir-ast:multiple-value-foreign-call-ast) context)
  (with-accessors ((results cleavir-ast-to-hir::results)
		   (successors cleavir-ast-to-hir::successors))
      context
    (let* ((all-args (clasp-cleavir-ast:argument-asts ast))
	   (temps (cleavir-ast-to-hir::make-temps all-args)))
      (check-type (clasp-cleavir-ast:function-name ast) string)
      (cleavir-ast-to-hir:compile-arguments
       all-args
       temps
       (ecase (length successors)
	 (1
	  (if (typep results 'cleavir-ir:values-location)
                (make-instance 'clasp-cleavir-hir:multiple-value-foreign-call-instruction
                               :function-name (clasp-cleavir-ast:function-name ast)
                               :inputs temps
                               :outputs (list results)
                               :successors successors)
	      (let* ((values-temp (make-instance 'cleavir-ir:values-location)))
		(make-instance 'clasp-cleavir-hir:multiple-value-foreign-call-instruction
                               :function-name (clasp-cleavir-ast:function-name ast)
                               :inputs temps
                               :outputs (list values-temp)
                               :successors
                               (list (cleavir-ir:make-multiple-to-fixed-instruction
                                      values-temp results (first successors)))))))
	 (2
	  (error "MULTIPLE-VALUE-FOREIGN-CALL-AST appears in a Boolean context.")))
       (cleavir-ast-to-hir::invocation context)))))

(defmethod cleavir-ast-to-hir::compile-ast ((ast clasp-cleavir-ast:foreign-call-ast) context)
  (cleavir-ast-to-hir::assert-context ast context 1 1)
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
	  (if (typep (car results) 'cleavir-ir:lexical-location)
              (make-instance 'clasp-cleavir-hir:foreign-call-instruction
                             :function-name (clasp-cleavir-ast:function-name ast)
                             :foreign-types (clasp-cleavir-ast:foreign-types ast)
                             :inputs temps
                             :outputs results
                             :successors successors)
	      (let* ((result-temp (cleavir-ir:new-temporary))) ;; make-instance 'cleavir-ir:lexical-location)))
		(make-instance 'clasp-cleavir-hir:foreign-call-instruction
                               :function-name (clasp-cleavir-ast:function-name ast)
                               :foreign-types (clasp-cleavir-ast:foreign-types ast)
                               :inputs temps
                               :outputs (list result-temp)
                               :successors successors))))
	 (2
	  (error "FOREIGN-call-AST appears in a Boolean context.")))
       (cleavir-ast-to-hir::invocation context)))))

(defmethod cleavir-ast-to-hir::compile-ast ((ast clasp-cleavir-ast:foreign-call-pointer-ast) context)
  (cleavir-ast-to-hir::assert-context ast context 1 1)
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
	  (if (typep (car results) 'cleavir-ir:lexical-location)
              (make-instance 'clasp-cleavir-hir:foreign-call-pointer-instruction
                             :foreign-types (clasp-cleavir-ast:foreign-types ast)
                             :inputs temps
                             :outputs results
                             :successors successors)
	      (let* ((result-temp (cleavir-ir:new-temporary))) ;; make-instance 'cleavir-ir:lexical-location)))
		(make-instance 'clasp-cleavir-hir:foreign-call-pointer-instruction
                               :foreign-types (clasp-cleavir-ast:foreign-types ast)
                               :inputs temps
                               :outputs (list result-temp)
                               :successors successors))))
	 (2
	  (error "foreign-call-pointer-AST appears in a Boolean context.")))
       (cleavir-ast-to-hir::invocation context)))))


(defmethod cleavir-ast-to-hir:compile-ast ((ast cc-ast:vector-length-ast) context)
  (cleavir-ast-to-hir::assert-context ast context 1 1)
  (let ((temp (cleavir-ir:new-temporary)))
    (cleavir-ast-to-hir::compile-ast
     (cc-ast:vl-ast-vector ast)
     (cleavir-ast-to-hir::context
      (list temp)
      (list (clasp-cleavir-hir:make-vector-length-instruction
             temp (first (cleavir-ast-to-hir::results context))
             (first (cleavir-ast-to-hir::successors context))))
      (cleavir-ast-to-hir::invocation context)))))


(defmethod cleavir-ast-to-hir:compile-ast ((ast cc-ast:displacement-ast) context)
  (cleavir-ast-to-hir::assert-context ast context 1 1)
  (let ((temp (cleavir-ir:new-temporary)))
    (cleavir-ast-to-hir::compile-ast
     (cc-ast:displacement-ast-mdarray ast)
     (cleavir-ast-to-hir::context
      (list temp)
      (list (clasp-cleavir-hir:make-displacement-instruction
             temp (first (cleavir-ast-to-hir::results context))
             (first (cleavir-ast-to-hir::successors context))))
      (cleavir-ast-to-hir::invocation context)))))


(defmethod cleavir-ast-to-hir:compile-ast ((ast cc-ast:displaced-index-offset-ast) context)
  (cleavir-ast-to-hir::assert-context ast context 1 1)
  (let ((temp (cleavir-ir:new-temporary)))
    (cleavir-ast-to-hir::compile-ast
     (cc-ast:displaced-index-offset-ast-mdarray ast)
     (cleavir-ast-to-hir::context
      (list temp)
      (list (clasp-cleavir-hir:make-displaced-index-offset-instruction
             temp (first (cleavir-ast-to-hir::results context))
             (first (cleavir-ast-to-hir::successors context))))
      (cleavir-ast-to-hir::invocation context)))))


(defmethod cleavir-ast-to-hir:compile-ast ((ast cc-ast:array-total-size-ast) context)
  (cleavir-ast-to-hir::assert-context ast context 1 1)
  (let ((temp (cleavir-ir:new-temporary)))
    (cleavir-ast-to-hir::compile-ast
     (cc-ast:array-total-size-ast-mdarray ast)
     (cleavir-ast-to-hir::context
      (list temp)
      (list (clasp-cleavir-hir:make-array-total-size-instruction
             temp (first (cleavir-ast-to-hir::results context))
             (first (cleavir-ast-to-hir::successors context))))
      (cleavir-ast-to-hir::invocation context)))))


(defmethod cleavir-ast-to-hir:compile-ast ((ast cc-ast:array-rank-ast) context)
  (cleavir-ast-to-hir::assert-context ast context 1 1)
  (let ((temp (cleavir-ir:new-temporary)))
    (cleavir-ast-to-hir::compile-ast
     (cc-ast:array-rank-ast-mdarray ast)
     (cleavir-ast-to-hir::context
      (list temp)
      (list (clasp-cleavir-hir:make-array-rank-instruction
             temp (first (cleavir-ast-to-hir::results context))
             (first (cleavir-ast-to-hir::successors context))))
      (cleavir-ast-to-hir::invocation context)))))


(defmethod cleavir-ast-to-hir:compile-ast ((ast cc-ast:array-dimension-ast) context)
  (cleavir-ast-to-hir::assert-context ast context 1 1)
  (let ((mdarray-temp (cleavir-ir:new-temporary))
        (axis-temp (cleavir-ir:new-temporary)))
    (cleavir-ast-to-hir::compile-ast
     (cc-ast:array-dimension-ast-mdarray ast)
     (cleavir-ast-to-hir::context
      (list mdarray-temp)
      (list (cleavir-ast-to-hir::compile-ast
             (cc-ast:array-dimension-ast-axis ast)
             (cleavir-ast-to-hir::context
              (list axis-temp)
              (list (clasp-cleavir-hir:make-array-dimension-instruction
                     mdarray-temp axis-temp
                     (first (cleavir-ast-to-hir::results context))
                     (first (cleavir-ast-to-hir::successors context))))
              (cleavir-ast-to-hir::invocation context))))
      (cleavir-ast-to-hir::invocation context)))))


(defmethod cleavir-ast-to-hir:compile-ast ((ast cc-ast:bind-va-list-ast) context)
  (let ((temp (cleavir-ir:new-temporary)))
    (cleavir-ast-to-hir:compile-ast
     (cc-ast:va-list-ast ast)
     (cleavir-ast-to-hir::context
      (list temp)
      (list
       (clasp-cleavir-hir:make-bind-va-list-instruction
        (cleavir-ast-to-hir:translate-lambda-list (cleavir-ast:lambda-list ast))
        temp
        (cleavir-ast-to-hir:compile-ast
         (cleavir-ast:body-ast ast)
         context)))
      (cleavir-ast-to-hir::invocation context)))))



(defmethod cleavir-ast-to-hir::compile-ast ((ast clasp-cleavir-ast:precalc-value-reference-ast) context)
  (cleavir-ast-to-hir::assert-context ast context 1 1)
  (let* ((index (clasp-cleavir-ast:precalc-value-reference-ast-index ast))
         (input (cleavir-ir:make-immediate-input index)))
    (check-type index clasp-cleavir::literal)
    (clasp-cleavir-hir:make-precalc-value-instruction
     input
     (first (cleavir-ast-to-hir::results context))
     :successor (first (cleavir-ast-to-hir::successors context))
     :original-object (clasp-cleavir-ast:precalc-value-reference-ast-original-object ast)
     )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stores the current landing pad
;;;
;;; If *landing-pad* is set then all funcalls are converted to invoke
;;; layout-procedure resets *landing-pad*
;;;
(defvar *landing-pad* nil)


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
  (cleavir-ast-to-hir::assert-context ast context 1 1)
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
