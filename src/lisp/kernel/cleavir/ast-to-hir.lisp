(in-package :clasp-cleavir-ast-to-hir)

(defmethod cleavir-ast-to-hir:compile-ast ((ast clasp-cleavir-ast:debug-message-ast) context)
  (cleavir-ast-to-hir::assert-context ast context 1 1)
  (format t "cleavir-ast-to-hir:compile-ast on debug-message-ast successors: ~a~%" (cleavir-ast-to-hir::successors context))
  (make-instance 'clasp-cleavir-hir:debug-message-instruction 
		 :debug-message (clasp-cleavir-ast:debug-message ast)
		 :successors (cleavir-ast-to-hir::successors context)))

(defmethod cleavir-ast-to-hir:compile-ast ((ast clasp-cleavir-ast:debug-break-ast) context)
  (cleavir-ast-to-hir::assert-context ast context 1 1)
  (format t "cleavir-ast-to-hir:compile-ast on debug-break-ast successors: ~a~%" (cleavir-ast-to-hir::successors context))
  (make-instance 'clasp-cleavir-hir:debug-break-instruction 
		 :successors (cleavir-ast-to-hir::successors context)))


;;; We have to shadow Cleavir's method, as we have to save and load multiple values explicitly.
(defmethod cleavir-ast-to-hir:compile-ast :around
    ((ast cleavir-ast:multiple-value-prog1-ast) context)
  (let ((cleavir-ir:*policy* (cleavir-ast:policy ast))
        (cleavir-ir:*origin* (cleavir-ast:origin ast))
        (cleavir-ir:*dynamic-environment*
          (cleavir-ast-to-hir::dynamic-environment context)))
    (with-accessors ((results cleavir-ast-to-hir::results)
                     (successors cleavir-ast-to-hir::successors))
        context
      (cond ((null (cleavir-ast:form-asts ast)) ; trivial case
             (cleavir-ast-to-hir:compile-ast (cleavir-ast:first-form-ast ast) context))
            ((typep results 'cleavir-ir:values-location) ; hard case
             (let* (;; Values location for the first form. Despite the hoopla, means nothing
                    ;; in the runtime.
                    (mv (cleavir-ir:make-values-location))
                    ;; These two (lexical!) temporaries store the values.
                    ;; They will be typed in hir-to-mir, and are not used for anything else.
                    (nvals-temp (cleavir-ir:new-temporary))
                    (values-temp (cleavir-ir:new-temporary))
                    ;; Our final instruction - load the temporaries back into the values location.
                    (load (clasp-cleavir-hir:make-load-values-instruction
                           (list nvals-temp values-temp) results (first successors)))
                    ;; Now we're set up to compile the rest forms.
                    (next (loop with successor = load
                                for form-ast in (reverse (cleavir-ast:form-asts ast))
                                do (setf successor
                                         (cleavir-ast-to-hir:compile-ast
                                          form-ast
                                          (cleavir-ast-to-hir:clone-context
                                           context
                                           :results '()
                                           :successors (list successor))))
                                finally (return successor)))
                    ;; This instruction stores the result(s) of the first form into the temporaries.
                    (store (clasp-cleavir-hir:make-save-values-instruction
                            mv (list nvals-temp values-temp) next)))
               ;; Finally, we can compile the first form.
               (cleavir-ast-to-hir:compile-ast
                (cleavir-ast:first-form-ast ast)
                (cleavir-ast-to-hir:clone-context
                 context
                 :results mv
                 :successors (list store)))))
            (t ; simple case - results are just lexical locations - no special effort needed
             (call-next-method))))))

(defmethod cleavir-ast-to-hir:compile-ast ((ast clasp-cleavir-ast:multiple-value-foreign-call-ast) context)
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
       context))))

(defmethod cleavir-ast-to-hir:compile-ast ((ast clasp-cleavir-ast:foreign-call-ast) context)
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
	      (let ((result-temp (cleavir-ir:new-temporary)))
		(make-instance 'clasp-cleavir-hir:foreign-call-instruction
                               :function-name (clasp-cleavir-ast:function-name ast)
                               :foreign-types (clasp-cleavir-ast:foreign-types ast)
                               :inputs temps
                               :outputs (list result-temp)
                               :successors successors))))
	 (2
	  (error "FOREIGN-call-AST appears in a Boolean context.")))
       context))))

(defmethod cleavir-ast-to-hir:compile-ast ((ast clasp-cleavir-ast:foreign-call-pointer-ast) context)
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
	      (let ((result-temp (cleavir-ir:new-temporary)))
		(make-instance 'clasp-cleavir-hir:foreign-call-pointer-instruction
                               :foreign-types (clasp-cleavir-ast:foreign-types ast)
                               :inputs temps
                               :outputs (list result-temp)
                               :successors successors))))
	 (2
	  (error "foreign-call-pointer-AST appears in a Boolean context.")))
       context))))

(defmethod cleavir-ast-to-hir:compile-ast ((ast cc-ast:defcallback-ast) context)
  (let ((closure-temp (cleavir-ir:new-temporary)))
    (cleavir-ast-to-hir:compile-ast
     (cleavir-ast:callee-ast ast)
     (cleavir-ast-to-hir:clone-context
      context
      :results (list closure-temp)
      :successors
      (list (make-instance 'clasp-cleavir-hir:defcallback-instruction
              :args (cc-ast:defcallback-args ast)
              :inputs (list closure-temp)
              :successors (cleavir-ast-to-hir::successors context)))))))

(defmethod cleavir-ast-to-hir:compile-ast ((ast cc-ast:header-stamp-case-ast) context)
  (let ((derivable (cleavir-ast-to-hir:compile-ast (cc-ast:derivable-ast ast) context))
        (rack (cleavir-ast-to-hir:compile-ast (cc-ast:rack-ast ast) context))
        (wrapped (cleavir-ast-to-hir:compile-ast (cc-ast:wrapped-ast ast) context))
        (header (cleavir-ast-to-hir:compile-ast (cc-ast:header-ast ast) context))
        (temp (cleavir-ir:new-temporary)))
    (cleavir-ast-to-hir:compile-ast
     (cc-ast:stamp-ast ast)
     (cleavir-ast-to-hir:clone-context
      context
      :results (list temp)
      :successors
      (list (clasp-cleavir-hir:make-header-stamp-case-instruction
             temp derivable rack wrapped header))))))

;; FIXME: export names, deexport make-, have macroexpansion assert context
(cleavir-ast-to-hir::define-compile-functional-ast
    cc-ast:vector-length-ast clasp-cleavir-hir:vector-length-instruction
  (cleavir-ast:arg-ast))
(cleavir-ast-to-hir::define-compile-functional-ast
    cc-ast:displacement-ast clasp-cleavir-hir:displacement-instruction
  (cleavir-ast:arg-ast))
(cleavir-ast-to-hir::define-compile-functional-ast
    cc-ast:displaced-index-offset-ast clasp-cleavir-hir:displaced-index-offset-instruction
  (cleavir-ast:arg-ast))
(cleavir-ast-to-hir::define-compile-functional-ast
    cc-ast:array-total-size-ast clasp-cleavir-hir:array-total-size-instruction
  (cleavir-ast:arg-ast))
(cleavir-ast-to-hir::define-compile-functional-ast
    cc-ast:array-rank-ast clasp-cleavir-hir:array-rank-instruction
  (cleavir-ast:arg-ast))
(cleavir-ast-to-hir::define-compile-functional-ast
    cc-ast:array-dimension-ast clasp-cleavir-hir:array-dimension-instruction
  (cleavir-ast:arg1-ast cleavir-ast:arg2-ast))

(cleavir-ast-to-hir::define-compile-functional-ast
    cc-ast:header-stamp-ast clasp-cleavir-hir:header-stamp-instruction
  (cleavir-ast:arg-ast))
(cleavir-ast-to-hir::define-compile-functional-ast
    cc-ast:rack-stamp-ast clasp-cleavir-hir:rack-stamp-instruction
  (cleavir-ast:arg-ast))
(cleavir-ast-to-hir::define-compile-functional-ast
    cc-ast:wrapped-stamp-ast clasp-cleavir-hir:wrapped-stamp-instruction
  (cleavir-ast:arg-ast))
(cleavir-ast-to-hir::define-compile-functional-ast
    cc-ast:derivable-stamp-ast clasp-cleavir-hir:derivable-stamp-instruction
  (cleavir-ast:arg-ast))

(cleavir-ast-to-hir::define-compile-functional-ast
    cc-ast:vaslist-pop-ast clasp-cleavir-hir:vaslist-pop-instruction
  (cleavir-ast:arg-ast))
(cleavir-ast-to-hir::define-compile-functional-ast
    cc-ast:vaslist-length-ast clasp-cleavir-hir:vaslist-length-instruction
  (cleavir-ast:arg-ast))

(cleavir-ast-to-hir::define-compile-functional-ast
    cc-ast:cas-car-ast clasp-cleavir-hir:cas-car-instruction
  (clasp-cleavir-ast:cmp-ast cleavir-ast:value-ast cleavir-ast:cons-ast))
(cleavir-ast-to-hir::define-compile-functional-ast
    cc-ast:cas-cdr-ast clasp-cleavir-hir:cas-cdr-instruction
  (clasp-cleavir-ast:cmp-ast cleavir-ast:value-ast cleavir-ast:cons-ast))
(cleavir-ast-to-hir::define-compile-functional-ast
    cc-ast:slot-cas-ast clasp-cleavir-hir:slot-cas-instruction
  (clasp-cleavir-ast:cmp-ast cleavir-ast:value-ast
                             cleavir-ast:object-ast cleavir-ast:slot-number-ast))

(defmethod cleavir-ast-to-hir:compile-ast ((ast cc-ast:bind-va-list-ast) context)
  (let ((temp (cleavir-ir:new-temporary)))
    (cleavir-ast-to-hir:compile-ast
     (cc-ast:va-list-ast ast)
     (cleavir-ast-to-hir:clone-context
      context
      :results (list temp)
      :successors
      (list
       (clasp-cleavir-hir:make-bind-va-list-instruction
        (cleavir-ast-to-hir:translate-lambda-list (cleavir-ast:lambda-list ast))
        temp
        (clasp-cleavir-ast:rest-alloc ast)
        (cleavir-ast-to-hir:compile-ast
         (cleavir-ast:body-ast ast)
         context)))))))

(defmethod cleavir-ast-to-hir:compile-ast ((ast cc-ast:acas-ast) context)
  (cleavir-ast-to-hir::assert-context ast context 1 1)
  (let* ((array-temp (cleavir-ir:new-temporary))
         (index-temp (cleavir-ir:new-temporary))
         (cmp-temp (cleavir-ir:new-temporary))
         (new-temp (cleavir-ir:new-temporary))
         (type (cleavir-ast:element-type ast))
         (boxedp (cleavir-ast:boxed-p ast))
         (unboxed (if boxedp
                      (cleavir-ast-to-hir::results context)
                      (list (cleavir-ir:new-temporary))))
         (succ (if boxedp
                   (cleavir-ast-to-hir::successors context)
                   (list (cleavir-ast-to-hir::box-for-type type unboxed context))))
         (acas (make-instance 'clasp-cleavir-hir:acas-instruction
                              :element-type type
                              :simple-p (cleavir-ast:simple-p ast)
                              :boxed-p boxedp
                              :inputs (list array-temp index-temp cmp-temp new-temp)
                              :outputs unboxed
                              :successors succ)))
    (cleavir-ast-to-hir:compile-ast
     (cleavir-ast:array-ast ast)
     (cleavir-ast-to-hir:clone-context
      context
      :results (list array-temp)
      :successors
      (list
       (cleavir-ast-to-hir:compile-ast
        (cleavir-ast:index-ast ast)
        (cleavir-ast-to-hir:clone-context
         context
         :results (list index-temp)
         :successors
         (list
          (if boxedp
              (cleavir-ast-to-hir:compile-ast
               (cc-ast:cmp-ast ast)
               (cleavir-ast-to-hir:clone-context
                context
                :results (list cmp-temp)
                :successors
                (list
                 (cleavir-ast-to-hir:compile-ast
                  (cleavir-ast:value-ast ast)
                  (cleavir-ast-to-hir:clone-context
                   context
                   :results (list new-temp)
                   :successors (list acas))))))
              (let ((cmp-boxed-temp (cleavir-ir:new-temporary))
                    (new-boxed-temp (cleavir-ir:new-temporary)))
                (cleavir-ast-to-hir:compile-ast
                 (cc-ast:cmp-ast ast)
                 (cleavir-ast-to-hir:clone-context
                  context
                  :results (list cmp-boxed-temp)
                  :successors
                  (list
                   (cleavir-ast-to-hir::unbox-for-type
                    type cmp-boxed-temp cmp-temp
                    (cleavir-ast-to-hir:compile-ast
                     (cleavir-ast:value-ast ast)
                     (cleavir-ast-to-hir:clone-context
                      context
                      :results (list new-boxed-temp)
                      :successors
                      (list
                       (cleavir-ast-to-hir::unbox-for-type
                        type new-boxed-temp new-temp
                        acas))))))))))))))))))

(defmethod cleavir-ast-to-hir:compile-ast ((ast clasp-cleavir-ast:precalc-value-reference-ast) context)
  (cleavir-ast-to-hir::assert-context ast context 1 1)
  (let ((index (clasp-cleavir-ast:precalc-value-reference-ast-index ast)))
    (clasp-cleavir-hir:make-precalc-value-instruction
     index
     (first (cleavir-ast-to-hir::results context))
     :successor (first (cleavir-ast-to-hir::successors context))
     :original-object (clasp-cleavir-ast:precalc-value-reference-ast-original-object ast)
     :origin (clasp-cleavir::ensure-origin (cleavir-ast:origin ast) 999990)
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SETF-FDEFINITION-AST.

(defmethod cleavir-ast-to-hir:compile-ast ((ast clasp-cleavir-ast:setf-fdefinition-ast) context)
  (cleavir-ast-to-hir::assert-context ast context 1 1)
  (let ((temp (cleavir-ast-to-hir::make-temp)))
    (cleavir-ast-to-hir:compile-ast
     (cleavir-ast:name-ast ast)
     (cleavir-ast-to-hir:clone-context
      context
      :results (list temp)
      :successors
      (list
       (clasp-cleavir-hir:make-setf-fdefinition-instruction
	temp
	(first (cleavir-ast-to-hir::results context))
	(first (cleavir-ast-to-hir::successors context))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generate a THROW-INSTRUCTION
;;;

(defmethod cleavir-ast-to-hir:compile-ast ((ast clasp-cleavir-ast:throw-ast) context)
  (with-accessors ((successors cleavir-ast-to-hir::successors))
      context
    (let* ((tag-temp (cleavir-ast-to-hir::make-temp))
	   (result-successor 
	    (let* ((new-successor (clasp-cleavir-hir:make-throw-instruction tag-temp))
		   (new-context (cleavir-ast-to-hir:clone-context
                                 context
				 :successors (list new-successor))))
	      (cleavir-ast-to-hir:compile-ast (clasp-cleavir-ast:result-ast ast) new-context))))
      (cleavir-ast-to-hir:compile-ast (clasp-cleavir-ast:tag-ast ast)
				      (cleavir-ast-to-hir:clone-context
                                       context
                                       :results (list tag-temp)
                                       :successors (list result-successor))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generate a special variable binding.
;;;

(defmethod cleavir-ast-to-hir:compile-ast ((ast cc-ast:bind-ast) context)
  (let* ((sym (cleavir-ir:new-temporary))
         (old (cleavir-ir:new-temporary))
         (new (cleavir-ir:new-temporary))
         (dynenv-out (cleavir-ir:make-lexical-location '#:bind-dynenv))
         ;; Normal paths out.
         (unbinds (loop for succ in (cleavir-ast-to-hir::successors context)
                        collect (make-instance 'cleavir-ir:local-unwind-instruction
                                               :successors (list succ)
                                               ;; Before this instruction is executed,
                                               ;; the binding is still in place.
                                               ;; So if we, for example, unwind
                                               ;; to this instruction, we don't want
                                               ;; the unwinder to undo the binding-
                                               ;; the instruction will do it fine.
                                               :dynamic-environment dynenv-out)))
         (body
           (cleavir-ast-to-hir:compile-ast (cleavir-ast:body-ast ast)
                                           (cleavir-ast-to-hir:clone-context
                                            context
                                            :successors unbinds
                                            :dynamic-environment dynenv-out)))
         (wrapped-body (make-instance 'clasp-cleavir-hir:bind-instruction
                                      :inputs (list sym new)
                                      :outputs (list old dynenv-out)
                                      :successors (list body))))
    (cleavir-ast-to-hir:compile-ast
     (cleavir-ast:name-ast ast)
     (cleavir-ast-to-hir:clone-context
      context
      :results (list sym)
      :successors
      (list
       (cleavir-ast-to-hir:compile-ast
        (cleavir-ast:value-ast ast)
        (cleavir-ast-to-hir:clone-context
         context
         :results (list new)
         :successors (list wrapped-body))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UNWIND-PROTECT
;;;

(defmethod cleavir-ast-to-hir:compile-ast ((ast cc-ast:unwind-protect-ast) context)
  (let* ((thunk (cleavir-ir:new-temporary))
         (dynenv-out (cleavir-ir:make-lexical-location
                      '#:unwind-protect-dynenv))
         ;; Normal paths out
         (protects (loop for succ in (cleavir-ast-to-hir::successors context)
                         collect (make-instance 'cleavir-ir:local-unwind-instruction
                                   :successors (list succ)
                                   :dynamic-environment dynenv-out)))
         (body
           (cleavir-ast-to-hir:compile-ast (cleavir-ast:body-ast ast)
                                           (cleavir-ast-to-hir:clone-context
                                            context
                                            :successors protects
                                            :dynamic-environment dynenv-out)))
         (wrapped-body (make-instance 'clasp-cleavir-hir:unwind-protect-instruction
                                      :inputs (list thunk)
                                      :outputs (list dynenv-out)
                                      :successors (list body))))
    (cleavir-ast-to-hir:compile-ast
     (cc-ast:cleanup-ast ast)
     (cleavir-ast-to-hir:clone-context
      context
      :results (list thunk)
      :successors (list wrapped-body)))))
