(cl:in-package #:clasp-cleavir)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (push :passed-env *features*))


(defvar *debug-cleavir* nil)

;;; The first argument to this function is an instruction that has a
;;; single successor.  Whether a GO is required at the end of this
;;; function is determined by the code layout algorithm.  
;;; 
;;; The inputs are forms to be evaluated.  The outputs are symbols
;;; that are names of variables.
(defgeneric translate-simple-instruction (instruction inputs outputs abi))

(defgeneric translate-branch-instruction (instruction inputs outputs successors abi))



(defvar *basic-blocks*)
(defvar *ownerships*)
(defvar *tags*)
(defvar *vars*)


(defvar *entry-irbuilder*)

(setf (fdefinition 'cleavir-primop:call-with-variable-bound) 
      (fdefinition 'core:call-with-variable-bound))

(defun translate-datum (datum)
  (if (typep datum 'cleavir-ir:constant-input)
      (let* ((value (cleavir-ir:value datum))
	     (ltv-index (cmp:codegen-literal nil value))
	     (ltv-ref (cmp:irc-intrinsic "cc_loadTimeValueReference"
					 (if cmp:*generate-compile-file-load-time-values*
					     cmp:*load-time-value-holder-global-var*
					     cmp:*run-time-value-holder-global-var*)
					 (%size_t ltv-index))))
	ltv-ref)
      (let ((var (gethash datum *vars*)))
	(when (null var)
	  (cond
	    ;; Do nothing for values-location - there is only one
	    ((typep datum 'cleavir-ir:values-location)
	     #+(or)(setf var (alloca-mv-struct (string (gensym "V")))) )
	    ((typep datum 'cleavir-ir:immediate-input)
	     (setf var (%size_t (cleavir-ir:value datum))))
	    ((typep datum 'cleavir-ir:dynamic-lexical-location)
	     (setf var (alloca-t* (string (cleavir-ir:name datum)))))
	    ((typep datum 'cleavir-ir:static-lexical-location)
	     (setf var (alloca-t* (string (cleavir-ir:name datum)))))
	    #+(or)((typep datum 'cleavir-ir:load-time-value-input)
		   (format t "load-time-value-input - what does the datum look like: ~a~%" datum)
		   (warn "Get the load-time-value-input and setf var"))
	    (t (error "translate datum: ~a~%" datum)))
	  (setf (gethash datum *vars*) var))
	var)))

(defun translate-lambda-list-item (item)
  (cond ((symbolp item)
	 item)
	((consp item)
	 (ecase (length item)
	   (2 (list (translate-datum (first item))
		    nil
		    (translate-datum (second item))))
	   (3 (list (list (first item)
			  (translate-datum (second item)))
		    nil
		    (translate-datum (third item))))))
	(t
	 (translate-datum item))))

(defun translate-lambda-list (lambda-list)
  (mapcar #'translate-lambda-list-item lambda-list))

(defun layout-basic-block (basic-block abi)
  (destructuring-bind (first last owner) basic-block
    (declare (ignore owner))
    (cc-dbg-when *debug-log*
			 (format *debug-log* "- - - -  BEGIN layout-basic-block  owner: ~a~%" (cc-mir:describe-mir owner))
			 (loop for instruction = first
			    then (first (cleavir-ir:successors instruction))
			    until (eq instruction last)
			    do (format *debug-log* "     ~a~%" (cc-mir:describe-mir instruction))))
    (loop for instruction = first
       then (first (cleavir-ir:successors instruction))
       for inputs = (cleavir-ir:inputs instruction)
       for input-vars = (mapcar #'translate-datum inputs)
       for outputs = (cleavir-ir:outputs instruction)
       for output-vars = (mapcar #'translate-datum outputs)
       until (eq instruction last)
#||       do (cc-dbg-when *debug-log*
		       (format *debug-log* "     ~a~%" (cc-mir:describe-mir instruction)))
||#
       collect (translate-simple-instruction
		instruction input-vars output-vars abi))
    (let* ((inputs (cleavir-ir:inputs last))
	   (input-vars (mapcar #'translate-datum inputs))
	   (outputs (cleavir-ir:outputs last))
	   (output-vars (mapcar #'translate-datum outputs))
	   (successors (cleavir-ir:successors last))
	   (successor-tags (loop for successor in successors
			      collect (gethash successor *tags*))))
      (cc-dbg-when *debug-log*
		   (format *debug-log* "     ~a~%" (cc-mir:describe-mir last)))
      (if (= (length successors) 1)
	  (list (translate-simple-instruction
		 last input-vars output-vars abi)
		(if (typep (second basic-block) 'cleavir-ir:unwind-instruction)
		    (cmp:irc-unreachable)
		    (cmp:irc-br (gethash (first successors) *tags*))))
	  (list (translate-branch-instruction
		 last input-vars output-vars successor-tags abi)))
      (cc-dbg-when *debug-log*
		   (format *debug-log* "- - - -  END layout-basic-block  owner: ~a:~a   -->  ~a~%" (cleavir-ir-gml:label owner) (clasp-cleavir:instruction-gid owner) basic-block))
      )))

(defun get-or-create-lambda-name (instr)
  (if (typep instr 'cc-mir:enter-instruction)
      (clasp-cleavir-hir:lambda-name instr)
      'TOP-LEVEL))

(defun layout-procedure (initial-instruction abi)
  ;; I think this removes every basic-block that
  ;; isn't owned by this initial-instruction
  (let* ((clasp-cleavir-ast-to-hir:*landing-pad* nil)
	 (basic-blocks (remove initial-instruction
			       *basic-blocks*
			       :test-not #'eq :key #'third))
	 ;; Hypothesis: This finds the first basic block
	 (first (find initial-instruction basic-blocks
		      :test #'eq :key #'first))
	 ;; This gathers the rest of the basic blocks
	 (rest (remove first basic-blocks :test #'eq))
	 (lambda-name (get-or-create-lambda-name initial-instruction)))
    ;; HYPOTHESIS: This builds a function with no arguments
    ;; that will enclose and set up other functions with arguments
    (let* ((main-fn-name (format nil "cl->~a" lambda-name))
	   (cmp:*current-function-name* main-fn-name)
	   (cmp:*gv-current-function-name* (cmp:jit-make-global-string-ptr cmp:*current-function-name* "fn-name"))
	   (fn (llvm-sys:function-create
		cmp:+fn-prototype+
		'llvm-sys:internal-linkage
		(cmp:jit-function-name cmp:*current-function-name*)
		cmp:*the-module*))
	   (cmp:*current-function* fn)
	   (entry-block (cmp:irc-basic-block-create "entry" fn))
	   (*current-function-entry-basic-block* entry-block)
	   (*function-current-multiple-value-array-address* nil)
	   (*entry-irbuilder* (llvm-sys:make-irbuilder cmp:*llvm-context*))
	   (body-irbuilder (llvm-sys:make-irbuilder cmp:*llvm-context*))
	   (body-block (cmp:irc-basic-block-create "body")))
      (cc-dbg-when *debug-log*
		   (format *debug-log* "------------ BEGIN layout-procedure ~a~%" (llvm-sys:get-name fn))
		   (format *debug-log* "   basic-blocks for procedure~%")
		   (dolist (bb basic-blocks)
		     (destructuring-bind (first last owner) bb
		       (format *debug-log* "basic-block owner: ~a:~a   -->  ~a~%" (cleavir-ir-gml:label owner) (clasp-cleavir:instruction-gid owner) bb)
		       (loop for instruction = first
			  then (first (cleavir-ir:successors instruction))
			  until (eq instruction last)
			  do (format *debug-log* "     ~a~%" (cc-mir:describe-mir instruction)))
		       (format *debug-log* "     ~a~%" (cc-mir:describe-mir last)))))
      (let ((args (llvm-sys:get-argument-list fn)))
	(mapcar #'(lambda (arg argname) (llvm-sys:set-name arg argname))
		(llvm-sys:get-argument-list fn) cmp:+fn-prototype-argument-names+)
	;; Set the first argument attribute to be sret
	(if args
	    (let ((attribute-set (llvm-sys:attribute-set-get cmp:*llvm-context* 1 (list 'llvm-sys:attribute-struct-ret))))
	      (llvm-sys:add-attr (first args) attribute-set))))
      ;; Create a basic-block for every remaining tag
      (loop for block in rest
	 for instruction = (first block)
	 do (progn
	      #+(or)(format t "Creating basic block for rest instruction: ~a~%" instruction)
	      (setf (gethash instruction *tags*) (cmp:irc-basic-block-create "tag"))))
      (llvm-sys:set-insert-point-basic-block *entry-irbuilder* entry-block)
      ;; HYPOTHESIS: bind variables for every var owned by this
      ;; initial instruction and that are NOT outputs of the initial
      ;; instruction (passed arguments)  I think I should use passed arguments
      ;; to create allocas for them.
      (cmp:with-irbuilder (*entry-irbuilder*)
	(cmp:irc-low-level-trace :arguments)
	(cmp:with-dbg-function ("repl-FIX"
				:linkage-name "repl-FIX-LINKAGE-NAME"
				:function fn
				:function-type cmp:+fn-prototype+
				:form *form*)
	  (cmp:with-dbg-lexical-block (*form*)
	    #+use-ownerships(loop for var being each hash-key of *ownerships*
		     using (hash-value owner)
		     when (and (typep var '(or
					    cleavir-ir:lexical-location
					    cleavir-ir:values-location))
			       (eq owner initial-instruction)
			       #+(or)(not (member var (cleavir-ir:outputs
						       initial-instruction))))
		     collect (translate-datum var))
	    (llvm-sys:set-insert-point-basic-block body-irbuilder body-block)
	    (cmp:with-irbuilder (body-irbuilder)
	      (cmp:irc-begin-block body-block)
	      (layout-basic-block first abi)
	      (loop for block in rest
		 for instruction = (first block)
		 do (progn
		      #+(or)(format t "Laying out basic block: ~a~%" block)
		      #+(or)(format t "Inserting basic block for instruction: ~a~%" instruction)
		      (cmp:irc-begin-block (gethash instruction *tags*))
		      (layout-basic-block block abi)))))))
      ;; Finish up by jumping from the entry block to the body block
      (cmp:with-irbuilder (*entry-irbuilder*)
	(cmp:irc-br body-block))
      (cc-dbg-when *debug-log*
		   (format *debug-log* "----------END layout-procedure ~a~%" (llvm-sys:get-name fn)))
      (values fn :function-kind nil lambda-name))))

(defun translate (initial-instruction abi)
  (let* (#+use-ownerships(ownerships
			  (cleavir-hir-transformations:compute-ownerships initial-instruction)))
    (let* (#+use-ownerships(*ownerships* ownerships)
	   (*basic-blocks* (cleavir-basic-blocks:basic-blocks initial-instruction))
	   (*tags* (make-hash-table :test #'eq))
	   (*vars* (make-hash-table :test #'eq)))
      #+use-ownerships(setf *debug-ownerships* *ownerships*)
      (setf *debug-basic-blocks* *basic-blocks*
	    *debug-tags* *tags*
	    *debug-vars* *vars*)
      (cc-dbg-when *debug-log*
		   (let ((mir-pathname (make-pathname :name (format nil "mir~a" *debug-log-index*) :type "gml" :defaults (pathname *debug-log*))))
		     (multiple-value-bind (instruction-ids datum-ids)
			 (cleavir-ir-gml:draw-flowchart initial-instruction (namestring mir-pathname))
		       #+(or)(dolist (bb *basic-blocks*)
			       (destructuring-bind (first last owner) bb
				 (format *debug-log* "basic-block owner: ~a:~a   -->  ~a~%" (cleavir-ir-gml:label owner) (gethash owner instruction-ids) bb)
				 (loop for instruction = first
				    then (first (cleavir-ir:successors instruction))
				    until (eq instruction last)
				    do (format *debug-log* "     ~a:~a~%" (cleavir-ir-gml:label instruction) (gethash instruction instruction-ids)))
				 (format *debug-log* "     ~a:~a~%" (cleavir-ir-gml:label last) (gethash last instruction-ids)))))
		     (format *debug-log* "Wrote mir to: ~a~%" (namestring mir-pathname)))
		   (let ((mir-pathname (make-pathname :name (format nil "mir~a" *debug-log-index*) :type "dot" :defaults (pathname *debug-log*))))
		     (cleavir-ir-graphviz:draw-flowchart initial-instruction (namestring mir-pathname))
		     (format *debug-log* "Wrote mir to: ~a~%" (namestring mir-pathname))))
      (layout-procedure initial-instruction abi )
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-SIMPLE-INSTRUCTION.


(defmethod translate-simple-instruction
    ((instr cc-mir:enter-instruction) inputs outputs (abi abi-x86-64))
  (let* ((fn-args (llvm-sys:get-argument-list cmp:*current-function*))
	 (closed-env-arg (second fn-args))
	 (closed-env-dest (first outputs))
	 (calling-convention (cmp:make-calling-convention
			      :nargs (third fn-args)
			      :register-args (nthcdr 3 fn-args))))
    (llvm-sys:create-store cmp:*irbuilder* closed-env-arg closed-env-dest nil)
    #+(or)(format t " fn-args: ~a~%" fn-args)
    (let* ((lambda-list (cleavir-ir:lambda-list instr))
	   (static-environment-output (first (cleavir-ir:outputs instr)))
	   (args (cdr (cleavir-ir:outputs instr)))
	   (landing-pad (cc-mir:landing-pad instr)))
      (when landing-pad
	(let ((exn.slot (alloca-i8* "exn.slot"))
	      (ehselector.slot (alloca-i32 "ehselector.slot")))
	  (setf (clasp-cleavir:basic-block landing-pad) (clasp-cleavir:create-landing-pad exn.slot ehselector.slot landing-pad *tags*))))
      #+(or)(progn
	      (format t "    outputs: ~s~%" args)
	      (format t "translated outputs: ~s~%" (mapcar (lambda (x) (translate-datum x)) args))
	      (format t "lambda-list: ~a~%" lambda-list))
      (compile-lambda-list-code lambda-list args calling-convention))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:instruction) inputs outputs abi)
  (error "Implement instruction: ~a for abi: ~a~%" instruction abi)
  (format t "--------------- translate-simple-instruction ~a~%" instruction)
  (format t "    inputs: ~a~%" inputs)
  (format t "    outputs: ~a~%" outputs))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:assignment-instruction) inputs outputs abi)
  #+(or)(progn
	  (format t "--------------- translate-simple-instruction assignment-instruction~%")
	  (format t "    inputs: ~a~%" inputs)
	  (format t "    outputs: ~a~%" outputs))
  (let ((load (llvm-sys:create-load-value-twine cmp:*irbuilder* (first inputs) "tmp")))
    (llvm-sys:create-store cmp:*irbuilder* load (first outputs) nil)))


(defun ltv-global ()
  (if cmp:*generate-compile-file-load-time-values*
      cmp:*load-time-value-holder-global-var*
      cmp:*run-time-value-holder-global-var*))


(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:precalc-symbol-instruction) inputs outputs abi)
  (let ((idx (first inputs)))
;;    (format t "translate-simple-instruction (first inputs) --> ~a~%" (first inputs))
    (let ((result (cmp:irc-intrinsic-args "cc_precalcSymbol" (list (ltv-global) idx))))
      (llvm-sys:create-store cmp:*irbuilder* result (first outputs) nil))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:precalc-value-instruction) inputs outputs abi)
  (let ((idx (first inputs)))
    (let ((result (cmp:irc-intrinsic "cc_precalcValue" (ltv-global) idx)))
      (llvm-sys:create-store cmp:*irbuilder* result (first outputs) nil))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fixed-to-multiple-instruction) inputs outputs (abi abi-x86-64))
  ;; Write the first return value into the result
  (with-return-values (return-values abi)
    (%store (%size_t (length inputs)) (number-of-return-values return-values))
    (dotimes (i (length inputs))
      (%store (cmp:irc-load (elt inputs i)) (return-value-elt return-values i)))
    #+(or)(cmp:irc-intrinsic "cc_saveThreadLocalMultipleValues" (sret-arg return-values) (first outputs))
    ))

(defmethod translate-simple-instruction
    ((instr cleavir-ir:multiple-to-fixed-instruction) inputs outputs (abi abi-x86-64))
  ;; Create a basic block for each output
  (with-return-values (return-vals abi)
    #+(or)(cmp:irc-intrinsic "cc_loadThreadLocalMultipleValues" (sret-arg return-vals) (first inputs))
    (let* ((blocks (let (b) (dotimes (i (1+ (length outputs))) (push (cmp:irc-basic-block-create (format nil "mvn~a-" i) nil) b)) (nreverse b)))
	   (final-block (cmp:irc-basic-block-create "mvn-final" nil))
	   (switch (cmp:irc-switch (cmp:irc-load (number-of-return-values return-vals)) (car (last blocks)) (length blocks))))
      (dotimes (n (length blocks))
	(let ((block (elt blocks n)))
	  (cmp:irc-begin-block block)
	  (llvm-sys:add-case switch (%size_t n) block)
	  (dotimes (i (length outputs))
	    (if (< i n)
		(%store (cmp:irc-load (return-value-elt return-vals i)) (elt outputs i))
		(%store (%nil) (elt outputs i))))
	  (cmp:irc-br final-block)))
      (cmp:irc-begin-block final-block))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:funcall-instruction) inputs outputs (abi abi-x86-64))
  #+(or)(progn
	  (format t "--------------- translate-simple-instruction funcall-instruction~%")
	  (format t "    inputs: ~a~%" inputs)
	  (format t "    outputs: ~a~%" outputs))
  (let ((call (apply-closure "cc_call" (first inputs) (cdr inputs) abi)))
    (cc-dbg-when *debug-log*
		 (format *debug-log* "    translate-simple-instruction funcall-instruction: ~a~%" (cc-mir:describe-mir instruction))
		 (format *debug-log* "     instruction --> ~a~%" call))
    )
  #+(or)(with-return-values (return-vals abi)
	  (cmp:irc-intrinsic "cc_saveThreadLocalMultipleValues" (sret-arg return-vals) (first outputs))))


(defmethod translate-simple-instruction
    ((instruction clasp-cleavir:invoke-instruction) inputs outputs (abi abi-x86-64))
  #+(or)(progn
	  (format t "--------------- translate-simple-instruction funcall-instruction~%")
	  (format t "    inputs: ~a~%" inputs)
	  (format t "    outputs: ~a~%" outputs))
  (let* ((lpad (clasp-cleavir:landing-pad instruction)))
    (cmp:with-landing-pad (clasp-cleavir:basic-block lpad)
      (let ((call (apply-closure "cc_invoke" (first inputs) (cdr inputs) abi)))
	(cc-dbg-when *debug-log*
		     (format *debug-log* "    translate-simple-instruction invoke-instruction: ~a~%" (cc-mir:describe-mir instruction))
		     (format *debug-log* "     instruction --> ~a~%" call))
	)))
  #+(or)(with-return-values (return-vals abi)
	  (cmp:irc-intrinsic "cc_saveThreadLocalMultipleValues" (sret-arg return-vals) (first outputs))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:nop-instruction) inputs outputs abi)
  (llvm-sys:create-int-to-ptr cmp:*irbuilder* (cmp:jit-constant-size_t cmp:+nil-value+) cmp:+t*+ "nil"))



(defmethod translate-simple-instruction
    ((instruction cc-mir:indexed-unwind-instruction) inputs outputs abi)
  (cmp:irc-intrinsic "cc_throwDynamicGo" 
		     (%size_t (cc-mir:landing-pad-id instruction))
		     (%size_t (cc-mir:jump-id instruction))))



(defmethod translate-simple-instruction
    ((instruction cleavir-ir:create-cell-instruction) inputs outputs abi)
  (let ((result (cmp:irc-intrinsic "cc_makeCell")))
    (%store result (first outputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:write-cell-instruction) inputs outputs abi)
  (let ((cell (llvm-sys:create-load-value-twine cmp:*irbuilder* (first inputs) "cell"))
	(val (llvm-sys:create-load-value-twine cmp:*irbuilder* (second inputs) "val")))
    (cmp:irc-intrinsic "cc_writeCell" cell val)))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:read-cell-instruction) inputs outputs abi)
  (let ((cell (llvm-sys:create-load-value-twine cmp:*irbuilder* (first inputs) "cell")))
    (let ((result (cmp:irc-intrinsic "cc_readCell" cell)))
      (llvm-sys:create-store cmp:*irbuilder* result (first outputs) nil))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fetch-instruction) inputs outputs abi)
  (let ((env (cmp:irc-load (first inputs) "env"))
	(idx (second inputs)))
    (let ((result (cmp:irc-intrinsic "cc_fetch" env idx)))
      (llvm-sys:create-store cmp:*irbuilder* result (first outputs) nil))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fdefinition-instruction) inputs outputs abi)
  (let ((cell (cmp:irc-load (first inputs) "func-name")))
;;    (format t "translate-simple-instruction (first inputs) = ~a ~%" (first inputs))
    (let ((result (cmp:irc-intrinsic-args "cc_fdefinition" (list cell) :label "func")))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:debug-message-instruction) inputs outputs abi)
  (let ((msg (cmp:jit-constant-unique-string-ptr (clasp-cleavir-hir:debug-message instruction))))
    (cmp:irc-intrinsic "debugMessage" msg)))
	

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:setf-fdefinition-instruction) inputs outputs abi)
  (let ((cell (cmp:irc-load (first inputs) "setf-func-name")))
    (let ((result (cmp:irc-intrinsic "cc_getSetfFdefinition" cell)))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:symbol-value-instruction) inputs outputs abi)
  (let ((sym (cmp:irc-load (first inputs) "sym-name")))
    (let ((result (cmp:irc-intrinsic "cc_symbolValue" sym)))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:set-symbol-value-instruction) inputs outputs abi)
  (let ((sym (cmp:irc-load (first inputs) "sym-name"))
	(val (cmp:irc-load (second inputs) "value")))
    (cmp:irc-intrinsic "cc_setSymbolValue" sym val)))



(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enclose-instruction) inputs outputs abi)
  (declare (ignore inputs))
  (let* ((enter-instruction (cleavir-ir:code instruction)))
    (multiple-value-bind (enclosed-function function-kind unknown-ret lambda-name)
	(layout-procedure enter-instruction abi)
      #+(or)(push enclosed-function *functions-to-finalize*)
      #+(or)(progn
	      (warn "------- Implement enclose-instruction: ~a~%" instruction)
	      (format t "   enter-instruction: ~a~%" enter-instruction)
	      (format t "   enclosed-function: ~a~%" enclosed-function)
	      (format t "    inputs: ~a~%" inputs)
	      (format t "    outputs: ~a~%" outputs))
      (let* ((loaded-inputs (mapcar (lambda (x) (cmp:irc-load x "cell")) inputs))
	     (ltv-lambda-name-index (cmp:codegen-literal nil lambda-name))
	     (ltv-lambda-name (cmp:irc-intrinsic-args "cc_precalcValue" (list (ltv-global) (%size_t ltv-lambda-name-index)) :label (format nil "lambda-name->~a" lambda-name)))
	     (result (cmp:irc-intrinsic-args "cc_enclose" (list* ltv-lambda-name enclosed-function (%size_t (length inputs)) loaded-inputs) :label (format nil "closure->~a" lambda-name))))
	(cc-dbg-when *debug-log*
		     (format *debug-log* "cc_enclose with ~a cells~%" (length inputs))
		     (format *debug-log* "    inputs: ~a~%" inputs))
	(%store result (first outputs) nil)))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:multiple-value-call-instruction) inputs outputs abi)
  (with-return-values (return-vals abi)
    (let ((call (cmp:irc-intrinsic "cc_call_multipleValueOneFormCall" (sret-arg return-vals) (cmp:irc-load (first inputs)))))
      (cc-dbg-when *debug-log*
			   (format *debug-log* "    translate-simple-instruction multiple-value-call-instruction: ~a~%" (cc-mir:describe-mir instruction))
			   (format *debug-log* "     instruction --> ~a~%" call))
      )))      

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir:invoke-multiple-value-call-instruction) inputs outputs abi)
  (let* ((lpad (clasp-cleavir:landing-pad instruction)))
    (cmp:with-landing-pad (clasp-cleavir:basic-block lpad)
      (with-return-values (return-vals abi)
	(let ((call (cmp:irc-intrinsic "cc_invoke_multipleValueOneFormCall" (sret-arg return-vals) (cmp:irc-load (first inputs)))))
	  (cc-dbg-when *debug-log*
		       (format *debug-log* "    translate-simple-instruction invoke-multiple-value-call-instruction: ~a~%" (cc-mir:describe-mir instruction))
		       (format *debug-log* "     instruction --> ~a~%" call))
	  )))))



(defmethod translate-simple-instruction
    ((instruction cleavir-ir:the-instruction) inputs outputs abi)
  (declare (ignore outputs))
  #+(or) (warn "What should I do with the-instruction")
  )
#||  `(unless (typep ,(first inputs) ',(cleavir-ir:value-type instruction))
(error 'type-error
       :expected-type ',(cleavir-ir:value-type instruction)
       :datum ,(first inputs))))
||#




#+(or)(progn
(defmethod translate-simple-instruction
    ((instruction cleavir-ir:tailcall-instruction) inputs outputs)
  (declare (ignore outputs))
  `(return (funcall ,(first inputs) ,@(rest inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:the-instruction) inputs outputs)
  (declare (ignore outputs))
  `(unless (typep ,(first inputs) ',(cleavir-ir:value-type instruction))
     (error 'type-error
	    :expected-type ',(cleavir-ir:value-type instruction)
	    :datum ,(first inputs))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:car-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (car ,(first inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:cdr-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (cdr ,(first inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:rplaca-instruction) inputs outputs)
  (declare (ignore outputs))
  `(rplaca ,(first inputs) ,(second inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:rplacd-instruction) inputs outputs)
  (declare (ignore outputs))
  `(rplacd ,(first inputs) ,(second inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:t-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:bit-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unsigned-byte-8-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:short-float-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:single-float-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:double-float-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:long-float-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:t-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:bit-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unsigned-byte-8-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:short-float-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:single-float-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:double-float-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:long-float-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:multiple-to-fixed-instruction) inputs outputs)
  (let ((temp (gensym)))
    `(let ((,temp ,(first inputs)))
       (declare (ignorable ,temp))
       ,@(loop for output in outputs
	    collect `(setf ,output (pop ,temp))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unwind-instruction) inputs outputs)
  (gensym))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-BRANCH-INSTRUCTION.
(defmethod translate-branch-instruction
    ((instruction cleavir-ir:eq-instruction) inputs outputs successors abi)
  (let ((ceq (cmp:irc-icmp-eq (cmp:irc-load (first inputs)) (cmp:irc-load (second inputs)))))
    (cmp:irc-cond-br ceq (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:return-instruction) inputs outputs successors abi)
  (declare (ignore successors))
  #+(or)(with-return-values (return-vals abi)
	  (cmp:irc-intrinsic "cc_loadThreadLocalMultipleValues" (sret-arg return-vals) (first inputs)))
  (cmp:irc-ret-void))

#+(or)(progn
	(defmethod translate-branch-instruction
	    ((instruction cleavir-ir:typeq-instruction) inputs outputs successors abi)
	  `(if (typep ,(first inputs) ',(cleavir-ir:value-type instruction))
	       (go ,(second successors))
	       (go ,(first successors))))

	(defmethod translate-branch-instruction
	    ((instruction cleavir-ir:fixnum-add-instruction) inputs outputs successors)
	  (let ((result (gensym)))
	    `(let ((,result (+ ,(first inputs) ,(second inputs))))
	       (cond ((typep result 'fixnum)
		      (setq ,(first outputs) ,result)
		      (go ,(first successors)))
		     ((plusp ,result)
		      (setq ,(first outputs)
			    (+ ,result (* 2 most-negative-fixnum)))
		      (go ,(second successors)))
		     (t
		      (setq ,(first outputs)
			    (- ,result (* 2 most-negative-fixnum)))
		      (go ,(second successors)))))))

	(defmethod translate-branch-instruction
	    ((instruction cleavir-ir:fixnum-sub-instruction) inputs outputs successors)
	  (let ((result (gensym)))
	    `(let ((,result (- ,(first inputs) ,(second inputs))))
	       (cond ((typep result 'fixnum)
		      (setq ,(first outputs) ,result)
		      (go ,(first successors)))
		     ((plusp ,result)
		      (setq ,(first outputs)
			    (+ ,result (* 2 most-negative-fixnum)))
		      (go ,(second successors)))
		     (t
		      (setq ,(first outputs)
			    (- ,result (* 2 most-negative-fixnum)))
		      (go ,(second successors)))))))

	(defmethod translate-branch-instruction
	    ((instruction cleavir-ir:fixnum-less-instruction) inputs outputs successors)
	  (declare (ignore outputs))
	  `(if (< ,(first inputs) ,(second inputs))
	       (go ,(first successors))
	       (go ,(second successors))))

	(defmethod translate-branch-instruction
	    ((instruction cleavir-ir:fixnum-not-greater-instruction) inputs outputs successors)
	  (declare (ignore outputs))
	  `(if (<= ,(first inputs) ,(second inputs))
	       (go ,(first successors))
	       (go ,(second successors))))

	(defmethod translate-branch-instruction
	    ((instruction cleavir-ir:fixnum-equal-instruction) inputs outputs successors)
	  (declare (ignore outputs))
	  `(if (= ,(first inputs) ,(second inputs))
	       (go ,(first successors))
	       (go ,(second successors))))
	)

;;; When the FUNCALL-INSTRUCTION is the last instruction of a basic
;;; block, it is because there is a call to a function that will never
;;; return, such as ERROR, and the instruction then has no successors
;;; (which is why it is at the end of the basic block).
;;;
;;; We therefore must provide a method on TRANSLATE-BRANCH-INSTRUCTION
;;; (in addition to the method on TRANSLATE-SIMPLE-INSTRUCTION)
;;; specialized to FUNCALL-INSTRUCTION.
(defmethod translate-branch-instruction
    ((instruction cleavir-ir:funcall-instruction) inputs outputs successors abi)
  (declare (ignore outputs successors))
  `(funcall ,(first inputs) ,@(rest inputs)))










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; JIT the module
;;;
#||
(defun jit-module-run-main (module main-name)
  (let ((engine-builder (llvm-sys:make-engine-builder module))
	;; After make-engine-builder MODULE becomes invalid!!!!!
	(target-options (llvm-sys:make-target-options)))
    (llvm-sys:setf-no-frame-pointer-elim target-options t)
    (llvm-sys:setf-jitemit-debug-info target-options t)
    (llvm-sys:setf-jitemit-debug-info-to-disk target-options t)
    (llvm-sys:set-target-options engine-builder target-options)
;;;	 (llvm-sys:set-use-mcjit engine-builder t)
    (let*((execution-engine (llvm-sys:create engine-builder))
	  (stem (string-downcase (pathname-name filename)))
	  (main-fn-name llvm-sys:+clasp-main-function-name+)
	  (time-jit-start (clock-gettime-nanoseconds)))
      (llvm-sys:finalize-engine-and-register-with-gc-and-run-function execution-engine main-fn-name (namestring (truename filename)) 0 0 *load-time-value-holder-name* )
      )))
t)
nil)
||#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main entry point.
;;;
;;; This will compile a top-level form (doing all tlf processing)
;;; into the current *module*

;; All enclosed functions need to be finalized
(defvar *functions-to-finalize*)


(defun describe-form (form)
  (when (consp form)
    (cond
      ((eq 'core:*fset (car form))
       (let* ((name (cadr (cadr form)))
	      (is-macro (cadddr form))
	      (header (if is-macro
			  "defmacro"
			  "defun")))
	 (format t ";    ~a ~a~%" header name)))
      ((eq 'core:*make-constant (car form))
       (let* ((name (cadr (cadr form))))
	 (format t ";    *make-constant ~a~%" name)))
      ((eq 'cl:defun (car form))
       (format t ";    defun ~a~%" (cadr form)))
      ((eq 'cl:defgeneric (car form))
       (format t ";    defgeneric ~a~%" (cadr form)))
      ((eq 'cl:defstruct (car form))
       (format t ";    defstruct ~a~%" (cadr form)))
      ((eq 'cl:defclass (car form))
       (format t ";    defclass ~a~%" (cadr form)))
      ((eq 'cl:defmethod (car form))
       (format t ";    defmethod ~a~%" (cadr form)))
      ((eq 'cl:defmacro (car form))
       (format t ";    defun ~a~%" (cadr form)))
      ((eq 'cl:defconstant (car form))
       (format t ";    defconstant ~a~%" (cadr form)))
      ((eq 'cl:defvar (car form))
       (format t ";    defvar ~a~%" (cadr form)))
      ((eq 'cl:defparameter (car form))
       (format t ";    defparameter ~a~%" (cadr form)))
      (t ()))))




(defun do-compile (form)
  (cc-dbg-when *debug-log*
	       (format *debug-log* "==== Form: ~a~%" form))
  (handler-bind
      ((cleavir-env:no-variable-info
	(lambda (condition)
;;;	  (declare (ignore condition))
	  (warn "Condition: ~a" condition)
	  (invoke-restart 'cleavir-generate-ast:consider-special)))
       (cleavir-env:no-function-info
	(lambda (condition)
;;;	  (declare (ignore condition))
	  (warn "Condition: ~a" condition)
	  (invoke-restart 'cleavir-generate-ast:consider-global))))
    (when *compile-print* (describe-form form))
    (cc-dbg-when *debug-log*
		 (format *debug-log* "cleavir-generate-ast:*compiler* --> ~a~%" cleavir-generate-ast:*compiler*)
		 (format *debug-log* "cleavir-generate-ast::*current-form-is-top-level-p* --> ~a~%" 
			 (if (boundp 'cleavir-generate-ast::*current-form-is-top-level-p*) 
			     cleavir-generate-ast::*current-form-is-top-level-p* 
			     "UNBOUND" ))
		 (format *debug-log* "cleavir-generate-ast::*subforms-are-top-level-p* --> ~a~%" 
			 cleavir-generate-ast::*subforms-are-top-level-p*))
    (let* ((clasp-system (make-instance 'clasp))
	   (ast (let ((a (cleavir-generate-ast:generate-ast form #+passed-env env #-passed-env *clasp-env* clasp-system)))
		  (when *debug-cleavir* (draw-ast a))
		  a))
	   (hoisted-ast (clasp-cleavir-ast:hoist-load-time-value ast))
	   (hir (cleavir-ast-to-hir:compile-toplevel hoisted-ast)))
      (cc-dbg-when *debug-log*
		   (incf *debug-log-index*)
		   (let ((ast-pathname (make-pathname :name (format nil "ast~a" *debug-log-index*) 
						      :type "dot" 
						      :defaults (pathname *debug-log*))))
		     (cleavir-ast-graphviz:draw-ast hoisted-ast (namestring ast-pathname))
		     (format *debug-log* "Wrote ast to: ~a~%" (namestring ast-pathname))))
      (cleavir-hir-transformations:hir-transformations hir clasp-system nil nil)
      (cleavir-ir:hir-to-mir hir clasp-system nil nil)
      (when *debug-cleavir* (draw-mir hir)) ;; comment out
      (cc-mir:assign-mir-instruction-datum-ids hir)
      (clasp-cleavir:convert-funcalls hir)
      (setf *ast* hoisted-ast
	    *hir* hir)
      (let ((*form* form)
	    (abi (make-instance 'abi-x86-64)))
	(translate hir abi)))))

(defun cleavir-compile-t1expr (name form env pathname)
  #+(or)(and env (error "I don't support anything but top level environment compiles using cleavir env: ~a" env ))
  #+(or)(format t "Compiling: ~a~%" form)
  (multiple-value-bind (fn function-kind wrapped-env lambda-name warnp failp)
      (cmp:with-debug-info-generator (:module cmp:*the-module* :pathname pathname)
	(let* ((clasp-system (make-instance 'clasp))
	       (ast (cleavir-generate-ast:generate-ast form #+passed-env env #-passed-env *clasp-env* clasp-system))
	       (hoisted-ast (clasp-cleavir-ast:hoist-load-time-value ast))
	       (hir (cleavir-ast-to-hir:compile-toplevel hoisted-ast)))
	  (when *debug-cleavir* (draw-ast hoisted-ast)) ;; comment out
	  (cleavir-hir-transformations:hir-transformations hir clasp-system nil nil)
	  #+(or)(format t "About to draw *debug-cleavir* = ~a~%" *debug-cleavir*)
	  (when *debug-cleavir* (draw-hir hir)) ;; comment out
	  (cleavir-ir:hir-to-mir hir clasp-system nil nil)
	  (when *debug-cleavir* (draw-mir hir)) ;; comment out
	  (cc-mir:assign-mir-instruction-datum-ids hir)
	  (clasp-cleavir:convert-funcalls hir)
	  (setf *ast* hoisted-ast
		*hir* hir)
	  (let ((*form* form)
		(abi (make-instance 'abi-x86-64)))
	    (translate hir abi))))
    (cmp:cmp-log "------------  Finished building MCJIT Module - about to finalize-engine  Final module follows...\n")
    (or fn (error "There was no function returned by compile-lambda-function"))
    (cmp:cmp-log "fn --> %s\n" fn)
    (cmp:cmp-log-dump cmp:*the-module*)
    (when cmp:*dump-module-on-completion*
      (llvm-sys:dump cmp:*the-module*))
    (cmp:cmp-log "About to test and maybe set up the *run-time-execution-engine*\n")
    (if (not cmp:*run-time-execution-engine*)
	;; SETUP THE *run-time-execution-engine* here for the first time
	;; using the current module in *the-module*
	;; At this point the *the-module* will become invalid because
	;; the execution-engine will take ownership of it
	(setq cmp:*run-time-execution-engine* (cmp:create-run-time-execution-engine cmp:*the-module*))
	(llvm-sys:add-module cmp:*run-time-execution-engine* cmp:*the-module*))
    ;; At this point the Module in *the-module* is invalid because the
    ;; execution-engine owns it
    (cmp:cmp-log "The execution-engine now owns the module\n")
    (setq cmp:*the-module* nil)
    (cmp:cmp-log "About to finalize-engine with fn %s\n" fn)
    (let* ((fn-name (llvm-sys:get-name fn)) ;; this is the name of the function - a string
	   (setup-function
	    (llvm-sys:finalize-engine-and-register-with-gc-and-get-compiled-function
	     cmp:*run-time-execution-engine*
	     'REPL			; main fn name
	     fn				; llvm-fn
	     nil			; environment
	     cmp:*run-time-literals-external-name*
	     "repl-fn.txt"
	     0
	     0
	     nil)))
      (unless (compiled-function-p setup-function)
	(format t "Whoah cleavir-clasp compiled code eval --> ~s~%" compiled-function)
	(return-from cleavir-compile-t1expr (values nil t)))
      (let ((enclosed-function (funcall setup-function cmp:*run-time-literal-holder*)))
	(cmp:set-associated-funcs enclosed-function cmp:*all-funcs-for-one-compile*)
	(values enclosed-function warnp failp)))))





(defun cleavir-compile-file-form (form)
  (multiple-value-bind (fn kind #|| more ||#)
      (do-compile form)
    (cmp:with-ltv-function-codegen (result ltv-env)
      (cmp:irc-intrinsic "invokeTopLevelFunction" 
			 result 
			 fn 
			 (cmp:irc-renv ltv-env)
			 (cmp:jit-constant-unique-string-ptr "top-level")
			 cmp:*gv-source-file-info-handle*
			 (cmp:irc-i64-*current-source-pos-info*-filepos)
			 (cmp:irc-i32-*current-source-pos-info*-lineno)
			 (cmp:irc-i32-*current-source-pos-info*-column)
			 cmp:*load-time-value-holder-global-var*
			 ))))


(defun cleavir-compile (name form &key debug)
  (let ((cmp:*cleavir-compile-hook* #'cleavir-compile-t1expr)
	(cmp:*dump-module-on-completion* t)
	(cleavir-generate-ast:*compiler* 'cl:compile)
	(*debug-cleavir* debug))
    (compile name form)))





(defun cleavir-compile-file (given-input-pathname &rest args)
  (let ((*debug-log-index* 0)
	(cleavir-generate-ast:*compiler* 'cl:compile-file))
    (let ((cmp:*cleavir-compile-file-hook* #'cleavir-compile-file-form))
      (apply #'compile-file given-input-pathname args))))




#||
#+(or)(defun interpret-hir (initial-instruction)
(funcall (compile nil `(lambda () ,(translate initial-instruction)))))
(core:load-time-values-dump cmp::*run-time-literal-holder*)
(core:load-time-values-dump "globalRunTimeValues")
(print "Hello")

(apropos "load-time-values-dump")
||#
