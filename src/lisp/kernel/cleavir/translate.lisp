(cl:in-package #:clasp-cleavir)


(defvar *debug-cleavir* nil)
(export '*debug-cleavir*)

;;; The first argument to this function is an instruction that has a
;;; single successor.  Whether a GO is required at the end of this
;;; function is determined by the code layout algorithm.  
;;; 
;;; The inputs are forms to be evaluated.  The outputs are symbols
;;; that are names of variables.
(defgeneric translate-simple-instruction (instruction return-value inputs outputs abi))

(defgeneric translate-branch-instruction (instruction return-value inputs outputs successors abi))



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
	    (t (error "Add support to translate datum: ~a~%" datum)))
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

(defun layout-basic-block (basic-block return-value abi)
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
		instruction return-value input-vars output-vars abi))
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
		 last return-value input-vars output-vars abi)
		(if (typep (second basic-block) 'cleavir-ir:unwind-instruction)
		    (cmp:irc-unreachable)
                    (progn
                      (cmp:irc-low-level-trace :flow)
                      (cmp:irc-br (gethash (first successors) *tags*)))))
	  (list (translate-branch-instruction
		 last return-value input-vars output-vars successor-tags abi)))
      (cc-dbg-when *debug-log*
		   (format *debug-log* "- - - -  END layout-basic-block  owner: ~a:~a   -->  ~a~%" (cleavir-ir-gml::label owner) (clasp-cleavir:instruction-gid owner) basic-block))
      )))

(defun get-or-create-lambda-name (instr)
  (if (typep instr 'clasp-cleavir-hir:named-enter-instruction)
      (clasp-cleavir-hir:lambda-name instr)
      'TOP-LEVEL))

(defun layout-procedure (initial-instruction abi)
  ;; I think this removes every basic-block that
  ;; isn't owned by this initial-instruction
  (let* ((clasp-cleavir-ast-to-hir:*landing-pad* nil)
         return-value
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
    (let* ((main-fn-name lambda-name) ;;(format nil "cl->~a" lambda-name))
	   (cmp:*current-function-name* (cmp:jit-function-name main-fn-name))
	   (cmp:*gv-current-function-name* (cmp:jit-make-global-string-ptr cmp:*current-function-name* "fn-name"))
	   (fn (llvm-sys:function-create
		cmp:+fn-prototype+
		'llvm-sys:internal-linkage
		(cmp:jit-function-name main-fn-name) ;cmp:*current-function-name*)
		cmp:*the-module*))
	   (cmp:*current-function* fn)
	   (entry-block (cmp:irc-basic-block-create "entry" fn))
	   (*current-function-entry-basic-block* entry-block)
	   (*function-current-multiple-value-array-address* nil)
	   (*entry-irbuilder* (llvm-sys:make-irbuilder cmp:*llvm-context*))
	   (body-irbuilder (llvm-sys:make-irbuilder cmp:*llvm-context*))
	   (body-block (cmp:irc-basic-block-create "body")))
      (llvm-sys:set-personality-fn fn (cmp:irc-personality-function))
      (llvm-sys:add-fn-attr fn 'llvm-sys:attribute-uwtable)
      (push fn cmp:*all-functions-for-one-compile*)
      (cc-dbg-when *debug-log*
		   (format *debug-log* "------------ BEGIN layout-procedure ~a~%" (llvm-sys:get-name fn))
		   (format *debug-log* "   basic-blocks for procedure~%")
		   (dolist (bb basic-blocks)
		     (destructuring-bind (first last owner) bb
		       (format *debug-log* "basic-block owner: ~a:~a~%" (cleavir-ir-gml::label owner) (clasp-cleavir:instruction-gid owner))
		       (loop for instruction = first
			  then (first (cleavir-ir:successors instruction))
			  until (eq instruction last)
			  do (format *debug-log* "     ~a~%" (cc-mir:describe-mir instruction)))
		       (format *debug-log* "     ~a~%" (cc-mir:describe-mir last)))))
      (let ((args (llvm-sys:get-argument-list fn)))
	(mapcar #'(lambda (arg argname) (llvm-sys:set-name arg argname))
		(llvm-sys:get-argument-list fn) cmp:+fn-prototype-argument-names+)
	;; Set the first argument attribute to be sret
	#+(or)(if args
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
        (setq return-value (alloca-return_type))
        ;; In case of a non-local exit, zero out the number of returned
        ;; values
        (with-return-values (return-values return-value abi)
          (%store (%size_t 0) (number-of-return-values return-values)))
        (cmp:with-dbg-function ("repl-FIX"
                                :linkage-name (llvm-sys:get-name fn)
                                :function fn
                                :function-type cmp:+fn-prototype+
                                :form *form*)
          (cmp:with-dbg-lexical-block (*form*)
            (cmp:dbg-set-current-source-pos-for-irbuilder *form* *entry-irbuilder*)
            (cmp:dbg-set-current-source-pos-for-irbuilder *form* body-irbuilder)
            (cmp:irc-low-level-trace :arguments)
            #+use-ownerships(loop for var being each hash-key of *ownerships*
                               using (hash-value owner)
                               when (and (typep var '(or
                                                      cleavir-ir:lexical-location
                                                      cleavir-ir:values-location))
                                         (eq owner initial-instruction)
                                         #+(or)(not (member var (cleavir-ir:outputs
                                                                 initial-instruction))))
                               collect (translate-datum var)))))
      (cmp:with-dbg-function ("unused-with-dbg-function-name"
                              :linkage-name (llvm-sys:get-name fn)
                              :function fn
                              :function-type cmp:+fn-prototype+
                              :form *form*)
        (cmp:with-dbg-lexical-block (*form*)
          ;;          (cmp:dbg-set-current-source-pos *form*)
          (llvm-sys:set-insert-point-basic-block body-irbuilder body-block)
          (cmp:with-irbuilder (body-irbuilder)
            (cmp:irc-begin-block body-block)
            (layout-basic-block first return-value abi)
            (loop for block in rest
               for instruction = (first block)
               do (progn
                    #+(or)(format t "Laying out basic block: ~a~%" block)
                    #+(or)(format t "Inserting basic block for instruction: ~a~%" instruction)
                    (cmp:irc-begin-block (gethash instruction *tags*))
                    (layout-basic-block block return-value abi))))
          ;; Finish up by jumping from the entry block to the body block
          (cmp:with-irbuilder (*entry-irbuilder*)
            (cmp:irc-low-level-trace :flow)
            (cmp:irc-br body-block))
          (cc-dbg-when *debug-log*
                       (format *debug-log* "----------END layout-procedure ~a~%" (llvm-sys:get-name fn)))
          (values fn :function-kind nil lambda-name))))))

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
      (cc-dbg-when 
       *debug-log*
       (let ((mir-pathname (make-pathname :name (format nil "mir~a" *debug-log-index*) :type "gml" :defaults (pathname *debug-log*))))
	 (multiple-value-bind (instruction-ids datum-ids)
	     (cleavir-ir-gml:draw-flowchart initial-instruction (namestring mir-pathname))
	   #+(or)(dolist (bb *basic-blocks*)
		   (destructuring-bind (first last owner) bb
		     (format *debug-log* "basic-block owner: ~a:~a   -->  ~a~%" (cleavir-ir-gml::label owner) (gethash owner instruction-ids) bb)
		     (loop for instruction = first
			then (first (cleavir-ir:successors instruction))
			until (eq instruction last)
			do (format *debug-log* "     ~a:~a~%" (cleavir-ir-gml::label instruction) (gethash instruction instruction-ids)))
		     (format *debug-log* "     ~a:~a~%" (cleavir-ir-gml::label last) (gethash last instruction-ids)))))
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
    ((instr cleavir-ir:enter-instruction) return-value inputs outputs (abi abi-x86-64))
  (let* ((fn-args (llvm-sys:get-argument-list cmp:*current-function*))
         (closed-env-dest (first outputs)))
    (multiple-value-bind (closed-env-arg calling-convention)
        (cmp:parse-function-arguments fn-args)
      (llvm-sys:create-store cmp:*irbuilder* closed-env-arg closed-env-dest nil)
      #+(or)(progn
              (format t "translate-simple-instruction for enter-instruction~%")
              (format t " fn-args: ~a~%" fn-args))
      (let* ((lambda-list (cleavir-ir:lambda-list instr))
             (static-environment-output (first (cleavir-ir:outputs instr)))
             (args (cdr (cleavir-ir:outputs instr))))
        #+(or)(progn
                (format t "    outputs: ~s~%" args)
                (format t "translated outputs: ~s~%" (mapcar (lambda (x) (translate-datum x)) args))
                (format t "lambda-list: ~a~%" lambda-list))
        (compile-lambda-list-code lambda-list args calling-convention)))))


(defmethod translate-simple-instruction
    ((instr clasp-cleavir-hir:landing-pad-named-enter-instruction) return-value inputs outputs (abi abi-x86-64))
  (let ((landing-pad (clasp-cleavir-hir:landing-pad instr)))
    (when landing-pad
      (let ((exn.slot (alloca-i8* "exn.slot"))
	    (ehselector.slot (alloca-i32 "ehselector.slot")))
	(setf (basic-block landing-pad)
	      (clasp-cleavir:create-landing-pad exn.slot ehselector.slot instr return-value landing-pad *tags* abi)))
      (cmp:with-irbuilder (*entry-irbuilder*)
        (cmp:irc-low-level-trace :cclasp-eh)
	(let ((result (cmp:irc-intrinsic "cc_pushLandingPadFrame")))
	  (cmp:irc-store result (translate-datum (clasp-cleavir-hir:frame-holder instr)))))))
  (call-next-method))




(defmethod translate-simple-instruction
    ((instruction cleavir-ir:instruction) return-value inputs outputs abi)
  (error "Implement instruction: ~a for abi: ~a~%" instruction abi)
  (format t "--------------- translate-simple-instruction ~a~%" instruction)
  (format t "    inputs: ~a~%" inputs)
  (format t "    outputs: ~a~%" outputs))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:assignment-instruction) return-value inputs outputs abi)
  #+(or)(progn
	  (format t "--------------- translate-simple-instruction assignment-instruction~%")
	  (format t "    inputs: ~a~%" inputs)
	  (format t "    outputs: ~a~%" outputs))
  (cmp:irc-low-level-trace :flow)
  (let ((input (first inputs))
        (output (first outputs)))
    (cond
      ((typep input 'llvm-sys:constant-int)
       (let ((val (%inttoptr input cmp:+t*+)))
         (%store val output)))
      (t
       (let ((load (%load input)))
         (%store load output))))))

(defun ltv-global ()
  (if cmp:*generate-compile-file-load-time-values*
      cmp:*load-time-value-holder-global-var*
      cmp:*run-time-value-holder-global-var*))


(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:precalc-symbol-instruction) return-value inputs outputs abi)
  (let ((idx (first inputs)))
    ;;    (format t "translate-simple-instruction (first inputs) --> ~a~%" (first inputs))
    (cmp:irc-low-level-trace :flow)
    (let ((label (format nil "~s" (clasp-cleavir-hir:precalc-symbol-instruction-original-object instruction))))
      (let ((result (cmp:irc-intrinsic-args "cc_precalcSymbol" (list (ltv-global) idx) :label label)))
	(llvm-sys:create-store cmp:*irbuilder* result (first outputs) nil)))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:precalc-value-instruction) return-value inputs outputs abi)
  (let ((idx (first inputs)))
    (cmp:irc-low-level-trace :flow)
    (let ((result (cmp:irc-intrinsic "cc_precalcValue" (ltv-global) idx)))
      (llvm-sys:create-store cmp:*irbuilder* result (first outputs) nil))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fixed-to-multiple-instruction) return-value inputs outputs (abi abi-x86-64))
  ;; Write the first return value into the result
  (with-return-values (return-values return-value abi)
    (%store (%size_t (length inputs)) (number-of-return-values return-values))
    (dotimes (i (length inputs))
      (%store (cmp:irc-load (elt inputs i)) (return-value-elt return-values i)))
    #+(or)(cmp:irc-intrinsic "cc_saveThreadLocalMultipleValues" (sret-arg return-values) (first outputs))
    ))

(defmethod translate-simple-instruction
    ((instr cleavir-ir:multiple-to-fixed-instruction) return-value inputs outputs (abi abi-x86-64))
  ;; Create a basic block for each output
  (cmp:irc-low-level-trace :flow)
  (with-return-values (return-vals return-value abi)
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
          (cmp:irc-low-level-trace :flow)
	  (cmp:irc-br final-block)))
      (cmp:irc-begin-block final-block))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:intrinsic-call-instruction) return-value inputs outputs (abi abi-x86-64))
  (cmp:irc-low-level-trace :flow)
  (let ((call (clasp-cleavir:unsafe-intrinsic-call :call (clasp-cleavir-hir:function-name instruction) return-value inputs abi)))
    (cc-dbg-when *debug-log*
		 (format *debug-log* "    translate-simple-instruction intrinsic-call-instruction: ~a~%" (cc-mir:describe-mir instruction))
		 (format *debug-log* "     instruction --> ~a~%" call))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:funcall-instruction) return-value inputs outputs (abi abi-x86-64))
  #+(or)(progn
	  (format t "--------------- translate-simple-instruction funcall-instruction~%")
	  (format t "    inputs: ~a~%" inputs)
	  (format t "    outputs: ~a~%" outputs))
  (cmp:irc-low-level-trace :flow)
  (let ((call (closure-call :call "cc_call" (first inputs) return-value (cdr inputs) abi)))
    (cc-dbg-when *debug-log*
		 (format *debug-log* "    translate-simple-instruction funcall-instruction: ~a~%" (cc-mir:describe-mir instruction))
		 (format *debug-log* "     instruction --> ~a~%" call))))



(defmethod translate-simple-instruction
    ((instruction clasp-cleavir:invoke-instruction) return-value inputs outputs (abi abi-x86-64))
  #+(or)(progn
	  (format t "--------------- translate-simple-instruction funcall-instruction~%")
	  (format t "    inputs: ~a~%" inputs)
	  (format t "    outputs: ~a~%" outputs))
  (let* ((lpad (clasp-cleavir::landing-pad instruction)))
    (or (basic-block lpad) (error "There must be a basic-block defined for the landing-pad"))
    (cmp:irc-low-level-trace :flow)
    (let ((call (closure-call :invoke "cc_call" (first inputs) return-value (cdr inputs) abi :landing-pad (basic-block lpad))))
      (cc-dbg-when *debug-log*
		   (format *debug-log* "    translate-simple-instruction invoke-instruction: ~a~%" (cc-mir:describe-mir instruction))
		   (format *debug-log* "     instruction --> ~a~%" call)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:nop-instruction) return-value inputs outputs abi)
  (%nil))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:indexed-unwind-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (with-return-values (return-vals return-value abi)
    ;; Save whatever is in return-vals in the multiple-value array
    (cmp:irc-intrinsic "cc_saveMultipleValue0" return-value) ;; (sret-arg return-vals))
    (cmp:irc-low-level-trace :cclasp-eh)
    (cmp:irc-intrinsic "cc_unwind" (cmp::irc-load (first inputs)) (%size_t (clasp-cleavir-hir:jump-id instruction)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:create-cell-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (let ((result (cmp:irc-intrinsic "cc_makeCell")))
    (%store result (first outputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:write-cell-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (let ((cell (llvm-sys:create-load-value-twine cmp:*irbuilder* (first inputs) "cell"))
	(val (llvm-sys:create-load-value-twine cmp:*irbuilder* (second inputs) "val")))
    (cmp:irc-intrinsic "cc_writeCell" cell val)))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:read-cell-instruction) return-value inputs outputs abi)
  (let ((cell (llvm-sys:create-load-value-twine cmp:*irbuilder* (first inputs) "cell")))
  (cmp:irc-low-level-trace :flow)
    (let ((result (cmp:irc-intrinsic "cc_readCell" cell)))
      (llvm-sys:create-store cmp:*irbuilder* result (first outputs) nil))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fetch-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (let ((env (cmp:irc-load (first inputs) "env"))
	(idx (second inputs)))
    (let ((result (cmp:irc-intrinsic "cc_fetch" env idx)))
      (llvm-sys:create-store cmp:*irbuilder* result (first outputs) nil))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fdefinition-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (let ((cell (cmp:irc-load (first inputs) "func-name")))
;;    (format t "translate-simple-instruction (first inputs) = ~a ~%" (first inputs))
    (let ((result (cmp:irc-intrinsic-args "cc_safe_fdefinition" (list cell) :label "func")))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:debug-message-instruction) return-value inputs outputs abi)
  (let ((msg (cmp:jit-constant-unique-string-ptr (clasp-cleavir-hir:debug-message instruction))))
    (cmp:irc-intrinsic "debugMessage" msg)))
	

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:setf-fdefinition-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (let ((cell (cmp:irc-load (first inputs) "setf-func-name")))
    (let ((result (cmp:irc-intrinsic "cc_safe_setfdefinition" cell)))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:symbol-value-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (let ((sym (cmp:irc-load (first inputs) "sym-name")))
    (let ((result (cmp:irc-create-call "cc_safe_symbol_value" (list sym))))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:set-symbol-value-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (let ((sym (cmp:irc-load (first inputs) "sym-name"))
	(val (cmp:irc-load (second inputs) "value")))
    (cmp:irc-intrinsic "cc_setSymbolValue" sym val)))



(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enclose-instruction) return-value inputs outputs abi)
  (declare (ignore inputs))
  (cmp:irc-low-level-trace :flow)
  #||
  (format t "translate-simple-instruction::enclose-instruction~%")
  (format t "         *current-source-pos-info*: ~a~%" core:*current-source-pos-info*)
||#
  (let* ((enter-instruction (cleavir-ir:code instruction)))
    (multiple-value-bind (enclosed-function function-kind unknown-ret lambda-name)
	(layout-procedure enter-instruction abi)
      (let* ((loaded-inputs (mapcar (lambda (x) (cmp:irc-load x "cell")) inputs))
	     (ltv-lambda-name-index (cmp:codegen-literal nil lambda-name))
	     (ltv-lambda-name (cmp:irc-intrinsic-args "cc_precalcValue" (list (ltv-global) (%size_t ltv-lambda-name-index)) :label (format nil "lambda-name->~a" lambda-name)))
	     (result (cmp:irc-intrinsic-args
                      "cc_enclose"
                      (list* ltv-lambda-name
                             enclosed-function
                             cmp:*gv-source-file-info-handle*
                             (cmp:irc-size_t-*current-source-pos-info*-filepos)
                             (cmp:irc-size_t-*current-source-pos-info*-lineno)
                             (cmp:irc-size_t-*current-source-pos-info*-column)
                             (%size_t (length inputs))
                             loaded-inputs)
                      :label (format nil "closure->~a" lambda-name))))
	(cc-dbg-when *debug-log*
		     (format *debug-log* "cc_enclose with ~a cells~%" (length inputs))
		     (format *debug-log* "    inputs: ~a~%" inputs))
	(%store result (first outputs) nil)))))

(defmethod translate-simple-instruction
    ((instruction cc-mir:stack-enclose-instruction) return-value inputs outputs abi)
  (declare (ignore inputs))
  (cmp:irc-low-level-trace :flow)
  (let* ((enter-instruction (cleavir-ir:code instruction)))
    (multiple-value-bind (enclosed-function function-kind unknown-ret lambda-name)
        (layout-procedure enter-instruction abi)
      (let* ((loaded-inputs (mapcar (lambda (x) (cmp:irc-load x "cell")) inputs))
             (stack-allocated-closure-space (alloca-i8 (core:closure-with-slots-size (length inputs)) "stack-allocated-closure"))
             (ptr-to-sacs
              (llvm-sys:create-bit-cast cmp:*irbuilder* stack-allocated-closure-space cmp:+i8*+ "closure-ptr"))
             (ltv-lambda-name-index (cmp:codegen-literal nil lambda-name))
             (ltv-lambda-name (cmp:irc-intrinsic-args "cc_precalcValue" (list (ltv-global) (%size_t ltv-lambda-name-index)) :label (format nil "lambda-name->~a" lambda-name)))
             (result
              (progn
                (cmp:irc-intrinsic-args
                 "cc_stack_enclose"
                 (list* ptr-to-sacs
                        ltv-lambda-name
                        enclosed-function
                        cmp:*gv-source-file-info-handle*
                        (cmp:irc-size_t-*current-source-pos-info*-filepos)
                        (cmp:irc-size_t-*current-source-pos-info*-lineno)
                        (cmp:irc-size_t-*current-source-pos-info*-column)
                        (%size_t (length inputs))
                        loaded-inputs)
                 :label (format nil "closure->~a" lambda-name)))
               #+(or) (progn
                        (cmp:irc-intrinsic-args
                         "cc_enclose"
                         (list* ltv-lambda-name
                                enclosed-function
                                cmp:*gv-source-file-info-handle*
                                (cmp:irc-size_t-*current-source-pos-info*-filepos)
                                (cmp:irc-size_t-*current-source-pos-info*-lineno)
                                (cmp:irc-size_t-*current-source-pos-info*-column)
                                (%size_t (length inputs))
                                loaded-inputs)
                         :label (format nil "closure->~a" lambda-name)))
               ))
        (cc-dbg-when *debug-log*
                     (format *debug-log* "cc_enclose with ~a cells~%" (length inputs))
                     (format *debug-log* "    inputs: ~a~%" inputs))
        (%store result (first outputs) nil)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:multiple-value-call-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (with-return-values (return-vals return-value abi)
    (cmp:irc-intrinsic "cc_saveMultipleValue0" return-value) ;; (sret-arg return-vals))
    (let ((call-result (cmp:irc-create-call "cc_call_multipleValueOneFormCall" 
				     (list (cmp:irc-load (first inputs))))))
      (%store call-result return-value)
      (cc-dbg-when 
       *debug-log*
       (format *debug-log* "    translate-simple-instruction multiple-value-call-instruction: ~a~%" 
	       (cc-mir:describe-mir instruction))
       (format *debug-log* "     instruction --> ~a~%" call-result))
      )))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir:invoke-multiple-value-call-instruction) return-value inputs outputs abi)
  (cmp:irc-low-level-trace :flow)
  (let* ((lpad (clasp-cleavir::landing-pad instruction)))
    (with-return-values (return-vals return-value abi)
      (cmp:irc-intrinsic "cc_saveMultipleValue0" return-value) ;; (sret-arg return-vals))
      (let ((call-result (cmp:irc-create-invoke "cc_call_multipleValueOneFormCall" 
					 (list (cmp:irc-load (first inputs)))
					 (basic-block lpad))))
        (%store call-result return-value)
	(cc-dbg-when 
	 *debug-log*
	 (format *debug-log* "    translate-simple-instruction invoke-multiple-value-call-instruction: ~a~%" 
		 (cc-mir:describe-mir instruction))
	 (format *debug-log* "     instruction --> ~a~%" call-result))
	))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:memref2-instruction) return-value inputs outputs abi)
  (let* ((tptr (%load (first inputs)))
         (offset (second inputs))
         (ui-tptr (%ptrtoint tptr cmp:+uintptr_t+))
         (ui-offset (%bit-cast offset cmp:+uintptr_t+)))
    (let* ((uiptr (%add ui-tptr ui-offset))
           (ptr (%inttoptr uiptr cmp::+t**+))
           (read-val (%load ptr)))
      (%store read-val (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:memset2-instruction) return-value inputs outputs abi)
  (let* ((tptr (%load (first inputs)))
         (offset (second inputs))
         (ui-tptr (%ptrtoint tptr cmp:+uintptr_t+))
         (ui-offset (%bit-cast offset cmp:+uintptr_t+)))
    (let* ((uiptr (%add ui-tptr ui-offset))
           (dest (%inttoptr uiptr cmp::+t**+ "memset2-dest"))
           (val (%load (third inputs) "memset2-val")))
      (%store val dest))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:the-instruction) return-value inputs outputs abi)
  (declare (ignore outputs))
  #+(or) (warn "What should I do with the-instruction")
  )
#||  `(unless (typep ,(first inputs) ',(cleavir-ir:value-type instruction))
(error 'type-error
       :expected-type ',(cleavir-ir:value-type instruction)
       :datum ,(first inputs))))
||#




#+(or)
(progn
  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:tailcall-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(return (funcall ,(first inputs) ,@(rest inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:the-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(unless (typep ,(first inputs) ',(cleavir-ir:value-type instruction))
       (error 'type-error
	      :expected-type ',(cleavir-ir:value-type instruction)
	      :datum ,(first inputs))))


  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:car-instruction) return-value inputs outputs)
    `(setq ,(first outputs)
	   (car ,(first inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:cdr-instruction) return-value inputs outputs)
    `(setq ,(first outputs)
	   (cdr ,(first inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:rplaca-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(rplaca ,(first inputs) ,(second inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:rplacd-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(rplacd ,(first inputs) ,(second inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:t-aref-instruction) return-value inputs outputs)
    `(setq ,(first outputs)
	   (row-major-aref ,(first inputs) ,(second inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:bit-aref-instruction) return-value inputs outputs)
    `(setq ,(first outputs)
	   (row-major-aref ,(first inputs) ,(second inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:unsigned-byte-8-aref-instruction) return-value inputs outputs)
    `(setq ,(first outputs)
	   (row-major-aref ,(first inputs) ,(second inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:short-float-aref-instruction) return-value inputs outputs)
    `(setq ,(first outputs)
	   (row-major-aref ,(first inputs) ,(second inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:single-float-aref-instruction) return-value inputs outputs)
    `(setq ,(first outputs)
	   (row-major-aref ,(first inputs) ,(second inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:double-float-aref-instruction) return-value inputs outputs)
    `(setq ,(first outputs)
	   (row-major-aref ,(first inputs) ,(second inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:long-float-aref-instruction) return-value inputs outputs)
    `(setq ,(first outputs)
	   (row-major-aref ,(first inputs) ,(second inputs))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:t-aset-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(setf (row-major-aref ,(first inputs) ,(second inputs))
	   ,(third inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:bit-aset-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(setf (row-major-aref ,(first inputs) ,(second inputs))
	   ,(third inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:unsigned-byte-8-aset-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(setf (row-major-aref ,(first inputs) ,(second inputs))
	   ,(third inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:short-float-aset-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(setf (row-major-aref ,(first inputs) ,(second inputs))
	   ,(third inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:single-float-aset-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(setf (row-major-aref ,(first inputs) ,(second inputs))
	   ,(third inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:double-float-aset-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(setf (row-major-aref ,(first inputs) ,(second inputs))
	   ,(third inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:long-float-aset-instruction) return-value inputs outputs)
    (declare (ignore outputs))
    `(setf (row-major-aref ,(first inputs) ,(second inputs))
	   ,(third inputs)))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:multiple-to-fixed-instruction) return-value inputs outputs)
    (let ((temp (gensym)))
      `(let ((,temp ,(first inputs)))
	 (declare (ignorable ,temp))
	 ,@(loop for output in outputs
	      collect `(setf ,output (pop ,temp))))))

  (defmethod translate-simple-instruction
      ((instruction cleavir-ir:unwind-instruction) return-value inputs outputs)
    (gensym))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-BRANCH-INSTRUCTION.
(defmethod translate-branch-instruction
    ((instruction cleavir-ir:eq-instruction) return-value inputs outputs successors abi)
  (let ((ceq (cmp:irc-icmp-eq (cmp:irc-load (first inputs)) (cmp:irc-load (second inputs)))))
    (cmp:irc-low-level-trace :flow)
    (cmp:irc-cond-br ceq (first successors) (second successors))))

(defmethod translate-branch-instruction
          ((instruction cleavir-ir:consp-instruction) return-value inputs outputs successors abi)
        (let* ((x (%load (first inputs)))
               (tag (%and (%ptrtoint x cmp::+i32+) (%i32 cmp:+tag-mask+) "tag-only"))
               (cmp (%icmp-eq tag (%i32 cmp:+cons-tag+) "consp-test")))
          (%cond-br cmp (first successors) (second successors))) :likely t)

(defmethod translate-branch-instruction
          ((instruction cleavir-ir:fixnump-instruction) return-value inputs outputs successors abi)
        (let* ((x (%load (first inputs)))
               (tag (%and (%ptrtoint x cmp::+i32+) (%i32 cmp:+fixnum-mask+) "fixnum-tag-only"))
               (cmp (%icmp-eq tag (%i32 cmp:+fixnum-tag+) "fixnump-test")))
          (%cond-br cmp (first successors) (second successors))) :likely t)


(defmethod translate-branch-instruction
    ((instruction cleavir-ir:return-instruction) return-value inputs outputs successors abi)
  (declare (ignore successors))
  (cmp:irc-low-level-trace :flow)
  (llvm-sys:create-ret cmp:*irbuilder* (%load return-value)))


(defmethod translate-branch-instruction
    ((instruction clasp-cleavir-hir:throw-instruction) return-value inputs outputs successors abi)
  (declare (ignore successors))
  (cmp:irc-low-level-trace :flow)
  #+(or)(with-return-values (return-vals return-value abi)
          (cmp:irc-intrinsic "cc_saveMultipleValue0" return-value #|(sret-arg return-vals)|#)
          (cmp:irc-intrinsic "cc_throw" (cmp:irc-load (first inputs))))
  (cmp:irc-intrinsic "cc_throw" (%load (first inputs)) (%load (second inputs)))
  (cmp:irc-unreachable))

(defmethod translate-branch-instruction
    ((instruction clasp-cleavir-hir:landing-pad-return-instruction) return-value inputs outputs successors abi)
  (cmp:irc-low-level-trace :cclasp-eh)
  (cmp:irc-intrinsic "cc_popLandingPadFrame" (cmp:irc-load (car (last inputs))))
  (call-next-method))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-add-instruction) return-value inputs outputs successors abi)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (result-with-overflow (%sadd.with-overflow x y abi)))
    (let ((val (%extract result-with-overflow 0 "result"))
          (overflow (%extract result-with-overflow 1 "overflow")))
      (%store (%inttoptr val cmp:+t*+) (first outputs))
      (%cond-br overflow (second successors) (first successors)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-sub-instruction) return-value inputs outputs successors abi)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (result-with-overflow (%ssub.with-overflow x y abi)))
    (let ((val (%extract result-with-overflow 0 "result"))
          (overflow (%extract result-with-overflow 1 "overflow")))
      (%store (%inttoptr val cmp:+t*+) (first outputs))
      (%cond-br overflow (second successors) (first successors)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-less-instruction) return-value inputs outputs successors abi)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (cmp-lt (%icmp-slt x y)))
      (%cond-br cmp-lt (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-not-greater-instruction) return-value inputs outputs successors abi)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (cmp-lt (%icmp-sle x y)))
      (%cond-br cmp-lt (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-equal-instruction) return-value inputs outputs successors abi)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (cmp-lt (%icmp-eq x y)))
      (%cond-br cmp-lt (first successors) (second successors))))

#+(or)(progn
	(defmethod translate-branch-instruction
	    ((instruction cleavir-ir:typeq-instruction) return-value inputs outputs successors abi)
	  `(if (typep ,(first inputs) ',(cleavir-ir:value-type instruction))
	       (go ,(second successors))
	       (go ,(first successors))))
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
    ((instruction cleavir-ir:funcall-instruction) return-value inputs outputs successors abi)
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
;;    (llvm-sys:setf-no-frame-pointer-elim target-options t)
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
      ((eq 'core:fset (car form))
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
      #+(or)((eq 'cl:eval-when (car form))
             (format t ";    eval-when ~a ~a~%" (cadr form) (caddr form)))
      (t ()))))


(defun my-hir-transformations (init-instr implementation processor os)
  (when *debug-cleavir* (draw-hir init-instr #P"/tmp/hir-before.dot")) ;; comment out
  (cleavir-hir-transformations:type-inference init-instr)
  (when *debug-cleavir* (draw-hir init-instr #P"/tmp/hir-after-ti.dot")) ;; comment out
  (cleavir-hir-transformations:eliminate-typeq init-instr)
  (when *debug-cleavir* (draw-hir init-instr #P"/tmp/hir-after-et.dot")) ;; comment out
  ;; The following breaks code when inlining takes place
  ;;  (cleavir-hir-transformations:eliminate-superfluous-temporaries init-instr)
  ;;  (when *debug-cleavir* (draw-hir init-instr #P"/tmp/hir-after-est.dot")) ;; comment out
  (cleavir-hir-transformations:process-captured-variables init-instr)
  (when *debug-cleavir* (draw-hir init-instr #P"/tmp/hir-after-pcv.dot")) ;; comment out
  )


(defun do-compile (form)
  (cc-dbg-when *debug-log*
               (incf *debug-log-index*)
               (format *debug-log* "====== STARTING TO LOG A NEW FORM - INDEX: ~d~%" *debug-log-index*)
	       (format *debug-log* "==== STARTING!!!!!   Form: ~a~%" form))
  (prog1
      (handler-bind
          ((cleavir-env:no-variable-info
            (lambda (condition)
;;;	  (declare (ignore condition))
              #+verbose-compiler(warn "Condition: ~a" condition)
              (invoke-restart 'cleavir-generate-ast::consider-special)))
           (cleavir-env:no-function-info
            (lambda (condition)
;;;	  (declare (ignore condition))
              #+verbose-compiler(warn "Condition: ~a" condition)
              (invoke-restart 'cleavir-generate-ast::consider-global))))
        (when *compile-print* (describe-form form))
        (cc-dbg-when *debug-log*
                     (format *debug-log* "cleavir-generate-ast:*compiler* --> ~a~%" cleavir-generate-ast:*compiler*)
                     (format *debug-log* "cleavir-generate-ast::*current-form-is-top-level-p* --> ~a~%" 
                             (if (boundp 'cleavir-generate-ast::*current-form-is-top-level-p*) 
                                 cleavir-generate-ast::*current-form-is-top-level-p* 
                                 "UNBOUND" )))
        (let* ((clasp-system *clasp-system*)
               (ast (let ((a (cleavir-generate-ast:generate-ast form *clasp-env* clasp-system)))
                      (when *debug-cleavir* (draw-ast a))
                      a))
               (hoisted-ast (clasp-cleavir-ast:hoist-load-time-value ast))
               (hir (cleavir-ast-to-hir:compile-toplevel hoisted-ast)))
          ;;(warn "Turn on cleavir-remove-useless-instructions:remove-useless-instructions hir - check out Evernote: CLEAVIR-REMOVE-USELESS-INSTRUCTIONS")
          ;; Beach says remove-useless-instructions is a bad idea right now - removing
          ;; (when *debug-cleavir* (draw-hir hir #P"/tmp/hir-pre-r-u-i.dot")) ;; comment out
          ;; (cleavir-remove-useless-instructions:remove-useless-instructions hir)
          ;; (when *debug-cleavir* (draw-hir hir #P"/tmp/hir-post-r-u-i.dot")) ;; comment out
          (cc-dbg-when *debug-log*
                       (let ((ast-pathname (make-pathname :name (format nil "ast~a" *debug-log-index*) 
                                                          :type "dot" 
                                                          :defaults (pathname *debug-log*))))
                         (cleavir-ast-graphviz:draw-ast hoisted-ast (namestring ast-pathname))
                         (format *debug-log* "Wrote ast to: ~a~%" (namestring ast-pathname))))
          (clasp-cleavir:convert-funcalls hir)
          ;; eliminate superfluous temporaries
          (my-hir-transformations hir clasp-system nil nil)
          (when *debug-cleavir* (draw-hir hir #P"/tmp/hir-pre-mir.dot")) ;; comment out
          (cleavir-ir:hir-to-mir hir clasp-system nil nil)
          (when *debug-cleavir* (draw-mir hir)) ;; comment out
          (clasp-cleavir:optimize-stack-enclose hir)
          (cc-mir:assign-mir-instruction-datum-ids hir)
          (setf *ast* hoisted-ast
                *hir* hir)
          (let ((*form* form)
                (abi *abi-x86-64*))
            (clasp-cleavir:finalize-unwind-and-landing-pad-instructions hir)
            (translate hir abi))))
    (cc-dbg-when *debug-log*
                 (format *debug-log* "==== ENDING!!!!!   Form: ~a~%" form))))

;; Set this to T to watch cleavir-compile-t1expr run
(defvar *cleavir-compile-verbose* nil)
(export '*cleavir-compile-verbose*)
(in-package :clasp-cleavir)
(defun cleavir-compile-t1expr (name form env pathname)
  (when *cleavir-compile-verbose*
    (format *trace-output* "Cleavir compiling t1expr: ~s~%" form)
    (format *trace-output* "          in environment: ~s~%" env ))
  (let ((cleavir-generate-ast:*compiler* 'cl:compile))
    (handler-bind
        ((cleavir-env:no-variable-info
          (lambda (condition)
;;;	  (declare (ignore condition))
            #+verbose-compiler(warn "Condition: ~a" condition)
            (invoke-restart 'cleavir-generate-ast::consider-special)))
         (cleavir-env:no-function-info
          (lambda (condition)
;;;	  (declare (ignore condition))
            #+verbose-compiler(warn "Condition: ~a" condition)
            (invoke-restart 'cleavir-generate-ast::consider-global))))
      (multiple-value-bind (fn function-kind wrapped-env lambda-name warnp failp)
          ;; The following test and true form should be removed`
          (cmp:with-debug-info-generator (:module cmp:*the-module* :pathname pathname)
            (let* ((clasp-system *clasp-system*)
                   (ast (cleavir-generate-ast:generate-ast form env clasp-system))
                   (hoisted-ast (clasp-cleavir-ast:hoist-load-time-value ast))
                   (hir (progn
                          (when *debug-cleavir* (draw-ast hoisted-ast)) ;; comment out
                          (cleavir-ast-to-hir:compile-toplevel hoisted-ast))))
              ;;(warn "Turn on cleavir-remove-useless-instructions:remove-useless-instructions hir - check out Evernote: CLEAVIR-REMOVE-USELESS-INSTRUCTIONS")
              ;; Beach says remove-useless-instructions is a bad idea right now - removing
              ;; (when *debug-cleavir* (draw-hir hir #P"/tmp/hir-pre-r-u-i.dot")) ;; comment out
              ;; (cleavir-remove-useless-instructions:remove-useless-instructions hir)
              ;; (when *debug-cleavir* (draw-hir hir #P"/tmp/hir-post-r-u-i.dot")) ;; comment out
              (clasp-cleavir:convert-funcalls hir)
              (my-hir-transformations hir clasp-system nil nil)
              #+(or)(format t "About to draw *debug-cleavir* = ~a~%" *debug-cleavir*)
              (cleavir-ir:hir-to-mir hir clasp-system nil nil) 
              (clasp-cleavir:optimize-stack-enclose hir)
              (cc-mir:assign-mir-instruction-datum-ids hir)
              #|| Moved up ||# #+(or) (clasp-cleavir:convert-funcalls hir)
              (setf *ast* hoisted-ast
                    *hir* hir)
              (let ((*form* form)
                    (abi *abi-x86-64*))
                (clasp-cleavir:finalize-unwind-and-landing-pad-instructions hir)
                (when *debug-cleavir* (draw-mir hir)) ;; comment out
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
                 fn			; llvm-fn
                 nil			; environment
                 cmp:*run-time-literals-external-name*
                 "repl-fn.txt"
                 4321
                 5678
                 nil)))
          (unless (compiled-function-p setup-function)
            (format t "cleavir-clasp compiled code but it didn't result in a compiled-function - eval --> ~s~%" setup-function)
            (return-from cleavir-compile-t1expr (values nil t)))
          (let ((enclosed-function (funcall setup-function)))
            ;;(format t "*all-functions-for-one-compile* -> ~s~%" cmp:*all-functions-for-one-compile*)
            ;;(cmp:set-associated-funcs enclosed-function cmp:*all-functions-for-one-compile*)
            (values enclosed-function warnp failp)))))))

(defun cleavir-compile-file-form (form)
  (let ((cleavir-generate-ast:*compiler* 'cl:compile-file))
    (multiple-value-bind (fn kind #|| more ||#)
	(do-compile form)
      (cmp:with-ltv-function-codegen (result ltv-env)
	(cmp:irc-intrinsic "invokeTopLevelFunction" 
			   result 
			   fn 
			   (cmp:irc-renv ltv-env)
			   (cmp:jit-constant-unique-string-ptr "top-level")
			   cmp:*gv-source-file-info-handle*
			   (cmp:irc-size_t-*current-source-pos-info*-filepos)
			   (cmp:irc-size_t-*current-source-pos-info*-lineno)
			   (cmp:irc-size_t-*current-source-pos-info*-column)
			   cmp:*load-time-value-holder-global-var*)))))

(defun cclasp-compile-in-env (name form &optional env)
  (let ((cleavir-generate-ast:*compiler* 'cl:compile)
	(cmp:*all-functions-for-one-compile* nil))
    (cmp:compile-in-env name form env #'cleavir-compile-t1expr)))
        
	
(defun cleavir-compile (name form &key (debug *debug-cleavir*))
  (let ((cmp:*dump-module-on-completion* debug)
	(*debug-cleavir* debug))
    (cclasp-compile-in-env name form nil)))

(defun cleavir-compile-file (given-input-pathname &rest args)
  (let ((*debug-log-index* 0)
	(cleavir-generate-ast:*compiler* 'cl:compile-file)
        (cmp:*cleavir-compile-file-hook* 'cleavir-compile-file-form))
    (apply #'cmp::compile-file* #'cleavir-compile-file-form given-input-pathname args)))

(defmacro with-debug-compile-file ((log-file &key debug-log-on) &rest body)
  `(with-open-file (clasp-cleavir::*debug-log* ,log-file :direction :output)
     (let ((clasp-cleavir::*debug-log-on* ,debug-log-on))
       ,@body)))
(export 'with-debug-compile-file)


(defmacro open-debug-log (log-file)
  `(eval-when (:compile-toplevel)
     (setq *debug-log* ,log-file :direction :output)
     (setq *debug-log-on* t)))

(defmacro close-debug-log ()
  `(eval-when (:compile-toplevel)
     (setq *debug-log-on* nil)
     (close *debug-log*)
     (setq *debug-log* nil)))
(export '(open-debug-log close-debug-log))
  
