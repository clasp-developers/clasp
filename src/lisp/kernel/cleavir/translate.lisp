(cl:in-package #:clasp-cleavir)

;;; The first argument to this function is an instruction that has a
;;; single successor.  Whether a GO is required at the end of this
;;; function is determined by the code layout algorithm.  
;;; 
;;; The inputs are forms to be evaluated.  The outputs are symbols
;;; that are names of variables.
(defgeneric translate-simple-instruction (instruction inputs outputs))

(defgeneric translate-branch-instruction (instruction inputs outputs successors))

(defvar *basic-blocks*)
(defvar *ownerships*)
(defvar *tags*)
(defvar *vars*)

(defun translate-datum (datum)
  (if (typep datum 'cleavir-ir:constant-input)
      `(quote ,(cleavir-ir:value datum))
      (let ((var (gethash datum *vars*)))
	(when (null var)
	  (setf var (gensym))
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

(defun layout-basic-block (basic-block)
  (destructuring-bind (first last owner) basic-block
    (declare (ignore owner))
    (append (loop for instruction = first
		    then (first (cleavir-ir:successors instruction))
		  for inputs = (cleavir-ir:inputs instruction)
		  for input-vars = (mapcar #'translate-datum inputs)
		  for outputs = (cleavir-ir:outputs instruction)
		  for output-vars = (mapcar #'translate-datum outputs)
		  until (eq instruction last)
		  collect (translate-simple-instruction
			   instruction input-vars output-vars))
	    (let* ((inputs (cleavir-ir:inputs last))
		   (input-vars (mapcar #'translate-datum inputs))
		   (outputs (cleavir-ir:outputs last))
		   (output-vars (mapcar #'translate-datum outputs))
		   (successors (cleavir-ir:successors last))
		   (successor-tags (loop for successor in successors
					 collect (gethash successor *tags*))))
	      (if (= (length successors) 1)
		  (list (translate-simple-instruction
			 last input-vars output-vars)
			`(go ,(gethash (first successors) *tags*)))
		  (list (translate-branch-instruction
			 last input-vars output-vars successor-tags)))))))

(defun layout-procedure (initial-instruction)
  ;; I think this removes every basic-block that
  ;; isn't owned by this initial-instruction
  (let* ((basic-blocks (remove initial-instruction
			       *basic-blocks*
			       :test-not #'eq :key #'third))
	 ;; Hypothesis: This finds the first basic block
	 (first (find initial-instruction basic-blocks
		      :test #'eq :key #'first))
	 ;; This gathers the rest of the basic blocks
	 (rest (remove first basic-blocks :test #'eq)))
    ;; Assign tags to all basic block except the first one
    (loop for block in rest
       for instruction = (first block)
       do (setf (gethash instruction *tags*) (gensym)))
    ;; HYPOTHESIS: This builds a function with no arguments
    ;; that will enclose and set up other functions with arguments
    (let* ((fn (with-ir-function ("repl" :function-type cmp:+fn-void+)))
	   (block (cmp:irc-basic-block-create "entry" fn))
	   (irbuilder (llvm-sys:make-irbuilder cmp:*llvm-context*)))
      (llvm-sys:set-insert-point-basic-block irbuilder block)
      ;; HYPOTHESIS: bind variables for every var owned by this
      ;; initial instruction and that are NOT outputs of the initial
      ;; instruction (passed arguments)  I think I should use passed arguments
      ;; to create allocas for them.
      (cmp:with-irbuilder (irbuilder)
	#+(or)(progn
		(loop for var being each hash-key of *ownerships*
		   using (hash-value owner)
		   when (and (typep var '(or
					  cleavir-ir:lexical-location
					  cleavir-ir:values-location))
			     (eq owner initial-instruction)
			     #+(or)(not (member var (cleavir-ir:outputs
						     initial-instruction))))
		   do (llvm-sys:create-alloca *irbuilder* +tsp+ (jit-constant-i32 1) (string (translate-datum var)))
		     #||collect (translate-datum var)||#)
		(layout-basic-block first)
		(loop for basic-block in rest
		   do (progn
			(irc-begin-block (string (gethash (first basic-block) *tags*)))
			(layout-basic-block basic-block))))
	(llvm-sys:create-ret-void cmp:*irbuilder*)
	))))

(defun translate (initial-instruction)
  (let* ((ownerships
	   (cleavir-hir-transformations:compute-ownerships initial-instruction))
	 (*ownerships* ownerships)
	 (*basic-blocks* (cleavir-basic-blocks:basic-blocks initial-instruction))
	 (*tags* (make-hash-table :test #'eq))
	 (*vars* (make-hash-table :test #'eq)))
    (layout-procedure initial-instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-SIMPLE-INSTRUCTION.

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enclose-instruction) inputs outputs)
  (declare (ignore inputs))
  (let ((enter-instruction (cleavir-ir:code instruction)))
    `(setq ,(first outputs)
	   (lambda ,(translate-lambda-list
		     (cleavir-ir:lambda-list enter-instruction))
	     ,(layout-procedure enter-instruction)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enter-instruction) inputs outputs)
  (declare (ignore inputs outputs))
  (gensym))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:assignment-instruction) inputs outputs)
  `(setq ,(first outputs) ,(first inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:funcall-instruction) inputs outputs)
  `(setf ,(first outputs)
	 (multiple-value-list (funcall ,(first inputs) ,@(rest inputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:multiple-value-call-instruction) inputs outputs)
  `(setf ,(first outputs)
	 (multiple-value-list
	  (funcall ,(first inputs)
		   (append ,@(rest inputs))))))

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
    ((instruction cleavir-ir:fdefinition-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (fdefinition ,(first inputs))))

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
    ((instruction cleavir-ir:fixed-to-multiple-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (list ,@inputs)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-BRANCH-INSTRUCTION.

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:eq-instruction) inputs outputs successors)
  `(if (eq ,(first inputs) ,(second inputs))
       (go ,(first successors))
       (go ,(second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:typeq-instruction) inputs outputs successors)
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

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:return-instruction) inputs outputs successors)
  (declare (ignore successors))
  `(return (apply #'values ,(first inputs))))

;;; When the FUNCALL-INSTRUCTION is the last instruction of a basic
;;; block, it is because there is a call to a function that will never
;;; return, such as ERROR, and the instruction then has no successors
;;; (which is why it is at the end of the basic block).
;;;
;;; We therefore must provide a method on TRANSLATE-BRANCH-INSTRUCTION
;;; (in addition to the method on TRANSLATE-SIMPLE-INSTRUCTION)
;;; specialized to FUNCALL-INSTRUCTION.
(defmethod translate-branch-instruction
    ((instruction cleavir-ir:funcall-instruction) inputs outputs successors)
  (declare (ignore outputs successors))
  `(funcall ,(first inputs) ,@(rest inputs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main entry point.
;;;
;;; This will compile a top-level form (doing all tlf processing)
;;; into the current *module*

(defun cleavir-compile-t1expr (name form env)
  (and env (error "I don't support anything but top level environment compiles using cleavir"))
  (let* ((cleavir-generate-ast:*compiler* 'cl:compile)
	 (ast (cleavir-generate-ast:generate-ast form *clasp-env*))
	 (hir (cleavir-ast-to-hir:compile-toplevel ast))
	 (clasp-inst (make-instance 'clasp)))
    (cleavir-hir-transformations:hir-transformations hir clasp-inst nil nil)
    (cleavir-ir:hir-to-mir hir clasp-inst nil nil)
    (setf *form* form
	  *ast* ast
	  *hir* hir)
    (translate hir)
    (format t "About to dump cmp:*the-module*~%")
    (llvm-sys:dump cmp:*the-module*)
    t))

(defun cleavir-compile (name form)
  (let ((cmp:*cleavir-compile-hook* #'cleavir-compile-t1expr))
    (compile name form)))

#+(or)(defun interpret-hir (initial-instruction)
	(funcall (compile nil `(lambda () ,(translate initial-instruction)))))
