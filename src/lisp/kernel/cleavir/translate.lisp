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

(defvar *debug-basic-blocks*)
(defvar *debug-ownerships*)
(defvar *debug-tags*)
(defvar *debug-vars*)


;; The current function being compiled
(defvar *current-function* nil)

(defun translate-datum (datum)
  (if (typep datum 'cleavir-ir:constant-input)
      (break "Get datum into the load-time-values array") ;;`(quote ,(cleavir-ir:value datum))
      (let ((var (gethash datum *vars*)))
	(when (null var)
	  (cond
	    ((typep datum 'cleavir-ir:values-location)
	     (warn "Get the values-location and setf var"))
	    ((typep datum 'cleavir-ir:immediate-input)
	     (setf var (cmp:jit-constant-i32 (cleavir-ir:value datum))))
	    ((typep datum 'cleavir-ir:dynamic-lexical-location)
	     (setf var (llvm-sys:create-alloca cmp:*irbuilder* cmp:+t*+ (cmp:jit-constant-i32 1) (string (cleavir-ir:name datum)))))
	    ((typep datum 'cleavir-ir:load-time-value-input)
	     (format t "load-time-value-input - what does the datum look like: ~a~%" datum)
	     (warn "Get the load-time-value-input and setf var"))
	    (t (error "translate datum: ~a~%" datum))))
	(setf (gethash datum *vars*) var)
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
    ;; HYPOTHESIS: This builds a function with no arguments
    ;; that will enclose and set up other functions with arguments
    (let* ((fn (llvm-sys:function-create
		cmp:+fn-prototype+
		'llvm-sys:internal-linkage
		(cmp:jit-function-name "REPL")
		cmp:*the-module*))
	   (*current-function* fn)
	   (block (cmp:irc-basic-block-create "entry" fn))
	   (irbuilder (llvm-sys:make-irbuilder cmp:*llvm-context*)))
      (let ((args (llvm-sys:get-argument-list fn)))
	(mapcar #'(lambda (arg argname) (llvm-sys:set-name arg argname))
		(llvm-sys:get-argument-list fn) cmp:+fn-prototype-argument-names+)
	;; Set the first argument attribute to be sret
	(if args
	    (let ((attribute-set (llvm-sys:attribute-set-get cmp:*llvm-context* 1 (list 'llvm-sys:attribute-struct-ret))))
	      (llvm-sys:add-attr (first args) attribute-set))))
      ;; Assign tags to all basic block except the first one
      (loop for block in rest
	 for instruction = (first block)
	 do (setf (gethash instruction *tags*) (cmp:irc-basic-block-create "tag" fn)))
      (llvm-sys:set-insert-point-basic-block irbuilder block)
      ;; HYPOTHESIS: bind variables for every var owned by this
      ;; initial instruction and that are NOT outputs of the initial
      ;; instruction (passed arguments)  I think I should use passed arguments
      ;; to create allocas for them.
      (cmp:with-irbuilder (irbuilder)
	(cmp:with-dbg-function ("repl-FIX"
				:linkage-name "repl-FIX-LINKAGE-NAME"
				:function fn
				:function-type cmp:+fn-prototype+
				:form *form*)
	  (cmp:with-dbg-lexical-block (*form*)
	    (loop for var being each hash-key of *ownerships*
	       using (hash-value owner)
	       when (and (typep var '(or
				      cleavir-ir:lexical-location
				      cleavir-ir:values-location))
			 (eq owner initial-instruction)
			 #+(or)(not (member var (cleavir-ir:outputs
						 initial-instruction))))
	       collect (translate-datum var))
	    (layout-basic-block first)
	    #+(or)(loop for basic-block in rest
		     do (progn
			  (irc-begin-block (string (gethash (first basic-block) *tags*)))
			  (layout-basic-block basic-block)))
	    )))
      (values fn :function-kind nil :lambda-name nil nil))))

(defun translate (initial-instruction)
  (let* ((ownerships
	  (cleavir-hir-transformations:compute-ownerships initial-instruction)))
    (let*((*ownerships* ownerships)
	  (*basic-blocks* (cleavir-basic-blocks:basic-blocks initial-instruction))
	  (*tags* (make-hash-table :test #'eq))
	  (*vars* (make-hash-table :test #'eq)))
      (setf *debug-basic-blocks* *basic-blocks*
	    *debug-ownerships* *ownerships*
	    *debug-tags* *tags*
	    *debug-vars* *vars*)
      (layout-procedure initial-instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-SIMPLE-INSTRUCTION.

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enclose-instruction) inputs outputs)
  (declare (ignore inputs))
  (let ((enter-instruction (cleavir-ir:code instruction)))
    (format t "~a~%" `(setq ,(first outputs)
			    (lambda ,(translate-lambda-list
				      (cleavir-ir:lambda-list enter-instruction))
			      ,(layout-procedure enter-instruction))))))

(defmethod translate-simple-instruction
    ((instr cleavir-ir:enter-instruction) inputs outputs)
  (declare (ignore inputs outputs))
  ;; (gensym)
  (let* ((lambda-list (cleavir-ir:lambda-list instr))
	 (static-environment-output (first (cleavir-ir:outputs instr))))
    (format t "lambda-list: ~a~%" lambda-list)
    (format t "static-environment-output: ~a~%" static-environment-output)
    (multiple-value-bind (reqs opts restarg key-flag keys allow-other-keys auxs)
	(core:process-lambda-list lambda-list 'core::function)
      (format t "reqs: ~a~%" reqs)
      (format t "opts: ~a~%" opts)
      (format t "key-flag: ~a~%" key-flag)
      (format t "keys: ~a~%" keys)
      (format t "allow-other-keys: ~a~%" allow-other-keys)
      (format t "Specializing enter-instruction~%")
      )))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:instruction) inputs outputs)
  (format t "--------------- translate-simple-instruction ~a~%" instruction)
  (format t "    inputs: ~a~%" inputs)
  (format t "    outputs: ~a~%" outputs))

#+(or)(progn
	(defmethod translate-simple-instruction
	    ((instruction cleavir-ir:assignment-instruction) inputs outputs)
	  (format t "--------------- translate-simple-instruction assignment-instruction~%")
	  (format t "    inputs: ~a~%" inputs)
	  (format t "    outputs: ~a~%" outputs)
	  (break "Check arguments"))


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
	)
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
  (cmp:irc-ret-void))

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
	 (hoisted-ast (clasp-cleavir-ast:hoist-load-time-value ast))
	 (hir (cleavir-ast-to-hir:compile-toplevel hoisted-ast))
	 (clasp-inst (make-instance 'clasp)))
    (cleavir-hir-transformations:hir-transformations hir clasp-inst nil nil)
    (cleavir-ir:hir-to-mir hir clasp-inst nil nil)
    (draw-mir hir)
    (setf *ast* hoisted-ast
	  *hir* hir)
    (let ((*form* form))
      (translate hir))
    ))

(defun cleavir-compile (name form)
  (let ((cmp:*cleavir-compile-hook* #'cleavir-compile-t1expr)
	(cmp:*dump-module-on-completion* t))
    (compile name form)))

#+(or)(defun interpret-hir (initial-instruction)
	(funcall (compile nil `(lambda () ,(translate initial-instruction)))))
