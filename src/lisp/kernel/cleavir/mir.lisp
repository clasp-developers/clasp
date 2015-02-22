(in-package #:cc-mir)

(defun insert-after (new old)
  (cleavir-ir:insert-instruction-after new old)
  new)



(defclass enter-instruction (cleavir-ir:enter-instruction)
  ((%debug-label :initform (gensym "ENTER-") :reader debug-label)))


(defmethod cleavir-ir-graphviz:label ((instr enter-instruction))
  (with-output-to-string (stream)
    (format stream "~a ~a" (debug-label instr) (mapcar #'cleavir-ir-graphviz::format-item (cleavir-ir:lambda-list instr)))))

(defmethod cl:print-object ((instr enter-instruction) stream)
  (format stream "#<~a ~a>" (class-name (class-of instr)) (debug-label instr)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLOSURE-POINTER-DYNAMIC-LEXICAL-LOCATION
;;;
;;; Stores a core::Closure* pointer
;;;
(defclass closure-pointer-dynamic-lexical-location (cleavir-ir:dynamic-lexical-location) ())


(defmethod cleavir-ir-graphviz:draw-datum ((datum closure-pointer-dynamic-lexical-location) stream)
  (format stream "  ~a [shape = octagon, style = filled];~%"
	  (cleavir-ir-graphviz:datum-id datum))
  (format stream "   ~a [fillcolor = cyan, label = \"~a\"]~%"
	  (cleavir-ir-graphviz:datum-id datum) (cleavir-ir-graphviz:name datum)))




#+(or)(progn
	(defclass check-min-arguments (llvm-instruction cleavir-ir:one-successor-mixin)
	  ((%fixed-arg-num :initarg :fixed-arg-num :accessor fixed-arg-num)))

	(defmethod cleavir-ir-graphviz:label ((instruction check-min-arguments))
	  (with-output-to-string (stream)
	    (format stream "CHECK-MIN-ARGUMENTS ~a" (fixed-arg-num instruction))))

	(defclass bind-argument-static-environment (llvm-instruction cleavir-ir:one-successor-mixin) ())
	(defclass bind-fixed-arg (llvm-instruction cleavir-ir:one-successor-mixin) 
	  ((%arg-index :initarg :arg-index :accessor arg-index)))

	(defmethod cleavir-ir-graphviz:label ((instruction bind-fixed-arg))
	  (with-output-to-string (stream)
	    (format stream "BIND-FIXED-ARG ~a" (arg-index instruction))))

	(defmethod cleavir-ir:specialize ((instr cleavir-ir:enter-instruction)
					  (impl clasp) proc os)
	  (let* ((lambda-list (cleavir-ir:lambda-list instr))
		 (static-environment-output (first (cleavir-ir:outputs instr)))
		 (cur-instr (insert-after (make-instance 'bind-argument-static-environment
							 :outputs (list (pop (cleavir-ir:outputs instr))))
					  instr))
		 (arg-index 0))
	    (format t "lambda-list: ~a~%" lambda-list)
	    (multiple-value-bind (reqs opts restarg key-flag keys allow-other-keys auxs)
		(core:process-lambda-list lambda-list 'core::function)
	      (format t "reqs: ~a~%" reqs)
	      (format t "opts: ~a~%" opts)
	      (format t "key-flag: ~a~%" key-flag)
	      (format t "keys: ~a~%" keys)
	      (format t "allow-other-keys: ~a~%" allow-other-keys)
	      (format t "Specializing enter-instruction~%")
	      (setq cur-instr (insert-after (make-instance 'check-min-arguments
							   :fixed-arg-num (car reqs))
					    cur-instr))
	      (dolist (cur-req (cdr reqs))
		(setq cur-instr (insert-after (make-instance 'bind-fixed-arg
							     :arg-index arg-index
							     :outputs (list (pop (cleavir-ir:outputs instr))))
					      cur-instr))
		(incf arg-index))
	      (change-class instr 'enter-instruction))))
	)




  
