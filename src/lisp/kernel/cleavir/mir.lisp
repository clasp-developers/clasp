(in-package #:cc-mir)

(defun insert-after (new old)
  (cleavir-ir:insert-instruction-after new old)
  new)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; STACK-ENCLOSE
;;;
;;; Stores a core::ClosureWithSlots* pointer on the stack
;;;

(defclass stack-enclose-instruction (cleavir-ir:enclose-instruction) ())

(defun make-stack-enclose-instruction (output successor code)
  (make-instance 'stack-enclose-instruction
                 :outputs (list output)
                 :successors (list successor)
                 :code code))

(defmethod cleavir-ir-graphviz:draw-instruction ((instruction enclose-instruction) stream)
  (format stream "   ~a [label = \"stack-enclose\"];~%"
	  (instruction-id instruction))
  (format stream "  ~a -> ~a [color = pink, style = dashed];~%"
	  (gethash (code instruction) *instruction-table*)
	  (instruction-id instruction)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLOSURE-POINTER-DYNAMIC-LEXICAL-LOCATION
;;;
;;; Stores a core::Closure* pointer
;;;
(defclass closure-pointer-dynamic-lexical-location (cleavir-ir:dynamic-lexical-location) ())


(defmethod cleavir-ir-graphviz:draw-datum ((datum closure-pointer-dynamic-lexical-location) stream)
  (format stream "  ~a [shape = octagon, style = filled];~%"
	  (cleavir-ir-graphviz::datum-id datum))
  (format stream "   ~a [fillcolor = cyan, label = \"~a\"]~%"
	  (cleavir-ir-graphviz::datum-id datum) (cleavir-ir-graphviz::name datum)))




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




  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Assign a unique integer index to every instruction
;;; using the stealth mixin - see system.lisp
;;;

(defun assign-mir-instruction-datum-ids (top)
  (let ((id 1)
	(datums (make-hash-table)))
    (cleavir-ir:map-instructions 
     (lambda (instr)
       (setf (clasp-cleavir:instruction-gid instr) (incf id))
       (loop for datum in (append (cleavir-ir:inputs instr) (cleavir-ir:outputs instr))
	    do (unless (gethash datum datums)
		 (setf (gethash datum datums) t)
		 (setf (clasp-cleavir:datum-gid datum) (incf id)))))
     top)))


(defgeneric label-datum (datum))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum CONSTANT-INPUT.

(defmethod label-datum ((datum cleavir-ir:constant-input))
  (cleavir-ir:value datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum LEXICAL-LOCATION.

(defmethod label-datum ((datum cleavir-ir:lexical-location))
  (cleavir-ir:name datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum DYNAMIC-LEXICAL-LOCATION.

(defmethod label-datum ((datum cleavir-ir:dynamic-lexical-location))
  (cleavir-ir:name datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum STATIC-LEXICAL-LOCATION.

(defmethod label-datum ((datum cleavir-ir:static-lexical-location))
  (cleavir-ir:name datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum VALUES-LOCATION.

(defmethod label-datum ((datum cleavir-ir:values-location)) "V")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum IMMEDIATE-INPUT.

(defmethod label-datum ((datum cleavir-ir:immediate-input)) (cleavir-ir:value datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum LOAD-TIME-VALUE-INPUT.

(defmethod label-datum ((datum cleavir-ir:load-time-value-input))
  "LTV")


(defun describe-mir (instr )
  (with-output-to-string (stream)
    (format stream "~a " (cleavir-ir-graphviz:label instr))
    (when (cleavir-ir:inputs instr)
      (format stream "(")
      (loop for datum in (cleavir-ir:inputs instr)
	 do (format stream "~a " (label-datum datum)))
      (format stream ")"))
    (when (or (cleavir-ir:inputs instr) (cleavir-ir:outputs instr))
      (format stream " -> "))
    (when (cleavir-ir:outputs instr)
      (format stream "(")
      (loop for datum in (cleavir-ir:outputs instr)
	 do (format stream "~a " (label-datum datum)))
      (format stream ")"))))
