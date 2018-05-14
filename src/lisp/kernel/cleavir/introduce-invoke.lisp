;;;; Written by Robert Strandh 2015
;;;;

(in-package :clasp-cleavir)

;;;; We will not introduce any LANDING-PAD instruction.  The reason for
;;;; that is that it would create an incorrect control flow in the
;;;; flowchart.  Take for instance the following code:
;;;;
;;;;    (tagbody 
;;;;       (ff (lambda () (go a)) (lambda () (go b)))
;;;;     a
;;;;       (print 'a)
;;;;       (go out)
;;;;     b
;;;;       (print 'b)
;;;;     out)
;;;;
;;;; The (GO A) form generates an UNWIND-INSTRUCTION having the form
;;;; (PRINT 'A) as its successor, and the (GO B) form generates an
;;;; UNWIND-INSTRUCTION having the form (PRINT 'B) as its successor.
;;;; If we use a LANDING-PAD instruction with (PRINT 'A) and (PRINT 'B)
;;;; as its successors, then we give the false impression that there
;;;; is a possible control flow from (GO A) to (PRINT 'B) and vice
;;;; versa.
;;;;
;;;; So we keep the graph as it is.
;;;; Each function that is the target of an UNWIND
;;;; gets a unique landing pad that is the unwind return of every invoke in
;;;; the function.  That landing pad can jump to every successor in the function.
;;;; but we convert some
;;;; FUNCALL-INSTRUCTIONs to INVOKE-INSTRUCTIONs and each
;;;; INVOKE-INSTRUCTION contains the unique landing pad for the
;;;; function.

;;; Return a list of all the UNWIND-INSTRUCTIONs in a program
;;; represented as a flowchart.
(defun find-unwinds (initial-instruction)
  (let ((result '()))
    (cleavir-ir:map-instructions-with-owner
     (lambda (instruction owner)
       (declare (ignore owner))
       (when (typep instruction 'cleavir-ir:unwind-instruction)
	 (push instruction result)))
     initial-instruction)
    result))

;;; The function-info class represents information about a function that some instuctions
;;; will need. %unwinds is a list of instructions.  The elements of that list are
;;; the successors of each UNWIND-INSTRUCTION that has the function as its
;;; invocation.
;;
;; I use HIR:landing-pad-instruction
(defclass function-info ()
  ((%enter-instruction :initarg :enter-instruction :accessor enter-instruction)
   (%debug-on :initform nil :accessor debug-on)
   (%landing-pad-for-cleanup :initform nil :accessor landing-pad-for-cleanup)
   (%on-entry-for-cleanup :initform nil :accessor on-entry-for-cleanup)
   (%on-exit-for-cleanup :initform nil :accessor on-exit-for-cleanup)
   (%calling-convention :initarg :calling-convention :accessor calling-convention)
   (%unwind-target :initarg :unwind-target :accessor unwind-target)
   (%exn.slot :initform nil :accessor exn.slot)
   (%ehselector.slot :initform nil :accessor ehselector.slot)
   (%unwinds :initarg :unwinds :initform nil :accessor unwinds)
   (%landing-pad-for-unwind :initform nil :accessor landing-pad-for-unwind)
   (%on-entry-for-unwind :initform nil :accessor on-entry-for-unwind)
   (%on-exit-for-unwind :initform nil :accessor on-exit-for-unwind)))


;;; Given a list of all the UNWIND-INSTRUCTIONs in a program, create a
;;; list of lists that have the form (ENTER-INSTRUCTION FUNCTION-INFO LEXICAL-VARIABLE)
;;; The ENTER-INSTRUCTION is for a function that is the
;;; invocation of one or more UNWIND-INSTRUCTIONs;
;;; The FUNCTION-INFO has unwind-target set to T to indicate that the function
;;; corresponding to the ENTER-INSTRUCTION is an unwind target and
;;; the LEXICAL-VARIABLE is the 'frame-holder' - the result of calling MAKE-TEMP.
;;; REMEMBER the following from the cleavir documentation on the
;;; UNWIND instruction...
;;; The invocation of the UNWIND-INSTRUCTION is the
;;; ENTER-INSTRUCTION that represents the function invocation at
;;; which execution should continue after the stack has been
;;; unwound.
;;;
;;; You could rewrite this to use a hash-table of invocations to function-info and
;;; frame-holder
(defun compute-function-info-and-maybe-closed-over-unwind-argument (unwinds)
  (loop with remaining = unwinds
        for enter = (when remaining (cleavir-ir:invocation (first remaining)))
        for current-unwinds = (when remaining
                                (remove enter remaining
                                        :key #'cleavir-ir:invocation
                                        :test-not #'eq))
        until (null remaining)
        collect (let ((frame-holder (cleavir-ast-to-hir::make-temp)))
                  (list enter (make-instance 'function-info :unwind-target t) frame-holder))
        do (setf remaining
                 (set-difference remaining current-unwinds))))

;;; Given a function F that is the invocation of at least one
;;; UNWIND-INSTRUCTION.  We convert every FUNCALL-INSTRUCTION in F to
;;; an instance of the class INVOKE-INSTRUCTION.  The difference
;;; between a FUNCALL-INSTRUCTION and an INVOKE-INSTRUCTION is that
;;; the latter contains an instance of the class LANDING-PAD.  There
;;; is a single such instance shared by all the INVOKE-INSTRUCTIONs of
;;; F.
(defclass invoke-instruction (cleavir-ir:funcall-instruction)
  ((%function-info :initarg :function-info :reader function-info)))

(defmethod cleavir-ir-graphviz:label ((instr clasp-cleavir:invoke-instruction))
  "invoke")

(defclass invoke-multiple-value-call-instruction (cleavir-ir:multiple-value-call-instruction)
  ((%function-info :initarg :function-info :reader function-info)))

(defmethod cleavir-ir-graphviz:label ((instr clasp-cleavir:invoke-multiple-value-call-instruction))
  "invoke-MVC")

(defun convert-funcalls (initial-instruction)
  (let* ((unwinds (find-unwinds initial-instruction))
	 (enter-instruction-function-infos (compute-function-info-and-maybe-closed-over-unwind-argument unwinds))
         (map-enter-to-function-info (make-hash-table)))
    (cc-dbg-when
     clasp-cleavir:*debug-log*
     (format clasp-cleavir:*debug-log* "enter-instruction-function-infos: ~a~%" (length enter-instruction-function-infos))
     (dolist (elp enter-instruction-function-infos)
       (format clasp-cleavir:*debug-log* "Landing-pad in ~a~%"
               (cc-mir:describe-mir (car elp)))))
    (cleavir-ir:map-instructions-with-owner
     (lambda (instruction owner)
       (cond
	 ((typep instruction 'cleavir-ir:funcall-instruction)
	  (let ((function-info (second (assoc owner enter-instruction-function-infos))))
	    (unless (null function-info)
	      (cc-dbg-when *debug-log*
			   (format *debug-log* "Translating call to invoke: ~a~%" 
				   (cc-mir:describe-mir instruction)))
	      (change-class instruction 'invoke-instruction
			    :function-info function-info))))
	 ((typep instruction 'cleavir-ir:multiple-value-call-instruction)
	  (let ((function-info (second (assoc owner enter-instruction-function-infos))))
	    (unless (null function-info)
	      (cc-dbg-when *debug-log*
			   (format *debug-log* "Translating multiple-value-call to invoke-multiple-value-call: ~a~%" 
				   (cc-mir:describe-mir instruction)))
	      (change-class instruction 'invoke-multiple-value-call-instruction
			    :function-info function-info))))
	 (t nil)))
     initial-instruction)
    ;; Associate function-info with its enter instruction
    (dolist (one-function-info enter-instruction-function-infos)
      (destructuring-bind (enter function-info frame-holder)
          one-function-info
        ;; associate the enter with a landing pad
        (setf (gethash enter map-enter-to-function-info) function-info)
	(setf (clasp-cleavir-hir:frame-holder enter) frame-holder)))
    ;;
    ;; Now change all of the unwind-instructions to indexed-unwind-instructions
    ;;
    (dolist (unwind unwinds)
      (let* ((enter (cleavir-ir:invocation unwind)))
	(change-class unwind 'clasp-cleavir-hir:indexed-unwind-instruction
		      :inputs (list (clasp-cleavir-hir:frame-holder enter)))))
    map-enter-to-function-info))

(defun finalize-unwind-and-landing-pad-instructions (initial-instruction map-enter-to-function-info)
  ;; Identify all UNWIND-INSTRUCTIONs and LANDING-PAD-INSTRUCTIONs
  ;; under the initial-instruction
  ;; change all return-instruction(s) that are owned by
  ;; enter-instruction(s) that are associated with function-infos 
  ;; to landing-pad-return-instruction(s)
  (let (unwinds enters-with-landing-pad landing-pad-returns)
    (cleavir-ir:map-instructions-with-owner
     (lambda (instruction owner)
       (cond
	 ((typep instruction 'cleavir-ir:unwind-instruction)
	  (push instruction unwinds))
         ((typep instruction 'cleavir-ir:enter-instruction)
          (when (gethash instruction map-enter-to-function-info)
            (push instruction enters-with-landing-pad)))
     	 ((and (typep instruction 'cleavir-ir:return-instruction)
               (gethash owner map-enter-to-function-info))
	  (change-class instruction
			'clasp-cleavir-hir:landing-pad-return-instruction
			:inputs (append (cleavir-ir:inputs instruction) (list (clasp-cleavir-hir:frame-holder owner)))
			:function-info (gethash owner map-enter-to-function-info)))))
     initial-instruction)
    ;; add each unwind to its corresponding function-info
    (dolist (unwind unwinds)
      (let* ((enter (cleavir-ir:invocation unwind))
	     (function-info (gethash enter map-enter-to-function-info)))
	(push unwind (unwinds function-info))))
    ;; Set the jump-id of each unwind according to where it is in its function-info unwind list
    (dolist (unwind unwinds)
      (let* ((enter (cleavir-ir:invocation unwind))
	     (function-info (gethash enter map-enter-to-function-info))
	     (jump-id (position unwind (unwinds function-info))))
	(setf (clasp-cleavir-hir:jump-id unwind) jump-id)))))
