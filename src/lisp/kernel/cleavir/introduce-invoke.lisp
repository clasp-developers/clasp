;;;; Written by Robert Strandh 2015
;;;;

(in-package :clasp-cleavir)

;;;; We do not introduce any LANDING-PAD instruction.  The reason for
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
;;;; UNWIND-INSTRUCTION having the form (PRINT 'B) as its successor If
;;;; we use a LANDING-PAD instruction with (PRINT 'A) and (PRINT 'B)
;;;; as its successors, then we give the false impression that there
;;;; is a possible control flow from (GO A) to (PRINT 'B) and vice
;;;; versa.
;;;;
;;;; So we keep the graph as it is, but we convert some
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

;;; This class represents the landing pad for an entire function F.
;;; TARGETS is a list of instructions.  The elements of that list are
;;; the successors of each UNWIND-INSTRUCTION that has F as its
;;; invocation.
;;
;; I use HIR:landing-pad-instruction
(defclass landing-pad ()
  ((%unwinds :initarg :unwinds :initform nil :accessor unwinds)
   (%basic-block :initarg :basic-block :accessor basic-block)))

;;; Given a list of all the UNWIND-INSTRUCTIONs in a program, create a
;;; list (an association list) of CONS cell such that the CAR of each
;;; CONS cell is the ENTER-INSTRUCTION of a function that is the
;;; invocation of at least one UNWIND-INSTRUCTION and the CDR of the
;;; CONS cell is a landing pad containing a list of the successors of
;;; the UNWIND-INSTRUCTIONs that have that ENTER-INSTRUCTION as their
;;; invocation.
(defun compute-landing-pads (unwinds)
  (loop with remaining = unwinds
     for enter = (when remaining (cleavir-ir:invocation (first remaining)))
     for current-unwinds = (when remaining
			     (remove enter remaining
				     :key #'cleavir-ir:invocation
				     :test-not #'eq))
     until (null remaining)
     collect (let ((frame-holder (cleavir-ast-to-hir::make-temp)))
	       (list enter (make-instance 'landing-pad) frame-holder))
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
  ((%landing-pad :initarg :landing-pad :reader landing-pad)))

(defmethod cleavir-ir-graphviz:label ((instr clasp-cleavir:invoke-instruction))
  "invoke")


(defclass invoke-multiple-value-call-instruction (cleavir-ir:multiple-value-call-instruction)
  ((%landing-pad :initarg :landing-pad :reader landing-pad)))

(defmethod cleavir-ir-graphviz:label ((instr clasp-cleavir:invoke-multiple-value-call-instruction))
  "invoke-MVC")



(defun convert-funcalls (initial-instruction)
  (let* ((unwinds (find-unwinds initial-instruction))
	 (landing-pads (compute-landing-pads unwinds)))
    (cc-dbg-when clasp-cleavir:*debug-log*
		 (format clasp-cleavir:*debug-log* "Landing-pads: ~a~%" (length landing-pads))
		 (dolist (elp landing-pads)
		   (format clasp-cleavir:*debug-log* "Landing-pad in ~a~%" (cc-mir:describe-mir (car elp)))))
    (cleavir-ir:map-instructions-with-owner
     (lambda (instruction owner)
       (cond
	 ((typep instruction 'cleavir-ir:funcall-instruction)
	  (let ((landing-pad (second (assoc owner landing-pads))))
	    (unless (null landing-pad)
	      (cc-dbg-when *debug-log*
			   (format *debug-log* "Translating call to invoke: ~a~%" 
				   (cc-mir:describe-mir instruction)))
	      (change-class instruction 'invoke-instruction
			    :landing-pad landing-pad))))
	 ((typep instruction 'cleavir-ir:multiple-value-call-instruction)
	  (let ((landing-pad (second (assoc owner landing-pads))))
	    (unless (null landing-pad)
	      (cc-dbg-when *debug-log*
			   (format *debug-log* "Translating multiple-value-call to invoke-multiple-value-call: ~a~%" 
				   (cc-mir:describe-mir instruction)))
	      (change-class instruction 'invoke-multiple-value-call-instruction
			    :landing-pad landing-pad))))
	 (t nil)))
     initial-instruction)
    ;; insert landing pad after its enter instruction
    (dolist (lp landing-pads)
      (let ((enter (car lp))
	    (landing-pad (second lp))
	    (frame-holder (third lp)))
	(change-class enter 'clasp-cleavir-hir:landing-pad-named-enter-instruction
		      :landing-pad landing-pad)
	(setf (clasp-cleavir-hir:frame-holder enter) frame-holder)))
    ;;
    ;; Now change all of the unwind-instructions to indexed-unwind-instructions
    ;;
    (dolist (unwind unwinds)
      (let* ((enter (cleavir-ir:invocation unwind))
	     (landing-pad (second (assoc enter landing-pads))))
	(change-class unwind 'clasp-cleavir-hir:indexed-unwind-instruction
		      :inputs (list (clasp-cleavir-hir:frame-holder enter)))))))



(defun finalize-unwind-and-landing-pad-instructions (initial-instruction)
  ;; Identify all UNWIND-INSTRUCTIONs and LANDING-PAD-INSTRUCTIONs
  ;; under the initial-instruction
  ;; change all return-instruction(s) that are owned by
  ;; landing-pad-named-enter-instruction(s)
  ;; to landing-pad-return-instruction(s)
  (let (unwinds landing-pad-enters landing-pad-returns)
    (cleavir-ir:map-instructions-with-owner
     (lambda (instruction owner)
       (cond
	 ((typep instruction 'cleavir-ir:unwind-instruction)
	  (push instruction unwinds))
	 ((typep instruction 'clasp-cleavir-hir:landing-pad-named-enter-instruction)
	  (push instruction landing-pad-enters))
     	 ((and (typep instruction 'cleavir-ir:return-instruction)
	       (typep owner 'clasp-cleavir-hir:landing-pad-named-enter-instruction))
	  (change-class instruction
			'clasp-cleavir-hir:landing-pad-return-instruction
			:inputs (append (cleavir-ir:inputs instruction) (list (clasp-cleavir-hir:frame-holder owner)))
			:landing-pad (clasp-cleavir-hir:landing-pad owner)))))
     initial-instruction)
    ;; add each unwind to its corresponding landing-pad
    (dolist (unwind unwinds)
      (let* ((enter (cleavir-ir:invocation unwind))
	     (landing-pad (clasp-cleavir-hir:landing-pad enter)))
	(push unwind (unwinds landing-pad))))
    ;; Set the jump-id of each unwind according to where it is in its landing-pad unwind list
    (dolist (unwind unwinds)
      (let* ((enter (cleavir-ir:invocation unwind))
	     (landing-pad (clasp-cleavir-hir:landing-pad enter))
	     (jump-id (position unwind (unwinds landing-pad))))
	(setf (clasp-cleavir-hir:jump-id unwind) jump-id)))))
