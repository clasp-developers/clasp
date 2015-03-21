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
(defclass landing-pad ()
  ((%targets :initarg :targets :reader targets)
   (%id :initarg :id :accessor landing-pad-id)
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
     for targets = (when remaining
		     (loop for cu in current-unwinds
			collect (first (cleavir-ir:successors cu))))
     until (null remaining)
     collect (cons enter (make-instance 'landing-pad :targets targets))
     do (setf remaining
	      (set-difference remaining current-unwinds))))



;; From before
#+(or)(defun compute-landing-pads (unwinds)
	(when unwinds
	  (loop with remaining = unwinds
	     until (null remaining)
	     for enter = (cleavir-ir:invocation (first remaining))
	     for current-unwinds = (remove enter remaining
					   :key #'cleavir-ir:invocation
					   :test-not #'eq)
	     for targets = (loop for cu in current-unwinds
			      collect (first (cleavir-ir:successors cu)))
	     collect (cons enter (make-instance 'landing-pad :targets targets))
	     do (setf remaining
		      (set-difference remaining current-unwinds)))))

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



(defvar *landing-pad-id* 98000000)
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
	  (let ((landing-pad (cdr (assoc owner landing-pads))))
	    (unless (null landing-pad)
	      (cc-dbg-when *debug-log*
			   (format *debug-log* "Translating call to invoke: ~a~%" 
				   (cc-mir:describe-mir instruction)))
	      (change-class instruction 'invoke-instruction
			    :landing-pad landing-pad))))
	 ((typep instruction 'cleavir-ir:multiple-value-call-instruction)
	  (let ((landing-pad (cdr (assoc owner landing-pads))))
	    (unless (null landing-pad)
	      (cc-dbg-when *debug-log*
			   (format *debug-log* "Translating multiple-value-call to invoke-multiple-value-call: ~a~%" 
				   (cc-mir:describe-mir instruction)))
	      (change-class instruction 'invoke-multiple-value-call-instruction
			    :landing-pad landing-pad))))
	 (t nil)))
     initial-instruction)
    ;; associate each landing pad with a landing-pad-id
    ;; and associate each landing pad with its enter instruction
    (let ((landing-pad-id *landing-pad-id*))
      (dolist (lp landing-pads)
	(let ((enter (car lp))
	      (landing-pad (cdr lp)))
	  (setf (landing-pad-id landing-pad) landing-pad-id)
	  (setf (cc-mir:landing-pad enter) landing-pad)
	  (incf landing-pad-id)))
      (setq *landing-pad-id* landing-pad-id))
    ;;
    ;; Now change all of the unwind-instructions to indexed-unwind-instructions
    ;;
    (dolist (unwind unwinds)
      (let* ((unwind-enter (cleavir-ir:invocation unwind))
	     (landing-pad (cdr (assoc unwind-enter landing-pads)))
	     (landing-pad-id (landing-pad-id landing-pad))
	     (jump-id (position (first (cleavir-ir:successors unwind)) (clasp-cleavir:targets landing-pad))))
	(change-class unwind 'cc-mir:indexed-unwind-instruction
		      :landing-pad-id landing-pad-id
		      :jump-id jump-id)))
    ))
