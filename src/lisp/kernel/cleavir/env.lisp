(in-package :clasp-cleavir-env)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UNWIND-PROTECT

;;; This class is used to augment an environment with a block
;;; introduced by BLOCK.  Client code can supply an IDENTITY object
;;; that is used to distinguish between different blocks with the same
;;; name.
(defclass unwind-protect (cleavir-environment:entry)
  ((%unwind-protect-ast :initarg :unwind-protect-ast :accessor unwind-protect-ast)))

(defgeneric add-unwind-protect (environment ast))

(defmethod add-unwind-protect (environment ast)
  (make-instance 'unwind-protect
		 :next environment
		 :unwind-protect-ast ast))

(defmethod print-object ((object unwind-protect) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "unwind-protect")))




(defgeneric unwind-protect-info (environment))

(defmethod unwind-protect-info ((environment unwind-protect))
  environment)

(defmethod unwind-protect-info ((environment cleavir-environment:entry))
  (unwind-protect-info (cleavir-environment:next environment)))

(defmethod unwind-protect-info (environment) nil)



