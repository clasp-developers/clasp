(in-package :clasp-cleavir)

;;; Define the CLASP system
(defclass clasp () ())

(defvar *clasp-system* (make-instance 'clasp))


#+(or)(defmacro cc-dbg-when (cond &rest body) nil)
(defmacro cc-dbg-when (cond &rest body)
  `(when (and ,cond *debug-log-on*)
     ,@body))

(defvar *debug-log* nil)
(defvar *debug-log-on* nil)
(defvar *debug-instruction-function* nil)
(defvar *debug-log-index* 0)
(defvar *debug-basic-blocks*)
(defvar *debug-ownerships*)
(defvar *debug-tags*)
(defvar *debug-vars*)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stealth mixins
;;;
;;;
(defvar *stealth-mixins* (make-hash-table))

(defmacro class-stealth-mixins (class)
  `(gethash ,class *stealth-mixins*))

(defun class-equalp (c1 c2)
  (when (symbolp c1) (setf c1 (find-class c1)))
  (when (symbolp c2) (setf c2 (find-class c2)))
  (eq c1 c2))

(defmacro define-stealth-mixin (name super-classes victim-class &rest for-defclass)
  `(progn
     ;; First define the class we talk about
     (defclass ,name ,super-classes ,@for-defclass)
     (clos::ensure-class
      ',victim-class
       :direct-superclasses
       (adjoin ',name
		(and (find-class ',victim-class nil) (clos::class-direct-superclasses
						      (find-class ',victim-class)))
		:test #'class-equalp))
     ;; Register it as a new mixin for the victim class
     (pushnew ',name (class-stealth-mixins ',victim-class))
     (defmethod clos::ensure-class-using-class :around 
	 (class (name (eql ',victim-class))
		&rest arguments
		&key (direct-superclasses nil direct-superclasses-p) &allow-other-keys)
	 (dolist (k (class-stealth-mixins name)) 
	   (pushnew k direct-superclasses
		    :test #'class-equalp)) 
	 (apply #'call-next-method class name
		  :direct-superclasses direct-superclasses
		  arguments))
     ',name))



(define-stealth-mixin secret-instruction-id () cleavir-ir:instruction
		      ((%gid :initform 0 :accessor instruction-gid)))

(define-stealth-mixin secret-datum-id () cleavir-ir:datum
		      ((%gid :initform 0 :accessor datum-gid)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add labels to graphviz drawings
;;;

(defmethod cleavir-ir-graphviz::label :around (instr)
  (with-output-to-string (s)
    (format s "~a:~a" (call-next-method) (instruction-gid instr))))

(defmethod cleavir-ir-graphviz::name :around (datum)
  (with-output-to-string (s)
    (format s "~a:~a" (call-next-method) (datum-gid datum))))

