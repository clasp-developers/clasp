(in-package #:sys)

(defun %write-object (object stream)
  (incless:write-object incless-intrinsic:*client* object stream))

(defun %circle-check (object)
  (incless:circle-check incless-intrinsic:*client* object nil))

(defmethod print-object (object stream)
  (write-ugly-object object stream))

(defmethod incless:printing-char-p ((client incless-intrinsic:client) char)
  (printing-char-p char))

(define-compiler-macro assert-failure (&whole form &rest args)
  (declare (ignore args))
  (invistra:expand-function incless-intrinsic:*client* form 4))

(define-compiler-macro simple-program-error (&whole form &rest args)
  (declare (ignore args))
  (invistra:expand-function incless-intrinsic:*client* form 1))

(define-compiler-macro simple-reader-error (&whole form &rest args)
  (declare (ignore args))
  (invistra:expand-function incless-intrinsic:*client* form 2))

(define-compiler-macro signal-simple-error (&whole form &rest args)
  (declare (ignore args))
  (invistra:expand-function incless-intrinsic:*client* form 2 3))

(in-package #:mp)

(define-compiler-macro interrupt (&whole form &rest args)
  (declare (ignore args))
  (invistra:expand-function incless-intrinsic:*client* form 3))

(define-compiler-macro raise (&whole form &rest args)
  (declare (ignore args))
  (invistra:expand-function incless-intrinsic:*client* form 2))
