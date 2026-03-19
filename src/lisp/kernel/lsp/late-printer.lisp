(in-package #:sys)

(defmethod incless:printing-char-p ((client incless-intrinsic:client) char)
  (core:printing-char-p char))

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
