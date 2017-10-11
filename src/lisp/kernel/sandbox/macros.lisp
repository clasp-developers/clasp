;;; definition works anywhere, probably- not implementation specific
(defmacro multiple-value-call (fdesignator &rest args)
  `(cleavir-primop:multiple-value-call (coerce-fdesignator fdesignator) ,@args))

(defmacro defun (name lambda-list &body body)
  `(progn
     (setf (fdefinition ',name)
           (lambda ,lambda-list ,@body))
     ',name))
