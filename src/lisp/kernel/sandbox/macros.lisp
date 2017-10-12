(defmacro defun (name lambda-list &body body)
  `(progn
     (setf (fdefinition ',name)
           (lambda ,lambda-list ,@body))
     ',name))
