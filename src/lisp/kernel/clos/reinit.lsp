(in-package "CLOS")

(defvar *reinitializers* (make-hash-table :test #'equal))

(defun ensure-reinitializer (keys)
  (or (gethash keys *reinitializers*)
      (setf (gethash keys *reinitializers*)
            (make-reinitializer keys))))

(defun make-reinitializer (keys)
  (make-instance 'standard-generic-function
    :lambda-list `(instance
                   &rest initargs
                   &key ,@(loop for key in keys
                                for varname = (gensym (symbol-name key))
                                collect `((,key ,varname))))
    :specializer-profile (vector t)))

(define-compiler-macro reinitialize-instance
    (&whole form instance &rest initargs &environment env)
  (multiple-value-bind (keys syms bindings validp)
      (static-gfs::extract initargs env)
    (if validp
        `(funcall (load-time-value (ensure-reinitializer ',keys))
                  ,@initargs)
        form)))
