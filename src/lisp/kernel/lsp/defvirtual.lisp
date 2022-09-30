(in-package :core)

(defmacro defvirtual (name lambda-list &body body)
    (multiple-value-bind (simple-lambda-list dispatch-symbol dispatch-class dispatch-index)
        (core:process-single-dispatch-lambda-list lambda-list)
      (declare (ignore dispatch-index))
      (multiple-value-bind (declares code docstring specials)
          (core:process-declarations body t)
        (declare (ignore specials))
        (let ((fn `(lambda ,simple-lambda-list
                     ;; as with cl:defmethod, anything specialized is
                     ;; implicitly declared ignorable.
                     (declare (ignorable ,dispatch-symbol))
                     (declare ,@declares)
                     ,@(when docstring (list docstring))
                     ,@code))
              ;; number of required arguments.
              (nreq (first (core:process-lambda-list simple-lambda-list
                                                     'function))))
          `(core:ensure-single-dispatch-method (fdefinition ',name)
                                               ',name
                                               (find-class ',dispatch-class)
                                               :nreq ,nreq
                                               :docstring ,docstring
                                               :body ,fn)))))

(export '(defvirtual))
