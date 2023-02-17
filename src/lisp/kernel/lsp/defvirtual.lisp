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
                   (declare (ignorable ,dispatch-symbol)
                            ,@declares)
                   ,@(when docstring (list docstring))
                   ,@code)))
        `(core:ensure-single-dispatch-method (fdefinition ',name)
                                             ',name
                                             (find-class ',dispatch-class)
                                             :docstring ,docstring
                                             :body ,fn)))))

(export '(defvirtual))
