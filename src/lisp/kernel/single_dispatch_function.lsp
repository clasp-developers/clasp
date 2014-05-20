
(defmacro def-sd-generic (name &optional (index-on-argument 0))
  (ensure-single-dispatch-generic-function name index-on-argument)
)

(defmacro def-sd-method (sd-gf-name raw-lambda-list &body body)
  (multiple-value-bind (lambda-list dispatch-symbol dispatch-class-symbol)
      (process-single-dispatch-lambda-list raw-lambda-list)
    (multiple-value-bind (declares code docs) (process-declarations body)
      (let* ((llh (make-lambda-list-handler lambda-list declares))
	     (dispatch-on-required-index (single-dispatch-on-argument llh dispatch-symbol))
	     (gf (let ((gf-find (find-single-dispatch-generic-function sd-gf-name)))
		   (if gf-find
		       (if (eql (single-dispatch-generic-function-dispatch-on-index gf-find) dispatch-on-required-index)
			   gf-find
			   (error (bformat nil "Mismatch between single-dispatch-generic function dispatch-argument-index[%d] and that defined in the sd-method: %s" (single-dispatch-generic-function-dispatch-on-index gf-find) raw-lambda-list)))
		       (ensure-single-dispatch-generic-function sd-gf-name dispatch-on-required-index)))))
	(ensure-single-dispatch-method sd-gf-name (find-class dispatch-class-symbol)
				       :lambda-list-handler llh
				       :declares declares
				       :docstring docs
				       :body code)))))

