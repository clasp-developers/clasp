(defmethod fgf-foo ((x integer)) :integer)
(defmethod fgf-foo ((x string)) :string)
(test dispatch-integer (fgf-foo 1) (:integer))
(test dispatch-string (fgf-foo "testing") (:string))
(defmethod fgf-foo ((x symbol)) :symbol)
(test dispatch-symbol (fgf-foo :yadda) (:symbol))
(test-expect-error dispatch-no-applicable-method (fgf-foo 1.2) :description "This should not dispatch")

;;; Enough methods that the discriminator needs long instructions (#1731).
(test-true dispatch-many-methods
           (let ((classes (loop for i below 300
                                collect (make-symbol (format nil "FGF-MANY-~d" i)))))
             (dolist (c classes) (eval `(defclass ,c () ())))
             (eval '(defgeneric fgf-many (x)))
             (loop for c in classes for i from 0
                   do (eval `(defmethod fgf-many ((x ,c)) ,i)))
             (loop for c in classes for i from 0
                   always (eql i (funcall 'fgf-many (make-instance c)))))
           :description "Dispatch past 255 methods")
