(defmethod fgf-foo ((x integer)) :integer)
(defmethod fgf-foo ((x string)) :string)
(test dispatch-integer (fgf-foo 1) (:integer))
(test dispatch-string (fgf-foo "testing") (:string))
(defmethod fgf-foo ((x symbol)) :symbol)
(test dispatch-symbol (fgf-foo :yadda) (:symbol))
(test-expect-error dispatch-no-applicable-method (fgf-foo 1.2) :description "This should not dispatch")

(defmethod fgf-eql ((x (eql :alpha))) :alpha)
(defmethod fgf-eql ((x (eql :beta))) :beta)
(test dispatch-eql-alpha (fgf-eql :alpha) (:alpha))
(test dispatch-eql-beta (fgf-eql :beta) (:beta))
;;; Without memoization every eql-specialized call is a full dispatch miss.
(test-true dispatch-eql-memoized
           (progn (fgf-eql :alpha)
                  (plusp (length (clos::generic-function-call-history #'fgf-eql)))))
