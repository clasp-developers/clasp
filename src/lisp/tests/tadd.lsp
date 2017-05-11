

(defun foo (&rest x)
  (multiple-value-call #'list (values-list x)))
;;1 2 3 4 5 6 7 8))) ;; (values-list x)))

