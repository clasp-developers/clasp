(in-package #:clasp-tests)

(test-true dolist-declare-elements
           (let ((did-warn nil))
             (flet ((catch-warnings (&optional condition)
                      (setq did-warn t)
                      (muffle-warning condition)))
               (handler-bind ((warning #'catch-warnings))
                 (compile nil
                          '(lambda()
                            (let ((sum 0))
                              (dolist (a (list 1 2 3) sum)
                                (declare (type fixnum a))
                                (incf sum a)))))))
             (not did-warn)))


(test-true dolist-var-nil-at-return
           (let ((sum 0))
             (null
              (dolist (a (list 1 2 3) a)
                (incf sum a)))))
