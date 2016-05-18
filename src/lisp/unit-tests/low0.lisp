
(test (apply #'list 1 2 '(3 4)) '(1 2 3 4))
(test (apply #'list '(3 4)) '(3 4))
(test (apply #'list 1 2 3 nil) '(1 2 3))

(defun test-apply0 (&va-rest rest) (apply #'list 1 2 rest))
(test (test-apply0 3 4) '(1 2 3 4))
(defun test-apply1 (&va-rest rest) (apply #'list rest))
(test (test-apply1 3 4) '(3 4))

