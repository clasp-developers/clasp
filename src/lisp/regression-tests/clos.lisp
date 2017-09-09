
(defun foo (x) :nothing)
(test (null (core:generic-function-p (fdefinition 'foo))))
(defgeneric gf123bar (x))
(test (core:generic-function-p (fdefinition 'gf123bar)))
