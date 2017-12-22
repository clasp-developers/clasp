
(defun foo (x) :nothing)
(test generic-function-p-negative (not (core:generic-function-p (fdefinition 'foo))))
(defgeneric gf123bar (x))
(test generic-function-p-positive (core:generic-function-p (fdefinition 'gf123bar)))
