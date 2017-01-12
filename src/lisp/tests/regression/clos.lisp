
(defun foo (x) :nothing)
(test (null (core:generic-function-p (fdefinition 'foo))))
(defgeneric bar (x) :something)
(test (core:generic-function-p (fdefinition 'bar)))
