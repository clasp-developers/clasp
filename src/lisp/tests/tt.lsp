
(defun foo (f x y z l)
  (core:multiple-value-foreign-call "apply_method3" x y z l))
