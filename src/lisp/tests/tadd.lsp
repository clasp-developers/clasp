
(defun foo-add (x y) (+ x y))





(defun foo-sub (x y) (- x y))




(defun c ()
  (error "foo"))



(defun b ()
  (c))




(defun a ()
  (b))

