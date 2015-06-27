

(defun tadd (x y) (+ x y))

(defun num-add (num)
  (let ((x 0))
    (dotimes (i num)
      (setq x (+ x i)))
    (print x)))
