
(defun c (m n o p q r s t) (error "test"))

(defun b (a b c d e) (c 10 20 30 a b c d e))

(defun a (x y) (b 1 2 x y 3))


