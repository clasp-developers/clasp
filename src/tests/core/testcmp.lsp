
(defun a (x y) (+ x y))

(print (a 1 2))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (make-package :yoda))

(print 'yoda::b)
