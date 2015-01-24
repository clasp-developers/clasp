(defun a (x y) (+ x y))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (format t "Loading t.lsp\n"))

