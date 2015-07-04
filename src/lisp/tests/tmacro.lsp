(eval-when (:compile-toplevel :load-toplevel)
  (defmacro foo (&rest args)
    (print 'expand)
    10)
  (defun bar ()
    (foo ((x y)))))

