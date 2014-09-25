(eval-when (:load-toplevel :compile-toplevel :execute)
  (macrolet
      ((defdef (def* def)
         `(print (list ',def* ',def))))
    (defdef defun* defun)))
