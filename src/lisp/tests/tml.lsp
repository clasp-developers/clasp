(eval-when (:load-toplevel :compile-toplevel :execute)
  (macrolet
      ((defdef (def* def)
         `(format t "def* = ~a   def = ~a~%" ',def* ',def )))
    (defdef defgeneric* defgeneric)
    (defdef defun* defun)))

