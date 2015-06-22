
(eval-when (:compile-toplevel :load-toplevel)
  (macrolet ((pow2 (x) (let ((tx (gensym))) `(let ((,tx ,x)) (* ,tx ,tx)))))
    (format *trace-output* "Evaluating (pow2 4) --> ~a~%" (pow2 4))))

