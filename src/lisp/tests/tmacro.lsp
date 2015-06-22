
#+(or)
(defmacro deftparameter (&whole whole var form &optional doc-string)
  `(eval-when (:compile-toplevel)
     (print '("The values of var form doc-string --> |" ,var ,form ,doc-string "|"))
     (print '("The value of whole --> |" , whole  "|"))))


(defmacro defo (x &optional y)
  `(eval-when (:compile-toplevel)
     (print '("The values of x y --> |" ,x ,y "|"))))

(defo)
