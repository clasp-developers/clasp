
(defparameter ht (make-hash-table))
(core::hash-table-setf-gethash ht 'a 100)
(core::hash-table-setf-gethash ht 'b 200)

(print (gethash 'a ht))
