
(defmacro setf-gethash (ht key val)
  #+cando `(hash-table-setf-gethash ,ht ,key ,val)
  #-cando `(setf (gethash ,key ,ht) ,val)
  )

(defparameter *ht*
	 '#.(let ((ht (make-hash-table :test #'eql))
		  (ar (make-array '(2 3))))
	      (dotimes (i (array-total-size ar))
		(setf-row-major-aref ar i i))
	      (setf-gethash ht 'a '(100 101 102))
	      (setf-gethash ht 'b 2)
	      (setf-gethash ht 'c ar)
	      (setf-gethash ht 'd 4)
	      (setf-gethash ht 'e 5)
	      ht))
(format t "*ht* = ~a~%" *ht*)
(maphash #'(lambda (k v) (print (list k v))) *ht*)
				     
