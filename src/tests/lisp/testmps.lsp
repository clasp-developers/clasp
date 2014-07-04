
(defun a ()
  (dotimes (zz 100)
    (let ((a `(cl:destructuring-bind)))
      (dotimes (i 1000)
        (let ((j (make-array 100)))))
      (if (eq (car a) 'cl:destructuring-bind)
          (print "Same")
          (print "Different")))))
