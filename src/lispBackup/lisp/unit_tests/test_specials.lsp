


(test (let (l)
	(tagbody
	   (let ((x 3))
	     (unwind-protect
		  (progn
		    (setq l (cons 1 l))
		    (if (numberp x) (go out)))
		 (print x)
		 (setq l (cons 2 l))))
	 out
	   (setq l (cons 3 l)))
	(print (list "l= " l))
	(equal l '(3 2 1)))
      )



(defun test-labels (x)
  (labels ((my-add (y) (+ y 30)))
    (my-add x)))

(test (equal (test-labels 10) 40))
