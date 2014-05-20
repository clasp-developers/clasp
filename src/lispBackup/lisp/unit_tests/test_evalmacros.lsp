


(test (let (l (x 3))
	(print (list "start x=" x))
	(while (>= x 0)
	  (print (list "x=" x))
	  (setq l (cons x l))
	  (setq x (- x 1)))
	(print (list "l=" l))
	(equal '(0 1 2 3) l)))

(test (let (l (x 3))
	(core::until (< x 0)
		     (setq l (cons x l))
		     (setq x (- x 1)))
	(equal '(0 1 2 3) l)))


(test (let (l)
	(dotimes (k 5)
	  (core::case k
	    (1 (setq l (cons "value1" l)))
	    (3 (setq l (cons "value3" l)))
	    (otherwise ())))
	(equal l '( "value3" "value1" ))))
		     
