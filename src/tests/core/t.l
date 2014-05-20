(load "features.l")


(let ((a 0))
  (setq a (locally
	    (print (list "a = " a))
	    1
	    (print (list "a = " a))
	    2
	    (print (list "a = " a))
	    3))
  (print (list " final a = " a)))
