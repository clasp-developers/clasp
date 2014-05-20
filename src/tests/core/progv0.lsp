(test
 (equal (let ((*x* 100))
	  (progv '(*x*) '(4) (list *x* (symbol-value '*x*))))
	'(100 4)))


    
