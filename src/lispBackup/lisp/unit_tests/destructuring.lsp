




(test (destructuring-bind ( a b ) (list 100 200 )
	(and (equal a 100) (equal b 200))))
(test (destructuring-bind ( ( a b ) ) (list (list 100 200 ))
	(and (equal a 100) (equal b 200))))
(test (destructuring-bind ( &key ( (:x (aa (bb cc))) (1 2)) ) (list :x (list 100 (list 150 200)) )
	(and (equal aa 100) (equal bb 150) (equal cc 200))))




