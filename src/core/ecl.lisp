(multiple-value-bind (x y) #.(values 1 2) (print (list x y)))


(mapcar #'list '(1 2) '(3 4))

(if (values nil 1)
    (values 1 2)
)


    (print "f"))


(let (l)
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
  (print l)
  (equal l '(3 2 1)))

   
