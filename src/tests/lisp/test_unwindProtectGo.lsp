
(make-test (:name :unwind-protect-go)
	   (let (r) 
	     (tagbody 
	      a 
		(push :a r) 
		(unwind-protect 
		     (progn (penv) (go b) (push :skip r))
		  (push :up1 r)
		  (push :up2 r)) 
	      b 
		(push :b r))
	     (unless (equal r '(:b :up2 :up1 :a))
	       (error "fail"))))

