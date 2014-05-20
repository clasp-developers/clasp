(let ((a 0))
  (setq a (locally
	    (print (list "a = " a))
	    1
	    (print (list "a = " a))
	    2
	    (print (list "a = " a))
	    3))
  (print (list " final a = " a)))


(let ((colonp nil)
      (atsignp t)
      (args '( test )))
  (setq args (progn
	       1
	       (print (list "line 9 args: " args))
	       2
	       (print (list "line 11 args: " args))
	       3))
  (print (list "final args: " args))
  )


#|
(let ((colonp nil)
      (atsignp t)
      (args '( test )))
  (setq args (progn
	       (fmt-log "line 2177 args: " args)
	       (if atsignp
		   (progn
		     (fmt-log "line 2180 args: " args)
		     100))))
  (print (list "final args: " args))
  )
|#
