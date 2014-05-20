(block nil
  (let ((a 1))
    (tagbody
     a
       (print "a")
       (apply #'(lambda () (return-from nil 'nil) nil))
       )
    )
  )
