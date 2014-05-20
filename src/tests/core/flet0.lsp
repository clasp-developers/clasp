(flet
    ((one (x) (return-from one (+ 1 x)) (print "dont reach")))
  (print (one 10)))
