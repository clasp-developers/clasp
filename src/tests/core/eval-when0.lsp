(print "start")
(eval-when (:compile-toplevel :load-toplevel)
  (print "eval-when body"))
(print "end")
