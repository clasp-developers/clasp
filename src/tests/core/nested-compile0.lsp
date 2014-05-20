
(defvar *inner-compile-var* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (print "Starting")
  (compile 'a '(lambda (x) (+ 3 x) (setq *inner-compile-var* (+ 3 x)) ))
  (print (list "(a 10) -->" (a 10)))
  (print "Finishing")
  )
(test (eql 13 *inner-compile-var*))
