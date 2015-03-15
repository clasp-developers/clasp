

(defparameter *a* (block test
		    (bformat t "test-a\n")
		    (return-from test :b)
		    (print "b\n")))
(bformat t "*a* = %s\n" *a*)


(defparameter *b* (block test2
		    (bformat t "test2-a\n")
		    :b))
(bformat t "*b* = %s\n" *b*)


(defparameter *c* (block test3
		    (bformat t "test3-a\n")
		    (funcall (function (lambda () (return-from test3 :zzz))))
		    :c))
(bformat t "*c* = %s\n" *c*)
