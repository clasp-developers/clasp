

(defun foo ()
  (tagbody
   a
     (print "Hello")
     (funcall (lambda () (go b)))
     (print "Skip me")
   b
     (print "Leaving")
     ))
