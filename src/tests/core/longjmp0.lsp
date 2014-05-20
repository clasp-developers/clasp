
(tagbody
   (go A)
   B
   (print "B")
   (go C)
   A
   (print "A")
   (funcall #'(lambda () (print "In A lambda") (go B)))
   C
   (print "C")
)
