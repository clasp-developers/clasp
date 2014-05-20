
(tagbody
   top
   (print "top")
   (tagbody
      (print "inner-top")
      (go done)
      (print "inner-bottom")
      )
   done
   (print "outer-bottom"))


#|
(tagbody
   (go c)
 a
   (tagbody
      aa
      (print "aa")
      (go b)
      bb
      (print "bb"))
   (go b)
 b
   (print "b")
   (go done)
 c
   (print "c")
   (go a)
 done
   (print "done"))
|#
