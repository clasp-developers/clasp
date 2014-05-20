(block nil
  (tagbody
     (let ((a 1))
       (print a)
       (gdb "test")
       (go done)
       )
   done
     (print "done")
     (return-from nil nil)))
