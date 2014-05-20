(defun xxx (&key (a 1) (b 2))
  (bformat t "a=%d  b=%d\n" a b))



#|

(let ((b 1))
  (tagbody
   top
     (let ((a 1))
       (setq b (+ b a))
       (print b)
       (error "Stuff")
       (if (< b 10)
	   (go top)
	   (go done))
       (print "Never get here"))
   done
     (print "Hello"))
  )
|#
