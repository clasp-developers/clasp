
(let ((start-time (get-universal-time))
      (a 0))
  (tagbody
   top
     (setq a (+ a 1))
     (if (< a 10000)
	 (go top))
   done
     )
  (let* ((stop-time (get-universal-time))
	 (duration (- stop-time start-time)))
    (bformat t "Iterated: %d iterations took %d seconds" a duration)))

      
