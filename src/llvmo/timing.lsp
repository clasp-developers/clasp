
(let ((a 1)
      (start-time (get-universal-time)))
  (dotimes (i 10)
    (dotimes (j 100)
      (setq a (+ 1 a))))
  (let* ((stop-time (get-universal-time))
	 (duration (- stop-time start-time)))
    (bformat t "Iterated: %d iterations took %d seconds" a duration)))



