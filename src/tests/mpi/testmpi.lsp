
(defun master ()
  (format t "Entered master~%")
  (dotimes (i (1- (mpi:size mpi:*world*)))
    (multiple-value-bind (data source tag)
	(mpi:recv mpi:*world* mpi:+any-source+ mpi:+any-tag+)
      (format t "MASTER Received ~a from source[~a] tag[~a]~%" data source tag)))
  (format t "Leaving master~%")
  )


(defun worker ()
  (format t "Entered worker~%")
  (mpi:send mpi:*world* 0 0 (list "Worker" (mpi:rank mpi:*world*) "reporting for duty")))


(format t "About to start master/worker for rank: ~a~%" (mpi:rank mpi:*world*))

(if (eql (mpi:rank mpi:*world*) 0)
    (master)
    (worker))
