
(defun fibn (reps num &aux (rnum 0) (p1 0) (p2 0) (z 0))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type fixnum reps num rnum p1 p2 z))
  (dotimes (r reps)
    (setq p1 1
          p2 1
          rnum (- num 2))
    (dotimes (i rnum)
      (setq z (+ p1 p2)
            p2 p1
            p1 z)))
  z)
