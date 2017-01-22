(defun fibn (reps num)
  (declare (optimize speed (safety 0) (debug 0))
           (fixnum reps num))
  (let ((z 0))
    (declare (fixnum z))
    (dotimes (r reps)
      (declare (fixnum r))
      (let ((p1 1)
            (p2 1))
        (declare (fixnum p1 p2))
        (dotimes (i (- num 2))
          (declare (fixnum i))
          (setf z (+ p1 p2)
                p2 p1
                p1 z))))
    z))

#|


(clasp-cleavir:cleavir-compile
 'fibn
 '(lambda (reps num)
   (declare (optimize speed (safety 0) (debug 0)))
   (let ((z 0))
     (declare (fixnum reps num z))
     (dotimes (r reps)
       (declare (fixnum r))
       (let* ((p1 1)
              (p2 1))
         (declare (fixnum p1 p2))
         (dotimes (i (- num 2))
           (declare (fixnum i))
           (setf z (+ p1 p2)
                 p2 p1
                 p1 z))))
     z))
 :debug t)
|#
