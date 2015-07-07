;; cc_precalcValue
;; loadTimeValueReference
;; cc_precalcSymbol

(defun fibn (reps num &aux rnum p1 p2 z)
  (dotimes (r reps)
    (setq p1 1
          p2 1
          rnum (- num 2))
    (dotimes (i rnum)
      (setq z (+ p1 p2)
            p2 p1
            p1 z)))
  z)
