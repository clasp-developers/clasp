

;;
;; Calculate the longest Hailstone sequence
;;


(defun hailstone (max)
  (let ((start-time (get-universal-time))
	(longest 0)
	(terms 0))
    (do* ((i 1 (+ 1 i))
	  (j i i)
	  (this-terms 1 1))
	 ((>= i max) ())
      (do ()
	  ((= j 1) ())
	(setq this-terms (+ 1 this-terms))
	(setq j (if (= (mod j 2) 0)
		    (/ j 2)
		    (+ (* 3 j) 1))))
      (when (> this-terms terms)
	(setq terms this-terms)
	(setq longest i)))
    (bformat t "Longest hailstone sequence %d (terms=%d)\n" longest terms)
    (let* ((end-time (get-universal-time))
	   (duration (- end-time start-time)))
      (bformat t "Searched %d terms - required %d seconds\n" max duration))))

(bformat t "-------- Iterated --------\n")
;;(hailstone 200)

#|
(co:compile 'hailstone)

(bformat t "-------- Compiled ---------\n")

(hailstone 200)

(hailstone 1000)

(hailstone 10000)
|#
