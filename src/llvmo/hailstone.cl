;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defun hailstone (max)
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (let ((start-time (get-universal-time))
	(longest 0)
	(terms 0))
    (do* ((i 113370 (+ 1 i))
	  (j i i)
	  (this-terms 1 1))
	 ((>= i max) ())
      (do ()
	  ((= j 1) ())
	(setq this-terms (+ 1 this-terms))
	(setq j (if (= (mod j 2) 0)
		    (/ j 2)
		    (+ (* 3 j) 1)))
	(format t "j=~a~%" j )
	)
      (when (> this-terms terms)
	(setq terms this-terms)
	(setq longest i))
	(format t "Just checked starting integer ~a~%" i))
#|      (when (eql (mod i 1 ) 0)
	(format t "Just checked starting integer ~a~%" i))
      )
|#
    (format t "Longest hailstone sequence ~a(terms=~a)~%" longest terms)
    (let* ((end-time (get-universal-time))
	   (duration (- end-time start-time)))
      (format t "Searched ~a terms - required ~a seconds~%" max duration))))


#+cando (progn
	  (compile 'hailstone)
	  (hailstone 500000))


#|

(hailstone 500000)

|#
