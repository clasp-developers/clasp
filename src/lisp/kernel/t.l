

(defvar a '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
(dotimes (i (1+ (/ (length a) 4)))
  (let* ((start (* i 4))
	 (stop (* (1+ i) 4))
	 (end (if (> stop (length a)) nil stop)))
    (print (subseq a start end))))



(format nil "~A:~A" "Hi" "There")


(princ "Hi there")
