
(defun compute-factorial (n)
  (if (<= n 1)
      1
      (let ((f n))
        (tagbody
         top
           (setq n (1- n))
           (when (<= n 1)
             (go done))
           (setq f (* f n))
           (go top)
         done)
        f)))



                 
                             
             
