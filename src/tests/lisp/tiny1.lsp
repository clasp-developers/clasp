(defun cleavir-clasp-test (x mult) 
  (let ((total 0) 
	(count 0))
    (tagbody 
     top
       (setq total (+ total x))
       (setq count (1+ count))
       (if (< count mult)
	   (go top))
       )
    total))



