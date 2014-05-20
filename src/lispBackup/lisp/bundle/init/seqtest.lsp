(defun delete-list (which sequence start end count test test-not key)
  (with-tests (test test-not key)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (with-start-end (start end sequence)
      (with-count (%count count :output sequence)
	(let* ((splice (cons nil sequence))
               (output splice)
	       (index 0))
	  (declare (fixnum index)
		   (cons splice))
	  (while (and sequence (< index start))
	    (setf sequence (cdr (truly-the cons sequence))
		  splice (cdr (truly-the cons splice))
		  index (1+ index)))
	  (block nil
	    (tagbody
	     top
	       (unless (< index end)
		 (return))
	       (let ((elt (car (truly-the cons sequence))))
		 (setf sequence (cdr (truly-the cons sequence)))
		 (cond ((compare which (key elt))
			(setf (cdr splice) sequence)
			(when (zerop (decf %count))
			  (return)))
		       (t
			(setf splice (cdr splice))))
		 (incf index)
		 )
	       (go top)
	       ))
          (cdr output))))))

(defun delete (which sequence &key test test-not (start 0) end
               from-end count key)
  (declare (optimize (speed 3) (safety 1) (debug 0))
           (ext:check-arguments-type nil))
  (cond ((listp sequence)
         (if from-end
             (let ((l (length sequence)))
               (nreverse
                (delete-list which (nreverse sequence)
                             (if end (- l end) 0) (- l start)
                             count test test-not key)))
             (delete-list which sequence start end count test test-not key)))
        ((not (vectorp sequence))
         (signal-type-error sequence 'sequence))
        ((array-has-fill-pointer-p (truly-the vector sequence))
         (multiple-value-bind (sequence l)
             (filter-vector which sequence sequence start end from-end count
                            test test-not key)
           (setf (fill-pointer (truly-the vector sequence)) l)
           sequence))
        (t
         (values (filter-vector which nil sequence start end from-end count
                                test test-not key)))))



(defun test-delete ()
  (delete 3 '(1 2 3 4 5 3 34 2 3 ) :test #'eql ))
