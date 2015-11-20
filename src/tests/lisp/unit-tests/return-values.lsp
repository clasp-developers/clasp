
(defun return-from-over-apply ()
  (declare (core:lambda-name return-from-over-apply))
  (block foo 
    (apply (lambda () 
	     (declare (core:lambda-name inner)) 
	     (return-from foo 'expected-result)) nil)))

(test (eq (return-from-over-apply) 'expected-result))
