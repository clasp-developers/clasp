
(test-equal
 (flet ((flet1 (n) (+ n n)))
   (flet ((flet1 (n) (+ 2 (flet1 n))))
     (flet1 2)))
  6 )

(defun dummy-function () 'top-level)
(funcall #'dummy-function)


(flet ((dummy-function () 'shadow))
  (funcall #'dummy-function))



(test (eq (funcall #'dummy-function) (funcall 'dummy-function)) )

(test (not
       (flet ((dummy-function () 'shadow))
	 (eq (funcall #'dummy-function)
	     (funcall 'dummy-function)))))

(defun recursive-times (k n)
  (labels ((temp (n) 
	     (if (zerop n) 0 (+ k (temp (1- n))))))
    (temp n)))

(test (eq (recursive-times 2 3) 6))

(defmacro mlets (x &environment env) 
  (let ((form `(babbit ,x)))
    (macroexpand form env)))

(test (eq (macrolet ((babbit (z) `(+ ,z ,z))) (mlets 5)) 10))

(test-float-eq (flet ((safesqrt (x) (sqrt (abs x))))
		 ;; The safesqrt function is used in two places.
		 (safesqrt (apply #'+ (map 'list #'safesqrt '(1 2 3 4 5 6)))))
	       3.291173 )


(defun integer-power (n k)     
  (declare (integer n))         
  (declare (type (integer 0 *) k))
  (labels ((expt0 (x k a)
	     (declare (integer x a) (type (integer 0 *) k))
	     (cond ((zerop k) a)
		   ((evenp k) (expt1 (* x x) (floor k 2) a))
		   (t (expt0 (* x x) (floor k 2) (* x a)))))
	   (expt1 (x k a)
	     (declare (integer x a) (type (integer 0 *) k))
	     (cond ((evenp k) (expt1 (* x x) (floor k 2) a))
		   (t (expt0 (* x x) (floor k 2) (* x a))))))
    (expt0 n k 1)))

(defun example (y l)
  (flet ((attach (x)
	   (setq l (append l (list x)))))
    (declare (inline attach))
    (dolist (x y)
      (unless (null (cdr x))
	(attach x)))
    l))

(test-equal
 (example '((a apple apricot) (b banana) (c cherry) (d) (e))
          '((1) (2) (3) (4 2) (5) (6 3 2)))
 '((1) (2) (3) (4 2) (5) (6 3 2) (A APPLE APRICOT) (B BANANA) (C CHERRY)))
