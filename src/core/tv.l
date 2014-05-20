

(multiple-value-bind (a b c) (values 1 2 3)
		      (print (% "a[%d] b[%d] c[%d]" a b c )))


(defparameter *a* (list 1 2 (values 10 11 12) 4))
(print *a*)

(let (a b)
  (setq (values *a* a b) (values 0 1 2 ))
  (multiple-value-bind (*a* x) (values -1 -2)
    (print (% "multiple-value-bind *a*=%d x=%d" *a* x)))
  (print (% "after setq *a*=%d a=%d b=%d" *a* a b))
  (multiple-value-setq (a b) (values -10 -20))
  (print (% "after multiple-value-setq a=%d b=%d" a b))
		       )