
(defun inner (x)
  (throw 'test))


(defun outer (a)
  (let ((x (inner a)))))


(catch 'test (outer 10))
