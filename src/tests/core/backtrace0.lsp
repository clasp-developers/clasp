
(defun c ()
  (backtrace))

(defun b () 
  (c))

(defun a ()
  (b))

(a)
