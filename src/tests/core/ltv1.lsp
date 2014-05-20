(defvar *foo-arrays* (list (make-array 7) (make-array 8)))
(defun foo1-ref (n) (aref (load-time-value (first *foo-arrays*) nil) n))
(defun set-foo1-ref (n val) (setf (aref (load-time-value (first *foo-arrays*) nil) n) val))



;;(defvar *foo-arrays* (list (make-array 7) (make-array 8)))
;;(defun foo1-ref (n) (aref (first *foo-arrays*) n))
;;(defun set-foo1-ref (n val) (setf (aref (first *foo-arrays*) n) val))
