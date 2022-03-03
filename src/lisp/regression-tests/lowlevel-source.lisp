
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass foo () ())
  (defun make-foo () (make-instance 'foo))
  (defmethod make-load-form ((o foo) &optional env)
    (declare (ignore env))
    (values '(let () (make-foo)) '(let ()))))

(defun foo () #.(make-foo))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass bar () ())
  (defun make-bar () (make-instance 'bar))
  (defmethod make-load-form ((o bar) &optional env)
    (declare (ignore env))
    '(let () (make-bar))))

(defun bar () #.(make-bar))

