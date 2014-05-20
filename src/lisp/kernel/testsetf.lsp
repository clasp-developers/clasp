

(in-package :core)
(defmacro declare (x) ())

(defparameter a '(1 2 3))

(print (macroexpand-1 '(setf (cadr a) 100)))
(setf (cadr a) 100)
