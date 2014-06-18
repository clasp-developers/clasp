;; http://sourceforge.net/p/ecls/bugs/262

(declaim (ftype (function (cons)   t)       foo))
(declaim (ftype (function (t cons) t) (setf foo)))

(defun foo (cons)
  (first cons))

(defun (setf foo) (value cons)
  (setf (first cons) value))

(defvar *c* (cons 'x 'y))

(foo *c*) ;; correctly returns 'x

;; signals an error:
;; Z is not of type CONS.
;;   [Condition of type TYPE-ERROR]
(deftest sf262--declaim-type-foo-setf-foo.lsp
         (assert (eq 'z
                     (setf (foo *c*) 'z))))
