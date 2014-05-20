
(defvar *secret* 999)
(defun foo (x &key key-arg)
  (declare (special *secret*))
  (values (+ *secret* x) *secret*))

(defun bar (*secret* a)
  (multiple-value-list (foo a :key-arg t)))

(test-equal (bar 2 3) '(5 2) "defun-declare-special")

