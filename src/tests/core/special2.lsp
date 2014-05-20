
(defvar *secret* 777)

(defun foo (x &key key-arg)
  (let (z)
    (values (+ *secret* x) *secret*)))

(defun bar (*secret* a)
  (multiple-value-list (foo a :key-arg t)))

(test-equal (bar 2 3) '(5 2) "defun-let-defvar-special")

