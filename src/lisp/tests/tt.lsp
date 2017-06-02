
(defun bar ()
;;  (error "foo")
  (throw 'catchme 'test))

(defun baz ()
  (bar))

(defun foo ()
  (catch 'catchme
    (baz)))

(defun zippy ()
  (block hello
    (funcall (lambda () (return-from hello nil)))))
