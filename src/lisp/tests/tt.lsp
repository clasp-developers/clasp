

(defun bar ()
  (throw 'ttt nil))

(defun foo ()
  (catch 'ttt
    (bar)))

(foo)

