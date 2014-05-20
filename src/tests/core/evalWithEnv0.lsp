
(defun ev (form)
  (eval-with-env form nil nil t t))
