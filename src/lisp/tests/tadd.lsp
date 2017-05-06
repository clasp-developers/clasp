
(defun foo ()
  (let ((x 1))
    (flet ((bar () 2))
      (let ((z 3))
        (let* ((w 4))
          z)))))
