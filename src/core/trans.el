

(defun trans ()
  (interactive)
  (let ((cmd (concat "./mtr " (buffer-file-name))))
    (shell-command cmd 1)))
