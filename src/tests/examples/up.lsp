
(defun a ()
  (unwind-protect
       (progn
	 (bformat t "normal\n")
	 nil)
    (bformat t "cleanup")))
