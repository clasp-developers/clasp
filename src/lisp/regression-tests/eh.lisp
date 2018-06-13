


(defun foo1 ()
  (block x
    (unwind-protect nil
      (funcall (lambda () (return-from x))))))

