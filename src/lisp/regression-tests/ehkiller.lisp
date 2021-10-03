(defun eh-foo ()
  (block x
    (block y
      (unwind-protect
          (return-from y 1)
        (funcall (lambda () (return-from x 2)))))))
(test eh-foo (eh-foo) (2))
      
(defun eh-bar ()
  (block x
      (unwind-protect
          nil
        (funcall (lambda () (return-from x 2))))))
(test eh-bar (eh-bar) (2))

(defun eh-baz ()
  (block x
    (unwind-protect
        (return-from x 1)
      (block z
        (funcall (lambda () (return-from z 2)))))))'

(defun eh-bab (a)
  (block x
    (funcall (lambda ()
               (block y
                 (when (eq a 'x)
                   (return-from x a))
                 (funcall (lambda ()
                            (when (eq a 'y)
                              (return-from y a))
                            (funcall (lambda () (return-from x a))))))))))


                              
(test eh-baz (eh-baz) (1))
