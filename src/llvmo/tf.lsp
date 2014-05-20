
(defun testfn (x)
  (print x))

(bformat t "testfn type = %s\n" (class-name (symbol-function 'testfn)))
(bformat t "Compiling testfn\n")
(co:compile 'testfn)
(bformat t "After compiling testfn\n")
(bformat t "testfn type = %s\n" (class-name (symbol-function 'testfn)))

