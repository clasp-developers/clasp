(defun cmp-two ()
  (mp:process-run-function 'cmpa (lambda () (compile-file "sys:tests;ta.lsp" :print t)))
  (mp:process-run-function 'cmpb (lambda () (compile-file "sys:tests;tb.lsp" :print t))))

(defvar *mutex-print-mutex* (mp:make-lock :name 'print :recursive t))
(defun mutex-print (fmt &rest args)
  (unwind-protect
       (progn
         #++(mp:get-lock *mutex-print-mutex*)
         (apply #'format t fmt args))
    (progn
      #++(mp:giveup-lock *mutex-print-mutex*))))

(defun foo (n)
  (loop for i from 0 to n
     do (mutex-print "Process ~a   step ~a~%" mp:*current-process* i)))

(defun print-two (n)
  (mp:process-run-function 'a (lambda () (foo n)))
  (mp:process-run-function 'b (lambda () (foo n))))
 

