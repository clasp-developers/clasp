(setq *print-pretty* t)
(progn
  (format t "spacer " )
  (format t "form: ~a~%" '(si::fset 'pushnew #'(lambda (w e))
						 (let ((item (cadr w)))))))

