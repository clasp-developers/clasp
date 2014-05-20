
(define-condition core::too-few-arguments-error (error) ())

(define-condition core::too-many-arguments-error (error) ())



(core:iload 'lsp/ffi)
(setq core:*print-repl-read* t)

(load "../serveEvent/serve-event.lisp")
(use-package :serve-event)

(trace add-fd-handler)

(defun test-stdin ()
  (format t "DOING STDIN~%")
  (with-fd-handler (0 :input #'(lambda (fd) (declare (ignore fd))
				       (format t "Got data ~A~%" (read-char)))
		      )
    (loop ;; FIXME: End condition
       (format t "Entering serve-all-events...~%") (force-output)
       (serve-all-events 5)
       (format t "Events served~%"))))

(print (macroexpand-1 '  (with-fd-handler (0 :input #'(lambda (fd) (declare (ignore fd))
				       (format t "Got data~%")
				       (read-char)))
    (loop ;; FIXME: End condition
       (format t "Entering serve-all-events...~%") (force-output)
       (serve-all-events 5)
       (format t "Events served~%")))))
