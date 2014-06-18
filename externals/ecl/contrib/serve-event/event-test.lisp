
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'serve-event)
(in-package "SERVE-EVENT")

(defun test-stdin ()
  (format t "DOING STDIN~%")
  (with-fd-handler (0 :input #'(lambda (fd) (declare (ignore fd))
                                       (format t "Got data~%")
                                       (read-char)))
    (loop ;; FIXME: End condition
       (format t "Entering serve-all-events...~%")(force-output)
       (serve-all-events 5)
       (format t "Events served~%"))))

(test-stdin)
