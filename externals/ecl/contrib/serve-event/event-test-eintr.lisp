;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test that serve-event can be interrupted
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'serve-event)

(defun serve-event-loop ()
  (serve-event:serve-all-events)
  (format t "serve-event-loop interrupted~%")
  (serve-event-loop))

(let ((thread
       (mp:process-run-function 'loop #'serve-event-loop)))
  (defun interrupt-loop ()
    (sleep 5)
    (mp:interrupt-process thread (lambda ()))
    (interrupt-loop)))

(interrupt-loop)