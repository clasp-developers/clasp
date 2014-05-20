


(load "sys:lisp;serve-event;serve-event.lisp")
(use-package :serve-event)

(load "sys:lisp;sockets;sockets.lisp")


;;(require :sb-bsd-sockets)

(defpackage echo-server
  (:use :cl :sb-bsd-sockets)
  (:export "run-server")
  )

(in-package echo-server)

(defvar *port* 7000)

(defun make-echoer (stream id disconnector)
  (lambda (_)
    (declare (ignore _))
    (handler-case
        (let ((line (read-line stream)))
;;          (setf line (subseq line 0 (1- (length line))))
          (cond ((string= line "quit")
                 (funcall disconnector))
                (t
                 (format t "~a: ~a~%" id line)
		 (let ((result (eval (read-from-string line))))
		   (format t "-->~a: ~a~%" id result)
		   (format stream "-->~a: ~a~%" id result))
                 (force-output stream))))
      (end-of-file ()
        (funcall disconnector)))))

(defun make-disconnector (socket id)
  (lambda ()
    (let ((fd (socket-file-descriptor socket)))
      (format t "~a: closing~%" id)
      (serve-event::invalidate-descriptor fd)
      (socket-close socket))))

(defun serve (socket id)
  (let ((stream (socket-make-stream socket :output t :input t))
        (fd (socket-file-descriptor socket)))
    (serve-event::add-fd-handler fd
                             :input
                             (make-echoer stream
                                          id
                                          (make-disconnector socket id)))))

(defun echo-server (&optional (port *port*))
  (format t "Starting echo-server~%")
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp))
        (counter 0))
    (socket-bind socket #(127 0 0 1) port)
    (socket-listen socket 5)
    (serve-event::add-fd-handler (socket-file-descriptor socket)
                             :input
                             (lambda (_)
                               (declare (ignore _))
                               (incf counter)
                               (format t "Accepted client ~A~%" counter)
                               (serve (socket-accept socket) counter)))))



#+sb-thread
(sb-thread:make-thread (lambda ()
                         (echo-server)
                         (loop
                            (serve-event::serve-all-events))))
#-sb-thread
(echo-server)


(defun run-server ()
  "Runs a server that takes s-exps, evaluates them and returns the result - Loop never exits"
  (loop (serve-event:serve-all-events)))


(format t "Use (echo-server:run-server) to run the server~%")


