(defun socket-server (&optional (port 9734))
  (let ((listen (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream
                               :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address listen) t)
    (sb-bsd-sockets:socket-bind listen
                                (sb-bsd-sockets:make-inet-address "127.0.0.1")
                                port)
    ;; Create a connection queue, ignore child exit details and wait for clients.
    (sb-bsd-sockets:socket-listen listen 5) ; listen(server_sockfd, 5);
    (unwind-protect
         (loop
           (format t "Server waiting~%")
           ;; Accept connection.
           (let* ((socket (sb-bsd-sockets:socket-accept listen))
                  (stream (sb-bsd-sockets:socket-make-stream socket
                                                             :input t
                                                             :output t
                                                             :buffering :none
                                                             :external-format :default
                                                             :element-type :default)))
             (let ((connection-file-name (read-line stream)))
               (format t "Received: ~s~%" connection-file-name)
               (finish-output)
               (multiple-value-bind (maybe-error pid child-stream)
                   (core:fork)
                 (if (= pid 0)
                     (loop
                       (format t "Child is running~%")
                       (sleep 10)))))
             #+(or)(progn
                     (sb-bsd-sockets:socket-close socket)
                     (format t "Exiting for now...~%"))))
      (sb-bsd-sockets:socket-close listen))))

#||
             ;; client_len = sizeof(client_address);
             ;; client_sockfd = accept(server_sockfd,(struct sockaddr *)&client_address, &client_len);
             (format t "Got client-sockfd~%")
             (multiple-value-bind (maybe-error pid stream)
                 (core:fork)
               (format t "fork -> ~s~%" pid)
               (if (= pid 0)
                   (progn
                     ;; We are the child
                     (format t "Child reading~%")
                     (let ((buffer (sb-bsd-sockets:socket-receive client-sockfd (make-array 256 :element-type 'base-char :fill-pointer 0))))
                       (format t "   read from child: ~s~%" buffer))
                     (sb-bsd-sockets:socket-send client-sockfd "RECV" 4)
                     (sb-bsd-sockets:socket-close client-sockfd)
                     (core:exit))))))))
||#
