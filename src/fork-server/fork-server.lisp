
(format t "fork-server.lisp starting *features* -> ~s~%" *features*)
(format t "fork-server.lisp - loading start-cando.lisp~%") 
(load "source-dir:extensions;cando;src;lisp;start-cando.lisp")

(in-package :cl-user)

(defparameter *parent-cleanup* t)
(defmacro with-directory-cleanup ((dir &key (cleanup t)) &body code)
  `(unwind-protect
        (progn
          (ensure-directories-exist ,dir)
          ,@code)
     (progn
       (format t "Process ~a is cleaning up directory ~a~%" (core:getpid) ,dir)
     (when (and *parent-cleanup* ,cleanup) (core:rmdir ,dir)))))

(defmacro with-file-cleanup ((stream filename &key (cleanup t)) &body body)
  `(unwind-protect
        (progn
          (with-open-file (,stream ,filename :direction :output :if-exists :supersede)
            ,@body))
     (progn
       (format t "Process ~a is cleaning up file ~a~%" (core:getpid) ,filename)
       (when (and *parent-cleanup* ,cleanup) (delete-file ,filename)))))

(defmacro with-server-data ((&key server-info-directory pid port) &body body)
  (let ((pidstream (gensym))
        (portstream (gensym)))
    `(with-directory-cleanup (,server-info-directory)
       (with-file-cleanup (,pidstream (make-pathname :name "pidfile" :defaults ,server-info-directory))
         (with-file-cleanup (,portstream (make-pathname :name "portfile" :defaults ,server-info-directory))
           (format ,pidstream "~d~%" ,pid)
           (format ,portstream "~d~%" ,port)
           ,@body)))))

(defmacro with-child-data ((&key child-info-directory child-pid client-pid connection-file-name) &body body)
  (let ((pidstream (gensym))
        (clientstream (gensym))
        (cstream (gensym)))
    `(with-directory-cleanup (,child-info-directory)
       (with-file-cleanup (,pidstream (make-pathname :name "child-pidfile" :defaults ,child-info-directory))
         (with-file-cleanup (,clientstream (make-pathname :name "client-pid" :defaults ,child-info-directory))
           (with-file-cleanup (,cstream (make-pathname :name "connection-file-name" :defaults ,child-info-directory))
             (format ,pidstream "~d~%" ,child-pid)
             (format ,clientstream "~d~%" ,client-pid)
             (format ,cstream "~s~%" ,connection-file-name)
             ,@body))))))


(defun main-jupyterlab-fork-server (&optional (server-info-directory #P"/tmp/clasp-fork-server/"))
  (format t "Warming up the compiler... #dispatchers -> ~d~%" cmp::*dispatcher-count*)
  (compile-file "sys:kernel;lsp;foundation.lsp" :output-file (core:mkstemp "/tmp/foundation"))
  (format t "The compiler is warmed up #dispatchers -> ~d~%" cmp::*dispatcher-count*)
  (let ((listen (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream
                               :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address listen) t)
    (sb-bsd-sockets:socket-bind listen
                                (sb-bsd-sockets:make-inet-address "127.0.0.1")
                                0)
    (format t "Listening to socket~%")
    (multiple-value-bind (address port)
        (sb-bsd-sockets:socket-name listen)
      (with-server-data (:server-info-directory server-info-directory
                         :pid (core:getpid)
                         :port port)
        (format t "server pid: ~a~%" (core:getpid))
        (format t "server-info-directory -> ~a~%" server-info-directory)
        ;; Create a connection queue, ignore child exit details and wait for clients.
        (sb-bsd-sockets:socket-listen listen 5) ; listen(server_sockfd, 5);
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
            (unwind-protect
                 (let ((connection-file-name (read-line stream)))
                   (format t "Received connection-file-name: ~s~%" connection-file-name)
                   (finish-output)
		   (format t "server-info-directory: ~s~%" server-info-directory)
                   (finish-output)
                   (gctools:change-sigchld-sigport-handlers)
                   (multiple-value-bind (maybe-error pid child-stream)
                       (core:fork)
                     (if (= pid 0)
                         (let* ((child-directory (merge-pathnames (make-pathname :directory (list :relative (format nil "~d" (core:getpid)))) server-info-directory))
                                (child-connection-file-name (make-pathname :name connection-file-name :defaults child-directory))
                                (working-directory (read-line stream)))
                           (format t "Changing working directory for child to: ~s~%" working-directory)
                           (ext:chdir working-directory t)
                           (format stream "~d~%" (core:getpid))
                           (finish-output stream)
                           (let ((client-pid (parse-integer (read-line stream))))
                             (close stream)
                             (with-child-data (:child-info-directory child-directory
                                               :connection-file-name child-connection-file-name
                                               :child-pid (core:getpid)
                                               :client-pid client-pid)
                               (format t "Starting cl-jupyter-kernel-start child pid ~d~%" (core:getpid))
                               (let* ((delay (ext:getenv "CLASP_FORK_SERVER_DELAY"))
                                      (delay-seconds (if delay (parse-integer delay) nil)))
                                 (when delay-seconds
                                   (format t "Delaying ~d seconds before starting server~%" delay-seconds)
                                   (sleep delay-seconds)
                                   (format t "Done delay~%")))
                               (cando-user:cl-jupyter-kernel-start connection-file-name)
                               (sb-bsd-sockets:socket-close listen))
                             (format t "Child ~a is exiting - client ~a should be killed as well...~%" (core:getpid) client-pid)
                             (core:cexit))))))
              (progn
                ;; parent
                (close stream)))))))
    (sb-bsd-sockets:socket-close listen)))

(export 'main-jupyterlab-fork-server)

(in-package :cando-user)
(format t "Entered :cando-user package~%")


(format t "Starting jupyterlab-fork-server *load-pathname* -> ~s~%" *load-pathname*)
(cl-user:main-jupyterlab-fork-server)
