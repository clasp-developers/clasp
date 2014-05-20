(defun nslookup (hostname)
   "Performs a DNS look up for HOSTNAME and returns the address as a
   four element array, suitable for socket-connect.  If HOSTNAME is
   not found, a host-not-found-error condition is thrown."
   (if hostname
       (host-ent-address (get-host-by-name hostname))
       nil))

;; open a server port

(defun tcp-server (&key (interface (machine-instance)) (port 8000)  
(backlog 5))
   "Returns a socket server bound to PORT on INTERFACE.  Defaults to (machine-instance):8000.
   The queue size BACKLOG defaults to 5 connections."
   (handler-case
     (let ((server (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
       (socket-bind server (nslookup interface) port)
       (socket-listen server backlog)
       server)
     (address-in-use-error ()
                           (format t "address ~A : ~A is already in use" interface port)
                           (force-output)
                           nil)))

;; return a connection

(defun tcp-accept (server &key (timeout 30))
   "Waits up to TIMEOUT (defaults to 30) seconds for a client to connect to SERVER.
   A new socket is returned, which is bound to the client connection.  If a timeout
   occured, nil is returned."
   (when server
       (unless-timeout timeout
         (socket-accept server))))

;; connect to a server

(defun tcp-connect (server port &optional (timeout 5))
   "Returns a socket connected to SERVER:PORT.  If an error occurs, or the attempt times out
   after TIMOUT (default 5) secons, nil is returned."
   (when (and server port)
       (handler-case
         (unless-timeout timeout
           (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
             (socket-connect socket (nslookup server) port)
             socket))
         (host-not-found-error ()
                               (format t "host ~A not found." server)
                               (force-output)
                               nil))))

;; so once tcp-connect has connected to a server, tcp-connect returns a  
;; socket on the client ands tcp-accept on the server

;; print and read plain text

(defun tcp-print-raw (socket line)
   "Print LINE to SOCKET.  CRLF is added.  Returns the number of bytes written."
   (when (and socket line)
     (socket-send socket (concatenate 'string line (list #\return #\newline)) nil)))

(defun tcp-read-raw (socket &key (maxsize 65536) (timeout 5))
   "Reads one line from SOCKET, removes CRLF, and returns it.  The buffer size
   is 65536 bytes, unless MAXSIZE is specified.  If no result is received
   within TIMEOUT seconds (defaults to 5), nil is returned."
   (when socket
     (if-timeout (timeout (format t "socket-receive timed out after ~A seconds.~%" timeout) (force-output) nil)
       (values (socket-receive socket nil maxsize)))))

;; print and read printable objects

(defun tcp-print (socket object)
   "Writes OBJECT to SOCKET so that it can be received using tcp-read."
   (when (and socket object)
     (tcp-print-raw socket
                    (let ((ostream (make-string-output-stream)))
                      (print object ostream)
                      (get-output-stream-string ostream)))))

(defun tcp-read (socket &key (timeout 10) (maxsize 1048576))
   "Reads and returns a lisp object from the connection SOCKET."
   (when socket
     (let ((s (tcp-read-raw socket :maxsize maxsize :timeout timeout)))
       (when s
         (with-input-from-string (istream s) (read istream nil nil))))))

