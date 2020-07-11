;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;; $Id$

;; This file is based on SBCL's SB-BSD-SOCKET module and has been
;; heavily modified to work with ECL by Julian Stecklina.
;; Port to Windows Sockets contributed by M. Goffioul.

;; You may do whatever you want with this file. (PUBLIC DOMAIN)

;; Trivial stuff is copied from SBCL's SB-BSD-SOCKETS, which is also
;; in the public domain.

(in-package "SB-BSD-SOCKETS")

#+clasp
(export '(
          GET-HOST-BY-NAME GET-HOST-BY-ADDRESS
          SOCKET-BIND SOCKET-ACCEPT SOCKET-CONNECT
          SOCKET-PEERNAME SOCKET-NAME SOCKET-LISTEN
          SOCKET-RECEIVE SOCKET-CLOSE SOCKET-MAKE-STREAM
          GET-PROTOCOL-BY-NAME MAKE-INET-ADDRESS LOCAL-SOCKET
          SOCKET INET-SOCKET SOCKET-FILE-DESCRIPTOR 
          SOCKET-FAMILY SOCKET-PROTOCOL SOCKET-TYPE
          SOCKET-ERROR NAME-SERVICE-ERROR NON-BLOCKING-MODE
          HOST-ENT-NAME HOST-ENT-ALIASES HOST-ENT-ADDRESS-TYPE
          ;;; these should be exported by define-name-service-condition
          NETDB-SUCCESS-ERROR NETDB-INTERNAL-ERROR
          HOST-NOT-FOUND-ERROR TRY-AGAIN-ERROR NO-RECOVERY-ERROR
          ;;; but aren't
          HOST-ENT-ADDRESSES HOST-ENT HOST-ENT-ADDRESS SOCKET-SEND))




;; Obviously this requires the one or other form of BSD compatible
;; socket interface.



;;; This courtesy of Pierre Mai in comp.lang.lisp 08 Jan 1999 00:51:44 +0100
;;; Message-ID: <87lnjebq0f.fsf@orion.dent.isdn.cs.tu-berlin.de>

(defun split (string &optional max (ws '(#\Space #\Tab)))
  "Split `string' along whitespace as defined by the sequence `ws'.
The whitespace is elided from the result.  The whole string will be
split, unless `max' is a non-negative integer, in which case the
string will be split into `max' tokens at most, the last one
containing the whole rest of the given `string', if any."
  (flet ((is-ws (char) (find char ws)))
    (loop for start = (position-if-not #'is-ws string)
          then (position-if-not #'is-ws string :start index)
          for index = (and start
                           (if (and max (= (1+ word-count) max))
                               nil
                             (position-if #'is-ws string :start start)))
          while start
          collect (subseq string start index)
          count 1 into word-count
          while index)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NAME RESOLVING
;;;

(defclass host-ent ()
  ((name :initarg :name :accessor host-ent-name)
   (aliases :initarg :aliases :accessor host-ent-aliases)
   (address-type :initarg :type :accessor host-ent-address-type)
                                        ; presently always AF_INET
   (addresses :initarg :addresses :accessor host-ent-addresses))
  (:documentation ""))

(defgeneric host-ent-address (host-ent)
  (:documentation ""))

(defmethod host-ent-address ((host-ent host-ent))
  (car (host-ent-addresses host-ent)))

;; FIXME: We should move this to using getaddrinfo
(defun get-host-by-name (host-name)
  "Returns a HOST-ENT instance for HOST-NAME or throws some kind of condition.
HOST-NAME may also be an IP address in dotted quad notation or some other
weird stuff - see gethostbyname(3) for grisly details."
  (let ((host-ent (make-instance 'host-ent)))
    (if (sockets-internal:ll-get-host-by-name host-name host-ent
                                              #'(setf host-ent-name)
                                              #'(setf host-ent-aliases)
                                              #'(setf host-ent-address-type)
                                              #'(setf host-ent-addresses))
        host-ent
        (name-service-error "get-host-by-name"))))

(defun get-host-by-address (address)
  (assert (and (typep address 'vector)
               (= (length address) 4)))
  (let ((host-ent (make-instance 'host-ent)))
    (if (sockets-internal:ll-get-host-by-address address host-ent
                                                 #'(setf host-ent-name)
                                                 #'(setf host-ent-aliases)
                                                 #'(setf host-ent-address-type)
                                                 #'(setf host-ent-addresses))
        host-ent
        (name-service-error "get-host-by-address"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SOCKET BASE CLASS AND GENERIC FUNCTIONS
;;;

(defclass socket ()
  ((file-descriptor :initarg :descriptor
                    :reader socket-file-descriptor)
   (family :initform (error "No socket family")
           :reader socket-family)
   (protocol :initarg :protocol
             :reader socket-protocol
             :documentation "Protocol used by the socket. If a
keyword, the symbol-name of the keyword will be passed to
GET-PROTOCOL-BY-NAME downcased, and the returned value used as
protocol. Other values are used as-is.")
   (type  :initarg :type
          :reader socket-type
          :initform :stream
          :documentation "Type of the socket: :STREAM or :DATAGRAM.")
   (stream)
)
  (:documentation "Common base class of all sockets, not meant to be
directly instantiated."))


(defmethod print-object ((object socket) stream)
  (print-unreadable-object (object stream :type t :identity t)
                           (princ "descriptor " stream)
                           (princ (slot-value object 'file-descriptor) stream)))

(defmethod shared-initialize :after ((socket socket) slot-names
                                     &key protocol type
                                     &allow-other-keys)
  (let* ((proto-num
          (cond ((and protocol (keywordp protocol))
                 (get-protocol-by-name (string-downcase (symbol-name protocol))))
                (protocol protocol)
                (t 0)))
         (fd (or (and (slot-boundp socket 'file-descriptor)
                      (socket-file-descriptor socket))
                 (ff-socket (socket-family socket)
                            (ecase (or type
                                       (socket-type socket))
                              ((:datagram) +sock-dgram+)
                              ((:stream) +sock-stream+))
                            proto-num))))
    (if (= fd -1) (socket-error "socket"))
    (setf (slot-value socket 'file-descriptor) fd
          (slot-value socket 'protocol) proto-num)
    #+ ignore
    (sb-ext:finalize socket (lambda () (sockint::close fd)))))

;; Generics

(defgeneric socket-bind (socket &rest address)
  (:documentation "Bind SOCKET to ADDRESS, which may vary according to
socket family.  For the INET family, pass ADDRESS and PORT as two
arguments; for FILE address family sockets, pass the filename string.
See also bind(2)"))

(defgeneric socket-accept (socket)
  (:documentation "Perform the accept(2) call, returning a
newly-created connected socket and the peer address as multiple
values"))

(defgeneric socket-connect (socket &rest address)
  (:documentation "Perform the connect(2) call to connect SOCKET to a
  remote PEER.  No useful return value."))

(defgeneric socket-peername (socket)
  (:documentation "Return the socket's peer; depending on the address
  family this may return multiple values"))

(defgeneric socket-name (socket)
  (:documentation "Return the address (as vector of bytes) and port
  that the socket is bound to, as multiple values."))

(defgeneric socket-listen (socket backlog)
  (:documentation "Mark SOCKET as willing to accept incoming connections.  BACKLOG
defines the maximum length that the queue of pending connections may
grow to before new connection attempts are refused.  See also listen(2)"))

(defgeneric socket-receive (socket buffer length
                            &key
                            oob peek waitall element-type)
  (:documentation "Read LENGTH octets from SOCKET into BUFFER (or a freshly-consed buffer if
NIL), using recvfrom(2).  If LENGTH is NIL, the length of BUFFER is
used, so at least one of these two arguments must be non-NIL.  If
BUFFER is supplied, it had better be of an element type one octet wide.
Returns the buffer, its length, and the address of the peer
that sent it, as multiple values.  On datagram sockets, sets MSG_TRUNC
so that the actual packet length is returned even if the buffer was too
small"))

(defgeneric socket-send (socket buffer length 
                         &key 
                         address external-format oob eor dontroute dontwait 
                         nosignal confirm more)
  (:documentation "Send length octets from buffer into socket, using sendto(2).
If buffer is a string, it will converted to octets according to external-format&
If length is nil, the length of the octet buffer is used. The format of address
depends on the socket type (for example for inet domain sockets it would be a 
list of an ip address and a port). If no socket address is provided, send(2) 
will be called instead. Returns the number of octets written."))


(defgeneric socket-close (socket &key abort)
  (:documentation "Close SOCKET.  May throw any kind of error that write(2) would have
thrown.  If SOCKET-MAKE-STREAM has been called, calls CLOSE on that
stream instead"))

(defgeneric socket-make-stream (socket  &rest args)
    (:documentation "Find or create a STREAM that can be used for IO
on SOCKET (which must be connected).  ARGS are passed onto
SB-SYS:MAKE-FD-STREAM."))

(defgeneric non-blocking-mode (socket)
  (:documentation "Is SOCKET in non-blocking mode?"))

(defgeneric (setf non-blocking-mode) (non-blocking-p socket)
  (:documentation "Put SOCKET in non-blocking mode - or not, according to NON-BLOCKING-P"))

(defgeneric socket-close-low-level (socket)
  (:documentation "Close SOCKET at low level. Do not use directly."))

;; Methods

(defmethod socket-listen ((socket socket) backlog)
  (let ((r (ff-listen (socket-file-descriptor socket) backlog)))
    (if (= r -1)
        (socket-error "listen"))))

(defmethod socket-close-low-level ((socket socket))
  (ff-close (socket-file-descriptor socket)))

(defmethod socket-close ((socket socket)  &key abort)
  ;; the close(2) manual page has all kinds of warning about not
  ;; checking the return value of close, on the grounds that an
  ;; earlier write(2) might have returned successfully w/o actually
  ;; writing the stuff to disk.  It then goes on to define the only
  ;; possible error return as EBADF (fd isn't a valid open file
  ;; descriptor).  Presumably this is an oversight and we could also
  ;; get anything that write(2) would have given us.

  ;; note that if you have a socket _and_ a stream on the same fd,
  ;; the socket will avoid doing anything to close the fd in case
  ;; the stream has done it already - if so, it may have been
  ;; reassigned to some other file, and closing it would be bad

  (let ((fd (socket-file-descriptor socket)))
    (unless (eql fd -1) ; already closed
      (cond ((slot-boundp socket 'stream)
             (let ((stream (slot-value socket 'stream)))
               #+threads
               (close (two-way-stream-input-stream stream) :abort abort)
               #+threads
               (close (two-way-stream-output-stream stream) :abort abort)
               #-threads
               (close stream :abort abort)) ;; closes fd indirectly
             (slot-makunbound socket 'stream))
            ((= (socket-close-low-level socket) -1)
             (socket-error "close")))
      (setf (slot-value socket 'file-descriptor) -1))))


;;; Receive data from a datagram socket, and return 4 values:
;;;   return-buffer, return-length, remote-host, and remove-port. 

(defmethod socket-receive ((socket socket) buffer length
                           &key oob peek waitall element-type)
  (unless (or buffer length)
    (error "You have to supply either buffer or length!"))
  (unless length
    (setq size (length buffer)))
  (let ((need-to-copy nil)
        (local-buffer nil))
    (cond ((null buffer)(setq local-buffer (sys:make-static-vector (upgraded-array-element-type '(unsigned-byte 8)) length)))
          (t (setq local-buffer (sys:make-static-vector (upgraded-array-element-type '(unsigned-byte 8)) length)
                   need-to-copy t)))
    (let ((length (or length (length local-buffer)))
          (fd (socket-file-descriptor socket)))
      (multiple-value-bind (len-recv errno remote-host remote-port)
          (ll-socket-receive fd local-buffer length oob peek waitall)
        (cond ((and (= len-recv -1)
                    (member errno (list +eagain+ +eintr+)))
               nil)
              ((= len-recv -1)
               (socket-error "receive"))
              (t
               (cond (need-to-copy
                      (dotimes (x len-recv)
                        (setf (aref buffer x) (aref local-buffer x)))
                      (values buffer len-recv remote-host remote-port))
                     (t (values local-buffer len-recv remote-host remote-port)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INET SOCKETS
;;;

;; We could refactor a lot here, if we pass sockaddr_foo structs around in Lisp. But
;; I do not feel comfortable with that.

(defun get-protocol-by-name (string-or-symbol)
  "Calls getprotobyname"
  (let ((string (string string-or-symbol)))
    (sockets-internal:ll-get-protocol-by-name string)))

(defun make-inet-address (dotted-quads)
  "Return a vector of octets given a string DOTTED-QUADS in the format
\"127.0.0.1\""
  (map 'vector
       #'parse-integer
       (split dotted-quads nil '(#\.))))

(defclass inet-socket (socket)
  ((family :initform +af-inet+))
  (:documentation "Class representing TCP and UDP sockets.

Examples:

 (make-instance 'inet-socket :type :stream :protocol :tcp)

 (make-instance 'inet-socket :type :datagram :protocol :udp)
"))

(defun make-inet-socket (type protocol)
  "Make an INET socket.  Deprecated in favour of make-instance"
  (make-instance 'inet-socket :type type :protocol protocol))


(defmethod socket-bind ((socket inet-socket) &rest address)
  (assert (= 2 (length address)) (address) "Socket-bind needs three parameters for inet sockets.")
  (let ((ip (first address))
        (port (second address)))
    (if (= -1 (ll-socket-bind-inet-socket
               port (aref ip 0) (aref ip 1) (aref ip 2) (aref ip 3)
               (socket-file-descriptor socket)))
        (socket-error "bind"))))

(defmethod socket-accept ((socket inet-socket))
  (let ((sfd (socket-file-descriptor socket)))
    (multiple-value-bind (fd vector port)
        (ll-socket-accept-inet-socket sfd)
      (cond
        ((= fd -1)
         (socket-error "accept"))
        (t
         (values
           (make-instance (class-of socket)
                          :type (socket-type socket)
                          :protocol (socket-protocol socket)
                          :descriptor fd)
           vector
           port))))))

(defmethod socket-connect ((socket inet-socket) &rest address)
  (let ((ip (first address))
        (port (second address)))
    (if (= -1
           (ll-socket-connect-inet-socket port (aref ip 0) (aref ip 1) (aref ip 2) (aref ip 3)
                                          (socket-file-descriptor socket)))
        (socket-error "connect"))))

(defmethod socket-peername ((socket inet-socket))
  (let* ((vector (make-array 4))
         (fd (socket-file-descriptor socket))
         (port (ll-socket-peername-inet-socket fd vector)))
    (if (>= port 0)
        (values vector port)
        (socket-error "getpeername"))))

(defmethod socket-name ((socket inet-socket))
  (let* ((vector (make-array 4))
         (fd (socket-file-descriptor socket))
         (port (ll-socket-name fd vector)))
    (if (>= port 0)
        (values vector port)
        (socket-error "getsockname"))))



(defmethod socket-send ((socket socket) buffer length
                        &key address external-format oob eor dontroute dontwait nosignal confirm more)
  (declare (ignore external-format more))
  (assert (or (stringp buffer) (typep buffer 'vector)))
  (let ((length (or length (length buffer)))
        (fd (socket-file-descriptor socket)))
    ;;; Now we need a buffer that is not moved by the GC
    ;;; So lets borrow that from our static vector code
    (let ((new-buffer (sys:make-static-vector (upgraded-array-element-type '(unsigned-byte 8)) length)))
      (dotimes (x length)
        (setf (aref new-buffer x)(aref buffer x)))
    (let ((len-sent
           (if address
               (progn
                 (assert (= 2 (length address)))
                 (ll-socket-send-address fd new-buffer length (second address)
                                         (aref (first address) 0) (aref (first address) 1)
                                         (aref (first address) 2) (aref (first address) 3)
                                         oob eor dontroute dontwait nosignal confirm))
               (ll-socket-send-no-address fd new-buffer length
                                          oob eor dontroute dontwait nosignal confirm))))
      (if (= len-sent -1)
          (socket-error "send")
          len-sent)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UNIX SOCKETS
;;;

(progn

(defclass local-socket (socket)
  ((family :initform +af-local+))
  (:documentation "Class representing local domain (AF_LOCAL) sockets,
also known as unix-domain sockets."))


(defmethod socket-bind ((socket local-socket) &rest address)
  (assert (= 1 (length address)) (address) "Socket-bind needs two parameters for local sockets.")
  (let ((name (first address))
        (fd (socket-file-descriptor socket))
        (family (socket-family socket)))
    (if (= -1
           (ll-socket-bind-local-socket fd name family))
        (socket-error "bind"))))

(defmethod socket-accept ((socket local-socket))
  (multiple-value-bind (fd name)
      (ll-socket-accept-local-socket (socket-file-descriptor socket))
    (cond
      ((= fd -1)
       (socket-error "accept"))
      (t
       (values
        (make-instance (class-of socket)
                       :type (socket-type socket)
                       :protocol (socket-protocol socket)
                       :descriptor fd)
        name)))))

(defmethod socket-connect ((socket local-socket) &rest address)
  (assert (= 1 (length address)) (address) "Socket-connect needs two parameters for local sockets.")
  (let ((path (first address))
        (fd (socket-file-descriptor socket))
        (family (socket-family socket)))
    (if (= -1
           (ll-socket-connect-local-socket fd family path))
        (socket-error "connect"))))

(defmethod socket-peername ((socket local-socket))
  (let* ((fd (socket-file-descriptor socket))
         (peer (ll-socket-peername-local-socket fd)))
    (if peer
        peer
        (socket-error "getpeername"))))

) ;#-:wsock



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NAMED PIPE SOCKETS [WIN32]
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NON-BLOCKING MODE
;;;

(defmethod non-blocking-mode ((socket socket))
  #-:wsock
  (let ((fd (socket-file-descriptor socket)))
    (not (zerop (ll-non-blocking-mode fd))))
)

(defmethod (setf non-blocking-mode) (non-blocking-p (socket socket))
  (let ((fd (socket-file-descriptor socket))
        (nblock (if non-blocking-p 1 0)))
    (if (= -1 (ll-setf-non-blocking-mode fd nblock))
        (socket-error #-:wsock "fcntl" )
        #-:wsock non-blocking-p
        )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; STREAMS
;;;
;;; To actually read/write to/from the sockets, we use Lisp streams. The
;;; following functions take care of building the streams. Fortunately
;;; we do not have to care about things like buffering, binary streams,
;;; etc, but we rather reuse the code from the C core. (For instance
;;; the sockets will be closed upon garbage collection)
;;;

(defun dup (fd)
  (ll-dup fd))

(defun make-stream-from-fd (fd mode &key buffering element-type (external-format :default)
                            (name "FD-STREAM"))
  (assert (stringp name) (name) "name must be a string.")
  (let* ((smm-mode (ecase mode
                       (:input +clasp-stream-mode-input+)
                       (:output +clasp-stream-mode-output+)
                       (:input-output +clasp-stream-mode-io+)
                       ))
         (external-format (unless (subtypep element-type 'integer) external-format))
         (stream (ll-make-stream-from-fd name fd smm-mode element-type external-format)))
    (when buffering
      (si::set-buffering-mode stream buffering))
    stream))

(defun auto-close-two-way-stream (stream)
  (ll-auto-close-two-way-stream stream))

(defun socket-make-stream-inner (fd input output buffering element-type external-format)
  ;; In Unix we have to create one stream per channel. The reason is
  ;; that buffered I/O is done using ANSI C FILEs which do not support
  ;; concurrent reads and writes -- if one thread is listening to the
  ;; FILE it blocks all output.  The solution is to create a
  ;; two-way-stream when both input and output are T, and force that
  ;; stream to close its components (small hack in ECL). In Windows we
  ;; do not have this problem because we do not know how to wrap a
  ;; FILE around a socket.
  (cond ((and input output)
         #-wsock
         (let* ((in (socket-make-stream-inner (dup fd) t nil buffering
                                              element-type external-format))
                (out (socket-make-stream-inner fd nil t buffering
                                               element-type external-format))
                (stream (make-two-way-stream in out)))
           (auto-close-two-way-stream stream)
           stream))
        (input
         (make-stream-from-fd fd #-wsock :input
                              :buffering buffering
                              :element-type element-type
                              :external-format external-format))
        (output
         (make-stream-from-fd fd #-wsock :output 
                              :buffering buffering
                              :element-type element-type
                              :external-format external-format))
        (t
         (error "SOCKET-MAKE-STREAM: at least one of :INPUT or :OUTPUT has to be true."))))

(defmethod socket-make-stream ((socket socket)
                               &key (input nil input-p)
                               (output nil output-p)
                               (buffering :full)
                               (element-type 'base-char)
                               (external-format :default))
  (let ((stream (and (slot-boundp socket 'stream)
                     (slot-value socket 'stream))))
    (unless stream
      ;; Complicated default logic for compatibility with previous releases
      ;; should disappear soon. (FIXME!)
      (unless (or input-p output-p)
        (setf input t output t))
      (setf stream (socket-make-stream-inner (socket-file-descriptor socket)
                                             input output buffering element-type
                                             external-format))
      (setf (slot-value socket 'stream) stream)
      #+ ignore
      (sb-ext:cancel-finalization socket))
    stream))

(defmethod ext::stream-fd ((socket socket))
  (socket-file-descriptor socket))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ERROR HANDLING
;;;
;;; A plethora of conditions are defined below, almost one for each
;;; possible error produced by the socket or DNS interface.
;;;


;;;
;;; 1) SOCKET ERRORS
;;;

(define-condition socket-error (error)
  ((errno :initform nil
          :initarg :errno
          :reader socket-error-errno)
   (symbol :initform nil :initarg :symbol :reader socket-error-symbol)
   (syscall  :initform "outer space" :initarg :syscall :reader socket-error-syscall))
  (:report (lambda (c s)
             (let ((num (socket-error-errno c)))
               (format s "Socket error in \"~A\": ~A (~A)"
                       (socket-error-syscall c)
                       (or (socket-error-symbol c) (socket-error-errno c))
                       #-:wsock
                       (ll-strerror num)))))
  (:documentation "Common base class of socket related conditions."))

(defmacro define-socket-condition (symbol name)
  `(let () ; Prevents evaluation of constant value at compilation time
     (define-condition ,name (socket-error)
       ((symbol :reader socket-error-symbol :initform (quote ,symbol))))
     (export ',name)
     (push (cons ,symbol (quote ,name)) *conditions-for-errno*)))

(defparameter *conditions-for-errno* nil)
;;; this needs the rest of the list adding to it, really.  They also
;;; need symbols to be added to constants.ccon
;;; I haven't yet thought of a non-kludgey way of keeping all this in
;;; the same place
(define-socket-condition +EADDRINUSE+ address-in-use-error)
(define-socket-condition +EAGAIN+ interrupted-error)
(define-socket-condition +EBADF+ bad-file-descriptor-error)
(define-socket-condition +ECONNREFUSED+ connection-refused-error)
(define-socket-condition +ETIMEDOUT+ operation-timeout-error)
(define-socket-condition +EINTR+ interrupted-error)
(define-socket-condition +EINVAL+ invalid-argument-error)
(define-socket-condition +ENOBUFS+ no-buffers-error)
(define-socket-condition +ENOMEM+ out-of-memory-error)
(define-socket-condition +EOPNOTSUPP+ operation-not-supported-error)
(define-socket-condition +EPERM+ operation-not-permitted-error)
(define-socket-condition +EPROTONOSUPPORT+ protocol-not-supported-error)
(define-socket-condition +ESOCKTNOSUPPORT+ socket-type-not-supported-error)
(define-socket-condition +ENETUNREACH+ network-unreachable-error)


(defun condition-for-errno (err)
  (or (cdr (assoc err *conditions-for-errno* :test #'eql)) 'socket-error))

(defun socket-error (where)
  (let* ((errno  (ll-socket-errno))
         (condition (condition-for-errno errno)))
    (error condition :errno errno  :syscall where)))

;;;
;;; 2) DNS ERRORS
;;;

(defvar *name-service-errno* 0
  "The value of h_errno, after it's been fetched from Unix-land by calling
GET-NAME-SERVICE-ERRNO")

(defun name-service-error (where)
  (get-name-service-errno)
  ;; Comment next to NETDB-INTERNAL in netdb.h says "See errno.".
  ;; This special case treatment hasn't actually been tested yet.
  (if (= *name-service-errno* +NETDB-INTERNAL+)
      (socket-error where)
    (let ((condition
           (condition-for-name-service-errno *name-service-errno*)))
      (error condition :errno *name-service-errno* :syscall where))))

(define-condition name-service-error (condition)
  ((errno :initform nil
          :initarg :errno
          :reader name-service-error-errno)
   (symbol :initform nil :initarg :symbol :reader name-service-error-symbol)
   (syscall :initform "an unknown location" :initarg :syscall :reader name-service-error-syscall))
  (:report (lambda (c s)
             (let ((num (name-service-error-errno c)))
               (format s "Name service error in \"~A\": ~A (~A)"
                       (name-service-error-syscall c)
                       (or (name-service-error-symbol c)
                           (name-service-error-errno c))
                       (get-name-service-error-message num))))))

(defmacro define-name-service-condition (symbol name)
  `(let ()
     (define-condition ,name (name-service-error)
       ((symbol :reader name-service-error-symbol :initform (quote ,symbol))))
     (push (cons ,symbol (quote ,name)) *conditions-for-name-service-errno*)
     (export (quote ,symbol))))

(defparameter *conditions-for-name-service-errno* nil)

(define-name-service-condition +NETDB-INTERNAL+ netdb-internal-error)
(define-name-service-condition +NETDB-SUCCESS+ netdb-success-error)
(define-name-service-condition +HOST-NOT-FOUND+ host-not-found-error)
(define-name-service-condition +TRY-AGAIN+ try-again-error)
(define-name-service-condition +NO-RECOVERY+ no-recovery-error)
;; this is the same as the next one
;;(define-name-service-condition NO_DATA no-data-error)
(define-name-service-condition +NO-ADDRESS+ no-address-error)

(defun condition-for-name-service-errno (err)
  (or (cdr (assoc err *conditions-for-name-service-errno* :test #'eql))
      'name-service))

(defun get-name-service-errno ()
  (setf *name-service-errno* (sockets-internal:ll-get-name-service-h-errno)))

(defun get-name-service-error-message (num)
  (ll-strerror num)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SOCKET OPTIONS
;;;

(defun get-sockopt-int (fd level const)
  (let ((ret (sockets-internal:ll-get-sockopt-int fd level const)))
    (if ret
        ret
        (error "Sockopt error: ~A" (ll-strerror-errno)))))

(defun get-sockopt-bool (fd level const)
  (let ((ret (sockets-internal:ll-get-sockopt-bool fd level const)))
    (if ret
        (/= ret 0)
        (error "Sockopt error: ~A" (ll-strerror-errno)))))


#-wsock
(defun get-sockopt-timeval (fd level const)
  (let ((ret (sockets-internal:ll-get-sockopt-timeval fd level const)))
    (if ret
        ret
        (error "Sockopt error: ~A" (ll-strerror-errno)))))

(defun get-sockopt-linger (fd level const)
  (let ((ret (sockets-internal:ll-get-sockopt-linger fd level const)))
    (if ret
        ret
        (error "Sockopt error: ~A" (ll-strerror-errno)))))

(defun set-sockopt-int (fd level const value)
  (let ((ret (ll-set-sockopt-int fd level const value)))
    (if ret
        value
        (error "Sockopt error: ~A" (ll-strerror-errno)))))

(defun set-sockopt-bool (fd level const value)
  (let ((ret (ll-set-sockopt-bool fd level const value)))
    (if ret
        value
        (error "Sockopt error: ~A" (ll-strerror-errno)))))

#-wsock
(defun set-sockopt-timeval (fd level const value)
  (let ((ret (ll-set-sockopt-timeval fd level const value)))
    (if ret
        value
        (error "Sockopt error: ~A" (ll-strerror-errno)))))


(defun set-sockopt-linger (fd level const value)
  (let ((ret (ll-set-sockopt-linger fd level const value)))
    (if ret
        value
        (error "Sockopt error: ~A" (ll-strerror-errno)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-sockopt (name c-level c-const type &optional (read-only nil))
    `(progn
       (export ',name)
       (defun ,name (socket)
         (,(intern (format nil "GET-SOCKOPT-~A" type))
           (socket-file-descriptor socket)
           ,c-level
           ,c-const
           ))
       ,@(unless read-only
           `((defun (setf ,name) (value socket)
               (,(intern (format nil "SET-SOCKOPT-~A" type))
                 (socket-file-descriptor socket)
                 ,c-level
                 ,c-const
                 value)))))))

(define-sockopt sockopt-type sockets-internal:+SOL-SOCKET+ sockets-internal:+SO-TYPE+ int t)
(define-sockopt sockopt-receive-buffer sockets-internal:+SOL-SOCKET+ sockets-internal:+SO-RCVBUF+ int)
(define-sockopt sockopt-receive-timeout sockets-internal:+SOL-SOCKET+ sockets-internal:+SO-RCVTIMEO+ timeval)
(define-sockopt sockopt-send-timeout sockets-internal:+SOL-SOCKET+ sockets-internal:+SO-SNDTIMEO+ timeval)
(define-sockopt sockopt-reuse-address sockets-internal:+SOL-SOCKET+ sockets-internal:+SO-REUSEADDR+ bool)
(define-sockopt sockopt-keep-alive sockets-internal:+SOL-SOCKET+ sockets-internal:+SO-KEEPALIVE+ bool)
(define-sockopt sockopt-dont-route sockets-internal:+SOL-SOCKET+ sockets-internal:+SO-DONTROUTE+ bool)
(define-sockopt sockopt-linger sockets-internal:+SOL-SOCKET+ sockets-internal:+SO-LINGER+ linger)

#-(or :sun4sol2 :linux :wsock :cygwin)
(define-sockopt sockopt-reuse-port sockets-internal:+SOL-SOCKET+ sockets-internal:+SO-REUSEPORT+ bool)

(define-sockopt sockopt-tcp-nodelay sockets-internal:+IPPROTO-TCP+ sockets-internal:+TCP-NODELAY+ bool)

;; Add sockopts here as you need them...

;; Finished loading
(provide 'sockets)
(provide 'sb-bsd-sockets)
