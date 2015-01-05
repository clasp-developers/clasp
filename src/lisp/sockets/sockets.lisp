;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;; $Id$

;; This file is based on SBCL's SB-BSD-SOCKET module and has been
;; heavily modified to work with ECL by Julian Stecklina.
;; Port to Windows Sockets contributed by M. Goffioul.

;; You may do whatever you want with this file. (PUBLIC DOMAIN)

;; Trivial stuff is copied from SBCL's SB-BSD-SOCKETS, which is also
;; in the public domain.

(in-package "SB-BSD-SOCKETS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (format t "Loading sockets.lisp"))

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
	  HOST-ENT-ADDRESSES HOST-ENT HOST-ENT-ADDRESS SOCKET-SEND))




;; Obviously this requires the one or other form of BSD compatible
;; socket interface.



;; Include the neccessary headers
#-clasp
(clines
 "#include <sys/types.h>"
 "#include <sys/socket.h>"
 "#include <sys/un.h>"
 "#define wincoerce(t,x) (x)"
 "#include <sys/time.h>"
 "#include <netdb.h>"
 "#include <string.h>"
 "#include <unistd.h>"
 "#include <netinet/in.h>"
 "#include <netinet/tcp.h>"
 "#include <errno.h>"
 "#include <fcntl.h>"
 "#ifndef MSG_CONFIRM"
 "#define MSG_CONFIRM 0"
 "#endif"
 "#ifndef MSG_NOSIGNAL"
 "#define MSG_NOSIGNAL 0"
 "#endif"
 "#ifndef MSG_DONTWAIT"
 "#define MSG_DONTWAIT 0"
 "#endif"
 "#ifndef MSG_EOR"
 "#define MSG_EOR 0"
 "#endif")



(eval-when (:compile-toplevel :execute)
  #-clasp(defmacro c-constant (c-name)
	  `(ffi:c-inline () () :int ,c-name :one-liner t))
  #+clasp(defmacro c-constant (c-name) `,c-name)
  #-clasp(defmacro define-c-constants (&rest args)
	  `(let ()	  ; Prevents evaluation of constant value form
	     ,@(loop
		  for (lisp-name c-name) on args by #'cddr
		  collect `(defconstant ,lisp-name (c-constant ,c-name))))))


#-clasp(define-c-constants
	  +af-inet+ "AF_INET"
	+af-local+ #-sun4sol2 "AF_LOCAL" #+sun4sol2 "AF_UNIX"
	+eagain+ "EAGAIN"
	+eintr+ "EINTR")


;; Foreign functions

#-clasp (progn
	 (defentry ff-socket (:int :int :int) (:int "socket") :no-interrupts t)
	 (defentry ff-listen (:int :int) (:int "listen") :no-interrupts t)
	 (defentry ff-close (:int) (:int "close") :no-interrupts t)
)

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
    (if (ffi:c-inline #+clasp sockets-internal:ll-get-host-by-name
		  (host-name host-ent
			     #'(setf host-ent-name)
			     #'(setf host-ent-aliases)
			     #'(setf host-ent-address-type)
			     #'(setf host-ent-addresses))
		  (:cstring t t t t t) t
		  "
{
	struct hostent *hostent = gethostbyname(#0);

	if (hostent != NULL) {
 	        char **aliases;
                char **addrs;
                cl_object aliases_list = ECL_NIL;
                cl_object addr_list = ECL_NIL;
                int length = hostent->h_length;

		funcall(3,#2,make_simple_base_string(hostent->h_name),#1);
                funcall(3,#4,ecl_make_integer(hostent->h_addrtype),#1);

                for (aliases = hostent->h_aliases; *aliases != NULL; aliases++) {
                        aliases_list = CONS(make_simple_base_string(*aliases),aliases_list);
                }
                funcall(3,#3,aliases_list,#1);

                for (addrs = hostent->h_addr_list; *addrs != NULL; addrs++) {
                        int pos;
                        cl_object vector = funcall(2,@make-array,MAKE_FIXNUM(length));
                        for (pos = 0; pos < length; pos++)
                                ecl_aset(vector, pos, MAKE_FIXNUM((unsigned char)((*addrs)[pos])));
                        addr_list = CONS(vector, addr_list);


                }
                funcall(3,#5,addr_list,#1);

                @(return) = #1;
	} else {
		@(return) = ECL_NIL;
	}
}"
		  :side-effects t)
	host-ent
	(name-service-error "get-host-by-name"))))

(defun get-host-by-address (address)
  (assert (and (typep address 'vector)
	       (= (length address) 4)))
  (let ((host-ent (make-instance 'host-ent)))
    (if
     (ffi:c-inline #+clasp sockets-internal:ll-get-host-by-address
	       (address host-ent
			#'(setf host-ent-name)
			#'(setf host-ent-aliases)
			#'(setf host-ent-address-type)
			#'(setf host-ent-addresses))
	       (t t t t t t) t
	       "
{
	unsigned char vector[4];
	struct hostent *hostent;
	vector[0] = fixint(ecl_aref(#0,0));
	vector[1] = fixint(ecl_aref(#0,1));
	vector[2] = fixint(ecl_aref(#0,2));
	vector[3] = fixint(ecl_aref(#0,3));
	ecl_disable_interrupts();
	hostent = gethostbyaddr(wincoerce(const char *, vector),4,AF_INET);
	ecl_enable_interrupts();

	if (hostent != NULL) {
 	        char **aliases;
                char **addrs;
                cl_object aliases_list = ECL_NIL;
                cl_object addr_list = ECL_NIL;
                int length = hostent->h_length;

		funcall(3,#2,make_simple_base_string(hostent->h_name),#1);
                funcall(3,#4,ecl_make_integer(hostent->h_addrtype),#1);

                for (aliases = hostent->h_aliases; *aliases != NULL; aliases++) {
                        aliases_list = CONS(make_simple_base_string(*aliases),aliases_list);
                }
                funcall(3,#3,aliases_list,#1);

                for (addrs = hostent->h_addr_list; *addrs != NULL; addrs++) {
                        int pos;
                        cl_object vector = funcall(2,@make-array,MAKE_FIXNUM(length));
                        for (pos = 0; pos < length; pos++)
                                ecl_aset(vector, pos, MAKE_FIXNUM((unsigned char)((*addrs)[pos])));
                        addr_list = CONS(vector, addr_list);


                }
                funcall(3,#5,addr_list,#1);

                @(return) = #1;
	} else {
		@(return) = ECL_NIL;
	}
}"
	       :side-effects t)
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
			      ((:datagram) +sock-dgram+ #|(c-constant "SOCK-DGRAM")|#)
			      ((:stream) +sock-stream+ #|(c-constant "SOCK-STREAM")|#))
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


(defgeneric socket-close (socket)
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

(defmethod socket-close ((socket socket))
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
               (close (two-way-stream-input-stream stream))
               #+threads
               (close (two-way-stream-output-stream stream))
               #-threads
               (close stream)) ;; closes fd indirectly
	     (slot-makunbound socket 'stream))
	    ((= (socket-close-low-level socket) -1)
	     (socket-error "close")))
      (setf (slot-value socket 'file-descriptor) -1))))

#-clasp
(ffi::clines "
static void *
safe_buffer_pointer(cl_object x, cl_index size)
{
	cl_type t = type_of(x);
	int ok = 0;
	if (t == t_base_string) {
		ok = (size <= x->base_string.dim);
	} else if (t == t_vector) {
		cl_elttype aet = (cl_elttype)x->vector.elttype;
		if (aet == aet_b8 || aet == aet_i8 || aet == aet_bc) {
			ok = (size <= x->vector.dim);
		} else if (aet == aet_fix || aet == aet_index) {
			cl_index divisor = sizeof(cl_index);
			size = (size + divisor - 1) / divisor;
			ok = (size <= x->vector.dim);
		}
	}
	if (!ok) {
		FEerror(\"Lisp object is not a valid socket buffer: ~A\", 1, x);
	}
	return (void *)x->vector.self.t;
}
")

;; FIXME: How bad is manipulating fillp directly?
(defmethod socket-receive ((socket socket) buffer length
			   &key oob peek waitall element-type)
  (unless (or buffer length) (error "You have to supply either buffer or length!"))
  (let ((buffer (or buffer (make-array length :element-type element-type)))
	(length (or length (length buffer)))
	(fd (socket-file-descriptor socket)))

    (multiple-value-bind (len-recv errno)
	   (ffi:c-inline #+clasp ll-socket-receive
		     (fd buffer length
		      oob peek waitall)
		     (:int :object :int :bool :bool :bool)
                  (values :long :int)
		     "
{
        int flags = ( #3 ? MSG_OOB : 0 )  |
                    ( #4 ? MSG_PEEK : 0 ) |
                    ( #5 ? MSG_WAITALL : 0 );
        cl_type type = type_of(#1);
	ssize_t len;

        ecl_disable_interrupts();
        len = recvfrom(#0, wincoerce(char*, safe_buffer_pointer(#1, #2)),
                       #2, flags, NULL,NULL);
	ecl_enable_interrupts();
        if (len >= 0) {
               if (type == t_vector) { #1->vector.fillp = len; }
               else if (type == t_base_string) { #1->base_string.fillp = len; }
        }
        @(return 0) = len;
        @(return 1) = errno;
}
"
                  :one-liner nil)
      (cond ((and (= len-recv -1)
                  (member errno (list +eagain+ +eintr+)))
             nil)
            ((= len-recv -1)
             (socket-error "receive"))
            (t 
             (values buffer len-recv))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INET SOCKETS
;;;

;; We could refactor a lot here, if we pass sockaddr_foo structs around in Lisp. But
;; I do not feel comfortable with that.

(defun get-protocol-by-name (string-or-symbol)
  "Calls getprotobyname"
  (let ((string (string string-or-symbol)))
    (ffi:c-inline #+clasp sockets-internal:ll-get-protocol-by-name
	      (string) (:cstring) :int
	      "getprotobyname(#0)->p_proto"
	      :one-liner t)))

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

#-clasp(Clines
       "
static void fill_inet_sockaddr(struct sockaddr_in *sockaddr, int port,
			       int a1, int a2, int a3, int a4)
{
#if defined(_MSC_VER) || defined(mingw32)
	memset(sockaddr,0,sizeof(struct sockaddr_in));
#else
	bzero(sockaddr,sizeof(struct sockaddr_in));
#endif
	sockaddr->sin_family = AF_INET;
	sockaddr->sin_port = htons(port);
	sockaddr->sin_addr.s_addr= htonl((uint32_t)a1<<24 | (uint32_t)a2<<16 | (uint32_t)a3<<8 | (uint32_t)a4) ;

}
")



(defmethod socket-bind ((socket inet-socket) &rest address)
  (assert (= 2 (length address)) (address) "Socket-bind needs three parameters for inet sockets.")
  (let ((ip (first address))
	(port (second address)))
    (if (= -1
	   (ffi:c-inline #+clasp ll-socket-bind-inet-socket
		     (port (aref ip 0) (aref ip 1) (aref ip 2) (aref ip 3)
			   (socket-file-descriptor socket))
		     (:int :int :int :int :int :int)
		     :int
		     "
{
	struct sockaddr_in sockaddr;
	int output;
	ecl_disable_interrupts();
	fill_inet_sockaddr(&sockaddr, #0, #1, #2, #3, #4);
	output = bind(#5,(struct sockaddr*)&sockaddr, sizeof(struct sockaddr_in));
	ecl_enable_interrupts();
	@(return) = output;
}"
		     :side-effects t))
	(socket-error "bind"))))

(defmethod socket-accept ((socket inet-socket))
  (let ((sfd (socket-file-descriptor socket)))
    (multiple-value-bind (fd vector port)
      (ffi:c-inline #+clasp ll-socket-accept-inet-socket
		(sfd) (:int) (values :int :object :int)
"{
        struct sockaddr_in sockaddr;
        socklen_t addr_len = (socklen_t)sizeof(struct sockaddr_in);
        int new_fd;

	ecl_disable_interrupts();
	new_fd = accept(#0, (struct sockaddr*)&sockaddr, &addr_len);
	ecl_enable_interrupts();

	@(return 0) = new_fd;
	@(return 1) = ECL_NIL;
	@(return 2) = 0;
        if (new_fd != -1) {
                uint32_t ip = ntohl(sockaddr.sin_addr.s_addr);
                uint16_t port = ntohs(sockaddr.sin_port);
                cl_object vector = cl_make_array(1,MAKE_FIXNUM(4));

                ecl_aset(vector,0, MAKE_FIXNUM( ip>>24 ));
		ecl_aset(vector,1, MAKE_FIXNUM( (ip>>16) & 0xFF));
		ecl_aset(vector,2, MAKE_FIXNUM( (ip>>8) & 0xFF));
                ecl_aset(vector,3, MAKE_FIXNUM( ip & 0xFF ));

		@(return 1) = vector;
		@(return 2) = port;
	}
}")
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
	   (ffi:c-inline #+clasp ll-socket-connect-inet-socket
		     (port (aref ip 0) (aref ip 1) (aref ip 2) (aref ip 3)
			   (socket-file-descriptor socket))
		     (:int :int :int :int :int :int)
		     :int
		     "
{
	struct sockaddr_in sockaddr;
	int output;

	ecl_disable_interrupts();
	fill_inet_sockaddr(&sockaddr, #0, #1, #2, #3, #4);
	output = connect(#5,(struct sockaddr*)&sockaddr, sizeof(struct sockaddr_in));
	ecl_enable_interrupts();

	@(return) = output;
}"))
	(socket-error "connect"))))

(defmethod socket-peername ((socket inet-socket))
  (let* ((vector (make-array 4))
	 (fd (socket-file-descriptor socket))
	 (port (ffi:c-inline #+clasp ll-socket-peername-inet-socket
			 (fd vector) (:int t) :int
"@01;{
        struct sockaddr_in name;
        socklen_t len = sizeof(struct sockaddr_in);
        int ret;

	ecl_disable_interrupts();
	ret = getpeername(#0,(struct sockaddr*)&name,&len);
	ecl_enable_interrupts();

        if (ret == 0) {
                uint32_t ip = ntohl(name.sin_addr.s_addr);
                uint16_t port = ntohs(name.sin_port);

                ecl_aset(#1,0, MAKE_FIXNUM( ip>>24 ));
		ecl_aset(#1,1, MAKE_FIXNUM( (ip>>16) & 0xFF));
		ecl_aset(#1,2, MAKE_FIXNUM( (ip>>8) & 0xFF));
                ecl_aset(#1,3, MAKE_FIXNUM( ip & 0xFF ));

                @(return) = port;
         } else {
                @(return) = -1;
         }
}")))
    (if (>= port 0)
	(values vector port)
	(socket-error "getpeername"))))

(defmethod socket-name ((socket inet-socket))
  (let* ((vector (make-array 4))
	 (fd (socket-file-descriptor socket))
	 (port (ffi:c-inline #+clasp ll-socket-name
			 (fd vector) (:int t) :int
"@01;{
        struct sockaddr_in name;
        socklen_t len = sizeof(struct sockaddr_in);
        int ret;

	ecl_disable_interrupts();
	ret = getsockname(#0,(struct sockaddr*)&name,&len);
	ecl_enable_interrupts();

        if (ret == 0) {
                uint32_t ip = ntohl(name.sin_addr.s_addr);
                uint16_t port = ntohs(name.sin_port);

                ecl_aset(#1,0, MAKE_FIXNUM( ip>>24 ));
		ecl_aset(#1,1, MAKE_FIXNUM( (ip>>16) & 0xFF));
		ecl_aset(#1,2, MAKE_FIXNUM( (ip>>8) & 0xFF));
                ecl_aset(#1,3, MAKE_FIXNUM( ip & 0xFF ));

                @(return) = port;
         } else {
                @(return) = -1;
         }
}")))
    (if (>= port 0)
	(values vector port)
	(socket-error "getsockname"))))



(defmethod socket-send ((socket socket) buffer length
			   &key address external-format oob eor dontroute dontwait nosignal confirm more)
  (declare (ignore external-format more))
  (assert (or (stringp buffer)
		(typep buffer 'vector)))
  (let (;eh, here goes string->octet convertion... 
	;When will ecl support Unicode?
	(length (or length (length buffer)))
	(fd (socket-file-descriptor socket)))
    (let ((len-sent
	   (if address
	       (progn
		 (assert (= 2 (length address)))
		 (ffi:c-inline #+clasp ll-socket-send-address
			   (fd buffer length 
			       (second address)
			       (aref (first address) 0)
			       (aref (first address) 1)
			       (aref (first address) 2)
			       (aref (first address) 3)
			       oob eor dontroute dontwait nosignal confirm)
		     (:int :object :int
			   :int :int :int :int :int
			   :bool :bool :bool :bool :bool :bool)
		     :long
		     "
{
	int sock = #0;
	int length = #2;
	void *buffer = safe_buffer_pointer(#1, length);
        int flags = ( #8 ? MSG_OOB : 0 )  |
                    ( #9 ? MSG_EOR : 0 ) |
                    ( #a ? MSG_DONTROUTE : 0 ) |
                    ( #b ? MSG_DONTWAIT : 0 ) |
                    ( #c ? MSG_NOSIGNAL : 0 ) |
                    ( #d ? MSG_CONFIRM : 0 );
        cl_type type = type_of(#1);
        struct sockaddr_in sockaddr;
	ssize_t len;

	ecl_disable_interrupts();
	fill_inet_sockaddr(&sockaddr, #3, #4, #5, #6, #7);
##if (MSG_NOSIGNAL == 0) && defined(SO_NOSIGPIPE)
	{
		int sockopt = #c;
		setsockopt(#0,SOL_SOCKET,SO_NOSIGPIPE,
			   wincoerce(char *,&sockopt),
			   sizeof(int));
	}
##endif
        len = sendto(sock, wincoerce(char *,buffer),
                     length, flags,(struct sockaddr*)&sockaddr, 
                     sizeof(struct sockaddr_in));
	ecl_enable_interrupts();
        @(return) = len;
}
"
		     :one-liner nil))
	       (ffi:c-inline #+clasp ll-socket-send-no-address
			 (fd buffer length 
			     oob eor dontroute dontwait nosignal confirm)
		     (:int :object :int
			   :bool :bool :bool :bool :bool :bool)
		     :long
		     "
{
	int sock = #0;
	int length = #2;
	void *buffer = safe_buffer_pointer(#1, length);
        int flags = ( #3 ? MSG_OOB : 0 )  |
                    ( #4 ? MSG_EOR : 0 ) |
                    ( #5 ? MSG_DONTROUTE : 0 ) |
                    ( #6 ? MSG_DONTWAIT : 0 ) |
                    ( #7 ? MSG_NOSIGNAL : 0 ) |
                    ( #8 ? MSG_CONFIRM : 0 );
        cl_type type = type_of(#1);
        ssize_t len;
	ecl_disable_interrupts();
##if (MSG_NOSIGNAL == 0) && defined(SO_NOSIGPIPE)
	{
		int sockopt = #7;
		setsockopt(#0,SOL_SOCKET,SO_NOSIGPIPE,
			   wincoerce(char *,&sockopt),
			   sizeof(int));
	}
##endif
	len = send(sock, wincoerce(char *, buffer), length, flags);
	ecl_enable_interrupts();
        @(return) = len;
}
"
		     :one-liner nil))))
      (if (= len-sent -1)
	  (socket-error "send")
	  len-sent))))

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
	   (ffi:c-inline #+clasp ll-socket-bind-local-socket
		     (fd name family) (:int :cstring :int) :int
		     "
{
        struct sockaddr_un sockaddr;
	size_t size;
	int output;
##ifdef BSD
        sockaddr.sun_len = sizeof(struct sockaddr_un);
##endif
        sockaddr.sun_family = #2;
        strncpy(sockaddr.sun_path,#1,sizeof(sockaddr.sun_path));
	sockaddr.sun_path[sizeof(sockaddr.sun_path)-1] = '\0';

	ecl_disable_interrupts();
	output = bind(#0,(struct sockaddr*)&sockaddr, sizeof(struct sockaddr_un));
	ecl_enable_interrupts();

        @(return) = output;
}"))
	(socket-error "bind"))))

(defmethod socket-accept ((socket local-socket))
  (multiple-value-bind (fd name)
      (ffi:c-inline #+clasp ll-socket-accept-local-socket
		((socket-file-descriptor socket)) (:int) (values :int :object)
"{
        struct sockaddr_un sockaddr;
        socklen_t addr_len = (socklen_t)sizeof(struct sockaddr_un);
        int new_fd;
	ecl_disable_interrupts();
	new_fd = accept(#0, (struct sockaddr *)&sockaddr, &addr_len);
	ecl_enable_interrupts();
	@(return 0) = new_fd;
	@(return 1) = (new_fd == -1) ? ECL_NIL : make_base_string_copy(sockaddr.sun_path);
}")
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
	   (ffi:c-inline #+clasp ll-socket-connect-local-socket
		     (fd family path) (:int :int :cstring) :int
		     "
{
        struct sockaddr_un sockaddr;
	int output;
##ifdef BSD
        sockaddr.sun_len = sizeof(struct sockaddr_un);
##endif
        sockaddr.sun_family = #1;
        strncpy(sockaddr.sun_path,#2,sizeof(sockaddr.sun_path));
	sockaddr.sun_path[sizeof(sockaddr.sun_path)-1] = '\0';

	ecl_disable_interrupts();
	output = connect(#0,(struct sockaddr*)&sockaddr, sizeof(struct sockaddr_un));
	ecl_enable_interrupts();

        @(return) = output;
}"))
	(socket-error "connect"))))

(defmethod socket-peername ((socket local-socket))
  (let* ((fd (socket-file-descriptor socket))
	 (peer (ffi:c-inline #+clasp ll-socket-peername-local-socket
			 (fd) (:int) t
			 "
{
        struct sockaddr_un name;
        socklen_t len = sizeof(struct sockaddr_un);
        int ret;

	ecl_disable_interrupts();
	ret = getpeername(#0,(struct sockaddr*)&name,&len);
	ecl_enable_interrupts();

        if (ret == 0) {
                @(return) = make_base_string_copy(name.sun_path);
        } else {
                @(return) = ECL_NIL;
        }
}")))
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
    (not (zerop (ffi:c-inline #+clasp ll-non-blocking-mode
			  (fd) (:int) :int "fcntl(#0,F_GETFL,NULL)&O_NONBLOCK" :one-liner t))))
)

(defmethod (setf non-blocking-mode) (non-blocking-p (socket socket))
  (let ((fd (socket-file-descriptor socket))
	(nblock (if non-blocking-p 1 0)))
    (if (= -1 (ffi:c-inline #+clasp ll-setf-non-blocking-mode
			(fd nblock) (:int :int) :int
	      #-:wsock
	      "
{
        int oldflags = fcntl(#0,F_GETFL,NULL);
        int newflags = (oldflags & ~O_NONBLOCK) |
                       (#1 ? O_NONBLOCK : 0);
	ecl_disable_interrupts();
        @(return) = fcntl(#0,F_SETFL,newflags);
	ecl_enable_interrupts();
}"))
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
  (ffi:c-inline #+clasp ll-dup
		(fd) (:int) :int "dup(#0)" :one-liner t))

(defun make-stream-from-fd (fd mode &key buffering element-type (external-format :default)
                            (name "FD-STREAM"))
  (assert (stringp name) (name) "name must be a string.")
  (let* ((smm-mode (ecase mode
		       (:input +clasp-stream-mode-input+ #|(c-constant "ecl_smm_input")|# )
		       (:output +clasp-stream-mode-output+ #| (c-constant "ecl_smm_output") |#)
		       (:input-output +clasp-stream-mode-io+ #|(c-constant "ecl_smm_io")|#)
		       ))
	 (external-format (unless (subtypep element-type 'integer) external-format))
         (stream (ffi:c-inline #+clasp ll-make-stream-from-fd
			   (name fd smm-mode element-type external-format)
			   (t :int :int t t)
			   t
			   "
ecl_make_stream_from_fd(#0,#1,(enum ecl_smmode)#2,
			ecl_normalize_stream_element_type(#3),
                        0,#4)"
			   :one-liner t)))
    (when buffering
      (si::set-buffering-mode stream buffering))
    stream))

(defun auto-close-two-way-stream (stream)
  (declare (si::c-local))
  (ffi:c-inline #+clasp ll-auto-close-two-way-stream
		(stream) (t) :void
                "(#0)->stream.flags |= ECL_STREAM_CLOSE_COMPONENTS"
                :one-liner t))

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
		       (ffi:c-inline #+clasp ll-strerror
				 (num) (:int) :cstring
				 "strerror(#0)" :one-liner t)))))
  (:documentation "Common base class of socket related conditions."))

(defmacro define-socket-condition (symbol name)
  `(let () ; Prevents evaluation of constant value at compilation time
;;     (defconstant ,symbol (c-constant ,(symbol-name symbol)))
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
  (if (= *name-service-errno* (c-constant +NETDB-INTERNAL+))
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
;;     (defconstant ,symbol (c-constant ,(symbol-name symbol)))
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
  (ffi:c-inline #+clasp ll-strerror (num) (:int) :cstring "strerror(#0)" :one-liner t)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SOCKET OPTIONS
;;;

(defun get-sockopt-int (fd level const)
  (let ((ret (ffi:c-inline #+clasp sockets-internal:ll-get-sockopt-int
		       (fd level const) (:int :int :int) t
"{
        int sockopt, ret;
        socklen_t socklen = sizeof(int);

	ecl_disable_interrupts();
	ret = getsockopt(#0,#1,#2,wincoerce(char*,&sockopt),&socklen);
	ecl_enable_interrupts();

        @(return) = (ret == 0) ? ecl_make_integer(sockopt) : ECL_NIL;
}")))
    (if ret
	ret
	(error "Sockopt error: ~A" (ffi:c-inline #+clasp ll-strerror-errno
					     () () :cstring "strerror(errno)" :one-liner t)))))

(defun get-sockopt-bool (fd level const)
  (let ((ret (ffi:c-inline #+clasp sockets-internal:ll-get-sockopt-bool
		       (fd level const) (:int :int :int) t
"{
        int sockopt, ret;
        socklen_t socklen = sizeof(int);

	ecl_disable_interrupts();
	ret = getsockopt(#0,#1,#2,wincoerce(char*,&sockopt),&socklen);
	ecl_enable_interrupts();

        @(return) = (ret == 0) ? ecl_make_integer(sockopt) : ECL_NIL;
}")))
    (if ret
	(/= ret 0)
	(error "Sockopt error: ~A" (ffi:c-inline #+clasp ll-strerror-errno
					     () () :cstring "strerror(errno)" :one-liner t)))))


#-wsock
(defun get-sockopt-timeval (fd level const)
  (let ((ret (ffi:c-inline #+clasp sockets-internal:ll-get-sockopt-timeval
		       (fd level const) (:int :int :int) t
"{
	struct timeval tv;
        socklen_t socklen = sizeof(struct timeval);
        int ret;

	ecl_disable_interrupts();
	ret = getsockopt(#0,#1,#2,wincoerce(char*,&tv),&socklen);
	ecl_enable_interrupts();

        @(return) = (ret == 0) ? ecl_make_doublefloat((double)tv.tv_sec
					+ ((double)tv.tv_usec) / 1000000.0) : ECL_NIL;
}")))
    (if ret
	ret
	(error "Sockopt error: ~A" (ffi:c-inline #+clasp ll-strerror-errno
					     () () :cstring "strerror(errno)" :one-liner t)))))

(defun get-sockopt-linger (fd level const)
  (let ((ret (ffi:c-inline #+clasp sockets-internal:ll-get-sockopt-linger
		       (fd level const) (:int :int :int) t
"{
	struct linger sockopt;
	socklen_t socklen = sizeof(struct linger);
	int ret;

	ecl_disable_interrupts();
	ret = getsockopt(#0,#1,#2,wincoerce(char*,&sockopt),&socklen);
	ecl_enable_interrupts();

	@(return) = (ret == 0) ? ecl_make_integer((sockopt.l_onoff != 0) ? sockopt.l_linger : 0) : ECL_NIL;
}")))
    (if ret
	ret
	(error "Sockopt error: ~A" (ffi:c-inline #+clasp ll-strerror-errno
					     () () :cstring "strerror(errno)" :one-liner t)))))

(defun set-sockopt-int (fd level const value)
  (let ((ret (ffi:c-inline #+clasp ll-set-sockopt-int
		       (fd level const value) (:int :int :int :int) t
"{
        int sockopt = #3;
        int ret;

	ecl_disable_interrupts();
	ret = setsockopt(#0,#1,#2,wincoerce(char *,&sockopt),sizeof(int));
	ecl_enable_interrupts();

        @(return) = (ret == 0) ? ECL_T : ECL_NIL;
}")))
    (if ret
	value
	(error "Sockopt error: ~A" (ffi:c-inline #+clasp ll-strerror-errno
					     () () :cstring "strerror(errno)" :one-liner t)))))

(defun set-sockopt-bool (fd level const value)
  (let ((ret (ffi:c-inline #+clasp ll-set-sockopt-bool
		       (fd level const value) (:int :int :int :object) t
"{
        int sockopt = (#3 == ECL_NIL) ? 0 : 1;
        int ret;

	ecl_disable_interrupts();
	ret = setsockopt(#0,#1,#2,wincoerce(char *,&sockopt),sizeof(int));
	ecl_enable_interrupts();

        @(return) = (ret == 0) ? ECL_T : ECL_NIL;
}")))
    (if ret
	value
	(error "Sockopt error: ~A" (ffi:c-inline #+clasp ll-strerror-errno
					     () () :cstring "strerror(errno)" :one-liner t)))))

#-wsock
(defun set-sockopt-timeval (fd level const value)
  (let ((ret (ffi:c-inline #+clasp ll-set-sockopt-timeval
		       (fd level const value) (:int :int :int :double) t
"{
	struct timeval tv;
	double tmp = #3;
	int ret;

	ecl_disable_interrupts();
	tv.tv_sec = (int)tmp;
	tv.tv_usec = (int)((tmp-floor(tmp))*1000000.0);
        ret = setsockopt(#0,#1,#2,&tv,sizeof(struct timeval));
	ecl_enable_interrupts();

        @(return) = (ret == 0) ? ECL_T : ECL_NIL;
}")))
    (if ret
	value
	(error "Sockopt error: ~A" (ffi:c-inline #+clasp ll-strerror-errno
					     () () :cstring "strerror(errno)" :one-liner t)))))


(defun set-sockopt-linger (fd level const value)
  (let ((ret (ffi:c-inline #+clasp ll-set-sockopt-linger
		       (fd level const value) (:int :int :int :int) t
"{
	struct linger sockopt = {0, 0};
	int value = #3;
	int ret;

	if (value > 0) {
		sockopt.l_onoff = 1;
		sockopt.l_linger = value;
	}

	ecl_disable_interrupts();
	ret = setsockopt(#0,#1,#2,wincoerce(char *,&sockopt),
			 sizeof(struct linger));
	ecl_enable_interrupts();

	@(return) = (ret == 0) ? ECL_T : ECL_NIL;
}")))
    (if ret
	value
	(error "Sockopt error: ~A" (ffi:c-inline #+clasp ll-strerror-errno
					     () () :cstring "strerror(errno)" :one-liner t)))))

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
