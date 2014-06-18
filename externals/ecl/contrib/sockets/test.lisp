;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;; $Id$

;; This file is based on SBCL's SB-BSD-SOCKET module and has been
;; heavily modified to work with ECL by Julian Stecklina.
;; Port to Windows Sockets contributed by M. Goffioul.

;; You may do whatever you want with this file. (PUBLIC DOMAIN)

;; Trivial stuff is copied from SBCL's SB-BSD-SOCKETS, which is also
;; in the public domain.

(in-package :cl-user)

(load "sys:sockets")
(load "../rt/rt")

(use-package :sb-bsd-sockets)
(use-package :sb-rt)

;;; a real address
(deftest make-inet-address
  (equalp (make-inet-address "127.0.0.1")  #(127 0 0 1))
  t)
;;; and an address with bit 8 set on some octets
(deftest make-inet-address2
  (equalp (make-inet-address "242.1.211.3")  #(242 1 211 3))
  t)

(deftest make-inet-socket
  ;; make a socket
  (let ((s (make-instance 'inet-socket :type :stream :protocol (get-protocol-by-name "tcp"))))
    (and (> (socket-file-descriptor s) 1) t))
  t)

(deftest make-inet-socket-keyword
    ;; make a socket
    (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
      (and (> (socket-file-descriptor s) 1) t))
  t)

(deftest make-inet-socket-wrong
    ;; fail to make a socket: check correct error return.  There's no nice
    ;; way to check the condition stuff on its own, which is a shame
    (handler-case
	(make-instance 'inet-socket :type :stream :protocol (get-protocol-by-name "udp"))
      ((or socket-type-not-supported-error protocol-not-supported-error) (c)
	(declare (ignorable c)) t)
      (:no-error nil))
  t)

(deftest make-inet-socket-keyword-wrong
    ;; same again with keywords
    (handler-case
	(make-instance 'inet-socket :type :stream :protocol :udp)
      ((or protocol-not-supported-error socket-type-not-supported-error) (c)
	(declare (ignorable c)) t)
      (:no-error nil))
  t)


(deftest non-block-socket
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (setf (non-blocking-mode s) t)
    (non-blocking-mode s))
  t)

(defun do-gc-portably ()
  ;; cmucl on linux has generational gc with a keyword argument,
  ;; sbcl GC function takes same arguments no matter what collector is in
  ;; use
  #+(or sbcl gencgc) (SB-EXT:gc :full t)
  #+ecl (ext:gc t)
  ;; other platforms have full gc or nothing
  #-(or sbcl gencgc ecl) (sb-ext:gc))

(deftest inet-socket-bind
  (let ((s (make-instance 'inet-socket :type :stream :protocol (get-protocol-by-name "tcp"))))
    ;; Given the functions we've got so far, if you can think of a
    ;; better way to make sure the bind succeeded than trying it
    ;; twice, let me know
    ;; 1974 has no special significance, unless you're the same age as me
    (do-gc-portably) ;gc should clear out any old sockets bound to this port
    (socket-bind s (make-inet-address "127.0.0.1") 1974)
    (handler-case
	(let ((s2 (make-instance 'inet-socket :type :stream :protocol (get-protocol-by-name "tcp"))))
	  (socket-bind s2 (make-inet-address "127.0.0.1") 1974)
	  nil)
      (address-in-use-error () t)))
  t)

(deftest simple-sockopt-test
  ;; test we can set SO_REUSEADDR on a socket and retrieve it, and in
  ;; the process that all the weird macros in sockopt happened right.
  (let ((s (make-instance 'inet-socket :type :stream :protocol (get-protocol-by-name "tcp"))))
    (setf (sockopt-reuse-address s) t)
    (sockopt-reuse-address s))
  t)

(defun read-buf-nonblock (buffer stream)
  "Like READ-SEQUENCE, but returns early if the full quantity of data isn't there to be read.  Blocks if no input at all"
  (let ((eof (gensym)))
    (do ((i 0 (1+ i))
         (c (read-char stream nil eof)
            (read-char-no-hang stream nil eof)))
        ((or (>= i (length buffer)) (not c) (eq c eof)) i)
      (setf (elt buffer i) c))))

(deftest name-service-return-type
  (vectorp (host-ent-address (get-host-by-address #(127 0 0 1))))
  t)

;;; these require that the echo services are turned on in inetd
(deftest simple-tcp-client
    (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp))
	  (data (make-string 200)))
      (socket-connect s #(127 0 0 1) 7)
      (let ((stream (socket-make-stream s :input t :output t :buffering :none)))
	(format stream "here is some text")
	(let ((data (subseq data 0 (read-buf-nonblock data stream))))
	  (format t "~&Got ~S back from TCP echo server~%" data)
	  (> (length data) 0))))
  t)

(deftest sockaddr-return-type
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (unwind-protect 
	 (progn
	   (socket-connect s #(127 0 0 1) 7)
	   (multiple-value-bind (host port) (socket-peername s)
	     (and (vectorp host)
		  (numberp port))))
      (socket-close s)))
  t)

(deftest simple-udp-client
  (let ((s (make-instance 'inet-socket :type :datagram :protocol (get-protocol-by-name "udp")))
        (data (make-string 200)))
    (format t "Socket type is ~A~%" (sockopt-type s))
    (socket-connect s #(127 0 0 1) 7)
    (let ((stream (socket-make-stream s :input t :output t :buffering :none)))
      (format stream "here is some text")
      (finish-output stream)
      (let ((data (subseq data 0 (read-buf-nonblock data stream))))
	(format t "~&Got ~S back from UDP echo server~%" data)
	(> (length data) 0))))
  t)

;;; A fairly rudimentary test that connects to the syslog socket and
;;; sends a message.  Priority 7 is kern.debug; you'll probably want
;;; to look at /etc/syslog.conf or local equivalent to find out where
;;; the message ended up
(deftest simple-local-client
    (progn
      ;; SunOS (Solaris) and Darwin systems don't have a socket at
      ;; /dev/log.  We might also be building in a chroot or
      ;; something, so don't fail this test just because the file is
      ;; unavailable, or if it's a symlink to some weird character
      ;; device.
      (when (and (probe-file "/dev/log")
		 #-ecl
		 (sb-posix:s-issock
		  (sb-posix::stat-mode (sb-posix:stat "/dev/log"))))
	(let ((s (make-instance 'local-socket :type :datagram)))
	  (format t "Connecting ~A... " s)
	  (finish-output)
	  (handler-case
	      (socket-connect s "/dev/log")
	    (socket-error ()
	      (setq s (make-instance 'local-socket :type :stream))
	      (format t "failed~%Retrying with ~A... " s)
	      (finish-output)
	      (socket-connect s "/dev/log")))
	  (format t "ok.~%")
	  (let ((stream (socket-make-stream s :input t :output t :buffering :none)))
	    (format stream
		    "<7>sb-bsd-sockets: Don't panic.  We're testing local-domain client code; this message can safely be ignored"))))
      t)
  t)


;;; these require that the internet (or bits of it, at least) is available

(deftest get-host-by-name
  (equalp (car (host-ent-addresses (get-host-by-name "a.root-servers.net")))
          #(198 41 0 4))
  t)

(deftest get-host-by-address
    (host-ent-name (get-host-by-address #(198 41 0 4)))
  "a.root-servers.net")

(deftest get-host-by-name-wrong
  (handler-case
   (get-host-by-name "foo.tninkpad.telent.net")
   (NAME-SERVICE-ERROR () t)
   (:no-error nil))
  t)

(defun http-stream (host port request)
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (socket-connect
     s (car (host-ent-addresses (get-host-by-name host))) port)
    (let ((stream (socket-make-stream s :input t :output t :buffering :none)))
      (format stream "~A HTTP/1.0~%~%" request))
    s))

(deftest simple-http-client-1
    (handler-case
	(let ((s (http-stream "ww.telent.net" 80 "HEAD /")))
	  (let ((data (make-string 200)))
	    (setf data (subseq data 0
			       (read-buf-nonblock data
						  (socket-make-stream s))))
	    (princ data)
	    (> (length data) 0)))
      (network-unreachable-error () 'network-unreachable))
  t)


(deftest sockopt-receive-buffer
    ;; on Linux x86, the receive buffer size appears to be doubled in the
    ;; kernel: we set a size of x and then getsockopt() returns 2x.
    ;; This is why we compare with >= instead of =
    (handler-case
	(let ((s (http-stream "ww.telent.net" 80 "HEAD /")))
	  (setf (sockopt-receive-buffer s) 1975)
	  (let ((data (make-string 200)))
	    (setf data (subseq data 0
			       (read-buf-nonblock data
						  (socket-make-stream s))))
	    (and (> (length data) 0)
		 (>= (sockopt-receive-buffer s) 1975))))
      (network-unreachable-error () 'network-unreachable))
  t)


;;; we don't have an automatic test for some of this yet.  There's no
;;; simple way to run servers and have something automatically connect
;;; to them as client, unless we spawn external programs.  Then we
;;; have to start telling people what external programs they should
;;; have installed.  Which, eventually, we will, but not just yet


;;; to check with this: can display packets from multiple peers
;;; peer address is shown correctly for each packet
;;; packet length is correct
;;; long (>500 byte) packets have the full length shown (doesn't work)

(defun udp-server (port)
  (let ((s (make-instance 'inet-socket :type :datagram :protocol :udp)))
    (socket-bind s #(0 0 0 0) port)
    (loop
     (multiple-value-bind (buf len address port) (socket-receive s nil 500)
       (format t "Received ~A bytes from ~A:~A - ~A ~%"
	       len address port (subseq buf 0 (min 10 len)))))))
