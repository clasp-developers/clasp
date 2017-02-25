;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;; $Id$

;; This file is based on SBCL's SB-BSD-SOCKET module and has been
;; heavily modified to work with ECL by Julian Stecklina.
;; Port to Windows Sockets contributed by M. Goffioul.

;; You may do whatever you want with this file. (PUBLIC DOMAIN)

;; Trivial stuff is copied from SBCL's SB-BSD-SOCKETS, which is also
;; in the public domain.

(defpackage "SB-BSD-SOCKETS"
  (:use "CL" "CORE")
  (:export "GET-HOST-BY-NAME" "GET-HOST-BY-ADDRESS"
           "SOCKET-BIND" "SOCKET-ACCEPT" "SOCKET-CONNECT"
           "SOCKET-PEERNAME" "SOCKET-NAME" "SOCKET-LISTEN"
           "SOCKET-RECEIVE" "SOCKET-CLOSE" "SOCKET-MAKE-STREAM"
           "GET-PROTOCOL-BY-NAME" "MAKE-INET-ADDRESS" "LOCAL-SOCKET"
           "SOCKET" "INET-SOCKET" "SOCKET-FILE-DESCRIPTOR" #+:win32 "NAMED-PIPE-SOCKET"
           "SOCKET-FAMILY" "SOCKET-PROTOCOL" "SOCKET-TYPE"
           "SOCKET-ERROR" "NAME-SERVICE-ERROR" "NON-BLOCKING-MODE"
           "HOST-ENT-NAME" "HOST-ENT-ALIASES" "HOST-ENT-ADDRESS-TYPE"
           "HOST-ENT-ADDRESSES" "HOST-ENT" "HOST-ENT-ADDRESS" "SOCKET-SEND"))
