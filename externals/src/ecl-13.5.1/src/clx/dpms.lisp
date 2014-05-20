
;;;; Original Author: Matthew Kennedy <mkennedy@gentoo.org>
;;;;
;;;; Documentation strings derived from DPMS.txt distributed with the Xorg X11
;;;; server implementation.  DPMS.txt contains the following copyright:
;;;;
;;;;  Copyright (C) Digital Equipment Corporation, 1996
;;;;
;;;;  Permission to use, copy, modify, distribute, and sell this documentation
;;;;  for any purpose is hereby granted without fee, provided that the above
;;;;  copyright notice and this permission notice appear in all copies.  Digital
;;;;  Equipment Corporation makes no representations about the suitability for
;;;;  any purpose of the information in this document.  This documentation is
;;;;  provided ``as is'' without express or implied warranty.

(defpackage :dpms
  (:use :common-lisp)
  (:import-from :xlib
                "DEFINE-EXTENSION"
                "DISPLAY"
                "WITH-BUFFER-REQUEST-AND-REPLY"
                "WITH-BUFFER-REQUEST"
                "EXTENSION-OPCODE"
                "CARD8-GET"
                "CARD16-GET"
                "BOOLEAN-GET"
                "CARD8"
                "CARD16"
                "DATA")
  (:export "DPMS-GET-VERSION"
           "DPMS-CAPABLE"
           "DPMS-GET-TIMEOUTS"
           "DPMS-SET-TIMEOUTS"
           "DPMS-ENABLE"
           "DPMS-DISABLE"
           "DPMS-FORCE-LEVEL"
           "DPMS-INFO"))

(in-package :dpms)

(define-extension "DPMS")

(defmacro dpms-opcode (display)
  `(extension-opcode ,display "DPMS"))

(defconstant +get-version+  0)
(defconstant +capable+      1)
(defconstant +get-timeouts+ 2)
(defconstant +set-timeouts+ 3)
(defconstant +enable+       4)
(defconstant +disable+      5)
(defconstant +force-level+  6)
(defconstant +info+         7)

(defun dpms-get-version (display &optional (major-version 1) (minor-version 1))
  "Return two values: the major and minor version of the DPMS
implementation the server supports.

If supplied, the MAJOR-VERSION and MINOR-VERSION indicate what
version of the protocol the client wants the server to implement."
  (declare (type display display))
  (with-buffer-request-and-reply (display (dpms-opcode display) nil)
      ((data +get-version+)
       (card16 major-version)
       (card16 minor-version))
    (values (card16-get 8)
            (card16-get 10))))

(defun dpms-capable (display)
  "True if the currently running server's devices are capable of
DPMS operations.  

The truth value of this request is implementation defined, but is
generally based on the capabilities of the graphic card and
monitor combination.  Also, the return value in the case of
heterogeneous multi-head servers is implementation defined."
  (declare (type display display))
  (with-buffer-request-and-reply (display (dpms-opcode display) nil)
      ((data +capable+))
    (boolean-get 8)))

(defun dpms-get-timeouts (display)
  "Return three values: the current values of the DPMS timeout
values.  The timeout values are (in order returned): standby,
suspend and off.  All values are in units of seconds.  A value of
zero for any timeout value indicates that the mode is disabled."
  (declare (type display display))
  (with-buffer-request-and-reply (display (dpms-opcode display) nil)
      ((data +get-timeouts+))
    (values (card16-get 8)
            (card16-get 10)
            (card16-get 12))))

(defun dpms-set-timeouts (display standby suspend off)
  "Set the values of the DPMS timeouts.  All values are in units
of seconds.  A value of zero for any timeout value disables that
mode."
  (declare (type display display))
  (with-buffer-request (display (dpms-opcode display))
    (data +set-timeouts+)
    (card16 standby)
    (card16 suspend)
    (card16 off)
    (card16 0))                         ;unused
  (values))

(defun dpms-enable (display)
  "Enable the DPMS characteristics of the server using the
server's currently stored timeouts.  If DPMS is already enabled,
no change is affected."
  (declare (type display display))
  (with-buffer-request (display (dpms-opcode display))
    (data +enable+))
  (values))

(defun dpms-disable (display)
  "Disable the DPMS characteristics of the server.  It does not
affect the core or extension screen savers.  If DPMS is already
disabled, no change is effected.

This request is provided so that DPMS may be disabled without
damaging the server's stored timeout values."
  (declare (type display display))
  (with-buffer-request (display (dpms-opcode display))
    ((data +disable+)))
  (values))

(defun dpms-force-level (display power-level)
  "Forces a specific DPMS level on the server.  Valid keyword
values for POWER-LEVEL are: DPMS-MODE-ON, DPMS-MODE-STANDBY,
DPMS-MODE-SUSPEND and DPMS-MODE-OFF."
  (declare (type display display))
  (with-buffer-request (display (dpms-opcode display))
    (data +force-level+)
    (card16 (ecase power-level
              (:dpms-mode-on 0)
              (:dpms-mode-standby 1)
              (:dpms-mode-suspend 2)
              (:dpms-mode-off 3)))
    (card16 0))                         ;unused
  (values))

(defun dpms-info (display)
  "Returns two valus: the DPMS power-level and state value for the display.

State is one of the keywords DPMS-ENABLED or DPMS-DISABLED.

If state is DPMS-ENABLED, then power level is returned as one of
the keywords DPMS-MODE-ON, DPMS-MODE-STANDBY, DPMS-MODE-SUSPEND
or DPMS-MODE-OFF.  If state is DPMS-DISABLED, then power-level is
undefined and returned as NIL."
  (declare (type display display))
  (with-buffer-request-and-reply (display (dpms-opcode display) nil)
      ((data +info+))
    (let ((state (if (boolean-get 10)
                     :dpms-enabled
                     :dpms-disabled)))
      (values (unless (eq state :dpms-disabled)
                (ecase (card16-get 8)
                  (0 :dpms-mode-on)
                  (1 :dpms-mode-standby)
                  (2 :dpms-mode-suspend)
                  (3 :dpms-mode-off)))
              state))))

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; End:
