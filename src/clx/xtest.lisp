;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Implementation of the XTest extension as described by
;;; http://www.x.org/docs/Xext/xtest.pdf
;;;
;;; Written by Lionel Flandrin <lionel.flandrin@gmail.com> in july
;;; 2008 and placed in the public domain.
;;;
;;; TODO:
;;; * Implement XTestSetVisualIDOfVisual and XTestDiscard
;;; * Add the missing (declare (type ...

(defpackage :xtest
  (:use :common-lisp :xlib)
  (:import-from :xlib
                #:data
                #:card8
                #:card8-get
                #:card16
                #:card16-get
                #:card32
                #:card32-get
                #:extension-opcode
                #:define-extension
                #:gcontext
                #:resource-id
                #:window-id
                #:cursor
                #:make-cursor
                #:with-buffer-request-and-reply
                #:with-buffer-request
                #:display)
  (:export
   ;; Constants
   #:+major-version+
   #:+minor-version+

   ;; Functions
   #:set-gc-context-of-gc
   #:get-version
   #:compare-cursor
   #:fake-motion-event
   #:fake-button-event
   #:fake-key-event
   #:grab-control))

(in-package :xtest)

(define-extension "XTEST")

(defmacro opcode (display)
  `(extension-opcode ,display "XTEST"))

;;; The version we implement
(defconstant +major-version+ 2)
(defconstant +minor-version+ 2)

(defconstant +none+           0)
(defconstant +current-cursor+ 1)

;;; XTest opcodes
(defconstant +get-version+    0)
(defconstant +compare-cursor+ 1)
(defconstant +fake-input+     2)
(defconstant +grab-control+   3)

;;; Fake events
(defconstant +fake-key-press+      2)
(defconstant +fake-key-release+    3)
(defconstant +fake-button-press+   4)
(defconstant +fake-button-release+ 5)
(defconstant +fake-motion-notify+  6)

;;; Client operations
(defun set-gc-context-of-gc (gcontext gcontext-id)
  (declare (type gcontext gcontext)
           (type resource-id gcontext-id))
    (setf (gcontext-id gcontext) gcontext-id))

;;; Server requests
(defun get-version (display &optional (major +major-version+) (minor +minor-version+))
  "Returns the major and minor version of the server's XTest implementation"
  (declare (type display display))
  (with-buffer-request-and-reply (display (opcode display) nil)
      ((data +get-version+)
       (card8 major)
       (card16 minor))
    (values (card8-get 1)
            (card16-get 8))))

(defun compare-cursor (display window &optional (cursor-id +current-cursor+))
  (declare (type display display)
           (type resource-id cursor-id)
           (type window window))
  (with-buffer-request-and-reply (display (opcode display) nil)
      ((data +compare-cursor+)
       (resource-id (window-id window))
       (resource-id cursor-id))
    (values (card8-get 1))))

(defun fake-motion-event (display x y &key (delay 0) relative (root-window-id 0))
  "Move the mouse pointer at coordinates (x, y). If :relative is t,
the movement is relative to the pointer's current position"
  (declare (type display display))
  (with-buffer-request (display (opcode display))
    (data +fake-input+)
    (card8 +fake-motion-notify+)
    (card8  (if relative 1 0))
    (pad16 0)
    (card32 delay)
    (card32 root-window-id)
    (pad32 0 0)
    (card16 x)
    (card16 y)
    (pad32 0 0)))

(defun fake-button-event (display button pressed &key (delay 0))
  "Send a fake button event (button pressed or released) to the
server. Most of the time, button 1 is the left one, 2 the middle and 3
the right one but it's not always the case."
  (declare (type display display))
  (with-buffer-request (display (opcode display))
    (data +fake-input+)
    (card8 (if pressed +fake-button-press+ +fake-button-release+))
    (card8 button)
    (pad16 0)
    (card32 delay)
    (pad32 0 0 0 0 0 0)))

(defun fake-key-event (display keycode pressed &key (delay 0))
  "Send a fake key event (key pressed or released) to the server based
on its keycode."
  (declare (type display display))
  (with-buffer-request (display (opcode display))
    (data +fake-input+)
    (card8 (if pressed +fake-key-press+ +fake-key-release+))
    (card8 keycode)
    (pad16 0)
    (card32 delay)
    (pad32 0 0 0 0 0 0)))

(defun grab-control (display grab?)
  "Make the client grab the server, that is allow it to make requests
even when another client grabs the server."
  (declare (type display display))
  (with-buffer-request (display (opcode display))
    (data +grab-control+)
    (card8 (if grab? 1 0))
    (pad8  0)
    (pad16 0)))

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; End:
