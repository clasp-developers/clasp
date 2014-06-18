;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:YES; Patch-file:T -*-

;;; CLX debugging code

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Copyright (C) 1987 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

;;; Created 04/09/87 14:30:41 by LaMott G. OREN

(in-package :xlib)

(export '(display-listen
	  readflush
	  check-buffer
	  check-finish
	  check-force
	  clear-next))

(defun display-listen (display)
  (listen (display-input-stream display)))

(defun readflush (display)
  ;; Flushes Display's input stream, returning what was there
  (let ((stream (display-input-stream display)))
    (loop while (listen stream) collect (read-byte stream))))

;;-----------------------------------------------------------------------------
;; The following are useful display-after functions

(defun check-buffer (display)
  ;; Ensure the output buffer in display is correct
  (with-buffer-output (display :length :none :sizes (8 16))
    (do* ((i 0 (+ i length))
	  request
	  length)
	 ((>= i buffer-boffset)
	  (unless (= i buffer-boffset)
	    (warn "Buffer size ~d  Requests end at ~d" buffer-boffset i)))
      
      (let ((buffer-boffset 0)
	    #+clx-overlapping-arrays
	    (buffer-woffset 0))
	(setq request (card8-get i))
	(setq length (* 4 (card16-get (+ i 2)))))
      (when (zerop request)
	(warn "Zero request in buffer")
	(return nil))
      (when (zerop length)
	(warn "Zero length in buffer")
	(return nil)))))

(defun check-finish (display)
  (check-buffer display)
  (display-finish-output display))

(defun check-force (display)
  (check-buffer display)
  (display-force-output display))

(defun clear-next (display)
  ;; Never append requests
  (setf (display-last-request display) nil))

;; End of file
