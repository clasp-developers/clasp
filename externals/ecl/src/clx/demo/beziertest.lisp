;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;; CLX Bezier Spline Extension demo program

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

(in-package :xlib)

(defun bezier-test (host &optional (pathname "/usr/X.V11R1/extensions/test/datafile"))
  ;; Display the part picture in /extensions/test/datafile
  (let* ((display (open-display host))
	 (width 800)
	 (height 800)
	 (screen (display-default-screen display))
	 (black (screen-black-pixel screen))
	 (white (screen-white-pixel screen))
	 (win (create-window
		:parent (screen-root screen)
		:background black
		:border white
		:border-width 1
		:colormap (screen-default-colormap screen)
		:bit-gravity :center
		:event-mask '(:exposure :key-press)
		:x 20 :y 20
		:width width :height height))
	 (gc (create-gcontext
	       :drawable win
	       :background black
	       :foreground white))
	 (lines (make-array (* 500 4) :fill-pointer 0 :element-type 'card16))
	 (curves (make-array (* 500 8) :fill-pointer 0 :element-type 'card16)))
    ;; Read the data
    (with-open-file (stream pathname)
      (loop 
	(case (read-char stream nil :eof)
	  (#\l (dotimes (i 4) (vector-push-extend (read stream) lines)))
	  (#\b (dotimes (i 8) (vector-push-extend (read stream) curves)))
	  ((#\space #\newline #\tab))
	  (otherwise (return)))))
    ;; The data points were created to fit in a 2048x2048 square,
    ;; this means scale_factor will always be small enough so that
    ;; we don't need to worry about overflows.
    (let ((factor (ash (min width height) 5)))
      (dotimes (i (length lines))
	(setf (aref lines i)
	      (ash (* (aref lines i) factor) -16)))
      (dotimes (i (length curves))
	(setf (aref curves i)
	      (ash (* (aref curves i) factor) -16))))
    
    (map-window win)				; Map the window
    ;; Handle events
    (unwind-protect
	(loop
	  (event-case (display :force-output-p t)
	    (exposure  ;; Come here on exposure events
	      (window count)
	      (when (zerop count) ;; Ignore all but the last exposure event
		(clear-area window)
		(draw-segments win gc lines)
		(draw-curves win gc curves)
		(draw-glyphs win gc 10 10 "Press any key to exit")
		;; Returning non-nil causes event-case to exit
		t))
	    (key-press () (return-from bezier-test t))))
      (close-display display))))
