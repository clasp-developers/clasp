;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;; CLX trapezoid Extension test program

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


(defun zoid-test ()
  ;; Display the part picture in /extensions/test/datafile
  (let* ((display (open-default-display))
	 (width 400)
	 (height 400)
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
	       :foreground white)))
    (initialize-extensions display)
    
    (map-window win)				; Map the window
    ;; Handle events
    (unwind-protect
	(loop
	  (event-case (display :force-output-p t)
	    (exposure  ;; Come here on exposure events
	      (window count)
	      (when (zerop count) ;; Ignore all but the last exposure event
		(clear-area window)
		;; NOT VERY INTERESTING, BUT CHECKS ALL THE POSSIBILITIES
		(draw-filled-trapezoids window gc  '(10 20 30 40 100 200))
		(setf (gcontext-trapezoid-alignment gc) :y)
		(draw-filled-trapezoids window gc  #(10 20 30 40 100 200))
		(with-gcontext (gc :trapezoid-alignment :x)
		  (draw-filled-trapezoids window gc  '(40 50 60 70 140 240)))
		(setf (gcontext-trapezoid-alignment gc) :x)
		(draw-filled-trapezoids window gc  #(40 50 60 70 80 90))
		(with-gcontext (gc :trapezoid-alignment :y)
		  (draw-filled-trapezoids window gc  #(40 50 60 70 140 240)))
		  
		(draw-glyphs window gc 10 10 "Press any key to exit")
		;; Returning non-nil causes event-case to exit
		t))
	    (key-press () (return-from zoid-test t))))
      (close-display display))))
