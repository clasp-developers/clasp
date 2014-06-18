;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;; CLX interface for Bezier Spline Extension.

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

(export 'draw-curves)

(define-extension "bezier")

(defun draw-curves (drawable gcontext points)
  ;; Draw Bezier splines on drawable using gcontext.
  ;; Points are a list of (x0 y0 x1 y1 x2 y2 x3 y3)
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type sequence points))
  (let* ((display (drawable-display drawable))
	 (opcode (extension-opcode display "bezier")))
    (with-buffer-request (display opcode :gc-force gcontext)
      ((data card8) 1) ;; X_PolyBezier - The minor_opcode for PolyBezier
      (drawable drawable)
      (gcontext gcontext)
      ((sequence :format int16) points))))
