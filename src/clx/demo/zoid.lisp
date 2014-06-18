;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;; CLX interface for Trapezoid Extension.

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

(export '(draw-filled-trapezoids
	   gcontext-trapezoid-alignment ;; Setf'able
	   ))

(define-extension "ZoidExtension")

(defun draw-filled-trapezoids (drawable gcontext points)
  ;; Draw trapezoids on drawable using gcontext.
  ;; Points are a list of either (y1 y2 y3 y4 x1 x2) ;; x-aligned
  ;;                      or     (x1 x2 x3 x4 y1 y2) ;; y-aligned
  ;; Alignment is determined by the GCONTEXT [see gcontext-trapezoid-alignment]
  ;; Alignment is set with the ALIGNMENT keyword argument, which may be
  ;; :X, :Y, or NIL (use previous alignment)
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type sequence points))
  (let* ((display (drawable-display drawable))
	 (opcode (extension-opcode display "ZoidExtension")))
    (with-buffer-request (display opcode :gc-force gcontext)
      ((data card8) 1) ;; X_PolyFillZoid
      (drawable drawable)
      (gcontext gcontext)
      ((sequence :format int16) points))))

(define-gcontext-accessor trapezoid-alignment :default :x
  :set-function set-trapezoid-alignment)

(defun set-trapezoid-alignment (gcontext alignment)
  (declare (type (member :x :y) alignment))
  (let* ((display (gcontext-display gcontext))
	 (opcode (extension-opcode display "ZoidExtension")))
    (with-buffer-request (display opcode)
      ((data card8) 2) ;; X_SetZoidAlignment
      (gcontext gcontext)
      ((member8 %error :x :y) alignment))))

