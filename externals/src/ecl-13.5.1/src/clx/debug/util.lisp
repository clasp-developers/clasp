;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:YES; -*-

;; CLX utilities

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

(export '(display-root
	  display-black
	  display-white
	  report-events
	  describe-window
	  describe-gc
	  degree
	  radian
	  display-refresh
	  root-tree
	  window-tree))

(defun display-root (display) (screen-root (display-default-screen display)))
(defun display-black (display) (screen-black-pixel (display-default-screen display)))
(defun display-white (display) (screen-white-pixel (display-default-screen display)))

(defun report-events (display)
  (loop
    (unless
      (process-event display :handler #'(lambda (&rest args) (print args)) :discard-p t :timeout 0.001)
      (return nil))))

(defun describe-window (window)
  (macrolet ((da (attribute &key (transform 'progn) (format "~s"))
	       (let ((func (intern (concatenate 'string (string 'window-)
						(string attribute)) 'xlib)))
		 `(format t "~%~22a ~?" ',attribute ,format (list (,transform (,func window))))))
	     (dg (attribute &key (transform 'progn) (format "~s"))
	       (let ((func (intern (concatenate 'string (string 'drawable-)
						(string attribute)) 'xlib)))
		 `(format t "~%~22a ~?" ',attribute ,format (list (,transform (,func window)))))))
    (with-state (window)
      (when (window-p window)
	(da visual :format "#x~x")
	(da class)
	(da gravity)
	(da bit-gravity)
	(da backing-store)
	(da backing-planes :format "#x~x")
	(da backing-pixel)
	(da save-under)
	(da colormap)
	(da colormap-installed-p)
	(da map-state)
	(da all-event-masks :transform make-event-keys :format "~{~<~%~1:;~s ~>~}")
	(da event-mask :transform make-event-keys :format "~{~<~%~1:;~s ~>~}")
	(da do-not-propagate-mask :transform make-event-keys :format "~{~<~%~1:;~s ~>~}")
	(da override-redirect)
	)
      (dg root)
      (dg depth)
      (dg x)
      (dg y)
      (dg width)
      (dg height)
      (dg border-width)

      )))
      
(defun describe-gc (gc)
  (macrolet ((dgc (name &key (transform 'progn) (format "~s"))
	       (let ((func (intern (concatenate 'string (string 'gcontext-)
						(string name)) 'xlib)))
		 `(format t "~%~22a ~?" ',name ,format (list (,transform (,func gc)))))))
    (dgc function)
    (dgc plane-mask)
    (dgc foreground)
    (dgc background)
    (dgc line-width)
    (dgc line-style)
    (dgc cap-style)
    (dgc join-style)
    (dgc fill-style)
    (dgc fill-rule)
    (dgc tile)
    (dgc stipple)
    (dgc ts-x)
    (dgc ts-y)
    (dgc font) ;; See below
    (dgc subwindow-mode)
    (dgc exposures)
    (dgc clip-x)
    (dgc clip-y)
;;    (dgc clip-ordering)
    (dgc clip-mask)
    (dgc dash-offset)
    (dgc dashes)
    (dgc arc-mode)
    ))

(defun degree (degrees)
  (* degrees (/ pi 180)))

(defun radian (radians)
  (round (* radians (/ 180 pi))))

(defun display-refresh (host)
  ;; Useful for when the system writes to the screen (sometimes scrolling!)
  (let ((display (open-display host)))
    (unwind-protect
	(let ((screen (display-default-screen display)))
	  (let ((win (create-window :parent (screen-root screen) :x 0 :y 0 :override-redirect :on
				    :width (screen-width screen) :height (screen-height screen)
				    :background (screen-black-pixel screen))))
	    (map-window win)
	    (display-finish-output display)
	    (unmap-window win)
	    (destroy-window win)
	    (display-finish-output display)))
      (close-display display))))

(defun root-tree (host)
  (let ((display (open-display host)))
    (unwind-protect
	(window-tree (screen-root (display-default-screen display)))
      (close-display display)))
  (values))

(defun window-tree (window &optional (depth 0))
  ;; Print the window tree and properties starting from WINDOW
  ;; Returns a list of windows in the order that they are printed.
  (declare (arglist window)
	   (type window window)
	   (values (list window)))
  (let ((props (mapcar #'(lambda (prop)
			   (multiple-value-bind (data type format)
			       (get-property window prop)
			     (case type
			       (:string (setq data (coerce data 'string))))
			     (list prop format type data)))
		       (list-properties window)))
	(result (list window)))
    (with-state (window)
      (format t "~%~v@t#x~x~20,20t X~3d Y~3d W~4d H~3d ~s" depth (window-id window)
	      (drawable-x window) (drawable-y window)
	      (drawable-width window) (drawable-height window)
	      (window-map-state window)))
    (dolist (prop props)
      (format t "~%~v@t~{~s ~}" (+ depth 2) prop))
    (dolist (w (query-tree window))
      (setq result (nconc result (window-tree w (+ depth 2)))))
    result))

