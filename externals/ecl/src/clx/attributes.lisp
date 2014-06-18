;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;;; Window Attributes

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

;;;	The special variable *window-attributes* is an alist containg:
;;;	(drawable attributes attribute-changes geometry geometry-changes)
;;;	Where DRAWABLE is the associated window or pixmap
;;;	      ATTRIBUTES is NIL or a reply-buffer containing the drawable's
;;;		         attributes for use by the accessors.
;;;	      ATTRIBUTE-CHANGES is NIL or an array.  The first element
;;;			 of the array is a "value-mask", indicating which
;;;			 attributes have changed.  The other elements are
;;;			 integers associated with the changed values, ready
;;;			 for insertion into a server request.
;;;	      GEOMETRY is like ATTRIBUTES, but for window geometry
;;;	      GEOMETRY-CHANGES is like ATTRIBUTE-CHANGES, but for window geometry
;;;
;;;	Attribute and Geometry accessors and SETF's look on the special variable
;;;	*window-attributes* for the drawable.  If its not there, the accessor is
;;;     NOT within a WITH-STATE, and a server request is made to get or put a value.
;;;     If an entry is found in *window-attributes*, the cache buffers are used
;;;	for the access.
;;;
;;;	All WITH-STATE has to do (re)bind *Window-attributes* to a list including
;;;	the new drawable.  The caches are initialized to NIL and allocated as needed.

(in-package :xlib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +attribute-size+ 44)
  (defconstant +geometry-size+ 24)
  (defconstant +context-size+ (max +attribute-size+ +geometry-size+ (* 16 4))))

(defvar *window-attributes* nil) ;; Bound to an alist of (drawable . state) within WITH-STATE

;; Window Attribute reply buffer resource
(defvar *context-free-list* nil) ;; resource of free reply buffers

(defun allocate-context ()
  (or (threaded-atomic-pop *context-free-list* reply-next reply-buffer)
      (make-reply-buffer +context-size+)))

(defun deallocate-context (context)
  (declare (type reply-buffer context))
  (threaded-atomic-push context *context-free-list* reply-next reply-buffer))

(defmacro state-attributes (state) `(second ,state))
(defmacro state-attribute-changes (state) `(third ,state))
(defmacro state-geometry (state) `(fourth ,state))
(defmacro state-geometry-changes (state) `(fifth ,state))
 
(defmacro drawable-equal-function ()
  ;; Since drawables are not always cached, we must use drawable-equal
  ;; to determine equality.
  ''drawable-equal)

(defmacro window-equal-function ()
  ;; Since windows are not always cached, we must use window-equal
  ;; to determine equality.
  ''window-equal)

(defmacro with-state ((drawable) &body body)
  ;; Allows a consistent view to be obtained of data returned by GetWindowAttributes
  ;; and GetGeometry, and allows a coherent update using ChangeWindowAttributes and
  ;; ConfigureWindow.  The body is not surrounded by a with-display.  Within the
  ;; indefinite scope of the body, on a per-process basis in a multi-process
  ;; environment, the first call within an Accessor Group on the specified drawable
  ;; (the object, not just the variable) causes the complete results of the protocol
  ;; request to be retained, and returned in any subsequent accessor calls.  Calls
  ;; within a Setf Group are delayed, and executed in a single request on exit from
  ;; the body.  In addition, if a call on a function within an Accessor Group follows
  ;; a call on a function in the corresponding Setf Group, then all delayed setfs for
  ;; that group are executed, any retained accessor information for that group is
  ;; discarded, the corresponding protocol request is (re)issued, and the results are
  ;; (again) retained, and returned in any subsequent accessor calls.

  ;; Accessor Group A (for GetWindowAttributes):
  ;; window-visual, window-visual-info, window-class, window-gravity, window-bit-gravity,
  ;; window-backing-store, window-backing-planes, window-backing-pixel,
  ;; window-save-under, window-colormap, window-colormap-installed-p,
  ;; window-map-state, window-all-event-masks, window-event-mask,
  ;; window-do-not-propagate-mask, window-override-redirect

  ;; Setf Group A (for ChangeWindowAttributes):
  ;; window-gravity, window-bit-gravity, window-backing-store, window-backing-planes,
  ;; window-backing-pixel, window-save-under, window-event-mask,
  ;; window-do-not-propagate-mask, window-override-redirect, window-colormap,
  ;; window-cursor

  ;; Accessor Group G (for GetGeometry):
  ;; drawable-root, drawable-depth, drawable-x, drawable-y, drawable-width,
  ;; drawable-height, drawable-border-width

  ;; Setf Group G (for ConfigureWindow):
  ;; drawable-x, drawable-y, drawable-width, drawable-height, drawable-border-width,
  ;; window-priority
  (let ((state-entry (gensym)))
     ;; alist of (drawable attributes attribute-changes geometry geometry-changes)
    `(with-stack-list (,state-entry ,drawable nil nil nil nil)
       (with-stack-list* (*window-attributes* ,state-entry *window-attributes*)
	 (multiple-value-prog1
	   (progn ,@body)
	   (cleanup-state-entry ,state-entry))))))

(defun cleanup-state-entry (state)
  ;; Return buffers to the free-list
  (let ((entry (state-attributes state)))
    (when entry (deallocate-context entry)))
  (let ((entry (state-attribute-changes state)))
    (when entry
      (put-window-attribute-changes (car state) entry)
      (deallocate-gcontext-state entry)))
  (let ((entry (state-geometry state)))
    (when entry (deallocate-context entry)))
  (let ((entry (state-geometry-changes state)))
    (when entry
      (put-drawable-geometry-changes (car state) entry)
      (deallocate-gcontext-state entry))))



(defun change-window-attribute (window number value)
  ;; Called from window attribute SETF's to alter an attribute value
  ;; number is the change-attributes request mask bit number
  (declare (type window window)
	   (type card8 number)
	   (type card32 value))
  (let ((state-entry nil)
	(changes nil))
    (if (and *window-attributes*
	     (setq state-entry (assoc window (the list *window-attributes*)
				      :test (window-equal-function))))
	(progn					; Within a WITH-STATE - cache changes
	  (setq changes (state-attribute-changes state-entry))
	  (unless changes
	    (setq changes (allocate-gcontext-state))
	    (setf (state-attribute-changes state-entry) changes)
	    (setf (aref changes 0) 0)) ;; Initialize mask to zero
	  (setf (aref changes 0) (logior (aref changes 0) (ash 1 number))) ;; set mask bit
	  (setf (aref changes (1+ number)) value))	;; save value
						; Send change to the server
      (with-buffer-request ((window-display window) +x-changewindowattributes+)
	(window window)
	(card32 (ash 1 number) value)))))
;;
;; These two are twins (change-window-attribute change-drawable-geometry)
;; If you change one, you probably need to change the other...
;;
(defun change-drawable-geometry (drawable number value)
  ;; Called from drawable geometry SETF's to alter an attribute value
  ;; number is the change-attributes request mask bit number
  (declare (type drawable drawable)
	   (type card8 number)
	   (type card29 value))
  (let ((state-entry nil)
	(changes nil))
    (if (and *window-attributes*
	     (setq state-entry (assoc drawable (the list *window-attributes*)
				      :test (drawable-equal-function))))
	(progn					; Within a WITH-STATE - cache changes
	  (setq changes (state-geometry-changes state-entry))
	  (unless changes
	    (setq changes (allocate-gcontext-state))
	    (setf (state-geometry-changes state-entry) changes)
	    (setf (aref changes 0) 0)) ;; Initialize mask to zero
	  (setf (aref changes 0) (logior (aref changes 0) (ash 1 number))) ;; set mask bit
	  (setf (aref changes (1+ number)) value))	;; save value
						; Send change to the server
      (with-buffer-request ((drawable-display drawable) +x-configurewindow+)
	(drawable drawable)
	(card16 (ash 1 number))
	(card29 value)))))

(defun get-window-attributes-buffer (window)
  (declare (type window window))
  (let ((state-entry nil)
	(changes nil))
    (or (and *window-attributes*
	     (setq state-entry (assoc window (the list *window-attributes*)
				      :test (window-equal-function)))
	     (null (setq changes (state-attribute-changes state-entry)))
	     (state-attributes state-entry))
	(let ((display (window-display window)))
	  (with-display (display)
	    ;; When SETF's have been done, flush changes to the server
	    (when changes
	      (put-window-attribute-changes window changes)
	      (deallocate-gcontext-state (state-attribute-changes state-entry))
	      (setf (state-attribute-changes state-entry) nil))
	    ;; Get window attributes
	    (with-buffer-request-and-reply (display +x-getwindowattributes+ size :sizes (8))
		 ((window window))
	      (let ((repbuf (or (state-attributes state-entry) (allocate-context))))
		(declare (type reply-buffer repbuf))
		;; Copy into repbuf from reply buffer
		(buffer-replace (reply-ibuf8 repbuf) buffer-bbuf 0 size)
		(when state-entry (setf (state-attributes state-entry) repbuf))
		repbuf)))))))

;;
;; These two are twins (get-window-attributes-buffer get-drawable-geometry-buffer)
;; If you change one, you probably need to change the other...
;;
(defun get-drawable-geometry-buffer (drawable)
  (declare (type drawable drawable))
  (let ((state-entry nil)
	(changes nil))
    (or (and *window-attributes*
	     (setq state-entry (assoc drawable (the list *window-attributes*)
				      :test (drawable-equal-function)))
	     (null (setq changes (state-geometry-changes state-entry)))
	     (state-geometry state-entry))
	(let ((display (drawable-display drawable)))
	  (with-display (display)
	    ;; When SETF's have been done, flush changes to the server
	    (when changes
	      (put-drawable-geometry-changes drawable changes)
	      (deallocate-gcontext-state (state-geometry-changes state-entry))
	      (setf (state-geometry-changes state-entry) nil))
	    ;; Get drawable attributes
	    (with-buffer-request-and-reply (display +x-getgeometry+ size :sizes (8))
		 ((drawable drawable))
	      (let ((repbuf (or (state-geometry state-entry) (allocate-context))))
		(declare (type reply-buffer repbuf))
		;; Copy into repbuf from reply buffer
		(buffer-replace (reply-ibuf8 repbuf) buffer-bbuf 0 size)
		(when state-entry (setf (state-geometry state-entry) repbuf))
		repbuf)))))))

(defun put-window-attribute-changes (window changes)
  ;; change window attributes
  ;; Always from Called within a WITH-DISPLAY
  (declare (type window window)
	   (type gcontext-state changes))
  (let* ((display (window-display window))
	 (mask (aref changes 0)))
    (declare (type display display)
	     (type mask32 mask))
    (with-buffer-request (display +x-changewindowattributes+)
      (window window)
      (card32 mask)
      (progn ;; Insert a word in the request for each one bit in the mask
	(do ((bits mask (ash bits -1))
	     (request-size 2)			;Word count
	     (i 1 (index+ i 1)))		;Entry count
	    ((zerop bits)
	     (card16-put 2 (index-incf request-size))
	     (index-incf (buffer-boffset display) (index* request-size 4)))
	  (declare (type mask32 bits)
		   (type array-index i request-size))
	  (when (oddp bits)
	    (card32-put (index* (index-incf request-size) 4) (aref changes i))))))))
;;
;; These two are twins (put-window-attribute-changes put-drawable-geometry-changes)
;; If you change one, you probably need to change the other...
;;
(defun put-drawable-geometry-changes (window changes)
  ;; change window attributes or geometry (depending on request-number...)
  ;; Always from Called within a WITH-DISPLAY
  (declare (type window window)
	   (type gcontext-state changes))
  (let* ((display (window-display window))
	 (mask (aref changes 0)))
    (declare (type display display)
	     (type mask16 mask))
    (with-buffer-request (display +x-configurewindow+)
      (window window)
      (card16 mask)
      (progn ;; Insert a word in the request for each one bit in the mask
	(do ((bits mask (ash bits -1))
	     (request-size 2)			;Word count
	     (i 1 (index+ i 1)))		;Entry count
	    ((zerop bits)
	     (card16-put 2 (incf request-size))
	     (index-incf (buffer-boffset display) (* request-size 4)))
	  (declare (type mask16 bits)
		   (type fixnum request-size)
		   (type array-index i))
	  (when (oddp bits)
	    (card29-put (* (incf request-size) 4) (aref changes i))))))))

(defmacro with-attributes ((window &rest options) &body body)
  `(let ((.with-attributes-reply-buffer. (get-window-attributes-buffer ,window)))
     (declare (type reply-buffer .with-attributes-reply-buffer.))
     (prog1 
       (with-buffer-input (.with-attributes-reply-buffer. ,@options) ,@body)
       (unless *window-attributes*
	 (deallocate-context .with-attributes-reply-buffer.)))))
;;
;; These two are twins (with-attributes with-geometry)
;; If you change one, you probably need to change the other...
;;
(defmacro with-geometry ((window &rest options) &body body)
  `(let ((.with-geometry-reply-buffer. (get-drawable-geometry-buffer ,window)))
     (declare (type reply-buffer .with-geometry-reply-buffer.))
     (prog1 
       (with-buffer-input (.with-geometry-reply-buffer. ,@options) ,@body)
       (unless *window-attributes*
	 (deallocate-context .with-geometry-reply-buffer.)))))

;;;-----------------------------------------------------------------------------
;;; Group A: (for GetWindowAttributes)
;;;-----------------------------------------------------------------------------

(defun window-visual (window)
  (declare (type window window))
  (declare (clx-values resource-id))
  (with-attributes (window :sizes 32)
    (resource-id-get 8)))

(defun window-visual-info (window)
  (declare (type window window))
  (declare (clx-values visual-info))
  (with-attributes (window :sizes 32)
    (visual-info (window-display window) (resource-id-get 8))))

(defun window-class (window)
  (declare (type window window))
  (declare (clx-values (member :input-output :input-only)))
  (with-attributes (window :sizes 16)
    (member16-get 12 :copy :input-output :input-only)))

(defun set-window-background (window background)
  (declare (type window window)
	   (type (or (member :none :parent-relative) pixel pixmap) background))
  (cond ((eq background :none) (change-window-attribute window 0 0))
	((eq background :parent-relative) (change-window-attribute window 0 1))
	((integerp background) ;; Background pixel
	 (change-window-attribute window 0 0) ;; pixmap :NONE
	 (change-window-attribute window 1 background))
	((type? background 'pixmap) ;; Background pixmap
	 (change-window-attribute window 0 (pixmap-id background)))
	(t (x-type-error background '(or (member :none :parent-relative) integer pixmap))))
  background)

#+Genera (eval-when (compile) (compiler:function-defined 'window-background))

(defsetf window-background set-window-background)

(defun set-window-border (window border)
  (declare (type window window)
	   (type (or (member :copy) pixel pixmap) border))
  (cond ((eq border :copy) (change-window-attribute window 2 0))
	((type? border 'pixmap) ;; Border pixmap
	 (change-window-attribute window 2 (pixmap-id border)))
	((integerp border) ;; Border pixel
	 (change-window-attribute window 3 border))
	(t (x-type-error border '(or (member :copy) integer pixmap))))
  border)

#+Genera (eval-when (compile) (compiler:function-defined 'window-border))

(defsetf window-border set-window-border)

(defun window-bit-gravity (window)
  ;; setf'able
  (declare (type window window))
  (declare (clx-values bit-gravity))
  (with-attributes (window :sizes 8)
    (member8-vector-get 14 +bit-gravity-vector+)))

(defun set-window-bit-gravity (window gravity)
  (change-window-attribute
    window 4 (encode-type (member-vector +bit-gravity-vector+) gravity))
  gravity)

(defsetf window-bit-gravity set-window-bit-gravity)

(defun window-gravity (window)
  ;; setf'able
  (declare (type window window))
  (declare (clx-values win-gravity))
  (with-attributes (window :sizes 8)
    (member8-vector-get 15 +win-gravity-vector+)))

(defun set-window-gravity (window gravity)
  (change-window-attribute
    window 5 (encode-type (member-vector +win-gravity-vector+) gravity))
  gravity)

(defsetf window-gravity set-window-gravity)

(defun window-backing-store (window)
  ;; setf'able
  (declare (type window window))
  (declare (clx-values (member :not-useful :when-mapped :always)))
  (with-attributes (window :sizes 8)
    (member8-get 1 :not-useful :when-mapped :always)))

(defun set-window-backing-store (window when)
  (change-window-attribute
    window 6 (encode-type (member :not-useful :when-mapped :always) when))
  when)

(defsetf window-backing-store set-window-backing-store)

(defun window-backing-planes (window)
  ;; setf'able
  (declare (type window window))
  (declare (clx-values pixel))
  (with-attributes (window :sizes 32)
    (card32-get 16)))

(defun set-window-backing-planes (window planes)
  (change-window-attribute window 7 (encode-type card32 planes))
  planes)

(defsetf window-backing-planes set-window-backing-planes)

(defun window-backing-pixel (window)
  ;; setf'able
  (declare (type window window))
  (declare (clx-values pixel))
  (with-attributes (window :sizes 32)
    (card32-get 20)))

(defun set-window-backing-pixel (window pixel)
  (change-window-attribute window 8 (encode-type card32 pixel))
  pixel)

(defsetf window-backing-pixel set-window-backing-pixel)

(defun window-save-under (window)
  ;; setf'able
  (declare (type window window))
  (declare (clx-values (member :off :on)))
  (with-attributes (window :sizes 8)
    (member8-get 24 :off :on)))

(defun set-window-save-under (window when)
  (change-window-attribute window 10 (encode-type (member :off :on) when))
  when)

(defsetf window-save-under set-window-save-under)

(defun window-override-redirect (window)
  ;; setf'able
  (declare (type window window))
  (declare (clx-values (member :off :on)))
  (with-attributes (window :sizes 8)
    (member8-get 27 :off :on)))

(defun set-window-override-redirect (window when)
  (change-window-attribute window 9 (encode-type (member :off :on) when))
  when)

(defsetf window-override-redirect set-window-override-redirect)

(defun window-event-mask (window)
  ;; setf'able
  (declare (type window window))
  (declare (clx-values mask32))
  (with-attributes (window :sizes 32)
    (card32-get 36)))

(defsetf window-event-mask (window) (event-mask)
  (let ((em (gensym)))
    `(let ((,em ,event-mask))
       (declare (type event-mask ,em))
       (change-window-attribute ,window 11 (encode-event-mask ,em))
       ,em)))

(defun window-do-not-propagate-mask (window)
  ;; setf'able
  (declare (type window window))
  (declare (clx-values mask32))
  (with-attributes (window :sizes 32)
    (card32-get 40)))

(defsetf window-do-not-propagate-mask (window) (device-event-mask)
  (let ((em (gensym)))
    `(let ((,em ,device-event-mask))
       (declare (type device-event-mask ,em))
       (change-window-attribute ,window 12 (encode-device-event-mask ,em))
       ,em)))

(defun window-colormap (window)
  (declare (type window window))
  (declare (clx-values (or null colormap)))
  (with-attributes (window :sizes 32)
    (let ((id (resource-id-get 28)))
      (if (zerop id)
	  nil
	  (let ((colormap (lookup-colormap (window-display window) id)))
	    (unless (colormap-visual-info colormap)
	      (setf (colormap-visual-info colormap)
		    (visual-info (window-display window) (resource-id-get 8))))
	    colormap)))))

(defun set-window-colormap (window colormap)
  (change-window-attribute
    window 13 (encode-type (or (member :copy) colormap) colormap))
  colormap)

(defsetf window-colormap set-window-colormap)

(defun window-cursor (window)
  (declare (type window window))
  (declare (clx-values cursor))
  window
  (error "~S can only be set" 'window-cursor))

(defun set-window-cursor (window cursor)
  (change-window-attribute
    window 14 (encode-type (or (member :none) cursor) cursor))
  cursor)

(defsetf window-cursor set-window-cursor)

(defun window-colormap-installed-p (window)
  (declare (type window window))
  (declare (clx-values generalized-boolean))
  (with-attributes (window :sizes 8)
    (boolean-get 25)))

(defun window-all-event-masks (window)
  (declare (type window window))
  (declare (clx-values mask32))
  (with-attributes (window :sizes 32)
    (card32-get 32)))

(defun window-map-state (window)
  (declare (type window window))
  (declare (clx-values (member :unmapped :unviewable :viewable)))
  (with-attributes (window :sizes 8)
    (member8-get 26 :unmapped :unviewable :viewable)))


;;;-----------------------------------------------------------------------------
;;; Group G: (for GetGeometry)
;;;-----------------------------------------------------------------------------

(defun drawable-root (drawable)
  (declare (type drawable drawable))
  (declare (clx-values window))
  (with-geometry (drawable :sizes 32)
    (window-get 8 (drawable-display drawable))))

(defun drawable-x (drawable)
  ;; setf'able
  (declare (type drawable drawable))
  (declare (clx-values int16))
  (with-geometry (drawable :sizes 16)
    (int16-get 12)))

(defun set-drawable-x (drawable x)
  (change-drawable-geometry drawable 0 (encode-type int16 x))
  x)

(defsetf drawable-x set-drawable-x)

(defun drawable-y (drawable)
  ;; setf'able
  (declare (type drawable drawable))
  (declare (clx-values int16))
  (with-geometry (drawable :sizes 16)
    (int16-get 14)))

(defun set-drawable-y (drawable y)
  (change-drawable-geometry drawable 1 (encode-type int16 y))
  y)

(defsetf drawable-y set-drawable-y)

(defun drawable-width (drawable)
  ;; setf'able
  ;; Inside width, excluding border.
  (declare (type drawable drawable))
  (declare (clx-values card16))
  (with-geometry (drawable :sizes 16)
    (card16-get 16)))

(defun set-drawable-width (drawable width)
  (change-drawable-geometry drawable 2 (encode-type card16 width))
  width)

(defsetf drawable-width set-drawable-width)

(defun drawable-height (drawable)
  ;; setf'able
  ;; Inside height, excluding border.
  (declare (type drawable drawable))
  (declare (clx-values card16))
  (with-geometry (drawable :sizes 16)
    (card16-get 18)))

(defun set-drawable-height (drawable height)
  (change-drawable-geometry drawable 3 (encode-type card16 height))
  height)

(defsetf drawable-height set-drawable-height)

(defun drawable-depth (drawable)
  (declare (type drawable drawable))
  (declare (clx-values card8))
  (with-geometry (drawable :sizes 8)
    (card8-get 1)))

(defun drawable-border-width (drawable)
  ;; setf'able
  (declare (type drawable drawable))
  (declare (clx-values integer))
  (with-geometry (drawable :sizes 16)
    (card16-get 20)))

(defun set-drawable-border-width (drawable width)
  (change-drawable-geometry drawable 4 (encode-type card16 width))
  width)

(defsetf drawable-border-width set-drawable-border-width)

(defun set-window-priority (mode window sibling)
  (declare (type (member :above :below :top-if :bottom-if :opposite) mode)
	   (type window window)
	   (type (or null window) sibling))
  (with-state (window)
    (change-drawable-geometry
      window 6 (encode-type (member :above :below :top-if :bottom-if :opposite) mode))
    (when sibling
      (change-drawable-geometry window 5 (encode-type window sibling))))
  mode)

#+Genera (eval-when (compile) (compiler:function-defined 'window-priority))

(defsetf window-priority (window &optional sibling) (mode)
  ;; A bit strange, but retains setf form.
  `(set-window-priority ,mode ,window ,sibling))
