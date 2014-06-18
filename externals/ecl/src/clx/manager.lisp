;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;; Window Manager Property functions

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

(defun wm-name (window)
  (declare (type window window))
  (declare (clx-values string))
  (get-property window :WM_NAME :type :STRING :result-type 'string :transform #'card8->char))

(defsetf wm-name (window) (name)
  `(set-string-property ,window :WM_NAME ,name))

(defun set-string-property (window property string)
  (declare (type window window)
	   (type keyword property)
	   (type stringable string))
  (change-property window property (string string) :STRING 8 :transform #'char->card8)
  string)

(defun wm-icon-name (window)
  (declare (type window window))
  (declare (clx-values string))
  (get-property window :WM_ICON_NAME :type :STRING
		:result-type 'string :transform #'card8->char))

(defsetf wm-icon-name (window) (name)
  `(set-string-property ,window :WM_ICON_NAME ,name))

(defun wm-client-machine (window)
  (declare (type window window))
  (declare (clx-values string))
  (get-property window :WM_CLIENT_MACHINE :type :STRING
		:result-type 'string :transform #'card8->char))

(defsetf wm-client-machine (window) (name)
  `(set-string-property ,window :WM_CLIENT_MACHINE ,name))

(defun get-wm-class (window)
  (declare (type window window))
  (declare (clx-values (or null name-string) (or null class-string)))
  (let ((value (get-property window :WM_CLASS :type :STRING :result-type '(vector card8))))
    (declare (type (or null (vector card8)) value))
    (when value
      (let* ((name-len (position 0 (the (vector card8) value)))
	     (name (subseq (the (vector card8) value) 0 name-len))
	     (class
              (when name-len
                (subseq (the (vector card8) value) (1+ name-len)
                        (position 0 (the (vector card8) value) :start (1+ name-len))))))
	(values (and (plusp (length name)) (map 'string #'card8->char name))
		(and (plusp (length class)) (map 'string #'card8->char class)))))))

(defun set-wm-class (window resource-name resource-class)
  (declare (type window window)
	   (type (or null stringable) resource-name resource-class))
  (change-property window :WM_CLASS
		   (concatenate '(vector card8)
				(map '(vector card8) #'char->card8
				     (string (or resource-name "")))
				#(0)
				(map '(vector card8) #'char->card8
				     (string (or resource-class "")))
				#(0))
		   :string 8)
  (values))

(defun wm-command (window)
  ;; Returns a list whose car is the command and 
  ;; whose cdr is the list of arguments
  (declare (type window window))
  (declare (clx-values list))
  (do* ((command-string (get-property window :WM_COMMAND :type :STRING
				      :result-type '(vector card8)))
	(command nil)
	(start 0 (1+ end))
	(end 0)
	(len (length command-string)))
       ((>= start len) (nreverse command))
    (setq end (position 0 command-string :start start))
    (push (map 'string #'card8->char (subseq command-string start end))
	  command)))

(defsetf wm-command set-wm-command)
(defun set-wm-command (window command)
  ;; Uses PRIN1 inside the ANSI common lisp form WITH-STANDARD-IO-SYNTAX (or
  ;; equivalent), with elements of command separated by NULL characters.  This
  ;; enables 
  ;;   (with-standard-io-syntax (mapcar #'read-from-string (wm-command window)))
  ;; to recover a lisp command.
  (declare (type window window)
	   (type list command))
  (change-property window :WM_COMMAND
		   (apply #'concatenate '(vector card8)
			  (mapcan #'(lambda (c)
				      (list (map '(vector card8) #'char->card8
						 (with-output-to-string (stream)
						   (with-standard-io-syntax 
						     (prin1 c stream))))
					    #(0)))
				  command))
		   :string 8)
  command)

;;-----------------------------------------------------------------------------
;; WM_HINTS

(def-clx-class (wm-hints)
  (input nil :type (or null (member :off :on)))
  (initial-state nil :type (or null (member :dont-care :normal :zoom :iconic :inactive)))
  (icon-pixmap nil :type (or null pixmap))
  (icon-window nil :type (or null window))
  (icon-x nil :type (or null card16))
  (icon-y nil :type (or null card16))
  (icon-mask nil :type (or null pixmap))
  (window-group nil :type (or null resource-id))
  (flags 0 :type card32)    ;; Extension-hook.  Exclusive-Or'ed with the FLAGS field
  ;; may be extended in the future
  )

(defun wm-hints (window)
  (declare (type window window))
  (declare (clx-values wm-hints))
  (let ((prop (get-property window :WM_HINTS :type :WM_HINTS :result-type 'vector)))
    (when prop
      (decode-wm-hints prop (window-display window)))))

(defsetf wm-hints set-wm-hints)
(defun set-wm-hints (window wm-hints)
  (declare (type window window)
	   (type wm-hints wm-hints))
  (declare (clx-values wm-hints))
  (change-property window :WM_HINTS (encode-wm-hints wm-hints) :WM_HINTS 32)
  wm-hints)

(defun decode-wm-hints (vector display)
  (declare (type (simple-vector 9) vector)
	   (type display display))
  (declare (clx-values wm-hints))
  (let ((input-hint 0)
	(state-hint 1)
	(icon-pixmap-hint 2)
	(icon-window-hint 3)
	(icon-position-hint 4)
	(icon-mask-hint 5)
	(window-group-hint 6))
    (let ((flags (aref vector 0))
	  (hints (make-wm-hints))
	  (%buffer display))
      (declare (type card32 flags)
	       (type wm-hints hints)
	       (type display %buffer))
      (setf (wm-hints-flags hints) flags)
      (when (logbitp input-hint flags)
	(setf (wm-hints-input hints) (decode-type (member :off :on) (aref vector 1))))
      (when (logbitp state-hint flags)
	(setf (wm-hints-initial-state hints)
	      (decode-type (member :dont-care :normal :zoom :iconic :inactive)
			   (aref vector 2))))
      (when (logbitp icon-pixmap-hint flags)
	(setf (wm-hints-icon-pixmap hints) (decode-type pixmap (aref vector 3))))
      (when (logbitp icon-window-hint flags)
	(setf (wm-hints-icon-window hints) (decode-type window (aref vector 4))))
      (when (logbitp icon-position-hint flags)
	(setf (wm-hints-icon-x hints) (aref vector 5)
	      (wm-hints-icon-y hints) (aref vector 6)))
      (when (logbitp icon-mask-hint flags)
	(setf (wm-hints-icon-mask hints) (decode-type pixmap (aref vector 7))))
      (when (and (logbitp window-group-hint flags) (> (length vector) 7))
	(setf (wm-hints-window-group hints) (aref vector 8)))
      hints)))


(defun encode-wm-hints (wm-hints)
  (declare (type wm-hints wm-hints))
  (declare (clx-values simple-vector))
  (let ((input-hint         #b1)
	(state-hint         #b10)
	(icon-pixmap-hint   #b100)
	(icon-window-hint   #b1000)
	(icon-position-hint #b10000)
	(icon-mask-hint     #b100000)
	(window-group-hint  #b1000000)
	(mask               #b1111111)
	)
    (let ((vector (make-array 9 :initial-element 0))
	  (flags 0))
      (declare (type (simple-vector 9) vector)
	       (type card16 flags))
      (when (wm-hints-input wm-hints)
	(setf flags input-hint
	      (aref vector 1) (encode-type (member :off :on) (wm-hints-input wm-hints))))
      (when (wm-hints-initial-state wm-hints)
	(setf flags (logior flags state-hint)
	      (aref vector 2) (encode-type (member :dont-care :normal :zoom :iconic :inactive)
					   (wm-hints-initial-state wm-hints))))
      (when (wm-hints-icon-pixmap wm-hints)
	(setf flags (logior flags icon-pixmap-hint)
	      (aref vector 3) (encode-type pixmap (wm-hints-icon-pixmap wm-hints))))
      (when (wm-hints-icon-window wm-hints)
	(setf flags (logior flags icon-window-hint)
	      (aref vector 4) (encode-type window (wm-hints-icon-window wm-hints))))
      (when (and (wm-hints-icon-x wm-hints) (wm-hints-icon-y wm-hints))
	(setf flags (logior flags icon-position-hint)
	      (aref vector 5) (encode-type card16 (wm-hints-icon-x wm-hints))
	      (aref vector 6) (encode-type card16 (wm-hints-icon-y wm-hints))))
      (when (wm-hints-icon-mask wm-hints)
	(setf flags (logior flags icon-mask-hint)
	      (aref vector 7) (encode-type pixmap (wm-hints-icon-mask wm-hints))))
      (when (wm-hints-window-group wm-hints)
	(setf flags (logior flags window-group-hint)
	      (aref vector 8) (wm-hints-window-group wm-hints)))
      (setf (aref vector 0) (logior flags (logandc2 (wm-hints-flags wm-hints) mask)))
      vector)))

;;-----------------------------------------------------------------------------
;; WM_SIZE_HINTS

(def-clx-class (wm-size-hints)
  (user-specified-position-p nil :type generalized-boolean) ;; True when user specified x y
  (user-specified-size-p nil :type generalized-boolean)     ;; True when user specified width height
  ;; the next four fields are obsolete when using a modern window manager
  ;; (that will use min-width and friends instead), but they should be set by
  ;; clients in case an old window manager is used
  (x nil :type (or null int32))
  (y nil :type (or null int32))
  (width nil :type (or null card32))
  (height nil :type (or null card32))
  (min-width nil :type (or null card32))
  (min-height nil :type (or null card32))
  (max-width nil :type (or null card32))
  (max-height nil :type (or null card32))
  (width-inc nil :type (or null card32))
  (height-inc nil :type (or null card32))
  (min-aspect nil :type (or null number))
  (max-aspect nil :type (or null number))
  (base-width nil :type (or null card32))
  (base-height nil :type (or null card32))
  (win-gravity nil :type (or null win-gravity))
  (program-specified-position-p nil :type generalized-boolean) ;; True when program specified x y
  (program-specified-size-p nil :type generalized-boolean)     ;; True when program specified width height
  )


(defun wm-normal-hints (window)
  (declare (type window window))
  (declare (clx-values wm-size-hints))
  (decode-wm-size-hints (get-property window :WM_NORMAL_HINTS :type :WM_SIZE_HINTS :result-type 'vector)))

(defsetf wm-normal-hints set-wm-normal-hints)
(defun set-wm-normal-hints (window hints)
  (declare (type window window)
	   (type wm-size-hints hints))
  (declare (clx-values wm-size-hints))
  (change-property window :WM_NORMAL_HINTS (encode-wm-size-hints hints) :WM_SIZE_HINTS 32)
  hints)

;;; OBSOLETE
(defun wm-zoom-hints (window)
  (declare (type window window))
  (declare (clx-values wm-size-hints))
  (decode-wm-size-hints (get-property window :WM_ZOOM_HINTS :type :WM_SIZE_HINTS :result-type 'vector)))

;;; OBSOLETE
(defsetf wm-zoom-hints set-wm-zoom-hints)
;;; OBSOLETE
(defun set-wm-zoom-hints (window hints)
  (declare (type window window)
	   (type wm-size-hints hints))
  (declare (clx-values wm-size-hints))
  (change-property window :WM_ZOOM_HINTS (encode-wm-size-hints hints) :WM_SIZE_HINTS 32)
  hints)

(defun decode-wm-size-hints (vector)
  (declare (type (or null (simple-vector *)) vector))
  (declare (clx-values (or null wm-size-hints)))
  (when vector
    (let ((flags (aref vector 0))
	  (hints (make-wm-size-hints)))
      (declare (type card16 flags)
	       (type wm-size-hints hints))
      (setf (wm-size-hints-user-specified-position-p hints) (logbitp 0 flags))
      (setf (wm-size-hints-user-specified-size-p hints) (logbitp 1 flags))
      (setf (wm-size-hints-program-specified-position-p hints) (logbitp 2 flags))
      (setf (wm-size-hints-program-specified-size-p hints) (logbitp 3 flags))
      (when (logbitp 4 flags)
	(setf (wm-size-hints-min-width hints) (aref vector 5)
	      (wm-size-hints-min-height hints) (aref vector 6)))
      (when (logbitp 5 flags)
	(setf (wm-size-hints-max-width hints) (aref vector 7)
	      (wm-size-hints-max-height hints) (aref vector 8)))
      (when (logbitp 6 flags)
	(setf (wm-size-hints-width-inc hints) (aref vector 9)
	      (wm-size-hints-height-inc hints) (aref vector 10)))
      (when (logbitp 7 flags)
	(setf (wm-size-hints-min-aspect hints) (/ (aref vector 11) (aref vector 12))
	      (wm-size-hints-max-aspect hints) (/ (aref vector 13) (aref vector 14))))
      (when (> (length vector) 15)
	;; This test is for backwards compatibility since old Xlib programs
	;; can set a size-hints structure that is too small.  See ICCCM.
	(when (logbitp 8 flags)
	  (setf (wm-size-hints-base-width hints) (aref vector 15)
		(wm-size-hints-base-height hints) (aref vector 16)))
	(when (logbitp 9 flags)
	  (setf (wm-size-hints-win-gravity hints)
		(decode-type (member-vector +win-gravity-vector+) (aref vector 17)))))
      ;; Obsolete fields
      (when (or (logbitp 0 flags) (logbitp 2 flags))
	(setf (wm-size-hints-x hints) (card32->int32 (aref vector 1))
	      (wm-size-hints-y hints) (card32->int32 (aref vector 2))))
      (when (or (logbitp 1 flags) (logbitp 3 flags))
	(setf (wm-size-hints-width hints) (aref vector 3)
	      (wm-size-hints-height hints) (aref vector 4)))
      hints)))

(defun encode-wm-size-hints (hints)
  (declare (type wm-size-hints hints))
  (declare (clx-values simple-vector))
  (let ((vector (make-array 18 :initial-element 0))
	(flags 0))
    (declare (type (simple-vector 18) vector)
	     (type card16 flags)) 
    (when (wm-size-hints-user-specified-position-p hints)
      (setf (ldb (byte 1 0) flags) 1))
    (when (wm-size-hints-user-specified-size-p hints)
      (setf (ldb (byte 1 1) flags) 1))
    (when (wm-size-hints-program-specified-position-p hints)
      (setf (ldb (byte 1 2) flags) 1))
    (when (wm-size-hints-program-specified-size-p hints)
      (setf (ldb (byte 1 3) flags) 1))
    (when (and (wm-size-hints-min-width hints) (wm-size-hints-min-height hints))
      (setf (ldb (byte 1 4) flags) 1
	    (aref vector 5) (wm-size-hints-min-width hints)
	    (aref vector 6) (wm-size-hints-min-height hints)))
    (when (and (wm-size-hints-max-width hints) (wm-size-hints-max-height hints))
      (setf (ldb (byte 1 5) flags) 1
	    (aref vector 7) (wm-size-hints-max-width hints)
	    (aref vector 8) (wm-size-hints-max-height hints)))
    (when (and (wm-size-hints-width-inc hints) (wm-size-hints-height-inc hints))
      (setf (ldb (byte 1 6) flags) 1
	    (aref vector 9) (wm-size-hints-width-inc hints)
	    (aref vector 10) (wm-size-hints-height-inc hints)))
    (let ((min-aspect (wm-size-hints-min-aspect hints))
	  (max-aspect (wm-size-hints-max-aspect hints)))
      (when (and min-aspect max-aspect)
	(setf (ldb (byte 1 7) flags) 1
	      min-aspect (rationalize min-aspect)
	      max-aspect (rationalize max-aspect)
	      (aref vector 11) (numerator min-aspect)
	      (aref vector 12) (denominator min-aspect)
	      (aref vector 13) (numerator max-aspect)
	      (aref vector 14) (denominator max-aspect))))
    (when (and (wm-size-hints-base-width hints)
	       (wm-size-hints-base-height hints))
      (setf (ldb (byte 1 8) flags) 1
	    (aref vector 15) (wm-size-hints-base-width hints)
	    (aref vector 16) (wm-size-hints-base-height hints)))
    (when (wm-size-hints-win-gravity hints)
      (setf (ldb (byte 1 9) flags) 1
	    (aref vector 17) (encode-type
			       (member-vector +win-gravity-vector+)
			       (wm-size-hints-win-gravity hints))))
    ;; Obsolete fields
    (when (and (wm-size-hints-x hints) (wm-size-hints-y hints)) 
      (unless (wm-size-hints-user-specified-position-p hints)
	(setf (ldb (byte 1 2) flags) 1))
      (setf (aref vector 1) (wm-size-hints-x hints)
	    (aref vector 2) (wm-size-hints-y hints)))
    (when (and (wm-size-hints-width hints) (wm-size-hints-height hints))
      (unless (wm-size-hints-user-specified-size-p hints)
	(setf (ldb (byte 1 3) flags) 1))
      (setf (aref vector 3) (wm-size-hints-width hints)
	    (aref vector 4) (wm-size-hints-height hints)))
    (setf (aref vector 0) flags)
    vector))

;;-----------------------------------------------------------------------------
;; Icon_Size

;; Use the same intermediate structure as WM_SIZE_HINTS

(defun icon-sizes (window)
  (declare (type window window))
  (declare (clx-values wm-size-hints))
  (let ((vector (get-property window :WM_ICON_SIZE :type :WM_ICON_SIZE :result-type 'vector)))
    (declare (type (or null (simple-vector 6)) vector))
    (when vector
      (make-wm-size-hints
	:min-width (aref vector 0)
	:min-height (aref vector 1)
	:max-width (aref vector 2)
	:max-height (aref vector 3)
	:width-inc (aref vector 4)
	:height-inc (aref vector 5)))))
  
(defsetf icon-sizes set-icon-sizes)
(defun set-icon-sizes (window wm-size-hints)
  (declare (type window window)
	   (type wm-size-hints wm-size-hints))
  (let ((vector (vector (wm-size-hints-min-width wm-size-hints)
			(wm-size-hints-min-height wm-size-hints)
			(wm-size-hints-max-width wm-size-hints)
			(wm-size-hints-max-height wm-size-hints)
			(wm-size-hints-width-inc wm-size-hints)
			(wm-size-hints-height-inc wm-size-hints))))
    (change-property window :WM_ICON_SIZE vector :WM_ICON_SIZE 32)
    wm-size-hints))

;;-----------------------------------------------------------------------------
;; WM-Protocols

(defun wm-protocols (window)
  (map 'list #'(lambda (id) (atom-name (window-display window) id))
       (get-property window :WM_PROTOCOLS :type :ATOM)))

(defsetf wm-protocols set-wm-protocols)
(defun set-wm-protocols (window protocols)
  (change-property window :WM_PROTOCOLS
		   (map 'list #'(lambda (atom) (intern-atom (window-display window) atom))
			protocols)
		   :ATOM 32)
  protocols)

;;-----------------------------------------------------------------------------
;; WM-Colormap-windows

(defun wm-colormap-windows (window)
  (values (get-property window :WM_COLORMAP_WINDOWS :type :WINDOW
			:transform #'(lambda (id)
				       (lookup-window (window-display window) id)))))

(defsetf wm-colormap-windows set-wm-colormap-windows)
(defun set-wm-colormap-windows (window colormap-windows)
  (change-property window :WM_COLORMAP_WINDOWS colormap-windows :WINDOW 32
		   :transform #'window-id)
  colormap-windows)

;;-----------------------------------------------------------------------------
;; Transient-For

(defun transient-for (window)
  (let ((prop (get-property window :WM_TRANSIENT_FOR :type :WINDOW :result-type 'list)))
    (and prop (lookup-window (window-display window) (car prop)))))

(defsetf transient-for set-transient-for)
(defun set-transient-for (window transient)
  (declare (type window window transient))
  (change-property window :WM_TRANSIENT_FOR (list (window-id transient)) :WINDOW 32)
  transient)

;;-----------------------------------------------------------------------------
;; Set-WM-Properties

(defun set-wm-properties (window &rest options &key 
			  name icon-name resource-name resource-class command
			  client-machine hints normal-hints zoom-hints
			  ;; the following are used for wm-normal-hints
			  (user-specified-position-p nil usppp)
			  (user-specified-size-p nil usspp)
			  (program-specified-position-p nil psppp)
			  (program-specified-size-p nil psspp)
			  x y width height min-width min-height max-width max-height
			  width-inc height-inc min-aspect max-aspect
			  base-width base-height win-gravity
			  ;; the following are used for wm-hints
			  input initial-state icon-pixmap icon-window
			  icon-x icon-y icon-mask window-group)
  ;; Set properties for WINDOW.
  (declare (arglist window &rest options &key 
		   name icon-name resource-name resource-class command
		   client-machine hints normal-hints
		   ;; the following are used for wm-normal-hints
		   user-specified-position-p user-specified-size-p
		   program-specified-position-p program-specified-size-p
		   min-width min-height max-width max-height
		   width-inc height-inc min-aspect max-aspect
		   base-width base-height win-gravity
		   ;; the following are used for wm-hints
		   input initial-state icon-pixmap icon-window
		   icon-x icon-y icon-mask window-group))
  (declare (type window window)
	   (type (or null stringable) name icon-name resource-name resource-class client-machine)
	   (type (or null list) command)
	   (type (or null wm-hints) hints)
	   (type (or null wm-size-hints) normal-hints zoom-hints)
	   (type generalized-boolean user-specified-position-p user-specified-size-p)
	   (type generalized-boolean program-specified-position-p program-specified-size-p)
	   (type (or null int32) x y)
	   (type (or null card32) width height min-width min-height max-width max-height width-inc height-inc base-width base-height)
	   (type (or null win-gravity) win-gravity)
	   (type (or null number) min-aspect max-aspect)
	   (type (or null (member :off :on)) input)
	   (type (or null (member :dont-care :normal :zoom :iconic :inactive)) initial-state)
	   (type (or null pixmap) icon-pixmap icon-mask)
	   (type (or null window) icon-window)
	   (type (or null card32) icon-x icon-y)
	   (type (or null resource-id) window-group)
	   (dynamic-extent options))
  (when name (setf (wm-name window) name))
  (when icon-name (setf (wm-icon-name window) icon-name))
  (when client-machine (setf (wm-client-machine window) client-machine))
  (when (or resource-name resource-class)
    (set-wm-class window resource-name resource-class))
  (when command (setf (wm-command window) command))
  ;; WM-HINTS
  (if (dolist (arg '(:input :initial-state :icon-pixmap :icon-window
			    :icon-x :icon-y :icon-mask :window-group))
	(when (getf options arg) (return t)))
      (let ((wm-hints (if hints (copy-wm-hints hints) (make-wm-hints))))
	(when input (setf (wm-hints-input wm-hints) input))
	(when initial-state (setf (wm-hints-initial-state wm-hints) initial-state))
	(when icon-pixmap (setf (wm-hints-icon-pixmap wm-hints) icon-pixmap))
	(when icon-window (setf (wm-hints-icon-window wm-hints) icon-window))
	(when icon-x (setf (wm-hints-icon-x wm-hints) icon-x))
	(when icon-y (setf (wm-hints-icon-y wm-hints) icon-y))
	(when icon-mask (setf (wm-hints-icon-mask wm-hints) icon-mask))
	(when window-group (setf (wm-hints-window-group wm-hints) window-group))
	(setf (wm-hints window) wm-hints))
      (when hints (setf (wm-hints window) hints)))
  ;; WM-NORMAL-HINTS
  (if (dolist (arg '(:x :y :width :height :min-width :min-height :max-width :max-height
			:width-inc :height-inc :min-aspect :max-aspect
			:user-specified-position-p :user-specified-size-p
			:program-specified-position-p :program-specified-size-p
			:base-width :base-height :win-gravity))
	(when (getf options arg) (return t)))
      (let ((size (if normal-hints (copy-wm-size-hints normal-hints) (make-wm-size-hints))))
	(when x (setf (wm-size-hints-x size) x))
	(when y (setf (wm-size-hints-y size) y))
	(when width (setf (wm-size-hints-width size) width))
	(when height (setf (wm-size-hints-height size) height))
	(when min-width (setf (wm-size-hints-min-width size) min-width))
	(when min-height (setf (wm-size-hints-min-height size) min-height))
	(when max-width (setf (wm-size-hints-max-width size) max-width))
	(when max-height (setf (wm-size-hints-max-height size) max-height))
	(when width-inc (setf (wm-size-hints-width-inc size) width-inc))
	(when height-inc (setf (wm-size-hints-height-inc size) height-inc))
	(when min-aspect (setf (wm-size-hints-min-aspect size) min-aspect))
	(when max-aspect (setf (wm-size-hints-max-aspect size) max-aspect))
	(when base-width (setf (wm-size-hints-base-width size) base-width))
	(when base-height (setf (wm-size-hints-base-height size) base-height))
	(when win-gravity (setf (wm-size-hints-win-gravity size) win-gravity))
	(when usppp
	  (setf (wm-size-hints-user-specified-position-p size) user-specified-position-p))
	(when usspp
	  (setf (wm-size-hints-user-specified-size-p size) user-specified-size-p))
	(when psppp
	  (setf (wm-size-hints-program-specified-position-p size) program-specified-position-p))
	(when psspp
	  (setf (wm-size-hints-program-specified-size-p size) program-specified-size-p))
	(setf (wm-normal-hints window) size))
      (when normal-hints (setf (wm-normal-hints window) normal-hints)))
  (when zoom-hints (setf (wm-zoom-hints window) zoom-hints))
  )

;;; OBSOLETE
(defun set-standard-properties (window &rest options)
  (declare (dynamic-extent options))
  (apply #'set-wm-properties window options))

;;-----------------------------------------------------------------------------
;; WM Control

(defun iconify-window (window screen)
  (declare (type window window)
	   (type screen screen))
  (let ((root (screen-root screen)))
    (declare (type window root))
    (send-event root :client-message '(:substructure-redirect :substructure-notify)
		:window window :format 32 :type :WM_CHANGE_STATE :data (list 3))))

(defun withdraw-window (window screen)
  (declare (type window window)
	   (type screen screen))
  (unmap-window window)
  (let ((root (screen-root screen)))
    (declare (type window root))
    (send-event root :unmap-notify '(:substructure-redirect :substructure-notify)
		:window window :event-window root :configure-p nil)))


;;-----------------------------------------------------------------------------
;; Colormaps

(def-clx-class (standard-colormap (:copier nil) (:predicate nil))
  (colormap nil :type (or null colormap))
  (base-pixel 0 :type pixel)
  (max-color nil :type (or null color))
  (mult-color nil :type (or null color))
  (visual nil :type (or null visual-info))
  (kill nil :type (or (member nil :release-by-freeing-colormap)
		      drawable gcontext cursor colormap font)))

(defun rgb-colormaps (window property)
  (declare (type window window)
	   (type (member :RGB_DEFAULT_MAP :RGB_BEST_MAP :RGB_RED_MAP
			 :RGB_GREEN_MAP :RGB_BLUE_MAP) property))
  (let ((prop (get-property window property :type :RGB_COLOR_MAP :result-type 'vector)))
    (declare (type (or null simple-vector) prop))
    (when prop
      (list (make-standard-colormap
	      :colormap (lookup-colormap (window-display window) (aref prop 0))
	      :base-pixel (aref prop 7)
	      :max-color (make-color :red   (card16->rgb-val (aref prop 1))
				     :green (card16->rgb-val (aref prop 3))
				     :blue  (card16->rgb-val (aref prop 5)))
	      :mult-color (make-color :red   (card16->rgb-val (aref prop 2))
				      :green (card16->rgb-val (aref prop 4))
				      :blue  (card16->rgb-val (aref prop 6)))
	      :visual (and (<= 9 (length prop))
			   (visual-info (window-display window) (aref prop 8)))
	      :kill (and (<= 10 (length prop))
			 (let ((killid (aref prop 9)))
			   (if (= killid 1)
			       :release-by-freeing-colormap
			       (lookup-resource-id (window-display window) killid)))))))))

(defsetf rgb-colormaps set-rgb-colormaps)
(defun set-rgb-colormaps (window property maps)
  (declare (type window window)
	   (type (member :RGB_DEFAULT_MAP :RGB_BEST_MAP :RGB_RED_MAP
			 :RGB_GREEN_MAP :RGB_BLUE_MAP) property)
	   (type list maps))
  (let ((prop (make-array (* 10 (length maps)) :element-type 'card32))
	(index -1))
    (dolist (map maps)
      (setf (aref prop (incf index))
	    (encode-type colormap (standard-colormap-colormap map)))
      (setf (aref prop (incf index))
	    (encode-type rgb-val (color-red (standard-colormap-max-color map))))
      (setf (aref prop (incf index))
	    (encode-type rgb-val (color-red (standard-colormap-mult-color map))))
      (setf (aref prop (incf index))
	    (encode-type rgb-val (color-green (standard-colormap-max-color map))))
      (setf (aref prop (incf index))
	    (encode-type rgb-val (color-green (standard-colormap-mult-color map))))
      (setf (aref prop (incf index))
	    (encode-type rgb-val (color-blue (standard-colormap-max-color map))))
      (setf (aref prop (incf index))
	    (encode-type rgb-val (color-blue (standard-colormap-mult-color map))))
      (setf (aref prop (incf index))
	    (standard-colormap-base-pixel map))
      (setf (aref prop (incf index))
	    (visual-info-id (standard-colormap-visual map)))
      (setf (aref prop (incf index))
	    (let ((kill (standard-colormap-kill map)))
	      (etypecase kill
		(symbol
		  (ecase kill
		    ((nil) 0)
		    ((:release-by-freeing-colormap) 1)))
		(drawable (drawable-id kill))
		(gcontext (gcontext-id kill))
		(cursor (cursor-id kill))
		(colormap (colormap-id kill))
		(font (font-id kill))))))
    (change-property window property prop :RGB_COLOR_MAP 32)))

;;; OBSOLETE
(defun get-standard-colormap (window property)
  (declare (type window window)
	   (type (member :RGB_DEFAULT_MAP :RGB_BEST_MAP :RGB_RED_MAP
			 :RGB_GREEN_MAP :RGB_BLUE_MAP) property))
  (declare (clx-values colormap base-pixel max-color mult-color))
  (let ((prop (get-property window property :type :RGB_COLOR_MAP :result-type 'vector)))
    (declare (type (or null simple-vector) prop))
    (when prop
      (values (lookup-colormap (window-display window) (aref prop 0))
	      (aref prop 7)			;Base Pixel
	      (make-color :red   (card16->rgb-val (aref prop 1))	;Max Color
			  :green (card16->rgb-val (aref prop 3))
			  :blue  (card16->rgb-val (aref prop 5)))
	      (make-color :red   (card16->rgb-val (aref prop 2))	;Mult color
			  :green (card16->rgb-val (aref prop 4))
			  :blue  (card16->rgb-val (aref prop 6)))))))

;;; OBSOLETE
(defun set-standard-colormap (window property colormap base-pixel max-color mult-color)
  (declare (type window window)
	   (type (member :RGB_DEFAULT_MAP :RGB_BEST_MAP :RGB_RED_MAP
			 :RGB_GREEN_MAP :RGB_BLUE_MAP) property)
	   (type colormap colormap)
	   (type pixel base-pixel)
	   (type color max-color mult-color))
  (let ((prop (vector (encode-type colormap colormap)
		      (encode-type rgb-val (color-red max-color))
		      (encode-type rgb-val (color-red mult-color))
		      (encode-type rgb-val (color-green max-color))
		      (encode-type rgb-val (color-green mult-color))
		      (encode-type rgb-val (color-blue max-color))
		      (encode-type rgb-val (color-blue mult-color))
		      base-pixel)))
    (change-property window property prop :RGB_COLOR_MAP 32)))

;;-----------------------------------------------------------------------------
;; Cut-Buffers

(defun cut-buffer (display &key (buffer 0) (type :STRING) (result-type 'string)
		   (transform #'card8->char) (start 0) end)
  ;; Return the contents of cut-buffer BUFFER
  (declare (type display display)
	   (type (integer 0 7) buffer)
	   (type xatom type)
	   (type array-index start)
	   (type (or null array-index) end)
	   (type t result-type)			;a sequence type
	   (type (or null (function (integer) t)) transform))
  (declare (clx-values sequence type format bytes-after))
  (let* ((root (screen-root (first (display-roots display))))
	 (property (aref '#(:CUT_BUFFER0 :CUT_BUFFER1 :CUT_BUFFER2 :CUT_BUFFER3
			    :CUT_BUFFER4 :CUT_BUFFER5 :CUT_BUFFER6 :CUT_BUFFER7)
			 buffer)))
    (get-property root property :type type :result-type result-type
		  :start start :end end :transform transform)))

;; Implement the following:
;; (defsetf cut-buffer (display &key (buffer 0) (type :string) (format 8)
;;			        (transform #'char->card8) (start 0) end) (data)
;; In order to avoid having to pass positional parameters to set-cut-buffer,
;; We've got to do the following.  WHAT A PAIN...
#-clx-ansi-common-lisp
(define-setf-method cut-buffer (display &rest option-list)
  (declare (dynamic-extent option-list))
  (do* ((options (copy-list option-list))
	(option options (cddr option))
	(store (gensym))
	(dtemp (gensym))
	(temps (list dtemp))
	(values (list display)))
       ((endp option)
	(values (nreverse temps)
		(nreverse values)
		(list store)
		`(set-cut-buffer ,store ,dtemp ,@options)
		`(cut-buffer ,@options)))
    (unless (member (car option) '(:buffer :type :format :start :end :transform))
      (error "Keyword arg ~s isn't recognized" (car option)))
    (let ((x (gensym)))
      (push x temps)
      (push (cadr option) values)
      (setf (cadr option) x))))

(defun
  #+clx-ansi-common-lisp (setf cut-buffer)
  #-clx-ansi-common-lisp set-cut-buffer
  (data display &key (buffer 0) (type :STRING) (format 8)
	(start 0) end (transform #'char->card8))
  (declare (type sequence data)
	   (type display display)
	   (type (integer 0 7) buffer)
	   (type xatom type)
	   (type (member 8 16 32) format)
	   (type array-index start)
	   (type (or null array-index) end)
	   (type (or null (function (integer) t)) transform))
  (let* ((root (screen-root (first (display-roots display))))
	 (property (aref '#(:CUT_BUFFER0 :CUT_BUFFER1 :CUT_BUFFER2 :CUT_BUFFER3
					 :CUT_BUFFER4 :CUT_BUFFER5 :CUT_BUFFER6 :CUT_BUFFER7)
			 buffer)))
    (change-property root property data type format :transform transform :start start :end end)
    data))

(defun rotate-cut-buffers (display &optional (delta 1) (careful-p t))
  ;; Positive rotates left, negative rotates right (opposite of actual protocol request).
  ;; When careful-p, ensure all cut-buffer properties are defined, to prevent errors.
  (declare (type display display)
	   (type int16 delta)
	   (type generalized-boolean careful-p))
  (let* ((root (screen-root (first (display-roots display))))
	 (buffers '#(:cut_buffer0 :cut_buffer1 :cut_buffer2 :cut_buffer3
		     :cut_buffer4 :cut_buffer5 :cut_buffer6 :cut_buffer7)))
    (when careful-p
      (let ((props (list-properties root)))
	(dotimes (i 8)
	  (unless (member (aref buffers i) props)
	    (setf (cut-buffer display :buffer i) "")))))
    (rotate-properties root buffers delta)))

