;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

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

(defun create-window (&key
		      window
		      (parent (required-arg parent))
		      (x (required-arg x))
		      (y (required-arg y))
		      (width (required-arg width))
		      (height (required-arg height))
		      (depth 0) (border-width 0)
		      (class :copy) (visual :copy)
		      background border
		      bit-gravity gravity
		      backing-store backing-planes backing-pixel save-under
		      event-mask do-not-propagate-mask override-redirect
		      colormap cursor)
  ;; Display is obtained from parent.  Only non-nil attributes are passed on in
  ;; the request: the function makes no assumptions about what the actual protocol
  ;; defaults are.  Width and height are the inside size, excluding border.
  (declare (type (or null window) window)
	   (type window parent)		; required
	   (type int16 x y) ;required
	   (type card16 width height) ;required
	   (type card16 depth border-width)
	   (type (member :copy :input-output :input-only) class)
	   (type (or (member :copy) visual-info resource-id) visual)
	   (type (or null (member :none :parent-relative) pixel pixmap) background)
	   (type (or null (member :copy) pixel pixmap) border)
	   (type (or null bit-gravity) bit-gravity)
	   (type (or null win-gravity) gravity)
	   (type (or null (member :not-useful :when-mapped :always)) backing-store)
	   (type (or null pixel) backing-planes backing-pixel)
	   (type (or null event-mask) event-mask)
	   (type (or null device-event-mask) do-not-propagate-mask)
	   (type (or null (member :on :off)) save-under override-redirect)
	   (type (or null (member :copy) colormap) colormap)
	   (type (or null (member :none) cursor) cursor))
  (declare (clx-values window))
  (let* ((display (window-display parent))
	 (window (or window (make-window :display display)))
	 (wid (allocate-resource-id display window 'window))
	 back-pixmap back-pixel
	 border-pixmap border-pixel)
    (declare (type display display)
	     (type window window)
	     (type resource-id wid)
	     (type (or null resource-id) back-pixmap border-pixmap)
	     (type (or null pixel) back-pixel border-pixel))
    (setf (window-id window) wid)
    (case background
      ((nil) nil)
      (:none (setq back-pixmap 0))
      (:parent-relative (setq back-pixmap 1))
      (otherwise
       (if (type? background 'pixmap)
	   (setq back-pixmap (pixmap-id background))
	 (if (integerp background)
	     (setq back-pixel background)
	   (x-type-error background
			 '(or null (member :none :parent-relative) integer pixmap))))))
    (case border
      ((nil) nil)
      (:copy (setq border-pixmap 0))
      (otherwise
       (if (type? border 'pixmap)
	   (setq border-pixmap (pixmap-id border))
	 (if (integerp border)
	     (setq border-pixel border)
	   (x-type-error border '(or null (member :copy) integer pixmap))))))
    (when event-mask
      (setq event-mask (encode-event-mask event-mask)))
    (when do-not-propagate-mask
      (setq do-not-propagate-mask (encode-device-event-mask do-not-propagate-mask)))

						;Make the request
    (with-buffer-request (display +x-createwindow+)
      (data depth)
      (resource-id wid)
      (window parent)
      (int16 x y)
      (card16 width height border-width)
      ((member16 :copy :input-output :input-only) class)
      (resource-id (cond ((eq visual :copy)
			  0)
			 ((typep visual 'resource-id)
			  visual)
			 (t
			  (visual-info-id visual))))
      (mask (card32 back-pixmap back-pixel border-pixmap border-pixel)
	    ((member-vector +bit-gravity-vector+) bit-gravity)
	    ((member-vector +win-gravity-vector+) gravity)
	    ((member :not-useful :when-mapped :always) backing-store)
	    (card32  backing-planes backing-pixel)
	    ((member :off :on) override-redirect save-under)
	    (card32 event-mask do-not-propagate-mask)
	    ((or (member :copy) colormap) colormap)
	    ((or (member :none) cursor) cursor)))
    window))

(defun destroy-window (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) +x-destroywindow+)
    (window window)))

(defun destroy-subwindows (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) +x-destroysubwindows+)
    (window window)))

(defun add-to-save-set (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) +x-changesaveset+)
    (data 0)
    (window window)))

(defun remove-from-save-set (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) +x-changesaveset+)
    (data 1)
    (window window)))

(defun reparent-window (window parent x y)
  (declare (type window window parent)
	   (type int16 x y))
  (with-buffer-request ((window-display window) +x-reparentwindow+)
    (window window parent)
    (int16 x y)))

(defun map-window (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) +x-mapwindow+)
    (window window)))

(defun map-subwindows (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) +x-mapsubwindows+)
    (window window)))

(defun unmap-window (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) +x-unmapwindow+)
    (window window)))

(defun unmap-subwindows (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) +x-unmapsubwindows+)
    (window window)))

(defun circulate-window-up (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) +x-circulatewindow+)
    (data 0)
    (window window)))

(defun circulate-window-down (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) +x-circulatewindow+)
    (data 1)
    (window window)))

(defun query-tree (window &key (result-type 'list))
  (declare (type window window)
	   (type t result-type)) ;;type specifier
  (declare (clx-values (clx-sequence window) parent root))
  (let ((display (window-display window)))
    (multiple-value-bind (root parent sequence)
	(with-buffer-request-and-reply (display +x-querytree+ nil :sizes (8 16 32))
	     ((window window))
	  (values
	    (window-get 8)
	    (resource-id-get 12)
	    (sequence-get :length (card16-get 16) :result-type result-type
			  :index +replysize+)))
      ;; Parent is NIL for root window
      (setq parent (and (plusp parent) (lookup-window display parent)))
      (dotimes (i (length sequence))		; Convert ID's to window's
	(setf (elt sequence i) (lookup-window display (elt sequence i))))
      (values sequence parent root))))

;; Although atom-ids are not visible in the normal user interface, atom-ids might
;; appear in window properties and other user data, so conversion hooks are needed.

(defun intern-atom (display name)
  (declare (type display display)
	   (type xatom name))
  (declare (clx-values resource-id))
  (let ((name (if (or (null name) (keywordp name))
		  name
		(kintern (string name)))))
    (declare (type symbol name))
    (or (atom-id name display)
	(let ((string (symbol-name name)))
	  (declare (type string string))
	  (multiple-value-bind (id)
	      (with-buffer-request-and-reply (display +x-internatom+ 12 :sizes 32)
		   ((data 0)
		    (card16 (length string))
		    (pad16 nil)
		    (string string))
		(values
		  (resource-id-get 8)))
	    (declare (type resource-id id))
	    (setf (atom-id name display) id)
	    id)))))

(defun find-atom (display name)
  ;; Same as INTERN-ATOM, but with the ONLY-IF-EXISTS flag True
  (declare (type display display)
	   (type xatom name))
  (declare (clx-values (or null resource-id)))
  (let ((name (if (or (null name) (keywordp name))
		  name
		(kintern (string name)))))
    (declare (type symbol name))
    (or (atom-id name display)
	(let ((string (symbol-name name)))
	  (declare (type string string))
	  (multiple-value-bind (id)
	      (with-buffer-request-and-reply (display +x-internatom+ 12 :sizes 32)
		   ((data 1)
		    (card16 (length string))
		    (pad16 nil)
		    (string string))
		(values
		  (or-get 8 null resource-id)))
	    (declare (type (or null resource-id) id))
	    (when id 
	      (setf (atom-id name display) id))
	    id)))))

(defun atom-name (display atom-id)
  (declare (type display display)
	   (type resource-id atom-id))
  (declare (clx-values keyword))
  (if (zerop atom-id)
      nil
  (or (id-atom atom-id display)
      (let ((keyword
	      (kintern
		  (with-buffer-request-and-reply
		       (display +x-getatomname+ nil :sizes (16))
		     ((resource-id atom-id))
		  (values
		    (string-get (card16-get 8) +replysize+))))))
	(declare (type keyword keyword))
	(setf (atom-id keyword display) atom-id)
	  keyword))))

;;; For binary compatibility with older code
(defun lookup-xatom (display atom-id)
  (declare (type display display)
	   (type resource-id atom-id))
  (atom-name display atom-id))

(defun change-property (window property data type format
		       &key (mode :replace) (start 0) end transform)
  ; Start and end affect sub-sequence extracted from data.
  ; Transform is applied to each extracted element.
  (declare (type window window)
	   (type xatom property type)
	   (type (member 8 16 32) format)
	   (type sequence data)
	   (type (member :replace :prepend :append) mode)
	   (type array-index start)
	   (type (or null array-index) end)
	   (type (or null (function (t) integer)) transform))
  (unless end (setq end (length data)))
  (let* ((display (window-display window))
	 (length (index- end start))
	 (property-id (intern-atom display property))
	 (type-id (intern-atom display type)))
    (declare (type display display)
	     (type array-index length)
	     (type resource-id property-id type-id))
    (with-buffer-request (display +x-changeproperty+)
      ((data (member :replace :prepend :append)) mode)
      (window window)
      (resource-id property-id type-id)
      (card8 format)
      (card32 length)
      (progn
	(ecase format
	  (8  (sequence-put 24 data :format card8
			    :start start :end end :transform transform))
	  (16 (sequence-put 24 data :format card16
			    :start start :end end :transform transform))
	  (32 (sequence-put 24 data :format card32
			    :start start :end end :transform transform)))))))

(defun delete-property (window property)
  (declare (type window window)
	   (type xatom property))
  (let* ((display (window-display window))
	 (property-id (intern-atom display property)))
    (declare (type display display)
	     (type resource-id property-id))
    (with-buffer-request (display +x-deleteproperty+)
      (window window)
      (resource-id property-id))))

(defun get-property (window property
		     &key type (start 0) end delete-p (result-type 'list) transform)
  ;; Transform is applied to each integer retrieved.
  (declare (type window window)
	   (type xatom property)
	   (type (or null xatom) type)
	   (type array-index start)
	   (type (or null array-index) end)
	   (type generalized-boolean delete-p)
	   (type t result-type)			;a sequence type
	   (type (or null (function (integer) t)) transform))
  (declare (clx-values data (or null type) format bytes-after))
  (let* ((display (window-display window))
	 (property-id (intern-atom display property))
	 (type-id (and type (intern-atom display type))))
    (declare (type display display)
	     (type resource-id property-id)
	     (type (or null resource-id) type-id))
    (multiple-value-bind (reply-format reply-type bytes-after data)
	(with-buffer-request-and-reply (display +x-getproperty+ nil :sizes (8 32))
	     (((data boolean) delete-p)
	      (window window)
	      (resource-id property-id)
	      ((or null resource-id) type-id)
	      (card32 start)
	      (card32 (index- (or end 64000) start)))
	  (let ((reply-format (card8-get 1))
		(reply-type (card32-get 8))
		(bytes-after (card32-get 12))
		(nitems (card32-get 16)))
	    (values
	      reply-format
	      reply-type
	      bytes-after
	      (and (plusp nitems)
		   (ecase reply-format
		     (0  nil) ;; (make-sequence result-type 0) ;; Property not found.
		     (8  (sequence-get :result-type result-type :format card8
				       :length nitems :transform transform
				       :index +replysize+))
		     (16 (sequence-get :result-type result-type :format card16
				       :length nitems :transform transform
				       :index +replysize+))
		     (32 (sequence-get :result-type result-type :format card32
				       :length nitems :transform transform
				       :index +replysize+)))))))
      (values data
	      (and (plusp reply-type) (atom-name display reply-type))
	      reply-format
	      bytes-after))))

(defun rotate-properties (window properties &optional (delta 1))
  ;; Positive rotates left, negative rotates right (opposite of actual protocol request).
  (declare (type window window)
	   (type sequence properties) ;; sequence of xatom
	   (type int16 delta))
  (let* ((display (window-display window))
	 (length (length properties))
	 (sequence (make-array length)))
    (declare (type display display)
	     (type array-index length))
    (with-vector (sequence vector)
      ;; Atoms must be interned before the RotateProperties request
      ;; is started to allow InternAtom requests to be made.
      (dotimes (i length)
	(setf (aref sequence i) (intern-atom display (elt properties i))))
      (with-buffer-request (display +x-rotateproperties+)
	(window window)
	(card16 length)
	(int16 (- delta))
	((sequence :end length) sequence))))
  nil)

(defun list-properties (window &key (result-type 'list))
  (declare (type window window)
	   (type t result-type)) ;; a sequence type
  (declare (clx-values (clx-sequence keyword)))
  (let ((display (window-display window)))
    (multiple-value-bind (seq)
	(with-buffer-request-and-reply (display +x-listproperties+ nil :sizes 16)
	     ((window window))
	  (values
	    (sequence-get :result-type result-type :length (card16-get 8)
			  :index +replysize+)))
      ;; lookup the atoms in the sequence
      (if (listp seq)
	  (do ((elt seq (cdr elt)))
	      ((endp elt) seq)
	    (setf (car elt) (atom-name display (car elt))))
	(dotimes (i (length seq) seq)
	  (setf (aref seq i) (atom-name display (aref seq i))))))))

(defun selection-owner (display selection)
  (declare (type display display)
	   (type xatom selection))
  (declare (clx-values (or null window)))
  (let ((selection-id (intern-atom display selection)))
    (declare (type resource-id selection-id))
    (multiple-value-bind (window)
	(with-buffer-request-and-reply (display +x-getselectionowner+ 12 :sizes 32)
	     ((resource-id selection-id))
	  (values
	    (resource-id-or-nil-get 8)))
      (and window (lookup-window display window)))))

(defun set-selection-owner (display selection owner &optional time)
  (declare (type display display)
	   (type xatom selection)
	   (type (or null window) owner)
	   (type timestamp time))
  (let ((selection-id (intern-atom display selection)))
    (declare (type resource-id selection-id))
    (with-buffer-request (display +x-setselectionowner+)
      ((or null window) owner)
      (resource-id selection-id)
      ((or null card32) time))
    owner))

(defsetf selection-owner (display selection &optional time) (owner)
  ;; A bit strange, but retains setf form.
  `(set-selection-owner ,display ,selection ,owner ,time))

(defun convert-selection (selection type requestor &optional property time)
  (declare (type xatom selection type)
	   (type window requestor)
	   (type (or null xatom) property)
	   (type timestamp time))
  (let* ((display (window-display requestor))
	 (selection-id (intern-atom display selection))
	 (type-id (intern-atom display type))
	 (property-id (and property (intern-atom display property))))
    (declare (type display display)
	     (type resource-id selection-id type-id)
	     (type (or null resource-id) property-id))
    (with-buffer-request (display +x-convertselection+)
      (window requestor)
      (resource-id selection-id type-id)
      ((or null resource-id) property-id)
      ((or null card32) time))))

(defun send-event (window event-key event-mask &rest args
		   &key propagate-p display &allow-other-keys)
  ;; Additional arguments depend on event-key, and are as specified further below
  ;; with declare-event, except that both resource-ids and resource objects are
  ;; accepted in the event components.  The display argument is only required if the
  ;; window is :pointer-window or :input-focus.
  (declare (type (or window (member :pointer-window :input-focus)) window)
	   (type event-key event-key)
	   (type (or null event-mask) event-mask)
	   (type generalized-boolean propagate-p)
	   (type (or null display) display)
	   (dynamic-extent args))
  (unless event-mask (setq event-mask 0))
  (unless display (setq display (window-display window)))
  (let ((internal-event-code (get-event-code event-key))
	(external-event-code (get-external-event-code display event-key)))
    (declare (type card8 internal-event-code external-event-code))
    ;; Ensure keyword atom-id's are cached
    (dolist (arg (cdr (assoc event-key '((:property-notify :atom)
					 (:selection-clear :selection)
					 (:selection-request :selection :target :property)
					 (:selection-notify :selection :target :property)
					 (:client-message :type))
			     :test #'eq)))
      (let ((keyword (getf args arg)))
	(intern-atom display keyword)))
    ;; Make the sendevent request
    (with-buffer-request (display +x-sendevent+)
      ((data boolean) propagate-p)
      (length 11) ;; 3 word request + 8 words for event = 11
      ((or (member :pointer-window :input-focus) window) window)
      (card32 (encode-event-mask event-mask))
      (card8 external-event-code)
      (progn
	(apply (svref *event-send-vector* internal-event-code) display args)
	(setf (buffer-boffset display) (index+ buffer-boffset 44))))))

(defun grab-pointer (window event-mask
		     &key owner-p sync-pointer-p sync-keyboard-p confine-to cursor time)
  (declare (type window window)
	   (type pointer-event-mask event-mask)
	   (type generalized-boolean owner-p sync-pointer-p sync-keyboard-p)
	   (type (or null window) confine-to)
	   (type (or null cursor) cursor)
	   (type timestamp time))
  (declare (clx-values grab-status))
  (let ((display (window-display window)))
    (with-buffer-request-and-reply (display +x-grabpointer+ nil :sizes 8)
	 (((data boolean) owner-p)
	  (window window)
	  (card16 (encode-pointer-event-mask event-mask))
	  (boolean (not sync-pointer-p) (not sync-keyboard-p))
	  ((or null window) confine-to)
	  ((or null cursor) cursor)
	  ((or null card32) time))
      (values
	(member8-get 1 :success :already-grabbed :invalid-time :not-viewable :frozen)))))

(defun ungrab-pointer (display &key time)
  (declare (type timestamp time))
  (with-buffer-request (display +x-ungrabpointer+)
    ((or null card32) time)))

(defun grab-button (window button event-mask
		    &key (modifiers :any)
			 owner-p sync-pointer-p sync-keyboard-p confine-to cursor)
  (declare (type window window)
	   (type (or (member :any) card8) button)
	   (type modifier-mask modifiers)
	   (type pointer-event-mask event-mask)
	   (type generalized-boolean owner-p sync-pointer-p sync-keyboard-p)
	   (type (or null window) confine-to)
	   (type (or null cursor) cursor))
  (with-buffer-request ((window-display window) +x-grabbutton+)
    ((data boolean) owner-p)
    (window window)
    (card16 (encode-pointer-event-mask event-mask))
    (boolean (not sync-pointer-p) (not sync-keyboard-p))
    ((or null window) confine-to)
    ((or null cursor) cursor)
    (card8 (if (eq button :any) 0 button))
    (pad8 1)
    (card16 (encode-modifier-mask modifiers))))

(defun ungrab-button (window button &key (modifiers :any))
  (declare (type window window)
	   (type (or (member :any) card8) button)
	   (type modifier-mask modifiers))
  (with-buffer-request ((window-display window) +x-ungrabbutton+)
    (data (if (eq button :any) 0 button))
    (window window)
    (card16 (encode-modifier-mask modifiers))))

(defun change-active-pointer-grab (display event-mask &optional cursor time)
  (declare (type display display)
	   (type pointer-event-mask event-mask)
	   (type (or null cursor) cursor)
	   (type timestamp time))
  (with-buffer-request (display +x-changeactivepointergrab+)
    ((or null cursor) cursor)
    ((or null card32) time)
    (card16 (encode-pointer-event-mask event-mask))))

(defun grab-keyboard (window &key owner-p sync-pointer-p sync-keyboard-p time)
  (declare (type window window)
	   (type generalized-boolean owner-p sync-pointer-p sync-keyboard-p)
	   (type timestamp time))
  (declare (clx-values grab-status))
  (let ((display (window-display window)))
    (with-buffer-request-and-reply (display +x-grabkeyboard+ nil :sizes 8)
	 (((data boolean) owner-p)
	  (window window)
	  ((or null card32) time)
	  (boolean (not sync-pointer-p) (not sync-keyboard-p)))
      (values
	(member8-get 1 :success :already-grabbed :invalid-time :not-viewable :frozen)))))

(defun ungrab-keyboard (display &key time)
  (declare (type display display)
	   (type timestamp time))
  (with-buffer-request (display +x-ungrabkeyboard+)
    ((or null card32) time)))

(defun grab-key (window key &key (modifiers 0) owner-p sync-pointer-p sync-keyboard-p)
  (declare (type window window)
	   (type generalized-boolean owner-p sync-pointer-p sync-keyboard-p)
	   (type (or (member :any) card8) key)
	   (type modifier-mask modifiers))
  (with-buffer-request ((window-display window) +x-grabkey+)
    ((data boolean) owner-p)
    (window window)
    (card16 (encode-modifier-mask modifiers))
    (card8 (if (eq key :any) 0 key))
    (boolean (not sync-pointer-p) (not sync-keyboard-p))))

(defun ungrab-key (window key &key (modifiers 0))
  (declare (type window window)
	   (type (or (member :any) card8) key)
	   (type modifier-mask modifiers))
  (with-buffer-request ((window-display window) +x-ungrabkey+)
    (data (if (eq key :any) 0 key))
    (window window)
    (card16 (encode-modifier-mask modifiers))))

(defun allow-events (display mode &optional time)
  (declare (type display display)
	   (type (member :async-pointer :sync-pointer :replay-pointer
			 :async-keyboard :sync-keyboard :replay-keyboard
			 :async-both :sync-both)
		 mode)
	   (type timestamp time))
  (with-buffer-request (display +x-allowevents+)
    ((data (member :async-pointer :sync-pointer :replay-pointer
		   :async-keyboard :sync-keyboard :replay-keyboard
		   :async-both :sync-both))
     mode)
    ((or null card32) time)))

(defun grab-server (display)
  (declare (type display display))
  (with-buffer-request (display +x-grabserver+)))

(defun ungrab-server (display)
  (with-buffer-request (display +x-ungrabserver+)))

(defmacro with-server-grabbed ((display) &body body)
  ;; The body is not surrounded by a with-display.
  (let ((disp (if (symbolp display) display (gensym))))
    `(let ((,disp ,display))
       (declare (type display ,disp))
       (unwind-protect
	   (progn
	     (grab-server ,disp)
	     ,@body)
	 (ungrab-server ,disp)))))

(defun query-pointer (window)
  (declare (type window window))
  (declare (clx-values x y same-screen-p child mask root-x root-y root))
  (let ((display (window-display window)))
    (with-buffer-request-and-reply (display +x-querypointer+ 26 :sizes (8 16 32))
	 ((window window))
      (values
	(int16-get 20)
	(int16-get 22)
	(boolean-get 1)
	(or-get 12 null window)
	(card16-get 24)
	(int16-get 16)
	(int16-get 18)
	(window-get 8)))))

(defun pointer-position (window)
  (declare (type window window))
  (declare (clx-values x y same-screen-p))
  (let ((display (window-display window)))
    (with-buffer-request-and-reply (display +x-querypointer+ 24 :sizes (8 16))
	 ((window window))
      (values
	(int16-get 20)
	(int16-get 22)
	(boolean-get 1)))))

(defun global-pointer-position (display)
  (declare (type display display))
  (declare (clx-values root-x root-y root))
  (with-buffer-request-and-reply (display +x-querypointer+ 20 :sizes (16 32))
       ((window (screen-root (first (display-roots display)))))
    (values
      (int16-get 16)
      (int16-get 18)
      (window-get 8))))

(defun motion-events (window &key start stop (result-type 'list))
  (declare (type window window)
	   (type timestamp start stop)
	   (type t result-type)) ;; a type specifier
  (declare (clx-values (repeat-seq (integer x) (integer y) (timestamp time))))
  (let ((display (window-display window)))
    (with-buffer-request-and-reply (display +x-getmotionevents+ nil :sizes 32)
	 ((window window)
	  ((or null card32) start stop))
      (values
	(sequence-get :result-type result-type :length (index* (card32-get 8) 3)
		      :index +replysize+)))))

(defun translate-coordinates (src src-x src-y dst)
  ;; Returns NIL when not on the same screen
  (declare (type window src)
	   (type int16 src-x src-y)
	   (type window dst))
  (declare (clx-values dst-x dst-y child))
  (let ((display (window-display src)))
    (with-buffer-request-and-reply (display +x-translatecoords+ 16 :sizes (8 16 32))
	 ((window src dst)
	  (int16 src-x src-y))
      (and (boolean-get 1)
	   (values
	     (int16-get 12)
	     (int16-get 14)
	     (or-get 8 null window))))))

(defun warp-pointer (dst dst-x dst-y)
  (declare (type window dst)
	   (type int16 dst-x dst-y))
  (with-buffer-request ((window-display dst) +x-warppointer+)
    (resource-id 0) ;; None
    (window dst)
    (int16 0 0)
    (card16 0 0)
    (int16 dst-x dst-y)))

(defun warp-pointer-relative (display x-off y-off)
  (declare (type display display)
	   (type int16 x-off y-off))
  (with-buffer-request (display +x-warppointer+)
    (resource-id 0) ;; None
    (resource-id 0) ;; None
    (int16 0 0)
    (card16 0 0)
    (int16 x-off y-off)))

(defun warp-pointer-if-inside (dst dst-x dst-y src src-x src-y
			       &optional src-width src-height)
  ;; Passing in a zero src-width or src-height is a no-op.
  ;; A null src-width or src-height translates into a zero value in the protocol request.
  (declare (type window dst src)
	   (type int16 dst-x dst-y src-x src-y)
	   (type (or null card16) src-width src-height))
  (unless (or (eql src-width 0) (eql src-height 0))
    (with-buffer-request ((window-display dst) +x-warppointer+)
      (window src dst)
      (int16 src-x src-y)
      (card16 (or src-width 0) (or src-height 0))
      (int16 dst-x dst-y))))

(defun warp-pointer-relative-if-inside (x-off y-off src src-x src-y
					&optional src-width src-height)
  ;; Passing in a zero src-width or src-height is a no-op.
  ;; A null src-width or src-height translates into a zero value in the protocol request.
  (declare (type window src)
	   (type int16 x-off y-off src-x src-y)
	   (type (or null card16) src-width src-height))
  (unless (or (eql src-width 0) (eql src-height 0))
    (with-buffer-request ((window-display src) +x-warppointer+)
      (window src)
      (resource-id 0) ;; None
      (int16 src-x src-y)
      (card16 (or src-width 0) (or src-height 0))
      (int16 x-off y-off))))

(defun set-input-focus (display focus revert-to &optional time)
  (declare (type display display)
	   (type (or (member :none :pointer-root) window) focus)
	   (type (member :none :pointer-root :parent) revert-to)
	   (type timestamp time))
  (with-buffer-request (display +x-setinputfocus+)
    ((data (member :none :pointer-root :parent)) revert-to)
    ((or window (member :none :pointer-root)) focus)
    ((or null card32) time)))

(defun input-focus (display)
  (declare (type display display))
  (declare (clx-values focus revert-to))
  (with-buffer-request-and-reply (display +x-getinputfocus+ 16 :sizes (8 32))
       ()
    (values
      (or-get 8 window (member :none :pointer-root))
      (member8-get 1 :none :pointer-root :parent))))

(defun query-keymap (display &optional bit-vector)
  (declare (type display display)
	   (type (or null (bit-vector 256)) bit-vector))
  (declare (clx-values (bit-vector 256)))
  (with-buffer-request-and-reply (display +x-querykeymap+ 40 :sizes 8)
       ()
    (values
      (bit-vector256-get 8 8 bit-vector))))

(defun create-pixmap (&key
		      pixmap
		      (width (required-arg width))
		      (height (required-arg height))
		      (depth (required-arg depth))
		      (drawable (required-arg drawable)))
  (declare (type (or null pixmap) pixmap)
	   (type card8 depth) ;; required
	   (type card16 width height) ;; required
	   (type drawable drawable)) ;; required
  (declare (clx-values pixmap))
  (let* ((display (drawable-display drawable))
	 (pixmap (or pixmap (make-pixmap :display display)))
	 (pid (allocate-resource-id display pixmap 'pixmap)))
    (setf (pixmap-id pixmap) pid)
    (with-buffer-request (display +x-createpixmap+)
      (data depth)
      (resource-id pid)
      (drawable drawable)
      (card16 width height))
    pixmap))

(defun free-pixmap (pixmap)
  (declare (type pixmap pixmap))
  (let ((display (pixmap-display pixmap)))
    (with-buffer-request (display +x-freepixmap+)
      (pixmap pixmap))
    (deallocate-resource-id display (pixmap-id pixmap) 'pixmap)))

(defun clear-area (window &key (x 0) (y 0) width height exposures-p)
  ;; Passing in a zero width or height is a no-op.
  ;; A null width or height translates into a zero value in the protocol request.
  (declare (type window window)
	   (type int16 x y)
	   (type (or null card16) width height)
	   (type generalized-boolean exposures-p))
  (unless (or (eql width 0) (eql height 0))
    (with-buffer-request ((window-display window) +x-cleartobackground+)
      ((data boolean) exposures-p)
      (window window)
      (int16 x y)
      (card16 (or width 0) (or height 0)))))

(defun copy-area (src gcontext src-x src-y width height dst dst-x dst-y)
  (declare (type drawable src dst)
	   (type gcontext gcontext)
	   (type int16 src-x src-y dst-x dst-y)
	   (type card16 width height))
  (with-buffer-request ((drawable-display src) +x-copyarea+ :gc-force gcontext)
    (drawable src dst)
    (gcontext gcontext)
    (int16 src-x src-y dst-x dst-y)
    (card16 width height)))

(defun copy-plane (src gcontext plane src-x src-y width height dst dst-x dst-y)
  (declare (type drawable src dst)
	   (type gcontext gcontext)
	   (type pixel plane)
	   (type int16 src-x src-y dst-x dst-y)
	   (type card16 width height))
  (with-buffer-request ((drawable-display src) +x-copyplane+ :gc-force gcontext)
    (drawable src dst)
    (gcontext gcontext)
    (int16 src-x src-y dst-x dst-y)
    (card16 width height)
    (card32 plane)))

(defun create-colormap (visual-info window &optional alloc-p)
  (declare (type (or visual-info resource-id) visual-info)
	   (type window window)
	   (type generalized-boolean alloc-p))
  (declare (clx-values colormap))
  (let ((display (window-display window)))
    (when (typep visual-info 'resource-id)
      (setf visual-info (visual-info display visual-info)))
    (let* ((colormap (make-colormap :display display :visual-info visual-info))
	   (id (allocate-resource-id display colormap 'colormap)))
      (setf (colormap-id colormap) id)
      (with-buffer-request (display +x-createcolormap+)
	((data boolean) alloc-p)
	(card29 id)
	(window window)
	(card29 (visual-info-id visual-info)))
      colormap)))

(defun free-colormap (colormap)
  (declare (type colormap colormap))
  (let ((display (colormap-display colormap)))
    (with-buffer-request (display +x-freecolormap+)
      (colormap colormap))
    (deallocate-resource-id display (colormap-id colormap) 'colormap)))

(defun copy-colormap-and-free (colormap)
  (declare (type colormap colormap))
  (declare (clx-values colormap))
  (let* ((display (colormap-display colormap))
	 (new-colormap (make-colormap :display display
				      :visual-info (colormap-visual-info colormap)))
	 (id (allocate-resource-id display new-colormap 'colormap)))
    (setf (colormap-id new-colormap) id)
    (with-buffer-request (display +x-copycolormapandfree+)
      (resource-id id)
      (colormap colormap))
    new-colormap))

(defun install-colormap (colormap)
  (declare (type colormap colormap))
  (with-buffer-request ((colormap-display colormap) +x-installcolormap+)
    (colormap colormap)))

(defun uninstall-colormap (colormap)
  (declare (type colormap colormap))
  (with-buffer-request ((colormap-display colormap) +x-uninstallcolormap+)
    (colormap colormap)))

(defun installed-colormaps (window &key (result-type 'list))
  (declare (type window window)
	   (type t result-type)) ;; CL type
  (declare (clx-values (clx-sequence colormap)))
  (let ((display (window-display window)))
    (flet ((get-colormap (id)
	     (lookup-colormap display id)))
      (with-buffer-request-and-reply (display +x-listinstalledcolormaps+ nil :sizes 16)
	   ((window window))
	(values
	  (sequence-get :result-type result-type :length (card16-get 8)
			:transform #'get-colormap :index +replysize+))))))

(defun alloc-color (colormap color)
  (declare (type colormap colormap)
	   (type (or stringable color) color))
  (declare (clx-values pixel screen-color exact-color))
  (let ((display (colormap-display colormap)))
    (etypecase color
      (color
	(with-buffer-request-and-reply (display +x-alloccolor+ 20 :sizes (16 32))
	     ((colormap colormap)
	      (rgb-val (color-red color)
		       (color-green color)
		       (color-blue color))
	      (pad16 nil))
	  (values
	    (card32-get 16)
	    (make-color :red (rgb-val-get 8)
			:green (rgb-val-get 10)
			:blue (rgb-val-get 12))
	    color)))
      (stringable
	(let* ((string (string color))
	       (length (length string)))
	  (with-buffer-request-and-reply (display +x-allocnamedcolor+ 24 :sizes (16 32))
	       ((colormap colormap)
		(card16 length)
		(pad16 nil)
		(string string))
	    (values
	      (card32-get 8)
	      (make-color :red (rgb-val-get 18)
			  :green (rgb-val-get 20)
			  :blue (rgb-val-get 22))
	      (make-color :red (rgb-val-get 12)
			  :green (rgb-val-get 14)
			  :blue (rgb-val-get 16)))))))))

(defun alloc-color-cells (colormap colors &key (planes 0) contiguous-p (result-type 'list))
  (declare (type colormap colormap)
	   (type card16 colors planes)
	   (type generalized-boolean contiguous-p)
	   (type t result-type)) ;; CL type
  (declare (clx-values (clx-sequence pixel) (clx-sequence mask)))
  (let ((display (colormap-display colormap)))
    (with-buffer-request-and-reply (display +x-alloccolorcells+ nil :sizes 16)
	 (((data boolean) contiguous-p)
	  (colormap colormap)
	  (card16 colors planes))
      (let ((pixel-length (card16-get 8))
	    (mask-length (card16-get 10)))
	(values
	  (sequence-get :result-type result-type :length pixel-length :index +replysize+)
	  (sequence-get :result-type result-type :length mask-length
			:index (index+ +replysize+ (index* pixel-length 4))))))))

(defun alloc-color-planes (colormap colors
			   &key (reds 0) (greens 0) (blues 0)
			   contiguous-p (result-type 'list))
  (declare (type colormap colormap)
	   (type card16 colors reds greens blues)
	   (type generalized-boolean contiguous-p)
	   (type t result-type)) ;; CL type
  (declare (clx-values (clx-sequence pixel) red-mask green-mask blue-mask))
  (let ((display (colormap-display colormap)))
    (with-buffer-request-and-reply (display +x-alloccolorplanes+ nil :sizes (16 32))
	 (((data boolean) contiguous-p)
	  (colormap colormap)
	  (card16 colors reds greens blues))
      (let ((red-mask (card32-get 12))
	    (green-mask (card32-get 16))
	    (blue-mask (card32-get 20)))
	(values
	  (sequence-get :result-type result-type :length (card16-get 8) :index +replysize+)
	  red-mask green-mask blue-mask)))))

(defun free-colors (colormap pixels &optional (plane-mask 0))
  (declare (type colormap colormap)
	   (type sequence pixels) ;; Sequence of integers
	   (type pixel plane-mask))
  (with-buffer-request ((colormap-display colormap) +x-freecolors+)
    (colormap colormap)
    (card32 plane-mask)
    (sequence pixels)))

(defun store-color (colormap pixel spec &key (red-p t) (green-p t) (blue-p t))
  (declare (type colormap colormap)
	   (type pixel pixel)
	   (type (or stringable color) spec)
	   (type generalized-boolean red-p green-p blue-p))
  (let ((display (colormap-display colormap))
	(flags 0))
    (declare (type display display)
	     (type card8 flags))
    (when red-p (setq flags 1))
    (when green-p (incf flags 2))
    (when blue-p (incf flags 4))
    (etypecase spec
      (color
	(with-buffer-request (display +x-storecolors+)
	  (colormap colormap)
	  (card32 pixel)
	  (rgb-val (color-red spec)
		   (color-green spec)
		   (color-blue spec))
	  (card8 flags)
	  (pad8 nil)))
      (stringable
	(let* ((string (string spec))
	       (length (length string)))
	  (with-buffer-request (display +x-storenamedcolor+)
	    ((data card8) flags)
	    (colormap colormap)
	    (card32 pixel)
	    (card16 length)
	    (pad16 nil)
	    (string string)))))))

(defun store-colors (colormap specs &key (red-p t) (green-p t) (blue-p t))
  ;; If stringables are specified for colors, it is unspecified whether all
  ;; stringables are first resolved and then a single StoreColors protocol request is
  ;; issued, or whether multiple StoreColors protocol requests are issued.
  (declare (type colormap colormap)
	   (type sequence specs)
	   (type generalized-boolean red-p green-p blue-p))
  (etypecase specs
    (list
      (do ((spec specs (cddr spec)))
	  ((endp spec))
	(store-color colormap (car spec) (cadr spec) :red-p red-p :green-p green-p :blue-p blue-p)))
    (vector
      (do ((i 0 (+ i 2))
	   (len (length specs)))
	  ((>= i len))
	(store-color colormap (aref specs i) (aref specs (1+ i)) :red-p red-p :green-p green-p :blue-p blue-p)))))

(defun query-colors (colormap pixels &key (result-type 'list))
  (declare (type colormap colormap)
	   (type sequence pixels) ;; sequence of integer
	   (type t result-type))   ;; a type specifier
  (declare (clx-values (clx-sequence color)))
  (let ((display (colormap-display colormap)))
    (with-buffer-request-and-reply (display +x-querycolors+ nil :sizes (8 16))
	 ((colormap colormap)
	  (sequence pixels))
      (let ((sequence (make-sequence result-type (card16-get 8))))
	(advance-buffer-offset +replysize+)
	(dotimes (i (length sequence) sequence)
	  (setf (elt sequence i)
		(make-color :red (rgb-val-get 0)
			    :green (rgb-val-get 2)
			    :blue (rgb-val-get 4)))
	  (advance-buffer-offset 8))))))

(defun lookup-color (colormap name)
  (declare (type colormap colormap)
	   (type stringable name))
  (declare (clx-values screen-color true-color))
  (let* ((display (colormap-display colormap))
	 (string (string name))
	 (length (length string)))
    (with-buffer-request-and-reply (display +x-lookupcolor+ 20 :sizes 16)
	 ((colormap colormap)
	  (card16 length)
	  (pad16 nil)
	  (string string))
      (values
	(make-color :red (rgb-val-get 14)
		    :green (rgb-val-get 16)
		    :blue (rgb-val-get 18))
	(make-color :red (rgb-val-get 8)
		    :green (rgb-val-get 10)
		    :blue (rgb-val-get 12))))))

(defun create-cursor (&key
		      (source (required-arg source))
		      mask
		      (x (required-arg x))
		      (y (required-arg y))
		      (foreground (required-arg foreground))
		      (background (required-arg background)))
  (declare (type pixmap source) ;; required
	   (type (or null pixmap) mask)
	   (type card16 x y) ;; required
	   (type (or null color) foreground background)) ;; required
  (declare (clx-values cursor))
  (let* ((display (pixmap-display source))
	 (cursor (make-cursor :display display))
	 (cid (allocate-resource-id display cursor 'cursor)))
    (setf (cursor-id cursor) cid)
    (with-buffer-request (display +x-createcursor+)
      (resource-id cid)
      (pixmap source)
      ((or null pixmap) mask)
      (rgb-val (color-red foreground)
	       (color-green foreground)
	       (color-blue foreground))
      (rgb-val (color-red background)
	       (color-green background)
	       (color-blue background))
      (card16 x y))
    cursor))

(defun create-glyph-cursor (&key
			    (source-font (required-arg source-font))
			    (source-char (required-arg source-char))
			    mask-font
			    mask-char
			    (foreground (required-arg foreground))
			    (background (required-arg background)))
  (declare (type font source-font) ;; Required
	   (type card16 source-char) ;; Required
	   (type (or null font) mask-font)
	   (type (or null card16) mask-char)
	   (type color foreground background)) ;; required
  (declare (clx-values cursor))
  (let* ((display (font-display source-font))
	 (cursor (make-cursor :display display))
	 (cid (allocate-resource-id display cursor 'cursor))
	 (source-font-id (font-id source-font))
	 (mask-font-id (if mask-font (font-id mask-font) 0)))
    (setf (cursor-id cursor) cid)
    (unless mask-char (setq mask-char 0))
    (with-buffer-request (display +x-createglyphcursor+)
      (resource-id cid source-font-id mask-font-id)
      (card16 source-char)
      (card16 mask-char)
      (rgb-val (color-red foreground)
	       (color-green foreground)
	       (color-blue foreground))
      (rgb-val (color-red background)
	       (color-green background)
	       (color-blue background)))
    cursor))

(defun free-cursor (cursor)
  (declare (type cursor cursor))
  (let ((display (cursor-display cursor)))
    (with-buffer-request (display +x-freecursor+)
      (cursor cursor))
    (deallocate-resource-id display (cursor-id cursor) 'cursor)))

(defun recolor-cursor (cursor foreground background)
  (declare (type cursor cursor)
	   (type color foreground background))
  (with-buffer-request ((cursor-display cursor) +x-recolorcursor+)
    (cursor cursor)
    (rgb-val (color-red foreground)
	     (color-green foreground)
	     (color-blue foreground))
    (rgb-val (color-red background)
	     (color-green background)
	     (color-blue background))
    ))

(defun query-best-cursor (width height drawable)
  (declare (type card16 width height)
	   (type (or drawable display) drawable))	
  (declare (clx-values width height))
  ;; Drawable can be a display for compatibility.
  (multiple-value-bind (display drawable)
      (if (type? drawable 'drawable)
	  (values (drawable-display drawable) drawable)
	(values drawable (screen-root (display-default-screen drawable))))
    (with-buffer-request-and-reply (display +x-querybestsize+ 12 :sizes 16)
	 ((data 0)
	  (window drawable)
	  (card16 width height))
      (values
	(card16-get 8)
	(card16-get 10)))))

(defun query-best-tile (width height drawable)
  (declare (type card16 width height)
	   (type drawable drawable))
  (declare (clx-values width height))
  (let ((display (drawable-display drawable)))
    (with-buffer-request-and-reply (display +x-querybestsize+ 12 :sizes 16)
	 ((data 1)
	  (drawable drawable)
	  (card16 width height))
      (values
	(card16-get 8)
	(card16-get 10)))))

(defun query-best-stipple (width height drawable)
  (declare (type card16 width height)
	   (type drawable drawable))
  (declare (clx-values width height))
  (let ((display (drawable-display drawable)))
    (with-buffer-request-and-reply (display +x-querybestsize+ 12 :sizes 16)
	 ((data 2)
	  (drawable drawable)
	  (card16 width height))
      (values
	(card16-get 8)
	(card16-get 10)))))

(defun query-extension (display name)
  (declare (type display display)
	   (type stringable name))
  (declare (clx-values major-opcode first-event first-error))
  (let ((string (string name)))
    (with-buffer-request-and-reply (display +x-queryextension+ 12 :sizes 8)
	 ((card16 (length string))
	  (pad16 nil)
	  (string string))
      (and (boolean-get 8)    ;; If present
	   (values
	     (card8-get 9)
	     (card8-get 10)
	     (card8-get 11))))))

(defun list-extensions (display &key (result-type 'list))
  (declare (type display display)
	   (type t result-type)) ;; CL type
  (declare (clx-values (clx-sequence string)))
  (with-buffer-request-and-reply (display +x-listextensions+ size :sizes 8)
       ()
    (values
      (read-sequence-string
	buffer-bbuf (index- size +replysize+) (card8-get 1) result-type +replysize+))))

(defun change-keyboard-control (display &key key-click-percent
				bell-percent bell-pitch bell-duration
				led led-mode key auto-repeat-mode)
  (declare (type display display)
	   (type (or null (member :default) int16) key-click-percent
						   bell-percent bell-pitch bell-duration)
	   (type (or null card8) led key)
	   (type (or null (member :on :off)) led-mode)
	   (type (or null (member :on :off :default)) auto-repeat-mode))
  (when (eq key-click-percent :default) (setq key-click-percent -1))
  (when (eq bell-percent :default) (setq bell-percent -1))
  (when (eq bell-pitch :default) (setq bell-pitch -1))
  (when (eq bell-duration :default) (setq bell-duration -1))
  (with-buffer-request (display +x-changekeyboardcontrol+ :sizes (32))
    (mask
      (integer key-click-percent bell-percent bell-pitch bell-duration)
      (card32 led)
      ((member :off :on) led-mode)
      (card32 key)
      ((member :off :on :default) auto-repeat-mode))))

(defun keyboard-control (display)
  (declare (type display display))
  (declare (clx-values key-click-percent bell-percent bell-pitch bell-duration
		  led-mask global-auto-repeat auto-repeats))
  (with-buffer-request-and-reply (display +x-getkeyboardcontrol+ 32 :sizes (8 16 32))
       ()
    (values
      (card8-get 12)
      (card8-get 13)
      (card16-get 14)
      (card16-get 16)
      (card32-get 8)
      (member8-get 1 :off :on)
      (bit-vector256-get 20))))

;;  The base volume should
;; be considered to be the "desired" volume in the normal case; that is, a
;; typical application should call XBell with 0 as the percent.  Rather
;; than using a simple sum, the percent argument is instead used as the
;; percentage of the remaining range to alter the base volume by.  That is,
;; the actual volume is:
;;	 if percent>=0:    base - [(base * percent) / 100] + percent
;;	 if percent<0:     base + [(base * percent) / 100]

(defun bell (display &optional (percent-from-normal 0))
  ;; It is assumed that an eventual audio extension to X will provide more complete control.
  (declare (type display display)
	   (type int8 percent-from-normal))
  (with-buffer-request (display +x-bell+)
    (data (int8->card8 percent-from-normal))))

(defun pointer-mapping (display &key (result-type 'list))
  (declare (type display display)
	   (type t result-type)) ;; CL type
  (declare (clx-values sequence)) ;; Sequence of card
  (with-buffer-request-and-reply (display +x-getpointermapping+ nil :sizes 8)
       ()
    (values
      (sequence-get :length (card8-get 1) :result-type result-type :format card8
		    :index +replysize+))))

(defun set-pointer-mapping (display map)
  ;; Can signal device-busy.
  (declare (type display display)
	   (type sequence map)) ;; Sequence of card8
  (when (with-buffer-request-and-reply (display +x-setpointermapping+ 2 :sizes 8)
	     ((data (length map))
	      ((sequence :format card8) map))
	  (values
	    (boolean-get 1)))
    (x-error 'device-busy :display display))
  map)

(defsetf pointer-mapping set-pointer-mapping)

(defun change-pointer-control (display &key acceleration threshold)
  ;; Acceleration is rationalized if necessary.
  (declare (type display display)
	   (type (or null (member :default) number) acceleration)
	   (type (or null (member :default) integer) threshold))
  (flet ((rationalize16 (number)
	   ;; Rationalize NUMBER into the ratio of two signed 16 bit numbers
	   (declare (type number number))
	   (declare (clx-values numerator denominator))
	   (do* ((rational (rationalize number))
		 (numerator (numerator rational) (ash numerator -1))
		 (denominator (denominator rational) (ash denominator -1)))
		((or (= numerator 1)
		     (and (< (abs numerator) #x8000)
			  (< denominator #x8000)))
		 (values
		   numerator (min denominator #x7fff))))))
    (declare (inline rationalize16))
    (let ((acceleration-p 1)
	  (threshold-p 1)
	  (numerator 0)
	  (denominator 1))
      (declare (type card8 acceleration-p threshold-p)
	       (type int16 numerator denominator))
      (cond ((eq acceleration :default) (setq numerator -1))
	    (acceleration (multiple-value-setq (numerator denominator)
			    (rationalize16 acceleration)))
	    (t (setq acceleration-p 0)))
      (cond ((eq threshold :default) (setq threshold -1))
	    ((null threshold) (setq threshold -1
				    threshold-p 0)))
      (with-buffer-request (display +x-changepointercontrol+)
	(int16 numerator denominator threshold)
	(card8 acceleration-p threshold-p)))))

(defun pointer-control (display)
  (declare (type display display))
  (declare (clx-values acceleration threshold))
  (with-buffer-request-and-reply (display +x-getpointercontrol+ 16 :sizes 16)
       ()
    (values
      (/ (card16-get 8) (card16-get 10))	; Should we float this?
      (card16-get 12))))

(defun set-screen-saver (display timeout interval blanking exposures)
  ;; Timeout and interval are in seconds, will be rounded to minutes.
  (declare (type display display)
	   (type (or (member :default) int16) timeout interval)
	   (type (member :on :off :default :yes :no) blanking exposures))
  (case blanking (:yes (setq blanking :on)) (:no (setq blanking :off)))
  (case exposures (:yes (setq exposures :on)) (:no (setq exposures :off)))
  (when (eq timeout :default) (setq timeout -1))
  (when (eq interval :default) (setq interval -1))
  (with-buffer-request (display +x-setscreensaver+)
    (int16 timeout interval)
    ((member8 :on :off :default) blanking exposures)))

(defun screen-saver (display)
  ;; Returns timeout and interval in seconds.
  (declare (type display display))
  (declare (clx-values timeout interval blanking exposures))
  (with-buffer-request-and-reply (display +x-getscreensaver+ 14 :sizes (8 16))
       ()
    (values
      (card16-get 8)
      (card16-get 10)
      (member8-get 12 :on :off :default)
      (member8-get 13 :on :off :default))))

(defun activate-screen-saver (display)
  (declare (type display display))
  (with-buffer-request (display +x-forcescreensaver+)
    (data 1)))

(defun reset-screen-saver (display)
  (declare (type display display))
  (with-buffer-request (display +x-forcescreensaver+)
    (data 0)))

(defun add-access-host (display host &optional (family :internet))
  ;; A string must be acceptable as a host, but otherwise the possible types for
  ;; host are not constrained, and will likely be very system dependent.
  ;; This implementation uses a list whose car is the family keyword
  ;; (:internet :DECnet :Chaos) and cdr is a list of network address bytes.
  (declare (type display display)
	   (type (or stringable list) host)
	   (type (or null (member :internet :decnet :chaos) card8) family))
  (change-access-host display host family nil))

(defun remove-access-host (display host &optional (family :internet))
  ;; A string must be acceptable as a host, but otherwise the possible types for
  ;; host are not constrained, and will likely be very system dependent.
  ;; This implementation uses a list whose car is the family keyword
  ;; (:internet :DECnet :Chaos) and cdr is a list of network address bytes.
  (declare (type display display)
	   (type (or stringable list) host)
	   (type (or null (member :internet :decnet :chaos) card8) family))
  (change-access-host display host family t))

(defun change-access-host (display host family remove-p)
  (declare (type display display)
	   (type (or stringable list) host)
	   (type (or null (member :internet :decnet :chaos) card8) family))
  (unless (consp host)
    (setq host (host-address host family)))
  (let ((family (car host))
	(address (cdr host)))
    (with-buffer-request (display +x-changehosts+)
      ((data boolean) remove-p)
      (card8 (encode-type (or null (member :internet :decnet :chaos) card32) family))
      (card16 (length address))
      ((sequence :format card8) address))))

(defun access-hosts (display &optional (result-type 'list))
  ;; The type of host objects returned is not constrained, except that the hosts must
  ;; be acceptable to add-access-host and remove-access-host.
  ;; This implementation uses a list whose car is the family keyword
  ;; (:internet :DECnet :Chaos) and cdr is a list of network address bytes.
  (declare (type display display)
	   (type t result-type)) ;; CL type
  (declare (clx-values (clx-sequence host) enabled-p))
  (with-buffer-request-and-reply (display +x-listhosts+ nil :sizes (8 16))
       ()
    (let* ((enabled-p (boolean-get 1))
	   (nhosts (card16-get 8))
	   (sequence (make-sequence result-type nhosts)))
      (advance-buffer-offset +replysize+)
      (dotimes (i nhosts)
	(let ((family (card8-get 0))
	      (len (card16-get 2)))
	  (setf (elt sequence i)
		(cons (if (< family 3)
			  (svref '#(:internet :decnet :chaos) family)
			family)
		      (sequence-get :length len :format card8 :result-type 'list
				    :index (+ buffer-boffset 4))))
	  (advance-buffer-offset (+ 4 (* 4 (ceiling len 4))))))
      (values
	sequence
	enabled-p))))

(defun access-control (display)
  (declare (type display display))
  (declare (clx-values generalized-boolean)) ;; True when access-control is ENABLED
  (with-buffer-request-and-reply (display +x-listhosts+ 2 :sizes 8)
       ()
    (boolean-get 1)))
  
(defun set-access-control (display enabled-p)
  (declare (type display display)
	   (type generalized-boolean enabled-p))
  (with-buffer-request (display +x-changeaccesscontrol+)
    ((data boolean) enabled-p))
  enabled-p)

(defsetf access-control set-access-control)

(defun close-down-mode (display)
  ;; setf'able
  ;; Cached locally in display object.
  (declare (type display display))
  (declare (clx-values (member :destroy :retain-permanent :retain-temporary nil)))
  (display-close-down-mode display))

(defun set-close-down-mode (display mode)
  ;; Cached locally in display object.
  (declare (type display display)
	   (type (member :destroy :retain-permanent :retain-temporary) mode))
  (setf (display-close-down-mode display) mode)
  (with-buffer-request (display +x-changeclosedownmode+ :sizes (32))
    ((data (member :destroy :retain-permanent :retain-temporary)) mode))
  mode)

(defsetf close-down-mode set-close-down-mode)

(defun kill-client (display resource-id)
  (declare (type display display)
	   (type resource-id resource-id))
  (with-buffer-request (display +x-killclient+)
    (resource-id resource-id)))

(defun kill-temporary-clients (display)
  (declare (type display display))
  (with-buffer-request (display +x-killclient+)
    (resource-id 0)))

(defun no-operation (display)
  (declare (type display display))
  (with-buffer-request (display +x-nooperation+)))
