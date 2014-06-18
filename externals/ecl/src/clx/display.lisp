;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;;; This file contains definitions for the DISPLAY object for Common-Lisp X windows version 11

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

;;; Authorizaton

(defparameter *known-authorizations* '("MIT-MAGIC-COOKIE-1"))

;;; X11 Authorization: to prevent malicious users from snooping on a
;;; display, X servers may require connection requests to be
;;; authorized. The X server (or display manager) will create a random
;;; key on startup, and store it as an entry in a file generally named
;;; $HOME/.Xauthority (see xauth(1) and the AUTHORITY-PATHNAME
;;; function). Clients must extract from this file the "magic cookie"
;;; that corresponds to the server they wish to connect to, and send
;;; it as authorization data when opening the display.

;;; The format of the .Xauthority file is documented in the XFree
;;; sources, in the file xc/lib/Xau/README.

;;; Stolen from the cmucl sources, with patches by Hannu Rummukainen and
;;; Scott Fahlman.

(defun read-xauth-entry (stream)
  (labels ((read-short (stream &optional (eof-errorp t))
             (let ((high-byte (read-byte stream eof-errorp)))
               (and high-byte
                    (dpb high-byte (byte 8 8) (read-byte stream)))))
           (read-short-length-string (stream)
             (let ((length (read-short stream)))
               (let ((string (make-string length)))
                 (dotimes (k length)
                   (setf (schar string k) (card8->char (read-byte stream))))
                 string)))
           (read-short-length-vector (stream)
             (let ((length (read-short stream)))
               (let ((vector (make-array length 
                                         :element-type '(unsigned-byte 8))))
                 (dotimes (k length)
                   (setf (aref vector k) (read-byte stream)))
                 vector))))
    (let ((family-id (read-short stream nil)))
      (if (null family-id)
          (list nil nil nil nil nil)
          (let* ((address-data (read-short-length-vector stream))
                 (number (parse-integer (read-short-length-string stream)))
                 (name (read-short-length-string stream))
                 (data (read-short-length-vector stream))
                 (family (car (rassoc family-id *protocol-families*))))
            (unless family
              (return-from read-xauth-entry
                ;; we return FAMILY-ID to signal to
                ;; GET-BEST-AUTHORIZATION that we haven't finished
                ;; with the stream.
                (list family-id nil nil nil nil)))
            (let ((address 
                   (case family
                     (:local (map 'string #'code-char address-data))
                     (:internet (coerce address-data 'list))
                     ;; FIXME: we can probably afford not to support
                     ;; :DECNET or :CHAOSNET in this modern age, but
                     ;; :INTERNET6 probably deserve support.  -- CSR,
                     ;; 2005-08-07
                     (t nil))))
              ;; if ADDRESS is NIL by this time, we will never match
              ;; the address of DISPLAY.
              (list family address number name data)))))))

(defun get-best-authorization (host display protocol)
  ;; parse .Xauthority, extract the cookie for DISPLAY on HOST.
  ;; PROTOCOL determines whether the server connection is using an
  ;; Internet protocol (value of :internet) or a non-network
  ;; protocol such as Unix domain sockets (value of :local).  Returns
  ;; two strings: an authorization name (very likely the string
  ;; "MIT-MAGIC-COOKIE-1") and an authorization key, represented as
  ;; fixnums in a vector.  If we fail to find an appropriate cookie,
  ;; return two empty strings.
  (let ((pathname (authority-pathname)))
    (when pathname
      (with-open-file (stream pathname :element-type '(unsigned-byte 8)
			      :if-does-not-exist nil)
	(when stream
	  (let* ((host-address (and (eql protocol :internet)
				    (rest (host-address host protocol))))
		 (best-name nil) (best-pos nil)
		 (best-data nil))
	    ;; Check for the localhost address, in which case we're
	    ;; really FamilyLocal.
	    (when (or (eql protocol :local)
		      (and (eql protocol :internet)
			   (equal host-address '(127 0 0 1))))
	      (setq host-address (get-host-name))
	      (setq protocol :local))
	    (loop
	     (destructuring-bind (family address number name data)
		 (read-xauth-entry stream)
	       (unless family (return))
	       (when (and (eql family protocol)
			  (equal host-address address)
			  (= number display)
			  (let ((pos1 (position name *known-authorizations*
						:test #'string=)))
			    (and pos1
				 (or (null best-pos)
				     (< pos1 best-pos)))))
		 (setf best-name name
		       best-pos (position name *known-authorizations*
					  :test #'string=)
		       best-data data))))
	    (when best-name
	      (return-from get-best-authorization
		(values best-name best-data)))))))
    (values "" "")))

(defmacro with-display ((display &key timeout inline)
			&body body)
  ;; This macro is for use in a multi-process environment.  It
  ;; provides exclusive access to the local display object for
  ;; multiple request generation.  It need not provide immediate
  ;; exclusive access for replies; that is, if another process is
  ;; waiting for a reply (while not in a with-display), then
  ;; synchronization need not (but can) occur immediately.  Except
  ;; where noted, all routines effectively contain an implicit
  ;; with-display where needed, so that correct synchronization is
  ;; always provided at the interface level on a per-call basis.
  ;; Nested uses of this macro will work correctly.  This macro does
  ;; not prevent concurrent event processing; see with-event-queue.
  `(with-buffer (,display
		 ,@(and timeout `(:timeout ,timeout))
		 ,@(and inline `(:inline ,inline)))
     ,@body))

;;
;; Resource id management
;;
(defun initialize-resource-allocator (display)
  ;; Find the resource-id-byte (appropriate for LDB & DPB) from the resource-id-mask
  (let ((id-mask (display-resource-id-mask display)))
    (unless (zerop id-mask) ;; zero mask is an error
      (do ((first 0 (index1+ first))
	   (mask id-mask (the mask32 (ash mask -1))))
	  ((oddp mask)
	   (setf (display-resource-id-byte display)
		 (byte (integer-length mask) first)))
	(declare (type array-index first)
		 (type mask32 mask))))))

(defun resourcealloc (display)
  ;; Allocate a resource-id for use in DISPLAY
  (declare (type display display))
  (declare (clx-values resource-id))
  (loop for next-count upfrom (1+ (display-resource-id-count display))
        repeat (1+ (display-resource-id-mask display))
        as id = (dpb next-count
                     (display-resource-id-byte display)
                     (display-resource-id-base display))
        unless (nth-value 1 (gethash id (display-resource-id-map display)))
        do (setf (display-resource-id-count display) next-count)
           (setf (gethash id (display-resource-id-map display)) t)
           (return-from resourcealloc id))
  ;; internal consistency check
  (assert (= (hash-table-count (display-resource-id-map display))
             (1+ (display-resource-id-mask display))))
  ;; tell the user what's gone wrong
  (error 'resource-ids-exhausted))

(defmacro allocate-resource-id (display object type)
  ;; Allocate a resource-id for OBJECT in DISPLAY
  `(with-display (,display)
     ,(if (member (eval type) +clx-cached-types+)
          `(let ((id (funcall (display-xid ,display) ,display)))
             (save-id ,display id ,object)
             id)
          `(funcall (display-xid ,display) ,display))))

(defmacro deallocate-resource-id (display id type)
  (declare (ignore type))
  ;; Deallocate a resource-id for OBJECT in DISPLAY
  `(deallocate-resource-id-internal ,display ,id))

(defun deallocate-resource-id-internal (display id)
  (with-display (display)
    (remhash id (display-resource-id-map display))))

(defun lookup-resource-id (display id)
  ;; Find the object associated with resource ID
  (gethash id (display-resource-id-map display)))

(defun save-id (display id object)
  ;; cache the object associated with ID for this display.
  (declare (type display display)
	   (type integer id)
	   (type t object))
  (declare (clx-values object))
  ;; we can't cache objects from other clients, because they may
  ;; become invalid without us being told about that.
  (let ((base (display-resource-id-base display))
        (mask (display-resource-id-mask display)))
    (when (= (logandc2 id mask) base)
      (setf (gethash id (display-resource-id-map display)) object))
    object))

;; Define functions to find the CLX data types given a display and resource-id
;; If the data type is being cached, look there first.
(macrolet ((generate-lookup-functions (useless-name &body types)
	    `(within-definition (,useless-name generate-lookup-functions)
	       ,@(mapcar
		   #'(lambda (type)
		       `(defun ,(xintern 'lookup- type)
			       (display id)
			  (declare (type display display)
				   (type resource-id id))
			  (declare (clx-values ,type))
			  ,(if (member type +clx-cached-types+)
			       `(let ((,type (lookup-resource-id display id)))
				  (cond ((null ,type) ;; Not found, create and save it.
					 (setq ,type (,(xintern 'make- type)
						      :display display :id id))
					 (save-id display id ,type))
					;; Found.  Check the type
					,(cond ((null +type-check?+)
						`(t ,type))
					       ((member type '(window pixmap))
						`((type? ,type 'drawable) ,type))
					       (t `((type? ,type ',type) ,type)))
					,@(when +type-check?+
					    `((t (x-error 'lookup-error
							  :id id
							  :display display
							  :type ',type
							  :object ,type))))))
			       ;; Not being cached.  Create a new one each time.
			       `(,(xintern 'make- type)
				 :display display :id id))))
		   types))))
  (generate-lookup-functions ignore
    drawable
    window
    pixmap
    gcontext
    cursor
    colormap
    font))

(defun id-atom (id display)
  ;; Return the cached atom for an atom ID
  (declare (type resource-id id)
	   (type display display))
  (declare (clx-values (or null keyword)))
  (gethash id (display-atom-id-map display)))

(defun atom-id (atom display)
  ;; Return the ID for an atom in DISPLAY
  (declare (type xatom atom)
	   (type display display))
  (declare (clx-values (or null resource-id)))
  (gethash (if (or (null atom) (keywordp atom)) atom (kintern atom))
	   (display-atom-cache display)))

(defun set-atom-id (atom display id)
  ;; Set the ID for an atom in DISPLAY
  (declare (type xatom atom)
	   (type display display)
	   (type resource-id id))
  (declare (clx-values resource-id))
  (let ((atom (if (or (null atom) (keywordp atom)) atom (kintern atom))))
    (setf (gethash id (display-atom-id-map display)) atom)
    (setf (gethash atom (display-atom-cache display)) id)
    id))

(defsetf atom-id set-atom-id)

(defun initialize-predefined-atoms (display)
  (dotimes (i (length +predefined-atoms+))
    (declare (type resource-id i))
    (setf (atom-id (svref +predefined-atoms+ i) display) i)))

(defun visual-info (display visual-id)
  (declare (type display display)
	   (type resource-id visual-id)
	   (clx-values visual-info))
  (when (zerop visual-id)
    (return-from visual-info nil))
  (dolist (screen (display-roots display))
    (declare (type screen screen))
    (dolist (depth (screen-depths screen))
      (declare (type cons depth))
      (dolist (visual-info (rest depth))
	(declare (type visual-info visual-info))
	(when (funcall (resource-id-map-test) visual-id (visual-info-id visual-info))
	  (return-from visual-info visual-info)))))
  (error "Visual info not found for id #x~x in display ~s." visual-id display))


;;
;; Display functions
;;
(defmacro with-event-queue ((display &key timeout inline)
			    &body body &environment env)
  ;; exclusive access to event queue
  `(macrolet ((with-event-queue ((display &key timeout) &body body)
		;; Speedup hack for lexically nested with-event-queues
		`(progn
		   (progn ,display ,@(and timeout `(,timeout)) nil)
		   ,@body)))
     ,(if (and (null inline) (macroexpand '(use-closures) env))
	  `(flet ((.with-event-queue-body. () ,@body))
	     #+clx-ansi-common-lisp
	     (declare (dynamic-extent #'.with-event-queue-body.))
	     (with-event-queue-function
	       ,display ,timeout #'.with-event-queue-body.))
	(let ((disp (if (or (symbolp display) (constantp display))
			display
		      '.display.)))
	  `(let (,@(unless (eq disp display) `((,disp ,display))))
	     (holding-lock ((display-event-lock ,disp) ,disp "CLX Event Lock"
			    ,@(and timeout `(:timeout ,timeout)))
	       ,@body))))))

(defun with-event-queue-function (display timeout function)
  (declare (type display display)
	   (type (or null number) timeout)
	   (type function function)
	   #+clx-ansi-common-lisp
	   (dynamic-extent function)
	   ;; FIXME: see SBCL bug #243
	   (ignorable display timeout)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg function))
  (with-event-queue (display :timeout timeout :inline t)
    (funcall function)))

(defmacro with-event-queue-internal ((display &key timeout) &body body)
  ;; exclusive access to the internal event queues
  (let ((disp (if (or (symbolp display) (constantp display)) display '.display.)))
    `(let (,@(unless (eq disp display) `((,disp ,display))))
       (holding-lock ((display-event-queue-lock ,disp) ,disp "CLX Event Queue Lock"
		      ,@(and timeout `(:timeout ,timeout)))
	 ,@body))))

(defun open-default-display (&optional display-name)
  "Open a connection to DISPLAY-NAME if supplied, or to the appropriate
default display as given by GET-DEFAULT-DISPLAY otherwise.

OPEN-DISPLAY-NAME always attempts to do display authorization.  The
hostname is resolved to an address, then authorization data for the
(protocol, host-address, displaynumber) triple is looked up in the
file given by AUTHORITY_PATHNAME (typically $HOME/.Xauthority).  If
the protocol is :local, or if the hostname resolves to the local host,
authority data for the local machine's actual hostname - as returned by
gethostname(3) - is used instead."
  (destructuring-bind (host display screen protocol)
      (get-default-display display-name)
    (let ((display (open-display host :display display :protocol protocol)))
      (setf (display-default-screen display) (nth screen (display-roots display)))
      display)))

(defun open-display (host &key (display 0) protocol authorization-name authorization-data)
  ;; Implementation specific routine to setup the buffer for a
  ;; specific host and display.  This must interface with the local
  ;; network facilities, and will probably do special things to
  ;; circumvent the nework when displaying on the local host.
  ;;
  ;; A string must be acceptable as a host, but otherwise the possible types
  ;; for host and protocol are not constrained, and will likely be very
  ;; system dependent.  The default protocol is system specific.  Authorization,
  ;; if any, is assumed to come from the environment somehow.
  (declare (type integer display))
  (declare (clx-values display))
  ;; Get the authorization mechanism from the environment.  Handle the
  ;; special case of a host name of "" and "unix" which means the
  ;; protocol is :local
  (when (null authorization-name)
    (multiple-value-setq (authorization-name authorization-data)
      (get-best-authorization host
			      display
			      (if (member host '("" "unix") :test #'equal)
				  :local
				  protocol))))
  ;; PROTOCOL is the network protocol (something like :TCP :DNA or :CHAOS). See OPEN-X-STREAM.
  (let* ((stream (open-x-stream host display protocol))
	 (disp (make-buffer *output-buffer-size* #'make-display-internal
			    :host host :display display
			    :output-stream stream :input-stream stream))
	 (ok-p nil))
    (unwind-protect
	(progn
	  (display-connect disp
			   :authorization-name authorization-name
			   :authorization-data authorization-data)
	  (setf (display-authorization-name disp) authorization-name)
	  (setf (display-authorization-data disp) authorization-data)
	  (initialize-resource-allocator disp)
	  (initialize-predefined-atoms disp)
	  (initialize-extensions disp)
	  (when (assoc "BIG-REQUESTS" (display-extension-alist disp)
		       :test #'string=)
	    (enable-big-requests disp))
	  (setq ok-p t))
      (unless ok-p (close-display disp :abort t)))
    disp))

(defun display-force-output (display)
  ; Output is normally buffered, this forces any buffered output to the server.
  (declare (type display display))
  (with-display (display)
    (buffer-force-output display)))

(defun close-display (display &key abort)
  ;; Close the host connection in DISPLAY
  (declare (type display display))
  (close-buffer display :abort abort))

(defun display-connect (display &key authorization-name authorization-data)
  (with-buffer-output (display :sizes (8 16))
    (card8-put
      0
      (ecase (display-byte-order display)
	(:lsbfirst #x6c)   ;; Ascii lowercase l - Least Significant Byte First
	(:msbfirst #x42))) ;; Ascii uppercase B -  Most Significant Byte First
    (card16-put 2 *protocol-major-version*)
    (card16-put 4 *protocol-minor-version*)
    (card16-put 6 (length authorization-name))
    (card16-put 8 (length authorization-data))
    (write-sequence-char display 12 authorization-name)
    (if (stringp authorization-data)
	(write-sequence-char display (lround (+ 12 (length authorization-name)))
			     authorization-data)
	(write-sequence-card8 display (lround (+ 12 (length authorization-name)))
			      authorization-data)))
  (buffer-force-output display)
  (let ((reply-buffer nil))
    (declare (type (or null reply-buffer) reply-buffer))
    (unwind-protect
	(progn
	  (setq reply-buffer (allocate-reply-buffer #x1000))
	  (with-buffer-input (reply-buffer :sizes (8 16 32))
	    (buffer-input display buffer-bbuf 0 8)
	    (let ((success (boolean-get 0))
		  (reason-length (card8-get 1))
		  (major-version (card16-get 2))
		  (minor-version (card16-get 4))
		  (total-length (card16-get 6))
		  vendor-length
		  num-roots
		  num-formats)
	      (declare (ignore total-length))
	      (unless success
		(x-error 'connection-failure
			 :major-version major-version
			 :minor-version minor-version
			 :host (display-host display)
			 :display (display-display display)
			 :reason
			 (progn (buffer-input display buffer-bbuf 0 reason-length)
				(string-get reason-length 0 :reply-buffer reply-buffer))))
	      (buffer-input display buffer-bbuf 0 32)
	      (setf (display-protocol-major-version display) major-version)
	      (setf (display-protocol-minor-version display) minor-version)
	      (setf (display-release-number display) (card32-get 0))
	      (setf (display-resource-id-base display) (card32-get 4))
	      (setf (display-resource-id-mask display) (card32-get 8))
	      (setf (display-motion-buffer-size display) (card32-get 12))
	      (setq vendor-length (card16-get 16))
	      (setf (display-max-request-length display) (card16-get 18))
	      (setq num-roots (card8-get 20))
	      (setq num-formats (card8-get 21))
	      ;; Get the image-info
	      (setf (display-image-lsb-first-p display) (zerop (card8-get 22)))
	      (let ((format (display-bitmap-format display)))
		(declare (type bitmap-format format))
		(setf (bitmap-format-lsb-first-p format) (zerop (card8-get 23)))
		(setf (bitmap-format-unit format) (card8-get 24))
		(setf (bitmap-format-pad format) (card8-get 25)))
	      (setf (display-min-keycode display) (card8-get 26))
	      (setf (display-max-keycode display) (card8-get 27))
	      ;; 4 bytes unused
	      ;; Get the vendor string
	      (buffer-input display buffer-bbuf 0 (lround vendor-length))
	      (setf (display-vendor-name display)
		    (string-get vendor-length 0 :reply-buffer reply-buffer))
	      ;; Initialize the pixmap formats
	      (dotimes (i num-formats) ;; loop gathering pixmap formats
		(declare (ignorable i))
		(buffer-input display buffer-bbuf 0 8)
		(push (make-pixmap-format :depth (card8-get 0)
					  :bits-per-pixel (card8-get 1)
					  :scanline-pad (card8-get 2))
						; 5 unused bytes
		      (display-pixmap-formats display)))
	      (setf (display-pixmap-formats display)
		    (nreverse (display-pixmap-formats display)))
	      ;; Initialize the screens
	      (dotimes (i num-roots)
		(declare (ignorable i))
		(buffer-input display buffer-bbuf 0 40)
		(let* ((root-id (card32-get 0))
		       (root (make-window :id root-id :display display))
		       (root-visual (card32-get 32))
		       (default-colormap-id (card32-get 4))
		       (default-colormap
			 (make-colormap :id default-colormap-id :display display))
		       (screen
			 (make-screen
			   :root root
			   :default-colormap default-colormap
			   :white-pixel (card32-get 8)
			   :black-pixel (card32-get 12)
			   :event-mask-at-open (card32-get 16)
			   :width  (card16-get 20)
			   :height (card16-get 22)
			   :width-in-millimeters  (card16-get 24)
			   :height-in-millimeters (card16-get 26)
			   :min-installed-maps (card16-get 28)
			   :max-installed-maps (card16-get 30)
			   :backing-stores (member8-get 36 :never :when-mapped :always)
			   :save-unders-p (boolean-get 37)
			   :root-depth (card8-get 38)))
		       (num-depths (card8-get 39))
		       (depths nil))
		  ;; Save root window for event reporting
		  (save-id display root-id root)
		  (save-id display default-colormap-id default-colormap)
		  ;; Create the depth AList for a screen, (depth . visual-infos)
		  (dotimes (j num-depths)
		    (declare (ignorable j))
		    (buffer-input display buffer-bbuf 0 8)
		    (let ((depth (card8-get 0))
			  (num-visuals (card16-get 2))
			  (visuals nil)) ;; 4 bytes unused
		      (dotimes (k num-visuals)
			(declare (ignorable k))
			(buffer-input display buffer-bbuf 0 24)
			(let* ((visual (card32-get 0))
			       (visual-info (make-visual-info
					      :id visual
					      :display display
					      :class (member8-get 4 :static-gray :gray-scale
								  :static-color :pseudo-color
								  :true-color :direct-color)
					      :bits-per-rgb (card8-get 5)
					      :colormap-entries (card16-get 6)
					      :red-mask (card32-get 8)
					      :green-mask (card32-get 12)
					      :blue-mask (card32-get 16)
					      ;; 4 bytes unused
					      )))
			  (push visual-info visuals)
			  (when (funcall (resource-id-map-test) root-visual visual)
			    (setf (screen-root-visual-info screen)
				  (setf (colormap-visual-info default-colormap)
					visual-info)))))
		      (push (cons depth (nreverse visuals)) depths)))
		  (setf (screen-depths screen) (nreverse depths))
		  (push screen (display-roots display))))
	      (setf (display-roots display) (nreverse (display-roots display)))
	      (setf (display-default-screen display) (first (display-roots display))))))
      (when reply-buffer
	(deallocate-reply-buffer reply-buffer))))
  display)

(defun display-protocol-version (display)
  (declare (type display display))
  (declare (clx-values major minor))
  (values (display-protocol-major-version display)
	  (display-protocol-minor-version display)))

(defun display-vendor (display)
  (declare (type display display))
  (declare (clx-values name release))
  (values (display-vendor-name display)
	  (display-release-number display)))

(defun display-nscreens (display)
  (declare (type display display))
  (length (display-roots display)))

#+comment ;; defined by the DISPLAY defstruct
(defsetf display-error-handler (display) (handler)
  ;; All errors (synchronous and asynchronous) are processed by
  ;; calling an error handler in the display.  If handler is a
  ;; sequence it is expected to contain handler functions specific to
  ;; each error; the error code is used to index the sequence,
  ;; fetching the appropriate handler.  Any results returned by the
  ;; handler are ignored; it is assumed the handler either takes care
  ;; of the error completely, or else signals. For all core errors,
  ;; the keyword/value argument pairs are:
  ;;    :display display
  ;;    :error-key error-key
  ;;    :major integer
  ;;    :minor integer
  ;;    :sequence integer
  ;;    :current-sequence integer
  ;; For :colormap, :cursor, :drawable, :font, :gcontext, :id-choice, :pixmap, and
  ;; :window errors another pair is:
  ;;    :resource-id integer
  ;; For :atom errors, another pair is:
  ;;    :atom-id integer
  ;; For :value errors, another pair is:
  ;;    :value integer
  )

  ;; setf'able
  ;; If defined, called after every protocol request is generated,
  ;; even those inside explicit with-display's, but never called from
  ;; inside the after-function itself.  The function is called inside
  ;; the effective with-display for the associated request.  Default
  ;; value is nil.  Can be set, for example, to #'display-force-output
  ;; or #'display-finish-output.

(defvar *inside-display-after-function* nil)

(defun display-invoke-after-function (display)
  ; Called after every protocal request is generated
  (declare (type display display))
  (when (and (display-after-function display)
	     (not *inside-display-after-function*))
    (let ((*inside-display-after-function* t)) ;; Ensure no recursive calls
      (funcall (display-after-function display) display))))

(defun display-finish-output (display)
  ;; Forces output, then causes a round-trip to ensure that all possible
  ;; errors and events have been received.
  (declare (type display display))
  (with-buffer-request-and-reply (display +x-getinputfocus+ 16 :sizes (8 32))
       ()
    )
  ;; Report asynchronous errors here if the user wants us to.
  (report-asynchronous-errors display :after-finish-output))

(defparameter
  *request-names*
  '#("error" "CreateWindow" "ChangeWindowAttributes" "GetWindowAttributes"
     "DestroyWindow" "DestroySubwindows" "ChangeSaveSet" "ReparentWindow"
     "MapWindow" "MapSubwindows" "UnmapWindow" "UnmapSubwindows"
     "ConfigureWindow" "CirculateWindow" "GetGeometry" "QueryTree"
     "InternAtom" "GetAtomName" "ChangeProperty" "DeleteProperty"
     "GetProperty" "ListProperties" "SetSelectionOwner" "GetSelectionOwner"
     "ConvertSelection" "SendEvent" "GrabPointer" "UngrabPointer"
     "GrabButton" "UngrabButton" "ChangeActivePointerGrab" "GrabKeyboard"
     "UngrabKeyboard" "GrabKey" "UngrabKey" "AllowEvents"
     "GrabServer" "UngrabServer" "QueryPointer" "GetMotionEvents"
     "TranslateCoords" "WarpPointer" "SetInputFocus" "GetInputFocus"
     "QueryKeymap" "OpenFont" "CloseFont" "QueryFont"
     "QueryTextExtents" "ListFonts" "ListFontsWithInfo" "SetFontPath"
     "GetFontPath" "CreatePixmap" "FreePixmap" "CreateGC"
     "ChangeGC" "CopyGC" "SetDashes" "SetClipRectangles"
     "FreeGC" "ClearToBackground" "CopyArea" "CopyPlane"
     "PolyPoint" "PolyLine" "PolySegment" "PolyRectangle"
     "PolyArc" "FillPoly" "PolyFillRectangle" "PolyFillArc"
     "PutImage" "GetImage" "PolyText8" "PolyText16"
     "ImageText8" "ImageText16" "CreateColormap" "FreeColormap"
     "CopyColormapAndFree" "InstallColormap" "UninstallColormap" "ListInstalledColormaps"
     "AllocColor" "AllocNamedColor" "AllocColorCells" "AllocColorPlanes"
     "FreeColors" "StoreColors" "StoreNamedColor" "QueryColors"
     "LookupColor" "CreateCursor" "CreateGlyphCursor" "FreeCursor"
     "RecolorCursor" "QueryBestSize" "QueryExtension" "ListExtensions"
     "SetKeyboardMapping" "GetKeyboardMapping" "ChangeKeyboardControl" "GetKeyboardControl"
     "Bell" "ChangePointerControl" "GetPointerControl" "SetScreenSaver"
     "GetScreenSaver" "ChangeHosts" "ListHosts" "ChangeAccessControl"
     "ChangeCloseDownMode" "KillClient" "RotateProperties" "ForceScreenSaver"
     "SetPointerMapping" "GetPointerMapping" "SetModifierMapping" "GetModifierMapping"))
