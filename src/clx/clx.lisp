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

;; Primary Interface Author:
;;	Robert W. Scheifler
;;	MIT Laboratory for Computer Science
;;	545 Technology Square, Room 418
;;	Cambridge, MA 02139
;;	rws@zermatt.lcs.mit.edu

;; Design Contributors:
;;	Dan Cerys, Texas Instruments
;;	Scott Fahlman, CMU
;;      Charles Hornig, Symbolics
;;      John Irwin, Franz
;;	Kerry Kimbrough, Texas Instruments
;;	Chris Lindblad, MIT
;;	Rob MacLachlan, CMU
;;	Mike McMahon, Symbolics
;;	David Moon, Symbolics
;;	LaMott Oren, Texas Instruments
;;	Daniel Weinreb, Symbolics
;;	John Wroclawski, MIT
;;	Richard Zippel, Symbolics

;; Primary Implementation Author:
;;	LaMott Oren, Texas Instruments

;; Implementation Contributors:
;;      Charles Hornig, Symbolics
;;      John Irwin, Franz
;;	Chris Lindblad, MIT
;;	Robert Scheifler, MIT

;;;
;;; Change history:
;;;
;;;  Date	Author		Description
;;; -------------------------------------------------------------------------------------
;;; 04/07/87	R.Scheifler	Created code stubs
;;; 04/08/87	L.Oren		Started Implementation
;;; 05/11/87	L.Oren		Included draft 3 revisions
;;; 07/07/87	L.Oren		Untested alpha release to MIT
;;; 07/17/87	L.Oren		Alpha release
;;; 08/**/87	C.Lindblad	Rewrite of buffer code
;;; 08/**/87	et al		Various random bug fixes
;;; 08/**/87	R.Scheifler	General syntactic and portability cleanups
;;; 08/**/87	R.Scheifler	Rewrite of gcontext caching and shadowing
;;; 09/02/87	L.Oren		Change events from resource-ids to objects
;;; 12/24/87	R.Budzianowski	KCL support
;;; 12/**/87	J.Irwin		ExCL 2.0 support
;;; 01/20/88	L.Oren		Add server extension mechanisms
;;; 01/20/88	L.Oren		Only force output when blocking on input
;;; 01/20/88	L.Oren		Uniform support for :event-window on events
;;; 01/28/88	L.Oren		Add window manager property functions
;;; 01/28/88	L.Oren		Add character translation facility
;;; 02/**/87	J.Irwin		Allegro 2.2 support

;;; This is considered a somewhat changeable interface.  Discussion of better
;;; integration with CLOS, support for user-specified subclassess of basic
;;; objects, and the additional functionality to match the C Xlib is still in
;;; progress.  Bug reports should be addressed to bug-clx@expo.lcs.mit.edu.

;; Note: all of the following is in the package XLIB.

(in-package :xlib)

(pushnew :clx *features*)
(pushnew :xlib *features*)

(defparameter *version* "MIT R5.02")
(pushnew :clx-mit-r4 *features*)
(pushnew :clx-mit-r5 *features*)

(defparameter *protocol-major-version* 11.)
(defparameter *protocol-minor-version* 0)

(defparameter *x-tcp-port* 6000) ;; add display number

;; Note: if you have read the Version 11 protocol document or C Xlib manual, most of
;; the relationships should be fairly obvious.  We have no intention of writing yet
;; another moby document for this interface.

;; Types employed: display, window, pixmap, cursor, font, gcontext, colormap, color.
;; These types are defined solely by a functional interface; we do not specify
;; whether they are implemented as structures or flavors or ...  Although functions
;; below are written using DEFUN, this is not an implementation requirement (although
;; it is a requirement that they be functions as opposed to macros or special forms).
;; It is unclear whether with-slots in the Common Lisp Object System must work on
;; them.

;; Windows, pixmaps, cursors, fonts, gcontexts, and colormaps are all represented as
;; compound objects, rather than as integer resource-ids.  This allows applications
;; to deal with multiple displays without having an explicit display argument in the
;; most common functions.  Every function uses the display object indicated by the
;; first argument that is or contains a display; it is an error if arguments contain
;; different displays, and predictable results are not guaranteed.

;; Each of window, pixmap, cursor, font, gcontext, and colormap have the following
;; five functions:

;(defun make-<mumble> (display resource-id)
;  ;; This function should almost never be called by applications, except in handling
;  ;; events.  To minimize consing in some implementations, this may use a cache in
;  ;; the display.  Make-gcontext creates with :cache-p nil.  Make-font creates with
;  ;; cache-p true.
;  (declare (type display display)
;	   (type integer resource-id)
;	   (clx-values <mumble>)))

;(defun <mumble>-display (<mumble>)
;  (declare (type <mumble> <mumble>)
;	   (clx-values display)))

;(defun <mumble>-id (<mumble>)
;  (declare (type <mumble> <mumble>)
;	   (clx-values integer)))

;(defun <mumble>-equal (<mumble>-1 <mumble>-2)
;  (declare (type <mumble> <mumble>-1 <mumble>-2)))

;(defun <mumble>-p (<mumble>-1 <mumble>-2)
;  (declare (type <mumble> <mumble>-1 <mumble>-2)
;	   (clx-values boolean)))


(deftype generalized-boolean () 't)	; (or null (not null))

(deftype card32 () '(unsigned-byte 32))

(deftype card29 () '(unsigned-byte 29))

(deftype card24 () '(unsigned-byte 24))

(deftype int32 () '(signed-byte 32))

(deftype card16 () '(unsigned-byte 16))

(deftype int16 () '(signed-byte 16))

(deftype card8 () '(unsigned-byte 8))

(deftype int8 () '(signed-byte 8))

(deftype card4 () '(unsigned-byte 4))

#-clx-ansi-common-lisp
(deftype real (&optional (min '*) (max '*))
  (labels ((convert (limit floatp)
	     (typecase limit
	       (number (if floatp (float limit 0s0) (rational limit)))
	       (list (map 'list #'convert limit))
	       (otherwise limit))))
    `(or (float ,(convert min t) ,(convert max t))
	 (rational ,(convert min nil) ,(convert max nil)))))

#-clx-ansi-common-lisp
(deftype base-char ()
  'string-char)

; Note that we are explicitly using a different rgb representation than what
; is actually transmitted in the protocol.

(deftype rgb-val () '(real 0 1))

; Note that we are explicitly using a different angle representation than what
; is actually transmitted in the protocol.

(deftype angle () '(real #.(* -2 pi) #.(* 2 pi)))

(deftype mask32 () 'card32)

(deftype mask16 () 'card16)

(deftype pixel () '(unsigned-byte 32))
(deftype image-depth () '(integer 0 32))

(deftype resource-id () 'card29)

(deftype keysym () 'card32)

; The following functions are provided by color objects:

; The intention is that IHS and YIQ and CYM interfaces will also exist.
; Note that we are explicitly using a different spectrum representation
; than what is actually transmitted in the protocol.

(def-clx-class (color (:constructor make-color-internal (red green blue))
		      (:copier nil) (:print-function print-color))
  (red 0.0 :type rgb-val)
  (green 0.0 :type rgb-val)
  (blue 0.0 :type rgb-val))

(defun print-color (color stream depth)
  (declare (type color color)
	   (ignore depth))
  (print-unreadable-object (color stream :type t)
    (prin1 (color-red color) stream)
    (write-string " " stream)
    (prin1 (color-green color) stream)
    (write-string " " stream)
    (prin1 (color-blue color) stream)))

(defun make-color (&key (red 1.0) (green 1.0) (blue 1.0) &allow-other-keys)
  (declare (type rgb-val red green blue))
  (declare (clx-values color))
  (make-color-internal red green blue))

(defun color-rgb (color)
  (declare (type color color))
  (declare (clx-values red green blue))
  (values (color-red color) (color-green color) (color-blue color)))

(def-clx-class (bitmap-format (:copier nil) (:print-function print-bitmap-format))
  (unit 8 :type (member 8 16 32))
  (pad 8 :type (member 8 16 32))
  (lsb-first-p nil :type generalized-boolean))

(defun print-bitmap-format (bitmap-format stream depth)
  (declare (type bitmap-format bitmap-format)
	   (ignore depth))
  (print-unreadable-object (bitmap-format stream :type t)
    (format stream "unit ~D pad ~D ~:[M~;L~]SB first"
	    (bitmap-format-unit bitmap-format)
	    (bitmap-format-pad bitmap-format)
	    (bitmap-format-lsb-first-p bitmap-format))))

(def-clx-class (pixmap-format (:copier nil) (:print-function print-pixmap-format))
  (depth 0 :type image-depth)
  (bits-per-pixel 8 :type (member 1 4 8 12 16 24 32))
  (scanline-pad 8 :type (member 8 16 32)))

(defun print-pixmap-format (pixmap-format stream depth)
  (declare (type pixmap-format pixmap-format)
	   (ignore depth))
  (print-unreadable-object (pixmap-format stream :type t)
    (format stream "depth ~D bits-per-pixel ~D scanline-pad ~D"
	    (pixmap-format-depth pixmap-format)
	    (pixmap-format-bits-per-pixel pixmap-format)
	    (pixmap-format-scanline-pad pixmap-format))))

(defparameter *atom-cache-size* 200)
(defparameter *resource-id-map-size* 500)

(def-clx-class (display (:include buffer)
			(:constructor make-display-internal)
			(:print-function print-display)
			(:copier nil))
  (host)					; Server Host
  (display 0 :type integer)			; Display number on host
  (after-function nil)				; Function to call after every request
  (event-lock
    (make-process-lock "CLX Event Lock"))	; with-event-queue lock
  (event-queue-lock
    (make-process-lock "CLX Event Queue Lock"))	; new-events/event-queue lock
  (event-queue-tail				; last event in the event queue
    nil :type (or null reply-buffer))
  (event-queue-head				; Threaded queue of events
    nil :type (or null reply-buffer))
  (atom-cache (make-hash-table :test (atom-cache-map-test) :size *atom-cache-size*)
	      :type hash-table)			; Hash table relating atoms keywords
						; to atom id's
  (font-cache nil)				; list of font
  (protocol-major-version 0 :type card16)	; Major version of server's X protocol
  (protocol-minor-version 0 :type card16)	; minor version of servers X protocol
  (vendor-name "" :type string)			; vendor of the server hardware
  (resource-id-base 0 :type resource-id)	; resouce ID base
  (resource-id-mask 0 :type resource-id)	; resource ID mask bits
  (resource-id-byte nil)			; resource ID mask field (used with DPB & LDB)
  (resource-id-count 0 :type resource-id)	; resource ID mask count
						; (used for allocating ID's)
  (resource-id-map (make-hash-table :test (resource-id-map-test)
				    :size *resource-id-map-size*)
		   :type hash-table)		; hash table maps resource-id's to
						; objects (used in lookup functions)
  (xid 'resourcealloc)				; allocator function
  (byte-order #+clx-little-endian :lsbfirst     ; connection byte order
	      #-clx-little-endian :msbfirst)
  (release-number 0 :type card32)		; release of the server
  (max-request-length 0 :type card16)		; maximum number 32 bit words in request
  (default-screen)				; default screen for operations
  (roots nil :type list)			; List of screens
  (motion-buffer-size 0 :type card32)		; size of motion buffer
  (xdefaults)					; contents of defaults from server
  (image-lsb-first-p nil :type generalized-boolean)
  (bitmap-format (make-bitmap-format)		; Screen image info
		 :type bitmap-format)
  (pixmap-formats nil :type sequence)		; list of pixmap formats
  (min-keycode 0 :type card8)			; minimum key-code
  (max-keycode 0 :type card8)			; maximum key-code
  (error-handler 'default-error-handler)	; Error handler function
  (close-down-mode :destroy)  			; Close down mode saved by Set-Close-Down-Mode
  (authorization-name "" :type string)
  (authorization-data "" :type (or (array (unsigned-byte 8)) string))
  (last-width nil :type (or null card29))	; Accumulated width of last string
  (keysym-mapping nil				; Keysym mapping cached from server
		  :type (or null (array * (* *))))
  (modifier-mapping nil :type list)		; ALIST of (keysym . state-mask) for all modifier keysyms
  (keysym-translation nil :type list)		; An alist of (keysym object function)
						; for display-local keysyms
  (extension-alist nil :type list)		; extension alist, which has elements:
						; (name major-opcode first-event first-error)
  (event-extensions '#() :type vector)		; Vector mapping X event-codes to event keys
  (performance-info)				; Hook for gathering performance info
  (trace-history)				; Hook for debug trace
  (plist nil :type list)			; hook for extension to hang data
  ;; These slots are used to manage multi-process input.
  (input-in-progress nil)			; Some process reading from the stream.
						; Updated with CONDITIONAL-STORE.
  (pending-commands nil)			; Threaded list of PENDING-COMMAND objects 
						; for all commands awaiting replies.
						; Protected by WITH-EVENT-QUEUE-INTERNAL.
  (asynchronous-errors nil)			; Threaded list of REPLY-BUFFER objects
						; containing error messages for commands
						; which did not expect replies.
						; Protected by WITH-EVENT-QUEUE-INTERNAL.
  (report-asynchronous-errors			; When to report asynchronous errors
    '(:immediately) :type list)			; The keywords that can be on this list 
						; are :IMMEDIATELY, :BEFORE-EVENT-HANDLING,
						; and :AFTER-FINISH-OUTPUT
  (event-process nil)				; Process ID of process awaiting events.
						; Protected by WITH-EVENT-QUEUE.
  (new-events nil :type (or null reply-buffer))	; Pointer to the first new event in the
						; event queue.
						; Protected by WITH-EVENT-QUEUE.
  (current-event-symbol				; Bound with PROGV by event handling macros 
    (list (gensym) (gensym)) :type cons)
  (atom-id-map (make-hash-table :test (resource-id-map-test)
				:size *atom-cache-size*)
	       :type hash-table)
  (extended-max-request-length 0 :type card32)
  )

(defun print-display-name (display stream)
  (declare (type (or null display) display))
  (cond (display
	 #-allegro (princ (display-host display) stream)
	 #+allegro (write-string (string (display-host display)) stream)
	 (write-string ":" stream)
	 (princ (display-display display) stream))
	(t
	 (write-string "(no display)" stream)))
  display)

(defun print-display (display stream depth)
  (declare (type display display)
	   (ignore depth))
  (print-unreadable-object (display stream :type t)
    (print-display-name display stream)
    (write-string " (" stream)
    (write-string (display-vendor-name display) stream)
    (write-string " R" stream)
    (prin1 (display-release-number display) stream)
    (write-string ")" stream)))

;;(deftype drawable () '(or window pixmap))

(def-clx-class (drawable (:copier nil) (:print-function print-drawable))
  (id 0 :type resource-id)
  (display nil :type (or null display))
  (plist nil :type list)			; Extension hook
  )

(defun print-drawable (drawable stream depth)
  (declare (type drawable drawable)
	   (ignore depth))
  (print-unreadable-object (drawable stream :type t)
    (print-display-name (drawable-display drawable) stream)
    (write-string " " stream)
    (let ((*print-base* 16)) (prin1 (drawable-id drawable) stream))))

(def-clx-class (window (:include drawable) (:copier nil)
		       (:print-function print-drawable))
  )

(def-clx-class (pixmap (:include drawable) (:copier nil)
		       (:print-function print-drawable))
  )

(def-clx-class (visual-info (:copier nil) (:print-function print-visual-info))
  (id 0 :type resource-id)
  (display nil :type (or null display))
  (class :static-gray :type (member :static-gray :static-color :true-color
				    :gray-scale :pseudo-color :direct-color))
  (red-mask 0 :type pixel)
  (green-mask 0 :type pixel)
  (blue-mask 0 :type pixel)
  (bits-per-rgb 1 :type card8)
  (colormap-entries 0 :type card16)
  (plist nil :type list)			; Extension hook
  )

(defun print-visual-info (visual-info stream depth)
  (declare (type visual-info visual-info)
	   (ignore depth))
  (print-unreadable-object (visual-info stream :type t)
    (prin1 (visual-info-bits-per-rgb visual-info) stream)
    (write-string "-bit " stream)
    (princ (visual-info-class visual-info) stream)
    (write-string " " stream)
    (print-display-name (visual-info-display visual-info) stream)
    (write-string " " stream)
    (prin1 (visual-info-id visual-info) stream)))

(def-clx-class (colormap (:copier nil) (:print-function print-colormap))
  (id 0 :type resource-id)
  (display nil :type (or null display))
  (visual-info nil :type (or null visual-info))
  )

(defun print-colormap (colormap stream depth)
  (declare (type colormap colormap)
	   (ignore depth))
  (print-unreadable-object (colormap stream :type t)
    (when (colormap-visual-info colormap)
      (princ (visual-info-class (colormap-visual-info colormap)) stream)
      (write-string " " stream))
    (print-display-name (colormap-display colormap) stream)
    (write-string " " stream)
    (prin1 (colormap-id colormap) stream)))

(def-clx-class (cursor (:copier nil) (:print-function print-cursor))
  (id 0 :type resource-id)
  (display nil :type (or null display))
  )

(defun print-cursor (cursor stream depth)
  (declare (type cursor cursor)
	   (ignore depth))
  (print-unreadable-object (cursor stream :type t)
    (print-display-name (cursor-display cursor) stream)
    (write-string " " stream)
    (prin1 (cursor-id cursor) stream)))

; Atoms are accepted as strings or symbols, and are always returned as keywords.
; Protocol-level integer atom ids are hidden, using a cache in the display object.

(deftype xatom () '(or string symbol))

(defconstant +predefined-atoms+
 '#(nil :PRIMARY :SECONDARY :ARC :ATOM :BITMAP
    :CARDINAL :COLORMAP :CURSOR
    :CUT_BUFFER0 :CUT_BUFFER1 :CUT_BUFFER2 :CUT_BUFFER3
    :CUT_BUFFER4 :CUT_BUFFER5 :CUT_BUFFER6 :CUT_BUFFER7
    :DRAWABLE :FONT :INTEGER :PIXMAP :POINT :RECTANGLE
    :RESOURCE_MANAGER :RGB_COLOR_MAP :RGB_BEST_MAP
    :RGB_BLUE_MAP :RGB_DEFAULT_MAP
    :RGB_GRAY_MAP :RGB_GREEN_MAP :RGB_RED_MAP :STRING
    :VISUALID :WINDOW :WM_COMMAND :WM_HINTS
    :WM_CLIENT_MACHINE :WM_ICON_NAME :WM_ICON_SIZE
    :WM_NAME :WM_NORMAL_HINTS :WM_SIZE_HINTS
    :WM_ZOOM_HINTS :MIN_SPACE :NORM_SPACE :MAX_SPACE
    :END_SPACE :SUPERSCRIPT_X :SUPERSCRIPT_Y
    :SUBSCRIPT_X :SUBSCRIPT_Y
    :UNDERLINE_POSITION :UNDERLINE_THICKNESS
    :STRIKEOUT_ASCENT :STRIKEOUT_DESCENT
    :ITALIC_ANGLE :X_HEIGHT :QUAD_WIDTH :WEIGHT
    :POINT_SIZE :RESOLUTION :COPYRIGHT :NOTICE
    :FONT_NAME :FAMILY_NAME :FULL_NAME :CAP_HEIGHT
    :WM_CLASS :WM_TRANSIENT_FOR))

(deftype stringable () '(or string symbol))

(deftype fontable () '(or stringable font))

; Nil stands for CurrentTime.

(deftype timestamp () '(or null card32))

(defconstant +bit-gravity-vector+
 '#(:forget :north-west :north :north-east :west
    :center :east :south-west :south
    :south-east :static))

(deftype bit-gravity ()
  '(member :forget :north-west :north :north-east :west
	   :center :east :south-west :south :south-east :static))

(defconstant +win-gravity-vector+
 '#(:unmap :north-west :north :north-east :west
    :center :east :south-west :south :south-east
    :static))

(defparameter *protocol-families*
  '(;; X11/X.h, Family*
    (:internet . 0)
    (:decnet . 1)
    (:chaos . 2)
    ;; X11/Xauth.h "not part of X standard"
    (:Local . 256)
    (:Wild . 65535)
    (:Netname . 254)
    (:Krb5Principal . 253)
    (:LocalHost . 252)))

(deftype win-gravity ()
  '(member :unmap :north-west :north :north-east :west
	   :center :east :south-west :south :south-east :static))

(deftype grab-status ()
  '(member :success :already-grabbed :invalid-time :not-viewable))

; An association list.

(deftype alist (key-type-and-name datum-type-and-name)
  (declare (ignore key-type-and-name datum-type-and-name))
  'list)

(deftype clx-list (&optional element-type) (declare (ignore element-type)) 'list)
(deftype clx-sequence (&optional element-type) (declare (ignore element-type)) 'sequence)

; A sequence, containing zero or more repetitions of the given elements,
; with the elements expressed as (type name).

(deftype repeat-seq (&rest elts) elts 'sequence)

(deftype point-seq () '(repeat-seq (int16 x) (int16 y)))

(deftype seg-seq () '(repeat-seq (int16 x1) (int16 y1) (int16 x2) (int16 y2)))

(deftype rect-seq () '(repeat-seq (int16 x) (int16 y) (card16 width) (card16 height)))

(deftype arc-seq ()
  '(repeat-seq (int16 x) (int16 y) (card16 width) (card16 height)
	       (angle angle1) (angle angle2)))

(deftype gcontext-state () 'simple-vector)

(def-clx-class (gcontext (:copier nil) (:print-function print-gcontext))
  ;; The accessors convert to CLX data types.
  (id 0 :type resource-id)
  (display nil :type (or null display))
  (drawable nil :type (or null drawable))
  (cache-p t :type generalized-boolean)
  (server-state (allocate-gcontext-state) :type gcontext-state)
  (local-state (allocate-gcontext-state) :type gcontext-state)
  (plist nil :type list)			; Extension hook
  (next nil #-explorer :type #-explorer (or null gcontext))
  )

(defun print-gcontext (gcontext stream depth)
  (declare (type gcontext gcontext)
	   (ignore depth))
  (print-unreadable-object (gcontext stream :type t)
    (print-display-name (gcontext-display gcontext) stream)
    (write-string " " stream)
    (prin1 (gcontext-id gcontext) stream)))

(defconstant +event-mask-vector+
 '#(:key-press :key-release :button-press :button-release
    :enter-window :leave-window :pointer-motion :pointer-motion-hint
    :button-1-motion :button-2-motion :button-3-motion :button-4-motion
    :button-5-motion :button-motion :keymap-state :exposure :visibility-change
    :structure-notify :resize-redirect :substructure-notify :substructure-redirect
    :focus-change :property-change :colormap-change :owner-grab-button))

(deftype event-mask-class ()
  '(member :key-press :key-release :owner-grab-button :button-press :button-release
	   :enter-window :leave-window :pointer-motion :pointer-motion-hint
	   :button-1-motion :button-2-motion :button-3-motion :button-4-motion
	   :button-5-motion :button-motion :exposure :visibility-change
	   :structure-notify :resize-redirect :substructure-notify :substructure-redirect
	   :focus-change :property-change :colormap-change :keymap-state))

(deftype event-mask ()
  '(or mask32 (clx-list event-mask-class)))

(defconstant +pointer-event-mask-vector+
  ;; the first two elements used to be '%error '%error (i.e. symbols, 
  ;; and not keywords) but the vector is supposed to contain 
  ;; keywords, so I renamed them -dan 2004.11.13
  '#(:%error :%error :button-press :button-release
     :enter-window :leave-window :pointer-motion :pointer-motion-hint
     :button-1-motion :button-2-motion :button-3-motion :button-4-motion
     :button-5-motion :button-motion :keymap-state))

(deftype pointer-event-mask-class ()
  '(member :button-press :button-release
	   :enter-window :leave-window :pointer-motion :pointer-motion-hint
	   :button-1-motion :button-2-motion :button-3-motion :button-4-motion
	   :button-5-motion :button-motion :keymap-state))

(deftype pointer-event-mask ()
  '(or mask32 (clx-list pointer-event-mask-class)))

(defconstant +device-event-mask-vector+
 '#(:key-press :key-release :button-press :button-release :pointer-motion
    :button-1-motion :button-2-motion :button-3-motion :button-4-motion
    :button-5-motion :button-motion))

(deftype device-event-mask-class ()
  '(member :key-press :key-release :button-press :button-release :pointer-motion
	   :button-1-motion :button-2-motion :button-3-motion :button-4-motion
	   :button-5-motion :button-motion))

(deftype device-event-mask ()
  '(or mask32 (clx-list device-event-mask-class)))

(defconstant +state-mask-vector+
 '#(:shift :lock :control :mod-1 :mod-2 :mod-3 :mod-4 :mod-5
    :button-1 :button-2 :button-3 :button-4 :button-5))

(deftype modifier-key ()
  '(member :shift :lock :control :mod-1 :mod-2 :mod-3 :mod-4 :mod-5))

(deftype modifier-mask ()
  '(or (member :any) mask16 (clx-list modifier-key)))

(deftype state-mask-key ()
  '(or modifier-key (member :button-1 :button-2 :button-3 :button-4 :button-5)))

(defconstant +gcontext-components+
 '(:function :plane-mask :foreground :background
   :line-width :line-style :cap-style :join-style :fill-style
   :fill-rule :tile :stipple :ts-x :ts-y :font :subwindow-mode
   :exposures :clip-x :clip-y :clip-mask :dash-offset :dashes
   :arc-mode))

(deftype gcontext-key ()
  '(member :function :plane-mask :foreground :background
	   :line-width :line-style :cap-style :join-style :fill-style
	   :fill-rule :tile :stipple :ts-x :ts-y :font :subwindow-mode
	   :exposures :clip-x :clip-y :clip-mask :dash-offset :dashes
	   :arc-mode))

(deftype event-key ()
  '(or (member :key-press :key-release :button-press :button-release 
        :motion-notify :enter-notify :leave-notify :focus-in :focus-out 
        :keymap-notify :exposure :graphics-exposure :no-exposure 
        :visibility-notify :create-notify :destroy-notify :unmap-notify 
        :map-notify :map-request :reparent-notify :configure-notify 
        :gravity-notify :resize-request :configure-request :circulate-notify 
        :circulate-request :property-notify :selection-clear 
        :selection-request :selection-notify :colormap-notify :client-message 
        :mapping-notify)
       (satisfies extension-event-key-p)))

(deftype error-key ()
  '(member :access :alloc :atom :colormap :cursor :drawable :font :gcontext :id-choice
	   :illegal-request :implementation :length :match :name :pixmap :value :window))

(deftype draw-direction ()
  '(member :left-to-right :right-to-left))

(defconstant +boole-vector+
 '#(#.boole-clr #.boole-and #.boole-andc2 #.boole-1
    #.boole-andc1 #.boole-2 #.boole-xor #.boole-ior
    #.boole-nor #.boole-eqv #.boole-c2 #.boole-orc2
    #.boole-c1 #.boole-orc1 #.boole-nand #.boole-set))

(deftype boole-constant ()
  `(member ,boole-clr ,boole-and ,boole-andc2 ,boole-1
	   ,boole-andc1 ,boole-2 ,boole-xor ,boole-ior
	   ,boole-nor ,boole-eqv ,boole-c2 ,boole-orc2
	   ,boole-c1 ,boole-orc1 ,boole-nand ,boole-set))

(def-clx-class (screen (:copier nil) (:print-function print-screen))
  (root nil :type (or null window))
  (width 0 :type card16)
  (height 0 :type card16)
  (width-in-millimeters 0 :type card16)
  (height-in-millimeters 0 :type card16)
  (depths nil :type (alist (image-depth depth) ((clx-list visual-info) visuals)))
  (root-depth 1 :type image-depth)
  (root-visual-info nil :type (or null visual-info))
  (default-colormap nil :type (or null colormap))
  (white-pixel 0 :type pixel)
  (black-pixel 1 :type pixel)
  (min-installed-maps 1 :type card16)
  (max-installed-maps 1 :type card16)
  (backing-stores :never :type (member :never :when-mapped :always))
  (save-unders-p nil :type generalized-boolean)
  (event-mask-at-open 0 :type mask32)
  (plist nil :type list)			; Extension hook
  )

(defun print-screen (screen stream depth)
  (declare (type screen screen)
	   (ignore depth))
  (print-unreadable-object (screen stream :type t)
    (let ((display (drawable-display (screen-root screen))))
      (print-display-name display stream)
      (write-string "." stream)
      (princ (position screen (display-roots display)) stream))
    (write-string " " stream)
    (prin1 (screen-width screen) stream)
    (write-string "x" stream)
    (prin1 (screen-height screen) stream)
    (write-string "x" stream)
    (prin1 (screen-root-depth screen) stream)
    (when (screen-root-visual-info screen)
      (write-string " " stream)
      (princ (visual-info-class (screen-root-visual-info screen)) stream))))

(defun screen-root-visual (screen)
  (declare (type screen screen)
	   (clx-values resource-id))
  (visual-info-id (screen-root-visual-info screen)))

;; The list contains alternating keywords and integers.
(deftype font-props () 'list)

(def-clx-class (font-info (:copier nil) (:predicate nil))
  (direction :left-to-right :type draw-direction)
  (min-char 0 :type card16)   ;; First character in font
  (max-char 0 :type card16)   ;; Last character in font
  (min-byte1 0 :type card8)   ;; The following are for 16 bit fonts
  (max-byte1 0 :type card8)   ;; and specify min&max values for
  (min-byte2 0 :type card8)   ;; the two character bytes
  (max-byte2 0 :type card8)
  (all-chars-exist-p nil :type generalized-boolean)
  (default-char 0 :type card16)
  (min-bounds nil :type (or null vector))
  (max-bounds nil :type (or null vector))
  (ascent 0 :type int16)
  (descent 0 :type int16)
  (properties nil :type font-props))

(def-clx-class (font (:constructor make-font-internal) (:copier nil)
		     (:print-function print-font))
  (id-internal nil :type (or null resource-id)) ;; NIL when not opened
  (display nil :type (or null display))
  (reference-count 0 :type fixnum)
  (name "" :type (or null string)) ;; NIL when ID is for a GContext
  (font-info-internal nil :type (or null font-info))
  (char-infos-internal nil :type (or null (simple-array int16 (*))))
  (local-only-p t :type generalized-boolean) ;; When T, always calculate text extents locally
  (plist nil :type list)			; Extension hook
  )

(defun print-font (font stream depth)
  (declare (type font font)
	   (ignore depth))
  (print-unreadable-object (font stream :type t)
    (if (font-name font)
	(princ (font-name font) stream)
      (write-string "(gcontext)" stream))
    (write-string " " stream)
    (print-display-name (font-display font) stream)
    (when (font-id-internal font)
      (write-string " " stream)
      (prin1 (font-id font) stream))))

(defun font-id (font)
  ;; Get font-id, opening font if needed
  (or (font-id-internal font)
      (open-font-internal font)))

(defun font-font-info (font)
  (or (font-font-info-internal font)
      (query-font font)))

(defun font-char-infos (font)
  (or (font-char-infos-internal font)
      (progn (query-font font)
	     (font-char-infos-internal font))))

(defun make-font (&key id
		  display
		  (reference-count 0)
		  (name "")
		  (local-only-p t)
		  font-info-internal)
  (make-font-internal :id-internal id
		      :display display
		      :reference-count reference-count
		      :name name
		      :local-only-p local-only-p
		      :font-info-internal font-info-internal))

; For each component (<name> <unspec> :type <type>) of font-info,
; there is a corresponding function:

;(defun font-<name> (font)
;  (declare (type font font)
;	   (clx-values <type>)))

(macrolet ((make-font-info-accessors (useless-name &body fields)
	     `(within-definition (,useless-name make-font-info-accessors)
		,@(mapcar
		    #'(lambda (field)
			(let* ((type (second field))
			       (n (string (first field)))
			       (name (xintern 'font- n))
			       (accessor (xintern 'font-info- n)))
			  `(defun ,name (font)
			     (declare (type font font))
			     (declare (clx-values ,type))
			     (,accessor (font-font-info font)))))
		    fields))))
  (make-font-info-accessors ignore
    (direction draw-direction)
    (min-char card16)
    (max-char card16)
    (min-byte1 card8)
    (max-byte1 card8)
    (min-byte2 card8)
    (max-byte2 card8)
    (all-chars-exist-p generalized-boolean)
    (default-char card16)
    (min-bounds vector)
    (max-bounds vector)
    (ascent int16)
    (descent int16)
    (properties font-props)))

(defun font-property (font name)
  (declare (type font font)
	   (type keyword name))
  (declare (clx-values (or null int32)))
  (getf (font-properties font) name))

(macrolet ((make-mumble-equal (type)
	     ;; Since caching is only done for objects created by the
	     ;; client, we must always compare ID and display for
	     ;; non-identical mumbles.
	     (let ((predicate (xintern type '-equal))
		   (id (xintern type '-id))
		   (dpy (xintern type '-display)))
		`(within-definition (,type make-mumble-equal)
		   (defun ,predicate (a b)
		     (declare (type ,type a b))
		     (or (eql a b)
			 (and (= (,id a) (,id b))
			      (eq (,dpy a) (,dpy b)))))))))
  (make-mumble-equal window)
  (make-mumble-equal pixmap)
  (make-mumble-equal cursor)
  (make-mumble-equal font)
  (make-mumble-equal gcontext)
  (make-mumble-equal colormap)
  (make-mumble-equal drawable))

;;;
;;; Event-mask encode/decode functions
;;;    Converts from keyword-lists to integer and back
;;;
(defun encode-mask (key-vector key-list key-type)
  ;; KEY-VECTOR is a vector containg bit-position keywords.  The
  ;; position of the keyword in the vector indicates its bit position
  ;; in the resulting mask.  KEY-LIST is either a mask or a list of
  ;; KEY-TYPE Returns NIL when KEY-LIST is not a list or mask.
  (declare (type (simple-array keyword (*)) key-vector)
	   (type (or mask32 list) key-list))
  (declare (clx-values (or mask32 null)))
  (typecase key-list
    (mask32 key-list)
    (list (let ((mask 0))
	    (dolist (key key-list mask)
	      (let ((bit (position key (the vector key-vector) :test #'eq)))
		(unless bit
		  (x-type-error key key-type))
		(setq mask (logior mask (ash 1 bit)))))))))

(defun decode-mask (key-vector mask)
  (declare (type (simple-array keyword (*)) key-vector)
	   (type mask32 mask))
  (declare (clx-values list))
  (do ((m mask (ash m -1))
       (bit 0 (1+ bit))
       (len (length key-vector))
       (result nil))       
      ((or (zerop m) (>= bit len)) result)
    (declare (type mask32 m)
	     (fixnum bit len)
	     (list result))
    (when (oddp m)
      (push (aref key-vector bit) result))))

(defun encode-event-mask (event-mask)
  (declare (type event-mask event-mask))
  (declare (clx-values mask32))
  (or (encode-mask +event-mask-vector+ event-mask 'event-mask-class)
      (x-type-error event-mask 'event-mask)))

(defun make-event-mask (&rest keys)
  ;; This is only defined for core events.
  ;; Useful for constructing event-mask, pointer-event-mask, device-event-mask.
  (declare (type (clx-list event-mask-class) keys))
  (declare (clx-values mask32))
  (encode-mask +event-mask-vector+ keys 'event-mask-class))

(defun make-event-keys (event-mask)
  ;; This is only defined for core events.
  (declare (type mask32 event-mask))
  (declare (clx-values (clx-list event-mask-class)))
  (decode-mask +event-mask-vector+ event-mask))

(defun encode-device-event-mask (device-event-mask)
  (declare (type device-event-mask device-event-mask))
  (declare (clx-values mask32))
  (or (encode-mask +device-event-mask-vector+ device-event-mask
		   'device-event-mask-class)
      (x-type-error device-event-mask 'device-event-mask)))

(defun encode-modifier-mask (modifier-mask)
  (declare (type modifier-mask modifier-mask))
  (declare (clx-values mask16))
  (or (and (eq modifier-mask :any) #x8000)
      (encode-mask +state-mask-vector+ modifier-mask 'modifier-key)
      (x-type-error modifier-mask 'modifier-mask)))

(defun encode-state-mask (state-mask)
  (declare (type (or mask16 (clx-list state-mask-key)) state-mask))
  (declare (clx-values mask16))
  (or (encode-mask +state-mask-vector+ state-mask 'state-mask-key)
      (x-type-error state-mask '(or mask16 (clx-list state-mask-key)))))

(defun make-state-mask (&rest keys)
  ;; Useful for constructing modifier-mask, state-mask.
  (declare (type (clx-list state-mask-key) keys))
  (declare (clx-values mask16))
  (encode-mask +state-mask-vector+ keys 'state-mask-key))

(defun make-state-keys (state-mask)
  (declare (type mask16 state-mask))
  (declare (clx-values (clx-list state-mask-key)))
  (decode-mask +state-mask-vector+ state-mask))

(defun encode-pointer-event-mask (pointer-event-mask)
  (declare (type pointer-event-mask pointer-event-mask))
  (declare (clx-values mask32))
  (or (encode-mask +pointer-event-mask-vector+ pointer-event-mask
		   'pointer-event-mask-class)
      (x-type-error pointer-event-mask 'pointer-event-mask)))
