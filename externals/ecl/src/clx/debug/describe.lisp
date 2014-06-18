;;; -*- Mode: Lisp; Package: XLIB; Syntax: COMMON-LISP; Base: 10; Lowercase: Yes; -*-

;;; Describe X11 protocol requests

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

;;; Created 07/15/87 by LaMott G. OREN

(in-package :xlib)

(defparameter *request-parameters* (make-array (length *request-names*)))

(defmacro x-request (name &rest fields)
  (unless (zerop (mod (length fields) 3))
    (format t "~%Field length not a multiple of 3 for ~a" name))
  (let ((request (position name *request-names* :test #'string-equal)))
     (if request
	 `(setf (aref *request-parameters* ,request) ',fields)
       `(format t "~%~s isn't an X11 request name" ',name))))

(defun print-history-description (buffer &optional (start 0))
  ;; Display an output history
  (reading-event (buffer)
    (let ((request (card8-get start))
	  (length (* 4 (card16-get (+ start 2))))
	  (margin 5))
      (format t "~a (~d) length ~d"
	      (request-name request) request length)
      (when (>= request (length *request-parameters*))
	(setq request 0))
      (do ((parms (aref *request-parameters* request) (cdddr parms))
	   (j start))	  
	  ((or (endp parms) (>= j length)))
	(let ((len (first parms))
	      (type (second parms))
	      (doc (third parms))
	      value)
	  (setq value (case len
			(1 (card8-get j))
			(2 (card16-get j))
			(4 (card32-get j))))
	  (format t "~%~v@t" margin)
	  (if value
	      (progn
		(print-value j value type doc)
		     (incf j len))
	    (progn 
	      (format t "~2d ~10a ~a"
		      j type doc)
	      (case type
		((listofvalue listofcard32 listofatom)
		 (format t " Words:~%~v@t" margin)
		 (dotimes (k (floor (- length (- j start)) 4))
		   (format t " ~d" (card32-get j))
		   (incf j 4)))
		(listofrectangle
		 (format t " Half-Words:~%~v@t" margin)
		 (dotimes (k (floor (- length (- j start)) 2))
		   (format t " ~d" (card16-get j))
		   (incf j 2)))
		(x (when (integerp len) (incf j len)))	; Unused
		(string8
		 (format t " Bytes:~%~v@t" margin)
		 (dotimes (k (- length (- j start)))
		   (format t "~a" (int-char (card8-get j)))
		   (incf j)))
		(otherwise
		 (format t " Bytes:~%~v@t" margin)
		 (dotimes (k (- length (- j start)))
		   (format t " ~d" (card8-get j))
		   (incf j)))))))))))

(defun print-value (i value type doc &aux temp)
  (format t "~2d ~3d " i value)
  (if (consp type)
      (case (first type)
	(bitmask (format t "~a" (nreverse (decode-mask (symbol-value (second type)) value)))
		 (setq type (car type)))
	(member (if (null (setq temp (nth value (cdr type))))
		    (format t "*****ERROR*****")
		  (format t "~a" temp))
		(setq type (car type))))
    (case type
      ((window pixmap drawable cursor font gcontext colormap atom)
       (format t "[#x~x]" value)
       #+comment
       (let ((temp (lookup-resource-id display value)))
	 (when (eq (first type) 'atom)
	   (setq temp (lookup-xatom display value)))
	 (when temp (format t " (~s)" (type-of temp)))))
      (int16 (setq temp (card16->int16 value))
	     (when (minusp temp) (format t "~d" temp)))
      (otherwise
       (when (and (numberp type) (not (= type value)))
	 (format t "*****ERROR*****")))))
  (format t "~30,10t ~10a ~a" type doc))

(x-request Error
   1	1			opcode
   1	CARD8			data
   2	8+n			request-length
   n	LISTofBYTE		data
   )

(x-request CreateWindow
   1	1			opcode
   1	CARD8			depth
   2	8+n			request-length
   4	WINDOW			wid
   4	WINDOW			parent
   2	INT16			x
   2	INT16			y
   2	CARD16			width
   2	CARD16			height
   2	CARD16			border-width
   2	(MEMBER CopyFromParent InputOutput InputOnly) class
   4	(OR (MEMBER CopyFromParent) VISUALID)		visual
   4	(BITMASK *create-bitmask*) value-mask
  4n	LISTofVALUE		value-list
  )

(defparameter *create-bitmask*
	      #(background-pixmap background-pixel border-pixmap border-pixel bit-gravity
		win-gravity backing-store backing-planes backing-pixel override-redirect
		save-under event-mask do-not-propagate-mask colormap cursor))

(x-request ChangeWindowAttributes
   1	2			opcode
   1	x			unused
   2	3+n			request-length
   4	WINDOW			window
   4	(BITMASK *create-bitmask*) value-mask
  4n	LISTofVALUE		value-list
  )

(x-request GetWindowAttributes
   1	3			opcode
   1	x			unused
   2	2			request-length
   4	WINDOW			window
)

(x-request DestroyWindow
   1	4			opcode
   1	x			unused
   2	2			request-length
   4	WINDOW			window
)

(x-request DestroySubwindows
   1	5			opcode
   1	x			unused
   2	2			request-length
   4	WINDOW			window
)

(x-request ChangeSaveSet
   1	6			opcode
   1	(MEMBER insert delete)	mode
   2	2			request-length
   4	WINDOW			window
)

(x-request ReparentWindow
  1	7			opcode
   1	x			unused
   2	4			request-length
   4	WINDOW			window
   4	WINDOW			parent
   2	INT16			x
   2	INT16			y
)

(x-request MapWindow
   1	8			opcode
   1	x			unused
   2	2			request-length
   4	WINDOW			window
)

(x-request MapSubwindows
   1	9			opcode
   1	x			unused
   2	2			request-length
   4	WINDOW			window
)

(x-request UnmapWindow
   1	10			opcode
   1	x			unused
   2	2			request-length
   4	WINDOW			window
)

(x-request UnmapSubwindows
   1	11			opcode
   1	x			unused
   2	2			request-length
   4	WINDOW			window
)

(x-request ConfigureWindow
   1	12			opcode
   1	x			unused
   2	3+n			request-length
   4	WINDOW			window
   2	BITMASK			value-mask
   2	x			unused
  4n	LISTofVALUE		value-list
)

(x-request CirculateWindow
   1	13			opcode
   1	(MEMBER RaiseLowest LowerHighest) direction
   2	2			request-length
   4	WINDOW			window
)

(x-request GetGeometry
   1	14			opcode
   1	x			unused
   2	2			request-length
   4	DRAWABLE		drawable
)

(x-request QueryTree
   1	15			opcode
   1	x			unused
   2	2			request-length
   4	WINDOW			window
)

(x-request InternAtom
   1	16			opcode
   1	BOOL			only-if-exists
   2	|2+(n+p)/4|		request-length
   2	n			length-of-name
   2	x			unused
   n	STRING8			name
   p	x			unused
)

(x-request GetAtomName
   1	17			opcode
   1	x			unused
   2	2			request-length
   4	ATOM			atom
)

(x-request ChangeProperty
   1	18			opcode
   1	(MEMBER replace prepend append)	mode
   2	|6+(n+p)/4|		request-length
   4	WINDOW			window
   4	ATOM			property
   4	ATOM			type
   1	CARD8			format
   3	x			unused
   4	CARD32			length-of-data-in-format-units
   n	LISTofBYTE		data
   p	x			unused
)

(x-request DeleteProperty
   1	19			opcode
   1	x			unused
   2	3			request-length
   4	WINDOW			window
   4	ATOM			property
)

(x-request GetProperty
   1	20			opcode
   1	BOOL			delete
   2	6			request-length
   4	WINDOW			window
   4	ATOM			property
   4	(OR (MEMBER anypropertytype) ATOM) type
   4	CARD32			long-offset
   4	CARD32			long-length
)

(x-request ListProperties
   1	21			opcode
   1	x			unused
   2	2			request-length
   4	WINDOW			window
)

(x-request SetSelectionOwner
   1	22			opcode
   1	x			unused
   2	4			request-length
   4	(OR (MEMBER none) WINDOW) owner
   4	ATOM			selection
   4	(OR (MEMBER currenttime) TIMESTAMP) time
)

(x-request GetSelectionOwner
   1	23			opcode
   1	x			unused
   2	2			request-length
   4	ATOM			selection
)

(x-request ConvertSelection
   1	24			opcode
   1	x			unused
   2	6			request-length
   4	WINDOW			requestor
   4	ATOM			selection
   4	ATOM			target
   4	(OR (MEMBER none) ATOM)	property
   4	(OR (MEMBER currenttime) TIMESTAMP) time
)

(x-request SendEvent
   1	25			opcode
   1	BOOL			propagate
   2	11			request-length
   4	(OR (MEMBER pointerwindow inputfocus) WINDOW) destination
   4	SETofEVENT		event-mask
  32	n			event
)

(x-request GrabPointer
   1	26			opcode
   1	BOOL			owner-events
   2	6			request-length
   4	WINDOW			grab-window
   2	SETofPOINTEREVENT	event-mask
   1	(MEMBER Synchronous Asynchronous) pointer-mode
   1	(MEMBER Synchronous Asynchronous) keyboard-mode
   4	(OR (MEMBER none) WINDOW) confine-to
   4	(OR (MEMBER none) CURSOR) cursor
   4	(OR (MEMBER currenttime) TIMESTAMP) timestamp
)

(x-request UngrabPointer
   1	27			opcode
   1	x			unused
   2	2			request-length
   4	(OR (MEMBER currenttime) TIMESTAMP) time
)

(x-request GrabButton
   1	28			opcode
   1	BOOL			owner-events
   2	6			request-length
   4	WINDOW			grab-window
   2	SETofPOINTEREVENT	event-mask
   1	(MEMBER Synchronous Asynchronous) pointer-mode
   1	(MEMBER Synchronous Asynchronous) keyboard-mode
   4	(OR (MEMBER none) WINDOW) confine-to
   4	(OR (MEMBER none) CURSOR) cursor
   1	(OR (MEMBER anybutton) BUTTON)button
   1	x			unused
   2	SETofKEYMASK		modifiers
)

(x-request UngrabButton
   1	29			opcode
   1	(OR (MEMBER anybutton) BUTTON) button
   2	3			request-length
   4	WINDOW			grab-window
   2	SETofKEYMASK		modifiers
   2	x			unused
)

(x-request ChangeActivePointerGrab
   1	30			opcode
   1	x			unused
   2	4			request-length
   4	(OR (MEMBER none) CURSOR) cursor
   4	(OR (MEMBER currenttime) TIMESTAMP) time
   2	SETofPOINTEREVENT	event-mask
   2	x			unused
)

(x-request GrabKeyboard
   1	31			opcode
   1	BOOL			owner-events
   2	4			request-length
   4	WINDOW			grab-window
   4	(OR (MEMBER currenttime) TIMESTAMP) time
   1	(MEMBER Synchronous Asynchronous) pointer-mode
   1	(MEMBER Synchronous Asynchronous) keyboard-mode
   2	x			unused
)

(x-request UngrabKeyboard
   1	32			opcode
   1	x			unused
   2	2			request-length
   4	(OR (MEMBER currenttime) TIMESTAMP) time
)

(x-request GrabKey
   1	33			opcode
   1	BOOL			owner-events
   2	4			request-length
   4	WINDOW			grab-window
   2	SETofKEYMASK		modifiers
   1	(OR (MEMBER anykey) KEYCODE) key
   1	(MEMBER Synchronous Asynchronous) pointer-mode
   1	(MEMBER Synchronous Asynchronous) keyboard-mode
   3	x			unused
)

(x-request UngrabKey
   1	34			opcode
   1	(OR (MEMBER anykey) KEYCODE) key
   2	3			request-length
   4	WINDOW			grab-window
   2	SETofKEYMASK		modifiers
   2	x			unused
)

(x-request AllowEvents
   1	35			opcode
   1	(MEMBER AsyncPointer SyncPointer ReplayPointer AsyncKeyboard SyncKeyboard ReplayKeyboard) mode
   2	2			request-length
   4	(OR (MEMBER currenttime) TIMESTAMP) time
)

(x-request GrabServer
   1	36			opcode
   1	x			unused
   2	1			request-length
)

(x-request UngrabServer
   1	37			opcode
   1	x			unused
   2	1			request-length
)

(x-request QueryPointer
   1	38			opcode
   1	x			unused
   2	2			request-length
   4	WINDOW			window
)

(x-request GetMotionEvents
   1	39			opcode
   1	x			unused
   2	4			request-length
   4	WINDOW			window
   4	(OR (MEMBER CURRENTTIME) TIMESTAMP)		start
   4	(OR (MEMBER CURRENTTIME) TIMESTAMP)		stop
)

(x-request TranslateCoords
   1	40			opcode
   1	x			unused
   2	4			request-length
   4	WINDOW			src-window
   4	WINDOW			dst-window
   2	INT16			src-x
   2	INT16			src-y
)

(x-request WarpPointer
   1	41			opcode
   1	x			unused
   2	6			request-length
   4	(OR (MEMBER none) WINDOW)			src-window
   4	WINDOW			dst-window
   2	INT16			src-x
   2	INT16			src-y
   2	CARD16			src-width
   2	CARD16			src-height
   2	INT16			dst-x
   2	INT16			dst-y
)

(x-request SetInputFocus
   1	42			opcode
   1	(MEMBER none pointerroot parent) revert-to
   2	3			request-length
   4	(OR (MEMBER none pointerroot) WINDOW) focus
   4	(OR (MEMBER CURRENTTIME) TIMESTAMP)		time
)

(x-request GetInputFocus
   1	43			opcode
   1	x			unused
   2	1			request-length
)

(x-request QueryKeymap
   1	44			opcode
   1	x			unused
   2	1			request-length
)

(x-request OpenFont
   1	45			opcode
   1	x			unused
   2	|3+(n+p)/4|		request-length
   4	FONT			fid
   2	n			length-of-name
   2	x			unused
   n	STRING8			name
   p	x			unused
)

(x-request CloseFont
   1	46			opcode
   1	x			unused
   2	2			request-length
   4	FONT			font
)

(x-request QueryFont
   1	47			opcode
   1	x			unused
   2	2			request-length
   4	FONTABLE		font
)

(x-request QueryTextExtents
   1	48			opcode
   1	BOOL			odd-length-p
   2	|2+(2n+p)/4|		request-length
   4	FONTABLE		font
  2n	STRING16		string
   p	x			unused
)

(x-request ListFonts
   1	49			opcode
   1	x			unused
   2	|2+(n+p)/4|		request-length
   2	CARD16			max-names
   2	n			length-of-pattern
   n	STRING8			pattern
   p	x			unused
)

(x-request ListFontsWithInfo
   1	50			opcode
   1	x			unused
   2	|2+(n+p)/4|		request-length
   2	CARD16			max-names
   2	n			length-of-pattern
   n	STRING8			pattern
   p	x			unused
)

(x-request SetFontPath
   1	51			opcode
   1	x			unused
   2	|2+(n+p)/4|		request-length
   2	CARD16			number-of-STRs-in-path
   2	x			unused
   n	LISTofSTR		path
   p	x			unused
)

(x-request GetFontPath
   1	52			opcode
   1	x			unused
   2	1			request-list
)

(x-request CreatePixmap
   1	53			opcode
   1	CARD8			depth
   2	4			request-length
   4	PIXMAP			pid
   4	DRAWABLE		drawable
   2	CARD16			width
   2	CARD16			height
)

(x-request FreePixmap
   1	54			opcode
   1	x			unused
   2	2			request-length
   4	PIXMAP			pixmap
)

(x-request CreateGC
   1	55			opcode
   1	x			unused
   2	4+n			request-length
   4	GCONTEXT		cid
   4	DRAWABLE		drawable
   4	(BITMASK *gc-bitmask*)	value-mask
  4n	LISTofVALUE		value-list
)

(defconstant *gc-bitmask*
	     #(function plane-mask foreground
	       background line-width line-style cap-style join-style
	       fill-style fill-rule tile stipple tile-stipple-x-origin
	       tile-stipple-y-origin font subwindow-mode graphics-exposures clip-x-origin
	       clip-y-origin clip-mask dash-offset dashes arc-mode))


(x-request ChangeGC
   1	56			opcode
   1	x			unused
   2	3+n			request-length
   4	GCONTEXT		gc
   4	(BITMASK *gc-bitmask*)	value-mask
  4n	LISTofVALUE		value-list
)

(x-request CopyGC
   1	57			opcode
   1	x			unused
   2	4			request-length
   4	GCONTEXT		src-gc
   4	GCONTEXT		dst-gc
   4	(BITMASK *gc-bitmask*)	value-mask
)

(x-request SetDashes
   1	58			opcode
   1	x			unused
   2	|3+(n+p)/4|		request-length
   4	GCONTEXT		gc
   2	CARD16			dash-offset
   2	n			length-of-dashes
   n	LISTofCARD8		dashes
   p	x			unused
)

(x-request SetClipRectangles
   1	59			opcode
   1	(MEMBER UnSorted YSorted YXSorted YXBanded) ordering
   2	3+2n			request-length
   4	GCONTEXT		gc
   2	INT16			clip-x-origin
   2	INT16			clip-y-origin
  8n	LISTofRECTANGLE		rectangles
)

(x-request FreeGC
   1	60			opcode
   1	x			unused
   2	2			request-length
   4	GCONTEXT		gc
)

(x-request ClearToBackground
   1	61			opcode
   1	BOOL			exposures
   2	4			request-length
   4	WINDOW			window
   2	INT16			x
   2	INT16			y
   2	CARD16			width
   2	CARD16			height
)

(x-request CopyArea
   1	62			opcode
   1	x			unused
   2	7			request-length
   4	DRAWABLE		src-drawable
   4	DRAWABLE		dst-drawable
   4	GCONTEXT		gc
   2	INT16			src-x
   2	INT16			src-y
   2	INT16			dst-x
   2	INT16			dst-y
   2	CARD16			width
   2	CARD16			height
)

(x-request CopyPlane
   1	63			opcode
   1	x			unused
   2	8			request-length
   4	DRAWABLE		src-drawable
   4	DRAWABLE		dst-drawable
   4	GCONTEXT		gc
   2	INT16			src-x
   2	INT16			src-y
   2	INT16			dst-x
   2	INT16			dst-y
   2	CARD16			width
   2	CARD16			height
   4	CARD32			bit-plane
)

(x-request PolyPoint
   1	64			opcode
   1	(MEMBER origin previous) coordinate-mode
   2	3+n			request-length
   4	DRAWABLE		drawable
   4	GCONTEXT		gc
  4n	LISTofPOINT		points
)

(x-request PolyLine
   1	65			opcode
   1	(MEMBER origin previous) coordinate-mode
   2	3+n			request-length
   4	DRAWABLE		drawable
   4	GCONTEXT		gc
  4n	LISTofPOINT		points
)

(x-request PolySegment
   1	66			opcode
   1	x			unused
   2	3+2n			request-length
   4	DRAWABLE		drawable
   4	GCONTEXT		gc
  8n	LISTofSEGMENT		segments
)

(x-request PolyRectangle
   1	67			opcode
   1	x			unused
   2	3+2n			request-length
   4	DRAWABLE		drawable
   4	GCONTEXT		gc
  8n	LISTofRECTANGLE		rectangles
)

(x-request PolyArc
   1	68			opcode
   1	x			unused
   2	3+3n			request-length
   4	DRAWABLE		drawable
   4	GCONTEXT		gc
 12n	LISTofARC		arcs
)

(x-request FillPoly
   1	69			opcode
   1	x			unused
   2	4+n			request-length
   4	DRAWABLE		drawable
   4	GCONTEXT		gc
   1	(MEMBER complex nonconvex convex) shape
   1	(MEMBER origin previous) coordinate-mode
   2	x			unused
  4n	LISTofPOINT		points
)

(x-request PolyFillRectangle
   1	70			opcode
   1	x			unused
   2	3+2n			request-length
   4	DRAWABLE		drawable
   4	GCONTEXT		gc
  8n	LISTofRECTANGLE		rectangles
)

(x-request PolyFillArc
   1	71			opcode
   1	x			unused
   2	3+3n			request-length
   4	DRAWABLE		drawable
   4	GCONTEXT		gc
 12n	LISTofARC		arcs
)

(x-request PutImage
   1	72			opcode
   1	(bitmap xypixmap zpixmap) format
   2	|6+(n+p)/4|		request-length
   4	DRAWABLE		drawable
   4	GCONTEXT		gc
   2	CARD16			width
   2	CARD16			height
   2	INT16			dst-x
   2	INT16			dst-y
   1	CARD8			left-pad
   1	CARD8			depth
   2	x			unused
   n	LISTofBYTE		data
   p	x			unused
)

(x-request GetImage
   1	73			opcode
   1	(MEMBER error xypixmap zpixmap)	format
   2	5			request-length
   4	DRAWABLE		drawable
   2	INT16			x
   2	INT16			y
   2	CARD16			width
   2	CARD16			height
   4	CARD32			plane-mask
)

(x-request PolyText8
   1	74			opcode
   1	x			unused
   2	|4+(n+p)/4|		request-length
   4	DRAWABLE		drawable
   4	GCONTEXT		gc
   2	INT16			x
   2	INT16			y
   n	LISTofTEXTITEM8		items
   p	x			unused
)

(x-request PolyText16
   1	75			opcode
   1	x			unused
   2	|4+(n+p)/4|		request-length
   4	DRAWABLE		drawable
   4	GCONTEXT		gc
   2	INT16			x
   2	INT16			y
   n	LISTofTEXTITEM16	items
   p	x			unused
)

(x-request ImageText8
   1	76			opcode
   1	n			length-of-string
   2	|4+(n+p)/4|		request-length
   4	DRAWABLE		drawable
   4	GCONTEXT		gc
   2	INT16			x
   2	INT16			y
   n	STRING8			string
   p	x			unused
)

(x-request ImageText16
   1	77			opcode
   1	n			number-of-CHAR2Bs-in-string
   2	|4+(2n+p)/4|		request-length
   4	DRAWABLE		drawable
   4	GCONTEXT		gc
   2	INT16			x
   2	INT16			y
  2n	STRING16		string
   p	x			unused
)

(x-request CreateColormap
   1	78			opcode
   1	(MEMBER none all)	alloc
   2	4			request-length
   4	COLORMAP		mid
   4	WINDOW			window
   4	VISUALID		visual
)

(x-request FreeColormap
   1	79			opcode
   1	x			unused
   2	2			request-length
   4	COLORMAP		cmap
)

(x-request CopyColormapAndFree
   1	80			opcode
   1	x			unused
   2	3			request-length
   4	COLORMAP		mid
   4	COLORMAP		src-cmap
)

(x-request InstallColormap
   1	81			opcode
   1	x			unused
   2	2			request-length
   4	COLORMAP		cmap
)

(x-request UninstallColormap
   1	82			opcode
   1	x			unused
   2	2			request-length
   4	COLORMAP		cmap
)

(x-request ListInstalledColormaps
   1	83			opcode
   1	x			unused
   2	2			request-length
   4	WINDOW			window
)

(x-request AllocColor
   1	84			opcode
   1	x			unused
   2	4			request-length
   4	COLORMAP		cmap
   2	CARD16			red
   2	CARD16			green
   2	CARD16			blue
   2	x			unused
)

(x-request AllocNamedColor
   1	85			opcode
   1	x			unused
   2	|3+(n+p)/4|		request-length
   4	COLORMAP		cmap
   2	n			length-of-name
   2	x			unused
   n	STRING8			name
   p	x			unused
)

(x-request AllocColorCells
   1	86			opcode
   1	BOOL			contiguous
   2	3			request-length
   4	COLORMAP		cmap
   2	CARD16			colors
   2	CARD16			planes
)

(x-request AllocColorPlanes
   1	87			opcode
   1	BOOL			contiguous
   2	4			request-length
   4	COLORMAP		cmap
   2	CARD16			colors
   2	CARD16			reds
   2	CARD16			greens
   2	CARD16			blues
)

(x-request FreeColors
   1	88			opcode
   1	x			unused
   2	3+n			request-length
   4	COLORMAP		cmap
   4	CARD32			plane-mask
  4n	LISTofCARD32		pixels
)

(x-request StoreColors
   1	89			opcode
   1	x			unused
   2	2+3n			request-length
   4	COLORMAP		cmap
 12n	LISTofCOLORITEM		items
)

(x-request StoreNamedColor
   1	90			opcode
   1	color-mask		do-red_do-green_do-blue
   2	|4+(n+p)/4|		request-length
   4	COLORMAP		cmap
   4	CARD32			pixel
   2	n			length-of-name
   2	x			unused
   n	STRING8			name
   p	x			unused
)

(x-request QueryColors
   1	91			opcode
   1	x			unused
   2	2+n			request-length
   4	COLORMAP		cmap
  4n	LISTofCARD32		pixels
)

(x-request LookupColor
   1	92			opcode
   1	x			unused
   2	|3+(n+p)/4|		request-length
   4	COLORMAP		cmap
   2	n			length-of-name
   2	x			unused
   n	STRING8			name
   p	x			unused
)

(x-request CreateCursor
   1	93			opcode
   1	x			unused
   2	8			request-length
   4	CURSOR			cid
   4	PIXMAP			source
   4	(OR (MEMBER none) PIXMAP) mask
   2	CARD16			fore-red
   2	CARD16			fore-green
   2	CARD16			fore-blue
   2	CARD16			back-red
   2	CARD16			back-green
   2	CARD16			back-blue
   2	CARD16			x
   2	CARD16			y
)

(x-request CreateGlyphCursor
   1	94			CreateGlyphCursor
   1	x			unused
   2	8			request-length
   4	CURSOR			cid
   4	FONT			source-font
   4	(OR (MEMBER none) FONT)	mask-font
   2	CARD16			source-char
   2	CARD16			mask-char
   2	CARD16			fore-red
   2	CARD16			fore-green
   2	CARD16			fore-blue
   2	CARD16			back-red
   2	CARD16			back-green
   2	CARD16			back-blue
)

(x-request FreeCursor
   1	95			opcode
   1	x			unused
   2	2			request-length
   4	CURSOR			cursor
)

(x-request RecolorCursor
   1	96			opcode
   1	x			unused
   2	5			request-length
   4	CURSOR			cursor
   2	CARD16			fore-red
   2	CARD16			fore-green
   2	CARD16			fore-blue
   2	CARD16			back-red
   2	CARD16			back-green
   2	CARD16			back-blue
)

(x-request QueryBestSize
   1	97			opcode
   1	(MEMBER cursor tile stipple) class
   2	3			request-length
   4	DRAWABLE		drawable
   2	CARD16			width
   2	CARD16			height
)

(x-request QueryExtension
   1	98			opcode
   1	x			unused
   2	|2+(n+p)/4|		request-length
   2	n			length-of-name
   2	x			unused
   n	STRING8			name
   p	x			unused
)

(x-request ListExtensions
   1	99			opcode
   1	x			unused
   2	1			request-length
)

(x-request SetKeyboardMapping
   1	100			opcode
   1	n			keycode-count
   2	2+nm			request-length
   1	KEYCODE			first-keycode
   1	m			keysyms-per-keycode
   2	x			unused
 4nm	LISTofKEYSYM		keysyms
)

(x-request GetKeyboardMapping
   1	101			opcode
   1	x			unused
   2	2			request-length
   1	KEYCODE			first-keycode
   1	CARD8			count
   2	x			unused
)

(x-request ChangeKeyboardControl
   1	102			opcode
   1	x			unused
   2	2+n			request-length
   4	BITMASK			value-mask
  4n	LISTofVALUE		value-list
)

(x-request GetKeyboardControl
   1	103			opcode
   1	x			unused
   2	1			request-length
)

(x-request Bell
   1	104			opcode
   1	INT8			percent
   2	1			request-length
)

(x-request ChangePointerControl
   1	105			opcode
   1	x			unused
   2	3			request-length
   2	INT16			acceleration-numerator
   2	INT16			acceleration-denominator
   2	INT16			threshold
   1	BOOL			do-acceleration
   1	BOOL			do-threshold
)

(x-request GetPointerControl
   1	106			GetPointerControl
   1	x			unused
   2	1			request-length
)

(x-request SetScreenSaver
   1	107			opcode
   1	x			unused
   2	3			request-length
   2	INT16			timeout
   2	INT16			interval
   1	(MEMBER no yes default)	prefer-blanking
   1	(MEMBER no yes default)	allow-exposures
   2	x			unused
)

(x-request GetScreenSaver
   1	108			opcode
   1	x			unused
   2	1			request-length
)

(x-request ChangeHosts
   1	109			opcode
   1	(MEMBER insert delete)	mode
   2	|2+(n+p)/4|		request-length
   1	(MEMBER internet decnet chaos) family
   1	x			unused
   2	CARD16			length-of-address
   n	LISTofCARD8		address
   p	x			unused
)

(x-request ListHosts
   1	110			opcode
   1	x			unused
   2	1			request-length
)

(x-request ChangeAccessControl
   1	111			opcode
   1	(MEMBER disable enable)	mode
   2	1			request-length
)

(x-request ChangeCloseDownMode
   1	112			opcode
   1	(MEMBER destroy retainpermanent retaintemporary) mode
   2	1			request-length
)

(x-request KillClient
   1	113			opcode
   1	x			unused
   2	2			request-length
   4	(MEMBER alltemporary CARD32) resource
)

(x-request RotateProperties
   1	114			opcode
   1	x			unused
   2	3+n			request-length
   4	WINDOW			window
   2	n			number-of-properties
   2	INT16			delta
  4n	LISTofATOM		properties
)

(x-request ForceScreenSaver
   1	115			ForceScreenSaver
   1	(MEMBER reset activate)	mode
   2	1			request-length
)

(x-request SetPointerMapping
   1	116			opcode
   1	n			length-of-map
   2	|1+(n+p)/4|		request-length
   n	LISTofCARD8		map
   p	x			unused
)

(x-request GetPointerMapping
   1	117			opcode
   1	x			unused
   2	1			request-length
)

(x-request SetModifierMapping
   1	118			opcode
   1	KEYCODE			Lock
   2	5			request-length
   1	KEYCODE			Shift_A
   1	KEYCODE			Shift_B
   1	KEYCODE			Control_A
   1	KEYCODE			Control_B
   1	KEYCODE			Mod1_A
   1	KEYCODE			Mod1_B
   1	KEYCODE			Mod2_A
   1	KEYCODE			Mod2_B
   1	KEYCODE			Mod3_A
   1	KEYCODE			Mod3_B
   1	KEYCODE			Mod4_A
   1	KEYCODE			Mod4_B
   1	KEYCODE			Mod5_A
   1	KEYCODE			Mod5_B
   2	x			unused
)

(x-request GetModifierMapping
   1	119			opcode
   1	x			unused
   2	1			request-length
)

#+comment
(x-request NoOperation
   1	127			opcode
   1	x			unused
   2	1			request-length
)
;; End of file
