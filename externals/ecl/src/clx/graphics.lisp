;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;;; CLX drawing requests

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

(defvar *inhibit-appending* nil)

(defun draw-point (drawable gcontext x y)
  ;; Should be clever about appending to existing buffered protocol request.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y))
  (let ((display (drawable-display drawable)))
    (declare (type display display))
    (with-display (display)
      (force-gcontext-changes-internal gcontext)
      (with-buffer-output (display :length +requestsize+)
	(let* ((last-request-byte (display-last-request display))
	       (current-boffset buffer-boffset))
	  ;; To append or not append, that is the question
	  (if (and (not *inhibit-appending*)
		   last-request-byte
		   ;; Same request?
		   (= (aref-card8 buffer-bbuf last-request-byte) +x-polypoint+)
		   (progn ;; Set buffer pointers to last request
		     (set-buffer-offset last-request-byte)
		     ;; same drawable and gcontext?
		     (or (compare-request (4)
			   (data 0)
			   (drawable drawable)
			   (gcontext gcontext))
			 (progn ;; If failed, reset buffer pointers
			   (set-buffer-offset current-boffset)
			   nil))))
	      ;; Append request
	      (progn
		;; Set new request length		
		(card16-put 2 (index+ 1 (index-ash (index- current-boffset last-request-byte)
						   -2)))
		(set-buffer-offset current-boffset)
		(put-items (0)			; Insert new point
		  (int16 x y))
		(setf (display-boffset display) (index+ buffer-boffset 4)))
	    ;; New Request
	    (progn
	      (put-items (4)
		(code +x-polypoint+)
		(data 0) ;; Relative-p false
		(length 4)
		(drawable drawable)
		(gcontext gcontext)
		(int16 x y))
	      (buffer-new-request-number display)
	      (setf (buffer-last-request display) buffer-boffset)
	      (setf (display-boffset display) (index+ buffer-boffset 16)))))))
    (display-invoke-after-function display))) 


(defun draw-points (drawable gcontext points &optional relative-p)
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type sequence points)		;(repeat-seq (integer x) (integer y))
	   (type generalized-boolean relative-p))
  (with-buffer-request ((drawable-display drawable) +x-polypoint+ :gc-force gcontext)
    ((data boolean) relative-p)
    (drawable drawable)
    (gcontext gcontext)
    ((sequence :format int16) points)))

(defun draw-line (drawable gcontext x1 y1 x2 y2 &optional relative-p)
  ;; Should be clever about appending to existing buffered protocol request.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x1 y1 x2 y2)
	   (type generalized-boolean relative-p))
  (let ((display (drawable-display drawable)))
    (declare (type display display))
    (when relative-p
      (incf x2 x1)
      (incf y2 y1))
    (with-display (display)
      (force-gcontext-changes-internal gcontext)
      (with-buffer-output (display :length +requestsize+)
	(let* ((last-request-byte (display-last-request display))
	       (current-boffset buffer-boffset))
	  ;; To append or not append, that is the question
	  (if (and (not *inhibit-appending*)
		   last-request-byte
		   ;; Same request?
		   (= (aref-card8 buffer-bbuf last-request-byte) +x-polysegment+)
		   (progn ;; Set buffer pointers to last request
		     (set-buffer-offset last-request-byte)
		     ;; same drawable and gcontext?
		     (or (compare-request (4)
			   (drawable drawable)
			   (gcontext gcontext))
			 (progn ;; If failed, reset buffer pointers
			   (set-buffer-offset current-boffset)
			   nil))))
	      ;; Append request
	      (progn
		;; Set new request length
		(card16-put 2 (index+ 2 (index-ash (index- current-boffset last-request-byte)
						   -2)))
		(set-buffer-offset current-boffset)
		(put-items (0)			; Insert new point
		  (int16 x1 y1 x2 y2))
		(setf (display-boffset display) (index+ buffer-boffset 8)))
	    ;; New Request
	    (progn
	      (put-items (4)
		(code +x-polysegment+)
		(length 5)
		(drawable drawable)
		(gcontext gcontext)
		(int16 x1 y1 x2 y2))
	      (buffer-new-request-number display)
	      (setf (buffer-last-request display) buffer-boffset)
	      (setf (display-boffset display) (index+ buffer-boffset 20)))))))
    (display-invoke-after-function display))) 

(defun draw-lines (drawable gcontext points &key relative-p fill-p (shape :complex))
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type sequence points) ;(repeat-seq (integer x) (integer y))
	   (type generalized-boolean relative-p fill-p)
	   (type (member :complex :non-convex :convex) shape))
  (if fill-p
      (fill-polygon drawable gcontext points relative-p shape)
    (with-buffer-request ((drawable-display drawable)  +x-polyline+ :gc-force gcontext)
      ((data boolean) relative-p)
      (drawable drawable)
      (gcontext gcontext)
      ((sequence :format int16) points))))

;; Internal function called from DRAW-LINES
(defun fill-polygon (drawable gcontext points relative-p shape)
  ;; This is clever about appending to previous requests.  Should it be?
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type sequence points)		;(repeat-seq (integer x) (integer y))
	   (type generalized-boolean relative-p)
	   (type (member :complex :non-convex :convex) shape))
  (with-buffer-request ((drawable-display drawable)  +x-fillpoly+ :gc-force gcontext)
    (drawable drawable)
    (gcontext gcontext)
    ((member8 :complex :non-convex :convex) shape)
    (boolean relative-p)
    ((sequence :format int16) points)))

(defun draw-segments (drawable gcontext segments)
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   ;; (repeat-seq (integer x1) (integer y1) (integer x2) (integer y2)))
	   (type sequence segments)) 
  (with-buffer-request ((drawable-display drawable) +x-polysegment+ :gc-force gcontext)
    (drawable drawable)
    (gcontext gcontext)
    ((sequence :format int16) segments)))

(defun draw-rectangle (drawable gcontext x y width height &optional fill-p)
  ;; Should be clever about appending to existing buffered protocol request.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y)
	   (type card16 width height)
	   (type generalized-boolean fill-p))
  (let ((display (drawable-display drawable))
	(request (if fill-p +x-polyfillrectangle+ +x-polyrectangle+)))
    (declare (type display display)
	     (type card16 request))
    (with-display (display)
      (force-gcontext-changes-internal gcontext)
      (with-buffer-output (display :length +requestsize+)
	(let* ((last-request-byte (display-last-request display))
	       (current-boffset buffer-boffset))
	  ;; To append or not append, that is the question
	  (if (and (not *inhibit-appending*)
		   last-request-byte
		   ;; Same request?
		   (= (aref-card8 buffer-bbuf last-request-byte) request)
		   (progn ;; Set buffer pointers to last request
		     (set-buffer-offset last-request-byte)
		     ;; same drawable and gcontext?
		     (or (compare-request (4)
			   (drawable drawable)
			   (gcontext gcontext))
			 (progn ;; If failed, reset buffer pointers
			   (set-buffer-offset current-boffset)
			   nil))))
	      ;; Append request
	      (progn
		;; Set new request length
		(card16-put 2 (index+ 2 (index-ash (index- current-boffset last-request-byte)
						   -2)))
		(set-buffer-offset current-boffset)
		(put-items (0)			; Insert new point
		  (int16 x y)
		  (card16 width height))
		(setf (display-boffset display) (index+ buffer-boffset 8)))
	    ;; New Request
	    (progn
	      (put-items (4)
		(code request)
		(length 5)
		(drawable drawable)
		(gcontext gcontext)
		(int16 x y)
		(card16 width height))
	      (buffer-new-request-number display)
	      (setf (buffer-last-request display) buffer-boffset)
	      (setf (display-boffset display) (index+ buffer-boffset 20)))))))
    (display-invoke-after-function display))) 

(defun draw-rectangles (drawable gcontext rectangles &optional fill-p)
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   ;; (repeat-seq (integer x) (integer y) (integer width) (integer height)))
	   (type sequence rectangles)
	   (type generalized-boolean fill-p))
  (with-buffer-request ((drawable-display drawable)
			(if fill-p +x-polyfillrectangle+ +x-polyrectangle+)
			:gc-force gcontext)
    (drawable drawable)
    (gcontext gcontext)
    ((sequence :format int16) rectangles)))

(defun draw-arc (drawable gcontext x y width height angle1 angle2 &optional fill-p)
  ;; Should be clever about appending to existing buffered protocol request.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y)
	   (type card16 width height)
	   (type angle angle1 angle2)
	   (type generalized-boolean fill-p))
  (let ((display (drawable-display drawable))
	(request (if fill-p +x-polyfillarc+ +x-polyarc+)))
    (declare (type display display)
	     (type card16 request))
    (with-display (display)
      (force-gcontext-changes-internal gcontext)
      (with-buffer-output (display :length +requestsize+)
	(let* ((last-request-byte (display-last-request display))
	       (current-boffset buffer-boffset))
	  ;; To append or not append, that is the question
	  (if (and (not *inhibit-appending*)
		   last-request-byte
		   ;; Same request?
		   (= (aref-card8 buffer-bbuf last-request-byte) request)
		   (progn ;; Set buffer pointers to last request
		     (set-buffer-offset last-request-byte)
		     ;; same drawable and gcontext?
		     (or (compare-request (4)
			   (drawable drawable)
			   (gcontext gcontext))
			 (progn ;; If failed, reset buffer pointers
			   (set-buffer-offset current-boffset)
			   nil))))
	      ;; Append request
	      (progn
		;; Set new request length		
		(card16-put 2 (index+ 3 (index-ash (index- current-boffset last-request-byte)
						   -2)))
		(set-buffer-offset current-boffset)
		(put-items (0)			; Insert new point
		  (int16 x y)
		  (card16 width height)
		  (angle angle1 angle2))
		(setf (display-boffset display) (index+ buffer-boffset 12)))
	    ;; New Request
	    (progn
	      (put-items (4)
		(code request)
		(length 6)
		(drawable drawable)
		(gcontext gcontext)
		(int16 x y)
		(card16 width height)
		(angle angle1 angle2))
	      (buffer-new-request-number display)
	      (setf (buffer-last-request display) buffer-boffset)
	      (setf (display-boffset display) (index+ buffer-boffset 24)))))))
    (display-invoke-after-function display))) 

(defun draw-arcs-list (drawable gcontext arcs &optional fill-p)
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type list arcs) 
	   (type generalized-boolean fill-p))
  (let* ((display (drawable-display drawable))
	 (limit (index- (buffer-size display) 12))
	 (length (length arcs))
	 (request (if fill-p +x-polyfillarc+ +x-polyarc+)))
    (with-buffer-request ((drawable-display drawable) request :gc-force gcontext)
      (drawable drawable)
      (gcontext gcontext)
      (progn
	(card16-put 2 (index+ (index-ash length -1) 3))	; Set request length (in words)
	(set-buffer-offset (index+ buffer-boffset 12))  ; Position to start of data
	(do ((arc arcs))
	    ((endp arc)
	     (setf (buffer-boffset display) buffer-boffset))
	  ;; Make sure there's room
	  (when (index>= buffer-boffset limit)
	    (setf (buffer-boffset display) buffer-boffset)
	    (buffer-flush display)
	    (set-buffer-offset (buffer-boffset display)))
	  (int16-put  0 (pop arc))
	  (int16-put  2 (pop arc))
	  (card16-put 4 (pop arc))
	  (card16-put 6 (pop arc))
	  (angle-put  8 (pop arc))
	  (angle-put 10 (pop arc))
	  (set-buffer-offset (index+ buffer-boffset 12)))))))

(defun draw-arcs-vector (drawable gcontext arcs &optional fill-p)
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type vector arcs) 
	   (type generalized-boolean fill-p))
  (let* ((display (drawable-display drawable))
	 (limit (index- (buffer-size display) 12))
	 (length (length arcs))
	 (request (if fill-p +x-polyfillarc+ +x-polyarc+)))
    (with-buffer-request ((drawable-display drawable) request :gc-force gcontext)
      (drawable drawable)
      (gcontext gcontext)
      (progn
	(card16-put 2 (index+ (index-ash length -1) 3))	; Set request length (in words)
	(set-buffer-offset (index+ buffer-boffset 12))  ; Position to start of data
	(do ((n 0 (index+ n 6))
	     (length (length arcs)))
	    ((index>= n length)
	     (setf (buffer-boffset display) buffer-boffset))
	  ;; Make sure there's room
	  (when (index>= buffer-boffset limit)
	    (setf (buffer-boffset display) buffer-boffset)
	    (buffer-flush display)
	    (set-buffer-offset (buffer-boffset display)))
	  (int16-put  0 (aref arcs (index+ n 0)))
	  (int16-put  2 (aref arcs (index+ n 1)))
	  (card16-put 4 (aref arcs (index+ n 2)))
	  (card16-put 6 (aref arcs (index+ n 3)))
	  (angle-put  8 (aref arcs (index+ n 4)))
	  (angle-put 10 (aref arcs (index+ n 5)))
	  (set-buffer-offset (index+ buffer-boffset 12)))))))

(defun draw-arcs (drawable gcontext arcs &optional fill-p)
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type sequence arcs) 
	   (type generalized-boolean fill-p))
  (etypecase arcs
    (list (draw-arcs-list drawable gcontext arcs fill-p))
    (vector (draw-arcs-vector drawable gcontext arcs fill-p))))

;; The following image routines are bare minimum.  It may be useful to define
;; some form of "image" object to hide representation details and format
;; conversions.  It also may be useful to provide stream-oriented interfaces
;; for reading and writing the data.

(defun put-raw-image (drawable gcontext data &key
		      (start 0)
		      (depth (required-arg depth))
		      (x (required-arg x))
		      (y (required-arg y))
		      (width (required-arg width))
		      (height (required-arg height))
		      (left-pad 0)
		      (format (required-arg format)))
  ;; Data must be a sequence of 8-bit quantities, already in the appropriate format
  ;; for transmission; the caller is responsible for all byte and bit swapping and
  ;; compaction.  Start is the starting index in data; the end is computed from the
  ;; other arguments.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type sequence data) ; Sequence of integers
	   (type array-index start)
	   (type card8 depth left-pad) ;; required
	   (type int16 x y) ;; required
	   (type card16 width height) ;; required
	   (type (member :bitmap :xy-pixmap :z-pixmap) format))
  (with-buffer-request ((drawable-display drawable) +x-putimage+ :gc-force gcontext)
    ((data (member :bitmap :xy-pixmap :z-pixmap)) format)
    (drawable drawable)
    (gcontext gcontext)
    (card16 width height)
    (int16 x y)
    (card8 left-pad depth)
    (pad16 nil)
    ((sequence :format card8 :start start) data)))

(defun get-raw-image (drawable &key
		      data
		      (start 0)
		      (x (required-arg x))
		      (y (required-arg y))
		      (width (required-arg width))
		      (height (required-arg height))
		      (plane-mask #xffffffff)
		      (format (required-arg format))
		      (result-type '(vector card8)))
  ;; If data is given, it is modified in place (and returned), otherwise a new sequence
  ;; is created and returned, with a size computed from the other arguments and the
  ;; returned depth.  The sequence is filled with 8-bit quantities, in transmission
  ;; format; the caller is responsible for any byte and bit swapping and compaction
  ;; required for further local use.
  (declare (type drawable drawable)
	   (type (or null sequence) data) ;; sequence of integers
	   (type int16 x y) ;; required
	   (type card16 width height) ;; required
	   (type array-index start)
	   (type pixel plane-mask)
	   (type (member :xy-pixmap :z-pixmap) format))
  (declare (clx-values (clx-sequence integer) depth visual-info))
  (let ((display (drawable-display drawable)))
    (with-buffer-request-and-reply (display +x-getimage+ nil :sizes (8 32))
	 (((data (member error :xy-pixmap :z-pixmap)) format)
	  (drawable drawable)
	  (int16 x y)
	  (card16 width height)
	  (card32 plane-mask))
      (let ((depth (card8-get 1))
	    (length (* 4 (card32-get 4)))
	    (visual (resource-id-get 8)))
	(values (sequence-get :result-type result-type :format card8
			      :length length :start start :data data
			      :index +replysize+)
		depth
		(visual-info display visual))))))
