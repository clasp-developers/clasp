;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: XFree86 video mode extension
;;;   Created: 2003 03 28 15:28
;;;    Author: Iban Hatchondo <hatchond@labri.fr>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2003 by Iban Hatchondo

;;;
;;; Permission is granted to any individual or institution to use,
;;; copy, modify, and distribute this software, provided that this
;;; complete copyright and permission notice is maintained, intact, in
;;; all copies and supporting documentation.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;

;;; THIS IS NOT AN X CONSORTIUM STANDARD OR AN X PROJECT TEAM SPECIFICATION

;;; DESCRIPTION
;;;
;;; These functions provide an interface to the server extension 
;;; XFree86-VidModeExtension which allows the video modes to be 
;;; queried, adjusted dynamically and the mode switching to be 
;;; controlled.

;;; [ personal notes ]
;;;
;;; The documentation on this extension is very poor, probably,
;;; because it is not an X standard nor an X project team spec.
;;; Because of that, it need to be tested on some XFree 3.3.6,
;;; and XFree 4.3.x to ensure that all request are correctly 
;;; constructed as well as to indentify any obsolete/wrong 
;;; functions I made.

(in-package :xlib)

(export '(mode-info
	  mode-info-dotclock
	  mode-info-hdisplay
	  mode-info-hsyncstart
	  mode-info-hsyncend
	  mode-info-htotal
	  mode-info-hskew
	  mode-info-vdisplay
	  mode-info-vsyncstart
	  mode-info-vsyncend
	  mode-info-vtotal
	  mode-info-flags
	  mode-info-privsize
	  mode-info-private
	  make-mode-info

	  xfree86-vidmode-query-version
	  xfree86-vidmode-set-client-version
	  xfree86-vidmode-get-permissions
	  xfree86-vidmode-mod-mode-line 
	  xfree86-vidmode-get-mode-line 
	  xfree86-vidmode-get-all-mode-lines
	  xfree86-vidmode-add-mode-line
	  xfree86-vidmode-delete-mode-line
	  xfree86-vidmode-validate-mode-line
	  xfree86-vidmode-get-gamma
	  xfree86-vidmode-set-gamma
	  xfree86-vidmode-get-gamma-ramp
	  xfree86-vidmode-set-gamma-ramp     
	  xfree86-vidmode-get-gamma-ramp-size
	  xfree86-vidmode-lock-mode-switch
	  xfree86-vidmode-switch-to-mode
	  xfree86-vidmode-switch-mode
	  xfree86-vidmode-select-next-mode
	  xfree86-vidmode-select-prev-mode
	  xfree86-vidmode-get-monitor
	  xfree86-vidmode-get-viewport
	  xfree86-vidmode-set-viewport
	  xfree86-vidmode-get-dotclocks)
	:xlib)

;; current version numbers
;;
;; major 0 == uses parameter-to-wire functions in XFree86 libXxf86vm.
;; major 1 == uses parameter-to-wire functions hard-coded in xvidtune client.
;; major 2 == uses new protocol version in XFree86 4.0.
(defconstant +xf86vidmode-major-version+ 2)
(defconstant +xf86vidmode-minor-version+ 2)

;; requests number.
(defconstant +query-version+        0)
(defconstant +get-mode-line+        1)
(defconstant +mod-mode-line+        2)
(defconstant +switch-mode+          3)
(defconstant +get-monitor+          4)
(defconstant +lock-mode-switch+     5)
(defconstant +get-all-mode-lines+   6)
(defconstant +add-mode-line+        7)
(defconstant +delete-mode-line+     8)
(defconstant +validate-mode-line+   9)
(defconstant +switch-to-mode+      10)
(defconstant +get-viewport+        11)
(defconstant +set-viewport+        12)

;; new for version 2.x of this extension.
(defconstant +get-dot-clocks+      13)
(defconstant +set-client-version+  14)
(defconstant +set-gamma+           15)
(defconstant +get-gamma+           16)
(defconstant +get-gamma-ramp+      17)
(defconstant +set-gamma-ramp+      18)
(defconstant +get-gamma-ramp-size+ 19)
(defconstant +get-permisions+      20)

(define-extension "XFree86-VidModeExtension"
  :events (:xfree86-vidmode-notify) 
  :errors (xf86-vidmode-bad-clock 
	   xf86-vidmode-bad-htimings 
	   xf86-vidmode-bad-vtimings
	   xf86-vidmode-mode-unsuitable
	   xf86-vidmode-extension-disabled
	   xf86-vidmode-client-not-local
	   xf86-vidmode-zoom-locked))

(define-condition xf86-vidmode-bad-clock (request-error) ())
(define-condition xf86-vidmode-bad-htimings (request-error) ())
(define-condition xf86-vidmode-bad-vtimings (request-error) ())
(define-condition xf86-vidmode-mode-unsuitable (request-error) ())
(define-condition xf86-vidmode-extension-disabled (request-error) ())
(define-condition xf86-vidmode-client-not-local (request-error) ())
(define-condition xf86-vidmode-zoom-locked (request-error) ())

(define-error xf86-vidmode-bad-clock decode-core-error)
(define-error xf86-vidmode-bad-htimings decode-core-error)
(define-error xf86-vidmode-bad-vtimings decode-core-error)
(define-error xf86-vidmode-mode-unsuitable decode-core-error)
(define-error xf86-vidmode-extension-disabled decode-core-error)
(define-error xf86-vidmode-client-not-local decode-core-error)
(define-error xf86-vidmode-zoom-locked  decode-core-error)

(declare-event :XFree86-VidMode-notify
  (card16 sequence)
  (window (window event-window)) ; the root window of event screen
  (int16 state)                  ; what happend
  (int16 kind)                   ; what happend
  (boolean forced-p)             ; extents of a new region
  ((or null card32) time))       ; event timestamp

(defstruct mode-info
  (dotclock 0 :type card32)
  (hdisplay   0 :type card16)
  (hsyncstart 0 :type card16)
  (hsyncend   0 :type card16)
  (htotal     0 :type card16)
  (hskew      0 :type card32)
  (vdisplay   0 :type card16)
  (vsyncstart 0 :type card16)
  (vsyncend   0 :type card16)
  (vtotal     0 :type card16)
  (flags      0 :type card32)
  (privsize   0 :type card32)
  (private    nil :type sequence))

(defmacro vidmode-opcode (display)
  `(extension-opcode ,display "XFree86-VidModeExtension"))

(declaim (inline screen-position))
(defun screen-position (screen display)
  (declare (type display display)
	   (type screen screen))
  (declare (clx-values position))
  (let ((position (position screen (xlib:display-roots display))))
    (if (not (numberp position))
	(error "screen ~A not found in display ~A" screen display)
	position)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                       ;;;;
;;;;              public XFree86-VidMode Extension routines                ;;;;
;;;;                                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xfree86-vidmode-query-version (display)
  "Determine the version of the extension built into the server.
return two values major-version and minor-version in that order."
  (declare (type display display))
  (with-buffer-request-and-reply
      (display (vidmode-opcode display) nil :sizes 16)
    ((data +query-version+))
   (let ((major (card16-get 8))
	 (minor (card16-get 10)))
     (declare (type card16 major minor))
     (when (>= major 2)
       (XFree86-VidMode-set-client-version display))
     (values major minor))))

(defun xfree86-vidmode-set-client-version (display)
  (declare (type display display))
  (with-buffer-request (display (vidmode-opcode display))
    (data +set-client-version+)
    (card16 +xf86vidmode-major-version+)
    (card16 +xf86vidmode-minor-version+)))

(defun xfree86-vidmode-get-permissions (dpy screen)
  (declare (type display dpy)
	   (type screen screen))
  (with-buffer-request-and-reply
      (dpy (vidmode-opcode dpy) nil :sizes (8 16 32))
    ((data +get-permisions+)
     (card16 (screen-position screen dpy))
     (card16 0))
   (values 
    (card32-get 8))))

(defun xfree86-vidmode-mod-mode-line (display screen mode-line)
  "Change the settings of the current video mode provided the 
requested settings are valid (e.g. they don't exceed the 
capabilities of the monitor)."
  (declare (type display display)
	   (type screen screen))
  (let* ((major (xfree86-vidmode-query-version display))
	 (v (mode-info->v-card16 mode-line major)))
    (declare (type card16 major)
	     (type simple-vector v))
    (with-buffer-request (display (vidmode-opcode display))
      (data +mod-mode-line+)
      (card32 (screen-position screen display))
      ((sequence :format card16 :start 2) v))))

(defun xfree86-vidmode-get-mode-line (display screen)
  "Query the settings for the currently selected video mode.
return a mode-info structure fields with the server answer.
If there are any server  private  values (currently  only 
applicable  to  the S3 server) the function will store it 
into the returned structure."
  (declare (clx-values mode-info)
	   (type display display)
	   (type screen screen))
  (let ((major (xfree86-vidmode-query-version display))
	(offset 8))
    (declare (type fixnum offset)
	     (type card16 major))    
    (with-buffer-request-and-reply
        (display (vidmode-opcode display) nil :sizes (8 16 32))
      ((data +get-mode-line+)
       (card16 (screen-position screen display))
       (card16 0))
     (let ((mode-info 
	    (make-mode-info
	        :dotclock (card32-get offset)
		:hdisplay (card16-get (incf offset 4))
		:hsyncstart (card16-get (incf offset 2))
		:hsyncend (card16-get (incf offset 2))
		:htotal (card16-get (incf offset 2))
		:hskew (if (< major 2) 0 (card16-get (incf offset 2)))
		:vdisplay (card16-get (incf offset 2))
		:vsyncstart (card16-get (incf offset 2))
		:vsyncend (card16-get (incf offset 2))
		:vtotal (card16-get (incf offset 2))
		:flags (card32-get (incf offset (if (< major 2) 2 4)))))
	   (size (card32-get (incf offset (if (< major 2) 4 16)))))
       (declare (type card32 size))
       (incf offset 4)
       (setf (mode-info-privsize mode-info) size
	     (mode-info-private mode-info)
	     (sequence-get :format card32 :index offset
			   :length size :result-type 'list))
       mode-info))))

(defun xfree86-vidmode-get-all-mode-lines (dpy screen)
  "Returns a list containing all video modes (as mode-info structure). 
The first element of the list corresponds to the current video mode."
  (declare (type display dpy)
	   (type screen screen))
  (multiple-value-bind (major minor) (xfree86-vidmode-query-version dpy)
    (declare (type card16 major minor))
    (with-buffer-request-and-reply 
        (dpy (vidmode-opcode dpy) nil :sizes (8 16 32))
      ((data +get-all-mode-lines+)
       (card16 (screen-position screen dpy)))
     (values 
      ;; Note: There was a bug in the protocol implementation in versions
      ;; 0.x with x < 8 (the .private field wasn't being passed over the wire).
      ;; Check the server's version, and accept the old format if appropriate.
      (loop with bug-p = (and (= major 0) (< minor 8))
	    with offset of-type fixnum = 32
            for i of-type card32 from 0 below (or (card32-get 8) 0)
	    collect
	     (let ((mode-info
		    (make-mode-info
		       :dotclock (card32-get offset)
		       :hdisplay (card16-get (incf offset 4))
		       :hsyncstart (card16-get (incf offset 2))
		       :hsyncend (card16-get (incf offset 2))
		       :htotal (card16-get (incf offset 2))
		       :hskew (if (< major 2) 0 (card32-get (incf offset 2)))
		       :vdisplay (card16-get (incf offset 4))
		       :vsyncstart (card16-get (incf offset 2))
		       :vsyncend (card16-get (incf offset 2))
		       :vtotal (card16-get (incf offset 2))
		       :flags (card32-get (incf offset (if (< major 2) 2 6)))))
		   (size (card32-get (incf offset (if (< major 2) 4 16)))))
		(declare (type card32 size))
		(incf offset 4)
		(when bug-p 
		  (setf size 0))
		(setf (mode-info-privsize mode-info) size
		      (mode-info-private mode-info)
		      (sequence-get :format card32 :index offset
				    :length size :result-type 'list))
		(incf offset (* 4 size))
		mode-info))))))

(defun xfree86-vidmode-add-mode-line (dpy scr new &key (after (make-mode-info)))
  (declare (type display dpy)
	   (type screen scr))
  (let* ((private (mode-info-private new))
	 (privsize (mode-info-privsize new))
	 (major (xfree86-vidmode-query-version dpy))
	 (i (if (< major 2) 14 22))
	 (v (make-array (- (+ (* 2 i) (* 2 privsize)) 2) :initial-element 0)))
    (declare (type card32 privsize)
	     (type fixnum i)
	     (type card16 major)
	     (type simple-vector v))
    (mode-info->v-card16 new major :encode-private nil :data v)
    (mode-info->v-card16 after major :encode-private nil :data v :index i)
    (setf i (- (* 2 i) 2))
    ;; strore private info (sequence card32) according clx bytes order.
    (loop for card of-type card32 in private
	  do (multiple-value-bind (w1 w2) (__card32->card16__ card)
	       (setf (svref v (incf i)) w1
		     (svref v (incf i)) w2)))
    
    (with-buffer-request (dpy (vidmode-opcode dpy))
      (data +add-mode-line+)
      (card32 (screen-position scr dpy))
      ((sequence :format card16) v))))

(defun xfree86-vidmode-delete-mode-line (dpy scr mode-info)
  "Delete mode argument. The specified mode must match an existing mode. 
To be considered a match, all of the fields of the given mode-info 
structure must match, except the privsize and private fields. 
If the mode to be deleted is the current mode, a mode switch to the next 
mode will occur first. The last remaining mode can not be deleted."
  (declare (type display dpy)
	   (type screen scr))
  (let* ((major (xfree86-vidmode-query-version dpy))
	 (v (mode-info->v-card16 mode-info major)))
    (declare (type card16 major)
	     (type simple-vector v))
    (with-buffer-request (dpy (vidmode-opcode dpy))
      (data +delete-mode-line+)
      (card32 (screen-position scr dpy))
      ((sequence :format card16) v))))

(defconstant +mode-status+
  '#(:MODE_BAD             ; unspecified reason 
     :MODE_ERROR           ; error condition 
     :MODE_OK              ; Mode OK 
     :MODE_HSYNC           ; hsync out of range 
     :MODE_VSYNC           ; vsync out of range 
     :MODE_H_ILLEGAL       ; mode has illegal horizontal timings 
     :MODE_V_ILLEGAL       ; mode has illegal horizontal timings 
     :MODE_BAD_WIDTH       ; requires an unsupported linepitch 
     :MODE_NO_MODE         ; no mode with a maching name 
     :MODE_NO_INTERLACE    ; interlaced mode not supported 
     :MODE_NO_DBLESCAN     ; doublescan mode not supported 
     :MODE_NO_VSCAN        ; multiscan mode not supported 
     :MODE_MEM             ; insufficient video memory 
     :MODE_VIRTUAL_X       ; mode width too large for specified virtual size 
     :MODE_VIRTUAL_Y       ; mode height too large for specified virtual size 
     :MODE_MEM_VIRT        ; insufficient video memory given virtual size 
     :MODE_NOCLOCK         ; no fixed clock available 
     :MODE_CLOCK_HIGH      ; clock required is too high 
     :MODE_CLOCK_LOW       ; clock required is too low 
     :MODE_CLOCK_RANGE     ; clock/mode isn't in a ClockRange 
     :MODE_BAD_HVALUE      ; horizontal timing was out of range 
     :MODE_BAD_VVALUE      ; vertical timing was out of range 
     :MODE_BAD_VSCAN       ; VScan value out of range 
     :MODE_HSYNC_NARROW    ; horizontal sync too narrow 
     :MODE_HSYNC_WIDE      ; horizontal sync too wide 
     :MODE_HBLANK_NARROW   ; horizontal blanking too narrow 
     :MODE_HBLANK_WIDE     ; horizontal blanking too wide 
     :MODE_VSYNC_NARROW    ; vertical sync too narrow 
     :MODE_VSYNC_WIDE      ; vertical sync too wide 
     :MODE_VBLANK_NARROW   ; vertical blanking too narrow 
     :MODE_VBLANK_WIDE     ; vertical blanking too wide 
     :MODE_PANEL           ; exceeds panel dimensions 
     :MODE_INTERLACE_WIDTH ; width too large for interlaced mode 
     :MODE_ONE_WIDTH       ; only one width is supported 
     :MODE_ONE_HEIGHT      ; only one height is supported 
     :MODE_ONE_SIZE        ; only one resolution is supported 
     ))

(defun decode-status-mode (status)
  (declare (type int32 status))
  (svref +mode-status+ (+ status 2)))

(defun xfree86-vidmode-validate-mode-line (dpy scr mode-info)
  "Checked the validity of a mode-info argument. If the specified mode can be 
used by the server (i.e. meets all the constraints placed upon a mode by the 
combination of the server, card, and monitor) the function returns :mode_ok
otherwise it returns a keyword indicating  the  reason why the mode is 
invalid."
  (declare (type display dpy)
	   (type screen scr))
  (let* ((major (xfree86-vidmode-query-version dpy))
	 (v (mode-info->v-card16 mode-info major)))
    (declare (type card16 major)
	     (type simple-vector v))
    (with-buffer-request-and-reply
        (dpy (vidmode-opcode dpy) nil :sizes (8 16 32))
      ((data +validate-mode-line+)
       (card32 (screen-position scr dpy))
       ((sequence :format card16) v))
     (let ((status (integer-get 8)))
       (declare (type int32 status))
       (when status (decode-status-mode status))))))

(defun xfree86-vidmode-get-gamma (display screen)
  (declare (type display display)
	   (type screen screen))
  (with-buffer-request-and-reply 
      (display (vidmode-opcode display) nil :sizes (8 16 32))
    ((data +get-gamma+)
     (card16 (screen-position screen display))
     (card16 0)
     (card32 0) (card32 0)
     (card32 0) (card32 0)
     (card32 0) (card32 0))
   (values 
    (/ (the card32 (or (card32-get 8) 0)) 10000.0)
    (/ (the card32 (or (card32-get 12) 0)) 10000.0)
    (/ (the card32 (or (card32-get 16) 0)) 10000.0))))

(defun xfree86-vidmode-set-gamma (dpy scr &key (red 1.0) (green 1.0) (blue 1.0))
  (declare (type display dpy)
	   (type screen scr)
	   (type (single-float 0.100f0 10.000f0) red green blue))
  (with-buffer-request (dpy (vidmode-opcode dpy))
    (data +set-gamma+)
    (card16 (screen-position scr dpy))
    (card16 0)
    (card32 (truncate (* red 10000)))
    (card32 (truncate (* green 10000)))
    (card32 (truncate (* blue 10000)))
    (card32 0) 
    (card32 0)
    (card32 0)))

(defun xfree86-vidmode-get-gamma-ramp (dpy scr size)
  (declare (type display dpy)
	   (type screen scr)
	   (type card16 size))
  (with-buffer-request-and-reply (dpy (vidmode-opcode dpy) nil :sizes (8 16 32))
    ((data +get-gamma-ramp+)
     (card16 (screen-position scr dpy))
     (card16 size))
   (let ((rep-size (* (the card16 (or (card16-get 8) 0)) 2)))
     (declare (type fixnum rep-size))
     (unless (zerop rep-size)
       (let* ((off1 (+ 32 rep-size (* 2 (mod rep-size 2))))
	      (off2 (+ off1 rep-size (* 2 (mod rep-size 2)))))
	 (declare (type fixnum off1 off2))
	 (values
	  (sequence-get :format card16 :length (card16-get 8)
			:index 32 :result-type 'list)
	  (sequence-get :format card16 :length (card16-get 8)
			:index off1 :result-type 'list)
	  (sequence-get :format card16 :length (card16-get 8)
			:index off2 :result-type 'list)))))))

(defun xfree86-vidmode-set-gamma-ramp (dpy scr size &key red green blue)
  (declare (type (or null simple-vector) red green blue)
	   (type card16 size)
	   (type display dpy)
	   (type screen scr))
  (with-buffer-request (dpy (vidmode-opcode dpy))
    (data +set-gamma-ramp+)
    (card16 (screen-position scr dpy))
    (card16 size)
    ((sequence :format card16) 
     (if (zerop (mod size 2))
	 (concatenate 'vector red green blue)
         (concatenate 'vector red '#(0) green '#(0) blue '#(0))))))

(defun xfree86-vidmode-get-gamma-ramp-size (dpy screen)
  (declare (type display dpy)
	   (type screen screen))
  (with-buffer-request-and-reply 
      (dpy (vidmode-opcode dpy) nil :sizes (8 16 32))
    ((data +get-gamma-ramp-size+)
     (card16 (screen-position screen dpy))
     (card16 0))
    (card16-get 8)))

(defun xfree86-vidmode-lock-mode-switch (display screen lock-p)
  "Allow or disallow mode switching whether the request to switch
modes comes from a call to the mode switching functions or from one 
of the mode switch key sequences (e.g. Ctrl-Alt-+ Ctrl-Alt--)."
  (declare (type display display)
	   (type screen screen)
	   (type boolean lock-p))
  (with-buffer-request (display (vidmode-opcode display))
    (data +lock-mode-switch+)
    (card16 (screen-position screen display))
    (card16 (if lock-p 1 0))))

(defun xfree86-vidmode-switch-to-mode (display screen mode-info)
  "Switch directly to the specified mode. The specified mode must match 
an existing mode. Matching is as specified in the description of the 
xf86-vidmode-delete-mode-line function."
  (declare (type display display)
	   (type screen screen))
  (multiple-value-bind (major minor) (xfree86-vidmode-query-version display)
    (declare (type card16 major minor))
    ;; Note: There was a bug in the protocol implementation in versions
    ;; 0.x with x < 8 (the .private field wasn't being passed over the wire).
    ;; Check the server's version, and accept the old format if appropriate.
    (let ((bug-p (and (= major 0) (< minor 8)))
	  (privsize (mode-info-privsize mode-info)))
      (declare (type boolean bug-p))
      (and bug-p (setf (mode-info-privsize mode-info) 0))
      (let ((v (mode-info->v-card16 mode-info major :encode-private bug-p)))
	(declare (type simple-vector v))
	(and bug-p (setf (mode-info-privsize mode-info) privsize))
	(with-buffer-request (display (vidmode-opcode display))
	  (data +switch-to-mode+)
	  (card32 (screen-position screen display))
	  ((sequence :format card16) v))))))

(defun xfree86-vidmode-switch-mode (display screen zoom)
  "Change the video mode to next (or previous) video mode, depending 
of zoom sign. If positive, switch to next mode, else switch to prev mode."
  (declare (type display display)
	   (type screen screen)
	   (type card16 zoom))
  (with-buffer-request (display (vidmode-opcode display))
    (data +switch-mode+)
    (card16 (screen-position screen display))
    (card16 zoom)))

(defun xfree86-vidmode-select-next-mode (display screen)
  "Change the video mode to next video mode"
  (declare (type display display)
	   (type screen screen))
  (with-buffer-request (display (vidmode-opcode display))
    (data +switch-mode+)
    (card16 (screen-position screen display))
    (card16 1)))

(defun xfree86-vidmode-select-prev-mode (display screen)
  "Change the video mode to previous video mode"
  (declare (type display display)
	   (type screen screen))
  (with-buffer-request (display (vidmode-opcode display))
    (data +switch-mode+)
    (card16 (screen-position screen display))
    (card16 #xFFFF)))

(defun xfree86-vidmode-get-monitor (dpy screen)
  "Information known to the server about the monitor is returned. 
Multiple value return:
 hsync (list of hi, low, ...)
 vsync (list of hi, low, ...)
 vendor name
 model name 

The hi and low values will be equal if a discreate value was given 
in the XF86Config file."
  (declare (type display dpy)
	   (type screen screen))
  (with-buffer-request-and-reply
      (dpy (vidmode-opcode dpy) nil :sizes (8 16 32))
    ((data +get-monitor+)
     (card16 (screen-position screen dpy))
     (card16 0))
   (let* ((vendor-name-length (card8-get 8))
	  (model-name-length (card8-get 9))
	  (pad (- 4 (mod vendor-name-length 4)))
	  (nhsync (card8-get 10))
	  (nvsync (card8-get 11))
	  (vindex (+ 32 (* 4 (+ nhsync nvsync))))
	  (mindex (+ vindex vendor-name-length pad))
	  (hsync (sequence-get :length nhsync :index 32 :result-type 'list))
	  (vsync (sequence-get :length nvsync :index (+ 32 (* nhsync 4))
			       :result-type 'list)))
     (declare (type card8 nhsync nvsync vendor-name-length model-name-length)
	      (type fixnum pad vindex mindex))
     (values 
      (loop for i of-type card32 in hsync
	    collect (/ (ldb (byte 16 0) i) 100.)
	    collect (/ (ldb (byte 32 16) i) 100.))
      (loop for i of-type card32 in vsync
	    collect (/ (ldb (byte 16 0) i) 100.)
	    collect (/ (ldb (byte 32 16) i) 100.))
      (string-get vendor-name-length vindex)
      (string-get model-name-length mindex)))))

(defun xfree86-vidmode-get-viewport (dpy screen)
  "Query the location of the upper left corner of the viewport into 
the virtual screen. The upper left coordinates will be returned as 
a multiple value."
  (declare (type display dpy)
	   (type screen screen))
  (multiple-value-bind (major minor) (xfree86-vidmode-query-version dpy)
    (declare (type card16 major minor))
    ;; Note: There was a bug in the protocol implementation in versions
    ;; 0.x with x < 8 (no reply was sent, so the client would hang)
    ;; Check the server's version, and don't wait for a reply with older
    ;; versions.
    (when (and (= major 0) (< minor 8))
      (format cl:*error-output* 
	      "running an old version ~a ~a~%"
	      major minor)
      (return-from xfree86-vidmode-get-viewport nil))
    (with-buffer-request-and-reply 
	(dpy (vidmode-opcode dpy) nil :sizes (8 16 32))
      ((data +get-viewport+)
       (card16 (screen-position screen dpy))
       (card16 0))
     (values
      (card32-get 8)
      (card32-get 12)))))
       
(defun xfree86-vidmode-set-viewport (dpy screen &key (x 0) (y 0))
  "Set upper left corner of the viewport into the virtual screen to the 
x and y keyword parameters value (zero will be theire default value)."
  (declare (type display dpy)
	   (type screen screen)
	   (type card32 x y))
  (with-buffer-request (dpy (vidmode-opcode dpy))
    (data +set-viewport+)
    (card16 (screen-position screen dpy))
    (card16 0)
    (card32 x)
    (card32 y)))

(defun xfree86-vidmode-get-dotclocks (dpy screen)
  "Returns as a multiple value return the server dotclock informations:
 flags
 maxclocks
 clock list"
  (declare (type display dpy)
	   (type screen screen))
  (with-buffer-request-and-reply 
      (dpy (vidmode-opcode dpy) nil :sizes (8 16 32))
    ((data +get-dot-clocks+)
     (card16 (screen-position screen dpy))
     (card16 0))
   (values
    (card32-get 8)  ; flags
    (card32-get 16) ; max clocks
    (sequence-get :length (card32-get 12) :format card32
		  :index 32 :result-type 'list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                       ;;;;
;;;;                       private utility routines                        ;;;;
;;;;                                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mode-info->v-card16
    (mode-info major &key (encode-private t) (index 0) data)
  (declare (type integer index)
	   (type card16 major)
	   (type boolean encode-private)
	   (type (or null simple-vector) data))
  (let ((dotclock (mode-info-dotclock mode-info))
	(hdisplay (mode-info-hdisplay mode-info))
	(hsyncstart (mode-info-hsyncstart mode-info))
	(hsyncend (mode-info-hsyncend mode-info))
	(htotal (mode-info-htotal mode-info))
	(hskew (mode-info-hskew mode-info))
	(vdisplay (mode-info-vdisplay mode-info))
	(vsyncstart (mode-info-vsyncstart mode-info))
	(vsyncend (mode-info-vsyncend mode-info))
	(vtotal (mode-info-vtotal mode-info))
	(flags (mode-info-flags mode-info))
	(privsize (mode-info-privsize mode-info))
	(private (mode-info-private mode-info)))
    (declare (type card16 hdisplay hsyncstart hsyncend htotal hskew)
	     (type card16 vdisplay vsyncstart vsyncend vtotal)
	     (type card32 dotclock flags privsize)
	     (type (or null sequence) private))
    (let* ((size (+ (if (< major 2) 14 22) (* privsize 2)))
	   (v (or data (make-array size :initial-element 0))))      
      (declare (type fixnum size)
	       (type simple-vector v))
      ;; store dotclock (card32) according clx bytes order.
      (multiple-value-bind (w1 w2) (__card32->card16__ dotclock)
	(setf (svref v index) w1
	      (svref v (incf index)) w2))
      (setf (svref v (incf index)) hdisplay
	    (svref v (incf index)) hsyncstart
	    (svref v (incf index)) hsyncend
	    (svref v (incf index)) htotal)
      (unless (< major 2)
	(setf (svref v (incf index)) hskew))
      (setf (svref v (incf index)) vdisplay
	    (svref v (incf index)) vsyncstart
	    (svref v (incf index)) vsyncend
	    (svref v (incf index)) vtotal)
      (unless (< major 2)
	(incf index))
      ;; strore flags (card32) according clx bytes order.
      (multiple-value-bind (w1 w2) (__card32->card16__ flags)
	(setf (svref v (incf index)) w1
	      (svref v (incf index)) w2))
      ;; strore privsize (card32) according clx bytes order.
      (multiple-value-bind (w1 w2) (__card32->card16__ privsize)
	(setf (svref v (incf index)) w1
	      (svref v (incf index)) w2))
      ;; reserverd byte32 1 2 3
      (unless (< major 2) (incf index 6))
      ;; strore private info (sequence card32) according clx bytes order.
      (when encode-private
	(loop for i of-type int32 in private
	      do (multiple-value-bind (w1 w2) (__card32->card16__ i)
		   (setf (svref v (incf index)) w1
			 (svref v (incf index)) w2))))
      v)))

(declaim (inline __card32->card16__))
(defun __card32->card16__ (i)
  (declare (type card32 i))
  #+clx-little-endian
  (progn (values (ldb (byte 16 0) i) (ldb (byte 32 16) i)))
  #-clx-little-endian
  (progn (values (ldb (byte 32 16) i) (ldb (byte 16 0) i))))
