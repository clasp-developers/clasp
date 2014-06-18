;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: X11 MIT Screensaver extension
;;;   Created: 2005-08-28 01:41
;;;    Author: Istvan Marko <mi-clx@kismala.com>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005 by Istvan Marko

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

;;; Description:
;;;
;;; This is a partial interface to the MIT-SCREEN-SAVER
;;; extension. Only the ScreenSaverQueryVersion and
;;; ScreenSaverQueryInfo requests are implemented because I couldn't
;;; think of a use for the rest. In fact, the only use I see for this
;;; extension is screen-saver-get-idle which provides and easy way to
;;; find out how long has it been since the last keyboard or mouse
;;; activity.

;;; A description of this extension can be found at
;;; doc/hardcopy/saver/saver.PS.gz in the X11 distribution.

(in-package :xlib)

(export '(screen-saver-query-version
	  screen-saver-query-info
	  screen-saver-get-idle)
        :xlib)

(define-extension "MIT-SCREEN-SAVER")

(defun screen-saver-query-version (display)
  (with-buffer-request-and-reply (display (extension-opcode display "MIT-SCREEN-SAVER")
                                          nil)
    ((data 0)
     (card8 1) ;client major version
     (card8 0) ;client minor version
     (card16 0)) ; unused
    (values
     (card16-get 8) ; server major version
     (card16-get 10)))) ; server minor version

(defun screen-saver-query-info (display drawable)
  (with-buffer-request-and-reply (display (extension-opcode display "MIT-SCREEN-SAVER")
                                          nil)
    ((data 1)
     (drawable drawable))
    (values
     (card8-get 1) ; state: off, on, disabled
     (window-get 8) ; screen saver window if active
     (card32-get 12) ; tilorsince msecs. how soon before the screen saver kicks in or how long has it been active
     (card32-get 16) ; idle msecs
     (card8-get 24)))) ; kind: Blanked, Internal, External

(defun screen-saver-get-idle (display drawable)
  "How long has it been since the last keyboard or mouse input"
   (multiple-value-bind (state window tilorsince idle kind) (screen-saver-query-info display drawable)
     (declare (ignore state window kind))
     (values idle tilorsince)))
