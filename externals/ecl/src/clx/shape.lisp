;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: X11 Shape extension
;;;   Created: 1999-05-14 11:31
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999 by Gilbert Baumann

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

;;; Use xc/doc/hardcopy/Xext/shape.PS.gz obtainable from e.g.
;;  ftp://ftp.xfree86.org/pub/XFree86/current/untarred/xc/hardcopy/Xext/shape.PS.gz

(in-package :xlib)

(export '(shape-query-version
          shape-rectangles
          shape-mask
          shape-combine
          shape-offset
          shape-query-extents
          shape-select-input
          shape-input-selected-p
          shape-get-rectangles)
        :xlib)

(define-extension "SHAPE"
    :events (:shape-notify))

(declare-event :shape-notify
               ((data (member8 :bounding :clip)) kind) ;shape kind
               (card16 sequence)
               (window (window event-window)) ;affected window
               (int16 x)                ;extents
               (int16 y)
               (card16 width)
               (card16 height)
               ((or null card32) time)  ;timestamp
               (boolean shaped-p))

(defun encode-shape-kind (kind)
  (ecase kind
    (:bounding 0)
    (:clip 1)))

(defun encode-shape-operation (operation)
  (ecase operation
    (:set 0)
    (:union 1)
    (:interset 2)
    (:subtract 3)
    (:invert 4)))

(defun encode-shape-rectangle-ordering (ordering)
  (ecase ordering
    ((:unsorted :un-sorted nil) 0)
    ((:y-sorted) 1)
    ((:yx-sorted) 2)
    ((:yx-banded) 3)))

(defun shape-query-version (display)
  (with-buffer-request-and-reply (display (extension-opcode display "SHAPE")
                                          nil :sizes 16)
    ((data 0))
    (values
     (card16-get 8)
     (card16-get 10))))

(defun shape-rectangles (window rectangles
                                &key (kind :bounding)
                                (x-offset 0)
                                (y-offset 0)
                                (operation :set)
                                (ordering :unsorted))
  (let* ((display (xlib:window-display window)))
    (with-buffer-request (display (extension-opcode display "SHAPE"))
      (data 1)
      (card8 (encode-shape-operation operation))
      (card8 (encode-shape-kind kind))
      (card8 (encode-shape-rectangle-ordering ordering))
      (card8 0)                         ;unused
      (window window)
      (int16 x-offset)
      (int16 y-offset)
      ((sequence :format int16) rectangles))))

(defun shape-mask (window pixmap
                          &key (kind :bounding)
                          (x-offset 0)
                          (y-offset 0)
                          (operation :set))
  (let* ((display (xlib:window-display window)))
    (with-buffer-request (display (extension-opcode display "SHAPE"))
      (data 2)
      (card8 (encode-shape-operation operation))
      (card8 (encode-shape-kind kind))
      (card16 0)                        ;unused
      (window window)
      (int16 x-offset)
      (int16 y-offset)
      ((or pixmap (member :none)) pixmap))))

(defun shape-combine (window source-window
                             &key (kind :bounding)
                             (source-kind :bounding)
                             (x-offset 0)
                             (y-offset 0)
                             (operation :set))
  (let* ((display (xlib:window-display window)))
    (with-buffer-request (display (extension-opcode display "SHAPE"))
      (data 3)
      (card8 (encode-shape-operation operation))
      (card8 (encode-shape-kind kind))
      (card8 (encode-shape-kind source-kind))
      (card8 0)                         ;unused
      (window window)
      (int16 x-offset)
      (int16 y-offset)
      (window source-window))))

(defun shape-offset (window &key (kind :bounding) (x-offset 0) (y-offset 0))
  (let* ((display (xlib:window-display window)))
    (with-buffer-request (display (extension-opcode display "SHAPE"))
      (data 4)
      (card8 (encode-shape-kind kind))
      (card8 0) (card8 0) (card8 0)     ;unused
      (window window)
      (int16 x-offset)
      (int16 y-offset))))

(defun shape-query-extents (window)
  (let* ((display (xlib:window-display window)))
    (with-buffer-request-and-reply (display (extension-opcode display "SHAPE")
                                            nil :sizes (8 16 32))
      ((data 5)
       (window window))
      (values
       (boolean-get 8)                  ;bounding shaped
       (boolean-get 9)                  ;clip shaped
       (int16-get 12)                   ;bounding shape extents x
       (int16-get 14)                   ;bounding shape extents y
       (card16-get 16)                  ;bounding shape extents width
       (card16-get 18)                  ;bounding shape extents height
       (int16-get 20)                   ;clip shape extents x
       (int16-get 22)                   ;clip shape extents y
       (card16-get 24)                  ;clip shape extents width
       (card16-get 26)))))              ;clip shape extents height

(defun shape-select-input (window selected-p)
  (let* ((display (window-display window)))
    (with-buffer-request (display (extension-opcode display "SHAPE"))
      (data 6)
      (window window)
      (boolean selected-p)) ))

(defun shape-input-selected-p (window)
  (let* ((display (window-display window)))
    (with-buffer-request-and-reply (display (extension-opcode display "SHAPE")
                                            nil :sizes (8))
      ((data 7)                         ;also wrong in documentation
       (window window))
      (boolean-get 1))))

(defun shape-get-rectangles (window &optional (kind :bounding)
                                    (result-type 'list))
  (let* ((display (window-display window)))
    (with-buffer-request-and-reply (display (extension-opcode display "SHAPE")
                                            nil :sizes (8 16 32))
      ((data 8)                         ;this was wrong in the specification
       (window window)
       (card8 (ecase kind
                (:bounding 0)
                (:clip 1))))
      (values
       (sequence-get :length (print (* 4 (card32-get 8)))
                     :result-type result-type
                     :format int16
                     :index +replysize+)
       (ecase (card8-get 1)
         (0 :unsorted)
         (1 :y-sorted)
         (2 :yx-sorted)
         (3 :yx-banded) )))))
