;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: The X Render Extension
;;;   Created: 2002-08-03
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;       $Id: xrender.lisp,v 1.5 2004/12/06 11:48:57 csr21 Exp $
;;; ---------------------------------------------------------------------------
;;;
;;; (c) copyright 2002, 2003 by Gilbert Baumann
;;; (c) copyright 2002 by Christian Sunesson
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

;;; NOTE: we need to watch maximum request sizes and somehow work
;;; around them. Sometimes e.g. in AddGlyphs this is not possible,
;;; which is a design failure.

;;; TODO

;; - some request are still to be implemented at all.
;;   + Can they not wait? Xrender seems to be in flux as the specification
;;     isn't even conforming to the acctual protocol. However backwards
;;     wierd that sound. --noss

;; - we need to invent something for the color values of e.g. 
;;   fill-rectangles; I would prefer some generic functions, so that
;;   we later can map CLIM design directly to colors.

;; - we want some conviencene function to turn graphics contexts into
;;   render pictures. --GB 2002-08-21

;; - also: uniform-alpha-picture display alpha-value
;;         uniform-color-picture display red green blue
;;   --GB 2002-08-21

;; - maybe we should aim for a higher level interface to
;;   color-trapzoids and color-triangles and offer a low level [raw]
;;   interface also for high performance apps?

;; - Write tests.

;;;; API issues

;; - On one hand we want convenience functions like RENDER-TRIANGLE or
;;   WITH-UNIFORM-COLOR-PICTURE. On the other hand if you are up to
;;   write a full rasterization library you obviously want high
;;   performance entry points as RENDER-TRIANGLES-1.

;; - We want to extend XLIB:COLOR into something with alpha channel.
;;   How to name it?

;; - WITH-UNIFORM-COLOR-PICTURE (var picture r g b &optional alpha) &body body
;;
;;   Example:
;;   (WITH-UNIFORM-COLOR-PICTURE (color dest 1.0 1.0 0.0)
;;     (RENDER-TRIANGLE dest color ...))

;; - Pose the filter and the transform slots of a picture.

;; - Also introduce a PICTURE-DEFAULT-MASK-FORMAT?

;; - COPY-PICTURE?

;; - WITH-PICTURE-OPTIONS ?
;;
;;   (WITH-PICTURE-OPTIONS (pic :repeat :on) ...)

;; - WITH-PICTURE ?
;;
;;   (WITH-PICTURE (picture drawable ...) ...)

;;

(in-package :xlib)

;; Beginning to collect the external interface for documentation.
(export '(render-create-picture
	  render-free-picture
	  
	  render-create-glyph-set
	  render-reference-glyph-set
	  render-free-glyph-set
	  
	  render-add-glyph
	  render-add-glyph-from-picture
	  render-free-glyph
          render-fill-rectangle

	  picture-format-display 
	  picture-format-id
	  picture-format-type
	  picture-format-depth
	  picture-format-red-byte
	  picture-format-green-byte
	  picture-format-blue-byte
	  picture-format-alpha-byte
	  picture-format-colormap

          ;; picture object
          picture-repeat
          picture-alpha-map
          picture-alpha-x-origin
          picture-alpha-y-origin
          picture-clip-x-origin
          picture-clip-y-origin
          picture-clip-mask
          picture-graphics-exposures
          picture-subwindow-mode
          picture-poly-edge
          picture-poly-mode
          picture-dither
          picture-component-alpha
          picture-drawable

          find-matching-picture-formats
          find-window-picture-format
          render-free-picture
          render-free-glyph-set
          render-query-version
          ;; render-query-picture-formats
          render-fill-rectangle
          render-composite
          render-create-glyph-set
          render-reference-glyph-set
          render-composite-glyphs
          render-add-glyph
          render-add-glyph-from-picture
          render-free-glyphs))

(pushnew :clx-ext-render *features*)

(define-extension "RENDER")

;;;; Request constants

;; Note: Although version numbers are given render.h where the request
;; numbers are defined, render-query-version returns 0.0 all displays
;; i tested. --GB 2004-07-21

(defconstant +X-RenderQueryVersion+ 0)                  ;done
(defconstant +X-RenderQueryPictFormats+ 1)
(defconstant +X-RenderQueryPictIndexValues+ 2)          ;0.7
(defconstant +X-RenderQueryDithers+ 3)
(defconstant +X-RenderCreatePicture+ 4)                 ;done
(defconstant +X-RenderChangePicture+ 5)                 ;done
(defconstant +X-RenderSetPictureClipRectangles+ 6)      ;done
(defconstant +X-RenderFreePicture+ 7)                   ;done
(defconstant +X-RenderComposite+ 8)                     ;we need better arglist
(defconstant +X-RenderScale+ 9)
(defconstant +X-RenderTrapezoids+ 10)                   ;low-level done
(defconstant +X-RenderTriangles+ 11)                    ;low-level done
(defconstant +X-RenderTriStrip+ 12)
(defconstant +X-RenderTriFan+ 13)
(defconstant +X-RenderColorTrapezoids+ 14)              ;nyi in X server, not mentioned in renderproto.h
(defconstant +X-RenderColorTriangles+ 15)               ;nyi in X server, not mentioned in renderproto.h
(defconstant +X-RenderTransform+ 16)                    ;commented out in render.h
(defconstant +X-RenderCreateGlyphSet+ 17)               ;done
(defconstant +X-RenderReferenceGlyphSet+ 18)            ;done
(defconstant +X-RenderFreeGlyphSet+ 19)                 ;done
(defconstant +X-RenderAddGlyphs+ 20)                    ;done, untested
(defconstant +X-RenderAddGlyphsFromPicture+ 21)         ;done, untested
(defconstant +X-RenderFreeGlyphs+ 22)                   ;done, untested
(defconstant +X-RenderCompositeGlyphs8+ 23)             ;done
(defconstant +X-RenderCompositeGlyphs16+ 24)            ;done
(defconstant +X-RenderCompositeGlyphs32+ 25)            ;done

;; >= 0.1
  
(defconstant +X-RenderFillRectangles+ 26)               ;single rectangle version done

;; >= 0.5

(defconstant +X-RenderCreateCursor+ 27)

;; >= 0.6

(defconstant +X-RenderSetPictureTransform+ 28)          ;I don't understand what this one should do.
(defconstant +X-RenderQueryFilters+ 29)                 ;seems to be there on server side
                                                        ; some guts of its implementation there.
(defconstant +X-RenderSetPictureFilter+ 30)
(defconstant +X-RenderCreateAnimCursor+ 31)             ;What has render to do with cursors?

;;;;

;; Sanity measures:

;; We do away with the distinction between pict-format and
;; picture-format-info. That is we cache picture-format-infos.

(defstruct render-info
  major-version
  minor-version
  picture-formats)

(defun display-render-info (display)
  (getf (xlib:display-plist display) 'render-info))

(defun (setf display-render-info) (new-value display)
  (setf (getf (xlib:display-plist display) 'render-info)
        new-value))

(defun ensure-render-initialized (display)
  "Ensures that the RENDER extension is initialized. Should be called
by every function, which attempts to generate RENDER requests."
  ;; xxx locking?
  (unless (display-render-info display)
    (let ((q (make-render-info)))
      (multiple-value-bind (maj min) (render-query-version display)
        (setf (render-info-major-version q) maj
              (render-info-minor-version q) min)
        (setf (render-info-picture-formats q)
              (make-hash-table :test #'eql))
        (dolist (pf (render-query-picture-formats display))
          (setf (gethash (picture-format-id pf) (render-info-picture-formats q))
                pf))
        (setf (display-render-info display) q)))))

(defun find-matching-picture-formats
    (display
     &key depth-min depth-max depth
          red-min red-max red
          green-min green-max green
          blue-min blue-max blue
          alpha-min alpha-max alpha
          type
          colormap)
  ;;
  (ensure-render-initialized display)
  (let ((res nil))
    (maphash (lambda (k f)
               (declare (ignore k))
               (when (and
                      (or (null type) (eql (picture-format-type f) type))
                      (or (null colormap)  (eql (picture-format-colormap f) colormap))
                      ;; min
                      (or (null depth-min) (>= (picture-format-depth f) depth-min))
                      (or (null red-min)   (>= (byte-size (picture-format-red-byte f)) red-min))
                      (or (null green-min) (>= (byte-size (picture-format-green-byte f)) green-min))
                      (or (null blue-min)  (>= (byte-size (picture-format-blue-byte f)) blue-min))
                      (or (null alpha-min) (>= (byte-size (picture-format-alpha-byte f)) alpha-min))
                      ;; max
                      (or (null depth-max) (<= (picture-format-depth f) depth-max))
                      (or (null red-max)   (<= (byte-size (picture-format-red-byte f)) red-max))
                      (or (null green-max) (<= (byte-size (picture-format-green-byte f)) green-max))
                      (or (null blue-max)  (<= (byte-size (picture-format-blue-byte f)) blue-max))
                      (or (null alpha-max) (<= (byte-size (picture-format-alpha-byte f)) alpha-max))
                      ;; match
                      (or (null depth)     (= (picture-format-depth f) depth))
                      (or (null red)       (= (byte-size (picture-format-red-byte f)) red))
                      (or (null green)     (= (byte-size (picture-format-green-byte f)) green))
                      (or (null blue)      (= (byte-size (picture-format-blue-byte f)) blue))
                      (or (null alpha)     (= (byte-size (picture-format-alpha-byte f)) alpha)))
                 (pushnew f res)))
             (render-info-picture-formats
              (display-render-info display)))
    res))

(defun find-window-picture-format (window)
  "Find the picture format which matches the given window."
  (let* ((vi (window-visual-info window))
         (display (window-display window)))
    (ensure-render-initialized display)
    (case (visual-info-class vi)
      ((:true-color)
       (maphash (lambda (k f)
                  (declare (ignore k))
                  (when (and (eql (picture-format-type f) :direct)
                             (eql (picture-format-depth f) (drawable-depth window))
                             (eql (dpb -1 (picture-format-red-byte f) 0)
                                  (visual-info-red-mask vi))
                             (eql (dpb -1 (picture-format-green-byte f) 0)
                                  (visual-info-green-mask vi))
                             (eql (dpb -1 (picture-format-blue-byte f) 0)
                                  (visual-info-blue-mask vi))
                             (eql (byte-size (picture-format-alpha-byte f)) 0))
                    (return-from find-window-picture-format f)))
                (render-info-picture-formats
                 (display-render-info display))))
      (t
       ))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-accessor picture (32)
    ((index) index :blip)
    ((index thing) `(resource-id-put ,index (picture-id ,thing))))
  (define-accessor glyph-set (32)
    ((index) index :blip)
    ((index thing) `(resource-id-put ,index (glyph-set-id ,thing)))))

;;; picture format

(defstruct picture-format
  display 
  (id   0 :type (unsigned-byte 29))
  type
  depth
  red-byte
  green-byte
  blue-byte
  alpha-byte
  colormap)

(defmethod print-object ((object picture-format) stream)
  (let ((abbrev
         (with-output-to-string (bag)
           ;; build an abbreviated representation of the format
           (let ((bytes (sort (list (cons "r" (picture-format-red-byte object))
                                    (cons "g" (picture-format-green-byte object))
                                    (cons "b" (picture-format-blue-byte object))
                                    (cons "a" (picture-format-alpha-byte object)))
                              #'>
                              :key #'(lambda (x) (byte-position (cdr x))))))
             (dolist (k bytes)
               (unless (zerop (byte-size (cdr k)))
                 (format bag " ~A~D" (car k) (byte-size (cdr k)))))))))
    (print-unreadable-object (object stream :type t :identity nil)
      (format stream "~D ~S ~S ~S~A"
              (picture-format-id object)
              (picture-format-colormap object)
              (picture-format-depth object)
              (picture-format-type object) abbrev))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-accessor picture-format (32)
    ((index)       `(gethash (read-card32 ,index)
                     (render-info-picture-formats (display-render-info .display.))))
    ((index thing) `(write-card32 ,index (picture-format-id ,thing))))
  (define-accessor render-op (8)
    ((index) `(member8-get ,index
               :clear :src :dst :over :over-reverse :in :in-reverse
               :out :out-reverse :atop :atop-reverse :xor :add :saturate
               '#:undefined-pict-op-Eh '#:undefined-pict-op-Fh
               :disjoint-clear :disjoint-src :disjoint-dst :disjoint-over
               :disjoint-over-reverse :disjoint-in :disjoint-in-reverse
               :disjoint-out :disjoint-out-reverse :disjoint-atop
               :disjoint-atop-reverse :disjoint-xor
               '#:undefined-pict-op-1Ch '#:undefined-pict-op-1Dh
               '#:undefined-pict-op-1Eh '#:undefined-pict-op-1Fh
               :conjoint-clear :conjoint-src :conjoint-dst :conjoint-over
               :conjoint-over-reverse :conjoint-in :conjoint-in-reverse
               :conjoint-out :conjoint-out-reverse :conjoint-atop
               :conjoint-atop-reverse :conjoint-xor))
    ((index thing) `(member8-put ,index ,thing
                     :clear :src :dst :over :over-reverse :in :in-reverse
                     :out :out-reverse :atop :atop-reverse :xor :add :saturate
                     '#:undefined-pict-op-Eh '#:undefined-pict-op-Fh
                     :disjoint-clear :disjoint-src :disjoint-dst :disjoint-over
                     :disjoint-over-reverse :disjoint-in :disjoint-in-reverse
                     :disjoint-out :disjoint-out-reverse :disjoint-atop
                     :disjoint-atop-reverse :disjoint-xor
                     '#:undefined-pict-op-1Ch '#:undefined-pict-op-1Dh
                     '#:undefined-pict-op-1Eh '#:undefined-pict-op-1Fh
                     :conjoint-clear :conjoint-src :conjoint-dst :conjoint-over
                     :conjoint-over-reverse :conjoint-in :conjoint-in-reverse
                     :conjoint-out :conjoint-out-reverse :conjoint-atop
                     :conjoint-atop-reverse :conjoint-xor)))
  (deftype render-op ()
    '(member :clear :src :dst :over :over-reverse :in :in-reverse
      :out :out-reverse :atop :atop-reverse :xor :add :saturate
      :disjoint-clear :disjoint-src :disjoint-dst :disjoint-over
      :disjoint-over-reverse :disjoint-in :disjoint-in-reverse
      :disjoint-out :disjoint-out-reverse :disjoint-atop
      :disjoint-atop-reverse :disjoint-xor
      :conjoint-clear :conjoint-src :conjoint-dst :conjoint-over
      :conjoint-over-reverse :conjoint-in :conjoint-in-reverse
      :conjoint-out :conjoint-out-reverse :conjoint-atop
      :conjoint-atop-reverse :conjoint-xor)))

;; Now these pictures objects are like graphics contexts. I was about
;; to introduce a synchronous mode, realizing that the RENDER protocol
;; provides no provision to actually query a picture object's values. 
;; *sigh*

(def-clx-class (picture (:copier nil))
  (id 0 :type resource-id)
  (display nil :type (or null display))
  (plist nil :type list)                ; Extension hook
  (format)
  (%changed-p)
  (%server-values)
  (%values)
  (%drawable))

(defun picture-drawable (picture)
  (picture-%drawable picture))

;; xx make id, display, format readonly

(defun %render-change-picture-clip-rectangles (picture rectangles)
  "Dont call me, use (SETF PICTURE-CLIP-MASK) instead."
  (declare (optimize (speed 0)))
  (let ((display (picture-display picture)))
    (ensure-render-initialized display)
    (with-buffer-request (display (extension-opcode display "RENDER"))
      (data +X-RenderSetPictureClipRectangles+)
      (picture picture)
      (int16    (picture-clip-x-origin picture))
      (int16    (picture-clip-y-origin picture))
      ((sequence :format int16) rectangles))))

(macrolet ((foo (&rest specs)
             `(progn
               ,@(loop for (type slot default) in specs
                       for index from 0
                       collect
                       `(progn
                         (defun ,(xintern 'picture- slot) (picture)
                           (aref (picture-%values picture) ,index))
                         (defun (setf ,(xintern 'picture- slot)) (new-value picture)
                           (setf (picture-%changed-p picture) t)
                           (setf (aref (picture-%values picture) ,index) new-value))))
    
               (defun synchronise-picture-state (picture)
                 (when (picture-%changed-p picture)
                   (let ((display (picture-display picture)))
                     (ensure-render-initialized display)
                     (with-buffer-request (display (extension-opcode display "RENDER"))
                       (data +X-RenderChangePicture+)
                       (picture picture)
                       (mask
                        ,@(loop for (type slot default) in specs
                                for index from 0
                                collect
                                `(,type (and
                                         ,(cond ((eql slot 'clip-mask)
                                                 `(not (typep (aref (picture-%values picture) ,index)
                                                        'sequence)))
                                                (t
                                                 't))
                                         (not (eq (aref (picture-%values picture) ,index)
                                                  (aref (picture-%server-values picture) ,index)))
                                         (setf (aref (picture-%server-values picture) ,index)
                                          (aref (picture-%values picture) ,index))))))))
                   ,(let ((index (position 'clip-mask specs :key #'second)))
                         `(unless (eql (aref (picture-%values picture) ,index)
                                   (aref (picture-%server-values picture)
                                    ,index))
                           (%render-change-picture-clip-rectangles
                            picture (aref (picture-%values picture) ,index))
                           (setf (aref (picture-%server-values picture) ,index)
                                 (aref (picture-%values picture) ,index))))

                   (setf (picture-%changed-p picture) nil)))

               (defun render-create-picture
                   (drawable
                    &key format
                         (picture (make-picture :display (drawable-display drawable)))
                         ,@(loop for (type slot default-value) in specs
                                 collect (cond ((eql slot 'clip-mask)
                                                `(clip-mask :none))
                                               (t
                                                slot)))
                         )
                 ;; xxx also offer to give a colormap instead of a picture-format
                 ;; values!
                 (let ((display (drawable-display drawable)))
                   (ensure-render-initialized display)
                   (unless format
                     ;; xxx check for drawable being a window
                     (setf format (find-window-picture-format drawable)))
                   (let ((pid (allocate-resource-id display picture 'picture)))
                     (setf (picture-id picture) pid)
                     (with-buffer-request (display (extension-opcode display "RENDER"))
                       (data +X-RenderCreatePicture+)
                       (resource-id pid)
                       (drawable drawable)
                       (picture-format format)
                       (mask
                        ,@(loop for (type slot default) in specs
                                collect
                                (cond ((eql slot 'clip-mask)
                                       (list type `(and
                                                    (not (typep clip-mask 'sequence))
                                                    clip-mask)))
                                      (t
                                       (list type slot)))))))
                   (when (typep clip-mask 'sequence)
                     (%render-change-picture-clip-rectangles picture clip-mask))
                   (setf (picture-format picture) format)
                   (setf (picture-%server-values picture)
                         (vector ,@(loop for (type slot default) in specs
                                         collect
                                         `(or ,slot ,default))))
                   (setf (picture-%values picture) (copy-seq (picture-%server-values picture)))
                   (setf (picture-%drawable picture) drawable)
                   picture))
    
               (defconstant +picture-state-length+
                 ,(length specs)) )))

  (foo ((member :off :on) repeat                                          :off)
       ((or (member :none) picture) alpha-map                             :none)
       (int16 alpha-x-origin                                              0)
       (int16 alpha-y-origin                                              0)
       (int16 clip-x-origin                                               0)
       (int16 clip-y-origin                                               0)
       ;; ### Now that is not correct is it?:
       ((or (member :none) pixmap) clip-mask                              :none)
       ((member :off :on) graphics-exposures                              :on)
       ((member :clip-by-children :include-inferiors) subwindow-mode      :clip-by-children)
       ((member :sharp :smooth) poly-edge                                 :smooth)
       ((member :precise :imprecise) poly-mode                            :precise)
       ((or (member :none) #||xatom||#) dither                            :none)
       ((member :off :on) component-alpha                                 :off)))

(defun render-free-picture (picture)
  (let ((display (picture-display picture)))
    (with-buffer-request (display (extension-opcode display "RENDER"))
      (data +X-RenderFreePicture+)
      (picture  picture))))

(defun render-free-glyph-set (glyph-set)
  (let ((display (glyph-set-display glyph-set)))
    (with-buffer-request (display (extension-opcode display "RENDER"))
      (data +X-RenderFreeGlyphSet+)
      (glyph-set  glyph-set))))

(defun render-query-version (display)
  (with-buffer-request-and-reply (display (extension-opcode display "RENDER") nil)
    ((data +X-RenderQueryVersion+)
     (card32 0)
     (card32 1))
    (values
     (card32-get 8)
     (card32-get 12) )))

(defun render-query-picture-formats (display)
  (with-buffer-request-and-reply (display (extension-opcode display "RENDER") nil)
    ((data +X-RenderQueryPictFormats+))
    (let ((n-picture-formats (card32-get 8))
          (n-screens      (card32-get 12))
          (n-depths       (card32-get 16))
          (n-visuals      (card32-get 20))
          (n-subpixel     (card32-get 24)))
      (declare (ignore n-screens n-depths n-visuals n-subpixel))
      (loop for i below n-picture-formats
            collect
            (let ((off (+ (* 8 4)
                          (* i 28))))   ;size of picture-format-info
              (make-picture-format
               :display display
               :id         (card32-get (+ off 0))
               :type       (member8-get (+ off 4) :indexed :direct)
               :depth      (card8-get   (+ off 5))
               :red-byte   (byte (integer-length (card16-get (+ off 10)))
                                 (card16-get (+ off 8)))
               :green-byte (byte (integer-length (card16-get (+ off 14)))
                                 (card16-get (+ off 12)))
               :blue-byte  (byte (integer-length (card16-get (+ off 18)))
                                 (card16-get (+ off 16)))
               :alpha-byte (byte (integer-length (card16-get (+ off 22)))
                                 (card16-get (+ off 20)))
               :colormap   (let ((cmid (card32-get (+ off 24))))
                             (unless (zerop cmid)
                               (lookup-colormap display cmid)))))))))

(defun render-fill-rectangle (picture op color x1 y1 w h)
  (let ((display (picture-display picture)))
    (ensure-render-initialized display)
    (synchronise-picture-state picture)
    (with-buffer-request (display (extension-opcode display "RENDER"))
      (data +X-RenderFillRectangles+)
      (render-op op)                         ;op
      (card8 0)                        ;pad
      (card16 0)                        ;pad
      (resource-id (picture-id picture))
      (card16 (elt color 0)) (card16 (elt color 1)) (card16 (elt color 2)) (card16 (elt color 3))
      (int16 x1) (int16 y1) (card16 w) (card16 h))))

;; fill rectangles, colors.

(defun render-triangles-1 (picture op source src-x src-y format coord-sequence)
  ;; For performance reasons we do a special typecase on (simple-array
  ;; (unsigned-byte 32) (*)), so that it'll be possible to have high
  ;; performance rasters.
  (macrolet ((guts ()
               '(let ((display (picture-display picture)))
                 (synchronise-picture-state picture)
                 (synchronise-picture-state source)
                 (with-buffer-request (display (extension-opcode display "RENDER"))
                   (data +X-RenderTriangles+)
                   (render-op op)                       ;op
                   (card8 0)                            ;pad
                   (card16 0)                           ;pad
                   (resource-id (picture-id source))
                   (resource-id (picture-id picture))
                   (picture-format format)
                   (int16 src-x)
                   (int16 src-y)
                   ((sequence :format int32) coord-sequence) ))))
    (typecase coord-sequence
      ((simple-array (unsigned-byte 32) (*))
       (locally
           (declare (type (simple-array (unsigned-byte 32) (*)) coord-sequence))
         (guts)))
      (t
       (guts)))))

#||
(defun render-set-picture-transform (picture mxx mxy dx mxy myy dy &optional (mwx 0) (mwy 0) (dw 1))
  ...)
||#

(defun render-set-picture-transform (picture a b c d e f p q r)
  (let ((display (picture-display picture)))
    (ensure-render-initialized display)
    (synchronise-picture-state picture)
    (with-buffer-request (display (extension-opcode display "RENDER"))
      (data +X-RenderSetPictureTransform+)
      #|
      (card8 0) ;; render-op op)                    ;op
      (card8 0)                         ;pad
      (card16 0)                        ;pad
      |#
      (resource-id (picture-id picture))
      
      (card32 a)
      (card32 b)
      (card32 c)
      
      (card32 d)
      (card32 e)
      (card32 f)
      
      (card32 p)
      (card32 q)
      (card32 r))))

(defun render-query-filters (drawable)
  (let ((display (drawable-display drawable)))
    (with-buffer-request-and-reply (display (extension-opcode display "RENDER") nil)
      ((data +X-RenderQueryFilters+)
       (drawable drawable))
      (let* ((len (card32-get 4))
             (n-aliases (card32-get 8))
             (n-filters (card32-get 12))
             (off (+ (* 8 4) (* 4 (ceiling (* 2 n-aliases) 4)))))
        (print (list :aliases
                     (loop for i below n-aliases collect (card16-get (+ (* 8 4) (* i 2))))))
        (print (list :foo len n-aliases n-filters
                     (loop for i below len
                           collect (card8-get (+ off 0 (* 4 i)))
                           collect (card8-get (+ off 1 (* 4 i)))
                           collect (card8-get (+ off 2 (* 4 i)))
                           collect (card8-get (+ off 3 (* 4 i))))))
        (print
         (labels ((grab-string (j)
                    (let ((n (card8-get j)))
                      (incf j)
                      (values
                       (map 'string #'code-char (loop repeat n collect (card8-get j) do (incf j)))
                       j))))
           (loop repeat n-filters collect
                 (multiple-value-bind (s j) (grab-string off)
                   (setf off j)
                   (intern (string-upcase s) :keyword)))))
        #+NIL
        (loop for i below n-picture-formats
              collect
              (let ((off (+ (* 8 4)
                            (* i 28)))) ;size of picture-format-info
                (make-picture-format
                 :display display
                 :id         (card32-get (+ off 0))
                 :type       (member8-get (+ off 4) :indexed :direct)
                 :depth      (card8-get   (+ off 5))
                 :red-byte   (byte (integer-length (card16-get (+ off 10)))
                                   (card16-get (+ off 8)))
                 :green-byte (byte (integer-length (card16-get (+ off 14)))
                                   (card16-get (+ off 12)))
                 :blue-byte  (byte (integer-length (card16-get (+ off 18)))
                                   (card16-get (+ off 16)))
                 :alpha-byte (byte (integer-length (card16-get (+ off 22)))
                                   (card16-get (+ off 20)))
                 :colormap   (let ((cmid (card32-get (+ off 24))))
                               (unless (zerop cmid)
                                 (lookup-colormap display cmid))))))))))

(defun render-set-filter (picture filter)
  (let ((display (picture-display picture)))
    (ensure-render-initialized display)
    (synchronise-picture-state picture)
    (with-buffer-request (display (extension-opcode display "RENDER"))
      (data +X-RenderSetPictureFilter+)
      (resource-id (picture-id picture))
      (card16 (length filter))
      (card16 0)                        ;pad
      ((sequence :format card8) (map 'vector #'char-code filter)))))
  


#||
(defun render-triangle (destination source x1 y1 x2 y2 x3 y3 &key (src-x 0) (src-y 0) (format nil) (op :over))
  (render-triangles-1 destination op source ...)
  )
||#

(defun render-trapezoids-1 (picture op source src-x src-y mask-format coord-sequence)
  ;; coord-sequence is  top bottom
  ;;                    line-1-x1 line-1-y1 line-1-x2 line-1-y2
  ;;                    line-2-x1 line-2-y1 line-2-x2 line-2-y2 ...
  ;;
  (let ((display (picture-display picture)))
    (synchronise-picture-state picture)
    (synchronise-picture-state source)
    (with-buffer-request (display (extension-opcode display "RENDER"))
      (data +X-RenderTrapezoids+)
      (render-op op)                    ;op
      (card8 0)                         ;pad
      (card16 0)                        ;pad
      (resource-id (picture-id source))
      (resource-id (picture-id picture))
      ((or (member :none) picture-format) mask-format)
      (int16 src-x)
      (int16 src-y)
      ((sequence :format int32) coord-sequence) )))

(defun render-composite (op
                         source mask dest
                         src-x src-y mask-x mask-y dst-x dst-y
                         width height)
  (let ((display (picture-display source)))
    (synchronise-picture-state source)
    (when mask (synchronise-picture-state mask))
    (synchronise-picture-state dest)
    (with-buffer-request (display (extension-opcode display "RENDER"))
      (data +X-RenderComposite+)
      (render-op op)                         ;op
      (card8 0)                         ;pad
      (card16 0)                        ;pad
      (resource-id (picture-id source))
      (resource-id (if mask (picture-id mask) 0))
      (resource-id (picture-id dest))
      (int16 src-x)
      (int16 src-y)
      (int16 mask-x)
      (int16 mask-y)
      (int16 dst-x)
      (int16 dst-y)
      (card16 width)
      (card16 height))))

(def-clx-class (glyph-set (:copier nil)
                        )
  (id 0 :type resource-id)
  (display nil :type (or null display))
  (plist nil :type list)                ; Extension hook
  (format))

(defun render-create-glyph-set (format &key glyph-set)
  (let ((display (picture-format-display format)))
    (let* ((glyph-set (or glyph-set (make-glyph-set :display display)))
           (gsid (setf (glyph-set-id glyph-set)
                       (allocate-resource-id display glyph-set 'glyph-set))))
      (declare (ignore gsid))
      (setf (glyph-set-format glyph-set) format)
      (with-buffer-request (display (extension-opcode display "RENDER"))
        (data +X-RenderCreateGlyphSet+)
        (glyph-set glyph-set)
        (picture-format format))
      glyph-set)))

(defun render-reference-glyph-set (existing-glyph-set &key glyph-set)
  (let ((display (glyph-set-display existing-glyph-set)))
    (let* ((glyph-set (or glyph-set (make-glyph-set :display display)))
           (gsid (setf (glyph-set-id glyph-set)
                       (allocate-resource-id display glyph-set 'glyph-set))))
      (declare (ignore gsid))
      (setf (glyph-set-format glyph-set)
            (glyph-set-format existing-glyph-set))
      (with-buffer-request (display (extension-opcode display "RENDER"))
        (data +X-RenderReferenceGlyphSet+)
        (glyph-set glyph-set)
        (glyph-set existing-glyph-set))
      glyph-set)))

(defun render-composite-glyphs-8 (dest glyph-set source dest-x dest-y sequence
                                  &key (op :over)
                                       (alu op) ;for the fun of it
                                       (src-x 0)
                                       (src-y 0)
                                       (mask-format :none)
                                       (start 0)
                                       (end (length sequence)))
  (let ((display (picture-display dest)))
    (ensure-render-initialized display)
    (synchronise-picture-state dest)
    (synchronise-picture-state source)
    (when (stringp sequence)
      ;; lazy me, but then you should not confuse glyphs with
      ;; characters anyway.
      (setf sequence (map 'vector #'char-code sequence)))
    (with-buffer-request (display (extension-opcode display "RENDER"))
      (data +X-RenderCompositeGlyphs8+)
      (render-op alu)
      (card8 0) (card16 0)              ;padding
      (picture source)
      (picture dest)
      ((or (member :none) picture-format) mask-format)
      (glyph-set glyph-set)
      (int16 src-x) (int16 src-y)
      (card8 (- end start)) ;length of glyph elt
      (card8 0) (card16 0) ;padding
      (int16 dest-x) (int16 dest-y)             ;dx, dy
      ((sequence :format card8) sequence))))

(defmacro %render-composite-glyphs
    (opcode type transform display dest glyph-set source dest-x dest-y sequence
     alu src-x src-y mask-format start end)
  (let ((size (ecase type (card8 1) (card16 2) (card32 4)))
	;; FIXME: the last chunk for CARD8 can be 254.
	(chunksize (ecase type (card8 252) (card16 254) (card32 254))))
    `(multiple-value-bind (nchunks leftover)
         (floor (- end start) ,chunksize)
       (let* ((payloadsize (+ (* nchunks (+ 8 (* ,chunksize ,size)))
			      (if (> leftover 0)
				  (+ 8 (* 4 (ceiling (* leftover ,size) 4)))
				  0)))
	      (request-length (+ 7 (/ payloadsize 4))))
	 (declare (integer request-length))
	 (with-buffer-request (,display (extension-opcode ,display "RENDER") :length (* 4 request-length))
	   (data ,opcode)
	   (length request-length)
	   (render-op ,alu)
	   (card8 0) (card16 0)                        ;padding
	   (picture ,source)
	   (picture ,dest)
	   ((or (member :none) picture-format) ,mask-format)
	   (glyph-set ,glyph-set)
	   (int16 ,src-x) (int16 ,src-y)
	   (progn
	     (let ((boffset (+ buffer-boffset 28))
		   (start ,start)
		   (end ,end)
		   (dest-x ,dest-x)
		   (dest-y ,dest-y))
	       (dotimes (i nchunks)
		 (set-buffer-offset boffset)
		 (put-items (0)
		   (card8 ,chunksize)
		   (card8 0)
		   (card16 0)
		   (int16 dest-x)
		   (int16 dest-y)
		   ((sequence :start start :end (+ start ,chunksize) :format ,type :transform ,transform :appending t) ,sequence))
		 (setq dest-x 0 dest-y 0)
		 (incf boffset (+ 8 (* ,chunksize ,size)))
		 (incf start ,chunksize))
	       (when (> leftover 0)
		 (set-buffer-offset boffset)
		 (put-items (0)
		   (card8 leftover)
		   (card8 0)
		   (card16 0)
		   (int16 dest-x)
		   (int16 dest-y)
		   ((sequence :start start :end end :format ,type :transform ,transform :appending t) ,sequence))
		 ;; padding?
		 (incf boffset (+ 8 (* 4 (ceiling (* leftover ,size) 4)))))
	       (setf (buffer-boffset ,display) boffset))))))))

(defun render-composite-glyphs (dest glyph-set source dest-x dest-y sequence
                                &key (op :over)
                                     (alu op)   ;for the fun of it
                                     (src-x 0)
                                     (src-y 0)
                                     (mask-format :none)
                                     (start 0)
                                     (end (length sequence)))
  ;; xxx do we want to go with some translate function as draw-glyphs?
  (declare (type array-index start end))
  (let ((display (picture-display dest)))
    (ensure-render-initialized display)
    (synchronise-picture-state dest)
    (synchronise-picture-state source)
    ;; hmm find out the element size
    (typecase sequence
      ((array (unsigned-byte 8) (*))
       (%render-composite-glyphs +X-RenderCompositeGlyphs8+ card8 nil
                                 display dest glyph-set source dest-x dest-y sequence alu src-x
                                 src-y mask-format start end))
      ((array (unsigned-byte 16) (*))
       (%render-composite-glyphs +X-RenderCompositeGlyphs16+ card16 nil
                                 display dest glyph-set source dest-x dest-y sequence alu src-x
                                 src-y mask-format start end))
      ((array (unsigned-byte 32) (*))
       (%render-composite-glyphs +X-RenderCompositeGlyphs32+ card32 nil
                                 display dest glyph-set source dest-x dest-y sequence alu src-x
                                 src-y mask-format start end))
      (string
       (%render-composite-glyphs #.(cond ((<= char-code-limit (expt 2 8))  '+X-RenderCompositeGlyphs8+)
                                         ((<= char-code-limit (expt 2 16)) '+X-RenderCompositeGlyphs16+)
                                         ((<= char-code-limit (expt 2 32)) '+X-RenderCompositeGlyphs32+)
                                         (t
                                          (error "Wow!")))
                                 #.(cond ((<= char-code-limit (expt 2 8))  'card8)
                                         ((<= char-code-limit (expt 2 16)) 'card16)
                                         ((<= char-code-limit (expt 2 32)) 'card32)
                                         (t
                                          (error "Wow!")))
                                 #'char-code
                                 display dest glyph-set source dest-x dest-y sequence alu src-x
                                 src-y mask-format start end))
      (t
       ;; should we bother testing the array element type?
       (%render-composite-glyphs +X-RenderCompositeGlyphs32+ card32
                                 #'(lambda (elt)
                                     (if (characterp elt)
                                         (char-code elt)
                                         elt))
                                 display dest glyph-set source dest-x dest-y sequence alu src-x
                                 src-y mask-format start end))) ))

;; --- idea: Allow data to be an image to avoid unecessary consing? - noss
(defun render-add-glyph (glyph-set id &key x-origin y-origin x-advance y-advance data)
  (let ((display (glyph-set-display glyph-set)))
    (ensure-render-initialized display)
    (let* ((w (array-dimension data 1))
           (h (array-dimension data 0))
           (bitmap-format (display-bitmap-format display))
           (unit (bitmap-format-unit bitmap-format))
           (byte-lsb-first-p (display-image-lsb-first-p display))
           (bit-lsb-first-p  (bitmap-format-lsb-first-p bitmap-format)))
      (let* ((byte-per-line (* 4 (ceiling 
				  (* w (picture-format-depth (glyph-set-format glyph-set)))
				  32)))
             (request-length (+ 28
				(* h byte-per-line))))
        (with-buffer-request (display (extension-opcode display "RENDER"))
          (data +X-RenderAddGlyphs+)
          (length (ceiling request-length 4))
          (glyph-set glyph-set)
          (card32 1) ;number glyphs
          (card32 id) ;id
          (card16 w)
          (card16 h)
          (int16 x-origin)
          (int16 y-origin)
          (int16 x-advance)
          (int16 y-advance)
          (progn
            (setf (buffer-boffset display) (advance-buffer-offset 28))
            (let ((im (create-image :width w :height h :depth 8 :data data)))
              (write-image-z display im 0 0 w h
                             byte-per-line ;padded bytes per line
                             unit byte-lsb-first-p bit-lsb-first-p)) ))) )))

(defun render-add-glyph-from-picture (glyph-set picture
                                      &key x-origin y-origin x-advance y-advance
                                           x y width height)
  ;; untested, the duplication of x-origin seems bogus.
  ;; Still untested, but these modifications seem to be more likely, (x,y) would be the offset into the picture.
  ;; and orgin advance would be properties of the defined glyph.
  (let ((display (glyph-set-display glyph-set)))
    (with-buffer-request (display (extension-opcode display "RENDER"))
      (data +X-RenderAddGlyphsFromPicture+)
      (glyph-set glyph-set)
      (picture picture)
      (card16 width)
      (card16 height)
      (card16 x-origin)
      (card16 y-origin)
      (card16 x-advance)
      (card16 y-advance)
      (card16 x)
      (card16 y))))

;; untested
(defun render-free-glyphs (glyph-set glyphs)
  "This request removes glyphs from glyph-set. Each glyph must exist in glyph-set (else a Match error results)."
  (let ((display (glyph-set-display glyph-set)))
    (with-buffer-request (display (extension-opcode display "RENDER"))
      (data +X-RenderFreeGlyphs+)
      (glyph-set glyph-set)
      ((sequence :format card32) glyphs))))


#||
;;; --------------------------------------------------------------------------------

;; testing code:

(defun x (op)
  (let ((dpy (open-display "")))
    (render-query-version dpy)
    (unwind-protect
         (let* ((win (screen-root (first (display-roots dpy))))
                (display dpy)
                (pf  (find-window-picture-format win))
                (pm   (xlib:create-pixmap
                       :depth (xlib:drawable-depth win)
                       :drawable win :width 1 :height 1))
                (pm.p  (render-create-picture pm
                                              :format pf
                                              :repeat :on))
                (win.p (render-create-picture win :format pf))
                (gs  (render-create-glyph-set (first
                                               (find-matching-picture-formats
                                                dpy
                                                :alpha 8
                                                :red-max 0
                                                :green-max 0
                                                :blue-max 0)))))
           (xlib:clear-area win)
           (render-fill-rectangle pm.p :src (list #xFFFF 0 0 0) 0 0 100 100)
           (render-add-glyph gs 18
                             :data (make-array (list 3 3)
                                               :initial-contents '((255 000 000)
                                                                   (000 255 000)
                                                                   (000 000 255))
                                               :element-type '(unsigned-byte 8))
                             :x-advance 4
                             :y-advance 0
                             :x-origin 0
                             :y-origin 0)
           (let ((w 50)
                 (h 50))
             (let ((data (make-array (list h w) :element-type '(unsigned-byte 8) :initial-element 0)))
               (dotimes (i w)
                 (dotimes (j h)
                   (setf (aref data i j) (* 3 i))))
               (render-add-glyph gs 17
                                 :data data
                                 :x-advance (+ w 2)
                                 :y-advance 0
                                 :x-origin 0
                                 :y-origin 0)))

           (render-composite-glyphs-8 win.p gs pm.p
                                      200 330
                                      (vector 17 18 18 17 17 17 17 17 17 17)
                                      :alu op
                                      )
                                      ;;
           (display-finish-output dpy)
           (close-display dpy)))))

(defun z (op)
  (let ((dpy (open-display "")))
    (unwind-protect
         (let* ((win (screen-root (first (display-roots dpy))))
                (pic (render-create-picture win))
                (fmt (first (find-matching-picture-formats
                             dpy
                             :red-min 8
                             :green-min 8
                             :blue-min 8
                             :alpha-min 8)))
                (px  (xlib:create-pixmap :width 256 :height 256 :depth (picture-format-depth fmt)
                      :drawable win))
                (px.pic (render-create-picture px :format fmt))
                (px.gc (xlib:create-gcontext :drawable px)))
           (xlib:clear-area win)
           ;;
           (render-fill-rectangle px.pic :src
                                  (list #x8000 #x0000 #x8000 #xFFFF)
                                  0 0 256 256)

           (render-composite :src pic pic px.pic
                             350 350 350 350 0 0 256 256)
           ;;
           (render-fill-rectangle px.pic :over
                                  (list #x8000 #x8000 #x8000 #x8000)
                                  0 0 100 100)
           (render-composite :src
                             px.pic px.pic pic
                             0 0 0 0 350 350
                             256 256)
           (render-fill-rectangle pic op (list #x0 #x0 #x0 #x8000) 200 200 800 800)
           (display-finish-output dpy))
      (close-display dpy))))

;;; ----------------------------------------------------------------------------------------------------

(defun y (op)
  (let ((dpy (open-display "")))
    (render-query-version dpy)
    (unwind-protect
         (let* ((win (screen-root (first (display-roots dpy))))
                (pic
                  (render-create-picture win))
                (px  (xlib:create-pixmap :drawable win
                      :width 256
                      :height 256
                      :depth 32))
                (px.gc (xlib:create-gcontext :drawable px)))
           (dotimes (x 256)
             (dotimes (y 256)
               (setf (xlib:gcontext-foreground px.gc)
                     (dpb x (byte 8 24)
                          (dpb y (byte 8 16)
                               (dpb y (byte 8 8)
                                    y))))
               (xlib:draw-point px px.gc x y)
               ))
           (xlib:clear-area win)
           (let ((q (render-create-picture px
                                           :format
                                           (first (find-matching-picture-formats
                                                   dpy
                                                   :depth 32
                                                   :alpha 8 :red 8 :green 8 :blue 8))
                                           :component-alpha :on
                                           :repeat :off)))
             (render-composite op
                               q
                               q 
                               pic
                               0 0
                               0 0
                               100 100
                               400 400))
           (let ()
             ;;(render-fill-rectangle pic op (list 255 255 255 255) 100 100 200 200)
             (display-finish-output dpy)))
      (close-display dpy))))

(defun zz ()
  (let* ((dpy (xlib:open-display ""))
        (win (screen-root (first (display-roots dpy))))
        (pic (render-create-picture win)))
    (xlib:clear-area win)
    (setf (picture-clip-mask pic) (list 100 100 200 2000))
    (render-fill-rectangle pic :over (list #xFFFF 0 0 #x400) 0 0 2000 2000)
    (display-finish-output dpy)
    (close-display dpy)))
||#


;;;; Cursors

(defun render-create-cursor (picture &optional (x 0) (y 0))
  (let ((display (picture-display picture)))
    (ensure-render-initialized display)
    (synchronise-picture-state picture)
    (let* ((cursor (make-cursor :display display))
           (cid (allocate-resource-id display cursor 'cursor)))
      (setf (cursor-id cursor) cid)
      (with-buffer-request (display (extension-opcode display "RENDER"))
        (data +X-RenderCreateCursor+)
        (resource-id cid)
        (resource-id (picture-id picture))
        (card16 x)
        (card16 y))
      cursor)))
