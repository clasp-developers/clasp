(defpackage :glx
  (:use :common-lisp :xlib)
  (:import-from :xlib
                "DEFINE-ACCESSOR"
                "DEF-CLX-CLASS"
                "DECLARE-EVENT"
                "ALLOCATE-RESOURCE-ID"
                "DEALLOCATE-RESOURCE-ID"
                "PRINT-DISPLAY-NAME"
                "WITH-BUFFER-REQUEST"
                "WITH-BUFFER-REQUEST-AND-REPLY"
                "READ-CARD32"
                "WRITE-CARD32"
                "CARD32-GET"
                "CARD8-GET"
                "SEQUENCE-GET"
                "SEQUENCE-PUT"
                "DATA"

                ;; Types
                "ARRAY-INDEX"
                "BUFFER-BYTES"

                "WITH-DISPLAY"
                "BUFFER-FLUSH"
                "BUFFER-WRITE"
                "BUFFER-FORCE-OUTPUT"
                "ASET-CARD8"
                "ASET-CARD16"
                "ASET-CARD32"
                )
  (:export ;; Constants
           "+VENDOR+"
           "+VERSION+"
           "+EXTENSIONS+"

           ;; Conditions
           "BAD-CONTEXT"
           "BAD-CONTEXT-STATE"
           "BAD-DRAWABLE"
           "BAD-PIXMAP"
           "BAD-CONTEXT-TAG"
           "BAD-CURRENT-WINDOW"
           "BAD-RENDER-REQUEST"
           "BAD-LARGE-REQUEST"
           "UNSUPPORTED-PRIVATE-REQUEST"
           "BAD-FB-CONFIG"
           "BAD-PBUFFER"
           "BAD-CURRENT-DRAWABLE"
           "BAD-WINDOW"

           ;; Requests
           "QUERY-VERSION"
           "QUERY-SERVER-STRING"
           "CREATE-CONTEXT"
           "DESTROY-CONTEXT"
           "IS-DIRECT"
           "QUERY-CONTEXT"
           "GET-DRAWABLE-ATTRIBUTES"
           "MAKE-CURRENT"
           ;;"GET-VISUAL-CONFIGS"
           "CHOOSE-VISUAL"
           "VISUAL-ATTRIBUTE"
           "VISUAL-ID"
           "RENDER"
           "SWAP-BUFFERS"
           "WAIT-GL"
           "WAIT-X"
           ))


(in-package :glx)


(declaim (optimize (debug 3) (safety 3)))


(define-extension "GLX"
    :events (:glx-pbuffer-clobber)
    :errors (bad-context
             bad-context-state
             bad-drawable
             bad-pixmap
             bad-context-tag
             bad-current-window
             bad-render-request
             bad-large-request
             unsupported-private-request
             bad-fb-config
             bad-pbuffer
             bad-current-drawable
             bad-window))


;;; Opcodes.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant +render+ 1)
(defconstant +create-context+ 3)
(defconstant +destroy-context+ 4)
(defconstant +make-current+ 5)
(defconstant +is-direct+ 6)
(defconstant +query-version+ 7)
(defconstant +wait-gl+ 8)
(defconstant +wait-x+ 9)
(defconstant +copy-context+ 10)
(defconstant +swap-buffers+ 11)
(defconstant +get-visual-configs+ 14)
(defconstant +destroy-glx-pixmap+ 15)
(defconstant +query-server-string+ 19)
(defconstant +client-info+ 20)
(defconstant +get-fb-configs+ 21)
(defconstant +query-context+ 25)
(defconstant +get-drawable-attributes+ 29)
)


;;; Constants

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant +vendor+ 1)
(defconstant +version+ 2)
(defconstant +extensions+ 3)
)


;;; Types

;;; FIXME:
;;; - Are all the 32-bit values unsigned?  Do we care?
;;; - These are not used much, yet.
(progn
  (deftype attribute-pair ())
  (deftype bitfield () 'mask32)
  (deftype bool32 () 'card32)           ; 1 for true and 0 for false 
  (deftype enum () 'card32)
  (deftype fbconfigid () 'card32)
  ;; FIXME: How to define these two?
  (deftype float32 () 'single-float)
  (deftype float64 () 'double-float)
  ;;(deftype glx-context () 'card32)
  (deftype context-tag () 'card32)
  ;;(deftype glx-drawable () 'card32)
  (deftype glx-pixmap () 'card32)
  (deftype glx-pbuffer () 'card32)
  (deftype glx-render-command () #|TODO|#)
  (deftype glx-window () 'card32)
  #-(and)
  (deftype visual-property ()
    "An ordered list of 32-bit property values followed by unordered pairs of
property types and property values."
    ;; FIXME: maybe CLX-LIST or even just LIST?
    'clx-sequence))


;;; FIXME: DEFINE-ACCESSOR interns getter and setter in XLIB package
;;; (using XINTERN).  Therefore the accessors defined below can only
;;; be accessed using double-colon, which is a bad style.  Or these
;;; forms must be taken to another file so the accessors exist before
;;; we get to this file.

#-(and)
(define-accessor glx-context-tag (32)
  ((index) `(read-card32 ,index))
  ((index thing) `(write-card32 ,index ,thing)))

#-(and)
(define-accessor glx-enum (32)
  ((index) `(read-card32 ,index))
  ((index thing) `(write-card32 ,index ,thing)))


;;; FIXME: I'm just not sure we need a seperate accessors for what
;;; essentially are aliases for other types.  Maybe use compiler
;;; macros?
;;;
;;; This trick won't do because CLX wants e.g. CONTEXT-TAG to be a
;;; known accessor.  The only trick left I think is to change the
;;; XINTERN function to intern the new symbols in the same package as
;;; he symbol part of it comes from.  Don't know if it would break
;;; anything, thought.  (I would be quite surprised if it did -- there
;;; is only one package in CLX after all: XLIB.)
;;;
;;; I also found the origin of the error (about symbol not being a
;;; known accessor): INDEX-INCREMENT function.  Looks like all we have
;;; to do is to add an XLIB::BYTE-WIDTH property to the type symbol
;;; plist.  But accessors are macros, not functions, anyway.

#-(and)
(progn
  (declaim (inline context-tag-get context-tag-put enum-get enum-put))
  (defun context-tag-get (index) (card32-get index))
  (defun context-tag-put (index thing) (card32-put index thing))
  (defun enum-get (index) (card32-get index))
  (defun enum-put (index thing) (card32-put index thing))
)


;;; Structures


(def-clx-class (context (:constructor %make-context)
                        (:print-function print-context)
                        (:copier nil))
  (id 0 :type resource-id)
  (display nil :type (or null display))
  (tag 0 :type card32)
  (drawable nil :type (or null drawable))
  ;; TODO: There can only be one current context (as far as I
  ;; understand).  If so, we'd need only one buffer (otherwise it's a
  ;; big waste to have a quarter megabyte buffer for each context; or
  ;; we could allocate/grow the buffer on demand).
  ;;
  ;; 256k buffer for Render command.  Big requests are served with
  ;; RenderLarge command.  First 8 octets are Render request fields.
  ;;
  (rbuf (make-array (+ 8 (* 256 1024)) :element-type '(unsigned-byte 8)) :type buffer-bytes)
  ;; Index into RBUF where the next rendering command should be inserted.
  (index 8 :type array-index))


(defun print-context (ctx stream depth)
  (declare (type context ctx)
           (ignore depth))
  (print-unreadable-object (ctx stream :type t)
    (print-display-name (context-display ctx) stream)
    (write-string " " stream)
    (princ (context-id ctx) stream)))


(def-clx-class (visual (:constructor %make-visual)
                       (:print-function print-visual)
                       (:copier nil))
  (id 0 :type resource-id)
  (attributes nil :type list))


(defun print-visual (visual stream depth)
  (declare (type visual visual)
           (ignore depth))
  (print-unreadable-object (visual stream :type t)
    (write-string "ID: " stream)
    (princ (visual-id visual) stream)
    (write-string " " stream)
    (princ (visual-attributes visual) stream)))



;;; Events.

(defconstant +damaged+ #x8017)
(defconstant +saved+ #x8018)
(defconstant +window+ #x8019)
(defconstant +pbuffer+ #x801a)


(declare-event :glx-pbuffer-clobber
  (card16 sequence)
  (card16 event-type) ;; +DAMAGED+ or +SAVED+
  (card16 draw-type) ;; +WINDOW+ or +PBUFFER+
  (resource-id drawable)
  ;; FIXME: (bitfield buffer-mask)
  (card32 buffer-mask)
  (card16 aux-buffer)
  (card16 x y width height count))



;;; Errors.

(define-condition bad-context (request-error) ())
(define-condition bad-context-state (request-error) ())
(define-condition bad-drawable (request-error) ())
(define-condition bad-pixmap (request-error) ())
(define-condition bad-context-tag (request-error) ())
(define-condition bad-current-window (request-error) ())
(define-condition bad-render-request (request-error) ())
(define-condition bad-large-request (request-error) ())
(define-condition unsupported-private-request (request-error) ())
(define-condition bad-fb-config (request-error) ())
(define-condition bad-pbuffer (request-error) ())
(define-condition bad-current-drawable (request-error) ())
(define-condition bad-window (request-error) ())

(define-error bad-context decode-core-error)
(define-error bad-context-state decode-core-error)
(define-error bad-drawable decode-core-error)
(define-error bad-pixmap decode-core-error)
(define-error bad-context-tag decode-core-error)
(define-error bad-current-window decode-core-error)
(define-error bad-render-request decode-core-error)
(define-error bad-large-request decode-core-error)
(define-error unsupported-private-request decode-core-error)
(define-error bad-fb-config decode-core-error)
(define-error bad-pbuffer decode-core-error)
(define-error bad-current-drawable decode-core-error)
(define-error bad-window decode-core-error)



;;; Requests.


(defun query-version (display)
  (with-buffer-request-and-reply (display (extension-opcode display "GLX") nil)
    ((data +query-version+)
     (card32 1)
     (card32 3))
    (values
     (card32-get 8)
     (card32-get 12))))


(defun query-server-string (display screen name)
  "NAME is one of +VENDOR+, +VERSION+ or +EXTENSIONS+"
  (with-buffer-request-and-reply (display (extension-opcode display "GLX") nil)
    ((data +query-server-string+)
     (card32 (or (position screen (display-roots display) :test #'eq) 0))
     (card32 name))
    (let* ((length (card32-get 12))
           (bytes (sequence-get :format card8
                                :result-type '(simple-array card8 (*))
                                :index 32
                                :length length)))
      (declare (type (simple-array card8 (*)) bytes)
               (type fixnum length))
      (map-into (make-string (1- length)) #'code-char bytes))))


(defun client-info (display)
  ;; TODO: This should be invoked automatically when using this
  ;; library in initialization stage.
  ;; 
  ;; TODO: No extensions supported yet.
  ;; 
  ;; *** Maybe the LENGTH field must be filled in some special way
  ;;     (similar to data)?
  (with-buffer-request (display (extension-opcode display "GLX"))
    (data +client-info+)
    (card32 4) ; length of the request
    (card32 1) ; major
    (card32 3) ; minor
    (card32 0) ; n
    ))


;;; XXX: This looks like an internal thing.  Should name appropriately.
(defun make-context (display)
  (let ((ctx (%make-context :display display)))
    (setf (context-id ctx)
          (allocate-resource-id display ctx 'context))
    ;; Prepare render request buffer.
    ctx))


(defun create-context (screen visual
                       &optional
                       (share-list 0)
                       (is-direct nil))
  "Do NOT use the direct mode, yet!"
  (let* ((root (screen-root screen))
         (display (drawable-display root))
         (ctx (make-context display)))
    (with-buffer-request (display (extension-opcode display "GLX"))
      (data +create-context+)
      (resource-id (context-id ctx))
      (resource-id visual)
      (card32 (or (position screen (display-roots display) :test #'eq) 0))
      (resource-id share-list)
      (boolean is-direct))
    ctx))


;;; TODO: Maybe make this var private to GLX-MAKE-CURRENT and GLX-GET-CURRENT-CONTEXT only?
;;;
(defvar *current-context* nil)


(defun destroy-context (ctx)
  (let ((id (context-id ctx))
        (display (context-display ctx)))
    (with-buffer-request (display (extension-opcode display "GLX"))
      (data +destroy-context+)
      (resource-id id))
    (deallocate-resource-id display id 'context)
    (setf (context-id ctx) 0)
    (when (eq ctx *current-context*)
      (setf *current-context* nil))))


(defun is-direct (ctx)
  (let ((display (context-display ctx)))
    (with-buffer-request-and-reply (display (extension-opcode display "GLX") nil)
        ((data +is-direct+)
         (resource-id (context-id ctx)))
      (card8-get 8))))


(defun query-context (ctx)
  ;; TODO: What are the attribute types?
  (let ((display (context-display ctx)))
    (with-buffer-request-and-reply (display (extension-opcode display "GLX") nil)
        ((data +query-context+)
         (resource-id (context-id ctx)))
      (let ((num-attributes (card32-get 8)))
        ;; FIXME: Is this really so?
        (declare (type fixnum num-attributes))
        (loop
           repeat num-attributes
           for i fixnum upfrom 32 by 8
           collecting (cons (card32-get i)
                            (card32-get (+ i 4))))))))


(defun get-drawable-attributes (drawable)
  (let ((display (drawable-display drawable)))
    (with-buffer-request-and-reply (display (extension-opcode display "GLX") nil)
        ((data +get-drawable-attributes+)
         (drawable drawable))
      (let ((num-attributes (card32-get 8)))
        ;; FIXME: Is this really so?
        (declare (type fixnum num-attributes))
        (loop
           repeat num-attributes
           for i fixnum upfrom 32 by 8
           collecting (cons (card32-get i)
                            (card32-get (+ i 4))))))))


;;; TODO: What is the idea behind passing drawable to this function?
;;; Can a context be made current for different drawables at different
;;; times?  (Man page on glXMakeCurrent says that context's viewport
;;; is set to the size of drawable when creating; it does not change
;;; afterwards.)
;;; 
(defun make-current (drawable ctx)
  (let ((display (drawable-display drawable))
        (old-tag (if *current-context* (context-tag *current-context*) 0)))
    (with-buffer-request-and-reply (display (extension-opcode display "GLX") nil)
        ((data +make-current+)
         (resource-id (drawable-id drawable))
         (resource-id (context-id ctx))
         ;; *** CARD32 is really a CONTEXT-TAG
         (card32 old-tag))
      (let ((new-tag (card32-get 8)))
        (setf (context-tag ctx) new-tag
              (context-drawable ctx) drawable
              (context-display ctx) display
              *current-context* ctx)))))


;;; FIXME: Decide how to represent and use these.
(eval-when (:load-toplevel :compile-toplevel :execute)
  (macrolet ((generate-config-properties ()
               (let ((list '((:glx-visual visual-id)
                             (:glx-class card32)
                             (:glx-rgba bool32)
                             (:glx-red-size card32)
                             (:glx-green-size card32)
                             (:glx-blue-size card32)
                             (:glx-alpha-size card32)
                             (:glx-accum-red-size card32)
                             (:glx-accum-green-size card32)
                             (:glx-accum-blue-size card32)
                             (:glx-accum-alpha-size card32)
                             (:glx-double-buffer bool32)
                             (:glx-stereo bool32)
                             (:glx-buffer-size card32)
                             (:glx-depth-size card32)
                             (:glx-stencil-size card32)
                             (:glx-aux-buffers card32)
                             (:glx-level int32))))
                 `(progn
                    ,@(loop for (symbol type) in list
                         collect `(setf (get ',symbol 'visual-config-property-type) ',type))
                    (defparameter *visual-config-properties*
                      (map 'vector #'car ',list))
                    (declaim (type simple-vector *visual-config-properties*))
                    (deftype visual-config-property ()
                      '(member ,@(mapcar #'car list)))))))
    (generate-config-properties)))


(defun make-visual (attributes)
  (let ((id-cons (first attributes)))
    (assert (eq :glx-visual (car id-cons))
            (id-cons)
            "GLX visual id must be first in attributes list!")
    (%make-visual :id (cdr id-cons)
                  :attributes (rest attributes))))


(defun visual-attribute (visual attribute)
  (assert (or (numberp attribute)
              (find attribute *visual-config-properties*))
          (attribute)
          "~S is not a known GLX visual attribute." attribute)
  (cdr (assoc attribute (visual-attributes visual))))


;;; TODO: Make this return nice structured objects with field values of correct type.
;;; FIXME: Looks like every other result is corrupted.
(defun get-visual-configs (screen)
  (let ((display (drawable-display (screen-root screen))))
    (with-buffer-request-and-reply (display (extension-opcode display "GLX") nil)
      ((data +get-visual-configs+)
       (card32 (or (position screen (display-roots display) :test #'eq) 0)))
      (let* ((num-visuals (card32-get 8))
             (num-properties (card32-get 12))
             (num-ordered (length *visual-config-properties*)))
        ;; FIXME: Is this really so?
        (declare (type fixnum num-ordered num-visuals num-properties))
        (loop
           with index fixnum = 28
           repeat num-visuals
           collecting (make-visual
                       (nconc (when (<= num-ordered num-properties)
                                (map 'list #'(lambda (property)
                                               (cons property (card32-get (incf index 4))))
                                     *visual-config-properties*))
                              (when (< num-ordered num-properties)
                                (loop repeat (/ (- num-properties num-ordered) 2)
                                   collecting (cons (card32-get (incf index 4))
                                                    (card32-get (incf index 4))))))))))))


(defun choose-visual (screen attributes)
  "ATTRIBUTES is a list of desired attributes for a visual.  The elements may be
either a symbol, which means that the boolean attribute with that name must be true; or
it can be a list of the form: (attribute-name value &optional (test '<=)) which means that
the attribute named attribute-name must satisfy the test when applied to the given value and
attribute's value in visual.
Example: '(:glx-rgba (:glx-alpha-size 4) :glx-double-buffer (:glx-class 4 =)."
  ;; TODO: Add type checks
  ;;
  ;; TODO: This function checks only supplied attributes; should check
  ;; all attributes, with default for boolean type being false, and
  ;; for number types zero.
  ;; 
  ;; TODO: Make this smarter, like the docstring says, instead of
  ;; parrotting the inflexible C API.
  ;; 
  (flet ((visual-matches-p (visual attributes)
           (dolist (attribute attributes t)
             (etypecase attribute
               (symbol (not (null (visual-attribute visual attribute))))
               (cons (<= (second attribute) (visual-attribute visual (car attribute))))))))
    (let* ((visuals (get-visual-configs screen))
           (candidates (loop
                          for visual in visuals
                          when (visual-matches-p visual attributes)
                          collect visual))
           (result (first candidates)))
    
      (dolist (candidate (rest candidates))
        ;; Visuals with glx-class 3 (pseudo-color) and 4 (true-color)
        ;; are preferred over glx-class 2 (static-color) and 5 (direct-color).
        (let ((class (visual-attribute candidate :glx-class)))
          (when (or (= class 3)
                    (= class 4))
            (setf result candidate))))
      result)))


(defun render ()
  (declare (optimize (debug 3)))
  (assert (context-p *current-context*)
          (*current-context*)
          "~S is not a context." *current-context*)
  (let* ((ctx *current-context*)
         (display (context-display ctx))
         (rbuf (context-rbuf ctx))
         (index (context-index ctx)))
    (declare (type buffer-bytes rbuf)
             (type array-index index))
    (when (< 8 index)
      (with-display (display)
        ;; Flush display's buffer first so we don't get messed up with X requests.
        (buffer-flush display)
        ;; First, update the Render request fields.
        (aset-card8 (extension-opcode display "GLX") rbuf 0)
        (aset-card8 1 rbuf 1)
        (aset-card16 (ceiling index 4) rbuf 2)
        (aset-card32 (context-tag ctx) rbuf 4)
        ;; Then send the request.
        (buffer-write rbuf display 0 (context-index ctx))
        ;; Start filling from the beginning
        (setf (context-index ctx) 8)))
    (values)))


(defun swap-buffers ()
  (assert (context-p *current-context*)
          (*current-context*)
          "~S is not a context." *current-context*)
  (let* ((ctx *current-context*)
         (display (context-display ctx)))
    ;; Make sure all rendering commands are sent away.
    (glx:render)
    (with-buffer-request (display (extension-opcode display "GLX"))
      (data +swap-buffers+)
      ;; *** GLX_CONTEXT_TAG
      (card32 (context-tag ctx))
      (resource-id (drawable-id (context-drawable ctx))))
    (display-force-output display)))


;;; FIXME: These two are more complicated than sending messages.  As I
;;; understand it, wait-gl should inhibit any X requests until all GL
;;; requests are sent...
(defun wait-gl ()
  (assert (context-p *current-context*)
          (*current-context*)
          "~S is not a context." *current-context*)
  (let* ((ctx *current-context*)
         (display (context-display ctx)))
    (with-buffer-request (display (extension-opcode display "GLX"))
      (data +wait-gl+)
      ;; *** GLX_CONTEXT_TAG
      (card32 (context-tag ctx)))))


(defun wait-x ()
  (assert (context-p *current-context*)
          (*current-context*)
          "~S is not a context." *current-context*)
  (let* ((ctx *current-context*)
         (display (context-display ctx)))
    (with-buffer-request (display (extension-opcode display "GLX"))
      (data +wait-x+)
      ;; *** GLX_CONTEXT_TAG
      (card32 (context-tag ctx)))))
