;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;; Tests image code by randomly reading, copying and then writing images to
;;; the exact same place on the screen.  If everything works, just the borders
;;; of the image windows appear.  If one of these image windows is garbled,
;;; then somewhere something is broken.  Entry point is the function
;;; IMAGE-TEST

(in-package :xlib)

(export '(image-test))

(defvar *image-test-host* "")

(defvar *image-test-nimages* 25)

(defvar *image-test-copy* t)

(defvar *image-test-copy-random-subimage* t)

(defvar *image-test-put-random-subimage* t)

(defvar *image-test-get-image-result-type-choices*
  '(image-x image-x image-xy image-z))

(defvar *image-test-get-image-image-x-format-choices*
  '(:xy-pixmap :z-pixmap))

(defun image-test
       (&key
	(host *image-test-host*)
	(nimages *image-test-nimages*)
	(copy *image-test-copy*)
	(copy-random-subimage *image-test-copy-random-subimage*)
	(put-random-subimage *image-test-put-random-subimage*)
	(get-image-result-type-choices
	  *image-test-get-image-result-type-choices*)
	(get-image-image-x-format-choices
	  *image-test-get-image-image-x-format-choices*))
  (declare (ignore host))
  (let* ((display nil)
	 (abort t)
	 (images nil))
    (loop 
      (setq images nil)
      (unwind-protect
	  (progn
	    (setq display (open-default-display))
	    (let* ((screen (display-default-screen display))
		   (window (screen-root screen))
		   (gcontext (create-gcontext
			      :foreground (screen-white-pixel screen)
			      :background (screen-black-pixel screen)
			      :drawable window
			      :font (open-font display "fixed"))))
	      (dotimes (i nimages)
		(let ((image (image-test-get-image
			       window
			       get-image-result-type-choices
			       get-image-image-x-format-choices)))
		  (format t "~&Image=~S~%" image)
		  (let ((copy (if copy
				  (image-test-copy-image
				    image
				    copy-random-subimage)
				image)))
		    (format t "~&Copy=~S~%" copy)
		    (push (list image copy) images)
		    (image-test-put-image
		      screen gcontext copy
		      (concatenate
			'string (image-info image) (image-info copy))
		      put-random-subimage))))
	      (unless (y-or-n-p "More ") (return))
	      (setq abort nil)))
	(close-display (shiftf display nil) :abort abort))
      (sleep 10))
    (reverse images)))

(defun image-test-choose (list)
  (nth (random (length list)) list))

(defun image-test-get-image (window result-type-choices image-x-format-choices)
  (let* ((x (random (floor (drawable-width window) 3)))
	 (y (random (floor (drawable-height window) 3)))
	 (hw (floor (- (drawable-width window) x) 3))
	 (hh (floor (- (drawable-height window) y) 3))
	 (width (+ hw hw (random hw)))
	 (height (+ hh hh (random hh)))
	 (result-type (image-test-choose result-type-choices))
	 (format
	   (ecase result-type
	     (image-x (image-test-choose image-x-format-choices))
	     (image-xy :xy-pixmap)
	     (image-z :z-pixmap)))
	 (image (get-image window :x x :y y :width width :height height
			   :format format :result-type result-type)))
    (setf (getf (image-plist image) :root-x) x)
    (setf (getf (image-plist image) :root-y) y)
    image))

(defun image-test-subimage-parameters (image random-subimage-p)
  (if random-subimage-p 
      (let* ((x (random (floor (image-width image) 3)))
	     (y (random (floor (image-height image) 3)))
	     (hw (floor (- (image-width image) x) 3))
	     (hh (floor (- (image-height image) y) 3))
	     (width (+ hw hw (random hw)))
	     (height (+ hh hh (random hh))))
	(values x y width height))
    (values 0 0 (image-width image) (image-height image))))

(defun image-test-copy-image (image random-subimage-p)
  (let ((result-type
	  (if (zerop (random 2))
	      (type-of image)
	    (etypecase image
	      (image-x (ecase (image-x-format image)
			 (:xy-pixmap 'image-xy)
			 (:z-pixmap 'image-z)))
	      ((or image-xy image-z) 'image-x)))))
    (multiple-value-bind (x y width height)
	(image-test-subimage-parameters image random-subimage-p)
      (incf (getf (image-plist image) :root-x) x)
      (incf (getf (image-plist image) :root-y) y)
      (copy-image image :x x :y y :width width :height height
		  :result-type result-type))))

(defun image-test-put-image (screen gcontext image info random-subimage-p)
  (multiple-value-bind (src-x src-y width height)
      (image-test-subimage-parameters image random-subimage-p)
    (let* ((border-width 1)
	   (root-x (getf (image-plist image) :root-x))
	   (root-y (getf (image-plist image) :root-y))
	   (x (+ src-x root-x (- border-width)))
	   (y (+ src-y root-y (- border-width))))
      (unless (or (zerop width) (zerop height))
	(let ((window
		(create-window
		  :parent (screen-root screen) :x x :y y
		  :width width :height height
		  :border-width border-width
		  :background (screen-white-pixel screen)
		  :override-redirect :on)))
	  (map-window window)
	  (display-finish-output (drawable-display window))
	  (put-image window gcontext image
		     :x 0 :y 0 :src-x src-x :src-y src-y
		     :width width :height height)
	  (draw-image-glyphs window gcontext 0 (1- height) info)
	  (display-finish-output (drawable-display window))
	  window)))))

(defun image-info (image)
  (etypecase image
    (image-x (ecase (image-x-format image)
	       (:xy-pixmap "XXY")
	       (:z-pixmap  "XZ ")))
    (image-xy "XY ")
    (image-z  "Z  ")))
