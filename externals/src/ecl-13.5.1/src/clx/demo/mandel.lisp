(defpackage "XMANDEL"
  (:use "CL")
  (:export "NEW-WINDOW" "EVENT-LOOP"))

(in-package "XMANDEL")

(defvar *display* (xlib:open-default-display))
(defvar *screen* (xlib:display-default-screen *display*))

(defvar *backing-store* (make-hash-table) "Backing store hashtable, keyed off window id")
(defvar *colmap* nil)
(defvar *helpwin* nil)
(defvar *zoom-table* (make-hash-table))
(defvar *zoomcolmap* (xlib:create-gcontext
		      :drawable (xlib:screen-root *screen*)
		      :foreground (xlib:screen-white-pixel *screen*)
		      :function boole-xor))
(defvar *white* (xlib:create-gcontext
		 :drawable (xlib:screen-root *screen*)
		 :foreground (xlib:screen-white-pixel *screen*)
		 ))
(defvar *winmap* (make-hash-table))
(defvar *textmap* (xlib:create-gcontext
		   :drawable (xlib:screen-root *screen*)
		   :foreground (xlib:screen-black-pixel *screen*)
		   :background (xlib:screen-white-pixel *screen*)))

;;; OK, this is an ugly hack to make sure we can handle
;;; shift and modstate in a sane way, alas we can't 100% rely
;;; on "current state of keyboard", since we only process events
;;; with a noticeable delay, at eth best of times, so a fast keyboarder
;;; can fool us, we are, however, IIRC, guaranteed that all events are
;;; serialised, so...
(defvar *modstate* nil)
(declaim (list *modstate*))
(defun make-shift-foo ()
  (let ((rv 0))
    (if (member :shift *modstate*)
	(setf rv 1))
    (if (member :character-set-switch *modstate*)
	(setf rv (+ rv 2)))
    rv))

(defstruct (mandel-square (:conc-name ms-))
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (s 512 :type fixnum)
  (base-r 0.0d0 :type double-float)
  (base-i 0.0d0 :type double-float)
  (maxiter 1024 :type fixnum)
  (dr 0.0d0 :type double-float)
  (di 0.0d0 :type double-float)
  win
  )

(defun make-queue (&rest args)
  (apply #'make-instance 'queue args))

(defclass queue ()
  ((head :initform nil :accessor q-head)
   (tail :initform nil :accessor q-tail)))
(defclass out-queue ()
  ((win-queues :accessor win-queues :initarg :xyzzy-1)
   (seen-windows :accessor windows :initform nil)
   (win-list :accessor win-list :initarg :xyzzy-2)
   (last-window :accessor last-window :initform nil))
  (:default-initargs :xyzzy-1 (make-hash-table)
		     :xyzzy-2 (make-instance 'queue)))

(defvar *sysqueue* (make-instance 'out-queue))

(defgeneric empty-p (queue))
(defgeneric empty (queue))
(defgeneric empty-win (queue win))
(defgeneric enqueue (queue item))
(defgeneric queue-push (queue item))
(defgeneric dequeue (queue))

(defmethod empty-p ((q null))
  t)
(defmethod empty-p ((q queue))
  (null (q-head q)))
(defmethod empty-p ((q out-queue))
  (let ((coll nil))
    (maphash #'(lambda (key val)
		 (declare (ignore key))
		 (push (empty-p val) coll))
	     (win-queues q))
    (every #'identity coll)))

(defmethod empty ((q null))
  nil)
(defmethod empty ((q queue))
  (setf (q-head q) nil)
  (setf (q-tail q) nil))
(defmethod empty ((q out-queue))
  (maphash #'(lambda (key val) (declare (ignore key)) (empty val))
	   (win-queues q)))
(defmethod empty-win ((q out-queue) win)
  (let ((temp-queue (gethash win (win-queues q))))
    (empty temp-queue)))
  
(defmethod enqueue ((q queue) item)
  (cond ((empty-p q)
	 (setf (q-head q) (cons item nil))
	 (setf (q-tail q) (q-head q)))
	(t (setf (cdr (q-tail q)) (cons item nil))
	   (setf (q-tail q) (cdr (q-tail q))))))
(defmethod enqueue ((q out-queue) item)
  (let ((windows (q-head (win-list q)))
	(win (ms-win item)))
    (declare (type xlib:window win))
    (unless (member win windows)
      (enqueue (win-list q) win))
    (unless (member win (windows q))
      (push win (windows q)))
    (let ((temp-queue (gethash win (win-queues q))))
      (if (null temp-queue)
	  (let ((new (make-queue)))
	    (setf (gethash win (win-queues q)) new)
	    (enqueue new item))
	(enqueue temp-queue item)))))

(defmethod queue-push ((q queue) item)
  (cond ((empty-p q)
	 (setf (q-head q) (cons item nil))
	 (setf (q-tail q) (q-head q)))
	(t (setf (q-head q) (cons item (q-head q))))))
(defmethod queue-push ((q out-queue) item)
  (let ((windows (q-head (win-list q)))
	(win (ms-win item)))
    (declare (type xlib:window win))
    (unless (member win windows)
      (enqueue (win-list q) win))
    (unless (member win (windows q))
      (push win (windows q)))
    (let ((temp-queue (gethash win (win-queues q))))
      (if (null temp-queue)
	  (let ((new (make-queue)))
	    (setf (gethash win (win-queues q)) new)
	    (queue-push new item))
	(queue-push temp-queue item)))))

(defmethod dequeue ((q out-queue))
  (if (empty-p q)
      nil
      (let ((windows (win-list q)))
	(do* ((next (dequeue windows))
	      (finished nil)
	      (val nil)
	      (temp-queue (gethash next (win-queues q))
			  (gethash next (win-queues q))))
	     (finished val) 
	  (cond ((empty-p temp-queue)
		 (setf next (dequeue windows)))
		(t (setf val (dequeue temp-queue))
		   (unless (empty-p temp-queue)
		     (enqueue windows next))
		   (setf finished t)))))))
(defmethod dequeue ((q queue))
  (prog1
    (car (q-head q))
    (if (not (empty-p q))
	(setf (q-head q) (cdr (q-head q))))
    (if (null (q-head q))
	(progn
	  (setf (q-head q) nil)
	  (setf (q-tail q) nil)))))

(defun iter (rc ic max)
  (declare (double-float rc ic)
	   (fixnum max))
  (do ((x 0.0d0 (the double-float (+ (- (* x x) (* y y)) rc)))
       (y 0.0d0 (the double-float (+ (* 2.0d0 x y) ic)))
       (n 1 (the fixnum (1+ n))))
      ((or (>= n max) (>= (+ (* x x) (* y y)) 4.0d0))
       n)))
;;; (a+bi)^2        -->
;;; (a+bi)(a+bi)    -->
;;; a^2+2abi+(bi)^2 -->
;;; a^2+2abi-b^2

(defclass zoomer ()
  ((zoom-type :initarg :type :reader zoom-type :type fixnum)
   (start-x :initarg :x :reader start-x :type fixnum)
   (start-y :initarg :y :reader start-y :type fixnum)
   (stop-x :accessor stop-x :initform -1 :type fixnum)
   (stop-y :accessor stop-y :initform -1 :type fixnum)
   (win :reader win :initarg :win)))

;;;(defmethod print-object ((object zoomer) stream)
;;;  (format stream "<zoomer [type ~a] [~a ~a] -> [~a ~a]>~%"
;;;	  (zoom-type object) (start-x object) (start-y object)
;;;	  (stop-x object) (stop-y object)))

(defun init-colours ()
  (unless *colmap*
    (setf *colmap* (make-array 256 :element-type 'xlib:gcontext :initial-element *zoomcolmap*))
    (setf (aref *colmap* 0) (xlib:create-gcontext
		    :drawable (xlib:screen-root *screen*)
		    :foreground (xlib:alloc-color
				 (xlib:screen-default-colormap *screen*)
				 (xlib:make-color :red 0
						  :green 0
						  :blue 0))))
    (loop for index from 1 to 255
	  do (setf (aref *colmap* index)
		   (xlib:create-gcontext
		    :drawable (xlib:screen-root *screen*)
		    :foreground (xlib:alloc-color
				 (xlib:screen-default-colormap *screen*)
				 (xlib:make-color :red (random 1.0)
						  :green (random 1.0)
						  :blue (random 1.0))))))))
(defmacro modcol (col max)
  `(if (= ,col ,max) 0 (1+ (mod ,col 255))))

(defun plot (win col x y max)
  (declare (fixnum col x y max))
  (let ((col (modcol col max)))
    (xlib:draw-point win (aref *colmap* col) x y)
    (setf (aref (the (simple-array (integer 0 255) (512 512))
		  (gethash win *backing-store*)) x y) col)))

(defun display-help ()
  (unless *helpwin*
    (setf *helpwin* (xlib:create-window
		     :parent (xlib:screen-root *screen*)
		     :x 512
		     :y 512
		     :width 310
		     :height 180
		     :event-mask (xlib:make-event-mask :exposure)
		     :backing-store :always
		     :background (xlib:screen-white-pixel *screen*)))
    (xlib:map-window *helpwin*)
    (xlib:display-force-output *display*))
  (unless (xlib:gcontext-font *textmap*)
    (let ((fixed (xlib:list-fonts *display* "fixed"))
	  font)
      (if fixed
	  (setf font (xlib:open-font *display* "fixed"))
	(error "Could not open suitable font"))
      (setf (xlib:gcontext-font *textmap*) (if (consp fixed)
					       (car fixed)
					     fixed))))
  (xlib:draw-rectangle *helpwin* *white* 0 0 (xlib:drawable-width *helpwin*) (xlib:drawable-height *helpwin*) t)
  (xlib:draw-glyphs *helpwin* *textmap* 10 13 "Button 1: Zoom same")
  (xlib:draw-glyphs *helpwin* *textmap* 10 33 "Button 2: Zoom new")
  (xlib:draw-glyphs *helpwin* *textmap* 10 53 "Button 3: Zoom out, same")
  (xlib:draw-glyphs *helpwin* *textmap* 10 93 "In general, click to zoom centred on mouse,")
  (xlib:draw-glyphs *helpwin* *textmap* 10 113 "drag to zoom a region.")
  (xlib:draw-glyphs *helpwin* *textmap* 10 153 "Q: quit")
  (xlib:display-force-output *display*))

(defun repaint-window (win x-low y-low x-high y-high)
  (declare (fixnum x-low y-low x-high y-high))
  (if (eq win *helpwin*)
      (display-help)
    (let ((bs (the (simple-array (integer 0 255) (512 512)) (gethash win *backing-store*))))
      (loop for y of-type fixnum from y-low to y-high
	    do
	    (loop for x of-type fixnum from x-low to x-high
		  do (xlib:draw-point win (aref *colmap* (aref bs x y)) x y))))))

(defun fill-square (win col x y s max)
  (declare (fixnum col x y s max))
  (let ((col (modcol col max)))
    (xlib:draw-rectangle win (aref *colmap* col) x y s s t)
    (let ((bs (the (simple-array (integer 0 255) (512 512)) (gethash win *backing-store*))))
      (loop for px of-type fixnum from x to (1- (+ x s))
	    do (loop for py of-type fixnum from y to (1- (+ y s))
		     do (setf (aref bs px py) col))))))

(defun make-square (win x y side bx by dx dy &optional (maxiter 1024))
  (declare (xlib:window win)
	   (fixnum x y side maxiter)
	   (double-float bx by dx dy))
  (let ((sq (make-mandel-square
	     :x x :y y :s side
	     :base-r bx :base-i by
	     :dr dx :di dy
	     :maxiter maxiter
	     :win win)))
    (queue-push *sysqueue* sq)))

(defun mandel-win (win lx ly hx hy &optional (maxiter 1024))
  (declare (xlib:window win)
	   (double-float lx ly hx hy)
	   (fixnum maxiter))
  (let ((dx (coerce (/ (- hx lx) 512.0d0) 'double-float))
	(dy (coerce (/ (- hy ly) 512.0d0) 'double-float)))	    
    (setf (gethash win *winmap*)
	  (make-mandel-square :x 0 :y 0 :s 512
			      :base-r lx :base-i ly
			      :dr dx :di dy :maxiter maxiter))
    (make-square win 0 256   256 lx ly dx dy maxiter)
    (make-square win 256 256 256 lx ly dx dy maxiter)
    (make-square win 256 0   256 lx ly dx dy maxiter)
    (make-square win 0 0     256 lx ly dx dy maxiter)))

(defun new-window (lx ly hx hy &optional (maxiter 1024))
  (let ((win (xlib:create-window
	      :parent (xlib:screen-root *screen*)
	      :x (+ 100 (random 50)) :y (+ 100 (random 50))
	      :width 512 :height 512
	      :bit-gravity :center
	      :event-mask (xlib:make-event-mask
			   :button-motion :button-press :button-release
			   :key-press :exposure)))
	(ar (make-array '(512 512)
			:element-type '(integer 0 255) :initial-element 0))
	)
    (setf (gethash win *backing-store*) ar)
    (xlib:map-window win)
    (mandel-win win
		(coerce lx 'double-float) (coerce ly 'double-float)
		(coerce hx 'double-float) (coerce hy 'double-float) maxiter)))

(defun fill-square-p (ix iy s bx by dx dy max win)
  (declare (fixnum ix iy s max)
	   (double-float bx by dx dy))
  (let ((norm (iter (+ bx (* ix dx)) (+ by (* iy dy)) max))) 
    (and
     (loop for px from ix below (+ ix s)
	   for x of-type double-float = (+ bx (* px dx))
	   with y = (+ by (* iy dy))
	   for i = (iter x y max)
	   do (plot win i px iy max)
	   while (= i norm)
	   finally (return t))
     (loop for py from iy below (+ s iy)
	   for y of-type double-float = (+ by (* py dy))
	   with x = (+ bx (* ix dx))
	   for i = (iter x y max)
	   do (plot win i ix py max)
	   while (= i norm)
	   finally (return t))
     (loop for px from (1- (+ s ix)) downto ix
	   for x of-type double-float = (+ bx (* px dx))
	   with y = (+ by (* dy (1- (+ s iy))))
	   for i = (iter x y max)
	   do (plot win i px iy max)
	   if (/= i norm) return nil
	   finally (return t))
     (loop for py from (1- (+ s iy)) downto iy
	   for y of-type double-float = (+ by (* py dy))
	   with x = (+ bx (* dx (1- (+ s ix))))
	   for i = (iter x y max)
	   do (plot win i ix py max)
	   if (/= i norm) return nil
	   finally (return t)))))

(defmacro z (base delta int)
  `(+ ,base (* ,delta ,int)))
(defun draw-square (square)
  (declare (mandel-square square))
  (let ((dx (ms-dr square))
	(dy (ms-di square))
	(base-x (ms-base-r square))
	(base-y (ms-base-i square))
	(maxiter (ms-maxiter square))
	(win (ms-win square))
	(x (ms-x square))
	(y (ms-y square))
	(s (ms-s square))
	)
    (declare (double-float dx dy base-x base-y)
	     (fixnum x y s maxiter))
          (cond
	    ((= s 2)
	     (plot win
		   (iter (z base-x dx (1+ x)) (z base-y dy (1+ y)) maxiter)
		   (1+ x) (1+ y) maxiter)
	     (plot win
		   (iter (z base-x dx (1+ x)) (z base-y dy y) maxiter)
		   (1+ x) y maxiter)
	     (plot win
		   (iter (z base-x dx x) (z base-y dy (1+ y)) maxiter)
		   x (1+ y) maxiter)
	     (plot win
		   (iter (z base-x dx x) (z base-y dy y) maxiter)
		   x y maxiter))
	    ((fill-square-p x y s base-x base-y dx dy maxiter win)
	     (fill-square win
			  (iter (z base-x dx x) (z base-y dy y) maxiter)
			  x y s maxiter))
	    (t (let ((new-s (/ s 2)))
		 (make-square win
			      x      y new-s
			      base-x base-y
			      dx dy
			      maxiter)
		 (make-square win
			      x (+ y new-s)  new-s
			      base-x base-y
			      dx dy
			      maxiter)
		 (make-square win
			      (+ x new-s) y  new-s
			      base-x base-y
			      dx dy
			      maxiter)
		 (make-square win
			      (+ x new-s)  (+ y new-s)  new-s
			      base-x base-y
			      dx dy
			      maxiter))))))

(defun create-zoom (win x y button)
  (setf (gethash win *zoom-table*)
	(make-instance 'zoomer
			 :x x :y y
			 :win win
			 :type (case button
				      (1 :zoom-same)
				      (2 :zoom-new)
				      (3 :zoom-out)))))

(defun update-zoom (win x y code)
  (declare (ignore code)
	   (fixnum x y))
  (let ((zoomer (gethash win *zoom-table*)))
    (when zoomer
      (let ((new-side (max 0
			   (- (the fixnum x) (the fixnum (start-x zoomer)))
			   (- (the fixnum y) (the fixnum (start-y zoomer))))))
	(let ((old-side (max 0
			     (- (the fixnum (stop-x zoomer))
				(the fixnum (start-x zoomer)))
			     (- (the fixnum (stop-y zoomer))
				(the fixnum (start-y zoomer))))))
	  (xlib:draw-rectangle win *zoomcolmap*
			       (the fixnum (start-x zoomer))
			       (the fixnum (start-y zoomer)) 
			       old-side old-side))
	(setf (stop-x zoomer) (max (the fixnum (start-x zoomer))
				   (the fixnum x)
				   ))
	(setf (stop-y zoomer) (max (the fixnum (start-y zoomer))
				   (the fixnum y)
				   ))
	(xlib:draw-rectangle win *zoomcolmap*
			     (the fixnum (start-x zoomer))
			     (the fixnum (start-y zoomer)) 
			     new-side new-side)
	(xlib:display-force-output *display*)))))

(defun finish-zoom (win x y code)
  (declare (ignore code))
  (let ((zoomer (gethash win *zoom-table*)))
    (setf (stop-x zoomer) x)
    (setf (stop-y zoomer) y)))

(defun do-zoom (win)
  (let ((zoomer (gethash win *zoom-table*)))
    (declare (zoomer zoomer))
    (setf (gethash win *zoom-table*) nil)
    (let ((dx (- (the fixnum (stop-x zoomer)) (the fixnum (start-x zoomer))))
	  (dy (- (the fixnum (stop-y zoomer)) (the fixnum (start-y zoomer))))
	  (sq (gethash win *winmap*)))
      (let ((side (max dx dy))
	    (x (the fixnum (start-x zoomer)))
	    (y (the fixnum (start-y zoomer)))
	    lx hx ly hy
	    )
	(if (< side 5)
	    (setf lx (+ (ms-base-r sq)
			(* (- x 128) (ms-dr sq)))
		  ly (+ (ms-base-i sq)
			(* (- y 128) (ms-di sq)))
		  hx (+ (ms-base-r sq)
			(* (+ x 128) (ms-dr sq)))
		  hy (+ (ms-base-i sq)
			(* (+ y 128) (ms-di sq))))
	    (setf lx (+ (ms-base-r sq)
			(* x (ms-dr sq)))
		  ly (+ (ms-base-i sq)
			(* y (ms-dr sq))) 
		  hx (+ (ms-base-r sq)
			(* (+ side x) (ms-dr sq)))
		  hy (+ (ms-base-i sq)
			(* (+ side y) (ms-dr sq)))))
;;;	(format t "DEBUG: zoomer is ~a~%~%" zoomer)
	(case (zoom-type zoomer)
	  (:zoom-new (new-window lx ly hx hy (ms-maxiter sq)))
	  (:zoom-same (empty-win *sysqueue* win)
		      (mandel-win win lx ly hx hy (ms-maxiter sq)))
	  (:zoom-out (empty-win *sysqueue* win)
		     (let ((br (ms-base-r sq))
			   (bi (ms-base-i sq))
			   (dr (ms-dr sq))
			   (di (ms-di sq)))
		       (mandel-win win
				   (- br (* 512 dr)) (- bi (* 512 di))
				   (+ (* 1024 dr) br) (+ (* 1024 di) bi)
				   (ms-maxiter sq))))
				 
	  (t (format t "Unknown/unimplemented zoom type ~a~%~%" (zoom-type zoomer))))))))

(defun quit-window (window)
  (let ((temp (gethash window (win-queues *sysqueue*))))
    (when temp
      (empty temp))))

(defun event-loop ()
  (init-colours)
  (do ((quit nil)
       (redisplay nil t))
      ((eq quit 'quit))
    (xlib:event-case (*display* :timeout 0)
       (:button-press (window x y code)
		      (create-zoom window x y code)
		      t)
       (:button-release (window x y code)
			(finish-zoom window x y code)
			(do-zoom window)
			t)
       (:motion-notify (window x y code)
		       (update-zoom window x y code)
		       t)
       (:exposure (window  x y width height count)
	    (let ((count count))
	      (declare (ignore count)
		      (fixnum x y width height))
	     (when redisplay
	       (repaint-window window x y (1- (+ x width)) (1- (+ y height)))))
	    t)
       (:key-press (window code)
	  (case (xlib:keysym->character
		 *display*
		 (xlib:keycode->keysym *display* code (make-shift-foo)))
	       (#\q (quit-window window))
	       (#\? (display-help))
	       ((:left-shift :right-shift)
		(push :shift *modstate*))
	       ((:left-control :right-control)
		(push :ctrl *modstate*))
	       (:character-set-switch
		(push :character-set-switch *modstate*)))
	  t)
       (:key-release (window code)
	  (let ((window window))
	    (declare (ignore window))
	    (case (xlib:keysym->character
		   *display*
		   (xlib:keycode->keysym *display* code 0))
	      (:character-set-switch
	       (setf *modstate* (delete :character-set-switch *modstate*)))
	      ((:left-control :right-control)
	       (setf *modstate* (delete :ctrl *modstate*)))
	      ((:left-shift :right-shift)
	       (setf *modstate* (delete :shift *modstate*)))))
	  t))
    (cond ((empty-p *sysqueue*)
	   nil)
	  (t (let ((square (dequeue *sysqueue*)))
	       (draw-square square))))))
