;;; -*- Mode: Lisp; Package: Demos -*-
;;;
;;; This file contains various graphics hacks written and ported over the
;;; years by various and numerous persons.
;;;
;;; This file should be portable to any valid Common Lisp with CLX -- DEC 88.
;;;

(defpackage :demos (:use :common-lisp)
  (:export do-all-demos demo))

(in-package :demos)


;;;; Graphic demos wrapper macro.

;;; This wrapper macro should be reconsidered with respect to its property
;;; list usage.  Possibly a demo structure should be used with *demos*
;;; pointing to these instead of function names.  Also, something should
;;; be done about a title window that displays the name of the demo while
;;; it is running.

(defparameter *demos* nil)

(defvar *display* nil)
(defvar *screen* nil)
(defvar *root* nil)
(defvar *black-pixel* nil)
(defvar *white-pixel* nil)
(defvar *window* nil)

(defmacro defdemo (fun-name demo-name args x y width height doc &rest forms)
  `(progn
     (defun ,fun-name ,args
       ,doc
       (unless *display*
	 #+:cmu
	 (multiple-value-setq (*display* *screen*) (ext:open-clx-display))
	 #+(or sbcl allegro clisp)
	 (progn
	   (setf *display* (xlib::open-default-display))
	   (setf *screen* (xlib:display-default-screen *display*)))
	 #-(or cmu sbcl allegro clisp)
	 (progn
	   ;; Portable method
	   (setf *display* (xlib:open-display (machine-instance)))
	   (setf *screen* (xlib:display-default-screen *display*)))
	 (setf *root* (xlib:screen-root *screen*))
	 (setf *black-pixel* (xlib:screen-black-pixel *screen*))
	 (setf *white-pixel* (xlib:screen-white-pixel *screen*)))
       (let ((*window* (xlib:create-window :parent *root*
					   :x ,x :y ,y
					   :event-mask nil
					   :width ,width :height ,height
					   :background *white-pixel*
					   :border *black-pixel*
					   :border-width 2
					   :override-redirect :on)))
	 (xlib:map-window *window*)
	 ;; 
	 ;; I hate to do this since this is not something any normal
	 ;; program should do ...
	 (setf (xlib:window-priority *window*) :above)
	 (xlib:display-finish-output *display*)
	 (unwind-protect
	      (progn ,@forms)
	   (xlib:unmap-window *window*)
	   (xlib:display-finish-output *display*))))
    (setf (get ',fun-name 'demo-name) ',demo-name)
    (setf (get ',fun-name 'demo-doc) ',doc)
    (export ',fun-name)
    (pushnew ',fun-name *demos*)
    ',fun-name))


;;;; Main entry points.

(defun do-all-demos ()
  (loop
   (dolist (demo *demos*)
     (funcall demo)
     (sleep 3))))

;;; DEMO is a hack to get by.  It should be based on creating a menu.  At
;;; that time, *name-to-function* should be deleted, since this mapping will
;;; be manifested in the menu slot name cross its action.  Also the
;;; "Shove-bounce" demo should be renamed to "Shove bounce"; likewise for
;;; "Fast-towers-of-Hanoi" and "Slow-towers-of-hanoi".
;;;

(defvar *name-to-function* (make-hash-table :test #'eq))
(defvar *keyword-package* (find-package "KEYWORD"))

(defun demo ()
  (macrolet ((read-demo ()
	       `(let ((*package* *keyword-package*))
		  (read))))
    (dolist (d *demos*)
      (setf (gethash (intern (string-upcase (get d 'demo-name))
			     *keyword-package*)
		     *name-to-function*)
	    d))
    (loop
      (fresh-line)
      (dolist (d *demos*)
	(write-string "   ")
	(write-line (get d 'demo-name)))
      (write-string "   ")
      (write-line "Help <demo name>")
      (write-string "   ")
      (write-line "Quit")
      (write-string "Enter demo name: ")
      (let ((demo (read-demo)))
	(case demo
	  (:help
	   (let* ((demo (read-demo))
		  (fun (gethash demo *name-to-function*)))
	     (fresh-line)
	     (if fun
		 (format t "~&~%~A~&~%" (get fun 'demo-doc))
		 (format t "Unknown demo name -- ~A." demo))))
	  (:quit (return t))
	  (t
	   (let ((fun (gethash demo *name-to-function*)))
	     (if fun
		 #+mp
		 (mp:make-process #'(lambda ()
				      (loop
				       (funcall fun)
				       (sleep 2)))
				  :name (format nil "~S" demo))
		 #-mp
		 (funcall fun)
		 (format t "~&~%Unknown demo name -- ~A.~&~%" demo)))))))))


;;;; Shared demo utilities.

(defun full-window-state (w)
  (xlib:with-state (w)
    (values (xlib:drawable-width w) (xlib:drawable-height w)
	    (xlib:drawable-x w) (xlib:drawable-y w)
	    (xlib:window-map-state w))))


;;;; Greynetic.

;;; GREYNETIC displays random sized and shaded boxes in a window.  This is
;;; real slow.  It needs work.
;;; 
(defun greynetic (window duration)
  (let* ((pixmap (xlib:create-pixmap :width 32 :height 32 :depth 1
				     :drawable window))
	 (gcontext (xlib:create-gcontext :drawable window
					 :background *white-pixel*
					 :foreground *black-pixel*
					 :tile pixmap
					 :fill-style :tiled)))
    (multiple-value-bind (width height) (full-window-state window)
      (dotimes (i duration)
	(let* ((pixmap-data (greynetic-pixmapper))
	       (image (xlib:create-image :width 32 :height 32
					 :depth 1 :data pixmap-data)))
	  (xlib:put-image pixmap gcontext image :x 0 :y 0 :width 32 :height 32)
	  (xlib:draw-rectangle window gcontext
			       (- (random width) 5)
			       (- (random height) 5)
			       (+ 4 (random (truncate width 3)))
			       (+ 4 (random (truncate height 3)))
			       t))
	(xlib:display-force-output *display*)))
    (xlib:free-gcontext gcontext)
    (xlib:free-pixmap pixmap)))

(defvar *greynetic-pixmap-array*
  (make-array '(32 32) :initial-element 0 :element-type 'xlib:pixel))

(defun greynetic-pixmapper ()
  (let ((pixmap-data *greynetic-pixmap-array*))
    (dotimes (i 4)
      (declare (fixnum i))
      (let ((nibble (random 16)))
	(setf nibble (logior nibble (ash nibble 4))
	      nibble (logior nibble (ash nibble 8))
	      nibble (logior nibble (ash nibble 12))
	      nibble (logior nibble (ash nibble 16)))
	(dotimes (j 32)
	  (let ((bit (if (logbitp j nibble) 1 0)))
	    (setf (aref pixmap-data i j) bit
		  (aref pixmap-data (+ 4 i) j) bit
		  (aref pixmap-data (+ 8 i) j) bit
		  (aref pixmap-data (+ 12 i) j) bit
		  (aref pixmap-data (+ 16 i) j) bit
		  (aref pixmap-data (+ 20 i) j) bit
		  (aref pixmap-data (+ 24 i) j) bit
		  (aref pixmap-data (+ 28 i) j) bit)))))
    pixmap-data))

#+nil
(defdemo greynetic-demo "Greynetic" (&optional (duration 300))
  100 100 600 600
  "Displays random grey rectangles."
  (greynetic *window* duration))


;;;; Qix.

(defstruct qix
  buffer
  (dx1 5)
  (dy1 10)
  (dx2 10)
  (dy2 5))

(defun construct-qix (length)
  (let ((qix (make-qix)))
    (setf (qix-buffer qix) (make-circular-list length))
    qix))

(defun make-circular-list (length)
  (let ((l (make-list length)))
    (rplacd (last l) l)))


(defun qix (window lengths duration)
  "Each length is the number of lines to put in a qix, and that many qix
  (of the correct size) are put up on the screen.  Lets the qix wander around
  the screen for Duration steps."
  (let ((histories (mapcar #'construct-qix lengths)))
    (multiple-value-bind (width height) (full-window-state window)
      (declare (fixnum width height))
      (xlib:clear-area window)
      (xlib:display-force-output *display*)
      (do ((h histories (cdr h))
	   (l lengths (cdr l)))
	  ((null h))
	(do ((x (qix-buffer (car h)) (cdr x))
	     (i 0 (1+ i)))
	    ((= i (car l)))
	  (rplaca x (make-array 4))))
      ;; Start each qix at a random spot on the screen.
      (dolist (h histories)
	(let ((x (random width))
	      (y (random height)))
	  (rplaca (qix-buffer h)
		  (make-array 4 :initial-contents (list x y x y)))))
      (rplacd (last histories) histories)
      (let ((x1 0) (y1 0) (x2 0) (y2 0)
	    (dx1 0) (dy1 0) (dx2 0) (dy2 0)
	    tem line next-line qix
	    (gc (xlib:create-gcontext :drawable window
				      :foreground *white-pixel*
				      :background *black-pixel*
				      :line-width 0 :line-style :solid
				      :function boole-c2)))
	(declare (fixnum x1 y1 x2 y2 dx1 dy1 dx2 dy2))
	(dotimes (i duration)
	  ;; Line is the next line in the next qix. Rotate this qix and
	  ;; the qix ring.
	  (setq qix (car histories))
	  (setq line (car (qix-buffer qix)))
	  (setq next-line (cadr (qix-buffer qix)))
	  (setf (qix-buffer qix) (cdr (qix-buffer qix)))
	  (setq histories (cdr histories))
	  (setf x1 (svref line 0))
	  (setf y1 (svref line 1))
	  (setf x2 (svref line 2))
	  (setf y2 (svref line 3))
	  (xlib:draw-line window gc x1 y1 x2 y2)
	  (setq dx1 (- (+ (qix-dx1 qix) (random 3)) 1))
	  (setq dy1 (- (+ (qix-dy1 qix) (random 3)) 1))
	  (setq dx2 (- (+ (qix-dx2 qix) (random 3)) 1))
	  (setq dy2 (- (+ (qix-dy2 qix) (random 3)) 1))
	  (cond ((> dx1 10) (setq dx1 10))
		((< dx1 -10) (setq dx1 -10)))
	  (cond ((> dy1 10) (setq dy1 10))
		((< dy1 -10) (setq dy1 -10)))
	  (cond ((> dx2 10) (setq dx2 10))
		((< dx2 -10) (setq dx2 -10)))
	  (cond ((> dy2 10) (setq dy2 10))
		((< dy2 -10) (setq dy2 -10)))
	  (cond ((or (>= (setq tem (+ x1 dx1)) width) (minusp tem))
		 (setq dx1 (- dx1))))
	  (cond ((or (>= (setq tem (+ x2 dx2)) width) (minusp tem))
		 (setq dx2 (- dx2))))
	  (cond ((or (>= (setq tem (+ y1 dy1)) height) (minusp tem))
		 (setq dy1 (- dy1))))
	  (cond ((or (>= (setq tem (+ y2 dy2)) height) (minusp tem))
		 (setq dy2 (- dy2))))
	  (setf (qix-dy2 qix) dy2)
	  (setf (qix-dx2 qix) dx2)
	  (setf (qix-dy1 qix) dy1)
	  (setf (qix-dx1 qix) dx1)
`	  (when (svref next-line 0)
	    (xlib:draw-line window gc
			    (svref next-line 0) (svref next-line 1)
			    (svref next-line 2) (svref next-line 3)))
	  (setf (svref next-line 0) (+ x1 dx1))
	  (setf (svref next-line 1) (+ y1 dy1))
	  (setf (svref next-line 2) (+ x2 dx2))
	  (setf (svref next-line 3) (+ y2 dy2))
	  (xlib:display-force-output *display*))))))


(defdemo qix-demo "Qix" (&optional (lengths '(30 30)) (duration 2000))
  0 0 700 700
  "Hypnotic wandering lines."
  (qix *window* lengths duration))



;;;; Petal.

;;; Fast sine constants:

(defconstant d360 #o5500)
(defconstant d270 #o4160)
(defconstant d180 #o2640)
(defconstant d90 #o1320)
(defconstant vecmax 2880)

(defparameter sin-array
  '#(#o0 #o435 #o1073 #o1531 #o2166 #o2623 #o3260 
     #o3714 #o4350 #o5003 #o5435 #o6066 #o6516 #o7145
     #o7573 #o10220 #o10644 #o11266 #o11706 #o12326 
     #o12743 #o13357 #o13771 #o14401 #o15007 #o15414
     #o16016 #o16416 #o17013 #o17407 #o20000 #o20366
     #o20752 #o21333 #o21711 #o22265 #o22636 #o23204
     #o23546 #o24106 #o24443 #o24774 #o25323 #o25645
     #o26165 #o26501 #o27011 #o27316 #o27617 #o30115
     #o30406 #o30674 #o31156 #o31434 #o31706 #o32154
     #o32416 #o32654 #o33106 #o33333 #o33554 #o33771
     #o34202 #o34406 #o34605 #o35000 #o35167 #o35351
     #o35526 #o35677 #o36043 #o36203 #o36336 #o36464
     #o36605 #o36721 #o37031 #o37134 #o37231 #o37322
     #o37407 #o37466 #o37540 #o37605 #o37646 #o37701
     #o37730 #o37751 #o37766 #o37775 #o40000))

(defmacro psin (val)
  `(let* ((val ,val)
	  neg
	  frac
	  sinlo)
     (if (>= val d180)
	 (setq neg t
	       val (- val d180)))
     (if (>= val d90)
	 (setq val (- d180 val)))
     (setq frac (logand val 7))
     (setq val (ash val -3))
     ;; 
     (setq sinlo (if (>= val 90)
		     (svref sin-array 90)
		     (svref sin-array val)))
     ;; 
     (if (< val 90)
	 (setq sinlo
	       (+ sinlo (ash (* frac (- (svref sin-array (1+ val)) sinlo))
			     -3))))
     ;; 
     (if neg
	 (- sinlo)
	 sinlo)))

(defmacro pcos (x)
  `(let ((tmp (- ,x d270)))
     (psin (if (minusp tmp) (+ tmp d360) tmp))))


;;;; Miscellaneous petal hackery.

(defmacro high-16bits-* (a b)
  `(let ((a-h (ash ,a -8))
	 (b-h (ash ,b -8)))
     (+ (* a-h b-h)
	(ash (* a-h (logand ,b 255)) -8)
	(ash (* b-h (logand ,a 255)) -8))))

(defun complete (style petal)
  (let ((repnum 1)
	factor cntval needed)
    (dotimes (i 3)
      (case i
	(0 (setq factor 2 cntval 6)) 
	(1 (setq factor 3 cntval 2))
	(2 (setq factor 5 cntval 1)))
      (do ()
	  ((or (minusp cntval) (not (zerop (rem style factor)))))
	(setq repnum (* repnum factor))
	(setq cntval (1- cntval))
	(setq style (floor style factor))))
    (setq needed (floor vecmax repnum))
    (if (and (not (oddp needed)) (oddp petal)) (floor needed 2) needed)))


;;;; Petal Parameters and Petal itself

(defparameter continuous t)
(defparameter styinc 2)
(defparameter petinc 1)
(defparameter scalfac-fac 8192)

(defun petal (petal-window &optional (how-many 10) (style 0) (petal 0))
  (let ((width 512)
	(height 512))
    (xlib:clear-area petal-window)
    (xlib:display-force-output *display*)
    (let ((veccnt 0)
	  (nustyle 722)
	  (nupetal 3)
	  (scalfac (1+ (floor scalfac-fac (min width height))))
	  (ctrx (floor width 2))
	  (ctry (floor height 2))
	  (tt 0)
	  (s 0)
	  (lststyle 0)
	  (lstpetal 0)
	  (petstyle 0)
	  (vectors 0)
	  (r 0)
	  (x1 0)
	  (y1 0)
	  (x2 0)
	  (y2 0)
	  (i 0)
	  (gc (xlib:create-gcontext :drawable petal-window
				    :foreground *black-pixel*
				    :background *white-pixel*
				    :line-width 0 :line-style :solid)))
      (loop
	(when (zerop veccnt)
	  (setq tt 0 s 0 lststyle style lstpetal petal petal nupetal
		style nustyle petstyle (rem (* petal style) d360)
		vectors (complete style petal))
	  (when continuous
	    (setq nupetal  (+ nupetal petinc)
		  nustyle (+ nustyle styinc)))
	  (when (or (/= lststyle style) (/= lstpetal petal))
	    (xlib:clear-area petal-window)
	    (xlib:display-force-output *display*)))
	(when (or (/= lststyle style) (/= lstpetal petal))
	  (setq veccnt (1+ veccnt) i veccnt x1 x2 y1 y2
		tt (rem (+ tt style) d360)
		s (rem (+ s petstyle) d360)
		r (pcos s))
	  (setq x2 (+ ctrx (floor (high-16bits-* (pcos tt) r) scalfac))
		y2 (+ ctry (floor (high-16bits-* (psin tt) r) scalfac)))
	  (when (/= i 1)
	    (xlib:draw-line petal-window gc x1 y1 x2 y2)
	    (xlib:display-force-output *display*)))
	(when (> veccnt vectors)
	  (setq veccnt 0)
	  (setq how-many (1- how-many))
	  (sleep 2)
	  (when (zerop how-many) (return)))))))

(defdemo petal-demo "Petal" (&optional (how-many 10) (style 0) (petal 0))
  100 100 512 512
  "Flower-like display."
  (petal *window* how-many style petal))


;;;; Hanoi.

;;; Random parameters:

(defparameter disk-thickness 15 "The thickness of a disk in pixels.")
(defparameter disk-spacing (+ disk-thickness 3)
  "The amount of vertical space used by a disk on a needle.")
(defvar *horizontal-velocity* 20 "The speed at which disks slide sideways.")
(defvar *vertical-velocity* 12 "The speed at which disks move up and down.")

;;; These variables are bound by the main function.

(defvar *hanoi-window* () "The window that Hanoi is happening on.")
(defvar *hanoi-window-height* () "The height of the viewport Hanoi is happening on.")
(defvar *transfer-height* () "The height at which disks are transferred.")
(defvar *hanoi-gcontext* () "The graphics context for Hanoi under X11.")

;;; Needle Functions

(defstruct disk
  size)

(defstruct needle
  position
  disk-stack)

;;; Needle-Top-Height returns the height of the top disk on NEEDLE.

(defun needle-top-height (needle)
  (- *hanoi-window-height*
     (* disk-spacing (length (the list (needle-disk-stack needle))))))

(defvar available-disks
  (do ((i 10 (+ i 10))
       (dlist () (cons (make-disk :size i) dlist)))
      ((> i 80) dlist)))

(defvar needle-1 (make-needle :position 184))
(defvar needle-2 (make-needle :position 382))
(defvar needle-3 (make-needle :position 584))

;;; Graphic interface abstraction:

;;; Invert-Rectangle calls the CLX function draw-rectangle with "fill-p"
;;; set to T.  Update-Screen forces the display output.
;;; 
(defmacro invert-rectangle (x y height width)
  `(xlib:draw-rectangle *hanoi-window* *hanoi-gcontext*
			,x ,y ,width ,height t))

(defmacro update-screen ()
  `(xlib:display-force-output *display*))


;;;; Moving disks up and down

;;; Slide-Up slides the image of a disk up from the coordinates X,
;;; START-Y to the point X, END-Y.  DISK-SIZE is the size of the disk to
;;; move.  START-Y must be greater than END-Y

(defun slide-up (start-y end-y x disk-size)
  (multiple-value-bind (number-moves pixels-left)
		       (truncate (- start-y end-y) *vertical-velocity*)
    (do ((x (- x disk-size))
	 (width (* disk-size 2))
	 (old-y start-y (- old-y *vertical-velocity*))
	 (new-y (- start-y *vertical-velocity*) (- new-y *vertical-velocity*))
	 (number-moves number-moves (1- number-moves)))
	((zerop number-moves)
	 (when (plusp pixels-left)
	   (invert-rectangle x (- old-y pixels-left) disk-thickness width)
	   (invert-rectangle x old-y disk-thickness width)
	   (update-screen)))
      ;; Loop body writes disk at new height & erases at old height.
      (invert-rectangle x old-y disk-thickness width)
      (invert-rectangle x new-y disk-thickness width)
      (update-screen))))

;;; Slide-Down slides the image of a disk down from the coordinates X,
;;; START-Y to the point X, END-Y.  DISK-SIZE is the size of the disk to
;;; move.  START-Y must be less than END-Y.

(defun slide-down (start-y end-y x disk-size)
  (multiple-value-bind (number-moves pixels-left)
		       (truncate (- end-y start-y) *vertical-velocity*)
    (do ((x (- x disk-size))
	 (width (* disk-size 2))
	 (old-y start-y (+ old-y *vertical-velocity*))
	 (new-y (+ start-y *vertical-velocity*) (+ new-y *vertical-velocity*))
	 (number-moves number-moves (1- number-moves)))
	((zerop number-moves)
	 (when (plusp pixels-left)
	   (invert-rectangle x (+ old-y pixels-left) disk-thickness width)
	   (invert-rectangle x old-y disk-thickness width)
	   (update-screen)))
      ;; Loop body writes disk at new height & erases at old height.
      (invert-rectangle X old-y disk-thickness width)
      (invert-rectangle X new-y disk-thickness width)
      (update-screen))))


;;;; Lifting and Droping Disks

;;; Lift-disk pops the top disk off of needle and raises it up to the
;;; transfer height.  The disk is returned.

(defun lift-disk (needle)
  "Pops the top disk off of NEEDLE, Lifts it above the needle, & returns it."
  (let* ((height (needle-top-height needle))
	 (disk (pop (needle-disk-stack needle))))
    (slide-up height
	      *transfer-height*
	      (needle-position needle)
	      (disk-size disk))
    disk))

;;; Drop-disk drops a disk positioned over needle at the transfer height
;;; onto needle.  The disk is pushed onto needle.

(defun drop-disk (disk needle)
  "DISK must be positioned above NEEDLE.  It is dropped onto NEEDLE."
  (push disk (needle-disk-stack needle))
  (slide-down *transfer-height*
	      (needle-top-height needle)
	      (needle-position needle)
	      (disk-size disk))
  t)


;;; Drop-initial-disk is the same as drop-disk except that the disk is
;;; drawn once before dropping.

(defun drop-initial-disk (disk needle)
  "DISK must be positioned above NEEDLE.  It is dropped onto NEEDLE."
  (let* ((size (disk-size disk))
	 (lx (- (needle-position needle) size)))
    (invert-rectangle lx *transfer-height* disk-thickness (* size 2))
    (push disk (needle-disk-stack needle))
    (slide-down *transfer-height*
		(needle-top-height needle)
		(needle-position needle)
		(disk-size disk))
    t))


;;;; Sliding Disks Right and Left

;;; Slide-Right slides the image of a disk located at START-X, Y to the
;;; position END-X, Y.  DISK-SIZE is the size of the disk.  START-X is
;;; less than END-X.

(defun slide-right (start-x end-x Y disk-size)
  (multiple-value-bind (number-moves pixels-left)
		       (truncate (- end-x start-x) *horizontal-velocity*)
    (do ((right-x (+ start-x disk-size) (+ right-x *horizontal-velocity*))
	 (left-x  (- start-x disk-size) (+ left-x  *horizontal-velocity*))
	 (number-moves number-moves (1- number-moves)))
	((zerop number-moves)
	 (when (plusp pixels-left)
	   (invert-rectangle right-x Y disk-thickness pixels-left)
	   (invert-rectangle left-x  Y disk-thickness pixels-left)
	   (update-screen)))
      ;; Loop body adds chunk *horizontal-velocity* pixels wide to right
      ;; side of disk, then chops off left side.
      (invert-rectangle right-x Y disk-thickness *horizontal-velocity*)
      (invert-rectangle left-x Y disk-thickness *horizontal-velocity*)
      (update-screen))))

;;; Slide-Left is the same as Slide-Right except that START-X is greater
;;; than END-X.

(defun slide-left (start-x end-x Y disk-size)
  (multiple-value-bind (number-moves pixels-left)
		       (truncate (- start-x end-x) *horizontal-velocity*)
    (do ((right-x (- (+ start-x disk-size) *horizontal-velocity*)
		  (- right-x *horizontal-velocity*))
	 (left-x  (- (- start-x disk-size) *horizontal-velocity*)
		  (- left-x  *horizontal-velocity*))
	 (number-moves number-moves (1- number-moves)))
	((zerop number-moves)
	 (when (plusp pixels-left)
	   (setq left-x  (- (+ left-x  *horizontal-velocity*) pixels-left))
	   (setq right-x (- (+ right-x *horizontal-velocity*) pixels-left))
	   (invert-rectangle left-x  Y disk-thickness pixels-left)
	   (invert-rectangle right-x Y disk-thickness pixels-left)
	   (update-screen)))
      ;; Loop body adds chunk *horizontal-velocity* pixels wide to left
      ;; side of disk, then chops off right side.
      (invert-rectangle left-x  Y disk-thickness *horizontal-velocity*)
      (invert-rectangle right-x Y disk-thickness *horizontal-velocity*)
      (update-screen))))


;;;; Transferring Disks

;;; Transfer disk slides a disk at the transfer height from a position
;;; over START-NEEDLE to a position over END-NEEDLE.  Modified disk is
;;; returned.

(defun transfer-disk (disk start-needle end-needle)
  "Moves DISK from a position over START-NEEDLE to a position over END-NEEDLE."
  (let ((start (needle-position start-needle))
	(end (needle-position end-needle)))
    (if (< start end)
	(slide-right start end *transfer-height* (disk-size disk))
	(slide-left start end *transfer-height* (disk-size disk)))
    disk))


;;; Move-One-Disk moves the top disk from START-NEEDLE to END-NEEDLE.

(defun move-one-disk (start-needle end-needle)
  "Moves the disk on top of START-NEEDLE to the top of END-NEEDLE."
  (drop-disk (transfer-disk (lift-disk start-needle)
			    start-needle
			    end-needle)
	     end-needle)
  t)

;;; Move-N-Disks moves the top N disks from START-NEEDLE to END-NEEDLE
;;; obeying the rules of the towers of hannoi problem.  To move the
;;; disks, a third needle, TEMP-NEEDLE, is needed for temporary storage.

(defun move-n-disks (n start-needle end-needle temp-needle)
  "Moves the top N disks from START-NEEDLE to END-NEEDLE.  
   Uses TEMP-NEEDLE for temporary storage."
  (cond ((= n 1)
	 (move-one-disk start-needle end-needle))
	(t
	 (move-n-disks (1- n) start-needle temp-needle end-needle)
	 (move-one-disk start-needle end-needle)
	 (move-n-disks (1- n) temp-needle end-needle start-needle)))
  t)


;;;; Hanoi itself.

(defun hanoi (window n)
  (multiple-value-bind (width height) (full-window-state window)
    (declare (ignore width))
    (let* ((*hanoi-window* window)
	   (*hanoi-window-height* height)
	   (*transfer-height* (- height (* disk-spacing n)))
	   (*hanoi-gcontext* (xlib:create-gcontext :drawable *hanoi-window*
						   :foreground *white-pixel*
						   :background *black-pixel*
						   :fill-style :solid
						   :function boole-c2)))
      (xlib:clear-area *hanoi-window*)
      (xlib:display-force-output *display*)
      (let ((needle-1 (make-needle :position 184))
	    (needle-2 (make-needle :position 382))
	    (needle-3 (make-needle :position 584)))
	(setf (needle-disk-stack needle-1) ())
	(setf (needle-disk-stack needle-2) ())
	(setf (needle-disk-stack needle-3) ())
	(do ((n n (1- n))
	     (available-disks available-disks (cdr available-disks)))
	    ((zerop n))
	  (drop-initial-disk (car available-disks) needle-1))
	(move-n-disks n needle-1 needle-3 needle-2)
	t))))

;;; Change the names of these when the DEMO loop isn't so stupid.
;;; 
(defdemo slow-hanoi-demo "Slow-towers-of-Hanoi" (&optional (how-many 4))
  0 100 768 300
  "Solves the Towers of Hanoi problem before your very eyes."
  (let ((*horizontal-velocity* 3)
	(*vertical-velocity* 1))
    (hanoi *window* how-many)))
;;;
(defdemo fast-hanoi-demo "Fast-towers-of-Hanoi" (&optional (how-many 7))
  0 100 768 300
  "Solves the Towers of Hanoi problem before your very eyes."
  (hanoi *window* how-many))



;;;; Bounce window.

;;; BOUNCE-WINDOW takes a window and seemingly drops it to the bottom of
;;; the screen.  Optionally, the window can have an initial x velocity,
;;; screen border elasticity, and gravity value.  The outer loop is
;;; entered the first time with the window at its initial height, but
;;; each iteration after this, the loop starts with the window at the
;;; bottom of the screen heading upward.  The inner loop, except for the
;;; first execution, carries the window up until the negative velocity
;;; becomes positive, carrying the window down to bottom when the
;;; velocity is positive.  Due to number lossage, ROUND'ing and
;;; TRUNC'ing when the velocity gets so small will cause the window to
;;; head upward with the same velocity over two iterations which will
;;; cause the window to bounce forever, so we have prev-neg-velocity and
;;; number-problems to check for this.  This is not crucial with the x
;;; velocity since the loop terminates as a function of the y velocity.
;;; 
(defun bounce-window (window &optional
			     (x-velocity 0) (elasticity 0.85) (gravity 2))
  (unless (< 0 elasticity 1)
    (error "Elasticity must be between 0 and 1."))
  (unless (plusp gravity)
    (error "Gravity must be positive."))
  (multiple-value-bind (width height x y mapped) (full-window-state window)
    (when (eq mapped :viewable)
      (let ((top-of-window-at-bottom (- (xlib:drawable-height *root*) height))
	    (left-of-window-at-right (- (xlib:drawable-width *root*) width))
	    (y-velocity 0)
	    (prev-neg-velocity most-negative-fixnum)
	    (number-problems nil))
	(declare (fixnum top-of-window-at-bottom left-of-window-at-right
			 y-velocity))
	(loop
	  (when (= prev-neg-velocity 0) (return t))
	  (let ((negative-velocity (minusp y-velocity)))
	    (loop
	      (let ((next-y (+ y y-velocity))
		    (next-y-velocity (+ y-velocity gravity)))
		(declare (fixnum next-y next-y-velocity))
		(when (> next-y top-of-window-at-bottom)
		  (cond
		   (number-problems
		    (setf y-velocity (incf prev-neg-velocity)))
		   (t
		    (setq y-velocity
			  (- (truncate (* elasticity y-velocity))))
		    (when (= y-velocity prev-neg-velocity)
		      (incf y-velocity)
		      (setf number-problems t))
		    (setf prev-neg-velocity y-velocity)))
		  (setf y top-of-window-at-bottom)
		  (setf (xlib:drawable-x window) x
			(xlib:drawable-y window) y)
		  (xlib:display-force-output *display*)
		  (return))
		(setq y-velocity next-y-velocity)
		(setq y next-y))
	      (when (and negative-velocity (>= y-velocity 0))
		(setf negative-velocity nil))
	      (let ((next-x (+ x x-velocity)))
		(declare (fixnum next-x))
		(when (or (> next-x left-of-window-at-right)
			  (< next-x 0))
		  (setq x-velocity (- (truncate (* elasticity x-velocity)))))
		(setq x next-x))
	      (setf (xlib:drawable-x window) x
		    (xlib:drawable-y window) y)
	      (xlib:display-force-output *display*))))))))

;;; Change the name of this when DEMO is not so stupid.
;;; 
(defdemo shove-bounce-demo "Shove-bounce" ()
  100 100 300 300
  "Drops the demo window with an inital X velocity which bounces off
  screen borders."
  (bounce-window *window* 30))

(defdemo bounce-demo "Bounce" ()
  100 100 300 300
  "Drops the demo window which bounces off screen borders."
  (bounce-window *window*))


;;;; Recurrence Demo

;;; Copyright (C) 1988 Michael O. Newton (newton@csvax.caltech.edu)

;;; Permission is granted to any individual or institution to use, copy,
;;; modify, and distribute this software, provided that this complete
;;; copyright and permission notice is maintained, intact, in all copies and
;;; supporting documentation.  

;;; The author provides this software "as is" without express or
;;; implied warranty.

;;; This routine plots the recurrence 
;;;      x <- y(1+sin(0.7x)) - 1.2(|x|)^.5
;;;      y <- .21 - x
;;; As described in a ?? 1983 issue of the Mathematical Intelligencer

(defun recurrence (display window &optional (point-count 10000))
  (let ((gc (xlib:create-gcontext :drawable window
				  :background *white-pixel*
				  :foreground *black-pixel*)))
    (multiple-value-bind (width height) (full-window-state window)
      (xlib:clear-area window)
      (draw-ppict window gc point-count 0.0 0.0 (* width 0.5) (* height 0.5))
      (xlib:display-force-output display)
      (sleep 4))
    (xlib:free-gcontext gc)))

;;; Draw points.  X assumes points are in the range of width x height,
;;; with 0,0 being upper left and 0,H being lower left.
;;; hw and hh are half-width and half-height of screen

(defun draw-ppict (win gc count x y hw hh)
  "Recursively draw pretty picture"
  (unless (zerop count)
    (let ((xf (floor (* (+ 1.0 x) hw ))) ;These lines center the picture
          (yf (floor (* (+ 0.7 y) hh ))))
      (xlib:draw-point win gc xf yf)
      (draw-ppict win gc (1- count) 
                  (- (* y (1+ (sin (* 0.7 x)))) (* 1.2 (sqrt (abs x))))
                  (- 0.21 x)
                  hw
                  hh))))

(defdemo recurrence-demo "Recurrence" ()
  10 10 700 700
  "Plots a cool recurrence relation."
  (recurrence *display* *window*))


;;;; Plaid

;;; 
;;; Translated from the X11 Plaid Demo written in C by Christopher Hoover.
;;; 

(defmacro rect-x (rects n)
  `(svref ,rects (ash ,n 2)))
(defmacro rect-y (rects n)
  `(svref ,rects (+ (ash ,n 2) 1)))
(defmacro rect-width (rects n)
  `(svref ,rects (+ (ash ,n 2) 2)))
(defmacro rect-height (rects n)
  `(svref ,rects (+ (ash ,n 2) 3)))

(defun plaid (display window &optional (num-iterations 10000) (num-rectangles 10))
  (let ((gcontext (xlib:create-gcontext :drawable window
					:function boole-c2
					:plane-mask (logxor *white-pixel*
							    *black-pixel*)
					:background *white-pixel*
					:foreground *black-pixel*
					:fill-style :solid))
	(rectangles (make-array (* 4 num-rectangles)
				:element-type 'number
				:initial-element 0)))
    (multiple-value-bind (width height) (full-window-state window)
      (let ((center-x (ash width -1))
	    (center-y (ash height -1))
	    (x-dir -2)
	    (y-dir -2)
	    (x-off 2)
	    (y-off 2))
	(dotimes (iter (truncate num-iterations num-rectangles))
	  (dotimes (i num-rectangles)
	    (setf (rect-x rectangles i) (- center-x x-off))
	    (setf (rect-y rectangles i) (- center-y y-off))
	    (setf (rect-width rectangles i) (ash x-off 1))
	    (setf (rect-height rectangles i) (ash y-off 1))
	    (incf x-off x-dir)
	    (incf y-off y-dir)
	    (when (or (<= x-off 0) (>= x-off center-x))
	      (decf x-off (ash x-dir 1))
	      (setf x-dir (- x-dir)))
	    (when (or (<= y-off 0) (>= y-off center-y))
	      (decf y-off (ash y-dir 1))
	      (setf y-dir (- y-dir))))
	  (xlib:draw-rectangles window gcontext rectangles t)
	  (xlib:display-force-output display))))
    (xlib:free-gcontext gcontext)))

(defdemo plaid-demo "Plaid" (&optional (iterations 10000) (num-rectangles 10))
  10 10 101 201
  "Plaid, man."
  (plaid *display* *window* iterations num-rectangles))


;;;; Bball demo

;;; 
;;; Ported to CLX by Blaine Burks
;;; 

(defvar *ball-size-x* 38)
(defvar *ball-size-y* 34)

(defmacro xor-ball (pixmap window gcontext x y)
  `(xlib:copy-area ,pixmap ,gcontext 0 0 *ball-size-x* *ball-size-y*
		   ,window ,x ,y))

(defconstant bball-gravity 1)
(defconstant maximum-x-drift 7)

(defvar *max-bball-x*)
(defvar *max-bball-y*)

(defstruct ball
  (x (random (- *max-bball-x* *ball-size-x*)))
  (y (random (- *max-bball-y* *ball-size-y*)))
  (dx (if (zerop (random 2)) (random maximum-x-drift)
	  (- (random maximum-x-drift))))
  (dy 0))

(defun get-bounce-image ()
  "Returns the pixmap to be bounced around the screen."
  (xlib::bitmap-image   #*000000000000000000000000000000000000
			#*000000000000000000000000000000000000
			#*000000000000000000001000000010000000
			#*000000000000000000000000000100000000
			#*000000000000000000000100001000000000
			#*000000000000000010000000010000000000
			#*000000000000000000100010000000000000
			#*000000000000000000001000000000000000
			#*000000000001111100000000000101010000
			#*000000000010000011000111000000000000
			#*000000000100000000111000000000000000
			#*000000000100000000000000000100000000
			#*000000000100000000001000100010000000
			#*000000111111100000010000000001000000
			#*000000111111100000100000100000100000
			#*000011111111111000000000000000000000
			#*001111111111111110000000100000000000
			#*001111111111111110000000000000000000
			#*011111111111111111000000000000000000
			#*011111111111111111000000000000000000
			#*111111111111110111100000000000000000
			#*111111111111111111100000000000000000
			#*111111111111111101100000000000000000
			#*111111111111111101100000000000000000
			#*111111111111111101100000000000000000
			#*111111111111111111100000000000000000
			#*111111111111110111100000000000000000
			#*011111111111111111000000000000000000
			#*011111111111011111000000000000000000
			#*001111111111111110000000000000000000
			#*001111111111111110000000000000000000
			#*000011111111111000000000000000000000
			#*000000111111100000000000000000000000
			#*000000000000000000000000000000000000))


(defun bounce-1-ball (pixmap window gcontext ball)
  (let ((x (ball-x ball))
	(y (ball-y ball))
	(dx (ball-dx ball))
	(dy (ball-dy ball)))
    (xor-ball pixmap window gcontext x y)
    (setq x (+ x dx))
    (setq y (+ y dy))
    (if (or (< x 0) (> x (- *max-bball-x* *ball-size-x*)))
	(setq x (- x dx)
	      dx (- dx)))
    (if (> y (- *max-bball-y* *ball-size-y*))
	(setq y (- y dy)
	      dy (- dy)))
    (setq dy (+ dy bball-gravity))
    (setf (ball-x ball) x)
    (setf (ball-y ball) y)
    (setf (ball-dx ball) dx)
    (setf (ball-dy ball) dy)
    (xor-ball pixmap window gcontext x y)))

(defun bounce-balls (display window how-many duration)
  (xlib:clear-area window)
  (xlib:display-force-output display)
  (multiple-value-bind (*max-bball-x* *max-bball-y*) (full-window-state window)
    (let* ((balls (do ((i 0 (1+ i))
		       (list () (cons (make-ball) list)))
		      ((= i how-many) list)))
	   (gcontext (xlib:create-gcontext :drawable window
					   :foreground *white-pixel*
					   :background *black-pixel*
					   :function boole-xor
					   :exposures :off))
	   (bounce-pixmap (xlib:create-pixmap :width 38 :height 34 :depth 1
					      :drawable window))
	   (pixmap-gc (xlib:create-gcontext :drawable bounce-pixmap
					    :foreground *white-pixel*
					    :background *black-pixel*)))
      (xlib:put-image bounce-pixmap pixmap-gc (get-bounce-image)
		      :x 0 :y 0 :width 38 :height 34)
      (xlib:free-gcontext pixmap-gc)
      (dolist (ball balls)
	(xor-ball bounce-pixmap window gcontext (ball-x ball) (ball-y ball)))
      (xlib:display-force-output display)
      (dotimes (i duration)
	(dolist (ball balls)
	  (bounce-1-ball bounce-pixmap window gcontext ball))
	(xlib:display-force-output display))
      (xlib:free-pixmap bounce-pixmap)
      (xlib:free-gcontext gcontext))))

#+nil
(defdemo bouncing-ball-demo "Bouncing-Ball" (&optional (how-many 5) (duration 500))
  34 34 700 500
  "Bouncing balls in space."
  (bounce-balls *display*  *window* how-many duration))
