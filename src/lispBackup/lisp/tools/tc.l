
(core:defstruct rectangle
  (width 101)
  (height 102))

(print (apropos "rectangle"))

(defparameter *e* (make-rectangle))
(bformat t "Initialized rectangle *e* = width[%d] height[%d]\n" (rectangle-width *e*) (rectangle-height *e*))

(defparameter *r* (make-rectangle :width 50 :height 60))

(bformat t "Initialized rectangle *r* = width[%d] height[%d]\n" (rectangle-width *r*) (rectangle-height *r*))

(setf (rectangle-width *r*) 10)
(setf (rectangle-height *r*) 20)

(bformat t "After setf Rectangle *r* = width[%d] height[%d]\n" (rectangle-width *r*) (rectangle-height *r*))