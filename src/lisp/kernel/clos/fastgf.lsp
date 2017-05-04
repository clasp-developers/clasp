(in-package :clos)
(defvar *core-clos-generic-functions* (all-generic-functions))
(defvar *core-classes* )
(defun calculate-core-classes ()
  (let ((all-classes (clos::subclasses* (find-class t)))
        core-classes)
    (loop for class in all-classes
         
(print *package*)
(format t "There are ~a *core-classes*~&" (length *core-classes*))
(format t "There are ~a *core-clos-generic-functions*~&" (length *core-clos-generic-functions*))
