(in-package #:static-gfs)

;;; This tells various things in CLOS that they need to do special work.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :static-gfs *features*))
