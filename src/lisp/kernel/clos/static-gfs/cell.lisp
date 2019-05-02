(in-package #:static-gfs)

(defclass constructor-cell (clos:funcallable-standard-object)
  ((name :accessor cell-name) (keys :accessor cell-keys))
  (:metaclass clos:funcallable-standard-class))

;;; Note: no reader.
;;; we use a setf function because this was used earlier in constructor.lisp
;;; when a setf expansion here would have been unavailable.
(defun (setf cell-function) (function cell)
  (clos:set-funcallable-instance-function cell function)
  function)

(defun make-constructor-cell (name keys &optional (function nil functionp))
  ;; Avoid make-instance, as it would cause circularity issues.
  ;; We could just apply #'make-instance, but let's be a bit quicker about it.
  ;; SBCL uses "defstruct-with-alternate-metaclass" or something to make a kind
  ;; of funcallable struct, which is interesting but a bit beyond our capabilities...
  ;; and this should have the same effect, plus we get to use clos for the accessors.
  ;; (The only access in the fast path is calling the thing, anyway.)
  (let ((instance (allocate-instance (find-class 'constructor-cell))))
    (setf (cell-name instance) name)
    (setf (cell-keys instance) keys)
    (when functionp (setf (cell-function instance) function))
    instance))
