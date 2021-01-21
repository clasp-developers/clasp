(in-package #:static-gfs)

(defvar *reinitializers* (make-hash-table :test #'equal))

(defun ensure-reinitializer (keys)
  (ensure-gethash keys *reinitializers* (make-reinitializer keys)))

;; This is used in finalize-inheritance and make-instances-obsolete,
;; and is analogous to invalidate-generic-functions-with-class-selector.
;; FIXME?: Could act more specifically as with specializer-direct-gfs
(defun invalidate-class-reinitializers* (class)
  (let ((classes (clos:subclasses* class)))
    (maphash (lambda (k reinitializer)
               (declare (ignore k))
               (maybe-invalidate-reinitializer-wrt-classes
                reinitializer classes))
             *reinitializers*)))

(defun wipe-all-reinitializers ()
  (maphash (lambda (k reinitializer)
             (declare (ignore k))
             (wipe-reinitializer reinitializer))
           *reinitializers*))

