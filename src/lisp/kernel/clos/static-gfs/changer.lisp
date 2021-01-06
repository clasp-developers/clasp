(in-package #:static-gfs)

;;; FIXME: Should be weak-key
(defvar *changer-cells* (make-hash-table :test #'eq))

(defun ensure-changer-classd-table (classd)
  (ensure-gethash classd *changer-cells*
                  (make-hash-table :test #'equal)))

(defun ensure-class-changer (classd keys)
  (ensure-gethash keys (ensure-changer-classd-table classd)
                  (make-class-changer classd keys)))

(defun map-changers (function designator)
  (let ((table (gethash designator *changer-cells*)))
    (when table
      (maphash (lambda (key value)
                 (declare (ignore key))
                 (funcall function value))
               table))))

(defun map-all-changers (function)
  (maphash (lambda (classd table)
             (declare (ignore classd))
             (maphash (lambda (keys changer)
                        (declare (ignore keys))
                        (funcall function changer))
                      table))
           *changer-cells*))

;;; Called from (setf find-class).
(defun invalidate-designated-changers (designator)
  (map-changers #'invalidate-changer designator))

;;; Called below.
(defun invalidate-class-changers (class)
  (invalidate-designated-changers class)
  (let ((name (proper-class-name class)))
    (when name (invalidate-designated-changers class))))

;;; Called from finalize-inheritance and make-instances-obsolete.
(defun invalidate-changers* (invalidating-class)
  (maphash (lambda (classd table)
             (let ((class (etypecase classd
                            (symbol (find-class classd nil))
                            (class classd))))
               (when class
                 (if (core:subclassp class invalidating-class)
                     ;; These changers are TO the class (or a subclass),
                     ;; so we must fully invalidate them.
                     (maphash (lambda (k changer)
                                (declare (ignore k))
                                (invalidate-changer changer))
                              table)
                     ;; These changers may still have the class in their call
                     ;; histories, so deal with that.
                     (maphash (lambda (k changer)
                                (declare (ignore k))
                                (changer-maybe-cut-wrt-class
                                 changer invalidating-class)))))))
           *changer-cells*))
