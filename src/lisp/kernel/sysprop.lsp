
(in-package :system)

(defvar *system-properties* (make-hash-table))

(defun l-put-sysprop (key area value)
  (multiple-value-bind (area-hash-table present-p)
      (gethash area *system-properties*)
    (if present-p
	(core::hash-table-setf-gethash area-hash-table key value)
	(let ((new-hash-table (make-hash-table)))
	  (core::hash-table-setf-gethash new-hash-table key value)
	  (core::hash-table-setf-gethash *system-properties* area new-hash-table)
	  value ))))

(defun l-get-sysprop (key area )
  (multiple-value-bind (area-hash-table present-p)
      (gethash area *system-properties*)
    (if present-p
	(gethash key area-hash-table)
	())))

(export '(put-sysprop get-sysprop))
