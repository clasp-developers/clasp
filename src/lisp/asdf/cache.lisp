;;;; -------------------------------------------------------------------------
;;;; Stamp cache

(uiop/package:define-package :asdf/cache
  (:use :uiop/common-lisp :uiop :asdf/upgrade)
  (:export #:get-file-stamp #:compute-file-stamp #:register-file-stamp
           #:set-asdf-cache-entry #:unset-asdf-cache-entry #:consult-asdf-cache
           #:do-asdf-cache #:normalize-namestring
           #:call-with-asdf-cache #:with-asdf-cache #:*asdf-cache*))
(in-package :asdf/cache)

;;; This stamp cache is useful for:
;; * consistency of stamps used within a single run
;; * fewer accesses to the filesystem
;; * the ability to test with fake timestamps, without touching files

(with-upgradability ()
  (defvar *asdf-cache* nil)

  (defun set-asdf-cache-entry (key value-list)
    (apply 'values
           (if *asdf-cache*
               (setf (gethash key *asdf-cache*) value-list)
               value-list)))

  (defun unset-asdf-cache-entry (key)
    (when *asdf-cache*
      (remhash key *asdf-cache*)))

  (defun consult-asdf-cache (key &optional thunk)
    (if *asdf-cache*
        (multiple-value-bind (results foundp) (gethash key *asdf-cache*)
          (if foundp
              (apply 'values results)
              (set-asdf-cache-entry key (multiple-value-list (call-function thunk)))))
        (call-function thunk)))

  (defmacro do-asdf-cache (key &body body)
    `(consult-asdf-cache ,key #'(lambda () ,@body)))

  (defun call-with-asdf-cache (thunk &key override key)
    (let ((fun (if key #'(lambda () (consult-asdf-cache key thunk)) thunk)))
      (if (and *asdf-cache* (not override))
          (funcall fun)
          (let ((*asdf-cache* (make-hash-table :test 'equal)))
            (funcall fun)))))

  (defmacro with-asdf-cache ((&key key override) &body body)
    `(call-with-asdf-cache #'(lambda () ,@body) :override ,override :key ,key))

  (defun normalize-namestring (pathname)
    (let ((resolved (resolve-symlinks*
                     (ensure-absolute-pathname
                      (physicalize-pathname pathname)
                      'get-pathname-defaults))))
      (with-pathname-defaults () (namestring resolved))))

  (defun compute-file-stamp (normalized-namestring)
    (with-pathname-defaults ()
      (safe-file-write-date normalized-namestring)))

  (defun register-file-stamp (file &optional (stamp nil stampp))
    (let* ((namestring (normalize-namestring file))
           (stamp (if stampp stamp (compute-file-stamp namestring))))
      (set-asdf-cache-entry `(get-file-stamp ,namestring) (list stamp))))

  (defun get-file-stamp (file)
    (when file
      (let ((namestring (normalize-namestring file)))
        (do-asdf-cache `(get-file-stamp ,namestring) (compute-file-stamp namestring))))))

