#+eclasp
(eval-when (:load-toplevel)
  (cl:in-package :cl-user)
  (setf ext:*snapshot-save-load-startup* 'sys::cclasp-snapshot-load-top-level)
  (sys::cclasp-top-level))
