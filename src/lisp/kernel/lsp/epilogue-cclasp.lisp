#+debug-lexical-var-reference-depth
(eval-when (:compile-toplevel)
  (report-lexical-var-reference-depth))

#+cclasp
(cl:defun sys::cclasp-snapshot-load-top-level ()
  (core:process-command-line-load-eval-sequence)
  (let ((core:*use-interpreter-for-eval* nil))
    (if (core:is-interactive-lisp)
        (core:top-level :noprint (core:noprint-p))
        (core:exit 0))))

#+cclasp
(cl:defun sys::cclasp-top-level ()
    (cl:in-package :cl-user)
  (let ((core:*use-interpreter-for-eval* nil))
    (core:process-extension-loads)
    (core:maybe-load-clasprc)
    (core:process-command-line-load-eval-sequence)
    (if (core:is-interactive-lisp)
        (core:top-level :noprint (core:noprint-p))
        (core:exit 0))))

#+cclasp
(eval-when (:load-toplevel)
  (cl:in-package :cl-user)
  (setf ext:*snapshot-save-load-startup* 'sys::cclasp-snapshot-load-top-level)
  (sys::cclasp-top-level))
