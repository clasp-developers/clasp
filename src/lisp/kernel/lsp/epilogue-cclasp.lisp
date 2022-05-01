#+debug-lexical-var-reference-depth
(eval-when (:compile-toplevel)
  (report-lexical-var-reference-depth))

#+cclasp
(cl:defun sys::cclasp-snapshot-load-foreign-libraries ()
  (when (find-package :cffi)
    (loop with list-foreign-libraries = (find-symbol "LIST-FOREIGN-LIBRARIES" :cffi)
          with load-foreign-library = (find-symbol "LOAD-FOREIGN-LIBRARY" :cffi)
          with foreign-library-name = (find-symbol "FOREIGN-LIBRARY-NAME" :cffi)
          for lib in (ignore-errors (funcall list-foreign-libraries :loaded-only nil))
          for name = (ignore-errors (funcall foreign-library-name lib))
          do (ignore-errors (funcall load-foreign-library name)))))

#+cclasp
(cl:defun sys::cclasp-snapshot-load-top-level ()
  (sys::cclasp-snapshot-load-foreign-libraries)
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
