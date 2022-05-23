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
(cl:defun sys::load-extensions ()
  (unless (member :ignore-extensions *features*)
    (require :asdf)
    (loop with load-system = (or (ignore-errors (find-symbol "QUICKLOAD" :quicklisp))
                                 (find-symbol "LOAD-SYSTEM" :asdf))
          for system in core:*extension-systems*
          do (funcall load-system system))))

#+cclasp
(defun sys::call-initialize-hooks ()
  (loop for hook in core:*initialize-hooks*
        do (funcall hook)))

#+cclasp
(defun sys::call-terminate-hooks ()
  (loop for hook in core:*terminate-hooks*
        do (funcall hook)))

#+cclasp
(cl:defun sys::cclasp-snapshot-load-top-level ()
  (let ((core:*use-interpreter-for-eval* nil))
    (sys::cclasp-snapshot-load-foreign-libraries)
    (sys::call-initialize-hooks)
    (unwind-protect
        (progn
          (core:process-command-line-load-eval-sequence)
          (if (core:is-interactive-lisp)
              (core:top-level :noprint (core:noprint-p))
              (core:exit 0)))
      (sys::call-terminate-hooks))))

#+cclasp
(cl:defun sys::cclasp-top-level ()
  (let ((core:*use-interpreter-for-eval* nil))
    (core:maybe-load-clasprc)
    (sys::load-extensions)
    (sys::call-initialize-hooks)
    (unwind-protect
        (progn
          (core:process-command-line-load-eval-sequence)
          (if (core:is-interactive-lisp)
              (core:top-level :noprint (core:noprint-p))
              (core:exit 0)))
      (sys::call-terminate-hooks))))

#+cclasp
(eval-when (:load-toplevel)
  (cl:in-package :cl-user)
  (setf ext:*snapshot-save-load-startup* 'sys::cclasp-snapshot-load-top-level)
  (sys::cclasp-top-level))
