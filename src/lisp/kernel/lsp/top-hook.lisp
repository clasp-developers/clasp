(defun sys::load-foreign-libraries ()
  (when (find-package :cffi)
    (loop with list-foreign-libraries = (find-symbol "LIST-FOREIGN-LIBRARIES" :cffi)
          with load-foreign-library = (find-symbol "LOAD-FOREIGN-LIBRARY" :cffi)
          with foreign-library-name = (find-symbol "FOREIGN-LIBRARY-NAME" :cffi)
          for lib in (ignore-errors (funcall list-foreign-libraries :loaded-only nil))
          for name = (ignore-errors (funcall foreign-library-name lib))
          do (ignore-errors (funcall load-foreign-library name)))))

(defun sys::load-extensions ()
  (when (and core:*extension-systems*
             (notany (lambda (feature)
                       (member feature '(:ignore-extensions :ignore-extension-systems)))
                     *features*))
    (require :asdf)
    (loop with load-system = (or (ignore-errors (find-symbol "QUICKLOAD" :quicklisp))
                                 (find-symbol "LOAD-SYSTEM" :asdf))
          for system in core:*extension-systems*
          do (funcall load-system system))))

(defun sys::call-initialize-hooks ()
  (loop for hook in core:*initialize-hooks*
        do (funcall hook)))

(defun sys::call-terminate-hooks ()
  (loop for hook in core:*terminate-hooks*
        do (funcall hook)))

(defun sys::standard-toplevel ()
  (ext:lock-package "COMMON-LISP")
  (ext:lock-package "CORE")
  
  (let ((core:*use-interpreter-for-eval* nil))
    #-staging (when (ext:getenv "CLASP_AUTOCOMPILATION")
                (funcall 'ext:start-autocompilation))
    (case (core:startup-type)
      ((:snapshot-file :embedded-snapshot)
       (sys::load-foreign-libraries))
      (otherwise
       (core:maybe-load-clasprc)
       (sys::load-extensions)))
    (sys::call-initialize-hooks)
    (unwind-protect
        (progn
          (core:process-command-line-load-eval-sequence)
          (if (core:is-interactive-lisp)
              (core:top-level)
              (core:exit 0)))
      (sys::call-terminate-hooks))))

(setf ext:*toplevel-hook* 'sys::standard-toplevel)
