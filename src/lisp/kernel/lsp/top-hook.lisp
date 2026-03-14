(in-package #:core)

(defun load-foreign-libraries ()
  (when (find-package :cffi)
    (loop with list-foreign-libraries = (find-symbol "LIST-FOREIGN-LIBRARIES" :cffi)
          with load-foreign-library = (find-symbol "LOAD-FOREIGN-LIBRARY" :cffi)
          with foreign-library-name = (find-symbol "FOREIGN-LIBRARY-NAME" :cffi)
          for lib in (ignore-errors (funcall list-foreign-libraries :loaded-only nil))
          for name = (ignore-errors (funcall foreign-library-name lib))
          do (ignore-errors (funcall load-foreign-library name)))))

(defun load-extensions ()
  (when (and *extension-systems*
             (notany (lambda (feature)
                       (member feature '(:ignore-extensions :ignore-extension-systems)))
                     *features*))
    (require :asdf)
    (loop with load-system = (or (ignore-errors (find-symbol "QUICKLOAD" :quicklisp))
                                 (find-symbol "LOAD-SYSTEM" :asdf))
          for system in *extension-systems*
          do (funcall load-system system))))

(defun call-initialize-hooks ()
  (loop for hook in *initialize-hooks*
        do (funcall hook)))

(defun call-terminate-hooks ()
  (loop for hook in *terminate-hooks*
        do (funcall hook)))

(defun maybe-load-clasprc ()
  "Maybe load the users startup code"
  (unless (no-rc-p)
    (let ((clasprc (rc-file-name)))
      (if (probe-file clasprc)
          (progn
            (unless (noinform-p)
              (format t "Loading resource file ~a~%" clasprc))
            (load-source clasprc))
          (unless (noinform-p)
            (format t "Resource file ~a not found, skipping loading of it.~%" clasprc))))))

(defun process-command-line-load-eval-sequence ()
  (loop for (cmd . arg) in (command-line-load-eval-sequence)
        do (ecase cmd
             (:load (load arg))
             (:script (load-source arg nil nil nil t))
             (:eval (eval (read-from-string arg))))))

(defun standard-toplevel ()
  (ext:lock-package "CORE")
  
  #-staging (when (ext:getenv "CLASP_AUTOCOMPILATION")
              (funcall 'ext:start-autocompilation))
  (case (startup-type)
    ((:snapshot-file :embedded-snapshot)
     (load-foreign-libraries))
    (otherwise
     (maybe-load-clasprc)
     (load-extensions)))
  (call-initialize-hooks)
  (unwind-protect
       (progn
         (process-command-line-load-eval-sequence)
         (if (is-interactive-lisp)
             (top-level)
             (exit 0)))
    (call-terminate-hooks)))

(setf ext:*toplevel-hook* 'standard-toplevel)
