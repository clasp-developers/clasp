
(let ((lt *load-truename*))
  (setq *default-pathname-defaults* (make-pathname :name nil :type nil :defaults lt)))

(load "packages.lisp")
(in-package :cscrape)

(load "foundation.lisp")
(load "tags.lisp")
(load "compile-commands.lisp")
(load "sourcepos.lisp")
(load "interpret-tags.lisp")
(load "code-generator.lisp")

(defparameter *clang-path* nil)

(defun do-scraping (args)
  (declare (optimize (debug 3)))
  (let* ((*clang-path* (first args))
         (app-root (second args))
         (app-config-fn (third args))
         (all-commands-fn (fourth args))
         (cpp-commands-fn (fifth args))
         (app-config (read-application-config app-config-fn)))
    ;; Run C-preprocessor to generate the missing files
    (let ((all-cc (read-compile-commands all-commands-fn))
          (cpp-cc (read-compile-commands cpp-commands-fn)))
      (update-cpps cpp-cc)
      (dolist (cc all-cc)
        (unless (probe-file (cpp-name cc))
          (run-cpp cc)))
      (let ((tags (read-all-tags-all-compile-commands all-cc)))
        (interpret-tags tags)
        (generate-code tags app-root app-config)))))
        
(let ((args (cdr (member "--" sb-ext:*posix-argv* :test #'string=))))
  (format t "args: ~a~%" args)
  (do-scraping args))
(print "Done")

(sb-ext:quit)
