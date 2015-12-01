(let ((lt (or *compile-file-truename* *load-truename*)))
  (setf *default-pathname-defaults* (make-pathname :name nil :type nil :defaults lt)))

(load "packages.lisp")
(in-package :cscrape)

(load "foundation.lisp")
(load "tags.lisp")
(load "compile-commands.lisp")
(load "sourcepos.lisp")
(load "interpret-tags.lisp")
(load "code-generator.lisp")

(defparameter *clang-path* nil)
(defparameter *tags* nil)
(defun do-scraping (args &key (run-preprocessor t))
  (declare (optimize (debug 3)))
  (let* ((*clang-path* (first args))
         (main-path (second args))
         (*default-pathname-defaults* (make-pathname :name nil :type nil :defaults main-path))
         (app-config-fn (third args))
         (all-commands-fn (fourth args))
         (cpp-commands-fn (fifth args))
         (app-config (read-application-config app-config-fn)))
    ;; Run C-preprocessor to generate the missing files
    (let ((all-cc (read-compile-commands all-commands-fn))
          (cpp-cc (read-compile-commands cpp-commands-fn)))
      (when run-preprocessor
        (update-cpps cpp-cc)
        (dolist (cc all-cc)
          (let ((ifile (merge-pathnames (cpp-name cc) *default-pathname-defaults*)))
            (unless (probe-file ifile)
              (format t "Could not find ~a~%" ifile)
              (run-cpp cc)))))
      (let ((tags (read-all-tags-all-compile-commands all-cc)))
        (setq *tags* tags)
        (interpret-tags tags)
        (generate-code tags main-path app-config)))))



(let ((args (cdr (member "--" sb-ext:*posix-argv* :test #'string=))))
  (format t "args: ~a~%" args)
  (do-scraping args))
(format t "Scraping done~%")

(sb-ext:quit)
