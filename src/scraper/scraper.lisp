(let ((lt (or *compile-file-truename* *load-truename*)))
  (setf *default-pathname-defaults* (make-pathname :name nil :type nil :defaults lt)))

(load "packages.lisp")
(in-package :cscrape)

(defparameter *clang-path* nil)
(defparameter *tags* nil)

(load "foundation.lisp")
(load "serialize.lisp")
(load "tags.lisp")
(load "compile-commands.lisp")
(load "sourcepos.lisp")
(load "interpret-tags.lisp")
(load "code-generator.lisp")

(defun update-all-sif-files (all-cc)
  "* Arguments
- all-cc :: A list of all compile-commands.
* Description
Update all of the scraped info files that need updating."
  (loop for cc in all-cc
     if (sif-file-needs-updating cc)
     do (update-sif-file cc)))

(defun read-all-tags-all-sif-files (all-cc)
  "* Arguments
- all-cc :: A list of all of the compile-commands.
* Description
Read all of the scraped info files and interpret their tags."
  (loop for cc in all-cc
     for tags = (read-sif-file cc)
     nconc tags))

(defparameter *exposed-classes* nil)
(defun do-scraping (args &key (run-preprocessor t))
  (declare (optimize (debug 3)))
  (let* ((*clang-path* (first args))
         (main-path (second args))
         (*default-pathname-defaults* (make-pathname :name nil :type nil :defaults main-path))
         (all-commands-fn (third args))
         (cpp-commands-fn (fourth args))
         (app-config (setup-application-config)))
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
      (update-all-sif-files all-cc)
      (format t "Reading sif files~%")
      (let ((tags (read-all-tags-all-sif-files all-cc)))
        (format t "Interpreting tags~%")
        (setf *tags* tags)
        (interpret-tags tags)
        (let ((exposed-classes (interpret-exposed-classes tags)))
          (setf *exposed-classes* exposed-classes)
          (format t "Generating code~%")
          (generate-code tags exposed-classes main-path app-config))
        (format t "Done scraping code~%")))))


#-testing-scraper
(progn
  (let ((args (cdr (member "--" sb-ext:*posix-argv* :test #'string=))))
  (format t "args: ~a~%" args)
  (do-scraping args))
  (format t "Scraping done~%")
  (sb-ext:quit))
