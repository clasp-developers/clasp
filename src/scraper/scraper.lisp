(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
;; This *is* in fact needed to avoid breaking extended characters.
;; Might mean that non-SBCL implementations should not be used for fear of breaking UTF8...
;; Source: Quickdocs server.
#+sbcl
(setf sb-impl::*default-external-format* :utf-8
      sb-alien::*default-c-string-external-format* :utf-8)

(let ((lt (or *compile-file-truename* *load-truename*)))
  (setf *default-pathname-defaults* (make-pathname :name nil :type nil :defaults lt)))

(load "packages.lisp")
(in-package :cscrape)

(defparameter *clang-path* nil)
(defparameter *tags* nil)

(load "foundation.lisp")
(load "serialize.lisp")
(load "parse.lisp")
(load "compile-commands.lisp")
(load "tags.lisp")
(load "conditions.lisp")
(load "sourcepos.lisp")
(load "interpret-tags.lisp")
(load "format.lisp")
(load "code-generator.lisp")

(defun update-all-sif-files (all-cc &key regenerate-sifs)
  "* Arguments
- all-cc :: A list of all compile-commands.
- regenerate-sifs :: Force regeneration of all sif files.
* Description
Update all of the scraped info files that need updating."
  (loop for cc in all-cc
     if (or regenerate-sifs (sif-file-needs-updating cc))
     do (update-sif-file cc)))

(defun read-all-tags-all-sif-files (all-cc)
  "* Arguments
- all-cc :: A list of all of the compile-commands.
* Description
Read all of the scraped info files and interpret their tags."
  (loop for cc in all-cc
     for tags = (read-sif-file cc)
     nconc tags))

(defparameter *classes* nil)
(defparameter *symbols* nil)
(defparameter *functions* nil)
(defparameter *enums* nil)
(defparameter *packages-to-create* nil)
(defun do-scraping (args &key (run-preprocessor t) regenerate-sifs)
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
        (format t "About to update all ~d missing preprocessor files~%" (length cpp-cc))
        (update-cpps cpp-cc)
        (dolist (cc all-cc)
          (let ((ifile (merge-pathnames (cpp-name cc) *default-pathname-defaults*)))
            (unless (probe-file ifile)
              (format t "Could not find ~a - regenerating it~%" ifile)
              (run-cpp cc 0)))))
      (update-all-sif-files all-cc :regenerate-sifs regenerate-sifs)
      (format t "Reading sif files~%")
      (let ((tags (read-all-tags-all-sif-files all-cc)))
        (format t "Interpreting tags~%")
        (setf *tags* tags)
        (multiple-value-bind (packages-to-create functions symbols classes enums)
            (interpret-tags tags)
          (setq *packages-to-create* packages-to-create)
          (setq *symbols* symbols)
          (setq *classes* classes)
          (setq *functions* functions)
          (setq *enums* enums)
          (format t "Generating code~%")
          (generate-code packages-to-create functions symbols classes enums main-path app-config))
        (format t "Done scraping code~%")))))


#-testing-scraper
(progn
  (let ((args (cdr (member "--" sb-ext:*posix-argv* :test #'string=))))
  (format t "args: ~a~%" args)
  (do-scraping args))
  (format t "Scraping done~%")
  (sb-ext:quit))
