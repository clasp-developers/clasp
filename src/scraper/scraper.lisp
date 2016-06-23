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
(defvar *generated-headers-path*)
(defvar *use-multiprocessing-if-available* t)
(defparameter *clang-path* nil)
(defparameter *tags* nil)

(load "foundation.lisp")
(load "serialize.lisp")
(load "parse.lisp")
(load "tags.lisp")
(load "compile-commands.lisp")
(load "conditions.lisp")
(load "sourcepos.lisp")
(load "interpret-tags.lisp")
(load "format.lisp")
(load "csubst.lisp")
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
     for tags = (read-sif-file (sif-name cc))
     nconc tags))

(defparameter *classes* nil)
(defparameter *symbols* nil)
(defparameter *functions* nil)
(defparameter *enums* nil)
(defparameter *packages-to-create* nil)
(defparameter *application-config* #P"include/clasp/main/application.config")
(export '*application-config*)

(defun process-all-sif-files (clasp-home-path build-path sif-files)
  (declare (optimize debug))
  (let* ((tags (loop for sif-file in sif-files
                  for sif-tags = (read-sif-file sif-file)
                  nconc sif-tags))
         (app-config-path (merge-pathnames *application-config* (pathname clasp-home-path))))
    (format t "app-config-path: ~a~%" app-config-path)
    (let ((app-config (setup-application-config app-config-path)))
      (unless app-config
        (error "Could not get app-config"))
      (format t "Interpreting tags~%")
      (setf *tags* tags)
      (multiple-value-bind (packages-to-create functions symbols classes enums initializers)
          (interpret-tags tags)
        (setq *packages-to-create* packages-to-create)
        (setq *symbols* symbols)
        (setq *classes* classes)
        (setq *functions* functions)
        (setq *enums* enums)
        (format t "Generating code~%")
        (generate-code packages-to-create functions symbols classes enums initializers build-path app-config))
      (format t "Done scraping code~%"))))
(export 'process-all-sif-files)

(defun do-scraping (args &key (run-preprocessor t) regenerate-sifs)
  (declare (optimize (debug 3)))
  (format t "do-scraping args -> ~a~%" args)
  (let* ((*clang-path* (first args))
         (main-path (second args))
         (*default-pathname-defaults* (make-pathname :name nil :type nil :defaults main-path))
         (all-commands-fn (third args))
         (cpp-commands-fn (fourth args))
         (app-config (setup-application-config *application-config*)))
    (format t "do-scraping *default-pathname-defaults* -> ~a~%" *default-pathname-defaults*)
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
      ;; update of sif files is done in forked processes
      #+(or)(update-all-sif-files all-cc :regenerate-sifs regenerate-sifs)
      (format t "Reading sif files~%")
      (process-all-sif-files main-path (mapcar (lambda (cc) (sif-name cc)) all-cc)))))


;;; Helper function to generate one sif file from one .i file
(defun generate-one-sif (input output)
  (generate-sif-file input output))

(defun generate-headers-from-all-sifs ()
  (let* ((minus-minus-pos (position "--" sb-ext:*posix-argv* :test #'string=))
         (args (cdr (nthcdr minus-minus-pos sb-ext:*posix-argv*)))
         (build-path (car args))
         (clasp-home-path (cadr args))
         (main-path (caddr args))
         (main-path (merge-pathnames (pathname main-path) (pathname build-path)))
         (sif-files (cdddr args))
         (*default-pathname-defaults* (pathname build-path)))
    (format t "clasp-home-path: ~a~%" clasp-home-path)
    (format t "build-path: ~a~%" build-path)
    (format t "main-path: ~a~%" main-path)
    (format t "*default-pathname-defaults*: ~a~%" *default-pathname-defaults*)
    (format t "sif-files: ~a~%" sif-files)
    (process-all-sif-files clasp-home-path build-path sif-files)))
    
(export '(generate-one-sif generate-headers-from-all-sifs))

(defun legacy-scraper ()
  (let ((args (cdr (member "--" sb-ext:*posix-argv* :test #'string=))))
    (format t "args: ~a~%" args)
    (format t "*default-pathname-defaults* --> ~a~%" *default-pathname-defaults*)
    (do-scraping args))
  (sb-ext:quit))

(export '(legacy-scraper))

