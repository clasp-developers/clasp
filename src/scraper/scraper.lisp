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
(defparameter *clang-path* nil)
(defparameter *tags* nil)

(load "foundation.lisp")
(load "serialize.lisp")
(load "parse.lisp")
(load "tags.lisp")
(load "extract-tags.lisp")
(load "sif-file.lisp")
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
    (format t "app-config-path    -> ~a~%" app-config-path)
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
  (declare (optimize (speed 3)))
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


(defun report-clang-errors (file command error-string)
  (format *error-output* "Error >>>>>>>> ~a~%" error-string)
  #+(or)(with-input-from-string (sin error-string)
    (loop for line in (read-line sin)
       when (search "error" line)
       do (progn
            (format *error-output* "error line: ~a~%" line)
            (sb-sys:os-exit 1)))))

(defun slurp-clang-output (file command)
  (handler-case
      (let* ((exit-code nil)
             (output
              (with-output-to-string (str)
                (let ((strerror (with-output-to-string (strerror)
                                  (let* ((process (sb-ext:run-program "sh" (list "-c" command)
                                                                      :search t
                                                                      :output str
                                                                      :error strerror
                                                                      :external-format :latin1))
                                         (status (sb-ext:process-status process)))
                                    (setf exit-code (when (member status '(:exited :signaled))
                                                      (sb-ext:process-exit-code process)))))))
                  (when (> (length strerror) 5) (report-clang-errors file command strerror))))))
        (when (and exit-code
                   (/= exit-code 0))
          (error "Clang returned with exit code: ~A, error message: ~S, the command was: ~S"
                 exit-code output command))
        (make-instance 'buffer-stream
                       :buffer output
                       :buffer-pathname file
                       :buffer-stream (make-string-input-stream output)))
    (error (msg) (format *error-output* "An error occurred~%"))))

(defun generate-one-sif (clang-command output)
  (let (#+(or)(*debug-io* (make-two-way-stream *standard-input* *standard-output*)))
    (let* ((bufs (slurp-clang-output output clang-command)))
      (when bufs
        (let ((tags (process-all-recognition-elements bufs))
              (sif-pathname output))
          (with-open-file (fout sif-pathname :direction :output :if-exists :supersede)
            (let ((*print-readably* t)
                  (*print-pretty* nil))
              (prin1 tags fout))))))))

(defun directory-pathname-p (pathname)
  (and pathname
       (let ((pathname (pathname pathname)))
         (flet ((check-one (x)
                  (member x '(nil :unspecific) :test 'equal)))
           (and (not (wild-pathname-p pathname))
                (check-one (pathname-name pathname))
                (check-one (pathname-type pathname))
                t)))))

(defun generate-headers-from-all-sifs ()
  (let* ((minus-minus-pos (position "--" sb-ext:*posix-argv* :test #'string=))
         (args (subseq sb-ext:*posix-argv* (1+ minus-minus-pos))))
    (destructuring-bind
          (build-path clasp-home-path &rest sif-files)
        args
      (let ((*default-pathname-defaults* (pathname build-path)))
        (format t "clasp-home-path             -> ~a~%" clasp-home-path)
        (format t "build-path                  -> ~a~%" build-path)
        (format t "*default-pathname-defaults* -> ~a~%" *default-pathname-defaults*)
        (assert (every 'directory-pathname-p (list clasp-home-path build-path)))
        (assert sif-files)
        (process-all-sif-files clasp-home-path build-path sif-files)))))
    
(export '(generate-one-sif generate-headers-from-all-sifs))

(defun legacy-scraper ()
  (let ((args (cdr (member "--" sb-ext:*posix-argv* :test #'string=))))
    (format t "args: ~a~%" args)
    (format t "*default-pathname-defaults* --> ~a~%" *default-pathname-defaults*)
    (do-scraping args))
  (sb-ext:exit))

(export '(legacy-scraper))

