(in-package :cscrape)

(defvar *generated-headers-path*)
(defparameter *clang-path* nil)
(defparameter *tags* nil)
(defparameter *classes* nil)
(defparameter *gc-managed-types* nil)
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
      (multiple-value-bind (packages-to-create functions symbols classes gc-managed-types enums initializers)
          (interpret-tags tags)
        (setq *packages-to-create* packages-to-create)
        (setq *symbols* symbols)
        (setq *classes* classes)
        (setq *gc-managed-types* gc-managed-types)
        (setq *functions* functions)
        (setq *enums* enums)
        (format t "Generating code~%")
        (generate-code packages-to-create functions symbols classes gc-managed-types enums initializers build-path app-config))
      (format t "Done scraping code~%"))))
(export 'process-all-sif-files)

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
  (let ((*debug-io* (make-two-way-stream *standard-input* *standard-output*)))
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
