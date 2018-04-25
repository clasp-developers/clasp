(in-package :cscrape)

(defparameter *tags* nil)
(defparameter *classes* nil)
(defparameter *gc-managed-types* nil)
(defparameter *symbols* nil)
(defparameter *functions* nil)
(defparameter *enums* nil)
(defparameter *packages-to-create* nil)

(defun process-all-sif-files (clasp-home-path build-path sif-files)
  (declare (optimize debug))
  (let* ((tags (loop for sif-file in sif-files
                  for sif-tags = (read-sif-file sif-file)
                  nconc sif-tags))
         (app-config-path (merge-pathnames #P"include/clasp/main/application.config" (pathname clasp-home-path))))
    (format t "app-config-path    -> ~a~%" app-config-path)
    (let ((app-config (read-application-config app-config-path)))
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

(defun run-clang (command output-file-path)
  (multiple-value-bind
        (stdout stderr exit-code)
      (uiop:run-program command
                        :output :string
                        :error-output :string
                        :ignore-error-status t
                        :external-format :latin1)
    #+(or)
    (when (> (length stderr) 5)
      (with-input-from-string (sin stderr)
        (loop for line in (read-line sin)
              when (search "error" line)
              do (progn
                   (format *error-output* "error line: ~a~%" line)
                   (sb-sys:os-exit 1)))))
    (unless (eql exit-code 0)
      (error "Clang ded")
      #+(or)
      (error "Clang returned with exit code: ~A~%the command was: ~{~A~^ ~}~%"
             exit-code command))
    (make-instance 'buffer-stream
                   :buffer stdout
                   :buffer-pathname output-file-path
                   :buffer-stream (make-string-input-stream stdout))))

(defun generate-sif-files (clang-command-line &rest files)
  (unless files
    (setf files (uiop:command-line-arguments)))
  (with-standard-io-syntax
    (let ((*debug-io* (make-two-way-stream *standard-input* *standard-output*)))
      (loop
        ;; It expects the .cxx and .sif files interleaved as command line args
        :for (cxx-file sif-file) :on files :by #'cddr
        :do
        (format t "Scraping ~A -> ~A~%" cxx-file sif-file)
        (let* ((clang-output (run-clang (append clang-command-line (list cxx-file)) sif-file))
               (tags (process-all-recognition-elements clang-output)))
          (with-open-file (fout sif-file :direction :output :if-exists :supersede :external-format :utf-8)
            (prin1 tags fout)))))))

(defun generate-headers-from-all-sifs ()
  (destructuring-bind
        (build-path clasp-home-path &rest sif-files)
      (uiop:command-line-arguments)
    (declare (ignore lisp-binary))
    (let ((*default-pathname-defaults* (pathname build-path)))
      (format t "clasp-home-path             -> ~a~%" clasp-home-path)
      (format t "build-path                  -> ~a~%" build-path)
      (format t "*default-pathname-defaults* -> ~a~%" *default-pathname-defaults*)
      (assert (every 'uiop:directory-pathname-p (list clasp-home-path build-path)))
      (assert sif-files)
      (process-all-sif-files clasp-home-path build-path sif-files))))

(export '(generate-sif-files generate-headers-from-all-sifs))
