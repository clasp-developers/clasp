(in-package :cscrape)

(defun process-all-sif-files (app-config clasp-home-path sif-files &key use-precise)
  (declare (optimize debug))
  (format t "process-all-sif-files: ~s~%" sif-files)
  (format t "Interpreting tags~%")
  (multiple-value-bind (packages-to-create functions setf-functions symbols classes
                        gc-managed-types enums pregcstartups initializers exposes terminators
                        forwards)
      (interpret-tags (loop for sif-file in sif-files
                            if (probe-file sif-file)
                              do (format t "Processing sif file: ~s~%" sif-file) and
                              nconc (read-sif-file sif-file)
                             else
                               do (format t "There was no file: ~s~%" sif-file)))
    (generate-code packages-to-create functions setf-functions symbols classes gc-managed-types
                   enums pregcstartups initializers exposes terminators app-config forwards
                   :use-precise use-precise))
  (format t "Done scraping code~%"))

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
      (error "Clang returned with exit code: ~a~%the command was: ~{~a~^ ~}~%stdout: ~s~%stderr: ~s~%"
             exit-code command stdout stderr))
    (make-instance 'buffer-stream
                   :buffer stdout
                   :buffer-pathname output-file-path
                   :buffer-stream (make-string-input-stream stdout))))

(defun generate-sif-files (clasp-home-path clang-command-line &rest files)
  (unless files
    (setf files (uiop:command-line-arguments)))
  (with-standard-io-syntax
    (let ((*debug-io* (make-two-way-stream *standard-input* *standard-output*)))
      (flet ((maybe-relative-path (path)
               (if (starts-with-subseq clasp-home-path path)
                   (subseq path (length clasp-home-path))
                   path)))
        (loop
          ;; It expects the .cxx and .sif files interleaved as command line args
          :for (cxx-file sif-file) :on files :by #'cddr
          :do
          (format t "Scraping ~A -> ~A~%" (maybe-relative-path cxx-file) (maybe-relative-path sif-file))
          (let* ((clang-output (run-clang (append clang-command-line (list cxx-file)) sif-file))
                 (tags (process-all-recognition-elements clang-output)))
            (with-open-file (fout sif-file :direction :output :if-exists :supersede :external-format :utf-8)
              (prin1 tags fout))))))))

(defun generate-sif (out in)
  (let* ((input (alexandria:read-file-into-string in))
         (buffer-stream (make-instance 'buffer-stream
                                       :buffer input
                                       :buffer-pathname out
                                       :buffer-stream (make-string-input-stream input))))
    (write-sif-file (process-all-recognition-elements buffer-stream)
                    out)))

(defun generate-headers (use-precise config clasp-code-path clasp-sys-path &rest sif-files)
  (format t "clasp-code-path             -> ~a~%" clasp-code-path)
  (format t "clasp-sys-path             -> ~a~%" clasp-sys-path)
  (format t "*default-pathname-defaults* -> ~a~%" *default-pathname-defaults*)
  (assert (every 'uiop:directory-pathname-p (list clasp-code-path clasp-sys-path)))
  (assert sif-files)
  (let ((*clasp-code* clasp-code-path)
        (*clasp-sys* clasp-sys-path))
    (process-all-sif-files config clasp-code-path sif-files :use-precise use-precise)))

(export '(generate-sif-files generate-headers))
