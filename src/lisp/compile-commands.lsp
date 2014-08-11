
(defstruct compile-project
  file-name
  recognize-compile-command
)

(defun extract-dir-command-file (line)
  (let ((parts (with-input-from-string (sin line)
                 (read sin))))
    (break "check parts")
    parts))

(defparameter *c* (make-compile-project :file-name "bjam.out"
                                        :recognize-compile-command "^\\s*\"clang[+][+]\".*\\s-c\\s.*"
                                        )
  )



(defun compile-project-load (proj)
  (let* ((regex (make-regex (compile-project-recognize-compile-command proj))))
    (with-open-file (stream (compile-project-file-name proj) :direction :input)
      (loop for line = (read-line stream nil 'done)
         until (eq line 'done)
         do (when (regex-matches regex line)
              (let ((match (regex-match regex line)))
                (multiple-value-bind (dir command file)
                    (extract-dir-command-file line)
                  (format t "directory: ~a~%" dir)
                  (format t "command: ~a~%" command)
                  (format t "file: ~a~%" file)
                  )
                ))
           ))
    )
  )
            
