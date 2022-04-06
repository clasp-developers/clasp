(in-package #:koga)

(defun message (level control-string &rest args)
  "Display a message using ANSI highlighting if possible. LEVEL should be NIL, :ERR,
:WARN or :EMPH."
  (when (interactive-stream-p *standard-output*)
    (format t "~c[~dm" #\escape
            (case level
              (:err      31)
              (:warn     33)
              (:emph     32)
              (otherwise 0))))
  (apply #'format t control-string args)
  (when (interactive-stream-p *standard-output*)
    (format t "~c[0m" #\escape))
  (terpri *standard-output*)
  (when (eq level :err)
    (uiop:quit 1)))

(defun remove-flag (item flags)
  "Remove a flag from FLAGS"
  (let ((pos (search item flags)))
    (cond ((not pos)
           flags)
          ((zerop pos)
           (subseq flags (1+ (length item))))
          (t
           (concatenate 'string (subseq flags 0 (1- pos))
                                (subseq flags (+ pos (length item))))))))

(defun join-plists (plists)
  "Join a collection of plists into a single plist. Duplicated keys will accumulated
into a list of values and values that are lists will be appended."
  (loop with result-plist = nil
        for plist in plists
        do (loop for (key value) on plist by #'cddr
                 for current-value = (getf result-plist key)
                 if current-value
                   do (setf (cdr (last current-value))
                            (if (listp value)
                                value
                                (list value)))
                 else
                   do (setf (getf result-plist key)
                            (if (listp value)
                                value
                                (list value))))
        finally (return result-plist)))

(defun run-program (command &key directory)
  "Run a program in interactive mode."
  (uiop:run-program command
                    :directory directory
                    :output :interactive
                    :error-output :output))

(defun run-program-capture (command &key directory)
  "Run a program and capture the output as a string."
  (ignore-errors
    (multiple-value-bind (standard-output error-output code)
        (uiop:run-program command
                          :directory directory
                          :ignore-error-status t
                          :output '(:string :stripped t)
                          :error-output '(:string :stripped t))
      (declare (ignore error-output))
      (when (zerop code)
        standard-output))))

(defun git-commit (configuration &key short)
  "Get the current commit. SHORT specifies to use a short commit style."
  (run-program-capture (format nil "~A rev-parse~:[~; --short~] HEAD"
                               (git configuration) short)))

(defun git-describe (configuration)
  "Describe the current commit based on the most recent tag."
  (run-program-capture (format nil "~A describe --always"
                               (git configuration))))

(defun hidden-component-p (component)
  "Return non-NIL if a filename component starts with a period."
  (equal #\. (uiop:first-char component)))

(defun shebangp (path)
  "Return non-NIL if file has a shebang."
  (ignore-errors (and (probe-file path)
                      (with-open-file (stream path)
                        (and (equal #\# (read-char stream nil nil))
                             (equal #\! (read-char stream nil nil)))))))
