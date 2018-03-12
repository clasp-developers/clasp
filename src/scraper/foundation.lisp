(in-package :cscrape)

(defparameter *begin-tag* "BEGIN_TAG_bfc54f90bafadf5")
(defparameter *end-tag* "END_TAG_bfc54f90bafadf5")


(defun fill-config (config line)
  (let* ((trimmed (string-trim " " line))
         (var-start (position #\space trimmed))
         (data-start (position #\< trimmed :start var-start))
         (var (string-trim " " (subseq trimmed var-start data-start)))
         (data (string-trim " <>" (subseq trimmed data-start))))
    (setf (gethash (intern var :keyword) config) data)))

(defun read-application-config (filename)
  (let ((config (make-hash-table :test #'equal)))
    (with-open-file (fin filename :direction :input :external-format :utf-8)
      (loop for l = (read-line fin nil 'eof)
         until (eq l 'eof)
         for tl = (string-trim '(#\space #\tab) l)
         do (cond
              ((string= (subseq tl 0 7) "#define")
               (fill-config config tl))
              (t (error "Illegal application.config line: ~a" l)))))
    config))

(defun setup-application-config (filename)
  (let ((config (read-application-config filename)))
    #+(or)(progn
            (maphash (lambda (k v) (format t "key: ~s  value: ~s~%" k v)) config)
            (finish-output))
    config))
