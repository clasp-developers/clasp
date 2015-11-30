(in-package :cscrape)

(defparameter *begin-tag* "BEGIN_TAG_bfc54f90bafadf5")
(defparameter *end-tag* "END_TAG_bfc54f90bafadf5")


(defun fill-config (config line)
  (let* ((trimmed (string-trim " " line))
         (var-start (position #\space trimmed))
         (data-start (position #\< trimmed :start var-start))
         (var (string-trim " " (subseq trimmed var-start data-start)))
         (data (string-trim " <>" (subseq trimmed data-start))))
    (setf (gethash var config) data)))
           
(defun read-application-config (filename)
  (let ((config (make-hash-table :test #'equal)))
    (with-open-file (fin filename :direction :input)
      (loop for l = (read-line fin nil 'eof)
         until (eq l 'eof)
         do (if (string= (subseq l 0 7) "#define")
                (fill-config config l)
                (error "Illegal application.config line: ~a" l))))
    config))

