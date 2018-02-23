(in-package :cscrape)

(defun generate-sif-file (input output)
  "* Arguments
- input :: A pathname.
- output :: A pathname.
* Description
Read the input .i file and extract the tags from it and write out a .sif file"
  (let* ((bufs (read-entire-file input))
         (tags (process-all-recognition-elements bufs))
         (sif-pathname output))
    (with-open-file (fout sif-pathname :direction :output :if-exists :supersede)
      (let ((*print-readably* t)
            (*print-pretty* nil))
        (prin1 tags fout)))))

(defun read-sif-file (sif-file)
  "* Arguments
- compile-command :: The compile-command.
* Description
Read a list of tags from the sif file."
  (let* ((sif-pathname (pathname sif-file))
         (*readtable* (copy-readtable)))
    (set-macro-character
     #\{
     (lambda (str char)
       (declare (ignore char))
       (let ((list (read-delimited-list #\} str t)))
         (let ((type (first list))
               (list (second list)))
           (let ((class (allocate-instance (find-class type))))
             (loop for i in list do
                   (setf (slot-value class (car i)) (cdr i)))
             class)))))
    (with-open-file (fin sif-pathname :direction :input)
      (read fin))))
