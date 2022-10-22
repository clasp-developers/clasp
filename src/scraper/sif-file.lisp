(in-package :cscrape)

(defun pprint-tag (stream object)
  (pprint-logical-block (stream nil :prefix "{" :suffix "}")
    (write (type-of object) :stream stream)
    (loop with first-slot = t
          for slot in (closer-mop:class-slots (find-class (type-of object)))
          for initargs = (closer-mop:slot-definition-initargs slot)
          for initfunction = (closer-mop:slot-definition-initfunction slot)
          for name = (closer-mop:slot-definition-name slot)
          when (and initargs
                    (slot-boundp object name)
                    #+(or)(or (not initfunction)
                        (eql (slot-value object name)
                             (funcall initfunction))))
            do (write-char #\Space stream)
               (cond (first-slot
                      (setf first-slot nil)
                      (pprint-indent :current 0 stream))
                     (t
                      (pprint-newline :fill stream)))
               (write (first initargs) :stream stream)
               (write-char #\Space stream)
               (write (slot-value object name) :stream stream))))

(defun write-sif-file (tags output)
  (let ((*package* (find-package :tags))
        (*print-readably* t)
        (*print-pretty* t)
        (*print-case* :downcase)
        (*print-right-margin* 100)
        (*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch 'tags:tag #'pprint-tag)
    (ninja:with-timestamp-preserving-stream (stream output :external-format :utf-8)
      (loop for tag in tags
            do (write tag :stream stream)
               (fresh-line stream)))))

(defun generate-sif-file (input output)
  "* Arguments
- input :: A pathname.
- output :: A pathname.
* Description
Read the input .i file and extract the tags from it and write out a .sif file"
  (write-sif-file (process-all-recognition-elements (read-entire-file input))
                  output))

(defun read-sif-file (sif-file)
  "* Arguments
- compile-command :: The compile-command.
* Description
Read a list of tags from the sif file."
  (let* ((sif-pathname (pathname sif-file))
         (*readtable* (copy-readtable))
         (*package* (find-package :tags)))
    (set-macro-character #\{
                         (lambda (stream char)
                           (declare (ignore char))
                           (apply #'make-instance (read-delimited-list #\} stream t))))
    (set-macro-character #\} (get-macro-character #\) nil))
    (with-open-file (stream sif-pathname :direction :input :external-format :utf-8)
      (loop for tag = (read stream nil stream)
            until (eq tag stream)
            collect tag))))
