(in-package :cscrape)


(defun gather-source-files (tags)
  (let ((source-files (make-hash-table :test #'equal)))
    (loop for tag in tags
         do (when (typep tag 'tags:source-tag-mixin)
              (let* ((source-file (tags:file tag))
                     (entries (gethash source-file source-files)))
                (push tag entries)
                (setf (gethash source-file source-files) entries))))
    (maphash (lambda (file entries)
               (let ((sorted-entries (sort entries #'< :key #'tags:line)))
                 (setf (gethash file source-files) sorted-entries)))
             source-files)
    source-files))


(defun calculate-character-offsets-one-file (source-file tags)
  (let ((cur-line 0)
        (char-offset 0)
        (prev-char-offset 0))
    (with-open-file (fin source-file :direction :input)
      (dolist (tag tags)
        (let ((search-line (tags:line tag)))
          (loop
             (if (>= cur-line search-line)
                 (progn
                   (setf (tags:character-offset tag) (1+ prev-char-offset))
                   (return nil))
                 (let ((l (read-line fin)))
                   (incf cur-line)
                   (setq prev-char-offset char-offset)
                   (setq char-offset (+ (length l) 1 char-offset))))))))))

(defun calculate-character-offsets (source-file-ht)
  (maphash (lambda (source-file tags)
             (calculate-character-offsets-one-file source-file tags))
           source-file-ht))
