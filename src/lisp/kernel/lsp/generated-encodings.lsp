(in-package :ext)

;;; file is loaded first, than compiled
(defvar *encoding-data* nil)

(defvar *encoding-cache* (make-hash-table))

(defun generate-encoding-hashtable (encoding)
  (let ((hash (gethash encoding *encoding-cache*)))
    (if hash
        hash
        (let ((spec (assoc encoding *encoding-data*)))
          (when spec
            (let ((table (make-hash-table)))
              (dolist (pair (rest spec))
                (let ((key (first pair))
                      (value (rest pair)))
                  (setf (gethash key table) value)
                  (setf (gethash value table) key)))
              (setf (gethash encoding *encoding-cache*) table)
              table))))))

;;; format per line :ISO-8859-2;0;0;
(defun process-encodings-file ()
  (setq *encoding-data* nil)
  (let ((file (translate-logical-pathname #P"SOURCE-DIR:tools-for-build;encodingdata.txt"))
        (old-encoding nil)
        (alist nil))
    (with-open-file (stream file :element-type 'character :direction :input :external-format :utf-8)
      (loop
        (let ((line (read-line stream nil :end)))
          (when (eq line :end)
            ;;; store the last encodings
            (when old-encoding
              (push (cons old-encoding (nreverse alist)) *encoding-data*))
            
            (return))
          (let* ((pos-semicolon-1 (position #\; line :test #'char=))
                 (encoding-string (subseq line 0 pos-semicolon-1))
                 (encoding (read-from-string encoding-string))
                 (pos-semicolon-2 (position #\; line :test #'char= :start (1+ pos-semicolon-1)))
                 (index (parse-integer (subseq line (1+ pos-semicolon-1) pos-semicolon-2)))
                 (pos-semicolon-3 (position #\; line :test #'char= :start (1+ pos-semicolon-2)))
                 (char-code (parse-integer (subseq line (1+ pos-semicolon-2) pos-semicolon-3))))
             ;;; Read a line
            (cond ((null old-encoding)
                   (setq old-encoding encoding)
                   (push (cons index (code-char char-code)) alist))
                  ((not (eq old-encoding encoding))
                    ;;; Store current data
                   (push (cons old-encoding (nreverse alist)) *encoding-data*)
                   (setq old-encoding encoding)
                    ;;; reset alist
                   (setq alist (list (cons index (code-char char-code)))))
                  (t
                   (push (cons index (code-char char-code)) alist)))))))
    (mapcar #'generate-encoding-hashtable (mapcar #'first *encoding-data*)))
  (values))

(eval-when (:compile-toplevel)
  (process-encodings-file))

(setq *encoding-cache* #.*encoding-cache*)
