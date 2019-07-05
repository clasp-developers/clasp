
(defstruct code metadata)
(defstruct location line column scope)
(defstruct subprogram function-name linkage-name scope file)
(defstruct file filename directory)

(defstruct module
  (source-lines (make-array 1024 :adjustable t :element-type t :fill-pointer 0))
  (metadata (make-hash-table :test #'eql)))

(defparameter *code*
  (cl-ppcre:create-scanner
   "!dbg !([0-9]+)"))
(defparameter *dilocation-no-column*
  (cl-ppcre:create-scanner
   "^!([0-9]+) = !DILocation\\(line: ([0-9]+), scope: !([0-9]+)"))
(defparameter *dilocation*
  (cl-ppcre:create-scanner
   "^!([0-9]+) = !DILocation\\(line: ([0-9]+), column: ([0-9]+), scope: !([0-9]+)"))
(defparameter *disubprogram*
  (cl-ppcre:create-scanner
   "^!([0-9]+) = !DISubprogram\\(name: \"([^\"]*)\", linkageName: \"([^\"]*)\", scope: !([0-9]+), file: !([0-9]+), line: ([0-9]+)"))
(defparameter *distinct-disubprogram*
  (cl-ppcre:create-scanner
   "^!([0-9]+) = distinct !DISubprogram\\(name: \"([^\"]*)\", linkageName: \"([^\"]*)\", scope: !([0-9]+), file: !([0-9]+), line: ([0-9]+)")
  )
(defparameter *difile*
  (cl-ppcre:create-scanner
   "^!([0-9]+) = !DIFile\\(filename: \"([^\"]*)\", directory: \"([^\"]*)\""))


(defun parse-metadata (line module)
  (cond
    ((multiple-value-bind (match parts)
         (cl-ppcre:scan-to-strings *dilocation-no-column* line)
       (when match
         (let ((key (parse-integer (elt parts 0)))
               (line (parse-integer (elt parts 1)))
               (scope (parse-integer (elt parts 2))))
           (setf (gethash key (module-metadata module))
                 (make-location :line line :column 0 :scope scope)))
         t)))
    ((multiple-value-bind (match parts)
         (cl-ppcre:scan-to-strings *dilocation* line)
       (when match
         (let ((key (parse-integer (elt parts 0)))
               (line (parse-integer (elt parts 1)))
               (column (parse-integer (elt parts 2)))
               (scope (parse-integer (elt parts 3))))
           (setf (gethash key (module-metadata module))
                 (make-location :line line :column column :scope scope)))
         t)))
    ((multiple-value-bind (match parts)
         (cl-ppcre:scan-to-strings *distinct-disubprogram* line)
       (when match
         (let ((key (parse-integer (elt parts 0)))
               (function-name (elt parts 1))
               (linkage-name (elt parts 2))
               (scope (parse-integer (elt parts 3)))
               (file (parse-integer (elt parts 4))))
           (setf (gethash key (module-metadata module))
                 (make-subprogram :function-name function-name
                                  :linkage-name linkage-name
                                  :scope scope
                                  :file file)))
         t)))
    ((multiple-value-bind (match parts)
         (cl-ppcre:scan-to-strings *difile* line)
       (when match
         (let ((key (parse-integer (elt parts 0)))
               (filename (elt parts 1))
               (directory (elt parts 2)))
           (setf (gethash key (module-metadata module))
                 (make-file :filename filename
                            :directory directory)))
         t)))
    ((multiple-value-bind (match parts)
         (cl-ppcre:scan-to-strings *disubprogram* line)
       (when match
         (let ((key (parse-integer (elt parts 0)))
               (function-name (elt parts 1))
               (linkage-name (elt parts 2))
               (scope (parse-integer (elt parts 3)))
               (file (parse-integer (elt parts 4))))
           (setf (gethash key (module-metadata module))
                 (make-subprogram :function-name function-name
                                  :linkage-name linkage-name
                                  :scope scope
                                  :file file)))
         t)))
    (t)))

    
(defun parse-line (line module)
  (let ((first-char (elt line 0)))
    (cond
      ((char= first-char #\@)
       #|skip|#
       )
      ((char= first-char #\space)
       (multiple-value-bind (match parts)
           (cl-ppcre:scan-to-strings *code* line)
         (when match
           (let ((metadata (parse-integer (elt parts 0))))
             (vector-push-extend (make-code :metadata metadata) (module-source-lines module)))
           t)))
      ((char= first-char #\!)
       (parse-metadata line module))
      )))


(defun parse-file (fin)
  (let ((code (make-module)))
    (loop for line = (read-line fin nil :eof)
          until (eq line :eof)
          when (string/= line "")
               do (parse-line line code))
    code))


(defun describe-metadata (module metadata-key)
  (let ((metadata (gethash metadata-key (module-metadata module))))
    (etypecase metadata
      (location
       (format nil "line: ~a column: ~a ~a"
               (location-line metadata)
               (location-column metadata)
               (describe-metadata module (location-scope metadata))))
      (subprogram
       (format nil " subprogram: ~a ~a"
               (subprogram-function-name metadata)
               (describe-metadata module (subprogram-file metadata))))
      (file
       (format nil " file: ~a/~a"
               (file-directory metadata)
               (file-filename metadata))))))

(defun analyze-module (module &optional num)
  (let ((analysis (make-hash-table)))
    (loop for code across (module-source-lines module)
          do (incf (gethash (code-metadata code) analysis 0)))
    (let (counts)
      (maphash (lambda (metadata count)
                 (push (cons count metadata) counts))
               analysis)
      (let ((result (sort counts #'> :key #'car)))
        (loop for x in (subseq result 0 num)
              for count = (car x)
              for metadata-key = (cdr x)
              do (format t "count: ~a ~a~%" count (describe-metadata module metadata-key))
              )))))
