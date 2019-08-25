
(defstruct code metadata)
(defstruct location line column scope inlined-at)
(defstruct subprogram function-name linkage-name scope file)
(defstruct file filename directory)

(defstruct module
  (source-lines (make-array 1024 :adjustable t :element-type t :fill-pointer 0))
  (metadata (make-hash-table :test #'eql))
  (inlined-at (make-hash-table :test #'eql)))

(defparameter *code*
  (cl-ppcre:create-scanner
   "!dbg !([0-9]+)"))
(defparameter *dilocation-no-column*
  (cl-ppcre:create-scanner
   "^!([0-9]+) = !DILocation\\(line: ([0-9]+), scope: !([0-9]+)"))
(defparameter *dilocation*
  (cl-ppcre:create-scanner
   "^!([0-9]+) = !DILocation\\(line: ([0-9]+), column: ([0-9]+), scope: !([0-9]+)"))
(defparameter *dilocation-inlined-at*
  (cl-ppcre:create-scanner
   "^!([0-9]+) = !DILocation\\(line: ([0-9]+), column: ([0-9]+), scope: !([0-9]+), inlinedAt: !([0-9]+)"))
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
         (cl-ppcre:scan-to-strings *dilocation-inlined-at* line)
       (when match
         (let* ((key (parse-integer (elt parts 0)))
                (line (parse-integer (elt parts 1)))
                (column (parse-integer (elt parts 2)))
                (scope (parse-integer (elt parts 3)))
                (inlined-at (parse-integer (elt parts 4)))
                (location (make-location :line line :column column :scope scope :inlined-at inlined-at)))
           (setf (gethash key (module-metadata module)) location
                 (gethash key (module-inlined-at module)) inlined-at))
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

(defun extract-source-location (module metadata-key)
  (let ((metadata (gethash metadata-key (module-metadata module))))
    (etypecase metadata
      (location
       (let* ((scope (gethash (location-scope metadata) (module-metadata module)))
              (file (gethash (subprogram-file scope) (module-metadata module)))
              (directory (file-directory file))
              (filename (file-filename file)))
         (values directory filename (location-line metadata) (location-column metadata)))))))

(defparameter *source-files* (make-hash-table :test #'equal))

(defun get-source-file (directory filename)
  (let ((source-code (gethash filename *source-files*)))
    (unless source-code
      (let ((code (with-open-file (fin (merge-pathnames (pathname filename)
                                                        (uiop:ensure-directory-pathname directory)) :direction :input)
                    (let ((buffer (make-string (file-length fin))))
                      (read-sequence buffer fin)
                      buffer))))
        (setf (gethash filename *source-files*) code)
        (setf source-code code)))
    source-code))

(defun read-to-line (sin lineno)
  "Return T if we can read to the lineno, otherwise if we hit EOF return NIL"
  (loop for l below (1- lineno)
        do (let ((line (read-line sin nil :eof)))
             (when (eq line :eof)
               (return-from read-to-line nil))))
  t)


(defun get-source-at-line-column (directory filename line column)
  (let* ((code (get-source-file directory filename))
         (sin (make-string-input-stream code)))
    (if (< line 99999)
        (let ((ok (read-to-line sin line)))
          (if ok
              (let ((pos (+ (file-position sin) (if (> column 2) (- column 2) 0))))
                (file-position sin pos)
                (read-line sin nil "HIT EOF"))
              "***** HIT EOF *****"))
        "********** BOGUS LINE ***********")))

(defun analyze-module (module &optional num func-num)
  (let ((analysis (make-hash-table :test #'eql))
        (function-count (make-hash-table :test #'equal)))
    (loop for code across (module-source-lines module)
;;          do (format t "code ~a~%" code)
          do (loop named inlined-at
                   for index = (code-metadata code)
                     then (progn
                            (gethash index (module-inlined-at module)))
                   for dilocation = (gethash index (module-metadata module))
                   for function-scope = (gethash (location-scope dilocation)
                                                 (module-metadata module))
                   do (unless index
                        (return-from inlined-at nil))
                      #+(or)(when (< (location-line dilocation) 9999)
                        (format t "dilocation ~a~%" dilocation)
                        (format t "function-scope ~a~%" function-scope))
                      (when (location-inlined-at dilocation)
                        (if (gethash (subprogram-function-name function-scope)
                                     function-count)
                            (incf (gethash (subprogram-function-name function-scope)
                                           function-count))
                            (setf (gethash (subprogram-function-name function-scope)
                                           function-count) 1)))
                      (if (gethash index analysis)
                          (incf (gethash index analysis))
                          (setf (gethash index analysis) 1))))
    (let (counts)
      (maphash (lambda (metadata count)
                 (push (cons count metadata) counts))
               analysis)
      (let ((result (sort counts #'> :key #'car)))
        (loop for x in result
              for count = (car x)
              for metadata-key = (cdr x)
              until (< count num)
              do (multiple-value-bind (directory filename line column)
                     (extract-source-location module metadata-key)
                   (let ((source (get-source-at-line-column directory filename line column)))
                     (unless (or (> line 65536) (= line 9995))
                       (format t "~10a ~a:~a:~a~%" count filename line column)
                       (format t "   ---> ~a~%" source)))))))
    (let (counts)
      (format t "Instructions generated by inlined functions~%")
      (maphash (lambda (name count)
                 (push (cons count name) counts))
               function-count)
      (let ((result (sort counts #'> :key #'car)))
        (loop for x in result
              for count = (car x)
              for name = (cdr x)
              until (< count func-num)
              do (format t "~10a ~a~%" count name))))))


(defun analyze-llvm-ir (filename &optional (num 100) (func-num 10))
  "Analyze the llvm-ir file and print the CL source locations that generate at least NUM lines of llvm-ir code"
  (let ((m (with-open-file (fin filename :direction :input)
             (parse-file fin))))
    (analyze-module m num func-num)))
