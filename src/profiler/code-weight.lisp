
(defstruct code metadata)
(defstruct location line column scope inlined-at)
(defstruct subprogram function-name linkage-name scope file)
(defstruct file filename directory)

(defparameter *total-instructions* 0)
(defstruct module
  (source-lines (make-array 1024 :adjustable t :element-type t :fill-pointer 0))
  (metadata (make-hash-table :test #'eql))
  (inlined-at (make-hash-table :test #'eql))
  (current-function-name nil)
  (current-function-count 0)
  (total-count 0)
  (function-sizes (make-hash-table :test #'equal)))

(defparameter *inlined-into*
  (cl-ppcre:create-scanner
   "inlined-into \"([^\"]+)\" \"([^\"]+)\" ([0-9]+)"))

(defparameter *function-start*
  (cl-ppcre:create-scanner
   "^define.*@([\"\-a-zA-Z0-9_]+)[(].*{$"))
(defparameter *code*
  (cl-ppcre:create-scanner
   "!dbg !([0-9]+)"))
(defparameter *function-end*
  (cl-ppcre:create-scanner
   "^}$"))
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
      ((multiple-value-bind (match parts)
           (cl-ppcre:scan-to-strings *function-start* line)
         (when match
           (let* ((name (string-trim "\"" (elt parts 0))))
             (setf (module-current-function-name module) name
                   (module-current-function-count module) 0))
           t)))
      ((char= first-char #\space)
       (multiple-value-bind (match parts)
           (cl-ppcre:scan-to-strings *code* line)
         (when match
           (incf *total-instructions*)
           (let ((metadata (parse-integer (elt parts 0))))
             (vector-push-extend (make-code :metadata metadata) (module-source-lines module)))
           (incf (module-current-function-count module))
           t)))
      ((char= first-char #\})
       (setf (gethash (module-current-function-name module) (module-function-sizes module)) (module-current-function-count module)
             (module-current-function-count module) 0))
      ((char= first-char #\!)
       (parse-metadata line module))
      )))


(defun parse-file (fin)
  (let ((module (make-module)))
    (loop for line = (read-line fin nil :eof)
          until (eq line :eof)
          when (string/= line "")
            do (parse-line line module))
    module))


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

(defun analyze-module (total-instructions-by-function module)
    (loop for code across (module-source-lines module)
          do (loop named inlined-at
                   for index = (code-metadata code) then (gethash index (module-inlined-at module))
                   while index
                   do (let* ((dilocation (gethash index (module-metadata module)))
                             (function-scope (gethash (location-scope dilocation) (module-metadata module))))
;;;                        (format t "Inlined function: ~a~%" (subprogram-function-name function-scope))
                        (when (location-inlined-at dilocation)
                          (incf (gethash (subprogram-function-name function-scope) total-instructions-by-function 0))))))
    total-instructions-by-function)

#|
              
      until (< count num)
      do (multiple-value-bind (directory filename line column)
                     (extract-source-location module metadata-key)
                   (let ((source (get-source-at-line-column directory filename line column)))
                     (unless (or (> line 65536) (= line 9995))
                       (format t "~10a ~a:~a:~a~%" count filename line column)
                       (format t "   ---> ~a~%" source)))))))
|#

(defstruct inlined-info name total-instructions inlined-times)

(defun generate-report (total-instructions-by-function inlined-times total-instructions func-num)
  (format t "Instructions generated by inlined functions~%")
  (let (counts)
    (maphash (lambda (name instruction-count)
               (let ((inlined-times (gethash name inlined-times)))
                 (push (make-inlined-info :name name
                                          :total-instructions instruction-count
                                          :inlined-times inlined-times)
                       counts)))
             total-instructions-by-function)
    (format t "~20a ~16a ~16a ~a~%" "Total" "times" "#instructions/" "name")
    (format t "~20a ~16a ~16a ~a~%" "instructions" "inlined" "times-inlined" "")
    (let ((result (sort counts #'> :key #'inlined-info-total-instructions)))
      (loop for x in result
            for count = (inlined-info-total-instructions x)
            for name = (inlined-info-name x)
            for function-count = (inlined-info-inlined-times x)
            until (< count func-num)
            do (let* ((number-of-inlines (if function-count
                                             (float (/ count function-count))
                                             "-"))
                      (function-count-string (if function-count
                                                 (format nil "~a" function-count)
                                                 "-")))
                 (format t "~20a ~16a ~16a ~a~%" count function-count-string (format nil "~,1f" number-of-inlines) name)))
          (format t "~10a  Total lines of llvm-IR~%" total-instructions)
      result)))


(defun load-inline-counts (info-file tree)
  (with-open-file (fin info-file :direction :input)
    (loop for line = (read-line fin nil :eof)
          until (eq line :eof)
          do (multiple-value-bind (match parts)
                 (cl-ppcre:scan-to-strings *inlined-into*  line)
               (when match
                 (let ((outer (elt parts 0))
                       (inner (elt parts 1))
                       (count (parse-integer (elt parts 2))))
                   (let* ((inner-alist (gethash outer tree))
                          (exists (assoc inner inner-alist :test #'string=)))
                     (when exists
                       (warn "In ~a~%~a inlines ~a already -> ~a : new count ~a~%" info-file outer inner exists count))
                     (push (cons inner count) inner-alist)
                     (setf (gethash outer tree) inner-alist)))
                 t)))))


(defun count-times-inlined (tree outer inner)
  (let ((inner-alist (gethash outer tree))
        (times 0))
    (when inner-alist
      (mapc (lambda (pair)
              (let ((tinner (car pair))
                    (count (cdr pair)))
                (cond
                  ((string= inner tinner)
                   (incf times count))
                  (t
                   (let ((total-count (count-times-inlined tree tinner inner)))
                     (incf times (* count total-count)))))))
            inner-alist))
    times))

(defun gather-inlined-functions (tree)
  (let ((inlined-functions (make-hash-table :test #'equal)))
    (maphash (lambda (outer inner-alist)
               (declare (ignore outer))
               (mapcar (lambda (pair)
                         (setf (gethash (car pair) inlined-functions) 0))
                        inner-alist))
             tree)
    inlined-functions))

(defun count-times-functions-inlined (tree)
  (let ((inlined-functions (gather-inlined-functions tree)))
    (maphash (lambda (outer-function inner-alist)
               (declare (ignore inner-alist))
               (maphash (lambda (inner-function dummy)
                          (declare (ignore dummy))
                          (let ((count (count-times-inlined tree outer-function inner-function)))
                            (incf (gethash inner-function inlined-functions 0) count)))
                        inlined-functions))
             tree)
    inlined-functions))


(defun read-all-modules (filenames)
  "Analyze the llvm-ir file and print the CL source locations that generate at least NUM lines of llvm-ir code"
  (let ((total-instructions-by-function (make-hash-table :test #'equal))
        (*total-instructions* 0))
    (loop for filename in filenames
          for module = (with-open-file (fin (make-pathname :type "ll" :defaults filename) :direction :input) (parse-file fin))
          do (when (= 0 (hash-table-count (module-inlined-at module)))
               (warn "There were no inlinedAt links in ~a" filename))
          do (analyze-module total-instructions-by-function module))
    (values total-instructions-by-function *total-instructions*)))


(defun read-all-info-files (filenames)
  "Analyze the llvm-ir file and print the CL source locations that generate at least NUM lines of llvm-ir code"
  (let ((tree (make-hash-table :test #'equal)))
    (loop for filename in filenames
          do (load-inline-counts (make-pathname :type "info" :defaults filename) tree))
    tree))

(defun analyze-llvm-ir (filenames &key (func-num 10))
  "Analyze the llvm-ir file and print the CL source locations that generate at least NUM lines of llvm-ir code"
  (multiple-value-bind (total-instructions-by-function total-instructions)
      (read-all-modules filenames)
    (let ((tree (read-all-info-files filenames)))
      (let ((inlined-times (count-times-functions-inlined tree)))
        (generate-report total-instructions-by-function inlined-times *total-instructions* func-num)
        (format t "~10a total-instructions~%" total-instructions)))))
