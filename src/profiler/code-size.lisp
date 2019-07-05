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
  (loop for l below (1- lineno)
        do (read-line sin)))


(defun get-source-at-line-column (directory filename line column)
  (let* ((code (get-source-file directory filename))
         (sin (make-string-input-stream code)))
    (if (< line 99999)
        (progn
          (read-to-line sin line)
          (file-position sin (+ (file-position sin) (if (> column 2) (- column 2) 0)))
          (read-line sin))
        "********** BOGUS LINE ***********")))

(defun analyze-module (module &optional num)
  (let ((analysis (make-hash-table)))
    (loop for code across (module-source-lines module)
          do (incf (gethash (code-metadata code) analysis 0)))
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
                     (format t "~10a ~a:~a:~a~%" count filename line column)
                     (format t "   ---> ~a~%" source))))))))


(defun analyze-llvm-ir (filename &optional (num 100))
  "Analyze the llvm-ir file and print the CL source locations that generate at least NUM lines of llvm-ir code"
  (let ((m (with-open-file (fin filename :direction :input)
             (parse-file fin))))
    (analyze-module m num)))
