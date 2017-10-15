
(defstruct symbol-entry
  start end symbol)

(defun read-string (&optional (stream *standard-input*))
  (loop
    for c = (peek-char nil stream nil nil) ; include whitespace
    while (and c (eql c (peek-char t stream nil nil))) ; skip whitespace
    collect (read-char stream) into letters
    finally (return (coerce letters 'string))))

(defun parse-symbol-table (stream)
  (declare (optimize (debug 3)))
  (let ((entries (make-array 256 :adjustable t :fill-pointer 0)))
    (loop for line = (read-line stream nil :eof)
          until (eq line :eof)
          do (with-input-from-string (sin line)
               (let* ((hex (read-string sin))
                      (start (parse-integer hex :start 2 :radix 16))
                      (size (parse-integer (read-string sin)))
                      (symbol (read-line sin)))
                 (vector-push-extend (make-symbol-entry :start start
                                                        :end (+ start size)
                                                        :symbol symbol)
                                     entries))))
    (sort entries #'< :key #'symbol-entry-start)))

(defun load-symbol-table (filename)
  (let ((fin (open filename :direction :input)))
    (unwind-protect
         (parse-symbol-table fin)
      (close fin))))

(defun find-address (address symbol-table)
  (let ((low 0)
        (high (1- (length symbol-table))))
    (loop for mid = (floor (+ high low) 2)
          for entry = (aref symbol-table mid)
          while (<= low high)
          do (cond
               ((< address (symbol-entry-start entry))
                (setf high (1- mid)))
               ((> address (symbol-entry-end entry))
                (setf low (1+ mid)))
               (t
                (return-from find-address (symbol-entry-symbol entry)))))
    nil))


(defclass buffer-stream ()
  ((buffer :initarg :buffer :accessor buffer)
   (buffer-pathname :initarg :buffer-pathname :accessor buffer-pathname)
   (buffer-stream :initarg :buffer-stream :accessor buffer-stream))
  (:documentation "Store the entire contents of a preprocessed file in memory along with a stream on the buffer and its original file name"))


(defun read-entire-file (filename)
  "* Arguments
- filename : A pathname
* Description
Read the contents of the filename into memory and return a buffer-stream on it."
  (with-open-file (stream (pathname filename))
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (make-instance 'buffer-stream
                     :buffer data
                     :buffer-pathname (truename (pathname filename))
                     :buffer-stream (make-string-input-stream data)))))


(defun symbolize-buffer (obuffer output symbol-table)
  (let ((buffer (buffer obuffer)))
    (loop for pos = 0 then next-pos
          for next-pos = (search "0x" (buffer buffer))
          do (if next-pos
                 (let ((hex-end (position-if (lambda (c) (not (digit-char-p c 16))) buffer :start (+ 2 next-pos))))
                   (if hex-end
                       (let* ((hex (subseq buffer (+ 2 next-pos) hex-end))
                            (address (parse-integer hex :start 2 :radix 16))
                            (symbol (find-address address symbol-table)))
                         (if symbol
                             (progn
                               (princ symbol output)
                               (setf next-pos (+ next-pos (length hex))))
                             (progn
                               (princ hex output)
                               (setf next-pos (+ next-pos (length hex))))))
                       (progn
                         (princ (subseq buffer pos next-pos) output))))
                 (princ (subseq buffer pos (length buffer)) output)))))
                 


(defun symbolize-file (source output symbol-table-filename)
  (let* ((symbol-table (load-symbol-table symbol-table-filename))
         (buffer (read-entire-file source)))
    (symbolize-buffer buffer output symbol-table-filename)))


(let ((source (second sb-ext:*posix-argv*))
      (symbol-table-filename (third sb-ext:*posix-argv*)))
  (symbolize-file source *standard-output* symbol-table-filename))



(with-open-file (fout "/tmp/out.txt" :direction :output)
  (symbolize-file "/tmp/sample.txt" fout "/tmp/clasp-symbols-95116-kuxRi9"))


(defparameter *s* (load-symbol-table "/tmp/clasp-symbols-95116-kuxRi9"))
(find-address #16r1284dd91a *s*)

(print #16r1284dd91a)
4971157786 4971157786
*s*
