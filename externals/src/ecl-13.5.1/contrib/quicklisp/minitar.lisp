(defpackage #:ql-minitar
  (:documentation
   "A simple implementation of unpacking the 'tar' file format.")
  (:use #:cl)
  (:export #:tarball-contents
           #:unpack-tarball))

(in-package #:ql-minitar)

(defun make-block-buffer ()
  (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0))

(defun skip-n-blocks (n stream)
  (let ((block (make-block-buffer)))
    (dotimes (i n)
      (read-sequence block stream))))

(defun ascii-subseq (vector start end)
  (let ((string (make-string (- end start))))
    (loop for i from 0
          for j from start below end
          do (setf (char string i) (code-char (aref vector j))))
    string))

(defun block-asciiz-string (block start length)
  (let* ((end (+ start length))
         (eos (or (position 0 block :start start :end end)
                            end)))
    (ascii-subseq block start eos)))

(defun prefix (header)
  (when (plusp (aref header 345))
    (block-asciiz-string header 345 155)))

(defun name (header)
  (block-asciiz-string header 0 100))

(defun payload-size (header)
  (values (parse-integer (block-asciiz-string header 124 12) :radix 8)))

(defun nth-block (n file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (let ((block (make-block-buffer)))
      (skip-n-blocks (1- n) stream)
      (read-sequence block stream)
      block)))

(defun payload-type (code)
  (case code
    (0 :file)
    (48 :file)
    (50 :symlink)
    (76 :long-name)
    (53 :directory)
    (103 :global-header)
    (t :unsupported)))

(defun full-path (header)
  (let ((prefix (prefix header))
        (name (name header)))
    (if prefix
        (format nil "~A/~A" prefix name)
        name)))

(defun save-file (file size stream)
  (multiple-value-bind (full-blocks partial)
      (truncate size 512)
    (ensure-directories-exist file)
    (with-open-file (outstream file
                     :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
      (let ((block (make-block-buffer)))
        (dotimes (i full-blocks)
          (read-sequence block stream)
          (write-sequence block outstream))
        (when (plusp partial)
          (read-sequence block stream)
          (write-sequence block outstream :end partial))))))

(defun unpack-tarball (tarfile &key (directory *default-pathname-defaults*))
  (let ((block (make-block-buffer)))
    (with-open-file (stream tarfile :element-type '(unsigned-byte 8))
      (loop
       (let ((size (read-sequence block stream)))
         (when (zerop size)
           (return))
         (unless (= size 512)
           (error "Bad size on tarfile"))
         (when (every #'zerop block)
           (return))
         (let* ((payload-code (aref block 156))
                (payload-type (payload-type payload-code))
                (tar-path (full-path block))
                (full-path (merge-pathnames tar-path directory))
                (payload-size (payload-size block))
                (block-count (ceiling (payload-size block) 512)))
         (case payload-type
           (:file
            (save-file full-path payload-size stream))
           (:directory
            (ensure-directories-exist full-path))
           ((:symlink :long-name :global-header)
            ;; These block types aren't required for Quicklisp archives
            (skip-n-blocks block-count stream))
           (t
            (warn "Unknown tar block payload code -- ~D" payload-code)
            (skip-n-blocks block-count stream)))))))))

(defun contents (tarfile)
  (let ((block (make-block-buffer))
        (result '()))
    (with-open-file (stream tarfile :element-type '(unsigned-byte 8))
      (loop
        (let ((size (read-sequence block stream)))
          (when (zerop size)
            (return (nreverse result)))
          (unless (= size 512)
            (error "Bad size on tarfile"))
          (when (every #'zerop block)
            (return (nreverse result)))
          (let* ((payload-type (payload-type (aref block 156)))
                 (tar-path (full-path block))
                 (payload-size (payload-size block)))
            (skip-n-blocks (ceiling payload-size 512) stream)
            (case payload-type
              (:file
               (push tar-path result))
              (:directory
               (push tar-path result)))))))))
