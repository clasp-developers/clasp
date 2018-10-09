#!/bin/sh
#--eval '(set-dispatch-macro-character #\# #\! (lambda (s c n)(declare (ignore c n)) (read-line s) (values)))' \
#|
SCRIPT_DIR="$(dirname "$0")"
exec sbcl --noinform --disable-ldb --lose-on-corruption --disable-debugger \
--no-sysinit --no-userinit --noprint \
--eval '(set-dispatch-macro-character #\# #\! (lambda (s c n)(declare (ignore c n)) (read-line s) (values)))' \
--eval "(defvar *script-args* '( $# \"$0\" \"$1\" \"$2\" \"$3\" \"$4\" \"$5\" \"$6\" \"$7\" \"$8\" \"$9\" ))" \
--eval "(require :asdf)" \
--load "$0"
|#

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
    (if (or (< address (symbol-entry-start (aref symbol-table 0)))
            (>= address (symbol-entry-end (aref symbol-table (1- (length symbol-table))))))
        nil
        (loop for mid = (floor (+ high low) 2)
              for entry = (aref symbol-table mid)
              while (<= low high)
              do (cond
                   ((< address (symbol-entry-start entry))
                    (setf high (1- mid)))
                   ((> address (symbol-entry-end entry))
                    (setf low (1+ mid)))
                   (t
                    (return-from find-address (symbol-entry-symbol entry))))))
    nil))


(defclass buffer-stream ()
  ((buffer :initarg :buffer :accessor buffer)
   (buffer-pathname :initarg :buffer-pathname :accessor buffer-pathname)
   (buffer-stream :initarg :buffer-stream :accessor buffer-stream))
  (:documentation "Store the entire contents of a preprocessed file in memory along with a stream on the buffer and its original file name"))


(defun read-entire-stream (stream &optional filename)
  "* Arguments
- filename : A pathname
* Description
Read the contents of the filename into memory and return a buffer-stream on it."
  (declare (optimize (debug 3)))
  (format *debug-io* "read-entire-stream~%")
  (let* ((len (file-length stream))
         (data (make-string len)))
    (format *debug-io* "File length: ~a~%" len)
    (read-sequence data stream)
    (format *debug-io* "making buffer-stream~%")
    (prog1
        (make-instance 'buffer-stream
                       :buffer data
                       :buffer-pathname filename
                       :buffer-stream (make-string-input-stream data))
      (format *debug-io* "returning from read-entire-stream~%"))))


(defun symbolicate-buffer (buffer output symbol-table)
  (let ((count 0)
        (replaced (make-hash-table :test #'equal)))
    (loop for pos = 0 then next-pos
          for next-pos = (search "0x" buffer :start2 pos)
          while next-pos
          do (princ (subseq buffer pos next-pos) output)
          do (let ((hex-end (position-if (lambda (c) (not (digit-char-p c 16))) buffer :start (+ 2 next-pos))))
               (unless hex-end
                 (setf hex-end (length buffer)))
               (let* ((hex (subseq buffer (+ 2 next-pos) hex-end))
                      (address (parse-integer hex :radix 16))
                      (symbol (find-address address symbol-table)))
                 (if symbol
                     (progn
                       (princ symbol output)
                       (setf next-pos hex-end)
                       (incf count)
                       (incf (gethash symbol replaced 0)))
                     (progn
                       (princ (subseq buffer next-pos hex-end) output)
                       (setf next-pos hex-end)))))
          finally (princ (subseq buffer pos (length buffer)) output))
    (format *debug-io* "Replaced ~a addresses~%" count)
    (format *debug-io* "         ~a unique symbols~%" (hash-table-count replaced))
    (let (unique)
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (push k unique)) replaced)
      (format *debug-io* "       Symbols:~%")
      (dolist (s unique)
        (format *debug-io*   "      ~a(~a)~%" s (gethash s replaced))))))
                 


(defun symbolicate-stream (input output symbol-table)
  (format *debug-io* "In symbolicate-stream input -> ~a output -> ~s  symbol-table length -> ~a~%" input output (length symbol-table))
  (let* ((buffer (read-entire-stream input)))
    (format *debug-io* "Read file~%")
    (symbolicate-buffer (buffer buffer) output symbol-table)))

(let* ((output-stream *standard-output*)
      (input-stream *standard-input*)
<<<<<<< HEAD
      (symbol-table nil)
      (nargs (car *script-args*))
      (args (subseq (cddr *script-args*) 0 nargs)))
  (declare (optimize (debug 3)) (ignore nargs))
  (format t "nargs: ~a args: ~s~%" nargs args)
  (loop for cur = args then (cddr cur)
=======
      (symbol-table nil))
  (declare (optimize (debug 3)))
  (loop for cur = (cddr *script-args*) then (cddr cur)
>>>>>>> Fix shebang in symbolicate.lisp
        for option = (first cur)
        for value = (second cur)
        while cur
        do (progn
             (format t "Handling argument ~a ~s~%" option value)
             (cond
               ((string= option "-o")
                (setf output-stream (open value :direction :output :if-exists :supersede)))
               ((string= option "-i")
                (setf input-stream (open value :direction :input :external-format '(:utf-8 :replacement #\?))))
               ((string= option "-s")
                (setf symbol-table (load-symbol-table value)))
               ((zerop (length option)))
               (t (error "Unknown option ~a - only -o {output} -i {input} -s {symbol-table} are allowed" option)))))
  (or symbol-table (error "The symbol-table must be provided using -s"))
  (format *debug-io* "Starting symbolicate~%")
  (symbolicate-stream input-stream output-stream symbol-table)
  (close output-stream)
  (close input-stream)
  (format *debug-io* "Done~%")
  (SB-EXT:EXIT))
