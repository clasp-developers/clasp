(in-package #:ninja)

(defparameter *line-end*
  ""
  "Text to append at the end of a wrapped line.")

(defparameter *line-start*
  ""
  "Text to prepend to the start of the next line after a line has been wrapped.")

(defparameter *line-width*
  100
  "Line width (in characters) where wrapping will become active.")


(defclass line-wrapping-stream
    (trivial-gray-streams:fundamental-character-output-stream)
  ((stream :reader line-wrapping-stream-stream
           :initarg :stream
           :documentation "The underlying stream which line wrapping done upon.")
   (buffer :reader line-wrapping-stream-buffer
           :initform (make-array 100 :adjustable t :fill-pointer 0 :element-type 'character)
           :documentation "An array containing text that has not yet been wrapped and not
written to the underlying stream.")
   (bufferp :accessor line-wrapping-stream-bufferp
            :initform nil
            :type boolean
            :documentation "A boolean value indicating whether the buffer is accumulating.")
   (column :accessor line-wrapping-stream-column
           :initform 0
           :type integer
           :documentation "The current column number.")))

(defmethod trivial-gray-streams:stream-write-char ((stream line-wrapping-stream) char)
  (with-accessors ((target line-wrapping-stream-stream)
                   (buffer line-wrapping-stream-buffer)
                   (bufferp line-wrapping-stream-bufferp)
                   (column line-wrapping-stream-column))
      stream
    (flet ((dump-buffer ()
             (let ((text (concatenate 'string *line-start*
                                      (string-left-trim " "
                                                        (concatenate 'string buffer
                                                                     (string char))))))
               (write-line *line-end* target)
               (write-string text target)
               (setf column (length text)
                     (fill-pointer buffer) 0
                     bufferp nil))))
    (cond ((char= #\Newline char)
           (when bufferp
             (write-string buffer target)
             (setf (fill-pointer buffer) 0
                   bufferp nil))
           (terpri target)
           (setf column 0))
          ((and bufferp
                (>= column (- *line-width* (length *line-end*))))
           (dump-buffer))
          ((and (char/= #\Space char)
                bufferp)
           (vector-push-extend char buffer)
           (incf column))
          ((char/= #\Space char)
           (write-char char target)
           (incf column))
          ((and bufferp
                (= (1- (length buffer))
                   (position #\Space buffer :from-end t)))
           (vector-push-extend char buffer)
           (incf column))
          (t
           (write-string buffer target)
           (setf (fill-pointer buffer) 0
                 bufferp t)
           (vector-push-extend char buffer)
           (incf column))))))

(defmethod trivial-gray-streams:stream-finish-output ((stream line-wrapping-stream))
  (finish-output (line-wrapping-stream-stream stream)))

(defmethod trivial-gray-streams:stream-line-column ((stream line-wrapping-stream))
  (line-wrapping-stream-column stream))

(defmethod cl:close ((stream line-wrapping-stream) &key abort)
  (close (line-wrapping-stream-stream stream)))

(defun make-line-wrapping-stream (stream)
  "Create a new line wrapping stream."
  (make-instance 'line-wrapping-stream :stream stream))
