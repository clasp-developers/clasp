(in-package #:clasp-tests)

(test listen-1 (WITH-INPUT-FROM-STRING (S "") (LISTEN S)) (nil))

(test listen-2
      (WITH-INPUT-FROM-STRING (S "x")
        (VALUES (READ-CHAR S)
                (LISTEN S)
                (UNREAD-CHAR #\x S)
                (NOT (LISTEN S))
                (READ-CHAR S)))
      (#\x nil nil nil #\x))

(test listen-3
      (WITH-input-from-string (S "xxx")
        (VALUES (NOT (LISTEN S))
                (HANDLER-CASE
                    (LOCALLY (DECLARE (OPTIMIZE SAFETY))
                      (LOOP (READ-CHAR S)))
                  (END-OF-FILE () (LISTEN S)))))
      (nil nil))

(test-type listen-4
    (progn
      (with-open-file (blah "nada.txt" :direction :output
                                       :if-does-not-exist :create
                                       :if-exists :supersede)
        (write-char #\a blah))
      (with-open-file (blah "nada.txt" :direction :input)
        (values (listen blah)
                (read-char blah)
                (listen blah))))
    (t #\a nil))

(test-expect-error broadcast-stream (make-broadcast-stream 1 2 3)
                   :type type-error)

;;; a broadcast-stream should be an ouput-stream
(test-expect-error broadcast-stream-1 (make-broadcast-stream *standard-input*)
                   :type type-error)

(test-expect-error CONCATENATED-STREAM-1
                   (MAKE-CONCATENATED-STREAM *error-output* 1 2 3 4)
                   :type type-error)

(test-expect-error CONCATENATED-STREAM-2
                   (MAKE-CONCATENATED-STREAM *standard-input* 1)
                   :type type-error)
 
(test WITH-INPUT-FROM-STRING-1
      (WITH-INPUT-FROM-STRING (S "0123456789" :START 2)
        (READ-LINE S))
      ("23456789" t))

(test WITH-INPUT-FROM-STRING-2
      (WITH-INPUT-FROM-STRING (S "0123456789" :end 2)
        (READ-LINE S))
      ("01" t))

(test WITH-INPUT-FROM-STRING-3
      (WITH-INPUT-FROM-STRING (S "0123456789" :end 9)
        (READ-LINE S))
      ("012345678" t))

(test WITH-INPUT-FROM-STRING-4
      (WITH-INPUT-FROM-STRING (S "0123456789" :end 10)
        (READ-LINE S))
      ("0123456789" t))

(test WITH-INPUT-FROM-STRING-5
      (WITH-INPUT-FROM-STRING (S "0123456789" :start 9)
        (READ-LINE S))
      ("9" t))

(test-true WITH-INPUT-FROM-STRING-6
           (let ((string (make-array 3 :element-type 'character
                                       :initial-contents (list (code-char 65)(code-char 65)(code-char 65)))))
             (string-equal string
                           (with-input-from-string (var string)
                             (read-line var)))))

(test-true WITH-INPUT-FROM-STRING-7
           (let ((string (make-array 3 :element-type 'character
                                       :initial-contents (list (code-char 256)(code-char 256)(code-char 256)))))
             (string-equal string
                           (with-input-from-string (var string)
                             (read-line var)))))

(test-true WITH-INPUT-FROM-STRING-8
           (let ((string (make-array 3 :element-type 'character
                                       :initial-contents (list (code-char 254)(code-char 255)(code-char 256)))))
             (string-equal string
                           (with-input-from-string (var string)
                             (read-line var)))))

(test WITH-INPUT-FROM-STRING-9
      (let ((string ""))
        (with-input-from-string (var string)
          (read-line var nil "")))
      ("" t))

(test-true WITH-INPUT-FROM-STRING-10
           (let ((string (make-string (* 8 1024) :initial-element #\a)))
             (string-equal string
                           (with-input-from-string (var string)
                             (read-line var)))))

(test-true WITH-INPUT-FROM-STRING-10a
           (let ((string (concatenate 'string
                                      (make-string 256 :initial-element #\a)
                                      (make-string 1 :initial-element (code-char 256)))))
             (string-equal string
                           (with-input-from-string (var string)
                             (read-line var)))))

(test-true WITH-INPUT-FROM-STRING-10b
           (let ((x (make-string 3000 :initial-element #\a)))
             (setf (char x 2999) (code-char 600))
             (string-equal x
                           (with-input-from-string (v x)
                             (read-line v)))))

(test-expect-error WITH-INPUT-FROM-STRING-11 (WITH-INPUT-FROM-STRING (S "")(read-char s))
                   :type end-of-file)

(test-expect-error WRITE-BYTE.ERROR.3 (WRITE-BYTE 0))
(test-expect-error WRITE-BYTE.ERROR.4 (WRITE-BYTE 0 nil) :type type-error)

(test-type FILE-STRING-LENGTH-1
    (with-open-file (blah "nada.txt" :direction :output
                                     :if-exists :supersede)
      (FILE-STRING-LENGTH  (MAKE-BROADCAST-STREAM blah)
                           "antidisestablishmentarianism"))
    (integer 0))

(test FILE-STRING-LENGTH-2
      (FILE-STRING-LENGTH  (MAKE-BROADCAST-STREAM)
                           "antidisestablishmentarianism")
      (1))

(test-type FILE-STRING-LENGTH-3
    (with-open-file (blah "nada.txt" :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :supersede)
      (FILE-STRING-LENGTH blah "antidisestablishmentarianism"))
    (integer 0))

;;; http://www.lispworks.com/documentation/HyperSpec/Body/t_broadc.htm#broadcast-stream
;;; * The functions file-length, file-position, file-string-length, and stream-external-format
;;; return the value from the last component stream;
;;; if there are no component streams, file-length and file-position return 0,
;;; file-string-length returns 1, and stream-external-format returns :default.

(test broadcast-stream-streams.3
      (let ((stream (make-broadcast-stream)))
        (values (file-length stream) (file-position stream)
                (file-string-length stream "foo")
                (stream-external-format stream)))
      (0 0 1 :default))


(test broadcast-stream-streams.4
       (let ((first-stream (make-string-output-stream)))
         (with-open-file (last-stream "bss-last.txt"
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
           (format last-stream "Hello world!~%")
           (finish-output last-stream)     ; for buffered streams
           (let ((broadcast (make-broadcast-stream first-stream last-stream)))
             (values (file-length last-stream) (file-length broadcast)
                     (file-position last-stream) (file-position broadcast)
                     (file-string-length last-stream "jd")
                     (file-string-length broadcast "jd")))))
      (13 13 13 13 2 2))

;;; did trap ../../src/core/lispStream.cc:1541 Illegal op Abort trap: 6
(test-expect-error stream-element-type.error.3.simplified
                   (stream-element-type 0)
                   :type type-error)

(require :gray-streams)

(defclass character-input-stream
    (gray:fundamental-character-input-stream)
  ((value :reader value
          :initarg :value)
   (index :accessor index
          :initform 0)))

(defmethod gray:stream-read-char ((stream character-input-stream))
  (with-accessors ((value value)
                   (index index))
      stream
    (if (< index (length value))
        (prog1 (char value index)
          (incf index))
        :eof)))

(defmethod gray:stream-unread-char ((stream character-input-stream) character)
  (with-accessors ((value value)
                   (index index))
      stream
    (when (zerop index)
      (error "Stream is at beginning, cannot unread character"))
    (when (char/= character (char value (decf index)))
      (error "Cannot unread a character that does not match."))
    nil))

(test-expect-error read-line.eof.01
  (read-line (make-instance 'character-input-stream :value ""))
  :type end-of-file)

(test read-line.eof.02
  (read-line (make-instance 'character-input-stream :value "") nil :wibble)
  (:wibble t))

(test read-line.eof.03
  (read-line (make-instance 'character-input-stream :value "a
"))
  ("a" nil))

(test read-line.eof.04
  (read-line (make-instance 'character-input-stream :value "a"))
  ("a" t))

(test close.abort.01
  (let* ((name (core:mkstemp "close-abort"))
         (stream (open name :if-does-not-exist :create :direction :output)))
    (write-string "wibble" stream)
    (close stream)
    (when (core:file-kind name nil)
      (delete-file name)))
  (t))

(test close.abort.02
  (let* ((name (core:mkstemp "close-abort"))
         (stream (open name :if-does-not-exist :create :direction :output)))
    (write-string "wibble" stream)
    (close stream :abort t)
    (when (core:file-kind name nil)
      (delete-file name)))
  (nil))

(test close.abort.03
  (let* ((name (core:mkstemp "close-abort"))
         (stream (open name :if-does-not-exist :create :direction :output))
         (buffer (make-array 3 :element-type 'character)))
    (write-string "foo" stream)
    (close stream)
    (setf stream (open name :if-does-not-exist :create :if-exists :supersede :direction :output))
    (write-string "bar" stream)
    (close stream)
    (setf stream (open name :direction :input))
    (read-sequence buffer stream :start 0 :end 3)
    (close stream)
    (delete-file name)
    buffer)
  ("bar"))

(test close.abort.04
  (let* ((name (core:mkstemp "close-abort"))
         (stream (open name :if-does-not-exist :create :direction :output))
         (buffer (make-array 3 :element-type 'character)))
    (write-string "foo" stream)
    (close stream)
    (setf stream (open name :if-does-not-exist :create :if-exists :supersede :direction :output))
    (write-string "bar" stream)
    (close stream :abort t)
    (setf stream (open name :direction :input))
    (read-sequence buffer stream :start 0 :end 3)
    (close stream)
    (delete-file name)
    buffer)
  ("foo"))

(defclass binary-input-stream (gray:fundamental-binary-input-stream)
  ((value :reader value
          :initarg :value)
   (index :accessor index
          :initform 0)))

(defmethod gray:stream-element-type ((stream binary-input-stream))
  '(unsigned-byte 8))

(defmethod gray:stream-read-byte ((stream binary-input-stream))
  (with-accessors ((value value)
                   (index index))
      stream
    (if (< index (length value))
        (prog1 (elt value index)
          (incf index))
        :eof)))

(test read-byte.01
  (let ((stream (make-instance 'binary-input-stream :value #())))
    (read-byte stream nil :wibble))
  (:wibble))

(test-expect-error read-byte.02
  (let ((stream (make-instance 'binary-input-stream :value #())))
    (read-byte stream))
  :type end-of-file)

(test read-byte.03
  (let ((stream (make-instance 'binary-input-stream :value #(73))))
    (values (read-byte stream nil :wibble)
            (read-byte stream nil :wibble)))
  (73 :wibble))

(defclass bidirectional-char-stream
    (gray:fundamental-character-input-stream
     gray:fundamental-character-output-stream)
  ())

(defmethod gray:stream-input-column ((s bidirectional-char-stream))
  23)

(defmethod gray:stream-input-line ((s bidirectional-char-stream))
  29)

(defmethod gray:stream-line-column ((s bidirectional-char-stream))
  31)

(defmethod gray:stream-line-number ((s bidirectional-char-stream))
  37)

(test gray-cursor.01
  (let ((stream (make-instance 'bidirectional-char-stream)))
    (values (core:stream-input-column stream)
            (core:stream-input-line stream)
            (core:stream-output-column stream)
            (core:stream-output-line stream)))
  (23 29 31 37))

(test input-cursor.01
  (let ((stream (make-string-input-stream "ab
cd")))
    (flet ((loc (ret)
             (list ret
                   :column (core:stream-input-column stream)
                   :line (core:stream-input-line stream))))
      (values (loc (read-char stream))
              (loc (unread-char #\a stream))
              (loc (peek-char nil stream))
              (loc (read-char stream))
              (loc (read-char stream))
              (loc (read-char stream))
              (loc (read-char stream)))))
  ((#\a :column 1 :line 1)
   (nil :column 0 :line 1)
   (#\a :column 0 :line 1)
   (#\a :column 1 :line 1)
   (#\b :column 2 :line 1)
   (#\newline :column 0 :line 2)
   (#\c :column 1 :line 2)))

(test output-cursor.01
  (let ((stream (make-string-output-stream)))
    (flet ((loc (ret)
             (list ret
                   :column (core:stream-output-column stream)
                   :line (core:stream-output-line stream))))
      (values (loc (write-char #\a stream))
              (loc (write-char #\b stream))
              (loc (write-char #\Newline stream))
              (loc (write-line "cd" stream))
              (loc (write-char #\e stream)))))
  ((#\a :column 1 :line 1)
   (#\b :column 2 :line 1)
   (#\Newline :column 0 :line 2)
   ("cd" :column 0 :line 3)
   (#\e :column 1 :line 3)))
