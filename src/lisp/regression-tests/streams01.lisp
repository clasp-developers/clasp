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
