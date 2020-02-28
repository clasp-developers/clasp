(in-package #:clasp-tests)

(test listen-1
      (not (WITH-INPUT-FROM-STRING (S "") (LISTEN S))))

(test listen-2
      (let ((res
             (multiple-value-list
              (WITH-INPUT-FROM-STRING (S "x")
                (VALUES (READ-CHAR S)
                        (LISTEN S)
                        (UNREAD-CHAR #\x S)
                        (NOT (LISTEN S))
                        (READ-CHAR S))))))
        (equalp res (list #\x
                          NIL
                          NIL
                          NIL
                          #\x))))

(test listen-3
      (let ((res
             (multiple-value-list
              (WITH-input-from-string (S "xxx")
                (VALUES (NOT (LISTEN S))
                        (HANDLER-CASE
                            (LOCALLY (DECLARE (OPTIMIZE SAFETY))
                          (LOOP (READ-CHAR S)))
                          (END-OF-FILE NIL (LISTEN S))))))))
        (equalp res (list nil nil))))

(test-expect-error
 broadcast-stream
 (make-broadcast-stream 1 2 3)
 :type type-error)

;;; a broadcast-stream should be an ouput-stream
(test-expect-error
 broadcast-stream-1
 (make-broadcast-stream *standard-input*)
 :type type-error)

(test-expect-error
 CONCATENATED-STREAM-1
 (MAKE-CONCATENATED-STREAM *error-output* 1 2 3 4)
 :type type-error)

(test-expect-error
 CONCATENATED-STREAM-2
 (MAKE-CONCATENATED-STREAM *standard-input* 1)
 :type type-error)
 
(test WITH-INPUT-FROM-STRING-1
      (string-equal "23456789"
                    (WITH-INPUT-FROM-STRING (S "0123456789" :START 2)
                      (READ-LINE S))))

(test WITH-INPUT-FROM-STRING-2
      (string-equal "01"
                    (WITH-INPUT-FROM-STRING (S "0123456789" :end 2)
                      (READ-LINE S))))

(test WITH-INPUT-FROM-STRING-3
      (string-equal "012345678"
                    (WITH-INPUT-FROM-STRING (S "0123456789" :end 9)
                      (READ-LINE S))))

(test WITH-INPUT-FROM-STRING-4
      (string-equal "0123456789"
                    (WITH-INPUT-FROM-STRING (S "0123456789" :end 10)
                      (READ-LINE S))))

(test WITH-INPUT-FROM-STRING-5
      (string-equal "9"
                    (WITH-INPUT-FROM-STRING (S "0123456789" :start 9)
                      (READ-LINE S))))

(test
 WITH-INPUT-FROM-STRING-6
 (let ((string (make-array 3 :element-type 'character
                           :initial-contents (list (code-char 65)(code-char 65)(code-char 65)))))
   (string-equal string
                 (with-input-from-string (var string)
                   (read-line var)))))

(test
 WITH-INPUT-FROM-STRING-7
 (let ((string (make-array 3 :element-type 'character
                           :initial-contents (list (code-char 256)(code-char 256)(code-char 256)))))
   (string-equal string
                 (with-input-from-string (var string)
                   (read-line var)))))

(test
 WITH-INPUT-FROM-STRING-8
 (let ((string (make-array 3 :element-type 'character
                           :initial-contents (list (code-char 254)(code-char 255)(code-char 256)))))
   (string-equal string
                 (with-input-from-string (var string)
                   (read-line var)))))

(test
 WITH-INPUT-FROM-STRING-9
 (let ((string ""))
   (string-equal string
                 (with-input-from-string (var string)
                   (read-line var nil "")))))

(test
 WITH-INPUT-FROM-STRING-10
 (let ((string (make-string (* 8 1024) :initial-element #\a)))
   (string-equal string
                 (with-input-from-string (var string)
                   (read-line var)))))

(test
 WITH-INPUT-FROM-STRING-10a
 (let ((string (concatenate 'string
                            (make-string 256 :initial-element #\a)
                            (make-string 1 :initial-element (code-char 256)))))
   (string-equal string
                 (with-input-from-string (var string)
                   (read-line var)))))

(test WITH-INPUT-FROM-STRING-10b
 (let ((x (make-string 3000 :initial-element #\a)))
   (setf (char x 2999) (code-char 600))
   (string-equal x
   (with-input-from-string (v x)
     (read-line v)))))

(test-expect-error WITH-INPUT-FROM-STRING-11 (WITH-INPUT-FROM-STRING (S "")(read-char s))
                   :type end-of-file)

(test-expect-error WRITE-BYTE.ERROR.3 (WRITE-BYTE 0))
(test-expect-error WRITE-BYTE.ERROR.4 (WRITE-BYTE 0 nil) :type type-error)

(test FILE-STRING-LENGTH-1
      (numberp
       (with-open-file (blah "nada.txt" :direction :output
                             :if-exists :supersede)
         (FILE-STRING-LENGTH  (MAKE-BROADCAST-STREAM blah)
                                      "antidisestablishmentarianism"))))

(test FILE-STRING-LENGTH-2
      (= 1
       (FILE-STRING-LENGTH  (MAKE-BROADCAST-STREAM)
                            "antidisestablishmentarianism")))

(test FILE-STRING-LENGTH-3
      (numberp
       (with-open-file (blah "nada.txt" :direction :output
                             :if-does-not-exist :create
                             :if-exists :supersede)
         (FILE-STRING-LENGTH blah "antidisestablishmentarianism"))))

;;; http://www.lispworks.com/documentation/HyperSpec/Body/t_broadc.htm#broadcast-stream
;;; * The functions file-length, file-position, file-string-length, and stream-external-format
;;; return the value from the last component stream;
;;; if there are no component streams, file-length and file-position return 0,
;;; file-string-length returns 1, and stream-external-format returns :default.

(test broadcast-stream-streams.3
      (let ((stream (make-broadcast-stream)))
        (and (= 0 (file-length stream))
             (= 0 (file-position stream))
             (= 1 (file-string-length stream "foo"))
             (eq :default (stream-external-format stream)))))


(test broadcast-stream-streams.4
       (let ((first-stream (make-string-output-stream)))
         (with-open-file (last-stream "bss-last.txt"
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
           (format last-stream "Hello world!~%")
           (finish-output last-stream)     ; for buffered streams
           (let ((broadcast (make-broadcast-stream first-stream last-stream)))
             (and
              (= 13 (file-length last-stream))
              (= 13 (file-length broadcast) (file-length last-stream))
              (= 13 (file-position broadcast) (file-position last-stream))
              (= 2
                 (file-string-length broadcast "jd")
                 (file-string-length last-stream "jd")))))))

;;; did trap ../../src/core/lispStream.cc:1541 Illegal op Abort trap: 6
(test stream-element-type.error.3.simplified
      (handler-case
          (stream-element-type 0)
        (error (e) e)))


