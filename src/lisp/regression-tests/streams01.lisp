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
                            (LOCALLY (DECLARE (OPTIMIZE SAFETY)) (LOOP (READ-CHAR S)))
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
 'CONCATENATED-STREAM-1
 (MAKE-CONCATENATED-STREAM *error-output* 1 2 3 4)
 :type type-error)

(test-expect-error
 'CONCATENATED-STREAM-2
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

(test-expect-error WITH-INPUT-FROM-STRING-6 (WITH-INPUT-FROM-STRING (S "")(read-char s))
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

(test broadcast-stream-streams.4
      (let ((stream (make-broadcast-stream)))
        (and (= 0 (file-length stream))
             (= 0 (file-position stream))
             (= 1 (file-string-length stream "foo"))
             (eq :default (stream-external-format stream)))))

(test broadcast-stream-streams.3
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


