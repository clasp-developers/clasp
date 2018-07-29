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

