(in-package #:clasp-tests)

(test read-1
      (equalp (list 1.111 1.111 1.111d0 1.111d0)
              (let ((result nil))
                (dolist (type (list 'short-float 'single-float 'double-float 'long-float) (reverse result))
                  (let ((*read-default-float-format* type))
                    (push (read-from-string "1.111") result))))))

(test read-2
      (string-equal
       "
#.ext:single-float-positive-infinity "
       (with-output-to-string (*standard-output*)
         (let ((*read-default-float-format* 'single-float)
               (*print-readably* nil))
           (print (read-from-string (format nil "12~40,2f" most-positive-single-float)))))))

(test read-3
      (string-equal
       (concatenate 'string (string #\Newline)
                    "#.ext:double-float-positive-infinity ")
       (with-output-to-string (*standard-output*)
         (let ((*read-default-float-format* 'double-float)
               (*print-readably* nil))
           (print (read-from-string (format nil "12~308,2f" most-positive-double-float)))))))


;;; Reader-errors

(test-expect-error read-4 (READ-FROM-STRING ")") :type reader-error)
(test-expect-error read-5 (READ-FROM-STRING ",1") :type reader-error)
(test-expect-error read-6 (READ-FROM-STRING ",") :type reader-error)
(test-expect-error read-7 (READ-FROM-STRING "#)" NIL NIL) :type reader-error)

(test read-8
      (let ((wide-string (make-string 4 :initial-element (code-char 256))))
        (string= wide-string
                 (with-input-from-string (stream wide-string)
                   (read-line stream)))))

(test-expect-error read-9 (read-byte) :type PROGRAM-ERROR)
(test-expect-error read-10 (read-byte nil) :type type-error)
(test-expect-error read-11 (read-byte t) :type type-error)
(test-expect-error read-12 (read-byte 23) :type type-error)
(test-expect-error read-13
                   (with-input-from-string (strm "wq12351523765127635765232")
                     (read-byte strm))
                   :type type-error)

;;; used to error with A string of dots was encountered by the reader.
(test error-mcclim-1
      (list :\.))

(test-expect-error read-14
                   (WITH-INPUT-FROM-STRING (S "") (READ S))
                   :type END-OF-FILE)

(test read-from-string-1a
      (multiple-value-bind
            (object position)
          (let ((*read-default-float-format* 'single-float))
            (read-from-string "123.1212 (1 2 3)" nil :end :preserve-whitespace nil))
        (and (= object 123.1212)
             ;;; position, is the index of the first character in the bounded string that was not read
             ;;; in this case why 9 and not 8
             (= position 9))))

(test read-from-string-1b
      (multiple-value-bind
            (object position)
          (let ((*read-default-float-format* 'single-float))
            (read-from-string "123.1212 (1 2 3)" nil :end :preserve-whitespace t))
        (and (= object 123.1212)
             ;;; position, is the index of the first character in the bounded string that was not read
             ;;; in this case why 9 and not 8
             (= position 8))))

(test read-from-string-1c
      (multiple-value-bind
            (object position)
          (let ((*read-default-float-format* 'single-float))
            (read-from-string " 1 3 5" t nil :start 2 :preserve-whitespace nil))
        (and (= object 3)
             ;;; position, is the index of the first character in the bounded string that was not read
             ;;; in this case why 9 and not 8
             (= position 5))))

(test read-from-string-1d
      (multiple-value-bind
            (object position)
          (let ((*read-default-float-format* 'single-float))
            (read-from-string " 1 3 5" t nil :start 2 :preserve-whitespace t))
        (and (= object 3)
             ;;; position, is the index of the first character in the bounded string that was not read
             ;;; in this case why 9 and not 8
             (= position 4))))

(test issue-678
      (floatp (first (list 1.e-7))))
  


