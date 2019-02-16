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

(test issue-382-a
      (arrayp (let ((*readtable* (copy-readtable nil)))
                (read-from-string "#a(T 1 (2))"))))

(test issue-382-b
      (arrayp (let ((*readtable* (copy-readtable nil)))
                (read-from-string "#a(T (1) (2))"))))

(test issue-382-c
      (arrayp (let ((*readtable* (copy-readtable nil)))
                (read-from-string "#a(T (2 2) ((1 2)(3 4)))"))))

(test issue-382-d
      (arrayp (let ((*readtable* (copy-readtable nil)))
                (read-from-string "#2A((0 1 5) (foo 2 (hot dog)))"))))

(defstruct %foo% %bar%)

(test sharp-s-new-readtable
      (let ((*readtable* (copy-readtable nil)))
        (%foo%-p (read-from-string "#S(%FOO% :%BAR% 1)"))))

(test 'SET-SYNTAX-FROM-CHAR-TRAIT-X-1a
      (let ((result
             (let ((chars (list #\Backspace #\Tab  #\Newline #\Linefeed #\Page #\Return #\Space #\Rubout)))
               ;;; 2.1.4 Character Syntax Types
               ;;; Backspace and Rubout are constituent but invalid
               (mapcar #'(lambda(c)
                           (WITH-STANDARD-IO-SYNTAX
                             (LET ((*READTABLE* (COPY-READTABLE NIL)))
                                (handler-case 
                                   (READ-FROM-STRING
                                    (CONCATENATE 'STRING (STRING C) "Z"))
                                 (reader-error (e) e)))))
                       chars))))
        (and (typep (first result) 'reader-error)
             (typep (eighth result) 'reader-error))))

(test SET-SYNTAX-FROM-CHAR-TRAIT-X-1b
      (let ((result
             (let ((chars (list #\Backspace #\Tab  #\Newline #\Linefeed #\Page #\Return #\Space #\Rubout)))
               (mapcar #'(lambda(c)
                           (WITH-STANDARD-IO-SYNTAX
                             (LET ((*READTABLE* (COPY-READTABLE NIL)))
                               (SET-SYNTAX-FROM-CHAR C #\X)
                               ;;; see 2.1.4.2 Constituent Traits
                               ;;; all these chars should fail
                               (handler-case 
                                   (READ-FROM-STRING
                                    (CONCATENATE 'STRING (STRING C) "Z"))
                                 (reader-error (e) e)))))
                       chars))))
        (every #'(lambda(a)(typep a 'reader-error)) result)))

  


