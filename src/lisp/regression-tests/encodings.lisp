(in-package #:clasp-tests)

(defpackage :asdf-test (:use :cl))

(defvar asdf-test::*LAMBDA-STRING*)

(defun %string-char-codes (s)
  (loop :for c :across s :collect (char-code c)))

;;; using :default or utf-8, this should give string length 1 with char char-code 955 for lambda
(test encoding-default
      (let ()
        (load #P"sys:modules;asdf;test;lambda.lisp")
        (equal (list 955) (%string-char-codes  asdf-test::*LAMBDA-STRING*))))

(test encoding-utf-8
      (let ()
        (load #P"sys:modules;asdf;test;lambda.lisp" :external-format :utf-8)
        (equal (list 955) (%string-char-codes  asdf-test::*LAMBDA-STRING*))))

;;; using :latin-1 or :ISO-8859-1, this should give string length 2 with char char-codes (206 187)

(test encoding-latin-1
      (let ()
        (load #P"sys:modules;asdf;test;lambda.lisp" :external-format :latin-1)
        (equal (list 206 187) (%string-char-codes  asdf-test::*LAMBDA-STRING*))))

(test encoding-ISO-8859-1
      (let ()
        (load #P"sys:modules;asdf;test;lambda.lisp" :external-format :iso-8859-1)
        (equal (list 206 187) (%string-char-codes  asdf-test::*LAMBDA-STRING*))))

;;; using us-ascii, the should be an encoding-error
(test-expect-error encoding-ascii-error
                   (load #P"sys:modules;asdf;test;lambda.lisp" :external-format :us-ascii)
                   :type ext:stream-decoding-error)

(test encoding-all-encodings-plain
      (let ((char (code-char 65))
            (filename "sys:regression-tests;encoding-test.txt"))
        (dolist (encoding (ext:all-encodings))
          (with-open-file (stream filename
                                  :direction :output
                                  :if-exists :overwrite
                                  :if-does-not-exist :create
                                  :external-format encoding)
            (write-char char stream))
          (with-open-file (stream filename
                                  :direction :input
                                  :external-format encoding)
            (let ((read-char (read-char stream)))
              (unless (member encoding '(:UCS-2 :UCS-4))
                (unless (char= char read-char)
                  (error "Chars do not match ~s ~s in encoding ~s~%" char read-char encoding))))))
        (when (probe-file filename)
          (delete-file filename))
        t))

(test encoding-latin-2-plain
      (with-open-file (stream "sys:regression-tests;latin-2-file.txt"
                              :direction :output
                              :if-exists :overwrite
                              :if-does-not-exist :create
                              :external-format :latin-2)
        (write-char (code-char 65) stream)))

(test encoding-latin-2-lambda
      (let ()
        (load #P"sys:modules;asdf;test;lambda.lisp" :external-format :latin-2)
        (equal (list 206 357) (%string-char-codes  asdf-test::*LAMBDA-STRING*))))

(test encoding-iso-8859-2-lambda
      (let ()
        (load #P"sys:modules;asdf;test;lambda.lisp" :external-format :ISO-8859-2)
        (equal (list 206 357) (%string-char-codes  asdf-test::*LAMBDA-STRING*))))
;;; clean-up
(delete-package (find-package :asdf-test))
