(in-package #:clasp-tests)

(defpackage :asdf-test (:use :cl))

(defvar asdf-test::*LAMBDA-STRING*)

(defun %string-char-codes (s)
  (loop :for c :across s :collect (char-code c)))

;;; using :default or utf-8, this should give string length 1 with char char-code 955 for lambda
(test encoding-default
      (let ()
        (load #P"sys:src;lisp;modules;asdf;test;lambda.lisp")
        (%string-char-codes  asdf-test::*LAMBDA-STRING*))
      ((955)))

(test encoding-utf-8
      (let ()
        (load #P"sys:src;lisp;modules;asdf;test;lambda.lisp" :external-format :utf-8)
        (%string-char-codes  asdf-test::*LAMBDA-STRING*))
      ((955)))

;;; using :latin-1 or :ISO-8859-1, this should give string length 2 with char char-codes (206 187)

(test encoding-latin-1
      (let ()
        (load #P"sys:src;lisp;modules;asdf;test;lambda.lisp" :external-format :latin-1)
        (%string-char-codes  asdf-test::*LAMBDA-STRING*))
      ((206 187)))

(test encoding-ISO-8859-1
      (let ()
        (load #P"sys:src;lisp;modules;asdf;test;lambda.lisp" :external-format :iso-8859-1)
        (%string-char-codes  asdf-test::*LAMBDA-STRING*))
      ((206 187)))

;;; using us-ascii, the should be an encoding-error
(test-expect-error encoding-ascii-error
                   (load #P"sys:src;lisp;modules;asdf;test;lambda.lisp" :external-format :us-ascii)
                   :type ext:stream-decoding-error)

(test encoding-all-encodings-plain
      (let ((char (code-char 65))
            (filename "sys:src;lisp;regression-tests;encoding-test.txt")
            (bad nil))
        (unwind-protect
             (dolist (encoding (ext:all-encodings) bad)
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
                       (push (list char read-char encoding) bad))))))
          (when (probe-file filename)
            (delete-file filename))))
      (nil))

(test encoding-latin-2-plain
      (with-open-file (stream "sys:src;lisp;regression-tests;latin-2-file.txt"
                              :direction :output
                              :if-exists :overwrite
                              :if-does-not-exist :create
                              :external-format :latin-2)
        (write-char (code-char 65) stream))
      (#\A))

(test encoding-latin-2-lambda
      (let ()
        (load #P"sys:src;lisp;modules;asdf;test;lambda.lisp" :external-format :latin-2)
        (%string-char-codes  asdf-test::*LAMBDA-STRING*))
      ((206 357)))

(test encoding-iso-8859-2-lambda
      (let ()
        (load #P"sys:src;lisp;modules;asdf;test;lambda.lisp" :external-format :ISO-8859-2)
        (%string-char-codes  asdf-test::*LAMBDA-STRING*))
      ((206 357)))

(test-true compile-file-with-lambda
      (let ()
        (compile-file #P"sys:src;lisp;regression-tests;latin2-check.lisp" :external-format :iso-8859-1 :parallel nil)
        (compile-file #P"sys:src;lisp;regression-tests;latin2-check.lisp" :external-format :iso-8859-1 :parallel t)))

(test-expect-error compile-file-with-lambda-default-encoding
                   (compile-file #P"sys:src;lisp;regression-tests;latin2-check.lisp" :external-format :us-ascii)
                   :type ext:stream-decoding-error)
;;; clean-up
(delete-package (find-package :asdf-test))
(delete-package (find-package :encoding-test))
