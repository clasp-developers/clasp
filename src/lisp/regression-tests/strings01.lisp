(in-package #:clasp-tests)

(test reverse-string (let ((s "abc")) (reverse s)) ("cba") :test 'string=)
(test-expect-error subseq-oob (subseq "abc" 0 5))
(test strings-with-nul0
      (substitute #\a #\Nul (prin1-to-string (make-string 3 :initial-element #\Nul)))
      ("\"aaa\"") :test 'string=)
(test strings-with-nul1
      (substitute #\X #\Nul (with-output-to-string (str)
                              (write-char #\Nul str) (princ "123" str)))
      ("X123") :test 'string=)
(test-true concatenate-with-nul0
           (concatenate 'string (make-string 3 :initial-element #\Nul) "abc"))
(test-true concatenate-with-nul1
           (string= (concatenate 'string (make-string 3 :initial-element #\Nul) "abc")
                    (core:bformat nil "%c%c%cabc" #\nul #\nul #\nul)))
(test-true copy-seq-with-nul0
           (string= (copy-seq (concatenate 'string (make-string 3 :initial-element #\Nul) "abc"))
                    (core:bformat nil "%c%c%cabc" #\nul #\nul #\nul)))
(test string=-with-nul0
      (substitute #\X #\Nul
                  (subseq (concatenate 'string "a" (make-string 3 :initial-element #\Nul) "bcd") 0))
      ("aXXXbcd") :test 'string=)
;;; previous test does not seem to be as specified in http://www.lispworks.com/documentation/HyperSpec/Body/f_parse_.htm
;;; if something follows space, signal an error
(test-expect-error parse-integer0 (parse-integer "123 456") :type parse-error)
(test parse-integer1 (parse-integer " 123 ") (123 5))
(test-expect-error parse-integer2 (parse-integer "   123a") :type parse-error)
(test parse-integer3 (parse-integer " +123 ") (123 6))
(test parse-integer3a (parse-integer " -123 ") (-123 6))
(test-expect-error parse-integer4 (parse-integer " +-123 ") :type parse-error)
(test parse-integer5 (parse-integer " 123a" :junk-allowed t) (123 4))
(test-expect-error parse-integer6 (parse-integer "+") :type parse-error)
(test-expect-error parse-integer7 (parse-integer "-") :type parse-error)
(test-expect-error parse-integer8 (parse-integer "") :type parse-error)
(test parse-integer9 (parse-integer "+" :junk-allowed t) (nil 1))
(test parse-integer10 (parse-integer "-" :junk-allowed t) (nil 1))
(test parse-integer11 (parse-integer "" :junk-allowed t) (nil 0))

(test-true type-of-string
           (or (subtypep (type-of "abc") '(simple-array base-char (3)))
               (subtypep (type-of "abc") '(simple-array character (3)))))


(test copy-to-simple-base-string0
      (core:copy-to-simple-base-string
       (make-array 3 :element-type 'character :adjustable nil :initial-element #\C))
      ("CCC") :test 'string=)
(test copy-to-simple-base-string1
      (core:copy-to-simple-base-string
       (make-array 3 :element-type 'character :adjustable t :initial-element #\C))
      ("CCC") :test 'string=)
(test copy-to-simple-base-string2
      (core:copy-to-simple-base-string
       (make-array 3 :element-type 'base-char :adjustable nil :initial-element #\C))
      ("CCC") :test 'string=)
(test copy-to-simple-base-string3
      (core:copy-to-simple-base-string
       (make-array 3 :element-type 'base-char :adjustable t :initial-element #\C))
      ("CCC") :test 'string=)
(test copy-to-simple-base-string4
      (core:copy-to-simple-base-string :CCC) ("CCC") :test 'string=)
(test copy-to-simple-base-string5
      (core:copy-to-simple-base-string #\C) ("C") :test 'string=)


(test-type closest-sequence-type0
    (make-sequence 'simple-base-string 0)
    (vector base-char))
(test-type closest-sequence-type1
    (make-sequence 'simple-string 0)
    (vector character))

;;; These should not return t, but the first index, where it is different
(test eql-1 (string/= "a" "b") (0))
(test eql-2 (string-not-equal "a" "b") (0))

(test-true babel-simple-strings-1
           (string-equal
            (make-array 4 :element-type 'character :initial-contents (list #\? (code-char 256) #\? #\? ))
            (let ((var (copy-seq "????")))
              (setf (char var 1) (code-char 256))
              var)))

;;; What does the file-compiler do with a real unicode
(test-true babel-simple-strings-2
           (equal
            (type-of "zażółć gęślą jaźń")
            '(SIMPLE-ARRAY CHARACTER (17))))
