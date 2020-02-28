(in-package #:clasp-tests)

(test reverse-string (let ((s "abc")) (string= (reverse s) "cba")))
(test-expect-error subseq-oob (subseq "abc" 0 5))
(test strings-with-nul0
      (string= "\"aaa\"" (substitute #\a #\Nul  (prin1-to-string (make-string 3 :initial-element #\Nul)))))
(test strings-with-nul1
      (string= "X123" (substitute #\X #\Nul (with-output-to-string (str)
                                              (write-char #\Nul str) (princ "123" str)))))
(test concatenate-with-nul0 (concatenate 'string (make-string 3 :initial-element #\Nul) "abc"))
(test concatenate-with-nul1 (string= (concatenate 'string (make-string 3 :initial-element #\Nul) "abc")
                                     (bformat nil "%c%c%cabc" #\nul #\nul #\nul)))
(test copy-seq-with-nul0 (string= (copy-seq (concatenate 'string (make-string 3 :initial-element #\Nul) "abc"))
                                  (bformat nil "%c%c%cabc" #\nul #\nul #\nul)))
(test string=-with-nul0
      (string= (substitute #\X #\Nul
                           (subseq (concatenate 'string "a" (make-string 3 :initial-element #\Nul) "bcd") 0))
               "aXXXbcd"))
;;; previous test does not seem to be as specified in http://www.lispworks.com/documentation/HyperSpec/Body/f_parse_.htm
;;; if something follows space, signal an error
(test-expect-error parse-integer0 (multiple-value-bind (val pos)
                                      (parse-integer "123 456"))
                   :type parse-error)
(test parse-integer1 (multiple-value-bind (val pos)
                         (parse-integer " 123 ")
                       (and (= val 123) (= pos 5))))

(test-expect-error parse-integer2 (multiple-value-bind (val pos)
                                      (parse-integer "   123a"))
                   :type parse-error)
(test parse-integer3
      (multiple-value-bind (val pos)
          (parse-integer " +123 ")
        (and (= val 123) (= pos 6))))

(test parse-integer3a
      (multiple-value-bind (val pos)
          (parse-integer " -123 ")
        (and (= val -123) (= pos 6))))

(test-expect-error parse-integer4
      (multiple-value-bind (val pos)
          (parse-integer " +-123 "))
      :type parse-error)

(test parse-integer5 (multiple-value-bind (val pos)
                                      (parse-integer " 123a" :junk-allowed t)
                                    (and (= val 123) (= pos 4))))

(test-expect-error parse-integer6
      (multiple-value-bind (val pos)
          (parse-integer "+"))
      :type parse-error)

(test-expect-error parse-integer7
      (multiple-value-bind (val pos)
          (parse-integer "-"))
      :type parse-error)

(test-expect-error parse-integer8
      (multiple-value-bind (val pos)
          (parse-integer ""))
      :type parse-error)

(test parse-integer9
      (not
       (multiple-value-bind (val pos)
           (parse-integer "+" :junk-allowed t))))

(test parse-integer10
      (not
       (multiple-value-bind (val pos)
           (parse-integer "-" :junk-allowed t))))

(test parse-integer11
      (not
       (multiple-value-bind (val pos)
           (parse-integer "" :junk-allowed t))))

(test type-of-string (or (subtypep (type-of "abc") '(simple-array base-char (3)))
                         (subtypep (type-of "abc") '(simple-array character (3)))))


(test copy-to-simple-base-string0
      (equal (core:copy-to-simple-base-string
              (make-array 3 :element-type 'character :adjustable nil :initial-element #\C))
             "CCC"))
(test copy-to-simple-base-string1
      (equal (core:copy-to-simple-base-string
              (make-array 3 :element-type 'character :adjustable t :initial-element #\C))
             "CCC"))
(test copy-to-simple-base-string2
      (equal (core:copy-to-simple-base-string
              (make-array 3 :element-type 'base-char :adjustable nil :initial-element #\C))
             "CCC"))
(test copy-to-simple-base-string3
      (equal (core:copy-to-simple-base-string
              (make-array 3 :element-type 'base-char :adjustable t :initial-element #\C))
             "CCC"))
(test copy-to-simple-base-string4
      (equal (core:copy-to-simple-base-string :CCC) "CCC"))
(test copy-to-simple-base-string5
      (equal (core:copy-to-simple-base-string #\C) "C"))


(test closest-sequence-type0 (equal (core::sequence-type-maker-info 'simple-base-string) '(vector base-char)))
(test closest-sequence-type1 (equal (core::sequence-type-maker-info 'simple-string) '(vector character)))

;;; These should not return t, but the first index, where it is different
(test eql-1 (eql 0 (string/= "a" "b")))
(test eql-2 (eql 0 (string-not-equal "a" "b")))

(test babel-simple-strings-1
      (string-equal
       (make-array 4 :element-type 'character :initial-contents (list #\? (code-char 256) #\? #\? ))
       (let ((var (copy-seq "????")))
         (setf (char var 1) (code-char 256))
         var)))

;;; What does the file-compiler do with a real unicode
(test babel-simple-strings-2
      (equal
       (type-of "zażółć gęślą jaźń")
       '(SIMPLE-ARRAY CHARACTER (17))))
