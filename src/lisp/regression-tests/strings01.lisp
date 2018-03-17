
(test read-from-string0 (eq (read-from-string (concatenate 'string "abc" (string #\nul) "AA")) (intern (concatenate 'string "ABC" (string #\nul) "AA"))))
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
(test parse-integer0 (multiple-value-bind (val pos) (parse-integer "123 456") (and (= val 123) (= pos 3))))
(test type-of-string (subtypep (type-of "abc") '(simple-array base-char (3))))


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


(test closest-sequence-type0 (eq (core::closest-sequence-type 'simple-base-string) 'base-char))
(test closest-sequence-type1 (eq (core::closest-sequence-type 'simple-string) 'character))
