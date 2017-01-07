
(test (eq (read-from-string (concatenate 'string "abc" (string #\nul) "AA")) (intern (concatenate 'string "ABC" (string #\nul) "AA"))))
(test (let ((s "abc")) (string= (nreverse s) "cba")))
(test (let ((s "abc")) (string= (reverse s) "cba")))
(test-expect-error (subseq "abc" 0 5))
(test (string= "\"aaa\"" (substitute #\a #\Nul  (prin1-to-string (make-string 3 :initial-element #\Nul)))))
(test (string= "X123" (substitute #\X #\Nul (with-output-to-string (str) (write-char #\Nul str) (princ "123" str)))))
(test (concatenate 'string (make-string 3 :initial-element #\Nul) "abc"))
(test (string= (concatenate 'string (make-string 3 :initial-element #\Nul) "abc") (bformat nil "%c%c%cabc" #\nul #\nul #\nul)))
(test (string= (copy-seq (concatenate 'string (make-string 3 :initial-element #\Nul) "abc")) (bformat nil "%c%c%cabc" #\nul #\nul #\nul)))
(test (string= (substitute #\X #\Nul (subseq (concatenate 'string "a" (make-string 3 :initial-element #\Nul) "bcd") 0)) "aXXXbcd"))
(test (multiple-value-bind (val pos) (parse-integer "123 456") (and (= val 123) (= pos 3))))

(test (= (count-occurances "azazbza" "ab") 4))

