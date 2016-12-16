
(test (eq (read-from-string (concatenate 'string "abc" (string #\nul) "AA")) (intern (concatenate 'string "ABC" (string #\nul) "AA"))))
(test (let ((s "abc")) (string= (nreverse s) "cba")))
(test (let ((s "abc")) (string= (reverse s) "cba")))
(test-expect-error (subseq "abc" 0 5))

