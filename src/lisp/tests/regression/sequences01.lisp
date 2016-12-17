(test-expect-error (subseq #(1 2 3) 1 5))
(test-expect-error (subseq '(1 2 3) 0 5))
(test (equal (subseq '(1 2 3 4 5) 0 3) (list 1 2 3)))
