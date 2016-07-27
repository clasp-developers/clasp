
(test (adjustable-array-p (make-array 10 :adjustable t)) t)
(test (adjust-array "abcde" 3) "abc")
(test (type-of (make-array 10 :element-type t :fill-pointer 3)) '(vector t 10))
(test (typep (make-array 10 :fill-pointer 3) 'simple-array) nil)
(test (typep (make-array 10 :fill-pointer 3) 'simple-vector) nil)
