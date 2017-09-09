
(test (= (array-dimension (make-array '(3 4) :element-type t) 0) 3))
;;(test (subtypep (class-of "abc") 'string))
(test (subtypep (type-of (make-sequence 'simple-string 10)) '(simple-array character (10))))
(test (subtypep (type-of (make-sequence 'simple-base-string 10)) '(simple-array base-char (10))))
(test (typep (make-array '(10 10)) 'simple-array))
(test (typep (make-array '(10 10) :adjustable t) 'array))
(test (null (typep (make-array '(10 10) :adjustable t) 'simple-array)))
(test (equalp (adjust-array #2A((1 2) (3 4)) '(3 3)) #2A((1 2 nil) (3 4 nil) (nil nil nil))))
