
(test array-dimension0 (= (array-dimension (make-array '(3 4) :element-type t) 0) 3))
;;(test (subtypep (class-of "abc") 'string))
(test make-sequence-simple-string
      (subtypep (type-of (make-sequence 'simple-string 10)) '(simple-array character (10))))
(test make-sequence-simple-base-string
      (subtypep (type-of (make-sequence 'simple-base-string 10)) '(simple-array base-char (10))))
(test typep-make-simple-array (typep (make-array '(10 10)) 'simple-array))
(test typep-make-adjustable-array (typep (make-array '(10 10) :adjustable t) 'array))
(test typep-make-adjustable-array-nonsimple (not (typep (make-array '(10 10) :adjustable t) 'simple-array)))
(test adjust-array0 (equalp (adjust-array #2A((1 2) (3 4)) '(3 3)) #2A((1 2 nil) (3 4 nil) (nil nil nil))))
