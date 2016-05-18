
(defmacro test (the-test expected-result &key (compare #'equal))
  (let ((calc-result (gensym)))
    `(let ((,calc-result ,the-test))
       (if (funcall ,compare ,calc-result ,expected-result)
           (format t "passed~%")
           (format t "Test ~a failed - got: ~a  expected: ~a~%" ',the-test ,calc-result ,expected-result)))))

(load "low0.lisp")
