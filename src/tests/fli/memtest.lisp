
(defun f1 ()
  (cffi:with-foreign-object (p :long-long)
    (setf (cffi:mem-ref p :long-long) -9223372036854775807)
    (cffi:mem-ref p :long-long)))

(defun f2 ()
  (cffi:with-foreign-object (p :unsigned-long-long)
    (setf (cffi:mem-ref p :unsigned-long-long) 18446744073709551615)
    (cffi:mem-ref p :unsigned-long-long)))
