(in-package #:clasp-tests)

(test read-1
      (equalp (list 1.111 1.111 1.111d0 1.111d0)
              (let ((result nil))
                (dolist (type (list 'short-float 'single-float 'double-float 'long-float) (reverse result))
                  (let ((*read-default-float-format* type))
                    (push (read-from-string "1.111") result))))))

(test read-2
      (string-equal
       "
#.ext::single-float-positive-infinity "
       (with-output-to-string (*standard-output*)
         (let ((*read-default-float-format* 'single-float)
               (*print-readably* nil))
           (print (read-from-string (format nil "12~40,2f" most-positive-single-float)))))))

#+that-does-crash-the-lisp-in-printing
(test read-3
      (string-equal
       "
#.exit::long-float-positive-infinity "
       (with-output-to-string (*standard-output*)
         (let ((*read-default-float-format* 'long-float)
               (*print-readably* nil))
           (print (read-from-string (format nil "12~308,2f" most-positive-long-float)))))))


