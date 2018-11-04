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
#.ext:single-float-positive-infinity "
       (with-output-to-string (*standard-output*)
         (let ((*read-default-float-format* 'single-float)
               (*print-readably* nil))
           (print (read-from-string (format nil "12~40,2f" most-positive-single-float)))))))

(test read-3
      (string-equal
       (concatenate 'string (string #\Newline)
                    "#.ext:double-float-positive-infinity ")
       (with-output-to-string (*standard-output*)
         (let ((*read-default-float-format* 'double-float)
               (*print-readably* nil))
           (print (read-from-string (format nil "12~308,2f" most-positive-double-float)))))))


;;; Reader-errors

(test-expect-error read-4 (READ-FROM-STRING ")") :type reader-error)
(test-expect-error read-5 (READ-FROM-STRING ",1") :type reader-error)
(test-expect-error read-6 (READ-FROM-STRING ",") :type reader-error)
(test-expect-error read-7 (READ-FROM-STRING "#)" NIL NIL) :type reader-error)


  


