;;
;; --------------------------------------------------
;; 
;; Install a reader macro to support insertion of C++ code into
;; common lisp code using #q{ .... #}
;;
(defun get-delimiter (char)
  (case char
    (#\{ #\})
    (#\( #\))
    (#\[ #\])
    (#\< #\>)
    (t char)))

(defun read-sharp-q (in c n)
  (declare (ignore c n))
  (let ((delimiter (get-delimiter (read-char in))))
    (let ((string (make-array '(0) :element-type 'character
                              :fill-pointer 0 :adjustable t)))
      (with-output-to-string (string-stream string)
        (loop :for char = (read-char in nil)
           :while (and char (not (and (char-equal char #\#) (char-equal (peek-char nil in) delimiter))))
           :do
           (princ char string-stream)))
      (read-char in nil)
      string)))

(set-dispatch-macro-character #\# #\q #'read-sharp-q)


