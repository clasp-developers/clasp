(in-package #:koga)

(defun write-ifndef (output-stream name)
  "Write a C ifndef preprocessor instruction."
  (format output-stream "#ifndef ~a~%" name))

(defun write-endif (output-stream)
  "Write a C endif preprocessor instruction."
  (write-line "#endif" output-stream))

(defun write-defines (output-stream &rest rest)
  "Write a C define preprocessor instructions from a plist. Values that arre NIL
will be skipped. Values that are eq to T will defined with no value. Symbol values
will be serialized without quotes. All other values will be serialized using
WRITE-TO-STRING."
  (loop for (name value) on rest by #'cddr
        when value
          do (format output-stream "#define ~a~@[ ~a~]~%"
                     name (cond ((eq t value)
                                 nil)
                                ((symbolp value)
                                 value)
                                (t
                                 (write-to-string value))))))

