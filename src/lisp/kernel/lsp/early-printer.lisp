(in-package #:incless)

(defgeneric write-object (client object stream)
  (:method ((client null) object stream)
    (sys:write-object object stream)))

#+(or)(defgeneric circle-detection-p (client stream)
  (:method (client stream)
    (declare (ignore client stream))
    nil))

(defgeneric whitespace-char-p (client ch)
  (:method ((client null) ch)
    (eq (core:syntax-type *readtable* ch) :whitespace)))
