(defpackage #:clasp-sandbox
  (:use #:cl))

(defpackage #:coerce
  (:export #:fdesignator))

(defpackage #:explicit
  (:export #:funcall #:apply)
  (:export #:member #:member-not #:member-if #:member-if-not)
  (:export #:eval #:compile))
