(defpackage #:clasp-sandbox
  (:use #:cl)
  (:export #:sandbox-environment #:repl)
  (:export #:fill-environment)
  (:export #:retarget))

(defpackage #:coerce
  (:use)
  (:export #:fdesignator))

(defpackage #:explicit
  (:use)
  (:export #:funcall #:apply)
  (:export #:member #:member-not #:member-if #:member-if-not
           #:assoc #:assoc-not #:assoc-if #:assoc-if-not)
  (:export #:mapc #:mapl #:mapcar #:maplist #:mapcan #:mapcon)
  (:export #:make-instance #:make-load-form)
  (:export #:macroexpand-1 #:macroexpand)
  (:export #:eval #:compile))
