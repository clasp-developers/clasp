(defpackage #:compilation-environment
  (:use #:cl)
  (:shadow #:optimize #:compiler-macro)
  (:export #:compilation-environment)
  (:export #:evaluation-environment))
