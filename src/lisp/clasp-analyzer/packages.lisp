(cl:in-package #:common-lisp-user)

(defpackage #:clasp-analyzer
  (:use #:common-lisp #:core)
  (:export
   ;; clang-tool stuff
   #:load-compilation-database

   ;; mps-interface stuff
   #:parallel-search-all-then-generate-code-and-quit
   ;; stuff
))
