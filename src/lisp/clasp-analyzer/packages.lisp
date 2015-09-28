(cl:in-package #:common-lisp-user)

(defpackage #:clasp-analyzer
  (:use #:common-lisp #:core)
  (:export
   ;; clang-tool stuff
   #:load-compilation-database

   ;; mps-interface stuff
   #:load-project
   #:parallel-search-all-then-generate-code-and-quit
   #:serial-search-all-then-generate-code-and-quit
   #:serial-search-all
   #:parallel-search-all
   #:analyze-project
   #:generate-code
   ;; stuff
))
