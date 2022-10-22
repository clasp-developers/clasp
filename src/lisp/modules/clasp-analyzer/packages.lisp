(cl:in-package #:common-lisp-user)

(defpackage #:clasp-analyzer
  (:shadow #:function-info #:function-type)
  (:use #:common-lisp #:ast-tooling #:clang-ast)
  (:shadow #:dump #:get-string #:size #:type)
  (:export #:setup-clasp-analyzer-compilation-tool-database
           #:load-project
           #:save-project
           #:serial-search-all
           #:serial-search/generate-code
           #:parallel-search/generate-code
           #:parallel-search-all-threaded
           #:analyze-project
           #:generate-code
           #:search-and-generate-code
           #:build-arguments-adjuster
           #:search-source-file
           #:merge-and-generate-code))
