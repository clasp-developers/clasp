(cl:in-package #:common-lisp-user)

(defpackage #:clang-tool
  (:use #:common-lisp #:core #:ast-tooling #:clang-ast)
  (:export
   #:with-compilation-tool-database
   #:main-pathname
   #:clang-database
   #:source-namestrings
   #:main-source-filename
   #:arguments-adjuster-list
   #:load-compilation-tool-database
   #:compile-matcher
   #:select-source-namestrings
   #:match-info
   #:id-to-node-map
   #:ast-context
   #:source-manager
   #:batch-run-multitool
   #:make-multitool
   #:multitool-add-matcher
   #:multitool-results
   #:code-match-callback
   #:mtag-node
   #:mtag-loc-start
   #:mtag-source
   #:mtag-name
   #:source-loc-as-string
   #:sub-match-run))

(defpackage #:clasp-analyzer
  (:use #:common-lisp #:core #:ast-tooling #:clang-ast)
  (:export
   #:setup-clasp-analyzer-compilation-tool-database
   #:search/generate-code
   #:load-project
   #:save-project
   #:serial-search-all
   #:search/generate-code
   #:analyze-project
   #:generate-code
   #:build-arguments-adjuster))
