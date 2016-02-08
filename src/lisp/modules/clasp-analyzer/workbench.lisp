(print "Testing")
(progn
  (require :clang-tool)
  (require :clasp-analyzer)
  (print "Done"))

(in-package :clasp-analyzer)

(defparameter *db*
  (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
   #P"app-resources:build-databases;clasp_compile_commands.json"
   :selection-pattern ".*cons\.cc.*$"
   :source-path-identifier "/clasp/"))

(defparameter *project* (search/generate-code *db*))
