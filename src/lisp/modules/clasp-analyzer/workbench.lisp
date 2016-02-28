(print "Testing")
(progn
  (require :clang-tool)
  (require :clasp-analyzer)
  (print "Done"))

(in-package :clasp-analyzer)

(defparameter *db*
  (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
   #P"app-resources:build-databases;clasp_compile_commands.json"
   :selection-pattern ".*lisp\.cc.*"
))

(clasp-analyzer:search/generate-code *db*)



(clang-tool:with-compilation-tool-database *db*
  (defparameter *project* (clasp-analyzer:load-project))
  (search/generate-code *db*))

(clang-tool:with-compilation-tool-database *db*
  (defparameter *project* (clasp-analyzer:load-project)))


(print "Done")
(defun gcode ()
  (clang-tool:with-compilation-tool-database *db*
    (let ((analysis (analyze-project *project*)))
      (generate-code analysis))))

(gcode)
(trace clasp-analyzer::scanner-for-lispallocs)



(print "Done")
