
(load "sys:modules;clang-tool;clang-tool.lisp")

(print "Done")

(defparameter *db* (clang-tool:load-compilation-tool-database "app-resources:build-databases;clasp_compile_commands.json" ))
(setf (clang-tool:source-namestrings *db*) (list (find-if (lambda (x) (search "cons" x)) (clang-tool:source-namestrings *db*))))

(clang-tool:load-asts *db*)
