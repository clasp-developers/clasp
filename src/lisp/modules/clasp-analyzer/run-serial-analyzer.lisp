(load (compile-file #P"sys:modules;clang-tool;clang-tool.lisp" :print t))
(load (compile-file #P"sys:modules;clasp-analyzer;clasp-analyzer.lisp" :print t))
(defparameter *compile-commands* "build/mpsprep/compile_commands.json")
(time (clasp-analyzer:serial-search/generate-code (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database (pathname *compile-commands*))))
(core:quit)

#|

(clasp-analyzer::generate-code clasp-analyzer::*analysis* :output-file "/tmp/project.cc")

|#
