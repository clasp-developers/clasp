(load #P"sys:modules;clang-tool;clang-tool.lisp" :print t)
(load #P"sys:modules;clasp-analyzer;clasp-analyzer.lisp" :print t)
(defparameter *compile-commands* "build/mpsprep/compile_commands.json")

(defun run-search (output-filename)
  (clasp-analyzer:parallel-search-all-threaded (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database (pathname *compile-commands*))
                                                     :output-file (translate-logical-pathname output-filename)))

