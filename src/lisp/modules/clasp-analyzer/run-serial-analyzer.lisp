(require :asdf)
(asdf:load-asd (probe-file "sys:modules;clang-tool;clang-tool.asd"))
(asdf:load-asd (probe-file "sys:modules;clasp-analyzer;clasp-analyzer.asd"))
(asdf:load-system :clasp-analyzer)

(defparameter *compile-commands* "build/mpsprep/compile_commands.json")

(defun run-search (output-filename &key (selection-pattern nil selection-pattern-p))
  (format t "output-filename: ~s~%" output-filename)
  (time (clasp-analyzer::serial-search/generate-code
         (if selection-pattern-p
             (progn
               (format t "Using selection pattern: ~s~%" selection-pattern)
               (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
                (pathname *compile-commands*)
                :selection-pattern selection-pattern))
             (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
              (pathname *compile-commands*)))
         :output-file (translate-logical-pathname output-filename))))

