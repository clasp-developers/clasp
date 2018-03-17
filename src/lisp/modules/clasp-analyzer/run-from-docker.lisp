(load (compile-file "/home/ubuntu/Dev/cando/src/lisp/modules/clang-tool/clang-tool.lisp"
                    :output-file "/home/app/mount-out/src/lisp/modules/clang-tool.fasl" :print t))
(load (compile-file "/home/ubuntu/Dev/cando/src/lisp/modules/clasp-analyzer/clasp-analyzer.lisp"
                    :output-file "/home/app/mount-out/src/lisp/modules/clasp-analyzer.fasl" :print t))
(defparameter *compile-commands* "/home/ubuntu/Dev/cando/build/mpsprep/compile_commands.json")
(time (clasp-analyzer:search/generate-code (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database (pathname *compile-commands*))
                                           :output-file "/home/app/mount-out/clasp_gc.cc"))
(core:quit)

