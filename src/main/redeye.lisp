
(load "sys:modules;clasp-analyzer;load-redeye.lisp")
(time (clasp-analyzer:search/generate-code (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database "app-resources:build-databases;clasp_compile_commands.json")))
(quit)
