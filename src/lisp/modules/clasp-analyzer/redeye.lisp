
(defparameter *compilation-database-file* (elt core:*command-line-arguments* 0))

(load (compile-file #P"sys:modules;clang-tool;clang-tool.lsp"
                    :output-file
                    (ensure-directories-exist
                     (translate-logical-pathname
                      (make-pathname
                       :host "tmp"
                       :defaults (compile-file-pathname #P"sys:modules;clang-tool;clang-tool.lsp"))))))

(load (compile-file #P"sys:modules;clasp-analyzer;clasp-analyzer.lsp"
                    :output-file
                    (ensure-directories-exist
                     (translate-logical-pathname
                      (make-pathname
                       :host "tmp"
                       :defaults (compile-file-pathname #P"sys:modules;clasp-analyzer;clasp-analyzer.lsp"))))))

(defparameter *compilation-database* (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database *compilation-database-file*))
(clasp-analyzer:search/generate-code *compilation-database*)
