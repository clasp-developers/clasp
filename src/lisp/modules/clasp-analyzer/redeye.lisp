
(unless (= (length core:*command-line-arguments*) 1 )
  (format t "You must provide the path to the compilation database~%")
  (sys:quit))

(defparameter *compilation-database-file* (elt core:*command-line-arguments* 0))

(format t "Loading clang-tool~%")
(load (compile-file #P"sys:modules;clang-tool;clang-tool.lisp"
                    :output-file
                    (ensure-directories-exist
                     (translate-logical-pathname
                      (make-pathname
                       :host "tmp"
                       :defaults (compile-file-pathname #P"sys:modules;clang-tool;clang-tool.lisp"))))))

(format t "Loading clasp-analyzer~%")
(load (compile-file #P"sys:modules;clasp-analyzer;clasp-analyzer.lisp"
                    :output-file
                    (ensure-directories-exist
                     (translate-logical-pathname
                      (make-pathname
                       :host "tmp"
                       :defaults (compile-file-pathname #P"sys:modules;clasp-analyzer;clasp-analyzer.lisp"))))))

(defparameter *compilation-database* (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database *compilation-database-file*))
(clasp-analyzer:search/generate-code *compilation-database*)
