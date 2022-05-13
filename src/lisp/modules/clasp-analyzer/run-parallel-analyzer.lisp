;;
;; Must be run from clasp/build
;;
;;


(require :asdf)
(asdf:load-asd (probe-file "sys:modules;clang-tool;clang-tool.asd"))
(asdf:load-asd (probe-file "sys:modules;clasp-analyzer;clasp-analyzer.asd"))
(asdf:load-system :clasp-analyzer)

(defparameter *compile-commands* "../build/preciseprep/compile_commands.json")

(defvar *project*)
(defvar *analysis*)

(defun run-search (&optional (output-filename "/tmp/static.sif") &key (pjobs 10) (subset nil))
  (format t "output-filename: ~s~%" output-filename)
  (let ((db (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database (pathname *compile-commands*))))
    (gctools:wait-for-user-signal "About to run parallel search")
    (clasp-analyzer:parallel-search/generate-code
     db
     :source-namestrings (when subset (subseq (clang-tool:source-namestrings db) 0 subset))
     :output-file (translate-logical-pathname output-filename)
     :pjobs pjobs)))

#||
(run-search)
(core:quit)
||#
