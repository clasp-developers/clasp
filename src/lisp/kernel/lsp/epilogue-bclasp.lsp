#+debug-lexical-var-reference-depth
(eval-when (:compile-toplevel)
  (report-lexical-var-reference-depth))
#+bclasp(eval-when (:load-toplevel)
          (format t "Starting bclasp~%")
          (cl:in-package :cl-user)
          (let ((core:*use-interpreter-for-eval* nil))
            (process-command-line-load-eval-sequence)
            (when (member :interactive *features*) (core:run-repl))))
