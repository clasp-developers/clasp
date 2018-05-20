#+debug-lexical-var-reference-depth
(eval-when (:compile-toplevel)
  (report-lexical-var-reference-depth))
#+bclasp(eval-when (:load-toplevel)
          (cl:in-package :cl-user)
          (let ((core:*use-interpreter-for-eval* nil))
            (core:process-command-line-load-eval-sequence)
            (when (member :interactive *features*)
              (format t "Starting bclasp~%")
              (core:run-repl))))

