#+debug-lexical-var-reference-depth
(eval-when (:compile-toplevel)
  (report-lexical-var-reference-depth))
#+cclasp(eval-when (:load-toplevel)
          (cl:in-package :cl-user)
          (core:process-extension-loads)
          (core:maybe-load-clasprc)
          (let ((core:*use-interpreter-for-eval* nil))
            (core:process-command-line-load-eval-sequence)
            (if (core:is-interactive-lisp)
                (core:top-level nil nil)
                (core:exit 0))))

