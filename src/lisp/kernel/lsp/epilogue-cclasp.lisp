#+debug-lexical-var-reference-depth
(eval-when (:compile-toplevel)
  (report-lexical-var-reference-depth))
#+cclasp
(eval-when (:load-toplevel)
  (cl:in-package :cl-user)
  (let ((core:*use-interpreter-for-eval* nil))
    ;; FIXME: probably can be made default with MPS.
    (setq clasp-cleavir::*eliminate-typeq* nil)
    (core:process-extension-loads)
    (core:maybe-load-clasprc)
    (core:process-command-line-load-eval-sequence)
    (if (core:is-interactive-lisp)
        (core:top-level :noprint (core:noprint-p))
        (core:exit 0))))
