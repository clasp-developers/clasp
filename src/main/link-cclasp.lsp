(make-package "CLEAVIR-AST")
(make-package "CLASP-CLEAVIR-AST")
(core::add-cleavir-to-*system-files*)
(setq *features* (list* :cclasp :clos *features*))
(setq *features* (core::recursive-remove-from-list :ecl-min *features*))
(setq *target-backend* (core::default-target-backend))
(load-system :start :pre-inline :system *system-files* :load-bitcode t)

;; Set up the cmp:*CLEAVIR-COMPILE-HOOK* so that COMPILE uses Cleavir
(eval-when (:execute :load-toplevel)
  (setq cmp:*cleavir-compile-hook* 'cleavir-compile-t1expr))
;; Set up the cmp:*CLEAVIR-COMPILE-HOOK* so that COMPILE-FILE uses Cleavir
(eval-when (:execute :load-toplevel)
  (setq cmp:*cleavir-compile-file-hook* 'clasp-cleavir::cleavir-compile-file-form))

(link-system :init :cclasp
             '(progn
               (make-package "CLEAVIR-AST")
               (make-package "CLASP-CLEAVIR-AST")
               (if (member :clos *features*) nil (setq *features* (cons :clos *features*)))
               (if (member :cclasp *features*) nil (setq *features* (cons :cclasp *features*)))
               (if (member :interactive *features*) 
                   (core:bformat t "Starting %s cclasp %s ... loading image... it takes a few seconds\n"
                                 (if (member :use-mps *features*) "MPS" "Boehm" ) (software-version))))
             '(progn
               (cl:in-package :cl-user)
               (require 'system)
               (core:load-clasprc)
               (core:process-command-line-load-eval-sequence)
               (let ((core:*use-interpreter-for-eval* nil))
                 (when (member :interactive *features*) (core:top-level)))))

(quit)


