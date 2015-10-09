(format t "Building cleavir clasp full version - loading compile-cclasp.lisp~%")
(defpackage #:cclasp-build
  (:use #:common-lisp #:core))
(in-package :cclasp-build)
(format t "Loading cleavir-system.lsp~%")
(core::add-cleavir-to-*system-files*)
(core:clean-system nil :no-prompt t :stage "cclasp")
(core:load-system :bclasp :pre-inline)
(setq *features (list* :clos :cclasp (remove :bclasp *features*)))
(setq core:*target-backend* (core::default-target-backend))
;; Set up the cmp:*CLEAVIR-COMPILE-HOOK* so that COMPILE uses Cleavir
(eval-when (:execute :load-toplevel)
  (setq cmp:*cleavir-compile-hook* 'cleavir-compile-t1expr))
;; Set up the cmp:*CLEAVIR-COMPILE-HOOK* so that COMPILE-FILE uses Cleavir
(eval-when (:execute :load-toplevel)
  (setq cmp:*cleavir-compile-file-hook* 'clasp-cleavir::cleavir-compile-file-form))
(let ((compiler-symbol (find-symbol "*COMPILER*" "CLEAVIR-GENERATE-AST")))
  (setf (symbol-value compiler-symbol) 'cl:compile-file))
(let* ((cmp:*cleavir-compile-file-hook* (fdefinition (find-symbol "CLEAVIR-COMPILE-FILE-FORM" "CLASP-CLEAVIR")))
       (bitcode-files (core:compile-system :init
                                           :cclasp
                                           :recompile t
                                           :reload nil)))
  (unless dont-link
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

    (link-cclasp from-mod to-mod :system system)))



   (let ((cmp:*compile-print* t))
    (compile-clasp :init :auto-cleavir
                   :recompile recompile 
                   :reload reload
                   :system system)))


  (cclasp-build:compile-full-cclasp cclasp-system))
(core:quit)
