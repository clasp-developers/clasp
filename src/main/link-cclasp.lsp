(load "sys:kernel;cleavir-system.lsp")
(defun setup-cclasp-system (init-files cleavir-files)
  ;; Remove the cmprepl file and append the rest of the cleavir files
  (append init-files
          (list :bclasp)
          cleavir-files
          (list #P"kernel/cleavir/inline")
          (list :cleavir-only)
          (list #P"kernel/cleavir/auto-compile")
          (list :auto-compile :cclasp)))
(defparameter *cleavir-system* (setup-cclasp-system *init-files* *cleavir-partial-system*))
(setq *features* (list* :cclasp :clos *features*))
(load-system :start :cleavir-only :system *cleavir-system*)
;; Set up the cmp:*CLEAVIR-COMPILE-HOOK* so that COMPILE uses Cleavir
(eval-when (:execute :load-toplevel)
  (setq cmp:*cleavir-compile-hook* 'cleavir-compile-t1expr))
;; Set up the cmp:*CLEAVIR-COMPILE-HOOK* so that COMPILE-FILE uses Cleavir
(eval-when (:execute :load-toplevel)
  (setq cmp:*cleavir-compile-file-hook* 'clasp-cleavir::cleavir-compile-file-form))
;;(compile-system :init :start :recompile t :reload t)
(let ((*target-backend* (default-target-backend)))
  (link-system :init :cclasp 
               (default-prologue-form (list :cclasp :clos))
               (default-epilogue-form)
               :system *cleavir-system*))
(quit)


