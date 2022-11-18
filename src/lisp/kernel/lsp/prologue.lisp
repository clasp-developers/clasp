#-clasp-min
(eval-when (:load-toplevel)
  (dolist (pkg '("CLEAVIR-AST" "CLASP-CLEAVIR-AST" "CLASP-CLEAVIR"))
    (unless (find-package pkg)         
      (make-package pkg)))
  (unless (member :staging *features*)
    (unless (or (member :cclasp *features*)
                (member :eclasp *features*))
      (setq *features* (cons #+extensions (if (member :ignore-extensions *features*)
                                              :cclasp
                                            :eclasp)
                             #-extensions :cclasp
                             *features*)))
    (unless (member :clos *features*)
      (setq *features* (cons :clos *features*)))))
