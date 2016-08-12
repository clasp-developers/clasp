#+ecl-min
(progn
  (if (member :interactive *features*)
      (progn
        (setq *features* (cons :ecl-min *features*))
        (setq *features* (cons :aclasp *features*))
        (bformat t "Starting %s ... loading image... it takes a few seconds\n"
                 (lisp-implementation-version)))))
#+bclasp
(progn
  (if (member :clos *features*) nil (setq *features* (cons :clos *features*)))
  (setq *features* (cons :bclasp *features*))
  (if (member :interactive *features*)
      (bformat t "Starting %s ... loading image... it takes a few seconds\n"
               (lisp-implementation-version))))
#+cclasp
(eval-when (:load-toplevel)
  (make-package "CLEAVIR-AST")
  (make-package "CLASP-CLEAVIR-AST")
  (make-package "CLASP-CLEAVIR")
  (setq *features* (cons :cclasp *features*))
  (if (member :clos *features*) nil (setq *features* (cons :clos *features*)))
  (if (member :cclasp *features*) nil (setq *features* (cons :cclasp *features*)))
  (if (member :interactive *features*) 
      (core:bformat t "Starting %s cclasp %s ... loading image... it takes a few seconds\n"
                    (if (member :use-mps *features*) "MPS" "Boehm" ) (software-version))))
