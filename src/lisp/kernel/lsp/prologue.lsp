#+clasp-min
(progn
  (if (member :interactive *features*)
      (progn
        (setq *features* (cons :clasp-min *features*))
        (setq *features* (cons :aclasp *features*))
        (format t "Starting ~a ... loading image... it takes a few seconds~%"
                 (lisp-implementation-version)))))
#+bclasp
(progn
  (if (member :clos *features*) nil (setq *features* (cons :clos *features*)))
  (setq *features* (cons :bclasp *features*))
  (if (member :interactive *features*)
      (format t "Starting ~a ... loading image... it takes a few seconds~%"
               (lisp-implementation-version))))
#+cclasp
(eval-when (:load-toplevel)
  (if (find-package "CLEAVIR-AST") nil (make-package "CLEAVIR-AST"))
  (if (find-package "CLASP-CLEAVIR-AST") nil (make-package "CLASP-CLEAVIR-AST"))
  (if (find-package "CLASP-CLEAVIR") nil (make-package "CLASP-CLEAVIR"))
  (setq *features* (cons :cclasp *features*))
  (if (member :clos *features*) nil (setq *features* (cons :clos *features*)))
  (if (member :cclasp *features*) nil (setq *features* (cons :cclasp *features*)))
  (if (member :interactive *features*) 
      (format t "Starting ~a cclasp ~a ... loading image... it takes a few seconds~%"
                    (if (member :use-mps *features*) "MPS" "Boehm" ) (software-version))))
