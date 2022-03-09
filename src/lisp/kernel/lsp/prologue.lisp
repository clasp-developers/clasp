#+clasp-min
(progn
  (setq *features* (cons :clasp-min *features*))
  (setq *features* (cons :aclasp *features*))
  (if (core:noinform-p)
      nil
      (progn
        (format t "Starting ~a ... loading image...~%"
                 (lisp-implementation-version)))))
#+bclasp
(progn
  (if (member :clos *features*) nil (setq *features* (cons :clos *features*)))
  (setq *features* (cons :bclasp *features*))
  (if (core:noinform-p)
      nil
      (format t "Starting ~a ... loading image...~%"
               (lisp-implementation-version))))
#+cclasp
(eval-when (:load-toplevel)
  (if (find-package "CLEAVIR-AST") nil (make-package "CLEAVIR-AST"))
  (if (find-package "CLASP-CLEAVIR-AST") nil (make-package "CLASP-CLEAVIR-AST"))
  (if (find-package "CLASP-CLEAVIR") nil (make-package "CLASP-CLEAVIR"))
  (setq *features* (cons :cclasp *features*))
  (if (member :clos *features*) nil (setq *features* (cons :clos *features*)))
  (if (member :cclasp *features*) nil (setq *features* (cons :cclasp *features*)))
  (if (core:noinform-p)
      nil
      (format t "Starting ~a ... loading image...~%"
               (lisp-implementation-version))))
