;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains code to compile cleavir and generate a new Clasp image
;;; in which Cleavir is the default compiler
;;;

;;(require :asdf)
;;(require :clasp-cleavir)


(export '(load-system link))
(load "sys:modules;asdf;build;asdf.lisp")
(load "sys:local-asdf-config.lisp")

(load "sys:kernel;cleavir;asdf-system-groveler.lisp")

(defun save-partial-system (filename system)
  "Save the list of files in *clasp-cleavir-files* to #P\"sys:kernel;cleavir-system.lsp\""
  (with-open-file (fout filename :direction :output)
    (print system fout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Save the current list of Cleavir files

(format t "Saving Cleavir files to sys:kernel;cleavir-system.lsp~%")
(save-partial-system #P"sys:kernel;cleavir-system.lsp"
                     (append
                      (asdf-system-groveler:determine-complete-set-of-asdf-source-files (list :clasp-cleavir))
                      (list :pre-inline
                            ;; auto-compile must preceed inline because the Cleavir compiler
                            ;; needs to be the default compiler before inlining is used to
                            ;; replace CL functions like CONSP, CAR, CDR, RPLACA etc
                            #P"sys:kernel/cleavir/auto-compile"
                            #P"sys:kernel/cleavir/inline"))
(format t "Done~%")
