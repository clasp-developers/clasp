;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains code to compile cleavir and generate a new Clasp image
;;; in which Cleavir is the default compiler
;;;

;;(require :asdf)
;;(require :clasp-cleavir)


(export '(load-system link))
(load "sys:kernel;asdf;build;asdf.lisp")
(load "sys:local-asdf-config.lisp")

(load "sys:kernel;cleavir;asdf-system-groveler.lisp")

(defun save-partial-system (filename system)
  "Save the list of files in *clasp-cleavir-files* to #P\"sys:kernel;cleavir-system.lsp\""
  (with-open-file (fout filename :direction :output)
    (print `(defparameter *cleavir-partial-system* ',system) fout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Save the current list of Cleavir files

(format t "Saving Cleavir files to sys:kernel;cleavir-system.lsp~%")
(save-partial-system #P"sys:kernel;cleavir-system.lsp"
                     (asdf-system-groveler:determine-complete-set-of-asdf-source-files
                      (list :clasp-cleavir)))
(format t "Done~%")
