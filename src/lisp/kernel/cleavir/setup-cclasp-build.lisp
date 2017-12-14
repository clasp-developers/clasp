;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains code to compile cleavir and generate a new Clasp image
;;; in which Cleavir is the default compiler
;;;

;;(require :asdf)
;;(require :clasp-cleavir)


(export '(load-system link))
(format t "Loading asdf~%")
(load "source-dir:src;lisp;modules;asdf;build;asdf.lisp")
(format t "loading local-asdf-config.lisp~%")
(load "source-dir:src;lisp;local-asdf-config.lisp")

(format t "loading source-dir:src;lisp;kernel;asdf-system-groveler.lisp~%")
(load "source-dir:src;lisp;kernel;cleavir;asdf-system-groveler.lisp")

(defun save-partial-system (filename system)
  "Save the list of files in *clasp-cleavir-files* to #P\"source-dir:src;lisp;kernel;cleavir-system.lsp\""
  (with-open-file (fout filename :direction :output)
    (print system fout)))

(defun save-partial-system-python (filename system)
  "Save the list of files in (system) to filename as a python file"
  (format t "Saving system to ~a~%" filename)
  (with-open-file (fout filename :direction :output :if-exists :rename)
    (format fout "cleavir_parts = [~%")
    (format fout "~&    ~s" (namestring (car system)))
    (dolist (one-file (cdr system))
      (format fout ",~&    ~s" (namestring one-file)))
    (format fout "~&]~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Save the current list of Cleavir files

(save-partial-system-python
 #P"source-dir:src;lisp;kernel;cleavir.py"
 (append
  (asdf-system-groveler:determine-complete-set-of-asdf-source-files (list :clasp-cleavir))
  (list
   ;; auto-compile must preceed inline because the Cleavir compiler
   ;; needs to be the default compiler before inlining is used to
   ;; replace CL functions like CONSP, CAR, CDR, RPLACA etc
   ;;#P"src/lisp/kernel/tags/pre-auto"
   #P"src/lisp/kernel/cleavir/auto-compile"
   #P"src/lisp/kernel/cleavir/inline")))
(format t "Done~%")
