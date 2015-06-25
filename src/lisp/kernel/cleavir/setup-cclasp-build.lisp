;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains code to compile cleavir and generate a new Clasp image
;;; in which Cleavir is the default compiler
;;;

;;(require :asdf)
;;(require :clasp-cleavir)

(in-package :clasp-cleavir)

(export '(load-system link))
(load "sys:kernel;cleavir;inline.lisp")
(load "sys:kernel;cleavir;asdf-system-groveler.lisp")


;;; Determine the source files required by the :clasp-cleavir
;;; ASDF system
(defvar *cleavir-clasp-only* 
  (asdf-system-groveler:determine-complete-set-of-asdf-source-files 
   (list :clasp-cleavir)))

(defun probe-all-files-in-system (system)
  (error "Do something"))

;;;
;;; Create a list of source files for clasp+cleavir
;;;   - Inject the kernel/cleavir/inlining.lisp file at :inlining
;;;   - #P"/kernel/cleavir/auto-compile" sets up automatic compilation of top-level forms
(defun setup-clasp-cleavir-files (&optional (init-files core:*init-files*) (cleavir-files *cleavir-clasp-only*))
  ;; Remove the cmprepl file and append the rest of the cleavir files
  (append init-files
          (list :bclasp)
          *cleavir-clasp-only*
          (list #P"kernel/cleavir/inline")
          (list #P"kernel/cleavir/auto-compile")
          (list :auto-compile :cclasp)))

;;; Setup the files to build for cclasp
(defparameter *clasp-cleavir-files*
  (setup-clasp-cleavir-files core:*init-files* *cleavir-clasp-only*))

(defun save-all-files (filename)
  "Save the list of files in *clasp-cleavir-files* to #P\"sys:kernel;cleavir-system.lsp\""
  (with-open-file (fout filename :direction :output)
    (print `(defparameter *cleavir-system* ',*clasp-cleavir-files*) fout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Save the current list of Cleavir files
(save-all-files #P"sys:kernel;cleavir-system.lsp")

