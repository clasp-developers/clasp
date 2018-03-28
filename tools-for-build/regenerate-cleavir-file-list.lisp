;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains is used to generate a flat list of files from
;;; the clasp-cleavir ASDF system.
;;;
;;; This file requires clasp, start it with:
;;; clasp --load regenerate-cleavir-file-list.lisp
;;;

(in-package :cl-user)

(require :asdf)

(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(translate-logical-pathname #P"sys:"))
   :ignore-inherited-configuration))

(load "source-dir:tools-for-build;asdf-system-groveler.lisp")

(defun save-file-list-as-python (filename file-list)
  "Save the list of files in FILE-LIST to FILENAME as a python file."
  (format t "Saving file list to ~S~%" filename)
  (with-open-file (fout filename :direction :output :if-exists :supersede)
    (format fout "#~%# This is a generated file, editing it is unwise~%#~%")
    (format fout "cleavir_file_list = [~%")
    (format fout "~&    ~s" (namestring (car file-list)))
    (dolist (one-file (cdr file-list))
      (format fout ",~&    ~s" (namestring one-file)))
    (format fout "~&]~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Save the current list of Cleavir files

(save-file-list-as-python
 (merge-pathnames #P"cleavir_file_list.py"
                  (or *load-truename*
                      *compile-file-truename*
                      (error "Both *LOAD-TRUENAME* and *COMPILE-FILE-TRUENAME* is NIL")))
 (append
  (asdf-system-groveler:determine-complete-set-of-asdf-source-files '(:clasp-cleavir))
  (list
   ;; auto-compile must preceed inline because the Cleavir compiler
   ;; needs to be the default compiler before inlining is used to
   ;; replace CL functions like CONSP, CAR, CDR, RPLACA etc
   ;;#P"src/lisp/kernel/tags/pre-auto"
   #P"src/lisp/kernel/cleavir/auto-compile"
   #P"src/lisp/kernel/cleavir/inline")))

(format t "Done~%")

(core:quit)
