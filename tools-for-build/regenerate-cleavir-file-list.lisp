;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains is used to generate a flat list of files from
;;; the clasp-cleavir ASDF system.
;;;
;;; This file requires clasp, start it with:
;;; clasp --load regenerate-cleavir-file-list.lisp
;;;

(in-package :cl-user)

(format t "Loading asdf~%")
#+bclasp
(defpackage "CLEAVIR-ENV" (:use :common-lisp))
#+bclasp
(progn
  (defun cleavir-env::optimize (arg) nil)
  (defun cleavir-env::optimize-info (arg) nil)
  (defparameter clasp-cleavir::*clasp-env* nil)
  (export '(cleavir-env::optimize cleavir-env::optimize-info) :cleavir-env)
  (export 'clasp-cleavir::*clasp-env* :clasp-cleavir)
  (load "sys:modules;asdf;build;asdf.lisp"))
#+cclasp
(require :asdf)


(defun save-file-list-as-python (filename file-list)
  "Save the list of files in FILE-LIST to FILENAME as a python file."
  (format t "Saving file list to ~S~%" filename)
  (with-open-file (fout filename :direction :output :if-exists :supersede)
    (format fout "#~%# This is a generated file, editing it is extremely unwise - instead run regenerate-cleavir-file-list.lisp~%#~%")
    (format fout "cleavir_file_list = [~%")
    (format fout "~&    ~s" (namestring (car file-list)))
    (dolist (one-file (cdr file-list))
      (format fout ",~&    ~s" (namestring one-file)))
    (format fout "~&]~%")))

(defun save-file-list-as-common-lisp (filename file-list)
  "Save the list of files in FILE-LIST to FILENAME as a common-lisp file."
  (format t "Saving file list to ~S~%" filename)
  (with-open-file (fout filename :direction :output :if-exists :supersede)
    (format fout ";;;~%;;; This is a generated file, editing it is extremely unwise - instead run regenerate-cleavir-file-list.lisp~%;;;~%")
    (format fout "~s~%" file-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generate the current list of Cleavir files 

(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(translate-logical-pathname #P"sys:"))
   :ignore-inherited-configuration))

(load (make-pathname
       :name "asdf-system-groveler"
       :type "lisp"
       :defaults (or *load-truename* *compile-file-truename*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Save the current list of Cleavir files

(defparameter *file-list*
  (append
   (asdf-system-groveler:determine-complete-set-of-asdf-source-files '(:clasp-cleavir))
   (list
    ;; auto-compile must preceed inline because the Cleavir compiler
    ;; needs to be the default compiler before inlining is used to
    ;; replace CL functions like CONSP, CAR, CDR, RPLACA etc
    ;;#P"src/lisp/kernel/tags/pre-auto"
    "src/lisp/kernel/cleavir/auto-compile"
    "src/lisp/kernel/cleavir/inline"
    "src/lisp/kernel/cleavir/transform")))

(save-file-list-as-python
 (merge-pathnames #P"cleavir_file_list.py"
                  (or *load-truename*
                      *compile-file-truename*
                      (error "Both *LOAD-TRUENAME* and *COMPILE-FILE-TRUENAME* is NIL")))
 *file-list*)

(save-file-list-as-common-lisp
 (merge-pathnames #P"cleavir-file-list.lisp"
                  (or *load-truename*
                      *compile-file-truename*
                      (error "Both *LOAD-TRUENAME* and *COMPILE-FILE-TRUENAME* are NIL")))
 *file-list*)

(format t "Done~%")

(core:quit)
