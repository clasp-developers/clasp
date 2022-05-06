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


(defun save-file-list-as-common-lisp (filename file-list)
  "Save the list of files in FILE-LIST to FILENAME as a common-lisp file."
  (when (probe-file filename)
    (rename-file filename (make-pathname :type "lispold" :defaults filename) :if-exists :supersede))
  (format t "Saving file list to ~S~%" filename)
  (with-open-file (fout filename :direction :output :if-exists :supersede)
    (format fout ";;;~%;;; This is a generated file, editing it is extremely unwise - instead run regenerate-cleavir-file-list.lisp~%;;;~%")
    (format fout "~s~%" file-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generate the current list of Cleavir files 

(let* ((here (or *load-truename* *compile-file-truename*))
       (root-clasp (make-pathname :directory
                                  (butlast (pathname-directory here))))
       (root-sys
         (make-pathname :directory
                        (append (butlast (pathname-directory here)) (list "src" "lisp")))))
  (format t "Root for clearvir-files is ~a ~%" root-sys)
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,root-sys)
     :ignore-inherited-configuration))
  (setf (logical-pathname-translations "source-dir")
        `((#P"SOURCE-DIR:**;*.*"
             ,(pathname (concatenate 'string (namestring root-clasp) "**/*.*")))))
  (flet ((fix-system (relative-directory-list system)
           (let ((directory (make-pathname :directory (append (butlast (pathname-directory here)) relative-directory-list) :defaults nil)))
             (setf (slot-value system 'asdf/component:relative-pathname) (namestring directory))
             (setf (slot-value system 'asdf/component:absolute-pathname) (namestring directory))
             ;;; update the children
             (dolist (child (asdf/component:component-children system))
               (setf (slot-value child 'asdf/component:absolute-pathname)
                     (namestring (make-pathname :directory (pathname-directory directory) :defaults (slot-value child 'asdf/component:absolute-pathname)))))
             (format t "Sourcedir for ~a is ~a ~%" system (asdf/component:component-pathname system)))))
    ;;; Alexandria might have the wrong source-pathname
    (unless (search (namestring root-sys) (namestring (asdf/component:component-pathname (asdf:find-system :alexandria))))
      (fix-system (list "src" "lisp" "kernel" "contrib" "alexandria")(asdf:find-system :alexandria)))
    ;;; Closer-mop as well
    (unless (search (namestring root-sys) (namestring (asdf/component:component-pathname (asdf:find-system :closer-mop))))
      (fix-system (list "src" "lisp" "kernel" "contrib" "closer-mop")(asdf:find-system :closer-mop)))
    (unless (search (namestring root-sys) (namestring (asdf/component:component-pathname (asdf:find-system :clasp-cleavir))))
      (fix-system (list "src" "lisp" "kernel" "cleavir")(asdf:find-system :clasp-cleavir)))
    (format t "Sourcedir is  ~a ~%" (translate-logical-pathname "source-dir:"))
    (load (make-pathname
           :name "asdf-system-groveler"
           :type "lisp"
           :defaults here))))

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
    "src/lisp/kernel/cleavir/inline")))

(save-file-list-as-common-lisp
 (merge-pathnames #P"cleavir-file-list.lisp"
                  (or *load-truename*
                      *compile-file-truename*
                      (error "Both *LOAD-TRUENAME* and *COMPILE-FILE-TRUENAME* are NIL")))
 *file-list*)

(format t "Done~%")

(core:quit)
