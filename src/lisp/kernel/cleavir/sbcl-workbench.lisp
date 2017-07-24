;;;
;;; Configure ASDF with directories in under SYS:
;;;
(require :asdf)
(progn
  (defparameter *clasp-home* #P"~/Development/clasp/")
  (defpackage "CORE" (:export #:&va-rest))
  (defpackage "CLOS" (:import-from :sb-mop #:ensure-class #:class-direct-superclasses #:ensure-class-using-class)
              (:export #:ensure-class #:class-direct-superclasses #:ensure-class-using-class))
  (defpackage "CMP" (:export #:*debug-compile-file-counter*))
  (defpackage "LITERAL" (:use :cl))
  (load (compile-file (merge-pathnames "src/lisp/kernel/cmp/cmpliteral.lsp" *clasp-home*)))
  (format t "Configuring ASDF for local directories~%")
  (defun all-subdirs (dir)
    (let (dirs)
      (labels ((trav (d)
                 (dolist (d (uiop:subdirectories d))
                   (push d dirs)
                   (trav d))))
        (trav dir))
      dirs))
  (push (merge-pathnames #P"src/lisp/kernel/cleavir/" *clasp-home*) asdf:*central-registry*)
  (push (merge-pathnames #P"src/lisp/kernel/contrib/Acclimation/" *clasp-home*) asdf:*central-registry*)
  (let* ((cleavir-dir (merge-pathnames #P"src/lisp/kernel/contrib/sicl/Code/" *clasp-home*))
         (dirs (all-subdirs (translate-logical-pathname cleavir-dir))))
       (dolist (dir dirs)
         (push dir asdf:*central-registry*))
       (format t "Added ~a subdirectories of ~a to ASDF:*CENTRAL-REGISTRY*~%"
               (length dirs) (translate-logical-pathname cleavir-dir)))
  )

(in-package :literal)

(in-package :core)
(defun create-tagged-immediate-value-or-nil (v)
  0)

(in-package :cl-user)

(progn
  (defparameter cmp:*debug-compile-file-counter* 0)
  (require :clasp-cleavir))



