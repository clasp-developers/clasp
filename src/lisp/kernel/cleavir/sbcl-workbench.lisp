;;;
;;; Configure ASDF with directories in under SYS:
;;;
(require :asdf)
(progn
  (defparameter *clasp-home* #P"~/Development/clasp/")
  (defpackage "CORE" (:use :cl)
              (:nicknames "SI")
              (:export #:&va-rest #:lambda-name
                       #:multiple-value-funcall
                       #:multiple-value-prog1-function
                       #:debug-message
                       #:multiple-value-foreign-call
                       #:foreign-call
                       #:foreign-call-pointer
                       #:catch-function
                       #:progv-function
                       #:process-declarations
                       #:value-frame
                       #:get-parent-environment
                       #:*top-level-form-stack*
                       #:create-tagged-immediate-value-or-nil
                       #:stack-monitor
                       #:specialp
                       #:cleavir-ast
                       #:global-inline-status
                       #:value-environment
                       #:environment
                       #:macroexpand-default
                       #:MAKE-VALUE-ENVIRONMENT-FOR-LOCALLY-SPECIAL-ENTRIES
                       #:make-symbol-macrolet-environment
                       #:add-symbol-macro
                       #:make-macrolet-environment
                       #:add-macro
                       #:*use-interpreter-for-eval*
                       #:interpret
                       #:call-with-variable-bound
                       ))
  (defpackage "LLVM-SYS" (:use :CL)
              (:export
               ))
  (defpackage "EXT" (:use :CL)
              (:export
               #:system
               ))

  (defpackage "CLOS" (:import-from :sb-mop #:ensure-class #:class-direct-superclasses #:ensure-class-using-class)
              (:export #:ensure-class #:class-direct-superclasses #:ensure-class-using-class))
  (defpackage "CMP" (:use :cl)
              (:export #:*debug-compile-file-counter*
                       #:codegen-rtv
                       #:treat-as-special-operator-p
                       ))
  (defpackage "LITERAL" (:use :cl)
              (:export #:with-load-time-value
                       #:compile-load-time-value-thunk
                       ))

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

(declaim (declaration core:lambda-name))
(in-package :cmp)
(defun codegen-rtv (result val)
  nil)

(in-package :literal)
(defmacro with-load-time-value (&body body)
  `(progn
     ,@body))
(defun compile-load-time-value-thunk (form)
  nil)

(in-package :cl-user)

(progn
  (defparameter cmp:*debug-compile-file-counter* 0)
  (require :clasp-cleavir))



