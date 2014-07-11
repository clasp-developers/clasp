;;;; ---------------------------------------------------------------------------
;;;; Re-export all the functionality in UIOP

(uiop/package:define-package :uiop/driver
  (:nicknames :uiop :asdf/driver :asdf-driver :asdf-utils)
  (:use :uiop/common-lisp)
   ;; NB: not reexporting uiop/common-lisp
   ;; which include all of CL with compatibility modifications on select platforms,
   ;; that could cause potential conflicts for packages that would :use (cl uiop)
   ;; or :use (closer-common-lisp uiop), etc.
  (:use-reexport
   :uiop/package :uiop/utility
   :uiop/os :uiop/pathname :uiop/stream :uiop/filesystem :uiop/image
   :uiop/run-program :uiop/lisp-build
   :uiop/configuration :uiop/backward-driver))

;; Provide both lowercase and uppercase, to satisfy more people.
(provide "uiop") (provide "UIOP")
