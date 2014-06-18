;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;
;;; Copyright (C) 2010, Alexander Gavrilov (angavrilov@gmail.com)
;;;
;;; This file defines the cl-simd ASDF system.
;;;
;;; Note that a completely independent definition
;;; is used to build the system as an ECL contrib.

(defsystem :cl-simd
  :version "1.0"
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;CL-SIMD;"
  :components
  #+(and sbcl sb-sse-intrinsics)
  ((:file "sse-package")
   (:file "sbcl-core" :depends-on ("sse-package"))
   (:file "sse-intrinsics" :depends-on ("sbcl-core"))
   (:file "sbcl-functions" :depends-on ("sse-intrinsics"))
   (:file "sbcl-arrays" :depends-on ("sbcl-functions"))
   (:file "sse-array-defs" :depends-on ("sbcl-arrays"))
   (:file "sse-utils" :depends-on ("sse-array-defs")))
  #+(and ecl sse2)
  ((:file "sse-package")
   (:file "ecl-sse-core" :depends-on ("sse-package"))
   (:file "sse-intrinsics" :depends-on ("ecl-sse-core"))
   (:file "sse-array-defs" :depends-on ("sse-intrinsics"))
   (:file "ecl-sse-utils" :depends-on ("sse-intrinsics"))
   (:file "sse-utils" :depends-on ("ecl-sse-utils")))
  #-(or (and sbcl sb-sse-intrinsics)
        (and ecl sse2))
  ())

#+(or (and sbcl sb-sse-intrinsics)
      (and ecl sse2))
(defmethod perform :after ((o load-op) (c (eql (find-system :cl-simd))))
  (provide :cl-simd))

(defmethod perform ((o test-op) (c (eql (find-system :cl-simd))))
  #+(or (and sbcl sb-sse-intrinsics)
        (and ecl sse2))
  (or (load (compile-file "test-sfmt.lisp"))
      (error "test-sfmt failed")))

