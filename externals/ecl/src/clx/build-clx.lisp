;;; -*- Mode: Lisp; Package: Xlib; Log: clx.log -*-

;;; Load this file if you want to compile CLX in its entirety.
(proclaim '(optimize (speed 3) (safety 1) (space 1)
		     (compilation-speed 0)))


;;; Hide CLOS from CLX, so objects stay implemented as structures.
;;;
#||
(when (find-package "CLOS")
  (rename-package (find-package "CLOS") "NO-CLOS-HERE"))
(when (find-package "PCL")
  (rename-package (find-package "PCL") "NO-PCL-HERE"))
(when (find-package "SB-PCL")
  (rename-package (find-package "SB-PCL") "NO-SB-PCL-HERE"))
||#

(when (find-package "XLIB")
  (delete-package "XLIB"))

(unless (find-package "XLIB")
  (make-package "XLIB" :use '("COMMON-LISP")))

#-sbcl
(compile-file "clx:defsystem.lisp" :error-file nil :load t)

#+sbcl
(progn (compile-file "clx:defsystem.lisp")
       (load "clx:defsystem"))

(with-compilation-unit ()
  (#+cmu xlib:compile-clx #-cmu compile-clx (pathname "CLX:")))
